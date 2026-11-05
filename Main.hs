{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception
import Control.Monad (foldM, forM, unless, when)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Char (chr, isAlphaNum, isDigit, isLower, isSpace, isUpper, ord, toLower)
import Data.Either (lefts)
import Data.Functor ((<&>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.IntMap as IM
import Data.List (find, foldl', intercalate, isInfixOf, isPrefixOf, nub, sort, tails)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Version (showVersion)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), getOpt, usageInfo)
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO
import System.Info (compilerName, compilerVersion)
import System.Posix.IO (stdInput)
import System.Posix.Terminal
  ( TerminalAttributes, TerminalMode (..), TerminalState (..), getTerminalAttributes
  , queryTerminal, setTerminalAttributes, withMinInput, withTime, withoutMode )
import System.Process (readCreateProcess, shell)
import Text.Parsec (ParseError, sourceColumn, sourceLine)
import Text.Parsec.Error (errorMessages, errorPos, showErrorMessages)
import qualified Text.PrettyPrint as P
import Text.Read (readMaybe)

import Lisp (Def, MDef, MExp, Name, evalMExp)
import Paths_lisp_m (version)
import Syntax (builtins, parseProgram, prettyMDef)

-- REPL state

data St = St
  { defs     :: Def String
  , lastFile :: Maybe FilePath
  , past     :: [String]                   -- input history, most recent first
  , keep     :: Bool                       -- whether history is saved to a file
  , colour   :: Bool
  , cooked   :: Maybe TerminalAttributes   -- Nothing when not on a terminal
  , errors   :: IORef Int
  }

-- Command-line options

data Flag = Load FilePath | Eval String | Interactive | NoColour | NoHistory | Help | Version
  deriving Eq

options :: [OptDescr Flag]
options =
  [ Option "l" ["load"]        (ReqArg Load "FILE") "load definitions from FILE (repeatable)"
  , Option "e" ["eval"]        (ReqArg Eval "EXPR") "evaluate EXPR, then exit (repeatable)"
  , Option "i" ["interactive"] (NoArg Interactive)  "start the REPL even after --eval"
  , Option ""  ["no-color"]    (NoArg NoColour)     "do not use colours"
  , Option ""  ["no-history"]  (NoArg NoHistory)    "do not read or write ~/.lisp_history"
  , Option "h" ["help"]        (NoArg Help)         "show this help, then exit"
  , Option "v" ["version"]     (NoArg Version)      "show the version, then exit"
  ]

usage :: String
usage = usageInfo header options ++ footer
  where
    header = unlines
      [ "Usage: lisp-m [OPTION]... [FILE]..."
      , "An interpreter for McCarthy's M-expressions."
      , ""
      , "Files and expressions are processed in the order given; a FILE"
      , "without an option is loaded as with --load. Without --eval, the REPL"
      , "starts afterwards; type :help there for its commands and keys."
      , ""
      , "Options:"
      ]
    footer = unlines
      [ ""
      , "Examples:"
      , "  lisp-m -l prelude.lisp-m                 load a file, then start the REPL"
      , "  lisp-m -l prelude.lisp-m -e 'ff[(A B)]'  load a file and print one result"
      , "  lisp-m < script.lisp-m                   run a script from standard input"
      , ""
      , "The exit status is 1 if --eval or a script reported errors,"
      , "and 2 if the command line is invalid."
      ]

versionText :: String
versionText = "lisp-m " ++ showVersion version ++ " (" ++ compilerName ++ " " ++ showVersion compilerVersion ++ ")"

main :: IO ()
main = getArgs <&> getOpt (ReturnInOrder Load) options >>= \case
  (flags, _, [])
    | Help `elem` flags    -> putStr usage
    | Version `elem` flags -> putStrLn versionText
    | otherwise            -> run flags
  (_, _, errs) -> do
    hPutStr stderr (concatMap ("lisp-m: " ++) errs ++ "Try 'lisp-m --help' for more information.\n")
    exitWith (ExitFailure 2)

run :: [Flag] -> IO ()
run flags = do
  let interactive = Interactive `elem` flags || not (any isEval flags)
      isEval = \case Eval _ -> True; _ -> False
  out <- hIsTerminalDevice stdout
  tty <- (&& (interactive && out)) <$> queryTerminal stdInput
  plain <- (\nc t -> isJust nc || t == Just "dumb") <$> lookupEnv "NO_COLOR" <*> lookupEnv "TERM"
  attrs <- if tty then Just <$> getTerminalAttributes stdInput else pure Nothing
  let keepHistory = tty && NoHistory `notElem` flags
  hist <- if keepHistory then loadHistory else pure []
  mapM_ (`hSetEncoding` utf8) [stdin, stdout]
  when tty $ do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout (BlockBuffering Nothing)
  errs <- newIORef 0
  -- Outside the editor, input is not echoed, so Ctrl-C does not print ^C
  let st = St [] Nothing hist keepHistory (out && not plain && NoColour `notElem` flags)
              (fmap (`withoutMode` EnableEcho) attrs) errs
      reset = mapM_ (\a -> setTerminalAttributes stdInput a Immediately) attrs
      step s = \case
        Load f -> loadFile s f
        Eval e -> fromMaybe s <$> process s e
        _ -> pure s
  flip finally reset $ do
    when tty (banner st)
    st' <- foldM step st flags
    when interactive (repl st')
  -- A REPL on a terminal reports errors as it goes; anything else fails
  n <- readIORef errs
  when (n > 0 && not tty) exitFailure

banner :: St -> IO ()
banner st = say $ paintWith st "1" "Lisp" ++ " - M-expressions after McCarthy (1960). Type "
  ++ paintWith st "1;36" ":help" ++ " for help, C-d to quit."

repl :: St -> IO ()
repl st = getInput st >>= \case
  Nothing -> pure ()
  Just s -> do
    st' <- remember st s
    next <- process st' s `catch` \case
      UserInterrupt -> failure st' "interrupted" >> pure (Just st')
      e -> throwIO e
    mapM_ repl next

getInput :: St -> IO (Maybe String)
getInput st = case cooked st of
  Just attrs -> do
    w <- termWidth
    editLine (Config (colour st) w (complete st) attrs) (past st)
  Nothing -> plainInput

-- Without a terminal, read lines until the brackets balance
plainInput :: IO (Maybe String)
plainInput = go []
  where
    go acc = isEOF >>= \case
      True -> pure (if null acc then Nothing else Just (block acc))
      False -> do
        l <- getLine
        let acc' = l : acc
        if ready (block acc') then pure (Just (block acc')) else go acc'
    block = intercalate "\n" . reverse

process :: St -> String -> IO (Maybe St)
process st s = case dropWhile isSpace s of
  "" -> pure (Just st)
  ':' : cmd -> command st (words cmd)
  _ -> case parseProgram "input" s of
    Left e -> parseFailure st "" s e >> pure (Just st)
    Right items -> Just <$> runItems False st items

runItems :: Bool -> St -> [Either (MDef String) (MExp String)] -> IO St
runItems quiet = foldM item
  where
    item st (Left d@(f, (xs, _))) = do
      let verb = if isJust (lookup f (defs st)) then "redefined " else "defined "
      unless quiet $
        say (paintWith st "2" verb ++ paint (colour st) (f ++ "[" ++ intercalate "; " xs ++ "]"))
      pure st{defs = d : filter ((/= f) . fst) (defs st)}
    item st (Right m) = evalPrint st m >> pure st

evalPrint :: St -> MExp String -> IO ()
evalPrint st m = try (evaluate (force (runExcept (runReaderT (evalMExp m) ([], defs st))))) >>= \case
  Right (Right out) -> say (paint (colour st) out)
  Right (Left msg)  -> failure st msg
  Left e            -> failure st (describe e)
  where
    force (Right s) = let out = show s in length out `seq` Right out
    force (Left msg) = length msg `seq` Left msg

describe :: SomeException -> String
describe e = case fromException e of
  Just UserInterrupt -> "interrupted"
  Just StackOverflow -> "stack overflow (recursion too deep)"
  Just HeapOverflow  -> "out of memory"
  _                  -> displayException e

loadFile :: St -> FilePath -> IO St
loadFile st "" = failure st "usage: :load file" >> pure st
loadFile st path = try (readFile' path) >>= \case
  Left e -> failure st (displayException (e :: IOException)) >> pure st
  Right src -> case parseProgram path src of
    Left e -> parseFailure st (" in " ++ path) src e >> pure st'
    Right items -> do
      st'' <- runItems True st' items
      let n = length (lefts items)
      info st ("loaded " ++ path ++ " (" ++ show n ++ " definition" ++ (if n == 1 then "" else "s") ++ ")")
      pure st''
  where st' = st{lastFile = Just path}

-- Commands

command :: St -> [String] -> IO (Maybe St)
command st [] = command st ["help"]
command st ("?" : args) = command st ("help" : args)
command st (name : args) = case find (\(n, _, _, _) -> n == name) commands of
  Just (_, _, _, run) -> run st args
  Nothing -> case filter (\(n, _, _, _) -> name `isPrefixOf` n) commands of
    [(_, _, _, run)] -> run st args
    [] -> failure st ("unknown command :" ++ name ++ ", try :help") >> pure (Just st)
    cs -> failure st ("ambiguous command :" ++ name ++ ", could be "
                      ++ intercalate " or " [':' : n | (n, _, _, _) <- cs]) >> pure (Just st)

commands :: [(String, String, String, St -> [String] -> IO (Maybe St))]
commands =
  [ ("help",   "",      "show this help",                \st _ -> help st >> continue st)
  , ("quit",   "",      "leave the REPL (or C-d)",       \_ _ -> pure Nothing)
  , ("defs",   "",      "list all definitions",          \st _ -> showDefs st (reverse (defs st)) >> continue st)
  , ("show",   "f ...", "show the given definitions",    \st fs -> showNamed st fs >> continue st)
  , ("load",   "file",  "load definitions from a file",  \st args -> Just <$> loadFile st (unwords args))
  , ("reload", "",      "load the last file again",      \st _ -> maybe (failure st "no file loaded yet" >> continue st)
                                                                        (fmap Just . loadFile st) (lastFile st))
  , ("forget", "f ...", "remove the given definitions",  \st fs -> Just <$> forget st fs)
  , ("reset",  "",      "remove all definitions",        \st _ -> info st "all definitions removed" >> continue st{defs = []})
  ]
  where continue = pure . Just

showDefs :: St -> [MDef String] -> IO ()
showDefs st [] = info st "no definitions"
showDefs st ds = mapM_ (say . paint (colour st) . P.render . prettyMDef) ds

showNamed :: St -> [Name] -> IO ()
showNamed st [] = failure st "usage: :show f ..."
showNamed st fs = mapM_ one fs
  where one f = maybe (failure st (f ++ " is not defined")) (\d -> showDefs st [(f, d)]) (lookup f (defs st))

forget :: St -> [Name] -> IO St
forget st [] = failure st "usage: :forget f ..." >> pure st
forget st fs = foldM one st fs
  where
    one s f
      | isJust (lookup f (defs s)) = info s ("forgot " ++ f) >> pure s{defs = filter ((/= f) . fst) (defs s)}
      | otherwise = failure s (f ++ " is not defined") >> pure s

help :: St -> IO ()
help st = mapM_ say $
     section "Commands" (paintWith st "1;36")
       [(':' : n ++ (if null a then "" else ' ' : a), d) | (n, a, d, _) <- commands]
  ++ section "Editing (Emacs keys)" (paintWith st "1;36")
       [ ("C-f C-b M-f M-b", "move by character or word")
       , ("C-a C-e", "move to the start or end of the line")
       , ("M-< M->", "move to the start or end of the input")
       , ("C-p C-n", "move between lines; on the first or last line, browse history")
       , ("M-p M-n", "browse history")
       , ("C-r", "search history (C-r again for older matches, C-g to give up)")
       , ("C-d DEL", "delete the next or previous character")
       , ("C-k C-u", "kill to the end or start of the line")
       , ("M-d M-DEL C-w", "kill the next or previous word")
       , ("C-y", "yank the last kill")
       , ("C-t", "transpose characters")
       , ("C-_ C-/", "undo")
       , ("RET", "submit when the brackets balance, otherwise start a new line")
       , ("C-j M-RET", "always start a new line")
       , ("TAB", "complete names, commands and file names")
       , ("C-l", "clear the screen")
       , ("C-g C-c", "cancel the current input")
       , ("C-d", "quit, when the input is empty")
       ]
  ++ section "Syntax" (paint (colour st))
       [ ("f[x; y] = e", "define a function")
       , ("f[e; e]", "call a function")
       , ("atom eq cons car cdr", "built-in functions")
       , ("{p -> e; q -> e}", "conditional: the first true p selects its e")
       , ("T NIL", "true and false; NIL is also the empty list")
       , ("A (A B) (A . B)", "S-expressions: atoms, lists and pairs")
       ]
  where
    section title style rows =
      "" : paintWith st "1" title : ["  " ++ style (padTo 22 k) ++ d | (k, d) <- rows]
    padTo n s = s ++ replicate (n - length s) ' '

complete :: St -> String -> Int -> IO (Int, [String])
complete st t p
  | ':' : cmd <- before, not (any isSpace cmd) =
      pure (0, [':' : n ++ " " | (n, _, _, _) <- commands, cmd `isPrefixOf` n])
  | ':' : rest <- before, let cmd = takeWhile (not . isSpace) rest
  , not (null cmd), cmd `isPrefixOf` "load" = files
  | otherwise =
      pure (start, sort (nub [n | not (null word), n <- builtins ++ map fst (defs st), word `isPrefixOf` n]))
  where
    before = take p t
    word = reverse (takeWhile isLower (reverse before))
    start = p - length word
    files = do
      let tok = reverse (takeWhile (not . isSpace) (reverse before))
          (dir, base) = let (b, d) = break (== '/') (reverse tok) in (reverse d, reverse b)
          hidden = take 1 base == "."
      names <- quietly [] (listDirectory (if null dir then "." else dir))
      cands <- forM (sort [n | n <- names, base `isPrefixOf` n, hidden || take 1 n /= "."]) $ \n -> do
        isDir <- doesDirectoryExist (dir ++ n)
        pure (dir ++ n ++ if isDir then "/" else "")
      pure (p - length tok, cands)

-- Messages

say :: String -> IO ()
say s = putStrLn s >> hFlush stdout

info :: St -> String -> IO ()
info st = say . paintWith st "2"

failure :: St -> String -> IO ()
failure st msg = modifyIORef' (errors st) (+ 1) >> say (paintWith st "1;31" "error: " ++ msg)

paintWith :: St -> String -> String -> String
paintWith st sty s = if colour st then sgr sty ++ s ++ "\ESC[0m" else s

parseFailure :: St -> String -> String -> ParseError -> IO ()
parseFailure st place src e = do
  failure st ("parse error" ++ place ++ " at line " ++ show l ++ ", column " ++ show c)
  say ("  " ++ paint (colour st) line)
  say ("  " ++ replicate (c - 1) ' ' ++ paintWith st "1;31" "^")
  mapM_ (info st . ("  " ++)) msgs
  where
    pos = errorPos e
    l = sourceLine pos
    c = sourceColumn pos
    line = fromMaybe "" (listToMaybe (drop (l - 1) (lines src)))
    msgs = filter (not . null) . lines $
      showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages e)

-- History and terminal

historyFile :: IO FilePath
historyFile = (++ "/.lisp_history") <$> getHomeDirectory

loadHistory :: IO [String]
loadHistory = quietly [] (reverse . mapMaybe readMaybe . lines <$> (historyFile >>= readFile'))

remember :: St -> String -> IO St
remember st s
  | isNothing (cooked st) || all isSpace s || take 1 (past st) == [s] = pure st
  | otherwise = do
      let h = s : past st
      when (keep st) $
        quietly () (historyFile >>= \f -> writeFile f (unlines (map show (reverse (take 1000 h)))))
      pure st{past = h}

quietly :: a -> IO a -> IO a
quietly d act = act `catch` \e -> const (pure d) (e :: IOException)

termWidth :: IO Int
termWidth = do
  r <- try (readCreateProcess (shell "stty size < /dev/tty 2> /dev/null") "")
  pure $ case fmap words r of
    Right [_, c] | Just n <- readMaybe c, n > 0 -> n
    _ -> const 80 (r :: Either SomeException String)

-- Highlighting

data Scan = Scan
  { opens  :: [(Int, Char, Int)]   -- unclosed brackets, innermost first: index, bracket, depth
  , pairs  :: [(Int, Int, Int)]    -- matched brackets: open index, close index, depth
  , strays :: [Int]                -- closing brackets that match nothing
  }

scan :: String -> Scan
scan = go 0 (Scan [] [] [])
  where
    go _ r [] = r
    go i r (c : cs)
      | c `elem` "([{" = go (i + 1) r{opens = (i, c, length (opens r)) : opens r} cs
      | Just o <- lookup c (zip ")]}" "([{") = case opens r of
          (j, o', d) : rest | o == o' -> go (i + 1) r{opens = rest, pairs = (j, i, d) : pairs r} cs
          _ -> go (i + 1) r{strays = i : strays r} cs
      | otherwise = go (i + 1) r cs

unclosed :: String -> [Int]
unclosed t = [j | (j, _, _) <- opens (scan t)]

isCommand :: String -> Bool
isCommand t = take 1 (dropWhile isSpace t) == ":"

-- Whether an input is complete, or so broken that the parser should report it
ready :: String -> Bool
ready t = isCommand t || null (opens r) || not (null (strays r))
  where r = scan t

-- One SGR style per character; the bracket pair at the cursor is shown inverted
highlight :: Maybe Int -> String -> [String]
highlight cursor s
  | isCommand s =
      let (sp, r) = span isSpace s
          (cmd, args) = break isSpace r
      in map (const "") sp ++ map (const "1;36") cmd ++ map (const "") args
  | otherwise = zipWith pick [0 ..] (tokens s)
  where
    Scan os ps bad = scan s
    rainbow d = ["94", "95", "96", "93", "92"] !! (d `mod` 5)
    bracket = IM.fromList $
         concat [[(j, rainbow d), (i, rainbow d)] | (j, i, d) <- ps]
      ++ [(j, rainbow d) | (j, _, d) <- os]
      ++ [(i, "1;97;41") | i <- bad]
    matched = case cursor of
      Nothing -> []
      Just c -> take 2 (concat ([[j, i] | (j, i, _) <- ps, i == c - 1] ++ [[j, i] | (j, i, _) <- ps, j == c]))
    pick i sty = case IM.lookup i bracket of
      Just b | i `elem` matched -> b ++ ";1;7"
             | otherwise -> b
      Nothing -> sty

tokens :: String -> [String]
tokens [] = []
tokens s@(c : cs)
  | isLower c =
      let (w, rest) = span isLower s
          sty | w `elem` builtins = "1;35"
              | take 1 (dropWhile isSpace rest) == "[" = "34"
              | otherwise = ""
      in map (const sty) w ++ tokens rest
  | isUpper c =
      let (w, rest) = span (\x -> isUpper x || isDigit x) s
      in map (const (if w `elem` ["T", "NIL"] then "36" else "32")) w ++ tokens rest
  | "->" `isPrefixOf` s = "1;33" : "1;33" : tokens (drop 2 s)
  | c == '=' = "1;33" : tokens cs
  | otherwise = "" : tokens cs

sgr :: String -> String
sgr s = if null s then "" else "\ESC[" ++ s ++ "m"

-- Styled text; each newline is followed by the continuation prompt
emit :: String -> String -> [(Char, String)] -> String
emit first cont cells = first ++ go "" cells
  where
    go cur [] = reset cur
    go cur (('\n', _) : rest) = reset cur ++ "\n" ++ cont ++ go "" rest
    go cur ((ch, s) : rest)
      | s == cur = ch : go cur rest
      | otherwise = reset cur ++ sgr s ++ ch : go s rest
    reset cur = if null cur then "" else "\ESC[0m"

paint :: Bool -> String -> String
paint c s = if c then emit "" "" (zip s (highlight Nothing s)) else s

-- Line editor

data Config = Config
  { coloured  :: Bool
  , width     :: Int
  , completer :: String -> Int -> IO (Int, [String])
  , restore   :: TerminalAttributes
  }

data Search = Search
  { query   :: String
  , found   :: Int            -- history index of the match, -1 for none
  , failing :: Bool
  , origin  :: (String, Int)  -- the input before searching
  }

data Ed = Ed
  { text    :: String
  , point   :: Int
  , history :: [String]
  , hpos    :: Int            -- history entry shown, -1 for the new input
  , stash   :: String         -- the new input, while browsing history
  , killed  :: String
  , killing :: Bool           -- whether the last command killed text
  , undos   :: [(String, Int)]
  , typing  :: Bool           -- whether the last command inserted a character
  , search  :: Maybe Search
  , row     :: Int            -- cursor row of the last render
  , bottom  :: Int            -- last row of the last render
  }

data Key
  = KChar Char | KCtrl Char | KMeta Char | KPaste String
  | KEnter | KTab | KBack | KDelete | KNone
  deriving Eq

data Result = Edit Ed | Done Ed | Quit Ed | Cancel Ed | Clear Ed | Listing [String] Ed

fresh :: [String] -> String -> Ed
fresh hist k = Ed "" 0 hist (-1) "" k False [] False Nothing 0 0

editLine :: Config -> [String] -> IO (Maybe String)
editLine cfg hist = withRaw cfg (render cfg True (fresh hist "") >>= loop)
  where
    loop ed = getKey >>= \k -> step cfg k ed >>= \case
      Edit e       -> render cfg True e >>= loop
      Done e       -> Just (text e) <$ render cfg False e
      Quit e       -> Nothing <$ render cfg False e
      Cancel e     -> render cfg False e >> render cfg True (fresh (history e) (killed e)) >>= loop
      Clear e      -> putStr "\ESC[H\ESC[2J" >> render cfg True e{row = 0} >>= loop
      Listing cs e -> listing cfg e cs >>= render cfg True >>= loop

withRaw :: Config -> IO a -> IO a
withRaw cfg = bracket_
  (setTerminalAttributes stdInput raw Immediately >> out "\ESC[?2004h")
  (setTerminalAttributes stdInput (restore cfg) Immediately >> out "\ESC[?2004l")
  where
    raw = foldl' withoutMode (restore cfg)
            [EnableEcho, ProcessInput, KeyboardInterrupts, StartStopOutput, ExtendedFunctions, MapCRtoLF]
            `withMinInput` 1 `withTime` 0
    out s = putStr s >> hFlush stdout

-- Redraw the input below the prompt. An inactive render leaves the
-- input on screen, without cursor highlighting, and moves to a new line.
render :: Config -> Bool -> Ed -> IO Ed
render cfg active ed = do
  putStr ("\ESC[?25l" ++ up (row ed) ++ "\r\ESC[J" ++ drawn ++ place ++ "\ESC[?25h")
  hFlush stdout
  pure ed{row = if active then fst cur else 0, bottom = if active then end else 0}
  where
    w = max 10 (width cfg)
    t = text ed
    styles
      | coloured cfg = highlight (if active then Just (point ed) else Nothing) t
      | otherwise = map (const "") t
    footer = if active then maybe "" label (search ed) else ""
    label s = (if failing s then "(failed reverse-i-search)`" else "(reverse-i-search)`") ++ query s ++ "': "
    -- Terminal positions, where a column of w means "about to wrap"
    adv (r, c) ch
      | ch == '\n' = (r + 1, 2)
      | c >= w = (r + 1, 1)
      | otherwise = (r, c + 1)
    norm (r, c) = if c >= w then (r + 1, 0) else (r, c)
    poss = scanl adv (0, 2) t
    cur = norm (poss !! point ed)
    afterText = last poss
    (endRow, endCol)
      | null footer = afterText
      | otherwise = foldl' adv (fst afterText + 1, 0) footer
    wrapped = endCol >= w
    end = if wrapped then endRow + 1 else endRow
    drawn = emit (style "1" "> ") (style "2" ". ") (zip t styles)
         ++ (if null footer then "" else "\n" ++ style "2" footer)
    place
      | active = (if wrapped then "\n" else "") ++ up (end - fst cur) ++ "\r" ++ right (snd cur)
      | otherwise = "\n"
    style s x = if coloured cfg then sgr s ++ x ++ "\ESC[0m" else x
    up n = if n > 0 then "\ESC[" ++ show n ++ "A" else ""
    right n = if n > 0 then "\ESC[" ++ show n ++ "C" else ""

-- Print completion candidates below the input
listing :: Config -> Ed -> [String] -> IO Ed
listing cfg ed cs = do
  putStr (down (bottom ed - row ed) ++ "\n")
  mapM_ (putStr . (++ "\n") . concatMap pad) (chunks cs)
  pure ed{row = 0, bottom = 0}
  where
    cw = maximum (map length cs) + 2
    pad s = s ++ replicate (cw - length s) ' '
    chunks [] = []
    chunks xs = let (a, b) = splitAt (max 1 (width cfg `div` cw)) xs in a : chunks b
    down n = if n > 0 then "\ESC[" ++ show n ++ "B" else ""

getKey :: IO Key
getKey = hGetChar stdin >>= \case
  '\r'   -> pure KEnter
  '\t'   -> pure KTab
  '\DEL' -> pure KBack
  '\b'   -> pure KBack
  '\ESC' -> escape
  c | c < ' ' -> pure (control (toLower (chr (ord c + 64))))
    | otherwise -> pure (KChar c)
  where
    control 'c' = KCtrl 'g'
    control c = KCtrl c

-- Escape sequences are mapped to the Emacs keys they stand for
escape :: IO Key
escape = next >>= \case
  Nothing     -> pure KNone
  Just '['    -> csi ""
  Just 'O'    -> maybe KNone (\c -> fromMaybe KNone (lookup [c] table)) <$> next
  Just '\r'   -> pure (KCtrl 'j')
  Just '\DEL' -> pure (KCtrl 'w')
  Just '\b'   -> pure (KCtrl 'w')
  Just c      -> pure (KMeta c)
  where
    next = do
      avail <- hWaitForInput stdin 100
      if avail then Just <$> hGetChar stdin else pure Nothing
    csi acc = next >>= \case
      Nothing -> pure KNone
      Just c | c >= '@' && c <= '~' -> decode (reverse (c : acc))
             | otherwise -> csi (c : acc)
    decode "200~" = KPaste <$> paste ""
    decode s = pure (fromMaybe KNone (lookup s table))
    table =
      [ ("A", KCtrl 'p'), ("B", KCtrl 'n'), ("C", KCtrl 'f'), ("D", KCtrl 'b')
      , ("H", KCtrl 'a'), ("F", KCtrl 'e'), ("1~", KCtrl 'a'), ("7~", KCtrl 'a')
      , ("4~", KCtrl 'e'), ("8~", KCtrl 'e'), ("3~", KDelete)
      , ("1;5C", KMeta 'f'), ("1;5D", KMeta 'b'), ("1;3C", KMeta 'f'), ("1;3D", KMeta 'b')
      ]
    -- Bracketed paste: everything up to ESC [ 2 0 1 ~
    paste acc
      | "~102[\ESC" `isPrefixOf` acc = pure (clean (reverse (drop 6 acc)))
      | otherwise = hGetChar stdin >>= paste . (: acc)
    clean ('\r' : '\n' : s) = '\n' : clean s
    clean ('\r' : s) = '\n' : clean s
    clean ('\t' : s) = ' ' : ' ' : clean s
    clean (c : s)
      | c == '\n' || c >= ' ' = c : clean s
      | otherwise = clean s
    clean [] = []

step :: Config -> Key -> Ed -> IO Result
step cfg key ed = case search ed of
  Just s -> searching cfg key s ed
  Nothing -> edit cfg key ed <&> \case
    Edit e -> Edit (record e)
    r -> r
  where
    typed = case key of KChar _ -> True; _ -> False
    -- Consecutive characters are undone together
    record e
      | text e /= text ed, key /= KCtrl '_', not (typed && typing ed) =
          e{undos = (text ed, point ed) : undos ed, typing = typed}
      | otherwise = e{typing = typed}

edit :: Config -> Key -> Ed -> IO Result
edit cfg key ed = case key of
  KChar c   -> ok (ins [c])
  KPaste s  -> ok (ins s)
  KEnter
    | ready t   -> pure (Done e0)
    | otherwise -> ok newline
  KCtrl 'j' -> ok newline
  KCtrl 'a' -> go (lineStart t p)
  KCtrl 'e' -> go (lineEnd t p)
  KCtrl 'f' -> go (min n (p + 1))
  KCtrl 'b' -> go (max 0 (p - 1))
  KMeta 'f' -> go (wordEnd t p)
  KMeta 'b' -> go (wordStart t p)
  KMeta '<' -> go 0
  KMeta '>' -> go n
  KCtrl 'p' -> maybe (ok (browse 1 e0)) go (lineUp t p)
  KCtrl 'n' -> maybe (ok (browse (-1) e0)) go (lineDown t p)
  KMeta 'p' -> ok (browse 1 e0)
  KMeta 'n' -> ok (browse (-1) e0)
  KCtrl 'd'
    | null t    -> pure (Quit e0)
    | otherwise -> ok (remove p (p + 1))
  KDelete   -> ok (remove p (p + 1))
  KBack     -> ok (remove (p - 1) p)
  KCtrl 'k' -> ok (cut (if lineEnd t p == p then p + 1 else lineEnd t p))
  KCtrl 'u' -> ok (cut (lineStart t p))
  KMeta 'd' -> ok (cut (wordEnd t p))
  KCtrl 'w' -> ok (cut (wordStart t p))
  KCtrl 'y' -> ok (ins (killed ed))
  KCtrl 't' -> ok twiddle
  KCtrl '_' -> ok (undo e0)
  KCtrl 'l' -> pure (Clear e0)
  KCtrl 'g' -> pure (Cancel e0)
  KCtrl 'r' -> ok e0{search = Just (Search "" (-1) False (t, p))}
  KTab      -> completion cfg e0
  _         -> ok e0
  where
    t = text ed
    p = point ed
    n = length t
    e0 = ed{killing = False}
    ok = pure . Edit
    go q = ok e0{point = q}
    ins s = e0{text = take p t ++ s ++ drop p t, point = p + length s}
    newline = ins ('\n' : replicate (indentation t p) ' ')
    remove a b
      | a < 0 || b > n = e0
      | otherwise = e0{text = take a t ++ drop b t, point = a}
    -- Consecutive kills are collected into one piece of text
    cut q
      | q > n = e0
      | q >= p = ed{text = take p t ++ drop q t, killed = gather (slice p q) True, killing = True}
      | otherwise = ed{text = take q t ++ drop p t, point = q, killed = gather (slice q p) False, killing = True}
    gather k forward
      | killing ed = if forward then killed ed ++ k else k ++ killed ed
      | otherwise = k
    slice a b = take (b - a) (drop a t)
    twiddle
      | n < 2 || p == 0 = e0
      | p == n = swap (p - 2)
      | otherwise = (swap (p - 1)){point = p + 1}
    swap i = e0{text = take i t ++ [t !! (i + 1), t !! i] ++ drop (i + 2) t}

undo :: Ed -> Ed
undo e = case undos e of
  (t, p) : rest -> e{text = t, point = p, undos = rest}
  [] -> e

-- Move d entries back in history (forward when d is negative)
browse :: Int -> Ed -> Ed
browse d e
  | i < -1 || i >= length (history e) = e
  | otherwise = e{text = h, point = if d > 0 then length h else lineEnd h 0, hpos = i, stash = st}
  where
    i = hpos e + d
    st = if hpos e == -1 then text e else stash e
    h = if i == -1 then st else history e !! i

searching :: Config -> Key -> Search -> Ed -> IO Result
searching cfg key s ed = case key of
  KChar c -> pure (Edit (seek (max 0 (found s)) (query s ++ [c])))
  KCtrl 'r' | not (null (query s)) -> pure (Edit (seek (found s + 1) (query s)))
  KBack
    | length (query s) <= 1 ->
        let (t, p) = origin s in pure (Edit ed{text = t, point = p, search = Just (Search "" (-1) False (t, p))})
    | otherwise -> pure (Edit (seek 0 (init (query s))))
  KCtrl 'g' -> let (t, p) = origin s in pure (Edit ed{text = t, point = p, search = Nothing})
  _ -> step cfg key accepted
  where
    accepted = ed
      { search = Nothing
      , hpos = if found s >= 0 then found s else hpos ed
      , stash = if hpos ed == -1 then fst (origin s) else stash ed
      }
    seek i q = case [(j, h) | (j, h) <- drop i (zip [0 ..] (history ed)), q `isInfixOf` h] of
      (j, h) : _ ->
        ed{text = h, point = length (takeWhile (not . isPrefixOf q) (tails h)), search = Just s{query = q, found = j, failing = False}}
      [] -> ed{search = Just s{query = q, failing = True}}

completion :: Config -> Ed -> IO Result
completion cfg ed = do
  (start, cands) <- completer cfg t p
  let word = take (p - start) (drop start t)
      common = foldr1 commonPrefix cands
  pure $ case cands of
    []
      | start == p -> Edit ed{text = take p t ++ "  " ++ drop p t, point = p + 2}
      | otherwise -> Edit ed
    _
      | length common > length word ->
          Edit ed{text = take start t ++ common ++ drop p t, point = start + length common}
      | length cands > 1 -> Listing cands ed
      | otherwise -> Edit ed
  where
    t = text ed
    p = point ed
    commonPrefix a b = map fst (takeWhile (uncurry (==)) (zip a b))

-- A new line is indented to just inside the innermost open bracket
indentation :: String -> Int -> Int
indentation t p = case unclosed (take p t) of
  j : _ -> j - lineStart t j + 1
  [] -> length (takeWhile (== ' ') (drop (lineStart t p) t))

lineStart, lineEnd, wordStart, wordEnd :: String -> Int -> Int
lineStart t p = p - length (takeWhile (/= '\n') (reverse (take p t)))
lineEnd t p = p + length (takeWhile (/= '\n') (drop p t))
wordEnd t p =
  let (a, rest) = span (not . isAlphaNum) (drop p t)
  in p + length a + length (takeWhile isAlphaNum rest)
wordStart t p =
  let (a, rest) = span (not . isAlphaNum) (reverse (take p t))
  in p - length a - length (takeWhile isAlphaNum rest)

lineUp, lineDown :: String -> Int -> Maybe Int
lineUp t p
  | s == 0 = Nothing
  | otherwise = Just (min (lineStart t (s - 1) + p - s) (s - 1))
  where s = lineStart t p
lineDown t p
  | e == length t = Nothing
  | otherwise = Just (min (e + 1 + p - lineStart t p) (lineEnd t (e + 1)))
  where e = lineEnd t p

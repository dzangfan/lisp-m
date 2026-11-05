;;; lisp-m-mode.el --- Major mode for M-expression Lisp files  -*- lexical-binding: t; -*-

;; Author: Li Dzangfan <dzangfan.li@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, lisp

;;; Commentary:

;; A major mode for `.lisp-m' files, written in the M-expression
;; language of McCarthy's 1960 paper as read by the lisp-m interpreter:
;;
;;   ff[x] = {atom[x] -> x; T -> ff[car[x]]}
;;
;; It provides syntax highlighting and indentation.  A new line inside
;; brackets is aligned just inside the innermost open bracket, as in
;;
;;   subst[x; y; z] = {atom[z] -> {eq[z; y] -> x; T -> z};
;;                     T -> cons[subst[x; y; car[z]]; subst[x; y; cdr[z]]]}
;;
;; unless the bracket ends its line, in which case the line is indented
;; by `lisp-m-indent-offset'.
;;
;; To use it, put this file on your `load-path' and add
;;
;;   (require 'lisp-m-mode)
;;
;; to your init file.

;;; Code:

(defgroup lisp-m nil
  "Editing M-expression Lisp files."
  :group 'languages
  :prefix "lisp-m-")

(defcustom lisp-m-indent-offset 2
  "Indentation inside a bracket that ends its line.
This is also the indentation of a definition body that starts on
the line after its `='."
  :type 'integer
  :safe #'integerp)

(defface lisp-m-atom-face
  '((t :inherit font-lock-constant-face))
  "Face for atoms, such as A in (A B).")

(defface lisp-m-truth-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the atoms T and NIL.")

(defface lisp-m-call-face
  `((t :inherit ,(if (facep 'font-lock-function-call-face)
                     'font-lock-function-call-face
                   'font-lock-function-name-face)))
  "Face for the names of called functions.")

(defface lisp-m-operator-face
  '((t :inherit font-lock-keyword-face))
  "Face for `->' and `='.")

(defconst lisp-m-builtins '("atom" "eq" "cons" "car" "cdr")
  "Functions built into the interpreter.")

(defvar lisp-m-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (dolist (c '(?\; ?. ?= ?- ?>))
      (modify-syntax-entry c "." st))
    st)
  "Syntax table for `lisp-m-mode'.")

(defconst lisp-m-font-lock-keywords
  `(;; Definitions: the name and parameters in f[x; y] =
    ("^[ \t]*\\([a-z]+\\)[ \t]*\\[\\([a-z; \t]*\\)\\][ \t]*="
     (1 font-lock-function-name-face)
     ("[a-z]+"
      (progn (goto-char (match-beginning 2)) (match-end 2))
      nil
      (0 font-lock-variable-name-face)))
    (,(regexp-opt lisp-m-builtins 'words) . font-lock-builtin-face)
    ("\\<\\([a-z]+\\)[ \t]*\\[" 1 'lisp-m-call-face)
    ("\\<\\(?:T\\|NIL\\)\\>" . 'lisp-m-truth-face)
    ("\\<[A-Z][A-Z0-9]*\\>" . 'lisp-m-atom-face)
    ("->\\|=" . 'lisp-m-operator-face)
    ;; Anything the parser cannot read
    ("[^][(){};.=> \t\n[:alnum:]-]" . font-lock-warning-face))
  "Highlighting for `lisp-m-mode'.")

(defun lisp-m--hanging-p (open)
  "Whether the bracket at OPEN is the last thing on its line."
  (save-excursion
    (goto-char (1+ open))
    (skip-chars-forward " \t")
    (eolp)))

(defun lisp-m--after-equals-p ()
  "Whether the previous non-blank text ends with `='."
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\n")
    (eq (char-before) ?=)))

(defun lisp-m-indent-level ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (back-to-indentation)
    ;; syntax-ppss moves point, so look at the line first
    (let* ((closing (looking-at "\\s)"))
           (open (nth 1 (syntax-ppss (line-beginning-position)))))
      (cond
       ((null open)
        (if (lisp-m--after-equals-p) lisp-m-indent-offset 0))
       ((lisp-m--hanging-p open)
        (goto-char open)
        (+ (current-indentation) (if closing 0 lisp-m-indent-offset)))
       (closing
        (goto-char open)
        (current-column))
       (t
        (goto-char (1+ open))
        (skip-chars-forward " \t")
        (current-column))))))

(defun lisp-m-indent-line ()
  "Indent the current line as M-expression code."
  (interactive)
  (let ((offset (- (current-column) (current-indentation))))
    (indent-line-to (lisp-m-indent-level))
    (when (> offset 0)
      (forward-char offset))))

;;;###autoload
(define-derived-mode lisp-m-mode prog-mode "Lisp-M"
  "Major mode for editing M-expression Lisp files."
  :syntax-table lisp-m-mode-syntax-table
  (setq-local font-lock-defaults '(lisp-m-font-lock-keywords t))
  (setq-local indent-line-function #'lisp-m-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local electric-indent-chars (append "])}" electric-indent-chars)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lisp-m\\'" . lisp-m-mode))

(provide 'lisp-m-mode)

;;; lisp-m-mode.el ends here

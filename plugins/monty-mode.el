(require 'generic-x)

;; font-lock-warning-face
;; for a construct that is peculiar, or that greatly changes the meaning of other text, like ‘;;;###autoload’ in Emacs Lisp and ‘#error’ in C.
;; font-lock-function-name-face
;; for the name of a function being defined or declared.
;; font-lock-variable-name-face
;; for the name of a variable being defined or declared.
;; font-lock-keyword-face
;; for a keyword with special syntactic significance, like ‘for’ and ‘if’ in C.
;; font-lock-comment-face
;; for comments.
;; font-lock-comment-delimiter-face
;; for comments delimiters, like ‘/*’ and ‘*/’ in C. On most terminals, this inherits from font-lock-comment-face.
;; font-lock-type-face
;; for the names of user-defined data types.
;; font-lock-constant-face
;; for the names of constants, like ‘NULL’ in C.
;; font-lock-builtin-face
;; for the names of built-in functions.
;; font-lock-string-face
;; for string constants.

(define-generic-mode 'monty-mode
  '("#")
  '("def" "type" "class" "if" "elif" "else" "return" "instance" "of")
  '(("\\bdebug\\b" . 'font-lock-builtin-face)
    ("\\b[A-Z][a-zA-Z0-9_]*\\b" . 'font-lock-type-face)
    ("\\b[0-9]+\\b" . font-lock-constant-face)
    ("\\bdef \\([a-z][a-zA-Z0-9_]*\\)\\b" . '(1 font-lock-function-name-face))
    )
  '("\\.my$")
  nil
  "A mode for Monty files"
  )

(provide 'monty-mode)

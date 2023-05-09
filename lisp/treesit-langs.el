;;; treesit-langs.el --- tree-sitter support for C and C++  -*- lexical-binding: t; -*-
;; Created    : November 2022
;; Keywords   : c c++ cpp languages tree-sitter
;;; Commentary:
;;

;; Tree-sitter support
;; @see https://github.com/casouri/tree-sitter-module
;;      https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter

;;; Code:
(require 'treesit)
(require 'c-ts-mode)

(defface sniper-font-lock-function-name-face
    '((t (:inherit font-lock-function-name-face
          :underline t
          :italic t)))
  "Font Lock mode face used to highlight function names.")

(defface sniper-font-lock-property-face
  '((t (:inherit font-lock-type-face
        :italic t)))
  "Font Lock mode face used to highlight properties of an object.
For example, the declaration and use of fields in a struct.")

(defface sniper-macro-face
  '((t :inherit (font-lock-warning-face bold)))
  "Face for the replace state tag in evil state indicator.")

(defface sniper-ts-hl-number-face
  '((t :inherit (font-lock-constant-face bold)))
  "Face for numbers.")

;; (setq treesit--font-lock-verbose t)

(defun sniper-c-ts-mode--font-lock-settings (mode)
  "Tree-sitter font-lock settings.
MODE is either `c' or `cpp'."
  (treesit-font-lock-rules
   :language mode
   :feature 'preprocessor
   :override 'prepend
   '((preproc_directive) @font-lock-preprocessor-face

     (preproc_def
      name: (identifier) @sniper-macro-face))

   :language mode
   :feature 'literal
   :override 'prepend
   '((number_literal) @sniper-ts-hl-number-face
     (char_literal) @font-lock-constant-face)

   :language mode
   :feature 'assignment
   :override 'prepend
   ;; TODO: Recursively highlight identifiers in parenthesized
   ;; expressions, see `c-ts-mode--fontify-struct-declarator' for
   ;; inspiration.
   '((assignment_expression
      left: (identifier) @font-lock-variable-name-face)
     (assignment_expression
      left: (field_expression argument: (_) @sniper-font-lock-property-face)))

   :language mode
   :feature 'function
   :override 'prepend
   '((call_expression
      function: (identifier) @sniper-font-lock-function-name-face))

   :language mode
   :feature 'variable
   :override 'append
   '(((identifier) @font-lock-constant-face
      (:match "^[A-Z_][A-Z_\\d]*$" @font-lock-constant-face))
     (identifier) @c-ts-mode--fontify-variable)))

(defun general-ts-mode-setup ()
  (treesit-font-lock-recompute-features
   nil
   '(property bracket delimiter operator variable function))
  (when (derived-mode-p 'css-ts-mode)
    (treesit-font-lock-recompute-features
     '(property))))

(defun bash-ts-setup ()
  (setq-local treesit-font-lock-level 4)
  (treesit-font-lock-recompute-features '(command string variable function operator bracket keyword)))

(defun c-ts-setup ()
  (setq-local electric-quote-comment nil)
  (setq-local electric-quote-string nil)
  (bug-reference-prog-mode)
  (setq c-ts-mode-indent-offset 4)
  (setq evil-shift-width 4)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local treesit-font-lock-level 4)
  (setq treesit-font-lock-settings
        (append (sniper-c-ts-mode--font-lock-settings 'c) treesit-font-lock-settings))
  (treesit-font-lock-recompute-features '(property definition variable function operator)))

(defun ts-css-setup ()
  (treesit-font-lock-recompute-features '(property) '(variable function)))

;;;###autoload
(define-minor-mode sniper-ts-mode
  "Toggle sniper-modeline on or off."
  :group 'sniper-tree-sitter
  :global t
  :lighter nil
  (if sniper-ts-mode
      (when (treesit-available-p)
        (mapc
          (lambda (mode)
            (add-to-list 'major-mode-remap-alist mode))
          '((c-mode          . c-ts-mode)
            (c++-mode        . c++-ts-mode)
            (css-mode        . css-ts-mode)
            (js-mode         . js-ts-mode)
            (js2-mode        . js-ts-mode)
            (json-mode       . json-ts-mode)
            (python-mode     . python-ts-mode)
            (sh-mode         . bash-ts-mode)))
        (add-hook 'prog-mode-hook #'general-ts-mode-setup)
        (add-hook 'c-ts-mode-hook #'c-ts-setup)
        (add-hook 'c++-ts-mode-hook #'c-ts-setup)
        (add-hook 'bash-ts-mode-hook #'bash-ts-setup)
        (add-hook 'css-ts-mode-hook 'ts-css-setup))))

(provide 'treesit-langs)
;;; treesit-langs.el ends here

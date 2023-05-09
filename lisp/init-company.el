
(use-package company
 :straight t
 :defer 0.1
 :config
 (global-company-mode t)
 (setq-default
  company-idle-delay 0.05
  company-minimum-prefix-length 2
  ))

(straight-use-package 'company-prescient)

(company-prescient-mode 1)

(use-package company-box
 :straight t
 :hook (company-mode . company-box-mode))

(use-package copilot
 :disabled
 :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
 :defer 1
 :hook
 (prog-mode . copilot-mode)
 :bind
 (("C-TAB" . 'copilot-accept-completion-by-word)
  ("C-<tab>" . 'copilot-accept-completion-by-word)
  :map copilot-completion-map
  ("<tab>" . 'copilot-accept-completion)
  ("TAB" . 'copilot-accept-completion))
 :config
 (with-eval-after-load 'company
   ;; disable inline previews
   (delq 'company-preview-if-just-one-frontend company-frontends)))

(provide 'init-company)


(use-package recentf
  :after evil
  :bind
  (:map evil-normal-state-map
        ("<leader> f r" . recentf))
  :init
  (recentf-mode 1))


(use-package cnfonts
  :disabled
  :straight t
  :config
  (cnfonts-mode 1))

;; vertico--search engine 
(use-package vertico
  :straight t
  :config
  (vertico-mode 1))
(use-package orderless
  :straight t
  :after vertico
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles orderless))
                                        )))
(use-package vertico-directory
  :load-path "/home/weland/.emacs.d/straight/repos/vertico/extensions/"
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; evil--keybinding
(use-package which-key
  :straight t
  :config
  (which-key-mode 1))
(use-package evil
  :straight t
  :after which-key
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal (kbd ","))
  (evil-define-key 'normal 'global (kbd "<leader>xs") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>xf") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>xb") 'ido-switch-buffer)
  (evil-define-key 'normal 'global
    "J" 'evil-scroll-page-down
    "K" 'evil-scroll-page-up)
  (evil-define-key 'normal 'global (kbd "<leader>pp") 'project-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>pg") 'project-find-regexp)
  (evil-set-initial-state 'ekg-notes-mode 'emacs))

(use-package evil-goggles
  :straight t
  :after evil
  :config
  (setq evil-goggles-enable-delete nil)
  (evil-goggles-mode 1))

;; default settings
(pixel-scroll-precision-mode 1)
(straight-use-package 'ef-themes)
(use-package catppuccin-theme
  :straight
  (catppuccin-theme :type git :host github :repo "catppuccin/emacs")
  :config
  (setq catppuccin-flavor 'mocha))

;; 亮色主题和暗色主题
(setq day-theme-list '( modus-operandi ef-day ef-spring ef-summer ef-frost ef-light ef-cyprus ef-duo-light ef-trio-light ef-tritanopia-light ef-deuteranopia-light))
(setq dark-theme-list '( modus-vivendi ef-night ef-autumn ef-winter ef-bio ef-cherie ef-duo-dark ef-trio-dark ef-tritanopia-dark ef-deuteranopia-dark))
;; (setq day-theme-list '(catppuccin))
;; (setq dark-theme-list '(catppuccin))

;; 随机选取主题
;;;###autoload
(defun my-random-element (my-list)
  "Return a random element from MY-LIST."
  (let ((my-length (length my-list))
        (my-random-index (random (length my-list))))
    (nth my-random-index my-list)))

;; 根据时间选择亮/暗主题
;;;###autoload
(defun synchronize-theme ()
  (setq hour
        (string-to-number
         (substring (current-time-string) 11 13)))
  (if (member hour (number-sequence 6 18))
      (progn
        (setq now (my-random-element day-theme-list))
        (setq lst (my-random-element dark-theme-list))
        )
    (setq now (my-random-element dark-theme-list))
    (setq lst (my-random-element day-theme-list))
    )
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme now t))
(synchronize-theme) ;; 启动时立即执行一次
;; 每小时执行一次
(let* ((current-minutes
        (string-to-number (substring (current-time-string) 14 16)))
       (current-seconds
        (string-to-number (substring (current-time-string) 17 20)))
       (remain-seconds
        ;; remaining seconds = 3600 - 60 * min - sec
        (- 3600 (* 60 current-minutes) current-seconds))
       )
  (run-with-timer remain-seconds 3600 'synchronize-theme))

;; pair
(electric-pair-mode 1)

;; 补全
(straight-use-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1) ;; 对eglot启用snippet
(add-hook 'c++-mode-hook 'eglot-ensure) ;; 在c++中使用lsp
(add-hook 'python-mode-hook 'eglot-ensure) ;; 在python中使用lsp
(straight-use-package 'eldoc-box)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
(setq eldoc-box-max-pixel-width 200)
(setq eldoc-box-max-pixel-height 300)
(use-package eglot
  :bind
  (:map evil-normal-state-map
        ("<leader> l f" . eglot-format)))

(use-package company
  :straight t
  :defer 0.1
  :config
  (global-company-mode t)
  (setq-default
   company-idle-delay 0.05
   company-minimum-prefix-length 2
   ))

(use-package org
  :defer 10
  :straight
  (org :type git :host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev"
        :remote "tecosaur")
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq org-startup-with-inline-images t)
  (setq org-startup-with-latex-preview t)
  (setq org-image-actual-width 500)
  (setq org-confirm-babel-evaluate nil))

(use-package org-roam
  :straight t
  :after org
  :defer 21
  :bind
  (:map evil-normal-state-map
        ("<leader> r f" . org-roam-node-find)
        ("<leader> r i" . org-roam-node-insert))
  :config
  (setq org-roam-directory "~/org/roam")
  (org-roam-db-autosync-mode 1))

(use-package org-roam-ui
  :straight
  (org-roam-ui :type git :host github :repo "o8vm/org-roam-ui")
  :after org-roam
  :hook (org-roam . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t)
  ;; external functions create by o8vm
  (setq org-roam-ui-export-repo "git@github.com:welandx/welandx.github.io.git")
  (setq org-roam-ui-root-dir "~/repo/org-roam-ui"))


(straight-use-package 'prescient)
(straight-use-package 'company-prescient)
(straight-use-package 'vertico-prescient)

(company-prescient-mode 1)
(vertico-prescient-mode 1)
;; treesit
(setq treesit-extra-load-path '(expand-file-name  "~/repo/ts"))

;; input
(use-package pyim
  :straight t
  :disabled
  :defer 100
  :config
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'xiaohe-shuangpin)
  
  (setq-default pyim-punctuation-translate-p '(no))
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-program-mode))
  (require 'pyim-cstring-utils)
  (global-set-key (kbd "M-f") 'pyim-forward-word)
  (global-set-key (kbd "M-b") 'pyim-backward-word))

(use-package pyim-basedict
  :straight t
  :after pyim
  :config
  (pyim-basedict-enable))

;; (defun my-orderless-regexp (orig-func component)
;;   (let ((result (funcall orig-func component)))
;;     (pyim-cregexp-build result)))

;; (advice-add 'orderless-regexp :around #'my-orderless-regexp)

;; reading
(use-package novel-mode
  :straight
  (novel-mode :type git :host github :repo "TLINDEN/novel-mode")
  :disabled
  :config
  (setq novel-mode-hook
        (lambda ()
          (evil-local-mode)
          (evil-define-key '(normal visual) 'novel-mode
            "n" 'novel-page-down
            "p" 'novel-page-up))))
(straight-use-package 'rg)

;; utils
(straight-use-package 'markdown-mode)
(straight-use-package 'posframe)

(use-package vundo
  :straight t)

(use-package undo-tree
  :straight t
  :disabled
  :config
  (global-undo-tree-mode 1))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(straight-use-package 'rotate)
(use-package rotate
  :straight t
  :bind
  (:map evil-normal-state-map
        ("<leader> l r" . rotate-layout)))

(use-package consult
  :straight t
  :bind
  (:map evil-normal-state-map
        ("<leader> c f" . consult-find)
        ("<leader> c l" . consult-line)))

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



(use-package org-download
  :straight t
  :hook (org-mode . org-download-enable)
  :config
  (setq-default org-download-image-dir "./images")
  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-method 'directory)
  :bind
  (:map evil-normal-state-map
        ("<leader> d c" . org-download-clipboard)
        ("<leader> d s" . org-download-screenshot)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(use-package telega
  :straight t
  :defer 100
  :bind
  (:map evil-normal-state-map
        ("<leader> t" . telega))
  (:map telega-chat-mode-map
        ("C-c C-t" . telega-sticker-choose-favorite-or-recent))
  :config
  (telega-mode-line-mode 1))


(use-package quickrun
  :straight t
  :defer 100
  :bind
  (:map evil-normal-state-map
        ("<leader> r r" . quickrun)))

(use-package embark
  :straight t
  :bind
  ("C-;" . embark-act)
  ("C-," . embark-dwim))

;;;###autoload
(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (cl-remove-if
                           (lambda (window)
                             (window-parameter window 'no-delete-other-windows))
                           (window-list))))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))
(eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "<leader>o") 'toggle-maximize-buffer))

;; (use-package jupyter
;;   :straight t
;;   :defer 6
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((emacs-lisp . t)
;;      (julia . t)
;;      (python . t)
;;      (jupyter . t)))
;;   (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
;;                                                        (:session . "py")
;;                                                        (:kernel . "python3"))))

;; (use-package ob-async
;;   :straight t
;;   :defer 6
;;   :config
;;   (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia")))


;; (use-package auctex
;;   :straight t
;;   :defer 6)

(use-package cdlatex
  :straight t
  :defer 6
  :hook (LaTeX-mode . turn-on-cdlatex))


(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(setq default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)


(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s ")
  (lazy-highlight-cleanup nil))

;; lazy load openai gpt
(add-to-list 'load-path "~/.emacs.d/site-lisp/mind-wave")
(run-with-idle-timer 10 nil
                     (lambda ()
                       (require 'mind-wave)))

(use-package iscroll
  :disabled
  :straight t
  :hook
  (org-mode . iscroll-mode)
  :after org)


(use-package olivetti
  :straight t
  :hook
  (org-mode . olivetti-mode)
  :after org)

(use-package emms
  :straight t
  :defer 200
  :bind
  (:map evil-normal-state-map
        ("<leader> e p" . emms-pause)
        ("<leader> e s" . emms-stop)
        ("<leader> e n" . emms-next)
        ("<leader> e b" . emms-previous)
        ("<leader> e r" . emms-random)
        ("<leader> e l" . emms-playlist-mode-go)
        ("<leader> e f" . emms-play-directory-tree)
        ("<leader> e a" . emms-add-directory-tree)
        ("<leader> e e" . emms-play-file)
        ("<leader> e d" . emms-play-directory)
        ("<leader> e c" . emms-play-playlist)
        ("<leader> e m" . emms-add-playlist)
        ("<leader> e t" . emms-play-playlist-tree)
        ("<leader> e u" . emms-add-playlist-tree)
        ("<leader> e i" . emms-playlist-mode-go)
        ("<leader> e o" . emms)
        ("<leader> e v" . emms-volume-mode-go))
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-vlc)
        emms-info-functions '(emms-info-native))
  (setq emms-source-file-default-directory "~/Music/"))

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

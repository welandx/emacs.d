(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'benchmark-init)
(benchmark-init/activate)

(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Happy hacking, Emacs user!")
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; 设置英文字体
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((string-equal system-type "darwin") ; macOS
  (when (member "Menlo" (font-family-list))
    (set-frame-font "Menlo" t t)))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Cascadia Code" (font-family-list))
    (set-frame-font "Cascadia Code-14" t t))))


;; 设置中文字体
(set-fontset-font
 t
 'han
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
    ((member "Microsoft JhengHei" (font-family-list)) "Microsoft JhengHei")
    ((member "SimHei" (font-family-list)) "SimHei")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Hei" (font-family-list)) "Hei")
    ((member "Heiti SC" (font-family-list)) "Heiti SC")
    ((member "Heiti TC" (font-family-list)) "Heiti TC")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")))))


(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(set-window-scroll-bars (minibuffer-window) nil nil)

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

(add-to-list 'custom-theme-load-path "~/.emacs.d/straight/repos/everforest-emacs/")
(setq day-theme-list '( modus-operandi-tinted ef-day ef-spring ef-summer ef-frost ef-light ef-cyprus ef-duo-light ef-trio-light ef-tritanopia-light ef-deuteranopia-light))
(setq dark-theme-list '( modus-vivendi-tinted ef-night ef-autumn ef-winter ef-bio ef-cherie ef-duo-dark ef-trio-dark ef-tritanopia-dark ef-deuteranopia-dark))
(setq day-theme-list '(modus-operandi-tinted everforest-hard-light))
(setq dark-theme-list '(modus-vivendi-tinted everforest-hard-dark))

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
;;(add-hook 'c++-mode-hook 'eglot-ensure) ;; 在c++中使用lsp
(add-hook 'python-mode-hook 'eglot-ensure) ;; 在python中使用lsp
(straight-use-package 'eldoc-box)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
(setq eldoc-box-max-pixel-width 200)
(setq eldoc-box-max-pixel-height 300)
(use-package eglot
  :bind
  (:map evil-normal-state-map
        ("<leader> l f" . eglot-format)))

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
(straight-use-package 'vertico-prescient)

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


 (use-package cdlatex
   :straight t
   :defer 6
   :hook (LaTeX-mode . turn-on-cdlatex))


 (use-package exec-path-from-shell
   :straight t
   :config
   (exec-path-from-shell-initialize))


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



 (use-package rime
   :straight t
   :custom
   (default-input-method "rime")
   (rime-show-candidate 'posframe)
   :config
   (setq rime-user-data-dir "~/.config/fcitx5/rime/")
   ;; 临时英文断言
   (setq rime-disable-predicates
         '(rime-predicate-evil-mode-p
           rime-predicate-current-input-punctuation-p
           rime-predicate-space-after-cc-p
           rime-predicate-current-uppercase-letter-p
           rime-predicate-tex-math-or-command-p
           rime-predicate-after-alphabet-char-p
           rime-predicate-prog-in-code-p)))

;; ;; (add-to-list 'load-path "~/repo/emacs-chinese-word-segmentation")
;; ;; (setq cns-prog "~/repo/emacs-chinese-word-segmentation/cnws")
;; ;; (setq cns-dict-directory "~/repo/emacs-chinese-word-segmentation/cppjieba/dict/")
;; ;; (setq cns-recent-segmentation-limit 20)
;; ;; (setq cns-debug nil) ; disable debug output, default is t
;; ;; (require 'cns nil t)
;; ;; (when (featurep 'cns)
;; ;;   (add-hook 'find-file-hook 'cns-auto-enable))
;; ;; (global-cns-mode)


;; ;; (evil-define-key 'normal 'global (kbd "e") 'cns-forward-word)
;; ;; (evil-define-key 'normal 'global (kbd "b") 'cns-backward-word)

;; (use-package cnhl
;;   :disabled
;;   :load-path "~/.emacs.d/site-lisp/cnhl"
;;   :hook
;;   (org-mode . cnhl-mode)
;;   (org-mode . cnhl-hl-buffer)
;;   (telega-chat-mode . cnhl-mode)
;;   :config
;;   (setq cnhl-nlp-selected "thulac")
;;   (cnhl-use-dependency 'hl)
;;   (setq cnhl-thulac-module-path
;;         (expand-file-name "~/repo/thulac/models/"))

;;   ;; color
;;   (defun cnhl-light-theme ()
;;     (interactive)
;;     (set-face-foreground 'cnhl-face-1 "#8F0000")
;;     (set-face-foreground 'cnhl-face-2 "#384034")
;;     (set-face-foreground 'cnhl-face-3 "#294080")
;;     (set-face-foreground 'cnhl-face-4 "#6D4046")
;;     (set-face-foreground 'cnhl-face-5 "#3F4020")
;;     (set-face-foreground 'cnhl-face-6 "#841F4F")
;;     (set-face-foreground 'cnhl-face-7 "gray15"))

;;   (defun cnhl-dark ()
;;     (interactive)
;;     (set-face-foreground 'cnhl-face-1 "#FFB6B0")
;;     (set-face-foreground 'cnhl-face-2 "#98FB98")
;;     (set-face-foreground 'cnhl-face-3 "#87CEFA")
;;     (set-face-foreground 'cnhl-face-4 "#FFD700")
;;     (set-face-foreground 'cnhl-face-5 "#FFA500")
;;     (set-face-foreground 'cnhl-face-6 "#FFC0CB")
;;     (set-face-foreground 'cnhl-face-7 "gray80"))

;;   (defun cnhl-theme ()
;;     (setq hour
;;           (string-to-number
;;            (substring (current-time-string) 11 13)))
;;     (if (member hour (number-sequence 6 18))
;;         (progn
;;           (cnhl-light-theme)
;;           )
;;       (cnhl-dark)
;;       ))
;;   (cnhl-theme)
;;   (run-with-timer 0 3600 'cnhl-theme)
;;   (advice-add 'telega-chatbuf-newline-or-input-send :after #'cnhl-hl-buffer))


 (use-package rainbow-mode
   :straight t
   :defer t)

 (use-package pdf-tools
   :straight t
   :defer t
   :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
   :config
   (pdf-tools-install)
   (pdf-loader-install))

 (use-package keycast
   :straight t
   :defer 99)


 (use-package dashboard
   :straight t)

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
  (telega-notifications-mode 1)
  (telega-mode-line-mode 1))

(use-package magit
  :straight t)

(provide 'myinit)

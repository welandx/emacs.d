
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


(use-package meow
  :straight t
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  :config

  (meow-setup)
  (meow-global-mode 1))


(use-package recentf
  :after meow
  :bind
  ("C-c " . recentf)
  :init
  (recentf-mode 1))


;; 设置英文字体
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((string-equal system-type "darwin") ; macOS
  (when (member "Menlo" (font-family-list))
    (set-frame-font "Menlo" t t)))
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "Hack" (font-family-list))
    (set-frame-font "Hack-12" t t))))


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


;; default settings
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(set-window-scroll-bars (minibuffer-window) nil nil)
(pixel-scroll-precision-mode 1)
(straight-use-package 'ef-themes)

;; 亮色主题和暗色主题
(setq day-theme-list '( modus-operandi ef-day ef-spring ef-summer ef-frost ef-light ef-cyprus ef-duo-light ef-trio-light ef-tritanopia-light ef-deuteranopia-light))
(setq dark-theme-list '( modus-vivendi ef-night ef-autumn ef-winter ef-bio ef-cherie ef-duo-dark ef-trio-dark ef-tritanopia-dark ef-deuteranopia-dark))

;; 随机选取主题
(defun my-random-element (my-list)
  "Return a random element from MY-LIST."
  (let ((my-length (length my-list))
        (my-random-index (random (length my-list))))
    (nth my-random-index my-list)))

;; 根据时间选择亮/暗主题
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
;;(require 'eldoc-box)
;;(add-hook 'eglot-muse-package
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
(straight-use-package 'pyim)
(straight-use-package 'pyim-basedict)
(pyim-basedict-enable)   ;; 拼音词库
(setq default-input-method "pyim")
(setq pyim-default-scheme 'xiaohe-shuangpin) ;; 使用小鹤双拼
(setq-default pyim-punctuation-translate-p '(no))
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-program-mode))
(require 'pyim-cstring-utils)
(global-set-key (kbd "M-f") 'pyim-forward-word)
(global-set-key (kbd "M-b") 'pyim-backward-word)

;; (defun my-orderless-regexp (orig-func component)
;;   (let ((result (funcall orig-func component)))
;;     (pyim-cregexp-build result)))

;; (advice-add 'orderless-regexp :around #'my-orderless-regexp)

;; reading
(straight-use-package
 '(novel-mode :type git :host github :repo "TLINDEN/novel-mode"))
(require 'novel-mode)
(evil-define-key '(normal visual) 'novel-mode
  "n" 'novel-page-down
  "p" 'novel-page-up) ;; not work: novel-mode is a minor mode
;; (defun weland/nov-tog ()
;;   (interactive)
;;   (evil-local-mode))
;; (advice-add 'novel-toggle :before #'weland/nov-tog)
(straight-use-package 'rg)

;; utils
(straight-use-package 'markdown-mode)
(straight-use-package 'posframe)

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode 1))
(straight-use-package 'evil-surround)
(global-evil-surround-mode 1)

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
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :defer 1
  :hook
  (prog-mode . copilot-mode)
  :bind
  (("C-TAB" . 'copilot-accept-completion-by-word)
   ("C-<tab>" . 'copilot-accept-completion-by-word)
   :map copilot-completion-map
   ("<tab>" . 'copilot-accept-completion)
   ("TAB" . 'copilot-accept-completion)))
;; you can utilize :map :hook and :config to customize copilot

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(use-package cnfonts
  :straight t
  :config
  (cnfonts-mode 1))

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
  :defer 10
  :config
  (evil-set-initial-state 'telega-mode 'normal))



(use-package quickrun
  :straight t
  :bind
  (:map evil-normal-state-map
        ("<leader> r r" . quickrun)))

(use-package embark
  :straight t
  :bind
  ("C-." . embark-act)
  ("C-," . embark-dwim))



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

(use-package jupyter
  :straight t
  :defer 6
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (julia . t)
     (python . t)
     (jupyter . t)))
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")
                                                       (:kernel . "python3"))))

(use-package ob-async
  :straight t
  :defer 6
  :config
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia")))


(use-package auctex
  :straight t
  :defer 6)

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
(setq c-basic-offset 4)

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
  :straight t
  :hook
  (org-mode . iscroll-mode)
  :after org)


(use-package olivetti
  :straight t
  :hook
  (org-mode . olivetti-mode)
  :after org)

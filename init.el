  ;;; -*- lexical-binding: t -*-


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; 我觉得有必要说一下dump和没有dump的逻辑关系。                                                                           ;;
     ;; 下面的注释中提到的dump，是指当前你启动的emacs加载的是通过dump.sh生成的pdmp文件加载的 。                                       ;;
     ;; 而注释中提到的非dump配置是指你不以pdmp文件加载emacs，也就是正常启动emacs。                                                  ;;
     ;; 这样区分的好处是可以让你想用pdmp时就用，不想用时可以正常用Emacs，但是我推荐你这样做，不然可能会出现一些问题。                          ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ;; dump后load-path值，当这个变量为nil时是不会加载pdmp的。
     (defvar +dumped-load-path nil
       "Not nil when using dump.")

     ;; 通过dump启动时的配置
     (when +dumped-load-path
       ;;恢复load-path
       (setq load-path +dumped-load-path)
       ;; 修改一下报错等级，这个读者按心意加，不影响dump
       (setq warning-minimum-level :emergency)
       ;; 一些功能失常的mode，需要重新开启
       (global-font-lock-mode t)
       (transient-mark-mode t))
 (unless +dumped-load-path
       ;; 下面的内容于dump无关，这里的内容只需要做你原本init.el内做的事情就够了。
   (add-to-list 'load-path "~/.emacs.d/lisp")
   (require 'myinit)


   )

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
;; set font for symbols
(set-fontset-font
 t
 'symbol
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Apple Symbols" (font-family-list)) "Apple Symbols")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "Symbola" (font-family-list)) "Symbola")))))

(progn
  ;; set font for emoji (if before emacs 28, should come after setting symbols. emacs 28 now has 'emoji . before, emoji is part of 'symbol)
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
       '(#x1f300 . #x1fad0)
     'emoji
     )
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))

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





;;lazy load openai gpt
(add-to-list 'load-path "~/.emacs.d/site-lisp/mind-wave")
 (run-with-idle-timer 10 nil
                     (lambda ()
                       (require 'mind-wave)))


;;(dashboard-open)

(use-package gcmh
  :straight t
  :config
  (gcmh-mode 1))

(run-with-idle-timer 2 nil
                     (lambda ()
                       (require 'init-company)))

(use-package org
  :bind
  (:map evil-normal-state-map
        ("<leader> a" . org-agenda))
  :config
  (setq org-agenda-files '("~/org" "~/org/roam/daily"))
  (setq org-todo-keywords '
    ((sequence "TODO(t)"
               "|"
               "DONE(d/!)" "ABORT(a@/!)")))
  (setq org-todo-keyword-faces
      '(
        ("ABORT" . (:foreground "grey" :weight bold)))))


(use-package auto-save
  :straight
  (:host github :repo "manateelazycat/auto-save")
  :config
  (auto-save-enable)

  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

  ;; custom predicates if you don't want auto save.
  ;; disable auto save mode when current filetype is an gpg file.
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p
             "gpg"
             (file-name-extension (buffer-name)) t)))))



(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)
(use-package lsp-treemacs
  :straight t
  :after treemacs
  :commands lsp-treemacs-errors-list)

    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
    (add-to-list 'major-mode-remap-alist
                 '(c-or-c++-mode . c-or-c++-ts-mode))

(require 'treesit-langs)

(setq word-wrap-by-category t)

(use-package magit
  :straight t)

;;; package --- Summary
;;; Commentary:
;;; Code:

(package-initialize)
;; (setq-default initial-major-mode 'fundamental-mode)

(setq mac-option-modifier  'meta)
(setq mac-command-modifier 'hyper)

(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper q)] 'save-buffers-kill-terminal)

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 0)

(column-number-mode t)
(line-number-mode t)

;; (setq inhibit-splash-screen t)
(fset 'yes-or-no-p'y-or-n-p)
(show-paren-mode t)
(global-auto-revert-mode t)
(setq auto-save-default nil)
(set-frame-font "Fira Code-20")
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(load-theme 'solarized-light t)

(setq scroll-margin 3
      scroll-preserve-screen-position t)

(global-hl-line-mode t)
(which-function-mode)

(setq python-shell-interpreter "python")
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq c-default-style "linux")
(setq default-tab-width 4)

(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)

(use-package diminish
  :diminish (visual-line-mode . "ω")
  :diminish hs-minor-mode
  :diminish abbrev-mode)

(use-package subword
  :diminish superword-mode
  :config (global-superword-mode t))

(use-package ace-window
    :defer 1
    :bind* ("M-o" . ace-window)
    :config
    (set-face-attribute
     'aw-leading-char-face nil
     :foreground "deep sky blue"
     :weight 'bold
     :height 3.0)
    ;; (set-face-attribute
    ;;  'aw-mode-line-face nil
    ;;  :inherit 'mode-line-buffer-id
    ;;  :foreground "lawn green")
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
          aw-dispatch-always t
          aw-dispatch-alist
          '((?x aw-delete-window "Ace - Delete Window")
            (?c aw-swap-window "Ace - Swap Window")
            (?n aw-flip-window)
            (?v aw-split-window-vert "Ace - Split Vert Window")
            (?h aw-split-window-horz "Ace - Split Horz Window")
            (?m delete-other-windows "Ace - Maximize Window")
            (?g delete-other-windows)
            (?b balance-windows)
            (?u (lambda ()
                  (progn
                    (winner-undo)
                    (setq this-command 'winner-undo))))
            (?r winner-redo)))
    (ace-window-display-mode t))

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-x c f" . counsel-find-file)
         ("C-x b"   . counsel-ibuffer)
         ("C-x c b" . counsel-bookmark)))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package ivy
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package avy
  :bind (("M-c" . avy-goto-char)
         ("M-s" . avy-goto-word-1)))


(use-package eglot
  :hook (rust-mode . eglot-ensure)
  :hook (python-mode . eglot-ensure))

(use-package flymake
  :hook (prog-mode . flymake-mode))

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook  (flymake-mode . flymake-diagnostic-at-point-mode)
  :config)

(use-package company
  :hook (prog-mode . company-mode)
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-require-match 'never))

(use-package undo-tree
  :defer 1
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff nil)))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t)
  :bind
  ("C-c C-k" . rust-run))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (ace-window solarized-theme wgrep smex counsel avy magit racket-mode sudo-edit use-package org diminish eglot flymake-diagnostic-at-point rust-mode undo-tree company))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here

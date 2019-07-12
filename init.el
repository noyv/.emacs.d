;;; package --- Summary
;;; Commentary:
;;; Code:

(package-initialize)
;; (setq-default initial-major-mode 'fundamental-mode)

(setq mac-option-modifier  'meta)
(setq mac-command-modifier 'hyper)
(global-set-key [(hyper q)] 'save-buffers-kill-terminal)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 0)

(column-number-mode t)
(line-number-mode t)
(show-paren-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)

(fset 'yes-or-no-p'y-or-n-p)

(set-frame-font "Fira Code-20")
(load-theme 'sanityinc-solarized-light t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq auto-save-default nil)
(setq scroll-margin 3
      scroll-preserve-screen-position t)

(setq-default python-shell-interpreter "python"
              indent-tabs-mode nil
              c-basic-offset 4
              c-default-style "linux"
              default-tab-width 4)

(setq-default package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)

(use-package subword
  :diminish superword-mode
  :config (global-superword-mode t))

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-x c f" . counsel-find-file)
         ("C-x b"   . counsel-switch-buffer)))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package ivy
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package avy
  :bind (("M-c" . avy-goto-char)
         ("M-s" . avy-goto-word-1)))

(use-package eglot
  :hook ((rust-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (c-mode . eglot-ensure)))

(use-package flymake
  :hook (prog-mode . flymake-mode))

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

(use-package recentf
  :config
  (recentf-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (eglot hungry-delete yasnippet-snippets color-theme-sanityinc-solarized wgrep smex counsel avy sudo-edit use-package diminish rust-mode undo-tree company))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here

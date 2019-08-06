;;; package --- Summary
;;; Commentary:
;;; Code:

(package-initialize)
(setq-default package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/snails"))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 0)
(column-number-mode t)
(line-number-mode t)
(set-frame-font "Fira Code-20")
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(load-theme 'eziam-light t)
(set-background-color "#c9cfcf")
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)

(show-paren-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)
(fset 'yes-or-no-p'y-or-n-p)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq auto-save-default nil)
(setq scroll-margin 3
      scroll-preserve-screen-position t)

(setq-default indent-tabs-mode nil
              c-basic-offset 4
              c-default-style "linux"
              default-tab-width 4)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(require 'ivy)
(setq ivy-initial-inputs-alist nil)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "")
(bind-key* "M-x"     'counsel-M-x)
(bind-key* "C-x b"   'counsel-switch-buffer)
(bind-key* "C-x C-f" 'counsel-find-file)
(bind-key* "C-c c"   'counsel-compile)
(bind-key* "C-s"     'swiper)
(bind-key* "C-c C-r" 'ivy-resume)

(require 'avy)
(bind-key* "M-c" 'avy-goto-char)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 20)

(require 'undo-tree)
(add-hook 'after-init-hook 'global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff nil)

(require 'company)
(setq company-show-numbers t)
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 20)

(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

(require 'diminish)
(eval-after-load 'undo-tree
  '(diminish 'undo-tree-mode))
(eval-after-load 'company
  '(diminish 'company-mode))
(eval-after-load 'yasnippet
  '(diminish 'yas-minor-mode))
(eval-after-load 'hungry-delete
  '(diminish 'hungry-delete-mode))
(eval-after-load 'subword
  '(diminish 'subword-mode))
(diminish 'eldoc-mode)

;; common settings for prog mode
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'hungry-delete-mode)

;; some setting for rust-mode


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (symbol-overlay yasnippet-snippets undo-tree smex rust-mode magit hungry-delete eziam-theme exec-path-from-shell diminish counsel company-tabnine bind-key avy))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
(provide 'init)
;;; init.el ends here

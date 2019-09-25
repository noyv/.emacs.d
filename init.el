;;; package --- Summary
;;; Commentary:
;;; Code:

(package-initialize)

(setq package-archives '(("local"   . "~/.emacs.d/elpa-local/")))

(menu-bar-mode -1)
(column-number-mode t)
(line-number-mode t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; (tool-bar-mode -1)
;; (scroll-bar-mode 0)
;; (load-theme 'eziam-light t)
;; (set-background-color "#c9cfcf")
;; (set-frame-font "Fira Code-20")
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

(show-paren-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)
(fset 'yes-or-no-p'y-or-n-p)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq auto-save-default nil)
(setq scroll-margin 3
      scroll-preserve-screen-position t)

(setq dired-listing-switches "-alGgh --group-directories-first --time-style \"+%m-%d %H:%M\"")
(setq directory-listing-before-filename-regexp
      (purecopy (concat "\\([0-2][0-9]:[0-5][0-9] \\)\\|"
			directory-listing-before-filename-regexp)))

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
(add-to-list 'recentf-exclude "\\.el\\'")

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
(setq company-backends (delete 'company-clang company-backends))

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

(require 'eglot)
(add-to-list 'eglot-server-programs
	     '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)

;; common settings for prog mode
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'hungry-delete-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit company fd-dired eglot counsel ivy smex swiper symbol-overlay yasnippet hungry-delete avy diminish cmake-mode undo-tree bind-key)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
(provide 'init)
;;; init.el ends here

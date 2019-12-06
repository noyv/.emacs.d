;;; package --- Summary
;;; Commentary:
;;; Code:

(package-initialize)

(setq package-archives '(("local"   . "~/.emacs.d/elpa-local/")))

(menu-bar-mode -1)
(column-number-mode t)
(line-number-mode t)
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)

(tool-bar-mode -1)
(scroll-bar-mode 0)
(load-theme 'sanityinc-solarized-light t)
(set-frame-font "hack-12")

(setq make-backup-files nil)
(global-auto-revert-mode t)
(fset 'yes-or-no-p'y-or-n-p)

(show-paren-mode t)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position 't)

(defun my-c-mode-hook ()
  (c-toggle-auto-hungry-state 1)
  (c-toggle-comment-style -1))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(require 'which-key)
(which-key-mode t)

(require 'general)
(general-define-key
  :states '(normal insert emacs) 
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :keymaps 'override
  "bb"    'counsel-buffer-or-recentf
  "bl"     (lambda () (interactive) (switch-to-buffer nil))
  "bk"    'kill-buffer
  "ee"    'flymake-goto-next-error
  "ff"    'counsel-find-file
  "fr"    'counsel-recentf
  "rr"    'ivy-resume
  "ss"    'swiper-isearch
  "v"      vc-prefix-map
  "<SPC>" 'counsel-M-x)

(require 'evil)
(evil-mode 1)
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

(evil-define-key '(normal motion) global-map "s" #'avy-goto-char-2)
(evil-define-key 'normal global-map "gc" 'comment-line)

(add-to-list 'evil-buffer-regexps '("*Packages*" . normal))
(evil-define-key 'normal package-menu-mode-map (kbd "i") #'package-menu-mark-install)
(evil-define-key 'normal package-menu-mode-map (kbd "d") #'package-menu-mark-delete)
(evil-define-key 'normal package-menu-mode-map (kbd "u") #'package-menu-mark-unmark)
(evil-define-key 'normal package-menu-mode-map (kbd "x") #'package-menu-execute)

(require 'keyfreq)
(setq keyfreq-excluded-commands
      '(self-insert-command
	abort-recursive-edit
	forward-char
	backward-char
	previous-line
	next-line))
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)
(setq recentf-max-saved-items 40)
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
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)
(diminish 'which-key-mode)

(require 'eglot)
(add-to-list 'eglot-server-programs
	     '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(setq-default flymake-diagnostic-functions nil)

;; common settings for prog mode
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(evil-escape-mode t)
 '(package-selected-packages
   '(smex avy counsel ivy which-key color-theme-sanityinc-solarized keyfreq evil-leader evil company eglot yasnippet diminish undo-tree general))
 '(pdf-view-midnight-colors '("#eeeeee" . "#000000")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
(provide 'init)
;;; init.el ends here

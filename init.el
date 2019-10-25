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

(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(show-paren-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)
(fset 'yes-or-no-p'y-or-n-p)
(setq make-backup-files nil)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq auto-save-default nil)
(setq scroll-margin 3
      scroll-preserve-screen-position t)

(setq dired-listing-switches "-alGgh --group-directories-first --time-style \"+%m-%d %H:%M\"")
(setq directory-listing-before-filename-regexp
      (purecopy (concat "\\([0-2][0-9]:[0-5][0-9] \\)\\|"
			directory-listing-before-filename-regexp)))

(unbind-key "C-s")

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
 "ff" 'counsel-find-file
 "fr" 'counsel-recentf
 "bb" 'counsel-switch-buffer
 "bk" 'kill-buffer
 "ci" 'evilnc-comment-or-uncomment-lines
 "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
 "cc" 'evilnc-copy-and-comment-lines
 "cp" 'evilnc-comment-or-uncomment-paragraphs
 "cr" 'comment-or-uncomment-region
 "cv" 'evilnc-toggle-invert-comment-line-by-line
 "."  'evilnc-copy-and-comment-operator)
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "gnus-.*-mode"))

(require 'evil)
(evil-mode 1)

(modify-syntax-entry ?_ "w")
(define-key evil-normal-state-map (kbd "*")
  (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'symbol)))))
(define-key evil-normal-state-map (kbd "#")
      (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'word)))))

(with-eval-after-load 'evil
    (require 'evil-anzu))
(global-anzu-mode +1)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(require 'ivy)
(setq ivy-initial-inputs-alist nil)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "")
(bind-key* "M-x"     'counsel-M-x)

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
(diminish 'abbrev-mode)

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
   '(evil-nerd-commenter evil-anzu anzu keyfreq evil-leader evil company fd-dired eglot counsel ivy smex yasnippet hungry-delete diminish undo-tree bind-key)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
(provide 'init)
;;; init.el ends here

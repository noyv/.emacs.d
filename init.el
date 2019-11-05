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

(tool-bar-mode -1)
(scroll-bar-mode 0)
(load-theme 'sanityinc-solarized-light t)
(set-frame-font "Fira Code-16")
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(show-paren-mode t)
(global-auto-revert-mode t)
(global-hl-line-mode t)
(global-whitespace-mode)
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
(unbind-key "C-x C-c")

(require 'which-key)
(which-key-mode)

(require 'ivy)
(setq ivy-initial-inputs-alist nil)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "")
(bind-key* "M-x" 'counsel-M-x)

(defun my-find-file ()
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'counsel-file-jump)))

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "bb" 'counsel-switch-buffer
  "bl" (lambda ()
	 (interactive)
	 (switch-to-buffer nil))
  "bk" 'kill-buffer
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "ff" 'my-find-file
  "fr" 'counsel-recentf
  "rr" 'ivy-resume
  "ss" 'counsel-grep-or-swiper
  "nn" 'narrow-to-region
  "nw" 'widen
  "v=" 'vc-diff
  "."  'evilnc-copy-and-comment-operator)
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "gnus-.*-mode"))

(require 'evil)
(evil-mode 1)
(modify-syntax-entry ?_ "w")

(require 'evil-matchit)
(global-evil-matchit-mode 1)

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
(eval-after-load 'subword
  '(diminish 'subword-mode))
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)
(diminish 'which-key-mode)
(diminish 'global-whitespace-mode)

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
 '(global-evil-matchit-mode t)
 '(package-selected-packages
   '(evil-matchit which-key color-theme-sanityinc-solarized evil-nerd-commenter keyfreq evil-leader evil company fd-dired eglot counsel ivy smex yasnippet diminish undo-tree bind-key))
 '(pdf-view-midnight-colors '("#eeeeee" . "#000000")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)


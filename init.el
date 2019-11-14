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
(set-frame-font "Fira Code-14")
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

(setq dired-listing-switches
      "-alGgh --group-directories-first --time-style \"+%m-%d %H:%M\"")
(setq directory-listing-before-filename-regexp
      (purecopy (concat "\\([0-2][0-9]:[0-5][0-9] \\)\\|"
			directory-listing-before-filename-regexp)))

(unbind-key "C-s")
(unbind-key "C-x C-c")

(require 'ivy)
(bind-key* "M-x" 'counsel-M-x)

(require 'which-key)
(which-key-mode)

(defvar mcfly-commands
  '(query-replace-regexp
    flush-lines
    keep-lines
    ivy-read
    swiper
    swiper-isearch))

(defvar mcfly-back-commands
  '(self-insert-command
    ivy-yank-char
    ivy-yank-word
    ivy-yank-symbol))

(defun mcfly-back-to-present ()
  (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
  (cond ((and (memq last-command mcfly-commands)
	      (equal (this-command-keys-vector) (kbd "M-p")))
	 ;; repeat one time to get straight to the first history item
	 (setq unread-command-events
	       (append unread-command-events
		       (listify-key-sequence (kbd "M-p")))))
	((memq this-command mcfly-back-commands)
	 (delete-region (point)
			(point-max)))))

(defun mcfly-time-travel ()
  (when (memq this-command mcfly-commands)
    (let* ((kbd (kbd "M-n"))
	   (cmd (key-binding kbd))
	   (future (and cmd
			(with-temp-buffer
			  (when (ignore-errors
				  (call-interactively cmd) t)
			    (buffer-string))))))
      (when future
	(save-excursion
	  (insert (propertize future 'face 'shadow)))
	(add-hook 'pre-command-hook 'mcfly-back-to-present nil t)))))

;; setup code
(add-hook 'minibuffer-setup-hook #'mcfly-time-travel)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "bb" 'counsel-buffer-or-recentf
  "bl" (lambda ()
	 (interactive)
	 (switch-to-buffer nil))
  "bk" 'kill-buffer
  "ej" 'flymake-goto-next-error
  "ek" 'flymake-goto-prev-error
  "ff" '(lambda()
	  (interactive)
	  (let ((current-prefix-arg 4))
	    (call-interactively 'counsel-file-jump)))
  "fr" 'counsel-recentf
  "rr" 'ivy-resume
  "ss" 'swiper-isearch
  "nn" 'narrow-to-region
  "nw" 'widen
  "v"  vc-prefix-map)
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "gnus-.*-mode"))

(ranger-override-dired-mode t)

(require 'evil)
(evil-mode 1)
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

(require 'evil-nerd-commenter)
(evilnc-default-hotkeys)

(require 'evil-surround)
(global-evil-surround-mode 1)

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
 '(global-evil-matchit-mode t)
 '(package-selected-packages
   '(evil-surround ranger counsel ivy which-key evil-matchit color-theme-sanityinc-solarized evil-nerd-commenter keyfreq evil-leader evil company eglot yasnippet diminish undo-tree bind-key))
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

;;; package --- Summary
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

;; init straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun local-graphic-config())

(defun local-terminal-config()
  (straight-use-package 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

(if (display-graphic-p)
    (local-graphic-config)
  (local-terminal-config))

;; some change to apperence
(setq-default tab-width 4
              indent-tabs-mode nil)

(setq inhibit-startup-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(setq custom-safe-themes t)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(column-number-mode t)
(global-hl-line-mode)

(add-to-list 'default-frame-alist '(font . "FiraCode-11"))
(load-theme 'modus-operandi t)

(fset 'yes-or-no-p'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(add-hook 'prog-mode-hook #'electric-pair-mode)

(savehist-mode)

(add-hook 'prog-mode-hook 'flymake-mode)

(recentf-mode t)
(add-to-list 'recentf-exclude "\\elpa")
(setq recentf-max-menu-items 32)
(setq recentf-max-saved-items 32)

(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)

(straight-use-package 'valign)
(add-hook 'org-mode-hook 'valign-mode)

(straight-use-package 'general)
(general-create-definer general-leader-def
  :states 'normal
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :prefix-command 'leader-prefix-command
  :prefix-map 'leader-prefix-map)
(general-leader-def "<SPC>" 'execute-extended-command)
(general-leader-def "="     '(:keymap vc-prefix-map :which-key "vc"))
(general-leader-def "p"     '(:keymap project-prefix-map :which-key "project"))

(straight-use-package 'sudo-edit)

(straight-use-package 'vterm)
(straight-use-package 'vterm-toggle)
(keymap-global-set "s-t" #'vterm-toggle)

(straight-use-package 'evil)
(defalias #'forward-evil-word #'forward-evil-symbol)
(setq evil-symbol-word-search t)
(setq evil-undo-system 'undo-redo)
(evil-mode 1)
(evil-set-initial-state 'vterm-mode 'emacs)
(general-def 'normal xref--xref-buffer-mode-map "RET" #'xref-goto-xref-and-quit :keymaps 'override)

(straight-use-package 'vertico)
(vertico-mode)

(straight-use-package 'orderless)
(setq completion-styles '(basic partial-completion orderless)
        completion-category-overrides '((file (styles basic partial-completion))))

(straight-use-package 'consult)
(with-eval-after-load 'consult
  (setq consult-buffer-filter '("^ " "\\*straight*"))
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  (consult-customize
   consult-line
   :add-history (seq-some #'thing-at-point '(region symbol)))
 
  (defalias 'consult-line-thing-at-point 'consult-line)

  (consult-customize
   consult-line-thing-at-point
   :initial (thing-at-point 'symbol)))

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(general-leader-def "ff" 'find-file)
(general-leader-def "fo" 'find-file-other-window)
(general-leader-def "fd" 'dired-jump)
(general-leader-def "fr" 'consult-buffer)
(general-leader-def "fe" 'consult-flymake)
(general-leader-def "fl" 'consult-line-thing-at-point)

(straight-use-package 'embark)
(keymap-global-set "M-o" 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command)

(straight-use-package 'embark-consult)
(add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)

(straight-use-package 'avy)
(general-def '(normal motion) global-map "s" #'avy-goto-char-timer)

(straight-use-package 'corfu)
(global-corfu-mode)
(setq corfu-auto t
      corfu-quit-no-match 'separator)

(straight-use-package 'yasnippet)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;; configure eglot
(straight-use-package 'eglot)
(setq-default eglot-ignored-server-capabilites '(:documentHighlightProvider))
(general-leader-def "ca" 'eglot-code-actions)
(general-leader-def "ci" 'eglot-code-action-organize-imports)
(general-leader-def "cr" 'eglot-rename)
(general-leader-def "cf" 'eglot-format)

;; when lang c/c++
(setq c-default-style "linux")
(setq c-basic-offset 4)
(defun my/c-mode-hook()
  (c-toggle-hungry-state 1)
  (c-toggle-comment-style -1))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'my/c-mode-hook)

;; when lang python
;; (add-hook 'python-mode-hook 'eglot-ensure)

;; when lang js
(add-hook 'js-mode-hook 'eglot-ensure)

;; when lang golang
(straight-use-package 'go-mode)
(add-hook 'go-mode-hook 'eglot-ensure)
(setq-default eglot-workspace-configuration
              '((gopls
                 (usePlaceholders . t))))

;; when lang rust
(straight-use-package 'rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)

;; when lang org
(straight-use-package '(org :type built-in))
(setq-default org-export-with-toc nil)
(setq-default org-export-with-section-numbers nil)
(setq-default org-html-head-include-default-style nil)
(setq-default org-startup-folded 'content)
(general-def '(normal motion) org-mode-map "TAB" #'org-cycle :keymaps 'override)

(straight-use-package '(zk :files (:defaults "zk-consult.el")))
(setq zk-directory "~/org")
(setq zk-file-extension "org")
(zk-setup-embark)
(setq zk-tag-grep-function #'zk-consult-grep-tag-search
      zk-grep-function #'zk-consult-grep)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-disable-insert-state-bindings t)
 '(evil-want-C-u-scroll t)
 '(pdf-view-midnight-colors '("#eeeeee" . "#000000")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here

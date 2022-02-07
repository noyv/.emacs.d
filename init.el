;;; package --- Summary
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

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

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; (global-auto-revert-mode t)
;; (delete-selection-mode t)

(setq-default tab-width 4
              indent-tabs-mode nil)

(setq inhibit-startup-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(setq custom-safe-themes t)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(menu-bar-mode -1)
(column-number-mode t)
(line-number-mode t)
(show-paren-mode t)
(global-hl-line-mode)

(fset 'yes-or-no-p'y-or-n-p)

(savehist-mode)

(recentf-mode t)
(add-to-list 'recentf-exclude "\\elpa")
(setq recentf-max-menu-items 32)
(setq recentf-max-saved-items 32)

(when (memq window-system '(mac ns x))
  (straight-use-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(defun local-graphic-config()
  (add-to-list 'default-frame-alist '(font . "Iosevka-18"))
  (load-theme 'modus-operandi t)
  (straight-use-package 'valign)
  (add-hook 'org-mode-hook 'valign-mode))

(defun local-terminal-config()
  (straight-use-package 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

(if (display-graphic-p)
    (local-graphic-config)
  (local-terminal-config))

(straight-use-package 'bind-key)
(straight-use-package 'sudo-edit)

(straight-use-package 'smartparens)
(add-hook 'prog-mode-hook 'smartparens-mode)

(straight-use-package 'markdown-mode)
(straight-use-package 'yaml-mode)

(straight-use-package 'general)
(general-create-definer general-leader-def
  :states '(normal insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :prefix-command 'leader-prefix-command
  :prefix-map 'leader-prefix-map)
(general-leader-def "<SPC>" 'execute-extended-command)
(general-leader-def "="     '(:keymap vc-prefix-map :which-key "vc"))
(general-leader-def "p"     '(:keymap project-prefix-map :which-key "project"))

(straight-use-package 'evil)
(defalias #'forward-evil-word #'forward-evil-symbol)
(setq evil-symbol-word-search t)
(setq evil-undo-system 'undo-redo)
(evil-mode 1)
(general-def 'normal xref--xref-buffer-mode-map "RET" #'xref-goto-xref-and-quit :keymaps 'override)

(straight-use-package 'vertico)
(vertico-mode)

(straight-use-package 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(straight-use-package 'marginalia)
(marginalia-mode t)

(straight-use-package 'consult)
(straight-use-package 'consult-flycheck)
(with-eval-after-load 'consult
  (setq consult-buffer-filter '("^ " "\\*Messages\\*" "\\*straight*"))
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-.")))
(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))
(general-leader-def "ff" 'find-file)
(general-leader-def "fr" 'consult-buffer)
(general-leader-def "fe" 'consult-flycheck)
(general-leader-def "fl" 'consult-line)

(straight-use-package 'embark)
(bind-key "M-o" 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command)

(straight-use-package 'embark-consult)
(add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)

(straight-use-package 'avy)
(general-def '(normal motion) global-map "s" #'avy-goto-char-timer)

(straight-use-package 'company)
(global-company-mode)
(setq company-show-numbers t)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 20)

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(add-hook 'prog-mode-hook 'yas-minor-mode)

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

;; configure eglot
(straight-use-package 'eglot)
(setq-default eglot-ignored-server-capabilites '(:documentHighlightProvider))
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
(setq eldoc-idle-dealy 2)
(general-leader-def "ca" 'eglot-code-actions)
(general-leader-def "ci" 'eglot-code-action-organize-imports)
(general-leader-def "cr" 'eglot-rename)
(general-leader-def "cf" 'eglot-format)

(straight-use-package 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)

;; when lang c/c++
(setq c-default-style "linux")
(setq c-basic-offset 4)
(defun my/c-mode-hook()
  (c-toggle-hungry-state 1)
  (c-toggle-comment-style -1))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'tree-sitter-hl-mode)
(add-hook 'c-mode-hook 'my/c-mode-hook)

;; when lang python
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'tree-sitter-hl-mode)

;; when lang js
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'tree-sitter-hl-mode)

;; when lang golang
(straight-use-package 'go-mode)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'tree-sitter-hl-mode)
(setq-default eglot-workspace-configuration
              '((gopls
                 (usePlaceholders . t))))

(straight-use-package 'flycheck-golangci-lint)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;; when lang rust
(straight-use-package 'rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'tree-sitter-hl-mode)

;; when lang org
(straight-use-package '(org :type built-in))
(setq-default org-export-with-toc nil)
(setq-default org-export-with-section-numbers nil)
(setq-default org-html-head-include-default-style nil)
(setq-default org-startup-folded 'content)
(general-def '(normal motion) org-mode-map "TAB" #'org-cycle :keymaps 'override)


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

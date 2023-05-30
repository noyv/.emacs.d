;;; package --- Summary
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

;; (setq force-load-messages t)
(setq read-process-output-max (* 1024 1024))

;; (require 'package)
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))

(setq package-selected-packages '())

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(when (eq (length package-selected-packages) 0)
  (package-refresh-contents)
  (defvar my-packages
    '(
      avy
      blamer
      company
      consult
      consult-flycheck
      evil
      flycheck
      general
      go-impl
      go-mode
      go-tag
      lsp-mode
      orderless
      org-roam
      plantuml-mode
      sudo-edit
      valign
      vertico
      vterm
      vterm-toggle
      yasnippet
      ))
  (mapc #'package-install my-packages))

;; (menu-bar-mode -1)
(column-number-mode t)
(global-hl-line-mode)

(setq inhibit-startup-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(add-to-list 'default-frame-alist '(font . "FiraCode-12"))
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

(fset 'yes-or-no-p'y-or-n-p)

(setq exec-path (append exec-path '("/usr/local/go/bin/" "~/go/bin/")))
(setenv "PATH" (concat "/usr/local/go/bin/" ":" "~/go/bin/" ":" (getenv "PATH")))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; (require 'elec-pair)
(add-hook 'prog-mode-hook #'electric-pair-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; (require 'recentf)
(recentf-mode t)
(add-to-list 'recentf-exclude "\\elpa")
(setq recentf-max-menu-items 128)
(setq recentf-max-saved-items 128)

;; (require 'dired)
(setq dired-recursive-deletes 'always)
(setq delete-by-moving-to-trash t)

;; (require 'isearch)
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format " [%s/%s]")

;; (require 'general)
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

;; (require 'vterm)
(keymap-global-set "s-t" #'vterm-toggle)
(general-leader-def "t" 'vterm-toggle)

;; (require 'evil)
(defalias #'forward-evil-word #'forward-evil-symbol)
(setq evil-symbol-word-search t)
(setq evil-undo-system 'undo-redo)
(setq evil-disable-insert-state-bindings t)
(setq evil-want-C-u-scroll t)
(setq evil-want-keybinding nil)
(evil-mode 1)

;; (require 'xref)
(evil-define-key 'normal xref--xref-buffer-mode-map (kbd "RET") 'xref-goto-xref)
(evil-define-key 'normal xref--xref-buffer-mode-map (kbd "C-<return>") 'xref-quit-and-goto-xref)
(evil-define-key 'normal xref--xref-buffer-mode-map "]]" 'xref-next-line)
(evil-define-key 'normal xref--xref-buffer-mode-map "[[" 'xref-prev-line)
(evil-define-key 'normal xref--xref-buffer-mode-map "o" 'xref-show-location-at-point)

;; (require 'package)
(evil-set-initial-state 'package-menu-mode 'normal)
(evil-define-key 'normal package-menu-mode-map (kbd "RET") 'package-menu-describe-package)
(evil-define-key 'normal package-menu-mode-map "i" 'package-menu-mark-install)
(evil-define-key 'normal package-menu-mode-map "U" 'package-menu-mark-upgrades)
(evil-define-key 'normal package-menu-mode-map "d" 'package-menu-mark-delete)
(evil-define-key 'normal package-menu-mode-map "gr" 'revert-buffer)
(evil-define-key 'normal package-menu-mode-map "u" 'package-menu-mark-unmark)
(evil-define-key 'normal package-menu-mode-map "x" 'package-menu-execute)

;; (require 'avy)
(evil-define-key '(normal motion) global-map "s" 'avy-goto-char-timer)

;; (require 'vertico)
(vertico-mode)
(keymap-set minibuffer-local-map "C-w" #'backward-kill-word)

(setq completion-styles '(basic partial-completion orderless))
(setq completion-category-overrides '((file (styles basic partial-completion))))

(require 'consult)
(consult-customize consult-ripgrep
                   :initial (consult--async-split-initial (thing-at-point 'symbol)))
(savehist-mode)
(general-leader-def "ff" 'find-file)
(general-leader-def "fo" 'find-file-other-window)
(general-leader-def "fd" 'dired-jump)
(general-leader-def "fr" 'consult-buffer)
(general-leader-def "fe" 'consult-flycheck)
(general-leader-def "fl" 'consult-line)

;; (require 'company)
(setq company-minimum-prefix-length 2)
(add-hook 'prog-mode-hook 'company-mode)

;; (require 'yasnippet)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;; (require 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)

;; (require 'lsp)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-log-io nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-lens-enable nil)
(setq lsp-diagnostics-disabled-modes '(sh-mode))

;; c/c++
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

;; cmake
(add-to-list 'auto-mode-alist '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))

;; typescript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-hook 'typescript-ts-base-mode-hook #'lsp-deferred)

;; yaml
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

;; golang
(setq go-test-args "-v -count=1")
(add-hook 'go-mode-hook #'lsp-deferred)

;; rust
(add-hook 'rust-ts-mode-hook #'lsp-deferred)

;; org
(setq org-startup-folded 'showeverything)
(setq org-use-sub-superscripts nil)
(general-def '(normal motion) org-mode-map "TAB" #'org-cycle :keymaps 'override)
(setq org-export-with-toc nil)
(setq org-html-head-include-default-style nil)
(setq org-id-locations-file (convert-standard-filename "~/org/.org-id-locations"))
(setq org-roam-directory (file-truename "~/org/"))
(setq org-roam-db-location "~/org/org-roam.db")
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

;; (setq plantuml-exec-mode 'server)
;; (setq plantuml-server-url "http://172.16.0.201:9000")
;; (require 'ob-plantuml)
;; (setq org-plantuml-jar-path "~/org/plantuml.jar")
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((plantuml . t))) ; this line activates plantuml

(provide 'init)
;;; init.el ends here

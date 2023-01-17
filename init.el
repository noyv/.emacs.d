;;; package --- Summary
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-

;; (setq force-load-messages t)

;; (require 'package)
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(menu-bar-mode -1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p'y-or-n-p)
(setq create-lockfiles nil)
(setq exec-path (append exec-path '("/usr/local/go/bin/" "~/go/bin/")))
(setenv "PATH" (concat "/usr/local/go/bin/" ":" "~/go/bin/" ":" (getenv "PATH")))
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(column-number-mode t)
(global-hl-line-mode)

(setq inhibit-startup-screen t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq make-backup-files nil)
(setq auto-save-default nil)

(add-to-list 'default-frame-alist '(font . "FiraCode-14"))

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

(keymap-global-set "s-t" #'vterm-toggle)

;; (require 'evil)
(defalias #'forward-evil-word #'forward-evil-symbol)
(setq evil-symbol-word-search t)
(setq evil-undo-system 'undo-redo)
(setq evil-disable-insert-state-bindings t)
(setq evil-want-C-u-scroll t)
(setq evil-want-keybinding nil)
(evil-mode 1)

;; (require 'vertico)
(vertico-mode)

(setq completion-styles '(basic partial-completion orderless))
(setq completion-category-overrides '((file (styles basic partial-completion))))

(require 'consult)
(savehist-mode)

(general-leader-def "ff" 'find-file)
(general-leader-def "fo" 'find-file-other-window)
(general-leader-def "fd" 'dired-jump)
(general-leader-def "fr" 'consult-buffer)
(general-leader-def "fe" 'consult-flycheck)
(general-leader-def "fl" 'consult-line)
(consult-customize consult-ripgrep
                   :initial (consult--async-split-initial (thing-at-point 'symbol)))

;; (require 'avy)
(general-def '(normal motion) global-map "s" #'avy-goto-char-timer)

;; (require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; (require 'yasnippet)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;; (require 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;; (require 'eglot)
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
(setq eglot-stay-out-of '(flymake))
(setq eglot-workspace-configuration
      '((gopls
         (usePlaceholders . t))))
(setq eglot-events-buffer-size 0)
(general-leader-def "ca" 'eglot-code-actions)
(general-leader-def "ci" 'eglot-code-action-organize-imports)
(general-leader-def "cr" 'eglot-rename)
(general-leader-def "cf" 'eglot-format)

;; (require 'go-mode)
(setq gofmt-command "goimports")
(setq go-test-args "-v -count=1")

;; (require 'org)
;; (require 'org-id)
;; (require 'ox)
;; (require 'org-roam)
(setq org-export-with-toc nil)
(setq org-html-head-include-default-style nil)
;; '(org-export-with-section-numbers nil)
(setq org-id-locations-file (convert-standard-filename "~/org/.org-id-locations"))
(setq org-startup-folded 'content)
(setq org-roam-directory (file-truename "~/org/"))
(setq org-roam-db-location "~/org/org-roam.db")
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
(general-def '(normal motion) org-mode-map "TAB" #'org-cycle :keymaps 'override)

;; (setq plantuml-exec-mode 'server)
;; (setq plantuml-server-url "http://172.16.0.201:9000")
;; (require 'ob-plantuml)
;; (setq org-plantuml-jar-path "~/org/plantuml.jar")
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((plantuml . t))) ; this line activates plantuml

(provide 'init)
;;; init.el ends here


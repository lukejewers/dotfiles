;;;; startup ;;;;
;; minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda () (message "Emacs loaded in %s with %d garbage collections."
                              (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done)))

;;; disable ;;;
(setq package-enable-at-startup nil
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message ";; *scratch*"
      ring-bell-function 'ignore
      warning-minimum-level :error)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom-vars.el")

;;; initialize package sources ;;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; initialize use-package
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t
      comp-async-report-warnings-errors nil
      comp-deferred-compilation t
      package-install-upgrade-built-in t)

;;;; appearance ;;;;
;; theme
(use-package gruber-darker-theme
  :config (load-theme 'gruber-darker t))

(setq custom-safe-themes t)
(set-frame-font "Iosevka 18" nil t)
(setq-default display-line-numbers 'nil
              frame-title-format '("%f")
              ns-use-proxy-icon nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; mode line
(use-package minions
  :config
  (minions-mode 1))

;;;; defaults ;;;;
(setq mac-left-control-modifier 'control
      mac-left-option-modifier 'super
      mac-right-control-modifier 'meta
      mac-right-option-modifier 'meta)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(setq-default electric-pair-preserve-balance nil
              vc-follow-symlinks t
              eldoc-echo-area-use-multiline-p nil
              indent-tabs-mode nil
              make-backup-files nil
              auto-save-default nil
              create-lockfiles nil
              resize-mini-windows nil)
(setq compilation-scroll-output 'first-error
      compilation-always-kill t)
(setq set-mark-command-repeat-pop t)
(setq xref-search-program 'ripgrep) ; project-find-regexp

;; global unset keys
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x C-c"))

;; global set keys
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
(global-set-key (kbd "C-M-8") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-9") 'enlarge-window)
(global-set-key (kbd "C-M-0") 'shrink-window)
(global-set-key (kbd "C-M--") 'shrink-window-horizontally)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-q") 'query-replace-regexp)

;; allow hash to be entered
(global-set-key (kbd "M-3")
                '(lambda ()
                   (interactive)
                   (insert "#")))

(global-set-key (kbd "M-s g") 'grep)
(setq grep-command "rg -nS --no-heading "
      grep-use-null-device nil)

(use-package wgrep
  :ensure t)

(use-package org
  :config (setq org-log-done t))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org")
  (org-roam-completion-everywhere t)
  :bind (("C-c o l" . org-roam-buffer-toggle)
         ("C-c o f" . org-roam-node-find)
         ("C-c o i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook (dired-mode . auto-revert-mode)
  :config (setq dired-kill-when-opening-new-dired-buffer t)
          (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("-" . dired-up-directory)))

(use-package dired-ranger
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle))
  :config (setq dired-subtree-use-backgrounds nil))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . hl-line-mode)
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil))

(defun close-all-but-current-buffer ()
  (interactive)
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(use-package whitespace
  :hook (before-save . whitespace-cleanup)
  :config (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style
        '(face trailing tabs indentation::space empty indention spaces trailing space-mark space-after-tab space-before-tab tab-mark)))

(use-package avy
  :ensure t
  :init (setq avy-timeout-seconds 0.3)
  :bind ("M-j" . avy-goto-char-timer))

(use-package transpose-frame
  :ensure t
  :bind ("C-x t f" . transpose-frame)
  ("C-x t r" . rotate-frame))

;;; text editing
(use-package move-text
  :bind ("M-p" . 'move-text-up)
  ("M-n" . 'move-text-down))

(use-package expand-region
  :bind ("C-=" . er/expand-region)
        ("C--" . er/contract-region))

(use-package multiple-cursors
  :ensure t
  :bind  ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-'" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines))

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))
(global-set-key (kbd "C-;") 'select-current-line)

(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))
(global-set-key (kbd "C-,") 'duplicate-line)

(defun copy-full-path-to-kill-ring ()
  "Copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))
(global-set-key (kbd "C-±") 'copy-full-path-to-kill-ring)

(use-package paredit
  :ensure t
  :hook
  (emacs-lisp-mode . enable-paredit-mode)
  (eval-expression-minibuffer-setup . enable-paredit-mode)
  (lisp-mode . enable-paredit-mode)
  (lisp-interaction-mode . enable-paredit-mode)
  (clojure-mode . enable-paredit-mode))

;;;; terminals & shells ;;;;;
(use-package exec-path-from-shell
   :config (exec-path-from-shell-initialize))

(use-package vterm
  :ensure t
  :bind ("M-`" . vterm)
        (:map vterm-mode-map
              ("C-y" . vterm-yank))
  :config
  (setq vterm-max-scrollback 10000))

(add-to-list 'display-buffer-alist
             '("*shell" (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.45)))

(defun toggle-shell (shell-str shell)
  "Close the current buffer if it is open, otherwise open a new one!"
  (interactive)
  (if (string-equal shell-str major-mode)
      (bury-buffer)
    (funcall shell)))

(defun shell-toggle () (interactive) (toggle-shell "shell-mode" 'shell))
(global-set-key (kbd "C-`") 'shell-toggle)

;;;; completion ;;;;
(use-package ido
  :config
  (ido-mode 1)
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-url-at-point nil)
  (setq ido-max-window-height 1))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :bind ("M-x" . smex))

(use-package company
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  :hook (after-init . global-company-mode))

;;;; version control ;;;;
(use-package magit
  :bind
  (("C-c m s" . magit-status)
   ("C-c m l" . magit-log)
   ("C-c m b" . magit-blame)))

;;;; languages ;;;;
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :init
  (fset #'jsonrpc--log-event #'ignore)
  :hook
  (js-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  ;; (c-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (lua-mode . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c l f" . eglot-format)
        ("C-c l r" . eglot-rename))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-ignored-server-capabilities '(:inlayHintProvider
                                       :documentHighlightProvider)))

(use-package flymake
  :bind (("C-c e n" . flymake-goto-next-error)
         ("C-c e p" . flymake-goto-prev-error)
         ("C-c e b" . flymake-show-buffer-diagnostics)))

(setq python-shell-interpreter "ipython")
(setq python-shell-completion-native-enable nil)

;;; code formatting
(setq c-default-style "linux"
      c-basic-offset 4)

(use-package blacken)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u1")))

;;; garbage collection ;;;
(setq gc-cons-threshold (* 1024 1024 100))

;;; .emacs ends here

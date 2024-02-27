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
(unless package-archive-contents (package-refresh-contents))

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
(load-theme 'gruber-darker t)
(setq custom-safe-themes t)
(set-frame-font "Iosevka 18" nil t)
(setq-default display-line-numbers 'relative
              frame-title-format '("%f")
              ns-use-proxy-icon nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(use-package minions
  :config
  (minions-mode 1))

;;;; defaults ;;;;
(setq mac-left-control-modifier 'control
      mac-left-option-modifier 'super
      mac-right-control-modifier 'meta
      mac-right-option-modifier 'hyper)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(setq-default electric-pair-preserve-balance nil
              compilation-scroll-output 'first-error
              vc-follow-symlinks t
              eldoc-echo-area-use-multiline-p nil
              indent-tabs-mode nil
              make-backup-files nil
              auto-save-default nil
              create-lockfiles nil)
(global-set-key (kbd "C-M-8") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-9") 'enlarge-window)
(global-set-key (kbd "C-M-0") 'shrink-window)
(global-set-key (kbd "C-M--") 'shrink-window-horizontally)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "H-f") 'forward-sexp)
(global-set-key (kbd "H-b") 'backward-sexp)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x C-c"))

;; allow hash to be entered
(global-set-key (kbd "M-3")
                '(lambda ()
                   (interactive)
                   (insert "#")))

(global-set-key (kbd "M-s g") 'grep)
(setq grep-command "grep -rn "
      grep-use-null-device nil)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package org)
(setq org-log-done t)

(use-package dired
  :ensure nil
  :commands (dired)
  :init (add-hook 'dired-mode-hook 'auto-revert-mode)
  :config (setq dired-kill-when-opening-new-dired-buffer t)
  :bind (:map dired-mode-map
              ("-" . dired-up-directory)))
(put 'dired-find-alternate-file 'disabled nil)
(require 'dired-x)

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . hl-line-mode)
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil))

(use-package whitespace
  :init (add-hook 'before-save-hook #'whitespace-cleanup)
  :config (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style
        '(face trailing tabs indentation::space empty indention spaces trailing space-mark space-after-tab space-before-tab tab-mark)))

(use-package avy
  :ensure t)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)

;;; text editing
(use-package move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-|") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:") 'mc/skip-to-previous-like-this)

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))
;; (global-set-key (kbd "C-;") 'select-current-line)

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

(use-package smartscan
  :ensure t
  :bind ("H-n" . smartscan-symbol-go-forward)
  ("H-p" . smartscan-symbol-go-backward))

(use-package paredit
  :init (autoload 'enable-paredit-mode "paredit" t)
  (add-hook 'emacs-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

;;;; terminals & shells ;;;;;
(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map ("C-y" . vterm-yank))
  :config
  (setq vterm-max-scrollback 10000))
(global-set-key (kbd "M-`") 'vterm)

(add-to-list 'display-buffer-alist
             '("*shell" (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.4)))

(add-to-list 'display-buffer-alist
             '("*eshell" (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.4)))

(defun toggle-shell (shell-str shell)
  "Close the current buffer if it is open, otherwise open a new one!"
  (interactive)
  (if (string-equal shell-str major-mode)
      (bury-buffer)
    (funcall shell)))

(defun shell-toggle () (interactive) (toggle-shell "shell-mode" 'shell))
(global-set-key (kbd "C-`") 'shell-toggle)
(defun eshell-toggle () (interactive) (toggle-shell "eshell-mode" 'eshell))
(global-set-key (kbd "C-!") 'eshell-toggle)

;;;; completion ;;;;
(ido-mode 1)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching t)
(use-package ido-completing-read+)
(ido-ubiquitous-mode 1)
(setq ido-use-url-at-point nil)

(use-package smex)
(global-set-key (kbd "M-x") 'smex)

(use-package company
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection)))
(add-hook 'after-init-hook 'global-company-mode)

;;;; version control ;;;;
(use-package magit
  :bind
  (("C-c m s" . magit-status)
   ("C-c m l" . magit-log)
   ("C-c m b" . magit-blame)))

;;;; languages ;;;;
(use-package tree-sitter-langs :ensure t)
(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package eglot
  :init
  (fset #'jsonrpc--log-event #'ignore)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'js-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  :custom
  (setq eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") 'eglot-format))

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(define-key flymake-mode-map (kbd "C-c l p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c l n") 'flymake-goto-next-error)

(setq python-shell-interpreter "ipython")
(setq python-shell-completion-native-enable nil)
(use-package poetry)

;;; code formatting
(setq c-default-style "linux"
      c-basic-offset 4)

(use-package blacken)

(use-package sqlformat
  :config (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;; garbage collection ;;;
(setq gc-cons-threshold (* 1024 1024 100))

;;; .emacs ends here

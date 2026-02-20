;; ==================== ;;
;; Early Initialization ;;
;; ==================== ;;

(defun my/display-startup-time ()
  (message "Emacs loaded in %.2f seconds with %d garbage collections"
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))
(add-hook 'emacs-startup-hook #'my/display-startup-time)

(setq gc-cons-threshold (* 1024 1024 100)
      gc-cons-percentage 0.6
      package-enable-at-startup nil)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 1024 1024 16))))

(setq frame-title-format '("%f")
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      ns-pop-up-frames nil
      ns-use-proxy-icon nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(background-color . "#181818"))
(set-face-attribute 'default nil :background "#181818" :foreground "#e4e4ef")

(setq inhibit-startup-screen t
      inhibit-startup-message t
      ring-bell-function 'ignore
      custom-safe-themes t
      use-dialog-box nil
      use-short-answers t
      inhibit-compacting-font-caches t
      mode-line-format nil)

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(when (eq system-type 'darwin)
  (setq mac-left-control-modifier 'control
        mac-option-modifier 'meta
        mac-command-modifier 'super))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file :noerror :nomessage)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; ================== ;;
;; Main Configuration ;;
;; ================== ;;

(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (c-ts-mode-indent-offset 4)
  (comint-process-echoes t)
  (comp-async-report-warnings-errors nil)
  (comp-deferred-compilation t)
  (completion-auto-select t)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (delete-selection-mode 1)
  (display-line-numbers 'nil)
  (electric-pair-preserve-balance nil)
  (help-window-select t)
  (history-length 500)
  (indent-tabs-mode nil)
  (lazy-highlight-initial-delay 0)
  (make-backup-files nil)
  (mode-line-collapse-minor-modes t)
  (package-install-upgrade-built-in t)
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-completion-native-enable nil)
  (resize-mini-windows nil)
  (set-mark-command-repeat-pop t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (truncate-lines nil)
  (use-package-always-ensure t)
  (use-package-compute-statistics t)
  (vc-follow-symlinks t)
  (whitespace-line-column 80)
  (xref-search-program 'ripgrep)
  :config
  (put 'narrow-to-region 'disabled nil)
  (with-eval-after-load 'c-ts-mode
    (define-key c-ts-mode-map (kbd "C-c .") nil))
  :bind
  (("<C-wheel-down>" . ignore)
   ("<C-wheel-up>" . ignore)
   ("<pinch>" . ignore)
   ("C-x C-c" . nil)
   ("C-x f" . find-file-at-point)
   ("C-x m" . nil)
   ("C-z" . nil)
   ("C-q" . query-replace-regexp)
   ("C-M--" . shrink-window-horizontally)
   ("C-M-0" . shrink-window)
   ("C-M-8" . enlarge-window-horizontally)
   ("C-M-9" . enlarge-window)
   ("M-3" . (lambda () (interactive) (insert "#")))
   ("C-x C-b" . ibuffer)
   ("C-x 2" . (lambda () (interactive) (split-window-vertically) (other-window 1)))
   ("C-x 3" . (lambda () (interactive) (split-window-horizontally) (other-window 1)))
   ("M-o" . other-window))
  :hook
  (ibuffer-mode . hl-line-mode)
  (before-save . whitespace-cleanup)
  (after-init . (lambda ()
                  (if (eq system-type 'darwin)
                      (set-frame-font "Iosevka 20" nil t)
                    (set-frame-font "Iosevka 16" nil t))
                  (show-paren-mode 1)
                  (editorconfig-mode 1)
                  (savehist-mode 1)
                  (pixel-scroll-precision-mode 1)
                  (electric-pair-mode 1)
                  (global-completion-preview-mode 1)
                  (global-auto-revert-mode 1)))
  (occur-mode . (lambda () (switch-to-buffer-other-window "*Occur*")))
  (html-mode . (lambda () (local-unset-key (kbd "M-o")))))

(use-package gruber-darker-theme
  :config (load-theme 'gruber-darker :no-confirm))

(use-package windmove
  :ensure nil
  :bind
  (("s-b" . windmove-left)
   ("s-f" . windmove-right)
   ("s-p" . windmove-up)
   ("s-n" . windmove-down)))

(use-package ido
  :ensure nil
  :demand t
  :config (ido-mode 1)
  :custom
  (ido-max-prospects 8)
  (ido-everywhere t)
  (ido-enable-flex-matching t)
  (ido-use-url-at-point nil))

(use-package ido-completing-read+
  :after ido
  :config (ido-ubiquitous-mode 1))

(use-package isearch
  :ensure nil
  :custom (isearch-lazy-count t)
  :bind (:map isearch-mode-map
              ("C-<return>" . isearch-done-opposite)
              ("C-q"        . isearch-query-replace-regexp))
  :init
  (defun isearch-done-opposite (&optional nopush edit)
    (interactive)
    (let ((forward isearch-forward))
      (isearch-done nopush edit)
      (when (and forward isearch-other-end)
        (goto-char isearch-other-end)))))

(use-package amx
  :init (amx-mode 1))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  :bind
  ("C-c c" . compile)
  ("C-c r" . recompile))

(use-package grep
  :ensure nil
  :defer t
  :bind
  ("C-c s s" . grep)
  ("C-c s p" . grep-project)
  ("C-c s ." . grep-dwim)
  ("C-c s r" . replace-regexp-no-move)
  :custom
  (grep-use-null-device nil)
  (grep-use-headings t)
  (grep-save-buffers t)
  (grep-command "rg -S --no-heading --color=never ")
  :config
  (setq grep-default-command "rg -S --no-heading --color=never ")

  (defun grep-project (&optional initial-input)
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (grep (read-shell-command "Grep project: "
                                (concat grep-default-command "" (or initial-input ""))
                                'grep-history))))
  (defun grep-dwim ()
    (interactive)
    (if-let* ((symbol (thing-at-point 'symbol t)))
        (let ((default-directory (project-root (project-current)))
              (command (concat grep-default-command (shell-quote-argument symbol) " .")))
          (grep command))
      (call-interactively 'grep-project)))

  (defun replace-regexp-no-move ()
    (interactive)
    (save-excursion
      (call-interactively 'replace-regexp))))

(use-package org
  :ensure nil
  :custom
  (org-log-done t)
  (org-directory "~/.me/org")
  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (setq tab-width 8))))

(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-do-revert-buffer t)
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;;;###autoload
(defun project-dired-home()
  (interactive)
  (let ((root (project-root (project-current t))))
    (dired root)))
(global-set-key (kbd "C-x p h") #'project-dired-home)

(use-package transpose-frame
  :defer t
  :bind ("C-z C-t" . transpose-frame))

(use-package move-text
  :defer t
  :bind
  ("M-p" . 'move-text-up)
  ("M-n" . 'move-text-down))

(use-package expand-region
  :defer t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package multiple-cursors
  :defer t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c ." . mc/mark-all-like-this)
  ("C-c ," . mc/edit-lines))

(use-package vterm
  :defer t
  :bind (:map vterm-mode-map
              ("C-z" . nil))
  :custom (vterm-always-compile-module t)
  :config
  (setq vterm-timer-delay 0.01)
  (setq vterm-max-scrollback 100000)
  (define-key project-prefix-map "t" #'vterm-project)
  (defvar my-original-mode-line-format mode-line-format)
  (defun vterm-update-mode-line ()
    (if vterm-copy-mode
        (setq-local mode-line-format
                    (append my-original-mode-line-format '(" [COPY-MODE]")))
      (setq-local mode-line-format my-original-mode-line-format)))
  (add-hook 'vterm-copy-mode-hook #'vterm-update-mode-line))

;;;###autoload
(defun vterm-project ()
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (project-name (file-name-nondirectory (directory-file-name default-directory)))
         (buffer-name (format "*vterm-%s*" project-name)))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (vterm buffer-name))))

(use-package eshell
  :ensure nil
  :hook (eshell-mode . eshell-setup)
  :custom
  (eshell-save-history-on-exit nil)
  (eshell-hist-ignoredups t)
  (eshell-history-size 10000)
  :config
  (defun eshell-setup ()
    "Configure eshell environment and settings."
    (setenv "TERM" "xterm-256color")
    (setq-local eshell-history-file-name (expand-file-name "eshell/history" user-emacs-directory))
    (add-hook 'eshell-pre-command-hook #'eshell-append-history :append :local))
  (defun eshell-append-history ()
    "Efficiently append last command to history file."
    (when-let* ((ring eshell-history-ring)
                (cmd (car (ring-elements ring))))
      (let ((eshell-history-ring (make-ring 1)))
        (ring-insert eshell-history-ring cmd)
        (eshell-write-history eshell-history-file-name t)))))

(use-package treesit
  :ensure nil
  :init
  (dolist (mode-map '(("\\.c\\'"    . c-ts-mode)
                      ("\\.h\\'"    . c-ts-mode)
                      ("\\.css\\'"  . css-ts-mode)
                      ("\\.js\\'"   . js-ts-mode)
                      ("\\.json\\'" . json-ts-mode)
                      ("\\.go\\'"   . go-ts-mode)
                      ("\\.lua\\'"  . lua-ts-mode)
                      ("\\.py\\'"   . python-ts-mode)
                      ("\\.rs\\'"   . rust-ts-mode)
                      ("\\.ts\\'"   . typescript-ts-mode)
                      ("\\.yaml\\'" . yaml-ts-mode)))
    (add-to-list 'auto-mode-alist mode-map))
  :custom
  (treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))
  (treesit-font-lock-level 4)
  :hook
  (treesit-ready . treesit-install-language-grammar))

(use-package magit
  :defer t
  :bind
  (("C-c m s" . magit-status)
   ("C-c m l" . magit-log)
   ("C-c m b" . magit-blame))
  :custom
  (when (eq system-type 'darwin)
    (magit-git-executable "/opt/homebrew/bin/git")))

(use-package eglot
  :custom
  (eglot-stay-out-of '(flymake eldoc completion-at-point))
  (eglot-ignored-server-capabilities
   '(:documentHighlightProvider
     :completionProvider
     :hoverProvider
     :signatureHelpProvider
     :codeActionProvider
     :documentFormattingProvider
     :renameProvider
     :inlayHintProvider
     :referencesProvider
     :typeDefinitionProvider
     :implementationProvider)))

(use-package gptel
  :ensure t
  :bind (("C-c <RET>" . gptel-send)
         ("C-z C-g"     . my/gptel-quick))
  :config
  (defun my/get-openrouter-key ()
    (let ((match (car (auth-source-search :host "openrouter.ai" :user "apikey"))))
      (if match
          (let ((secret (plist-get match :secret)))
            (if (functionp secret) (funcall secret) secret))
        (error "OpenRouter API key not found in ~/.authinfo"))))

  (defun my/gptel-quick ()
    (interactive)
    (if (string= (buffer-name) "*OpenRouter*")
        (switch-to-buffer nil)
      (switch-to-buffer (gptel "*OpenRouter*"))))

  (setq-default
   gptel-backend
   (gptel-make-openai "OpenRouter"
     :host "openrouter.ai"
     :endpoint "/api/v1/chat/completions"
     :stream t
     :key #'my/get-openrouter-key
     :models '("openai/gpt-5.2-codex"
               "google/gemini-3.1-pro-preview")))
  (setq-default gptel-model "openai/gpt-5.2-codex"))

(use-package pyvenv :defer t)
(use-package bluetooth :defer t)

;; ================ ;;
;; Custom Functions ;;
;; ================ ;;

;;;###autoload
(defun select-current-line ()
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))
(global-set-key (kbd "C-;") #'select-current-line)

;;;###autoload
(defun duplicate-line ()
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))
(global-set-key (kbd "C-,") #'duplicate-line)

;;;###autoload
(defun toggle-shell (shell-str shell &optional window-width)
  (interactive)
  (if (string-equal shell-str major-mode)
      (quit-window)
    (let ((display-buffer-alist
           (if window-width
               `((".*" (display-buffer-pop-up-window)
                  (window-width . ,window-width)))
             nil)))
      (funcall shell))))
(global-set-key (kbd "C-z C-z") (lambda () (interactive) (toggle-shell "vterm-mode" #'vterm)))
(global-set-key (kbd "C-z C-s") (lambda () (interactive) (toggle-shell "shell-mode" #'shell 0.45)))
(global-set-key (kbd "C-z C-e") (lambda () (interactive) (toggle-shell "eshell-mode" #'eshell 0.45)))

(provide 'init)

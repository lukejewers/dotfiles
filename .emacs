;; ==================== ;;
;; Early Initialization ;;
;; ==================== ;;

(setq gc-cons-threshold (* 1024 1024 100)
      gc-cons-percentage 0.6
      package-enable-at-startup nil)

(defun my-display-startup-time ()
  (message "Emacs loaded in %.2f seconds with %d garbage collections"
           (float-time (time-subtract after-init-time before-init-time)) gcs-done))
(add-hook 'emacs-startup-hook #'my-display-startup-time)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 1024 1024 16))))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(set-face-attribute 'default nil :background "#181818" :foreground "#e4e4ef")
(set-language-environment "UTF-8")

(setq default-frame-alist
      `((ns-transparent-titlebar . t)
        (background-color . "#181818")
        (font . ,(if (eq system-type 'darwin) "Iosevka 20" "Iosevka 16"))
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)))

(setq frame-title-format '("%f")
      frame-resize-pixelwise t
      window-resize-pixelwise t
      frame-inhibit-implied-resize t
      ns-pop-up-frames nil
      ns-use-proxy-icon nil
      inhibit-startup-screen t
      inhibit-startup-message t
      ring-bell-function 'ignore
      custom-safe-themes t
      use-dialog-box nil
      use-short-answers t
      inhibit-compacting-font-caches t
      mode-line-format nil
      warning-minimum-level :error
      warning-suppress-types '((lexical-binding))
      custom-file (locate-user-emacs-file "custom-vars.el")
      package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(load custom-file t t)
(when (eq system-type 'darwin)
  (setq mac-left-control-modifier 'control
        mac-option-modifier 'meta
        mac-command-modifier 'super))

;; ================== ;;
;; Main Configuration ;;
;; ================== ;;

(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (c-ts-mode-indent-offset 4)
  (comint-completion-addsuffix nil)
  (comint-process-echoes t)
  (comp-async-report-warnings-errors nil)
  (completion-auto-select t)
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  (delete-by-moving-to-trash t)
  (delete-pair-blink-delay 0.1)
  (delete-pair-push-mark t)
  (duplicate-line-final-position 1)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-preserve-balance nil)
  (help-window-select t)
  (highlight-nonselected-windows nil)
  (indent-tabs-mode nil)
  (lazy-highlight-initial-delay 0)
  (make-backup-files nil)
  (mode-line-collapse-minor-modes t)
  (package-install-upgrade-built-in t)
  (pcomplete-termination-string "")
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-completion-native-enable nil)
  (resize-mini-windows nil)
  (kill-do-not-save-duplicates t)
  (save-interprogram-paste-before-kill t)
  (set-mark-command-repeat-pop t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (use-package-always-ensure t)
  (use-package-compute-statistics t)
  (whitespace-line-column 80)
  (xref-search-program 'ripgrep)
  :config
  (blink-cursor-mode 0)
  (delete-selection-mode 1)
  (editorconfig-mode 1)
  (electric-pair-mode 1)
  (global-auto-revert-mode 1)
  (global-completion-preview-mode 1)
  (pixel-scroll-precision-mode 1)
  (put 'narrow-to-region 'disabled nil)
  (savehist-mode 1)
  (show-paren-mode 1)
  (tooltip-mode 0)
  (with-eval-after-load 'c-ts-mode (define-key c-ts-mode-map (kbd "C-c .") nil))
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
   ("C-," . duplicate-line)
   ("C-x C-b" . ibuffer)
   ("C-x 2" . (lambda () (interactive) (split-window-vertically)   (other-window 1)))
   ("C-x 3" . (lambda () (interactive) (split-window-horizontally) (other-window 1)))
   ("M-o" . other-window)
   ("C-M-z" . delete-pair)
   ("C-x p h" . my-project-dired-home)
   ("C-z C-t" . my-toggle-split-direction)
   ("C-z C-v" . (lambda () (interactive) (my-toggle-shell 'vterm-mode #'vterm)))
   ("C-z C-s" . (lambda () (interactive) (my-toggle-shell 'shell-mode #'shell 0.45)))
   ("C-z C-e" . (lambda () (interactive) (my-toggle-shell 'eshell-mode #'eshell 0.45))))
  :hook
  (before-save . whitespace-cleanup)
  (occur-mode . (lambda () (switch-to-buffer-other-window "*Occur*")))
  (html-mode . (lambda () (local-unset-key (kbd "M-o"))))
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (ibuffer-mode . hl-line-mode)
  (shell-mode . (lambda () (setq-local scroll-margin 1))))

(use-package recentf
  :config
  (recentf-mode 1)
  :bind ("C-x C-r" . 'recentf-open))

(use-package gruber-darker-theme
  :config (load-theme 'gruber-darker :no-confirm))

(use-package ido
  :ensure nil
  :demand t
  :config (ido-mode 1)
  :custom
  (ido-auto-merge-work-directories-length -1)
  (ido-max-prospects 10)
  (ido-everywhere t)
  (ido-enable-flex-matching t)
  (ido-use-url-at-point nil))

(use-package ido-completing-read+
  :after ido
  :config (ido-ubiquitous-mode 1))

(use-package amx
  :init (amx-mode 1))

(use-package isearch
  :ensure nil
  :custom (isearch-lazy-count t)
  :bind (:map isearch-mode-map ("C-q" . isearch-query-replace-regexp)))

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
  ("C-c s p" . my-grep-project)
  ("C-c s ." . my-grep-dwim)
  ("C-c s r" . my-replace-regexp-no-move)
  :custom
  (grep-use-null-device nil)
  (grep-save-buffers t)
  (grep-use-headings t)
  (grep-command "rg -S --no-heading --color=never ")
  :config
  (setq grep-default-command "rg -S --no-heading --color=never ")
  (defun my-grep-project (&optional initial-input)
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (grep (read-shell-command "Grep project: "
                                (concat grep-default-command "" (or initial-input ""))
                                'grep-history))))
  (defun my-grep-dwim ()
    (interactive)
    (if-let* ((symbol (thing-at-point 'symbol t)))
        (let ((default-directory (project-root (project-current)))
              (command (concat grep-default-command (shell-quote-argument symbol) " .")))
          (grep command))
      (call-interactively 'my-grep-project)))
  (defun my-replace-regexp-no-move ()
    (interactive)
    (save-excursion
      (call-interactively 'replace-regexp))))

(use-package org
  :ensure nil
  :custom
  (org-log-done t)
  (org-directory "~/me/org")
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
  :bind
  (:map dired-mode-map
        ("V" . my-reveal-file-at-point-in-finder))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package move-text
  :defer t
  :bind
  ("M-p" . move-text-up)
  ("M-n" . move-text-down))

(use-package expreg
  :defer t
  :bind
  ("C-=" . expreg-expand)
  ("C--" . expreg-contract))

(use-package multiple-cursors
  :defer t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c ." . mc/mark-all-like-this)
  ("C-c ," . mc/edit-lines))

(use-package vterm
  :defer t
  :bind (("C-x p v" . my-vterm-project)
         :map vterm-mode-map ("C-z" . nil))
  :custom (vterm-always-compile-module t)
  :config
  (setq vterm-timer-delay 0.01)
  (setq vterm-max-scrollback 100000)
  (defvar my-original-mode-line-format mode-line-format)
  (defun my-vterm-update-mode-line ()
    (if vterm-copy-mode
        (setq-local mode-line-format
                    (append my-original-mode-line-format '(" [COPY-MODE]")))
      (setq-local mode-line-format my-original-mode-line-format)))
  (defun my-vterm-project ()
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (project-name (file-name-nondirectory (directory-file-name default-directory)))
           (buffer-name (format "*vterm-%s*" project-name)))
      (if (get-buffer buffer-name)
          (switch-to-buffer buffer-name)
        (vterm buffer-name))))
  (add-hook 'vterm-copy-mode-hook #'my-vterm-update-mode-line))

(use-package treesit
  :ensure nil
  :custom
  (major-mode-remap-alist
   '((c-mode          . c-ts-mode)
     (c++-mode        . c++-ts-mode)
     (css-mode        . css-ts-mode)
     (js-mode         . js-ts-mode)
     (json-mode       . json-ts-mode)
     (go-mode         . go-ts-mode)
     (swift-mode      . swift-ts-mode)
     (lua-mode        . lua-ts-mode)
     (python-mode     . python-ts-mode)
     (rust-mode       . rust-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (yaml-mode       . yaml-ts-mode)))
  (treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))
  (treesit-font-lock-level 4))

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

(use-package pyvenv
  :defer t)

(use-package gptel
  :ensure t
  :config
  (setq gptel-model "z-ai/glm-5.1"
        gptel-default-mode 'org-mode
        gptel-backend (gptel-make-openai "gptel"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :stream t
                        :key 'gptel-api-key
                        :models '("google/gemini-3.1-pro-preview"
                                  "moonshotai/kimi-k2.5"
                                  "z-ai/glm-5.1"))))

;; ================ ;;
;; Custom Functions ;;
;; ================ ;;

(defun my-toggle-shell (shell-mode-symbol shell-func &optional window-width)
  (interactive)
  (if (eq major-mode shell-mode-symbol)
      (quit-window)
    (let ((display-buffer-alist
           (if window-width
               `((".*" (display-buffer-pop-up-window)
                  (window-width . ,window-width)))
             nil)))
      (funcall shell-func))))

(defun my-toggle-split-direction ()
  "Toggle between a horizontal and vertical split for two windows."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "This function only works with exactly 2 windows"))
  (let* ((this-win (selected-window))
         (next-win (next-window this-win))
         (next-buf (window-buffer next-win))
         (is-vertical (window-combined-p this-win)))
    (delete-window next-win)
    (let ((new-win (if is-vertical (split-window-right) (split-window-below))))
      (set-window-buffer new-win next-buf))))

(defun my-project-dired-home()
  "Open Dired at the root of the current project."
  (interactive)
  (let ((root (project-root (project-current t))))
    (dired root)))

(defun my-reveal-file-at-point-in-finder ()
  "Reveal the current file in macOS Finder."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "This command is only available on macOS"))
  (let (filename)
    (cond
     ((derived-mode-p 'dired-mode)
      (setq filename (or (dired-get-filename nil t) (dired-current-directory))))
     ((buffer-file-name)
      (setq filename (buffer-file-name)))
     (t (user-error "Not in a file or dired buffer")))
    (unless (file-exists-p filename)
      (user-error "File does not exist: %s" filename))
    (start-process "reveal-in-finder" nil "open" "-R" filename)
    (message "Revealing %s in Finder" (file-name-nondirectory filename))))

(provide 'init)

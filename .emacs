(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init
  (exec-path-from-shell-initialize))

(use-package emacs
  :ensure nil
  :custom
  (use-package-compute-statistics t)
  (comp-async-report-warnings-errors nil)
  (comp-deferred-compilation t)
  (package-install-upgrade-built-in t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (inhibit-startup-screen t)
  (treesit-font-lock-level 4)
  (inhibit-startup-message t)
  (make-backup-files nil)
  (ring-bell-function 'ignore)
  (custom-safe-themes t)
  (tab-always-indent 'complete)
  (electric-pair-preserve-balance nil)
  (vc-follow-symlinks t)
  (resize-mini-windows nil)
  (tab-width 4)
  (display-line-numbers 'nil)
  (use-dialog-box nil)
  (delete-selection-mode 1)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-ask-about save nil)
  (frame-title-format '("%f"))
  (ns-use-proxy-icon nil)
  (mac-left-control-modifier 'control)
  (mac-left-option-modifier 'super)
  (mac-right-control-modifier 'meta)
  (mac-right-option-modifier 'meta)
  (lazy-highlight-initial-delay 0)
  (help-window-select t)
  (completion-auto-select t)
  (set-mark-command-repeat-pop t)
  (isearch-lazy-count t)
  (xref-search-program 'ripgrep) ; project-find-regexp
  (grep-command "rg -nS --no-heading ")
  (grep-use-null-device nil)
  (project-switch-commands '((project-find-file "Find file" "f")
                             (project-find-dir "Find dir" "d")
                             (project-dired "Dired" "D")
                             (project-vterm "t")
                             (project-home "Home" "h")
                             (project-fzf "fzf", "z")
                             (project-find-regexp "Vterm" "g")
                             (magit-project-status "Magit" "m")))
  (c-basic-offset 4)
  (indent-tabs-mode nil)
  (smerge-command-prefix "C-c v")
  :init
  (auth-source-pass-enable)
  (setq gc-cons-threshold most-positive-fixnum)
  (set-frame-font "Iosevka 20" nil t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when scroll-bar-mode (scroll-bar-mode -1))
  (global-auto-revert-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (savehist-mode 1)
  (blink-cursor-mode -1)
  (global-completion-preview-mode 1)
  (modify-coding-system-alist 'file "" 'utf-8)
  (put 'narrow-to-region 'disabled nil)
  :bind
  (("<C-wheel-down>" . ignore)
   ("<C-wheel-up>" . ignore)
   ("<pinch>" . ignore)
   ("C-x C-c" . nil)
   ("C-x f" . nil)
   ("C-x m" . nil)
   ("C-x p h" . project-home)
   ("C-x p z" . project-fzf)
   ("C-z" . nil)
   ("M-o" . nil)
   ("C-M--" . shrink-window-horizontally)
   ("C-M-0" . shrink-window)
   ("C-M-8" . enlarge-window-horizontally)
   ("C-M-9" . enlarge-window)
   ("C-c c" . compile)
   ("C-c r" . recompile)
   ("C-q" . query-replace)
   ("C-x 2" . (lambda () (interactive) (split-window-vertically) (other-window 1)))
   ("C-x 3" . (lambda () (interactive) (split-window-horizontally) (other-window 1)))
   ("C-x f" . find-file-at-point)
   ("M-3" . (lambda () (interactive) (insert "#")))
   ("M-o" . other-window)
   ("s-b" . backward-sexp)
   ("s-f" . forward-sexp)
   ("s-n" . forward-list)
   ("s-p" . backward-list))
  :config
  ;; Custom file handling
  (setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))
  (load custom-file :noerror :nomessage)
  ;; Package archives
  (dolist (archive '(("melpa" . "https://melpa.org/packages/")
                     ("elpa" . "https://elpa.gnu.org/packages/")
                     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
    (add-to-list 'package-archives archive))
  ;; Mode-specific settings
  (add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))
  (add-hook 'html-mode-hook (lambda () (local-unset-key (kbd "M-o"))))
  ;; Frame and buffer settings
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'display-buffer-alist '("*shell" (display-buffer-in-side-window) (side . right) (window-width . 0.45)))
  ;; startup
  (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 16 1024 1024))))
  (add-hook 'emacs-startup-hook (lambda () (message "Emacs loaded in %s with %d garbage collections." (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done))))

(use-package gruber-darker-theme
  :ensure t
  :demand t
  :config (load-theme 'gruber-darker t))

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package rg
  :ensure t
  :defer t
  :bind
  ("C-c s s"   . rg)
  ("C-c s d"   . rg-current-dir-all-files)
  ("C-c s p"   . rg-project-all-files)
  ("C-c s ."   . rg-dwim)
  ("C-c s m"   . rg-menu)
  :config
  (defun rg-project-all-files (search-term)
    "Run ripgrep in project root searching all files."
    (interactive "sSearch term: ")
    (rg search-term "*" (project-root (project-current))))
  (defun rg-current-dir-all-files (search-term)
    "Run ripgrep in the current directory, searching all files."
    (interactive "sSearch term: ")
    (rg search-term "*" default-directory))
  (add-to-list 'rg-finish-functions (lambda (buffer _) (pop-to-buffer buffer))))

(use-package wgrep
  :ensure t
  :defer t
  :config (setq wgrep-auto-save-buffer t))

(use-package org
  :ensure nil
  :custom
  (org-log-done t)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (setq tab-width 8))))

(use-package dired
  :ensure nil
  :defer t
  :commands (dired)
  :hook (dired-mode . auto-revert-mode)
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (put 'dired-find-alternate-file 'disabled nil))

(defun project-home()
  "Open dired in the root directory of the current project."
  (interactive)
  (let ((root (project-root (project-current t))))
    (dired root)))

(use-package dired-ranger
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(use-package dired-subtree
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle))
  :custom (dired-subtree-use-backgrounds nil))

(use-package ibuffer
  :ensure t
  :defer t
  :bind
  ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . hl-line-mode))

(use-package transpose-frame
  :ensure t
  :defer t
  :bind ("C-z C-t" . transpose-frame))

(use-package whitespace
  :ensure t
  :defer t
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-line-column 80) ;; limit line length
  (whitespace-style
   '(face trailing tabs indentation::space empty indention spaces trailing space-mark space-after-tab space-before-tab tab-mark)))

(use-package fzf
  :ensure t
  :defer t
  :bind ("C-z C-f" . fzf-home)
  :config
  (setq fzf/args "-x --print-query --no-hscroll --color=fg:#e4e4ef,bg:#181818,hl:#ffdd33,fg+:#f6f6f6,bg+:#282828,hl+:#ffdd33,info:#96a6c8,prompt:#96a6c8,pointer:#ffdd33,marker:#73c936,spinner:#96a6c8,header:#73c936 --bind=ctrl-j:accept,ctrl-k:kill-line,ctrl-delete:backward-kill-word --walker-skip .git,.Trash,.nvm,.cache,.cargo,venv,.venv,.pyenv,.rustup,.next,node_modules,go,target,Library,Applications,Music,Movies"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "rg --no-heading -nH"
        fzf/position-bottom t
        fzf/window-height 10)

  (defun fzf-home ()
    "Run fzf from home directory."
    (interactive)
    (let ((default-directory "~/"))
      (fzf))))

(defun project-fzf ()
  "Run fzf in the project root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (fzf)))

(use-package avy
  :ensure t
  :defer t
  :bind ("M-j" . avy-goto-word-or-subword-1))

(use-package move-text
  :defer t
  :ensure t
  :bind
  ("M-p" . 'move-text-up)
  ("M-n" . 'move-text-down))

(use-package expand-region
  :ensure t
  :defer t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind
  ("C-M-SPC" . set-rectangular-region-anchor)
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

(defun copy-word-at-point ()
  "Copy the word at point to the kill ring."
  (interactive)
  (kill-new (thing-at-point 'word t)))
(global-set-key (kbd "C-c C-w") 'copy-word-at-point)

(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))
(global-set-key (kbd "C-,") 'duplicate-line)

(defun copy-full-path-to-kill-ring ()
  "Copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))
(global-set-key (kbd "C-±") 'copy-full-path-to-kill-ring)

(defun toggle-shell (shell-str shell)
  (interactive)
  (if (string-equal shell-str major-mode)
      (quit-window)
    (let ((display-buffer-alist
           '((".*" (display-buffer-pop-up-window)
              (window-width . 0.45)))))
      (funcall shell))))
(global-set-key (kbd "C-z C-e") (lambda () (interactive) (toggle-shell "eshell-mode" 'eshell)))
(global-set-key (kbd "C-z C-s") (lambda () (interactive) (toggle-shell "shell-mode" 'shell)))
(global-set-key (kbd "C-z C-z") (lambda () (interactive) (toggle-shell "vterm-mode" 'vterm)))

(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("C-z" . nil))
  :custom (vterm-always-compile-module t)
  :config
  (setq vterm-timer-delay 0.01)
  (setq vterm-max-scrollback 10000)
  (define-key project-prefix-map "t" #'project-vterm)
  (defun vterm-mode-line-color ()
    (let ((color (if vterm-copy-mode "DarkGoldenrod" "#282828")))
      (set-face-background 'mode-line color)))
  (add-hook 'vterm-copy-mode-hook #'vterm-mode-line-color))

(defun vterm-new ()
  "Create a new vterm buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "*vterm*")))
    (with-current-buffer buffer
      (vterm-mode))
    (switch-to-buffer buffer)))

(defun project-vterm ()
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (project-name (file-name-nondirectory (directory-file-name default-directory)))
         (buffer-name (format "*vterm-%s*" project-name)))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (vterm buffer-name))))

(use-package ido
  :ensure nil
  :demand t
  :config
  (ido-mode 1)
  :custom
  (ido-everywhere t)
  (ido-enable-flex-matching t)
  (ido-use-url-at-point nil)
  (ido-max-window-height 1))

(use-package ido-completing-read+
  :ensure t
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(use-package amx
  :ensure t
  :config
  (amx-mode 1))

(use-package marginalia
  :ensure t
  :defer t
  :config (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(use-package cape
  :ensure t
  :defer t
  :config
  (setq completion-at-point-functions
        (list (cape-capf-super
               #'cape-file
               #'cape-dabbrev
               #'cape-keyword
               #'cape-symbol
               #'cape-abbrev))))

(use-package magit
  :ensure t
  :defer t
  :bind
  (("C-c m s" . magit-status)
   ("C-c m l" . magit-log)
   ("C-c m b" . magit-blame))
  :custom
  (magit-process-finish-apply-ansi-colors t))

(use-package verb
  :ensure t
  :config
  (define-key global-map (kbd "C-c v") verb-command-map)
  :hook (org-mode . verb-mode))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :ensure t
  :defer t
  :hook
  (js-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (lua-mode . eglot-ensure)
  :custom
  (fset #'jsonrpc--log-event #'ignore)
  (jsonrpc-event-hook nil)
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-stderr-buffer-size 10000)
  (eglot-strict-mode nil)
  (eglot-sync-connect nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  (go-ts-mode-indent-offset tab-width)
  (eglot-autoshutdown t)
  (eglot-connect-timeout 30)
  (eglot-autoreconnect nil)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:documentHighlightProvider
     :documentSymbolProvider
     :workspaceSymbolProvider
     :codeActionProvider
     :codeLensProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :renameProvider
     :documentLinkProvider
     :colorProvider
     :foldingRangeProvider
     :executeCommandProvider
     :inlayHintProvider)))

(use-package flymake
  :ensure nil
  :defer t
  :bind ("M-g e" . flymake-show-buffer-diagnostics))

(use-package apheleia
  :ensure t
  :bind ("C-c f" . apheleia-format-buffer)
  :config
  (setf (alist-get 'pg_format apheleia-formatters) '("pg_format" "-s2" "-g" "-u1"))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . (isort black)))
  (add-to-list 'apheleia-mode-alist '(sql-mode . pg_format)))

(use-package editorconfig
  :ensure t
  :defer t
  :config (editorconfig-mode 1))

(use-package python
  :ensure nil
  :defer t
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-interpreter "ipython")
  (python-shell-completion-native-enable nil))

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  (pyvenv-mode 1))

(use-package dape
  :hook
  (after-init . dape-breakpoint-load)
  :config
  (dape-breakpoint-global-mode)
  (setq dape-buffer-window-arrangement 'right))

(use-package gptel
  :bind ("C-z C-c" . gptel-make-window)
  :config
  (defvar gptel-api-key-cache nil)
  (setq gptel-default-mode 'org-mode
        gptel-model "deepseek/deepseek-chat-v3-0324"
        gptel--system-message
        "You are an expert coding assistant. Please provide correct, idiomatic code with concise explanations."
        gptel-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :key (lambda ()
                 (or gptel-api-key-cache
                     (setq gptel-api-key-cache
                           (auth-source-pass-get 'secret "openrouter.ai/apikey"))))
          :models '("anthropic/claude-3.7-sonnet"
                    "deepseek/deepseek-chat-v3-0324"))))

(defun gptel-make-window ()
  (interactive)
  (let ((open-router-window (get-buffer-window "*OpenRouter*")))
    (if open-router-window
        (if (eq (selected-window) open-router-window)
            (delete-windows-on "*OpenRouter*")
          (select-window open-router-window))
      (progn
        (gptel "*OpenRouter*")
        (display-buffer "*OpenRouter*"
                        '((display-buffer-pop-up-window)
                          (window-parameters . ((split-window . t)
                                                (window-width . 0.5)))))
        (select-window (get-buffer-window "*OpenRouter*"))))))

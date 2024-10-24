(use-package emacs
  :ensure nil
  :custom
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
  (xref-search-program 'ripgrep) ; project-find-regexp
  (grep-command "rg -nS --no-heading ")
  (grep-use-null-device nil)
  (project-switch-commands '((project-find-file "Find file" "f")
                             (project-find-dir "Find dir" "d")
                             (project-dired "Dired" "D")
                             (project-find-regexp "g")
                             (magit-project-status "Magit" "m")))
  (c-default-style "linux")
  (c-basic-offset 4)
  :init
  (setq gc-cons-threshold most-positive-fixnum)
  (set-frame-font "Iosevka 18" nil t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when scroll-bar-mode
    (scroll-bar-mode -1))
  (global-auto-revert-mode 1)
  (indent-tabs-mode -1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (modify-coding-system-alist 'file "" 'utf-8)
  :config
  (setq custom-file (locate-user-emacs-file "~/.emacs.d/custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x f"))
  (global-unset-key (kbd "C-x C-c"))
  (global-set-key (kbd "<pinch>") 'ignore)
  (global-set-key (kbd "<C-wheel-up>") 'ignore)
  (global-set-key (kbd "<C-wheel-down>") 'ignore)
  (global-set-key (kbd "C-M-8") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-M-9") 'enlarge-window)
  (global-set-key (kbd "C-M-0") 'shrink-window)
  (global-set-key (kbd "C-M--") 'shrink-window-horizontally)
  (global-set-key (kbd "C-c c") 'compile)
  (global-set-key (kbd "C-c r") 'recompile)
  (global-set-key (kbd "C-q") 'query-replace)
  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#"))) ;; allow hash to be entered
  (global-set-key (kbd "s-f") 'forward-sexp)
  (global-set-key (kbd "s-b") 'backward-sexp)
  (global-set-key (kbd "s-n") 'forward-list)
  (global-set-key (kbd "s-p") 'backward-list)
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "M-s r") 'grep)
  (global-set-key (kbd "C-x 2") (lambda () (interactive) (split-window-vertically) (other-window 1)))
  (global-set-key (kbd "C-x 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))
  (add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)
  (add-hook 'occur-hook '(lambda () (switch-to-buffer-other-window "*Occur*")))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/elpa/"))
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'display-buffer-alist
               '("*shell" (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.45)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold (* 16 1024 1024))))
  (add-hook 'emacs-startup-hook
            (lambda () (message "Emacs loaded in %s with %d garbage collections."
                                (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done))))

(use-package gruber-darker-theme
  :ensure t
  :config (load-theme 'gruber-darker t))

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package wgrep
  :ensure t
  :defer t)

(use-package org
  :ensure nil
  :defer t
  :custom (org-log-done t))

(use-package dired
  :ensure nil
  :defer t
  :commands (dired)
  :hook (dired-mode . auto-revert-mode)
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("-" . dired-up-directory)))

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
  :hook
  (ibuffer-mode . hl-line-mode)
  :config
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil))

(defun close-all-but-current-buffer ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(use-package whitespace
  :ensure t
  :defer t
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-line-column 80) ;; limit line length
  (whitespace-style
   '(face trailing tabs indentation::space empty indention spaces trailing space-mark space-after-tab space-before-tab tab-mark)))

(use-package avy
  :ensure t
  :defer t
  :bind ("M-j" . avy-goto-char-2))

(use-package move-text
  :defer t
  :ensure t
  :bind ("M-p" . 'move-text-up)
  ("M-n" . 'move-text-down))

(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package multiple-cursors
  :ensure t
  :defer t
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

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config (exec-path-from-shell-initialize))

(use-package vterm
  :ensure t
  :defer t
  :bind ("M-`" . vterm)
  (:map vterm-mode-map
        ("C-y" . vterm-yank))
  :config
  (vterm-max-scrollback 10000))

(defun toggle-shell (shell-str shell)
  "Close the current buffer if it is open, otherwise open a new one!"
  (interactive)
  (if (string-equal shell-str major-mode)
      (bury-buffer)
    (funcall shell)))

(defun shell-toggle () (interactive) (toggle-shell "shell-mode" 'shell))
(global-set-key (kbd "C-`") 'shell-toggle)

(use-package ido
  :ensure nil
  :defer t
  :init
  (ido-mode 1)
  :custom
  (ido-everywhere t)
  (ido-enable-flex-matching t)
  (ido-use-url-at-point nil)
  (ido-max-window-height 1))

(use-package ido-completing-read+
  :ensure t
  :defer t
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :ensure t
  :defer t
  :bind ("M-x" . smex))

(use-package marginalia
  :ensure t
  :defer t
  :init
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(use-package corfu
  :ensure t
  :defer t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map ("TAB" . corfu-complete))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-quit-no-match t))

(use-package cape
  :ensure t
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package savehist
  :ensure t
  :defer t
  :init
  (savehist-mode))

(use-package magit
  :ensure t
  :defer t
  :bind
  (("C-c m s" . magit-status)
   ("C-c m l" . magit-log)
   ("C-c m b" . magit-blame)))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
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
  (eglot-sync-connect nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
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
  :bind
  ("M-g f" . flymake-show-buffer-diagnostics))

(use-package python
  :ensure nil
  :defer t
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-interpreter "ipython")
  (python-shell-completion-native-enable nil))

(use-package blacken
  :ensure t
  :defer t)

(use-package py-isort
  :ensure t
  :defer t)

(use-package pyvenv
  :ensure t
  :defer t)

(use-package editorconfig
  :ensure t
  :defer t
  :config
  (editorconfig-mode 1))

(use-package sqlformat
  :ensure t
  :defer t
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u1")))

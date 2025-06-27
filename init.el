;; Startup metrics
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; ==============================
;; Early Initialization Settings
;; ==============================

;; Increase startup speed
(setq gc-cons-threshold (* 1024 1024 100)  ; 100 MiB
      gc-cons-percentage 0.6
      package-enable-at-startup nil)

;; Frame Appearance and Behavior
(setq frame-title-format '("%f")
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      ns-pop-up-frames nil
      ns-use-proxy-icon nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))  ; Maximized but not fullscreen
(add-to-list 'default-frame-alist '(background-color . "#181818"))

;; Basic Settings
(setq inhibit-startup-screen t
      inhibit-startup-message t
      ring-bell-function 'ignore
      custom-safe-themes t
      use-dialog-box nil
      use-short-answers t
      inhibit-compacting-font-caches t
      vc-handled-backends '(Git))

;; Default to UTF-8 for all file operations
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)

;; Input handling
(setq mac-left-control-modifier 'control
      mac-right-control-modifier 'meta
      mac-option-modifier 'none
      mac-command-modifier 'super)

;; Avoid initial flash of light theme
(defun avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs."
  (set-face-attribute 'default nil :background "#181818" :foreground "#e4e4ef")
  (setq mode-line-format nil))
(avoid-initial-flash-of-light)

;; Disable UI elements for faster startup and cleaner look
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

;; Warning levels
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

;; Package archives
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Custom file
(setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

;; ==============================
;; Package Initialization
;; ==============================

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; ==============================
;; Main Configuration
;; ==============================

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

(use-package emacs
  :ensure nil
  :custom
  (use-package-compute-statistics t)
  (truncate-lines nil)
  (comp-async-report-warnings-errors nil)
  (comp-deferred-compilation t)
  (package-install-upgrade-built-in t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (treesit-font-lock-level 4)
  (make-backup-files nil)
  (custom-safe-themes t)
  (tab-always-indent 'complete)
  (electric-pair-preserve-balance nil)
  (vc-follow-symlinks t)
  (resize-mini-windows nil)
  (tab-width 4)
  (display-line-numbers 'nil)
  (delete-selection-mode 1)
  (history-length 500)
  (lazy-highlight-initial-delay 0)
  (help-window-select t)
  (completion-auto-select t)
  (set-mark-command-repeat-pop t)
  (isearch-lazy-count t)
  (xref-search-program 'ripgrep)
  (grep-command "rg -nS --no-heading ")
  (grep-use-null-device nil)
  (project-switch-commands '((project-find-file "Find file" "f")
                             (project-find-dir "Find dir" "d")
                             (project-dired "Dired" "D")
                             (project-vterm "t")
                             (project-home "Home" "h")
                             (project-fzf "fzf", "z")
                             (project-find-regexp "g")
                             (magit-project-status "Magit" "m")))
  (c-basic-offset 4)
  (indent-tabs-mode nil)
  (smerge-command-prefix "C-c v")
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-interpreter "ipython")
  (python-shell-completion-native-enable nil)
  (comint-process-echoes t)
  :init
  (auth-source-pass-enable)
  (global-auto-revert-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (savehist-mode 1)
  (set-frame-font "Iosevka 20" nil t)
  (global-completion-preview-mode 1)
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
   ("C-M-l" . mode-line-other-buffer)
   ("C-M--" . shrink-window-horizontally)
   ("C-M-0" . shrink-window)
   ("C-M-8" . enlarge-window-horizontally)
   ("C-M-9" . enlarge-window)
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
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))
  (add-hook 'html-mode-hook (lambda () (local-unset-key (kbd "M-o"))))
  (add-to-list 'display-buffer-alist '("*shell" (display-buffer-in-side-window) (side . right) (window-width . 0.45))))

(use-package gruber-darker-theme
  :ensure t
  :demand t
  :config (load-theme 'gruber-darker))

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-ask-about save nil)
  :bind
  ("C-c c" . compile)
  ("C-c r" . recompile)
  :init
  (defun compile-completing-read-history ()
    "Insert compile command from history using `completing-read'."
    (interactive)
    (let ((enable-recursive-minibuffers t))
      (let ((command (completing-read "Compile history: " compile-history)))
        (when command
          (delete-minibuffer-contents)
          (insert command)))))
  :config
  (define-key minibuffer-local-map (kbd "M-r") 'compile-completing-read-history))

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
  (org-directory "~/.me/org")
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

(use-package dired-ranger
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))


(defun project-home()
  "Open dired in the root directory of the current project."
  (interactive)
  (let ((root (project-root (project-current t))))
    (dired root)))

(use-package ibuffer
  :ensure t
  :defer t
  :bind ("C-x C-b" . ibuffer)
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
  (whitespace-line-column 80)
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

(use-package vterm
  :defer t
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

(use-package eshell
  :init
  (setq eshell-save-history-on-exit nil)
  :config
  (defun eshell-append-history ()
    "Call `eshell-write-history' with the `append' parameter set to `t'."
    (when eshell-history-ring
      (let ((newest-cmd-ring (make-ring 1)))
        (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
        (let ((eshell-history-ring newest-cmd-ring))
          (eshell-write-history eshell-history-file-name t)))))
  (add-hook 'eshell-pre-command-hook #'eshell-append-history))

(use-package em-hist
  :ensure nil
  :defer t
  :init
  (defun eshell-completing-read-history ()
    "History selection via completing-read when M-r is pressed."
    (interactive)
    (let* ((history (ring-elements eshell-history-ring))
           (selected-command (when history
                               (completing-read "Eshell history: " history nil t))))
      (when selected-command
        (insert selected-command))))
  :config
  (setq eshell-hist-ignoredups t
        eshell-history-file-name "~/.zsh_history"
        eshell-history-size 10000)
  (keymap-unset eshell-hist-mode-map "M-r" t)
  (define-key eshell-mode-map (kbd "M-r") 'eshell-completing-read-history))

(use-package ido
  :ensure nil
  :demand t
  :config (ido-mode 1)
  :custom
  (ido-everywhere t)
  (ido-enable-flex-matching t)
  (ido-use-url-at-point nil)
  (ido-max-window-height 1))

(use-package ido-completing-read+
  :ensure t
  :after ido
  :config (ido-ubiquitous-mode 1))

(use-package amx
  :ensure t
  :config (amx-mode 1))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package cape
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions
               (cape-capf-super #'cape-file #'cape-dabbrev #'cape-keyword #'cape-abbrev) t))

(use-package magit
  :ensure t
  :defer t
  :bind
  (("C-c m s" . magit-status)
   ("C-c m l" . magit-log)
   ("C-c m b" . magit-blame))
  :custom (magit-process-finish-apply-ansi-colors t))

(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package editorconfig
  :ensure t
  :defer t
  :config (editorconfig-mode 1))

(use-package pyvenv
  :ensure t
  :defer t)

(use-package dape
  :ensure t
  :defer t
  :hook (after-init . dape-breakpoint-load)
  :config
  (dape-breakpoint-global-mode))

(use-package emacos
  :ensure nil
  :load-path "firstparty/emacos")

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

(defun toggle-shell (shell-str shell)
  (interactive)
  (if (string-equal shell-str major-mode)
      (quit-window)
    (let ((display-buffer-alist
           '((".*" (display-buffer-pop-up-window)
              (window-width . 0.45)))))
      (funcall shell))))
(global-set-key (kbd "C-z C-z") (lambda () (interactive) (toggle-shell "vterm-mode" 'vterm)))
(global-set-key (kbd "C-z C-s") (lambda () (interactive) (toggle-shell "shell-mode" 'shell)))
(global-set-key (kbd "C-z C-e") (lambda () (interactive) (toggle-shell "eshell-mode" 'eshell)))

(provide 'init)

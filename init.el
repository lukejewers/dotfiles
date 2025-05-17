(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init
  (exec-path-from-shell-initialize))

(use-package emacs
  :ensure nil
  :custom
  (use-package-compute-statistics t)
  (truncate-lines t)
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
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-ask-about save nil)
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
                             (project-find-regexp "Vterm" "g")
                             (magit-project-status "Magit" "m")))
  (c-basic-offset 4)
  (indent-tabs-mode nil)
  (smerge-command-prefix "C-c v")
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-interpreter "ipython")
  (python-shell-completion-native-enable nil)
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

(use-package em-hist
  :ensure nil
  :defer t
  :init
  (defun eshell-history-ido ()
    "Present Eshell history using Ido for selection when M-r is pressed in Eshell."
    (interactive)
    (let* ((history (ring-elements eshell-history-ring))
           (selected-command (when history
                               (ido-completing-read "Eshell history: " history nil t))))
      (when selected-command
        (insert selected-command))))
  :config
  (setq eshell-hist-ignoredups t
        eshell-history-file-name "~/.zsh_history"
        eshell-history-size 10000)
  (keymap-unset eshell-hist-mode-map "M-r" t)
  (define-key eshell-mode-map (kbd "M-r") 'eshell-history-ido))

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
  :init (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package dape
  :ensure t
  :defer t
  :hook
  (after-init . dape-breakpoint-load)
  :config
  (dape-breakpoint-global-mode)
  (setq dape-buffer-window-arrangement 'right))

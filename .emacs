;;;; startup ;;;;
;; minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook(lambda () (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done)))

;;; disable ;;;
(setq package-enable-at-startup nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq ring-bell-function 'ignore)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom-vars.el")

;;; initialize package sources ;;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; auto-package-update
(use-package auto-package-update
  :defer t
  :custom
  (auto-package-update-interval 28)
  (auto-package-update-prompt-before-update t)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;;; appearance ;;;;
(load-theme 'gruber-darker t)
(setq custom-safe-themes t)
(set-frame-font "Iosevka 20" nil t)
(setq-default display-line-numbers 'relative)

;; show absolute path in frame title
(setq frame-title-format
      '(:eval (if buffer-file-name  (abbreviate-file-name buffer-file-name) "%b")))

;;;; defaults ;;;;
(setq mac-left-option-modifier 'control
      mac-left-control-modifier 'control
      mac-right-control-modifier 'meta
      mac-right-option-modifier 'meta
      mac-command-modifier 'super)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
(delete-selection-mode 1)
(setq vc-follow-symlinks t)
(setq-default indent-tabs-mode nil)
(setq-default
 truncate-lines t
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;; global keybindings
(global-set-key (kbd "C-M-8") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-9") 'shrink-window)
(global-set-key (kbd "C-M-0") 'enlarge-window)
(global-set-key (kbd "C-M--") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "M-g t") 'beginning-of-buffer)
(global-set-key (kbd "M-g b") 'end-of-buffer)

;; global keyunbindings
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x C-c"))

;; allow hash to be entered
(global-set-key (kbd "M-3")
                '(lambda ()
                   (interactive)
                   (insert "#")))

;; exec path from shell
(use-package
  exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; dired
(use-package
  dired
  :ensure nil
  :commands (dired)
  :init (add-hook 'dired-mode-hook 'auto-revert-mode)
  :bind (:map dired-mode-map
              ("-" . dired-up-directory)))
(put 'dired-find-alternate-file 'disabled nil)
(require 'dired-x)

;; ibuffer
(use-package
  ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . hl-line-mode)
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil))

;; whitespace
(whitespace-mode 1)
(customize-set-variable 'indent-tabs-mode nil)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-line-column 250)

;; editor config
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; move text
(use-package
  move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; org
(setq org-startup-truncated nil)

;; avy
(use-package
  avy
  :config
  (global-set-key (kbd "M-j") 'avy-goto-char-timer))

;; expand region
(use-package
  expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple cursors
(use-package
  multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-|") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:") 'mc/skip-to-previous-like-this)

;; select line
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))
(global-set-key (kbd "C-;") 'select-current-line)

;; duplicate line
(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))
(global-set-key (kbd "C-,") 'duplicate-line)

;; copy full path
(defun copy-full-path-to-kill-ring ()
  "Copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))
(global-set-key (kbd "C-±") 'copy-full-path-to-kill-ring)

;;;; terminals ;;;;;
;; vterm
(use-package vterm
  :ensure t
  :config (add-hook 'vterm-mode-hook (lambda ()
                                       (menu-bar--display-line-numbers-mode-none)
                                       (message nil))))

(define-key vterm-mode-map (kbd "M-f") 'vterm-send-M-f)
(define-key vterm-mode-map (kbd "M-b") 'vterm-send-M-b)
(define-key vterm-mode-map (kbd "M-p") 'vterm-send-M-p)
(define-key vterm-mode-map (kbd "M-n") 'vterm-send-M-n)

;; open terminals 1/3 screen size
(defun term-open (term-fn term-name)
  "Opens up a new terminal in the directory associated with the
current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name)) default-directory))
         (height (/ (window-total-height) 3))
         (dirname (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (funcall term-fn "new")
    (rename-buffer (concat "*" term-name ": " dirname "*"))))

(defun shell-open () (interactive) (term-open 'shell "shell"))
(global-set-key (kbd "C-x t s") 'shell-open)

(defun eshell-open () (interactive) (term-open 'eshell "eshell"))
(global-set-key (kbd "C-x t e") 'eshell-open)

(defun vterm-open () (interactive) (term-open 'vterm "vterm"))
(global-set-key (kbd "C-x t v") 'vterm-open)

;;;; completion ;;;;
;; ido
(ido-mode 1)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching t)
(use-package ido-completing-read+)
(ido-ubiquitous-mode 1)

;; smex
(use-package smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; corfu
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init (global-corfu-mode))

;;;; version control ;;;;
;; magit
(use-package
  magit
  :bind (("C-c m s" . magit-status)
         ("C-c m l" . magit-log)))

;;;; languages ;;;;
;; tree-sitter
(use-package tree-sitter-langs :ensure t)
(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; eglot
(use-package eglot
  :init (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure))

;; flymake
(define-key flymake-mode-map (kbd "C-c l p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c l n") 'flymake-goto-next-error)

;; python shell
(setq python-shell-interpreter "python3")
(setq python-shell-completion-native-enable nil)

;; blacken
(use-package blacken
  :hook (python-mode . blacken-mode))

;; c
(setq c-default-style "linux"
      c-basic-offset 4)

;; scss-mode
(use-package
  scss-mode
  :config (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))

;; sqlformat
(use-package sqlformat
  :config (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

;; sly
(use-package
  sly)
(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")

;; paredit
(use-package paredit
  :init (autoload 'enable-paredit-mode "paredit" t)
  (add-hook 'emacs-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

;;; garbage collection ;;;
(setq gc-cons-threshold (* 1024 1024 100))

;;; .emacs ends here

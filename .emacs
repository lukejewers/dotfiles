;; minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; lower threshold to speed up garbage collection
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))


;; initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; appearance
(setq inihibit-startup-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq ring-bell-function 'ignore)
(load-theme 'gruber-darker t)
(setq custom-safe-themes t)
(set-frame-font "Iosevka 20" nil t)
(setq-default display-line-numbers 'relative)

;; defaults
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
(delete-selection-mode 1)
(setq-default make-backup-files nil
	      indent-tabs-mode nil)

;; global keybindings
(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;; dired
(require 'dired-x)
(use-package dired-single)

;; eshell
(defun eshell-here ()
      "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
      (interactive)
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             (height (/ (window-total-height) 3))
             (name   (car (last (split-string parent "/" t)))))
        (split-window-vertically (- height))
        (other-window 1)
        (eshell "new")
        (rename-buffer (concat "*eshell: " name "*"))

        (insert (concat "ls"))
        (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

;; allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; expand region
(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; company
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

;; org mode
(require 'org)
(setq org-log-done t)

;; whitespace
(whitespace-mode 1)

;; multiple cursors
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-|")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; move text
(use-package move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

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

;; yasnippet
(use-package yasnippet)
(yas-global-mode 1)

;; flycheck
(use-package flycheck
  :init (global-flycheck-mode))

;; eglot
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-h ." . display-local-help)
              ("C-h d" . eldoc-doc-buffer)
              ("M-RET" . eglot-code-actions))
  :ensure t
  :hook ((c-mode  . eglot-ensure)
         (js-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :commands (eglot eglot-ensure))

;; python shell
(setq python-shell-interpreter "python3")
(setq python-shell-completion-native-enable nil)

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode))
  (("\\.jsx\\'" . web-mode))
  :config (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-pairing t))

;; sass-mode
(use-package sass-mode
  :config (add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
  :ensure t)

;; tide
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; emmet
(use-package emmet-mode)

;; haskell
(use-package haskell-mode :ensure t :mode "\\.hs\\'")

;; rust
(use-package rust-mode    :ensure t :mode "\\.rs\\'")

;; ido
(ido-mode 1)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching t)
(use-package ido-completing-read+)
(ido-ubiquitous-mode 1)
(use-package ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; smex
(use-package smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; paredit
(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-mode-hook       'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(add-hook 'clojure-mode-hook          'enable-paredit-mode)
(add-hook 'racket-mode-hook           'enable-paredit-mode)

;; magit
(use-package magit)
(setq magit-auto-revert-mode nil)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

;; sly
(use-package sly)
(setq inferior-lisp-program "/usr/local/bin/sbcl")

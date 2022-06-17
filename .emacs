;;;; startup ;;;;
;; minimize garbage collection during startup
(setq gc-cons-threshold (* 50 1000 1000))

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
(when (file-exists-p custom-file) (load custom-file))

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
(setq-default
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;; global keybindings
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "C-M-9") 'shrink-window)
(global-set-key (kbd "C-M-0") 'enlarge-window)
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

;;;; terminals ;;;;;
;; eshell
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name)) default-directory))
         (height (/ (window-total-height) 3))
         (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))
(global-set-key (kbd "C-x t e") 'eshell-here)

;; shell
(global-set-key (kbd "C-x t s") 'shell)

;; vterm
(use-package
  vterm
  :ensure t)
(add-hook 'vterm-mode-hook (lambda ()
                             (menu-bar--display-line-numbers-mode-none)
                             (message nil)))
;; vterm toggle
(use-package
  vterm-toggle
  :bind ("C-x v" . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _)
                   (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

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

;; company
(use-package
  company
  :ensure nil
  :init (global-company-mode t))

;;;; version control ;;;;
;; magit
(use-package
  magit
  :bind (("C-c m s" . magit-status)
         ("C-c m l" . magit-log)))

;;;; languages ;;;;
;; lsp-mode
(use-package
  lsp-mode
  :defer t
  :init (setq lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
              ("C-c l d" . lsp-describe-thing-at-point)
              ("C-c l a" . lsp-execute-code-action))
  :hook ((python-mode . lsp-deferred)
          (js-mode . lsp-deferred)
          (typescript-mode . lsp-deferred)
          (rust-mode . lsp-deferred)
          (c-mode . lsp-deferred))
  :custom
  (gc-cons-threshold 100000000)
  (lsp-enable-completion-at-point t)
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)
  (lsp-prefer-flymake nil)
  (lsp-enable-file-watchers nil))

;; lsp-ui
(use-package
  lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-signature-render-documentation nil))

;; flycheck
(use-package
  flycheck
  :init (global-flycheck-mode))

;; pyright
(use-package
  lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; pyvenv
(use-package pyvenv
  :init (setenv "WORKON_HOME" "~/.virtualenvs/")
  :config (pyvenv-mode t))

;; python shell
(setq python-shell-interpreter "python3")
(setq python-shell-completion-native-enable nil)

;; blacken
(use-package blacken
  :hook (python-mode . blacken-mode))

;; rust-mode
(use-package
  rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . lsp-deferred))
  :config (setq rust-format-on-save t))

;; web-mode
(use-package
  web-mode
  :mode (("\\.html?\\'" . web-mode))
  (("\\.jsx\\'" . web-mode))
  :config (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-pairing t))

;; scss-mode
(use-package
  scss-mode
  :config (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))

;; tide
(use-package
  tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;; (before-save . tide-format-before-save)
         ))

;; sly
(use-package
  sly)
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; yasnippet
(use-package
  yasnippet)
(yas-global-mode 1)

;; paredit
(use-package paredit)
(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'racket-mode-hook 'enable-paredit-mode)

;;; garbage collection ;;;
(setq gc-cons-threshold (* 2 1000 1000))

;;; .emacs ends here

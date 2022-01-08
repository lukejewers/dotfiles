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
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; whitespace
(whitespace-mode 1)

;; multiple cursors
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
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

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((python-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (tide-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (c-mode . lsp-deferred))
  :config (setq gc-cons-threshold 100000000)
          (setq lsp-completion-provider :capf)
          (setq lsp-idle-delay 0.500)
          (setq lsp-log-io nil)
          (setq lsp-prefer-flymake nil)
          (setq lsp-enable-file-watchers nil))

;; lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :config (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-signature-render-documentation nil))


;; lsp-pyright
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

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

;; ido
(ido-mode 1)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching t)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tide which-key web-mode-edit-element use-package-hydra typescript-mode treemacs smex sly paredit multiple-cursors move-text magit macrostep lsp-pyright ido-completing-read+ gruber-darker-theme flycheck expand-region exec-path-from-shell elpy eglot blacken)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

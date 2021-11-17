;; appearance
(setq inihibit-startup-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq ring-bell-function 'ignore)
(load-theme 'atom-one-dark t)
(set-frame-font "Iosevka 16" nil t)
(setq-default display-line-numbers 'relative)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)

;; global keybindings
(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; whitespace
(whitespace-mode 1)

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; lsp mode
(require 'lsp-mode)
(add-hook 'js-mode-hook 'lsp)
(add-hook 'python-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'lsp)

;; ido
(ido-mode 1)
(setq ido-everywhere 1)
(setq ido-enable-flex-matching t)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; smex
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; paredit
(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(add-hook 'clojure-mode-hook          'enable-paredit-mode)
(add-hook 'racket-mode-hook           'enable-paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(paredit smex ido-completing-read+ lsp-mode atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

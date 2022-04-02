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

;;;; appearance ;;;;
(load-theme 'gruber-darker t)
(setq custom-safe-themes t)
(set-frame-font "Iosevka 20" nil t)
(setq-default display-line-numbers 'relative)

;; mood-line
(use-package
  mood-line
  :init (mood-line-mode))

;; rainbow-mode
(use-package
  rainbow-mode
  :ensure t)

;;;; defaults ;;;;
(setq mac-left-option-modifier 'super
      mac-left-control-modifier 'control
      mac-right-control-modifier 'meta
      mac-right-option-modifier 'meta)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
(delete-selection-mode 1)
(setq-default make-backup-files nil)

;; global keybindings
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "C-M-9") 'shrink-window)
(global-set-key (kbd "C-M-0") 'enlarge-window)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)

;; global keyunbindings
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x f"))

;; allow hash to be entered
(global-set-key (kbd "M-3")
                '(lambda ()
                   (interactive)
                   (insert "#")))

;; exec path from shell
(use-package
  exec-path-from-shell
  :config (if (eq system-type 'darwin)
              (exec-path-from-shell-initialize)))

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

;; org mode
(require 'org)
(setq org-log-done t)

;; whitespace
(whitespace-mode 1)
(customize-set-variable 'indent-tabs-mode nil)
(add-hook 'before-save-hook #'whitespace-cleanup)
(setq whitespace-line-column 250)

;; editor config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; move text
(use-package
  move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

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
(global-set-key (kbd "C-!") 'eshell-here)

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
  :config
  (global-set-key (kbd "C-x t") 'vterm)
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _)
                   (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

;;;; completion ;;;;
;; vertico
(use-package
  vertico
  :ensure t
  :config (vertico-mode))

;; marginalia
(use-package
  marginalia
  :ensure t
  :config (marginalia-mode))

;; orderless
(use-package
  orderless
  :ensure t
  :config (setq completion-styles '(orderless)))

;; consult
(use-package
  consult
  :ensure t)

;; embark
(use-package
  embark
  :ensure t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" nil (window-parameters (mode-line-format . none)))))

;; embark consult
(use-package
  embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; company
(use-package
  company
  :ensure
  :init (global-company-mode t))

;; ido
;; (use-package ido
;; :config
;; (ido-mode +1)
;; (setq ido-everywhere t
;; ido-enable-flex-matching t))
;; (use-package ido-vertical-mode
;; :config
;; (ido-vertical-mode +1)
;; (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))
;; (use-package ido-completing-read+ :config (ido-ubiquitous-mode +1))
;; (use-package flx-ido :config (flx-ido-mode +1))

;; smex
;; (use-package
;; smex)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;; version control ;;;;
;; magit
(use-package
  magit)
(setq magit-auto-revert-mode nil)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

;;;; languages ;;;;
;; lsp-mode
(use-package
  lsp-mode
  :ensure t
  :defer t
  :init (setq lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action))
  :hook ((python-mode
          js-mode
          typescript-mode
          web-mode
          css-mode
          rust-mode
          c-mode) . lsp-deferred)
  :config
  (setq gc-cons-threshold 100000000)
  (setq lsp-prefer-capf t)
  (setq lsp-completion-provider
        :capf)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-file-watchers nil))

;; lsp-ui
(use-package
  lsp-ui
  :commands lsp-ui-mode
  :config (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-signature-render-documentation nil))

;; dap
(use-package
  dap-mode
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup)
  (require 'dap-chrome)
  (require 'dap-python))

;; flycheck
(use-package
  flycheck
  :init (global-flycheck-mode))

;; pyright
(use-package
  lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; python shell
(setq python-shell-interpreter "python3")
(setq python-shell-completion-native-enable nil)

;; rust
(use-package
  rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . lsp-deferred))
  :config (setq rust-format-on-save t))

;; web-mode
(use-package
  web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode))
  (("\\.jsx\\'" . web-mode))
  :config (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-pairing t))

;; sass-mode
(use-package
  sass-mode
  :config (add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
  :ensure t)

;; tide
(use-package
  tide
  :ensure t
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

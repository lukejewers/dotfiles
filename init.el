;; ==================== ;;
;; Early Initialization ;;
;; ==================== ;;

(setq gc-cons-threshold (* 1024 1024 100)
      gc-cons-percentage 0.6
      package-enable-at-startup nil)

(defun my-display-startup-time ()
  (message "Emacs loaded in %.2f seconds with %d garbage collections"
           (float-time (time-subtract after-init-time before-init-time)) gcs-done))
(add-hook 'emacs-startup-hook #'my-display-startup-time)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 1024 1024 16))))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(set-face-attribute 'default nil :background "#181818" :foreground "#e4e4ef")
(set-language-environment "UTF-8")

(setq default-frame-alist
      `((ns-transparent-titlebar . t)
        (background-color . "#181818")
        (font . ,(if (eq system-type 'darwin) "Iosevka 20" "Iosevka 18"))
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)))

(add-to-list 'display-buffer-alist
             '((or . ((derived-mode . occur-mode)
                      (derived-mode . grep-mode)
                      (derived-mode . compilation-mode)
                      (derived-mode . log-view-mode)
                      (derived-mode . help-mode)))
               (display-buffer-reuse-mode-window display-buffer-below-selected)
               (body-function . select-window)))

(setq frame-title-format '("%f")
      frame-resize-pixelwise t
      window-resize-pixelwise t
      frame-inhibit-implied-resize t
      ns-pop-up-frames nil
      ns-use-proxy-icon nil
      inhibit-startup-screen t
      inhibit-startup-message t
      ring-bell-function 'ignore
      custom-safe-themes t
      use-dialog-box nil
      use-short-answers t
      inhibit-compacting-font-caches t
      mode-line-format nil
      warning-minimum-level :error
      warning-suppress-types '((lexical-binding))
      custom-file (locate-user-emacs-file "custom-vars.el")
      package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(load custom-file t t)
(when (eq system-type 'darwin)
  (setq mac-left-control-modifier 'control
        mac-option-modifier 'meta
        mac-command-modifier 'super))

;; ================== ;;
;; Main Configuration ;;
;; ================== ;;

(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (c-ts-mode-indent-offset 4)
  (comint-completion-addsuffix nil)
  (comint-process-echoes t)
  (completion-auto-select t)
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  (delete-by-moving-to-trash t)
  (delete-pair-blink-delay 0.1)
  (delete-pair-push-mark t)
  (duplicate-line-final-position 1)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-preserve-balance nil)
  (help-window-select t)
  (highlight-nonselected-windows nil)
  (indent-tabs-mode nil)
  (lazy-highlight-initial-delay 0)
  (make-backup-files nil)
  (mode-line-collapse-minor-modes t)
  (package-install-upgrade-built-in t)
  (pcomplete-termination-string "")
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-completion-native-enable nil)
  (resize-mini-windows nil)
  (kill-do-not-save-duplicates t)
  (save-interprogram-paste-before-kill t)
  (set-mark-command-repeat-pop t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (use-package-always-ensure t)
  (use-package-compute-statistics t)
  (whitespace-line-column 80)
  (xref-search-program 'ripgrep)
  :config
  (blink-cursor-mode 0)
  (delete-selection-mode 1)
  (editorconfig-mode 1)
  (electric-pair-mode 1)
  (global-auto-revert-mode 1)
  (global-completion-preview-mode 1)
  (pixel-scroll-precision-mode 1)
  (put 'narrow-to-region 'disabled nil)
  (savehist-mode 1)
  (show-paren-mode 1)
  (tooltip-mode 0)
  (with-eval-after-load 'c-ts-mode (define-key c-ts-mode-map (kbd "C-c .") nil))
  :bind
  (("<C-wheel-down>" . ignore)
   ("<C-wheel-up>" . ignore)
   ("<pinch>" . ignore)
   ("C-," . duplicate-line)
   ("C-." . (lambda () (interactive) (my-toggle-buffer 'ghostel-mode #'ghostel 0.45)))
   ("C-M--" . shrink-window-horizontally)
   ("C-M-0" . shrink-window)
   ("C-M-8" . enlarge-window-horizontally)
   ("C-M-9" . enlarge-window)
   ("C-c d p" . delete-pair)
   ("C-c q" . query-replace-regexp)
   ("C-c Q" . my-replace-regexp-no-move)
   ("C-x 2" . (lambda () (interactive) (split-window-below) (other-window 1)))
   ("C-x 3" . (lambda () (interactive) (split-window-right) (other-window 1)))
   ("C-x C-b" . ibuffer)
   ("C-x C-c" . nil)
   ("C-x f" . find-file-at-point)
   ("C-x m" . nil)
   ("C-z" . nil)
   ("M-3" . (lambda () (interactive) (insert "#")))
   ("M-o" . other-window))
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (before-save . whitespace-cleanup)
  (compilation-filter . ansi-color-compilation-filter)
  (html-mode . (lambda () (local-unset-key (kbd "M-o"))))
  (ibuffer-mode . hl-line-mode)
  (shell-mode . (lambda () (setq-local scroll-margin 1))))

(use-package gruber-darker-theme
  :hook (after-init . (lambda () (load-theme 'gruber-darker t))))

(use-package ido
  :ensure nil
  :demand t
  :config (ido-mode 1)
  :custom
  (ido-auto-merge-work-directories-length -1)
  (ido-max-prospects 10)
  (ido-everywhere t)
  (ido-enable-flex-matching t)
  (ido-use-url-at-point nil))

(use-package ido-completing-read+
  :after ido
  :config (ido-ubiquitous-mode 1))

(use-package amx
  :hook (after-init . amx-mode))

(use-package transpose-frame
  :defer t
  :bind
  ("C-c w t" . transpose-frame)
  ("C-c w f" . flop-frame))

(use-package isearch
  :ensure nil
  :custom (isearch-lazy-count t)
  :bind (:map isearch-mode-map ("C-q" . isearch-query-replace-regexp)))

(use-package cape
  :preface
  (defun my-cape-setup-capf ()
    (add-hook 'completion-at-point-functions #'cape-file nil t)
    (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
    (add-hook 'completion-at-point-functions #'cape-keyword nil t))
  :hook
  ((prog-mode text-mode conf-mode) . my-cape-setup-capf))

(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  :bind
  ("C-c c" . compile)
  ("C-c r" . recompile))

(use-package grep
  :ensure nil
  :bind (("C-c s g" . grep)
         ("C-c s p" . my-project-grep)
         ("C-c s ." . my-project-grep-dwim))
  :custom
  (grep-command "rg -S --no-heading --color=never ")
  (grep-save-buffers t)
  (grep-use-headings t)
  (grep-use-null-device nil)
  :config
  (require 'project)
  (defun my-project-grep (&optional initial-input)
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (grep (read-shell-command "Grep project: "
                                (concat grep-command (or initial-input ""))
                                'grep-history))))
  (defun my-project-grep-dwim ()
    (interactive)
    (if-let* ((symbol (thing-at-point 'symbol t)))
        (let ((default-directory (project-root (project-current t))))
          (grep (concat grep-command
                        (shell-quote-argument symbol)
                        " .")))
      (call-interactively #'my-project-grep))))

(use-package org
  :ensure nil
  :custom
  (org-log-done t)
  (org-directory "~/me/org")
  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (setq tab-width 8))))

(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-do-revert-buffer t)
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package move-text
  :defer t
  :bind
  ("M-p" . move-text-up)
  ("M-n" . move-text-down))

(use-package expreg
  :defer t
  :bind
  ("C-=" . expreg-expand)
  ("C--" . expreg-contract))

(use-package multiple-cursors
  :defer t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c ." . mc/mark-all-like-this)
  ("C-c ," . mc/edit-lines))

(use-package ghostel
  :defer t
  :config
  (defun my-ghostel-send-backward-kill-word ()
    (interactive)
    (ghostel-send-string "\C-w"))
  (define-key ghostel-semi-char-mode-map
              (kbd "C-<backspace>")
              #'my-ghostel-send-backward-kill-word))

(use-package treesit
  :ensure nil
  :custom
  (treesit-extra-load-path '("~/.emacs.d/tree-sitter/"))
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
   '((c-mode          . c-ts-mode)
     (c++-mode        . c++-ts-mode)
     (css-mode        . css-ts-mode)
     (go-mode         . go-ts-mode)
     (js-mode         . js-ts-mode)
     (json-mode       . json-ts-mode)
     (python-mode     . python-ts-mode)
     (rust-mode       . rust-ts-mode)
     (swift-mode      . swift-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (yaml-mode       . yaml-ts-mode))))

(use-package magit
  :defer t
  :bind
  (("C-c m s" . magit-status)
   ("C-c m l" . magit-log)
   ("C-c m b" . magit-blame))
  :custom
  (when (eq system-type 'darwin)
    (magit-git-executable "/opt/homebrew/bin/git")))

(use-package gptel
  :defer t
  :ensure t
  :bind
  (("C-c g g" . gptel)
   ("C-c g a" . gptel-add)
   ("C-c g m" . gptel-menu))
  :config
  (setq gptel-model "moonshotai/kimi-k2.6"
        gptel-default-mode 'org-mode
        gptel-backend (gptel-make-openai "gptel"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :stream t
                        :key 'gptel-api-key
                        :models '("moonshotai/kimi-k2.6"
                                  "openai/gpt-5.5"
                                  "z-ai/glm-5.1"))))

;; ================ ;;
;; Custom Functions ;;
;; ================ ;;

(defun my-toggle-buffer (mode create-fn &optional window-width)
  "Toggle a buffer matching MODE, creating it with CREATE-FN if absent.
If WINDOW-WIDTH is a number, display the buffer in a popup window of that width."
  (interactive)
  (if (derived-mode-p mode)
      (quit-window)
    (let ((display-buffer-alist
           (when window-width
             `((".*" (display-buffer-pop-up-window)
                (window-width . ,window-width))))))
      (funcall create-fn))))

(defun my-replace-regexp-no-move ()
  "Call `replace-regexp` interactively without moving point."
  (interactive)
  (save-excursion (call-interactively 'replace-regexp)))

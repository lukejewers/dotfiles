(use-package emacs
  :ensure nil
  :init
  (setq gc-cons-threshold (* 1024 1024 100)
        gc-cons-percentage 0.6)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (set-face-attribute 'default nil :background "#181818" :foreground "#e4e4ef")
  (set-language-environment "UTF-8")
  (setq default-frame-alist
        `((ns-transparent-titlebar . t)
          (background-color . "#181818")
          (font .,(if (eq system-type 'darwin) "Iosevka 20" "Iosevka 18"))
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
  (setq frame-title-format '("%f — Emacs")
        frame-resize-pixelwise t
        window-resize-pixelwise t
        frame-inhibit-implied-resize t
        inhibit-startup-screen t
        inhibit-startup-message t
        ring-bell-function 'ignore
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
    (setq ns-pop-up-frames nil
          ns-use-proxy-icon nil
          mac-left-control-modifier 'control
          mac-option-modifier 'meta
          mac-command-modifier 'super))
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
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes t)
  (treesit-font-lock-level 4)
  (use-package-always-ensure t)
  (use-package-compute-statistics nil)
  (whitespace-line-column 80)
  (xref-search-program 'ripgrep)
  :config
  (blink-cursor-mode 0)
  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (put 'narrow-to-region 'disabled nil)
  (show-paren-mode 1)
  (tooltip-mode 0)
  (with-eval-after-load 'c-ts-mode (define-key c-ts-mode-map (kbd "C-c .") nil))
  :bind
  (("<C-wheel-down>" . ignore)
   ("<C-wheel-up>" . ignore)
   ("<pinch>" . ignore)
   ("C-," . duplicate-line)
   ("C-M--" . shrink-window-horizontally)
   ("C-M-0" . shrink-window)
   ("C-M-8" . enlarge-window-horizontally)
   ("C-M-9" . enlarge-window)
   ("C-c d p" . delete-pair)
   ("C-c q" . query-replace-regexp)
   ("C-c Q" . (lambda () (interactive) (save-excursion (call-interactively 'replace-regexp))))
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
  ((after-save . executable-make-buffer-file-executable-if-script-p)
   (before-save . delete-trailing-whitespace)
   (compilation-filter . ansi-color-compilation-filter)
   (emacs-startup . (lambda () (message "Emacs loaded in %.2f seconds with %d garbage collections"
                               (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
   (emacs-startup . (lambda () (setq gc-cons-threshold (* 1024 1024 16))))
   (emacs-startup . editorconfig-mode)
   (emacs-startup . global-auto-revert-mode)
   (emacs-startup . global-completion-preview-mode)
   (emacs-startup . pixel-scroll-precision-mode)
   (emacs-startup . savehist-mode)
   (html-mode . (lambda () (local-unset-key (kbd "M-o"))))
   (ibuffer-mode . hl-line-mode)))

(use-package gruber-darker-theme
  :config (load-theme 'gruber-darker t))

(use-package icomplete
  :ensure nil
  :custom
  (icomplete-compute-delay 0.0)
  :config
  (fido-mode 1)
  (with-eval-after-load 'gruber-darker-theme
    (custom-theme-set-faces
     'gruber-darker
     '(icomplete-first-match ((t (:foreground "#ffdd33" :weight bold))))
     '(icomplete-selected-match ((t (:foreground "#000000" :background "#ffdd33" :weight bold))))
     '(completions-highlight ((t (:background "#453d41")))))))

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
  (compilation-scroll-output nil)
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  :bind
  ("C-c c" . compile)
  ("C-c r" . recompile))

(use-package project
  :ensure nil
  :bind (("C-c f d" . my-fd-files)
         ("C-c f p" . my-project-fd-files))
  :config
  (defun my-fd-files (cmd)
    (interactive
     (list (read-shell-command "fd: " "fd -tf -p " 'shell-command-history)))
    (let ((buf (compilation-start cmd 'compilation-mode (lambda (_) "*fd*"))))
      (with-current-buffer buf
        (setq-local compilation-error-regexp-alist '(("^\\(.+\\)$" 1 nil nil 0)))
        (setq-local compilation-skip-threshold 0)
        (goto-char (point-min)))))
  (defun my-project-fd-files (cmd)
    (interactive
     (let* ((root (project-root (project-current t)))
            (default-directory root))
       (list (read-shell-command "fd project: " "fd -tf -p " 'shell-command-history))))
    (let* ((root (project-root (project-current t)))
           (default-directory root)
           (buf (compilation-start cmd 'compilation-mode (lambda (_) "*project fd*"))))
      (with-current-buffer buf
        (setq-local compilation-error-regexp-alist '(("^\\(.+\\)$" 1 nil nil 0)))
        (setq-local compilation-skip-threshold 0)
        (goto-char (point-min))))))

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
  :bind (("C-." . (lambda () (interactive)
                    (if (derived-mode-p 'ghostel-mode)
                        (quit-window)
                      (let ((display-buffer-alist
                             '((".*" (display-buffer-pop-up-window)
                                (window-width . 0.45)))))
                        (ghostel)))))
         :map ghostel-semi-char-mode-map
         ("C-<backspace>" . my-ghostel-backward-kill-word)
         ("C-k"  . my-ghostel-send-C-k-and-kill)
         :map project-prefix-map
         ("." . ghostel-project))
  :config
  (ghostel-comint-global-mode 1)
  (setopt ghostel-keymap-exceptions (cons "M-o" ghostel-keymap-exceptions))
  (defun my-ghostel-backward-kill-word ()
    (interactive)
    (kill-ring-save (save-excursion (backward-word) (point)) (point))
    (ghostel-send-key "backspace" "alt"))
  (defun my-ghostel-send-C-k-and-kill ()
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl")))

(use-package magit
  :defer t
  :bind
  (("C-c m s" . magit-status)
   ("C-c m l" . magit-log)
   ("C-c m b" . magit-blame))
  :custom
  (magit-git-executable
   (if (eq system-type 'darwin) "/opt/homebrew/bin/git" "git")))

(use-package gptel
  :defer t
  :ensure t
  :bind
  (("C-c g g" . (lambda () (interactive)
                  (if (bound-and-true-p gptel-mode)
                      (quit-window)
                    (gptel "*gptel*" nil nil t))))
   ("C-c g a" . gptel-add)
   ("C-c g m" . gptel-menu))
  :config
  (setq gptel-model 'deepseek/deepseek-v4-flash
        gptel-default-mode 'org-mode
        gptel-backend (gptel-make-openai "gptel"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :stream t
                        :key 'gptel-api-key
                        :models '("z-ai/glm-5.2"
                                  "moonshotai/kimi-k2.7-code"
                                  "deepseek/deepseek-v4-flash"
                                  "deepseek/deepseek-v4-pro"))))

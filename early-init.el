;; increase startup speed
(setq gc-cons-threshold (* 1024 1024 100)  ; 100 MiB
      gc-cons-percentage 0.6
      package-enable-at-startup nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; frame Appearance and Behavior
(setq frame-title-format '("%f")       ; Use buffer file name in title
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      ns-use-proxy-icon nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))  ; Maximized but not fullscreen

;; basic Settings
(setq inhibit-startup-screen t
      inhibit-startup-message t
      ring-bell-function 'ignore
      custom-safe-themes t
      use-dialog-box nil
      use-short-answers t
      inhibit-compacting-font-caches t
      vc-handled-backends '(Git))

;; default to UTF-8 for all file operations
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)

;; input handling
(setq mac-left-control-modifier 'control
      mac-right-control-modifier 'meta
      mac-command-modifier 'super)

;; avoid initial flash of light theme
(defun early-init/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs."
  (set-face-attribute 'default nil :background "#181818" :foreground "#e4e4ef")
  (setq mode-line-format nil))
(early-init/avoid-initial-flash-of-light)

;; disable UI elements for faster startup and cleaner look
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

;; warning levels
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

;; package archives
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; custom file
(setq custom-file (expand-file-name "custom-vars.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

(provide 'early-init)

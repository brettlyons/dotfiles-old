;;; package --- Summary
;;; Commentary:
;; My init.el.  Currently configured to automatically check for packages, and configure relevant pieces of those packages.

;;; Code:


;; package archives first
(require 'package)
(setq
 package-archives '(("marmalade" . "https://marmalade-repo.org/packages/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;; set up use-package to simplify this config
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(eval-when-compile
  (require 'use-package))


;; Turn on show-paren and electric-pair
(electric-pair-mode 1)
(show-paren-mode 1)
(display-time-mode t)

;; Spaces, not tabs
;; Tab width: 2
;; Indent Level: 2

(setq-default indent-tabs-mode nil
              tab-width 2
              indent-level 2
              evil-shift-width 2
              c-basic-offset 2)

(setq visible-cursor nil
      use-package-always-ensure t
      create-lockfiles nil
      make-backup-files nil
      column-number-mode t
      scroll-error-top-bottom t
      show-paren-delay 0.3
      sentence-end-double-space nil)

;; (setq use-package-verbose t) ;; uncomment for debugging init

;; adds line numbers to sourcecode files
(add-hook 'prog-mode-hook
  (function (lambda ()
    (linum-mode 1))))

;; mouse in terminal
(require 'mouse)
(xterm-mouse-mode 1)

(use-package auto-package-update
  :init
  (auto-package-update-maybe)
  (auto-package-update-at-time "00:15")
  (setq auto-package-update-delete-old-versions t))

(use-package evil-tabs)

(use-package evil-leader)

(use-package evil-org
  :defer 1
  :init
  (setq evil-want-C-i-jump nil))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package evil-surround
  :defer t
  :config
  (global-evil-surround-mode 1))

(use-package evil
  :init
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-insert-state-cursor '("green" bar))
  (global-evil-leader-mode)
  :config
  (global-evil-tabs-mode t)
  (evil-mode 1)
  (global-evil-surround-mode 1)
  :bind (:map evil-normal-state-map
              ("SPC" . ace-jump-mode)))

(use-package neotree
  :config
  (setq neo-smart-open t)
  (evil-leader/set-key
    "\\" 'neotree-toggle))

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)))

(use-package evil-org
  :defer 1
  :init
  (setq evil-want-C-i-jump nil))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package evil-surround
  :defer t
  :config
  (global-evil-surround-mode 1))

(use-package evil
  :init
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-insert-state-cursor '("green" bar))
  (global-evil-leader-mode)
  :config
  (global-evil-tabs-mode t)
  (evil-mode 1)
  (global-evil-surround-mode 1)
  :bind (:map evil-normal-state-map
              ("SPC" . ace-jump-mode)))

(use-package ace-jump-mode
  :defer t)

(use-package yasnippet
  :defer 1
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-snippets"))


;; :bind (:map yas-minor-mode-map
;;             ("<C-tab>" .'yas-expand))
;; --  ^ for use-package.
;; (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)

(use-package company
  :init
  (global-company-mode t))

;; http://www.flycheck.org/manual/latest/index.html
(use-package flycheck
  :defer 1
  :init
  (global-flycheck-mode))

(use-package flycheck-rust)

(use-package flycheck-elm)

(use-package json-mode
  :defer t)

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package material-theme)

(use-package cider
  :mode ("\\.clj$" . clojure-mode))

(use-package sbt-mode
  :mode ("\\.scala\\'" . scala-mode))

(use-package scala-mode
  :mode "\\.scala\\'")

(use-package ensime
  :pin melpa-stable
  :mode ("\\.scala\\'" . scala-mode)
  :config
  (electric-pair-mode 1)
  (show-paren-mode 1)
  :init
  (add-hook 'scala-mode-hook 'ensime-mode)
  :commands ensime ensime-mode)

;; (add-hook 'scala-mode-hook
;;           (let ((original-command (lookup-key scala-mode-map [tab])))
;;             `(lambda ()
;;                (setq yas-fallback-behavior
;;                      '(apply ,original-command))
;;                (local-set-key [tab] 'yas-expand))))

(use-package elixir-mode
  :defer t)

(use-package elm-mode
  :init
  (elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm)
  (flycheck-elm-setup)
  :config
  (setq elm-format-on-save t)
  :mode ("\\.elm\\'" . elm-mode))


;; copy & paste in linux
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; different site-lisp dirs for different OS's -- enabling shared home
(cond
  ((string-equal system-type "gnu/linux")
    (let ((default-directory "/usr/share/emacs/site-lisp/"))
      (normal-top-level-add-subdirs-to-load-path)))
  ((string-equal system-type "darwin")
    (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
      (normal-top-level-add-subdirs-to-load-path))))

;; autosave to file instead of #filename#
;; (defun save-buffer-if-visiting-file (&optional args)
;;   "Save the current buffer only if it is visiting a file"
;;   (interactive)
;;   (if (and (buffer-file-name) (buffer-modified-p))
;;       (save-buffer args)))
;; (add-hook 'auto-save-hook 'save-buffer-if-visiting-file)

;; autosave entire file to the file(no #filename# files or w/e)
(defun full-auto-save ()
  "Automatically save the file directly."
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))

(add-hook 'auto-save-hook 'full-auto-save)

;(require 'pbcopy)
;(turn-on-pbcopy)

;; start fullscreen (for OS X / floating WMs)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; another way to do that ^
;;(toggle-frame-maximized)
;; (custom-set-variables
;;  '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; golden ratio mode for split window editing
;(require 'golden-ratio)
;(golden-ratio-mode 1)
;(golden-ratio-auto-scale t)

;;semantic markup mode
;(semantic-mode 1)
;(setq semantic-complete-inline-analyzer-displayor-class 'semantic-displayor-ghost)

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better

;; MAC OS X FIX FOR LACKING ENV VARs
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

(use-package emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode)) ;; Auto-start in any web-mode

;; http://web-mode.org/
;; use web-mode for .jsx files
(use-package web-mode
  :init
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jsx$" . web-mode)
         ("\\.eex$" . web-mode)))

;; end of web-mode stuff for indent setting etc.

; org mode settings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
    ("e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (json-mode company-web web-mode use-package rust-mode rainbow-delimiters powerline neotree material-theme js2-mode flycheck-rust flycheck-elm flycheck-clojure evil-terminal-cursor-changer evil-tabs evil-surround evil-org ensime emmet-mode elm-mode elixir-mode company-quickhelp auto-complete ace-jump-mode)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#263238" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 143 :width normal :foundry "ADBO" :family "Source Code Pro")))))

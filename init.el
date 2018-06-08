;;; init --- Summary
;;; Commentary:
;;;

;;; Code:

;; Uncoment to profile launch-time
;; (add-to-list 'load-path "/Users/smaant/src/emacs/benchmark-init-el/")
;; (require 'benchmark-init-loaddefs)
;; (benchmark-init/activate)

(setq load-prefer-newer t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)

;; Define package repositories
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)
(setq message-log-max 16384)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(require 'subr-x)
(require 'rx)
(require 'time-date)

(use-package dash
  :ensure t
  :demand t)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
;; (when (not package-archive-contents)
;;   (package-refresh-contents))

(defconst disabled-keys '("<down>" "<up>" "<right>" "<left>" "M-DEL" "DEL" "<deletechar>" "<C-backspace>" "C-g"))
(defconst disabled-keys '())
(defconst smaant-modules "modules/" "Path with all confgurations.")

(eval-and-compile
  (push (expand-file-name "modules" user-emacs-directory) load-path))

(use-package smaant-navigation :load-path "modules")
(use-package smaant-dev :load-path "modules")
(use-package smaant-go :load-path "modules")
(use-package smaant-org :load-path "modules")

(provide 'init)

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
;; (load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
;(load "setup-js.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "0ec59d997a305e938d9ec8f63263a8fc12e17990aafc36ff3aff9bc5c5a202f0" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" default)))
 '(global-prettify-symbols-mode f)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(package-selected-packages
   (quote
    (java java-mode meghanada-mode meghanada plantuml-mode wsd-mode ldap-mode elpy monroe origami smartparens ein yaml-mode protobuf-mode go-guru go-direx flycheck yafolding web-mode paredit undo-tree-mode org-bullets org-bullets-mode solarized-theme org window-numbering window-number which-key wgrep-ag use-package undo-tree tagedit smex shm rainbow-delimiters python-mode multiple-cursors markdown-mode magit leuven-theme js2-mode ido-vertical-mode ido-ubiquitous ido-hacks ibuffer-projectile hindent highlight-symbol haskell-mode go-eldoc flx-ido exec-path-from-shell direx diff-hl delight clojure-mode-extra-font-locking ace-jump-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "purple3"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "forest green"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "magenta4"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "DodgerBlue2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "SlateBlue2"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "brown"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "DeepSkyBlue1"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "SlateGray4")))))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; rainbow mode for emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right)

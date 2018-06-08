 (require 'use-package)

(defun backward-kill-line ()
  (interactive)
  (kill-line 0))

(bind-keys
 ("M-n" . keyboard-quit)
 
 ("s-j" . indent-new-comment-line)
 ("M-RET" . indent-new-comment-line)

 ;; ("M-j" . left-char) ; indent-new-comment-line
 ;; ("M-l" . right-char) ; was downcase-word
 ;; ("M-i" . previous-line) ; tab-to-tab-stop
 ;; ("M-k" . next-line)
 ;; ("M-J" . left-word) ; paredit-join-sexps
 ;; ("M-L" . right-word) ; kill-sentence
 ;; ("M-I" . backward-paragraph)
 ;; ("M-K" . forward-paragraph)

 ;; ("<M-c-left>" . left-word)
 ;; ("S-<right>" . right-word)
 ;; ("S-<up>" . backward-paragraph)
 ;; ("S-<down>" . forward-paragraph)
 ;; ("S-<backspace>" . my-backward-kill-word)
 ;; ("S-<delete>" . kill-word)
 
 ;; ("M-u" . backward-delete-char-untabify) ; upcase-word
 ;; ("M-o" . delete-forward-char)
 ;; ("M-U" . my-backward-kill-word)
 ;; ("M-O" . kill-word)

 ("C-M-k" . backward-kill-line)

 ("M-8" . mark-sexp)
 ("M-7" . set-mark-command))

; Temporary
(mapc (lambda (key) (unbind-key key)) disabled-keys)
(mapc (lambda (key) (unbind-key key emacs-lisp-mode-map)) disabled-keys)
(mapc (lambda (key) (unbind-key key lisp-interaction-mode-map)) disabled-keys)
(mapc (lambda (key) (unbind-key key minibuffer-local-map)) disabled-keys)

(bind-keys
 :map minibuffer-local-map
 ("M-n" . minibuffer-keyboard-quit))
 
(use-package ido
  :demand t

  :bind (("C-x b" . ido-switch-buffer)
         ("C-x B" . ido-switch-buffer-other-window)
         ("C-x C-b" . ibuffer)
         ("C-c m" . imenu))

  :config
  (ido-mode t)
  
  (setq ido-everywhere t
        ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-case-fold t
        ido-enable-prefix nil
        ido-create-new-buffer 'always
        ido-auto-merge-work-directories-length -1
        ido-use-virtual-buffers t)
  (add-to-list 'ido-ignore-files "\\.DS_Store")

  (bind-keys
   :map minibuffer-local-map
   ("M-k" . ido-next-match)
   ("M-i" . ido-prev-match)
   ("M-j" . ido-vertical-prev-match)
   ("M-l" . ido-vertical-next-match))

  ; Temporary
  (defun my-ido-keys ()
    (mapc (lambda (key) (unbind-key key ido-completion-map)) disabled-keys))
  (add-hook 'ido-setup-hook #'my-ido-keys))

(use-package ag
  :ensure t
  :bind (("C-c p s s" . projectile-ag)))

(use-package wgrep-ag
  :ensure t
  :bind (("C-c C-p" . wgrep-change-to-wgrep-mode)))

(use-package ido-ubiquitous
  :ensure t
  :demand t
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-hacks
  :ensure t
  :demand t
  :config
  (ido-hacks-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :demand t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(use-package smex
  :ensure t
  :demand t
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

(use-package flx-ido
  :ensure t
  :demand t
  :config
  (flx-ido-mode 1))

(use-package recentf
  :ensure t
  :demand t
  :init (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 40))

(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-completion-system 'ido
        projectile-find-dir-includes-top-level t
        projectile-mode-line '(:eval
                               (let ((text (projectile-project-name)))
                                 (if (string= text "-")
                                     "" (concat " " text " ")))))
  (projectile-global-mode))

(use-package ibuffer-projectile
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(use-package direx
  :ensure t
  :bind (("C-c d d" . direx:jump-to-directory)
         ("C-c d p" . direx-project:jump-to-project-root))
  :config
  (bind-key "<M-down>" 'direx:next-sibling-item direx:direx-mode-map)
  (bind-key "<M-up>" 'direx:previous-sibling-item direx:direx-mode-map)
  (bind-key "<SPC>" 'direx:view-item direx:direx-mode-map))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("s->" . mc/skip-to-next-like-this)
         ("s-<" . mc/skip-to-previous-like-this)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-word-mode)
         ("C-c C-c SPC" . ace-jump-char-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark))
  :init (bind-key "C-c SPC" 'ace-jump-word-mode)
  :config
  (ace-jump-mode-enable-mark-sync))

(use-package window
  :bind (("C-c w =" . balance-windows)
         ("C-c w k" . delete-window)
         ("C-c w /" . split-window-right)
         ("C-c w -" . split-window-below)
         ("C-c w m" . delete-other-windows)))

(use-package window-number
  :ensure t
  :diminish window-number-mode
  :bind ("<C-tab>" . window-number-switch)
  :config
  (window-number-mode 1))

(column-number-mode 1)

;;
;; smooth scrolling
;;
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 'f) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse t) ;; scroll window under mouse

(setq redisplay-dont-pause 'f
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 1000
  scroll-preserve-screen-position 1)

;;
;; auto-saving
;;
;; (add-hook 'magit-status-mode-hook 'whitespace-cleanup)
;; (add-hook 'magit-status-sections-hook 'whitespace-cleanup)
;; (add-hook 'magit-status-refresh-hook 'whitespace-cleanup)
;; (add-hook 'magit-status-headers-hook 'whitespace-cleanup)

(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-frame (before other-frame-now activate)
  (when buffer-file-name (save-buffer)))

(provide 'smaant-navigation)
;;; smaant-navigation.el ends here

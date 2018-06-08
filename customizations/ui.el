;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Show line numbers
(global-linum-mode)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;(set-frame-font (concat "Inconsolata" "-" (number-to-string 15)))
(set-face-attribute 'default nil :height 140)

(message "window-system: %s" window-system)
(if window-system
  ;(load-theme 'leuven t)
  (progn
    (setq solarized-high-contrast-mode-line t)
    (load-theme 'solarized-light t))
  (progn
    (load-theme 'tomorrow-night t)
    (set-face-attribute 'linum nil :foreground "white")))


;(set-face-attribute 'default nil :height 120)
(setq initial-frame-alist '((top . 0) (left . 0) (width . 180) (height . 53)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

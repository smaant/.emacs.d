;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Shows imenu
(global-set-key (kbd "C-c m") 'imenu)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(require 'projectile)
(projectile-global-mode)

(defun find-file-in-projecttile-or-ido ()
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ido-find-file)))

(define-key global-map (kbd "C-x C-f") 'find-file-in-projecttile-or-ido)

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
;; Direx
;;
(require 'direx)
(global-set-key (kbd "C-x M-j") 'direx:jump-to-directory)
(global-set-key (kbd "C-x M-p") 'direx-project:jump-to-project-root)
(define-key direx:direx-mode-map (kbd "<M-down>") 'direx:next-sibling-item)
(define-key direx:direx-mode-map (kbd "<M-up>") 'direx:previous-sibling-item)
(define-key direx:direx-mode-map (kbd "<SPC>") 'direx:view-item)

;;
;; Multiple cursors
;;
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "s->") 'mc/skip-to-next-like-this)
;;
;; ido configuration
;;
(require 'ido)
;; (require 'ido-hacks)
(require 'flx-ido)
(require 'ido-vertical-mode)

(ido-mode 1)
(flx-ido-mode 1)
;; (ido-hacks-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)

(setq ido-enable-flex-matching t
      ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;;
;; ace-jump
;;
(define-key global-map (kbd "C-c SPC") 'ace-jump-word-mode)
(define-key global-map (kbd "C-c C-c SPC") 'ace-jump-char-mode)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;
;; Cmd+Shift+Enter
;;
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with indent"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<s-S-return>") 'newline-without-break-of-line)

;;
;; window-number && window-numbering
;;
(require 'window-number)
(window-number-mode 1)
(define-key global-map (kbd "<C-tab>") 'window-number-switch)

(require 'window-numbering)
(window-numbering-mode 1)
(dotimes (i 10)
  (define-key global-map (kbd (format "s-%s" i)) (intern (format "select-window-%s" i))))

(dotimes (i 10)
  (eval `(defun ,(intern (format "delete-window-%s" i)) (&optional args)
           ,(format "Delete the window with number %i." i)
           (interactive "P")
           (select-window-by-number ,i 1))))

(dotimes (i 10)
  (define-key global-map (kbd (format "C-s-%s" i)) (intern (format "delete-window-%s" i))))

;;
;; windows resizing
;;
(require 'windmove)

(defun resize-window (first? resize-first second? resize-second)
  (interactive)
  (if (funcall first?) 
      (funcall resize-first 1)
    (if (funcall second?)
        (funcall resize-second 1)
      nil)))

(defun top? ()
  (let ((window (windmove-find-other-window 'down)))
    (and window (not (equal window (minibuffer-window))))))

(defun bottom? ()
  (windmove-find-other-window 'down))

(defun left? ()
  (windmove-find-other-window 'right))

(defun right? ()
  (windmove-find-other-window 'left))

(define-key global-map (kbd "<S-s-down>")
  (lambda ()
    (interactive)
    (resize-window 'top? 'enlarge-window 'bottom? 'shrink-window)))

(define-key global-map (kbd "<S-s-up>")
  (lambda ()
    (interactive)
    (resize-window 'top? 'shrink-window 'bottom? 'enlarge-window)))

(define-key global-map (kbd "<S-s-left>")
  (lambda ()
    (interactive)
    (resize-window 'left? 'shrink-window-horizontally 'right? 'enlarge-window-horizontally)))

(define-key global-map (kbd "<S-s-right>")
  (lambda ()
    (interactive)
    (resize-window 'left? 'enlarge-window-horizontally 'right? 'shrink-window-horizontally)))


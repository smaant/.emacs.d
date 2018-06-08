(require 'use-package)

(use-package delight
  :ensure t
  :demand t)

(use-package flycheck
  :ensure t
  :bind (("C-c e l" . list-flycheck-errors)
         ("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error)
         ("C-c e c" . flycheck-buffer)
         ("C-c e C" . flycheck-clear)
         ("C-c e f" . flycheck-first-error)
         ("C-c e w" . flycheck-copy-errors-as-kill)
         ("C-c t f" . flycheck-mode))
  :config
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'right-fringe
        flycheck-standard-error-navigation nil
        flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list
        flycheck-scalastylerc "scalastyle_config.xml")

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'js2-mode)

  ;; Use italic face for checker name
  (set-face-attribute 'flycheck-error-list-checker-name nil
                      :inherit 'italic)
  :diminish flycheck-mode)

(use-package js2-mode
  :ensure t
  :mode "\\.js$"
  ;:init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (delight 'js2-mode "js2" :major)

  (defun paredit-singlequote (&optional n)
    (interactive "P")
    (cond ((paredit-in-string-p)
           (if (eq (point) (- (paredit-enclosing-string-end) 1))
               (forward-char)      ; Just move past the closing quote.
             ;; Don't split a \x into an escaped backslash and a string end.
             (if (paredit-in-string-escape-p) (forward-char))
             (insert ?\\ ?\' )))
          ((paredit-in-comment-p)
           (insert ?\' ))
          ((not (paredit-in-char-p))
           (paredit-insert-pair n ?\' ?\' 'paredit-forward-for-quote))))

  (progn (setq paredit-commands
               `(
                 "Basic Insertion Commands"
                 ("\""        paredit-singlequote
                  ("(frob grovel |full lexical)"
                   "(frob grovel \'|\' full lexical)"
                   "(frob grovel \'\'| full lexical)")
                  ("(foo \'bar |baz\' quux)"
                   "(foo \'bar \\\'|baz\' quux)")
                  ("(frob grovel)   ; full |lexical"
                   "(frob grovel)   ; full \"|lexical")))) nil)

  (defun my-js2-mode-hook ()
    (interactive)
    (js2-highlight-unused-variables-mode 1)
    (imenu-add-menubar-index)
    ;; (hideshowvis-enable)
    (setq js2-strict-missing-semi-warning nil
          js2-missing-semi-one-line-override nil
          js2-indent-switch-body t
          js2-highlight-level 3)

    (setq-default js2-basic-offset 2
                  js-indent-level 2)

    (set (make-local-variable 'paredit-space-for-delimiter-predicates)
         '((lambda (endp delimiter) nil)))
    (paredit-mode 1)

    (unbind-key "M-j" js2-mode-map))

  (add-hook 'js2-mode-hook 'my-js2-mode-hook)

  (use-package haskell-mode
    :ensure t
    :defer t
    :mode "\\.hs"
    
    :init
    ;; (require 'haskell-interactive-mode)
    ;; (require 'haskell-process)
                                        ;(use-package haskell-indentation-mode)
                                        ;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

    :config
    ;; (use-package hindent :ensure t)
    ;; (use-package shm :ensure t)
    ;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    (require 'haskell-interactive-mode)
    (require 'haskell-process)
    
    (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t))
    
    (bind-keys
     :map haskell-mode-map
     ("C-c C-c" . haskell-compile)
     ("C-c C-l" . haskell-process-load-or-reload)
     ("C-`" . haskell-interactive-bring)
     ;; ("C-c C-t" . haskell-process-do-type)
     ("C-c C-i" . haskell-process-do-info)
     ("C-c C-c" . haskell-process-cabal-build)
     ("C-c C-k" . haskell-interactive-mode-clear)
     ("C-c c" . haskell-process-cabal)

                                        ;interactive-haskell-mode-map
     ("M-." . haskell-mode-goto-loc)
     ("C-c C-t" . haskell-mode-show-type-at))

    ;; :bind
    ;; (:map haskell-mode-map
    ;;       ("C-c C-c" . haskell-compile)
    ;;       ("C-c C-l" . haskell-process-load-or-reload)
    ;;       ("C-`" . haskell-interactive-bring)
    ;;       ;; ("C-c C-t" . haskell-process-do-type)
    ;;       ("C-c C-i" . haskell-process-do-info)
    ;;       ("C-c C-c" . haskell-process-cabal-build)
    ;;       ("C-c C-k" . haskell-interactive-mode-clear)
    ;;       ("C-c c" . haskell-process-cabal)

    ;;       ;interactive-haskell-mode-map
    ;;       ("M-." . haskell-mode-goto-loc)
    ;;       ("C-c C-t" . haskell-mode-show-type-at))
    ))

(use-package nxml-mode
  :mode "//.xml$"
  :config
  (defun bf-pretty-print-xml-region (begin end)
    "Pretty format XML markup in region. You need to have nxml-mode
     http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
     this. The function inserts linebreaks to separate tags that have
     nothing but whitespace between them. It then indents the markup
     by using nxml's indentation rules."
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n") (setq end (1+ end)))
      (indent-region begin end))
    (message "Ah, much better!"))

  (define-key nxml-mode-map "C-M-\\" 'bf-pretty-print-xml-region))

(use-package json-mode
  :ensure t
  :mode "//.json$")

(use-package json-reformat
  :ensure t
  :bind ("C-c j r" . json-reformat-region))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun newline-without-break-of-line ()
  "1. move to end of the line.
   2. insert newline with indent"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(defun newline-above-without-break-of-line ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "<s-S-return>") 'newline-without-break-of-line)
(global-set-key (kbd "<M-S-return>") 'newline-above-without-break-of-line)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (bind-key "s-Z" 'undo-tree-redo))

(use-package paredit
  :ensure t
  :demand t
  :commands paredit-mode    
  :config
  (unbind-key "M-J" paredit-mode-map)
  (unbind-key "<C-left>" paredit-mode-map)
  (unbind-key "<C-right>" paredit-mode-map)
  (bind-keys
   :map paredit-mode-map
   ;("M-u" . paredit-backward-delete)
   ;("M-o" . paredit-forward-delete)
   ("<M-backspace>" . my-backward-kill-word)
   ("<M-delete>" . paredit-forward-kill-word))
  
  (mapc
   (lambda (mode) (add-hook mode #'enable-paredit-mode))
   '(ielm-mode-hook scheme-mode-hook lisp-interaction-mode-hook
     lisp-mode-hook emacs-lisp-mode-hook eval-expression-minibuffer-setup-hook))

  (mapc (lambda (key) (unbind-key key paredit-mode-map)) disabled-keys))

(use-package meghanada
  :ensure t
  :init
  (add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  
  (setq c-basic-offset 2
        meghanada-java-path "java"
        meghanada-maven-path "mvn"))

(provide 'smaant-dev)

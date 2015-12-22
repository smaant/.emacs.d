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
  :init (global-flycheck-mode)
  :config 
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
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (delight 'js2-mode "js2" :major)

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
                  js-indent-level 2))

  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

(use-package json-mode
  :ensure t
  :defer t)

(use-package json-reformat
  :ensure t
  :bind ("C-c x j" . json-reformat-region))

(provide 'smaant-dev)

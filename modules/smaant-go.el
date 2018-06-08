(require 'use-package)

(use-package go-mode
  :ensure t
  :mode "\\.go"

  :config
  (use-package go-eldoc
    :ensure t
    :demand t)
  (use-package flycheck
    :ensure t
    :demand t)
  (use-package go-direx
    :ensure t
    :demand t)
  ;; (use-package go-guru
  ;;   :ensure t
  ;;   :demand t)
  
  (bind-keys :map go-mode-map
			 ("M-." . godef-jump)
			 ("C-c C-c" . compile)
             ("C-c d g" . go-direx-pop-to-buffer))
  
  (setq-default tab-width 4)
  (defvaralias 'c-basic-offset 'tab-width)
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (setq compile-command "go build -v && go test -v && go vet && golint")
  (electric-pair-mode)
  (flycheck-mode)
  ;(go-guru-hl-identifier-mode)
  )

(provide 'smaant-go)

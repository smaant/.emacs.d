;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (defcustom exec-path-from-shell-arguments (list "-l") ""
    ;; :type '(repeat (string :tag "Shell argument"))
    ;; :group 'exec-path-from-shell
    )
  
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;;;;
;; Clojure
;;;;

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; Enable rainbow mode
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;(put-clojure-indent 'attempt-all 1)
;; (put-clojure-indent 'domonad 1)
;; (put-clojure-indent 'testing* 1)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2)
  ;; truckloads-api
  (require-auth 1)
  (require-experiment 2)
  (require-role 2)
  (timed 1)
  (dd/timed 3)
  (attempt-all 1)
  (domonad 1)
  (testing* 1)
  ;; speclj
  (describe 1)
  (it 1)
  (ensure 1)
  ;; slingshot
  (try+ 0))


;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c SPC") 'ace-jump-word-mode)))

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; (add-to-list 'load-path "~/src/emacs/cider")
(require 'cider)

(add-hook 'cider-mode-hook #'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

(setq nrepl-prompt-to-kill-server-buffer-on-quit nil)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun my-cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(dev/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key clojure-mode-map (kbd "C-c r") 'my-cider-refresh)
     (define-key cider-repl-mode-map (kbd "C-c r") 'my-cider-refresh)
     (define-key cider-repl-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-repl-mode-map (kbd "C-c k") 'cider-repl-clear-buffer)))

(defcustom cider-test-infer-test-ns 'cider-test-ns-fn
  "Function to infer the test namespace for NS.
The default implementation uses the simple Leiningen convention of appending
'-test' to the namespace name."
  :type 'symbol
  :group 'cider-test
  :package-version '(cider . "0.7.0"))

(defun cider-test-ns-fn (ns)
  (when ns
    (let ((suffix "-test")
          (prefix "test-")
          (file (first (last (split-string ns "\\.")))))
      (message "%s %s %s %s" file suffix prefix (or (string-prefix-p prefix file)
                                 (string-suffix-p suffix file)))
      (if (or (string-prefix-p prefix file)
              (string-suffix-p suffix file))
          ns
        (concat ns suffix)))))

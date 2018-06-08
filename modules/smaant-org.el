(require 'use-package)

(use-package org
  :mode "//.org$"
  :commands (org-mode org-capture)
  :bind (("C-c c" . org-capture)
         ("C-c i" . open-index-file)
         ("C-c a" . org-agenda)
         ("C-c r" . org-refile))
  
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))

  (defun org-file-path (filename)
    "Return the absolute address of an org file, given its relative name."
    (concat (file-name-as-directory org-directory) filename))

  (setq org-directory "~/org")
  (setq org-inbox-file (org-file-path "inbox.org")
        org-index-file (org-file-path "index.org")
        org-work-index-file (org-file-path "work-index.org")
        org-archive-location (concat (org-file-path "archive/archive.org") "::* From %s"))

  (load-library "find-lisp")
  (setq org-agenda-files
        (seq-filter (lambda ($f) (not (string-match "archive" $f)))
                    (find-lisp-find-files org-directory "\.org$")))

  (setq org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 2)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file) 
  

  (defun open-index-file ()
    "Open the master org TODO list."
    (interactive)
    (find-file org-index-file)
    (flycheck-mode -1)
    (end-of-buffer))
  
  :config
  (message "Configuring Org-mode")
  (use-package org-bullets :ensure t)
  (setq org-ellipsis " ..."
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-startup-truncated nil
        org-src-window-setup 'current-window)

  (setq org-capture-templates
      '(("g" "Groceries"
         checkitem
         (file (org-file-path "groceries.org")))

        ("l" "Today I Learned..."
         entry
         (file+datetree (org-file-path "til.org"))
         "* %?\n")

        ("r" "Reading"
         checkitem
         (file (org-file-path "to-read.org")))

        ("t" "Todo"
         entry
         (file org-index-file)
         "* TODO %?\n")

        ("w" "Work Todo"
         entry
         (file org-work-index-file)
         "* TODO %?\n")))
  
  (defun mark-done-and-archive ()
    "Mark the state of an org-mode item as DONE and archive it."
    (interactive)
    (org-todo 'done)
    (org-archive-subtree))

  (bind-keys
   :map org-mode-map
   ("<C-c C-x C-a>" . mark-done-and-archive)
   ;("M-I" . org-backward-element)
   ("<M-left>" . left-word)
   ("<M-right>" . right-word)
   ("<S-up>" . org-metaup)
   ("<S-down>" . org-metadown)
   ("<S-right>" . org-metaright)
   ("<S-left>" . org-metaleft)
   ;("C-I" . outline-up-heading)
   ;("M-K" . org-forward-element)
   )

  (setq org-log-done 'time)
  (require 'ox-md nil t)

  (global-set-key (kbd "C-c i") 'open-index-file))

(provide 'smaant-org)

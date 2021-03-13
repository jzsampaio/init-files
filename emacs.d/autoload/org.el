(require 'org)

(org-babel-do-load-languages
 'org-babel-load-languages '(
			     (plantuml . t)
			     (shell . t)
			     (python . t)
			     (fsharp . t)
			     (dot . t)
			     (gnuplot . t)
			     (haskell . t)
			     (sql . t)
			     )
 )
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook 'org-bullets-mode)

(setq
 org-todo-keywords '((sequence "TODO" "|" "DONE" "DEFERRED"))
 org-drill-add-random-noise-to-intervals-p t
 org-log-done 'time
 org-M-RET-may-split-line '((default . nil))
 org-capture-templates '(
			 (
			  "t" "Todo" entry
			  (file "~/.emacs.d/refile.org") ;; assumption: I always refile when capturing
			  "* TODO %?\n"
			  )
			 (
			  "m" "Meeting Summary" entry
			  (file "~/.emacs.d/refile.org") ;; assumption: I always refile when capturing
			  "* [info][meeting-summary]\n** Who joined?\n** What was discussed?\n** Next steps\n"
			  )
			 (
			  "p" "Meeting Summary - Portuguese" entry
			  (file "~/.emacs.d/refile.org") ;; assumption: I always refile when capturing
			  "* [info][resumo-reunião]\n** Quem participou?\n** O que foi discutido?\n** Próximos passos"
			  )
			 )
 org-jz-summary-files '( "~/Work/meeting-summaries.org" )
 ;; when re-filing offer limited number of options
 org-refile-targets '(
		      (nil :maxlevel . 1)
		      (org-agenda-files :maxlevel . 1)
		      (org-jz-summary-files :maxlevel . 1)
		      (("~/SideProjects/personal-todo.org") :maxlevel . 2)
		      )
 ;; setup agenda files
 org-agenda-files '(
		    "~/Work/todo-lists/datarisk.org"
		    "~/SideProjects/personal-todo.org"
		    )
 org-default-notes-file "~/.emacs.d/notes.org"
 org-log-refile "time"
 org-outline-path-complete-in-steps nil
 org-refile-use-outline-path "file"
 org-agenda-custom-commands '(
			      ("c" . "My Custom Agendas")
			      ("cu" "Unscheduled TODO" (
							(todo "" (
								  (org-agenda-overriding-header "\nUnscheduled TODO")
								  (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
								  )
							      )
							)
			       nil
			       nil
			       )
			      )
 org-agenda-window-setup "only-window"
 org-goto-interface 'outline-path-completion
 org-list-demote-modify-bullet '(
				 ("+" . "-")
				 ("-" . "*")
				 ("*" . "+"))
 org-list-indent-offset 2
 org-ellipsis " ↴"
 org-return-follows-link t
 org-startup-with-inline-images t
 org-catch-invisible-edits 'show-and-error
 org-log-into-drawer t
 org-log-done 'time
 org-bullets-bullet-list '("◉" "◎" "♠" "○" "►" "◇")
 org-confirm-babel-evaluate nil
 )

(require 'ox-beamer)

(setq org-journal-dir "/home/jz/.emacs.d/journal/"
      org-journal-time-format "%I:%M %p ")
(require 'org-journal)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; make some org commands available from anywhere (not only org mode)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-enabled-themes '(sanityinc-solarized-light))
 '(custom-safe-themes
   '("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(helm-mode t)
 '(lsp-keymap-prefix "C-c C-l")
 '(org-agenda-files
   '("~/.emacs.d/engineering-journal.org" "~/Work/todo-lists/datarisk.org" "~/SideProjects/personal-todo.org"))
 '(org-agenda-prefix-format
   '((agenda . " %i %-16c%?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(package-selected-packages
   '(all-the-icons ob-fsharp forge magit org-msg elfeed-org elfeed helm-org-ql org-super-agenda dockerfile-mode bookmark+ quelpa pdf-tools nov org-scrum ox-reveal nix-mode treemacs-projectile marginalia consult-lsp dap-mode lsp-treemacs lsp-ui treemacs lsp-mode yafolding rotate shrface gnuplot tj3-mode highlight-indent-guides company company-fuzzy org-journal editorconfig yaml-mode dashboard org-ref helm-org org-bullets plantuml-mode use-package dired-subtree exec-path-from-shell helm org-drill-table org-drill color-theme-sanityinc-solarized eglot-jl))
 '(safe-local-variable-values
   '((org-latex-compiler . "TEXINPUTS=%o:%o/../document-style/beamer-style: pdflatex")
     (org-latex-compiler . "TEXINPUTS=.:../document-style/beamer-style: pdflatex")
     (org-latex-pdf-process "TEXINPUTS=%o:%o/../document-style/beamer-style: %latex -interaction nonstopmode -output-directory %o %f" "TEXINPUTS=%o:%o/../document-style/beamer-style: %latex -interaction nonstopmode -output-directory %o %f" "TEXINPUTS=%o:%o/../document-style/beamer-style: %latex -interaction nonstopmode -output-directory %o %f")))
 '(send-mail-function 'mailclient-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(warning-suppress-types '((org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))

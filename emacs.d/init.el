(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-solarized-light))
 '(custom-safe-themes
   '("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(helm-mode t)
 '(lsp-keymap-prefix "C-c C-l")
 '(org-agenda-prefix-format
   '((agenda . " %i %-16c%?-12t% s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))
 '(package-selected-packages
   '(org-journal editorconfig flymake-diagnostic-at-point yaml-mode lsp-ui rotate magit-lfs magit dashboard company-jedi company-fuzzy company-ycmd company-lsp auto-complete org-ref ess py-autopep8 helm-org org-bullets plantuml-mode use-package dired-subtree project exec-path-from-shell ob-fsharp lsp-mode helm org-drill-table org-drill color-theme-sanityinc-solarized ibuffer-projectile org-projectile projectile eglot-jl))
 '(warning-suppress-types '((org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-to-list 'load-path "~/.emacs.d/lisp/")

(transient-mark-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(save-place-mode 1)
(projectile-mode +1)
(global-hl-line-mode)
(global-display-line-numbers-mode)
(exec-path-from-shell-initialize)
(global-auto-revert-mode 1)

(setq
 inhibit-splash-screen t
 ;; fix copy and past from clipboard
 x-select-enable-clipboard t
 interprogram-paste-function 'x-cut-buffer-or-selection-value
 doc-view-continuous t
 create-lockfiles nil
 auto-save-file-name-transforms  `((".*" "~/.emacs-autosaves/" t))
 backup-directory-alist `(("." . "~/.emacs-saves"))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 truncate-lines t
 visible-bell t
 )


(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))
(load-directory "~/.emacs.d/autoload")

;; Ido improves transition between buffers, files, and directories
(require 'ido)
(ido-mode t)
(setq ido-separator "\n")

;; removes all training white space while saving
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; start helm-help mode (a version of M-x which shows documentaiton)
(global-set-key (kbd "M-x") 'helm-M-x)
;; better buffer navigation w/ help
(global-set-key (kbd "C-z") 'helm-buffers-list)
(helm-mode 1)

(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents  . 10)
			  (projects . 10)
			  ))
  (dashboard-setup-startup-hook))


;; keyboard shortcuts
(global-set-key (kbd "C-M-g") 'magit-status)
(global-set-key [f1] 'eshell)
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-)") 'delete-pair)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

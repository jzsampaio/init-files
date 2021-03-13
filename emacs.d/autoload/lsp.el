(add-hook 'lsp-mode-hook (lambda () (local-set-key [f12] 'lsp-find-definition)))
(add-hook 'lsp-mode-hook (lambda () (local-set-key [S-f12] 'lsp-ui-peek-find-references)))
(setq lsp-ui-sideline-show-hover t)

(require 'flymake)
(define-key flymake-mode-map (kbd "M-S-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-S-p") 'flymake-goto-prev-error)
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(require 'company-lsp)
(global-set-key (kbd "C-.") 'company-complete)
(push 'company-files company-backends)
(add-hook 'after-init-hook 'global-company-mode)

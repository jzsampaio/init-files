(defun org-update-all-buffer ()
  (interactive)
  (org-update-all-dblocks)
  (org-babel-execute-buffer)
  )

; These next two modes auto-indents org-buffers as you type! NO NEED
; FOR to press C-c q or fill-paragraph ever again!
(defun my/auto-call-fill-paragraph-for-org-mode ()
    "Call two modes to automatically call fill-paragraph for you."
    (visual-line-mode)
    (org-indent-mode))
(add-hook 'org-mode-hook 'my/auto-call-fill-paragraph-for-org-mode)

(defun org-set-line-checkbox (arg)
  (interactive "P")
  (let ((n (or arg 1)))
    (when (region-active-p)
      (setq n (count-lines (region-beginning)
                           (region-end)))
      (goto-char (region-beginning)))
    (dotimes (i n)
      (beginning-of-line)
      (insert "- [ ] ")
      (forward-line))
    (beginning-of-line)))

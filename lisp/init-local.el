(global-hl-line-mode +1)
(setq-default cursor-type 'bar)

;; indent all the buffer
(defun indent-buffer ()
  "Just indent all the buffer."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil)))
(global-set-key (kbd "C-c i") 'indent-buffer)

;; revert all the buffers
(defun revert-all-buffers ()
  "Revert all non-modified buffers associated with a file.
This is to update existing buffers after a Git pull of their underlying files."
  (interactive)
  (save-current-buffer
    (mapc (lambda (b)
            (set-buffer b)
            (unless (or (null (buffer-file-name)) (buffer-modified-p))
              (revert-buffer t t)
              (message "Reverted %s\n" (buffer-file-name))))
          (buffer-list))))

(provide 'init-local)

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

;;my windows move
(defun my-window-move-right ()
  (interactive)
  (condition-case err
      (windmove-right)
    (error
     (other-frame -1))))

(defun my-window-move-left ()
  (interactive)
  (condition-case err
      (windmove-left)
    (error
     (other-frame -1))))

;;default font
(set-default-font "Free Mono 14" nil t)

(provide 'init-local)

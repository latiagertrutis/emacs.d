(global-hl-line-mode +1)
(setq-default cursor-type 'bar)

;; indent all the buffer
(defun indent-buffer ()
  "Just indent all the buffer."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil)))
(bind-key* "C-c i" 'indent-buffer)

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

;;deleteword without add to kill ring
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(bind-key* "M-d" 'my-delete-word)
(bind-key* "<C-backspace>" 'my-backward-delete-word)

(provide 'init-local)

(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq backward-delete-char-untabify-method 'all)
            (setq tab-width 4)
            (setq c-basic-offset 4)
            ))

(provide 'init-c)

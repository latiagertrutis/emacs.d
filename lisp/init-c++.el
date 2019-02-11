(add-hook 'c++-mode-hook
		  (lambda ()
			(setq indent-tabs-mode t)
			(setq backward-delete-char-untabify-method 'all)
			(setq tab-width 2)
			(setq c-basic-offset 2)
			))

(provide 'init-c++)

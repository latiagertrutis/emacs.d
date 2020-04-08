(defun my-go-config ()
  (lsp)
  (setq tab-width 4)
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook #'my-go-config)

(provide 'init-go)

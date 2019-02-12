(require-package 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require-package 'company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(provide 'init-irony)

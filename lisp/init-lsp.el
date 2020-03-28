;; Requires gopls installatio to work:
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
(require-package 'lsp-mode)


(with-eval-after-load 'company
  (require-package 'company-lsp)
  (push 'company-lsp company-backends))

(provide 'init-lsp)

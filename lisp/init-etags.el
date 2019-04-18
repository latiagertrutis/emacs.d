(require-package 'etags)
;; (use-package etags
;;              :defer t

;;              :config
;;              ;; FIXME
;;              ;; (defvar tags-make-n-visit-history '("--regex='/.*\(public\|static\|abstract\|protected\|private\).*function.*(/' ~/Pliizz/src/**/*.php"))
;;              ;; (setq savehist-file "~/.emacs.d/misc/history")
;;              )

;; (defvar tags-make-n-visit-history '("~/babao/src/babao/**/*.py ~/.local/lib/python*/site-packages/{{krakenex,configparser}*/**/,argparse}*.py"))
;; (eval-after-load "savehist"
;;   '(add-to-list 'savehist-additional-variables 'tags-make-n-visit-history))
;; (savehist-mode 1)
(setq tags-table-list
      '("~/.emacs.d"))

(defun refresh-tags (file-pattern out-file)
  "Create a tag file corresponding to FILE-PATTERN, then visit it."
  (shell-command (format "etags -o %s %s" out-file file-pattern))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table out-file nil)))

(defun refresh-tags-prev-dir ()
  (interactive)
  (refresh-tags "../**/*.[ch]" "~/.emacs.d/TAGS"))

(bind-key* "C-c C-t" 'refresh-tags-prev-dir)
(bind-key* "C-c C-d" 'xref-find-definitions)
(bind-key* "C-c C-r" 'xref-find-references)
(bind-key* "C-c C-b" 'xref-pop-marker-stack)
;; (bind-key* (kbd *altgr-r*) 'recentf-open-files)

;; ;; need system package "global"
;; (use-package ggtags
;;              :ensure t
;;              :defer t

;;              :init
;;              (use-package gtags
;;                           :ensure t
;;                           :defer t)
;;              (add-hook 'c-mode-common-hook
;;                        (lambda ()
;;                          (when (derived-mode-p 'c-mode 'c++-mode)
;;                            (ggtags-mode 1))))

;;              :config
;;              (setq-local hippie-expand-try-functions-list
;;                          (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))
;;              ;; (bind-key* (kbd *altgr-g*) 'ggtags-find-tag-dwim) #TODO
;;              ;; (bind-key* (kbd *altgr-x*) 'ggtags-find-reference)
;;              )


(provide 'init-etags)

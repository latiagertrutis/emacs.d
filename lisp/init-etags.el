(require-package 'etags)

;; (visit-tags-table "~/.emacs.d/TAGS")
(setq tag-file ".TAGS")

(defun refresh-project-tags ()
  "Rewfres the TAG file of one project."
  (interactive)
  (let
      ((tag-dir (locate-dominating-file buffer-file-name tag-file)))
    (setq tags-file-name (concat (file-name-as-directory tag-dir) tag-file))
    (visit-tags-table (concat (file-name-as-directory tag-dir) tag-file))
    (print (format "El nuevo tag es: %s" tags-file-name))
    (if tag-dir
        (shell-command (format
                        "find %s \\( -name \"*.[chCH]\" -o -name \"*.go\" \\) -print | etags - -o %s"
                        tag-dir
                        tags-file-name))
      (print (format "No %s file found in project." tag-file)))))

(bind-key* "M-g d" 'xref-find-definitions)
(bind-key* "M-g r" 'xref-find-references)
(bind-key* "M-g b" 'xref-pop-marker-stack)
(bind-key* "M-g o" 'xref-find-definitions-other-window)

(provide 'init-etags)

(require-package 'etags)

(setq tags-file-name "TAGS")
(setq tag-file ".TAGS")

(defun refresh-project-tags ()
  "Rewfres the TAG file of one project."
  (interactive)
  (let
      ((tag-dir (locate-dominating-file buffer-file-name tag-file)))
    (if tag-dir
        (shell-command (format
                        "find %s -name \"*.[chCH]\" -print | sed -e \"s/^/c:\\/msys64/\" | /mingw64/bin/etags.exe - -a -o %s"
                        tag-dir
                        "~/.emacs.d/TAGS"))
      (print (format "No %s file found in project." tag-file)))))

(bind-key* "M-g d" 'xref-find-definitions)
(bind-key* "M-g r" 'xref-find-references)
(bind-key* "M-g b" 'xref-pop-marker-stack)
(bind-key* "M-g o" 'xref-find-definitions-other-window)

(provide 'init-etags)

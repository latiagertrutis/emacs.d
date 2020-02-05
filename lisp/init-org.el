;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

;; We will use some of the packages from org extras, especially org-drill and org-mime for HTML exports
(maybe-require-package 'org-drill)
(maybe-require-package 'org-mime)
(maybe-require-package 'ox-pandoc)
(setq org-latex-classes
      '("beamer"
        "\\documentclass\[presentation\]\{beamer\}"
        ("\\section\{%s\}" . "\\section*\{%s\}")
        ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
        ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;; Function to determine work time
(defun calcFunc-dateDiffToHMS (date1 date2 worktime-per-day)
  "Calculate the difference of DATE1 and DATE2 in HMS form.
Each day counts with WORKTIME-PER-DAY hours."
  (cl-labels ((dateTrunc (date)
                         (calcFunc-date (calcFunc-year date)
                                        (calcFunc-month date)
                                        (calcFunc-day date)))
              (datep (date)
                     (and (listp date)
                          (eq (car date) 'date))))
    (if (and (datep date1)
             (datep date2))
        (let* ((business-days (calcFunc-bsub
                               (dateTrunc date1)
                               (dateTrunc date2))))
          (calcFunc-add
           (calcFunc-hms (calcFunc-mul business-days worktime-per-day) 0 0)
           (calcFunc-sub (calcFunc-time date1) (calcFunc-time date2)))
          )
      0)))

;; Better org return

(defun ha/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond
     ;; Open links like usual
     ((eq 'link (car (org-element-context)))
      (org-return))
     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((and (org-in-item-p) (not (bolp)))
      (if (org-element-property :contents-begin (org-element-context))
          (org-insert-heading)
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (progn (org-end-of-meta-data)
                 (org-insert-heading))
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))
     (t
      (org-return)))))
(after-load 'org
  (define-key org-mode-map (kbd "RET")  #'ha/org-return)
  (define-key org-mode-map (kbd "C-c t")  #'org-todo))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "PROGRESS(p!)" "|" "DONE(d!)")))
(setq org-log-done t)

;; Color and display

(maybe-require-package 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(maybe-require-package 'org-tree-slide)
(setq org-tree-slide-skip-outline-level 4)
(org-tree-slide-simple-profile)

;; Org present
(require-package 'org-present)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images t t)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

;; (when *is-a-mac*
;;   (maybe-require-package 'grab-mac-link))

;; (maybe-require-package 'org-cliplink)

;; (define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
;;   "A keymap for handy global access to org helpers, particularly clocking.")

;; (define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-jump-to-current-clock)
;; (define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
;; (define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
;; (define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
;; (define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)


;; ;; Various preferences
;; (setq org-log-done t
;;       org-edit-timestamp-down-means-later t
;;       org-hide-emphasis-markers t
;;       org-catch-invisible-edits 'show
;;       org-export-coding-system 'utf-8
;;       org-fast-tag-selection-single-key 'expert
;;       org-html-validation-link nil
;;       org-export-kill-product-buffer-when-displayed t
;;       org-tags-column 80)


;; ;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; ;; TODO: fail gracefully
;; (defun sanityinc/grab-ditaa (url jar-name)
;;   "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
;;   ;; TODO: handle errors
;;   (message "Grabbing %s for org." jar-name)
;;   (let ((zip-temp (make-temp-name "emacs-ditaa")))
;;     (unwind-protect
;;         (progn
;;           (when (executable-find "unzip")
;;             (url-copy-file url zip-temp)
;;             (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
;;                                    " " (shell-quote-argument jar-name) " > "
;;                                    (shell-quote-argument org-ditaa-jar-path)))))
;;       (when (file-exists-p zip-temp)
;;         (delete-file zip-temp)))))

;; (after-load 'ob-ditaa
;;   (unless (and (boundp 'org-ditaa-jar-path)
;;                (file-exists-p org-ditaa-jar-path))
;;     (let ((jar-name "ditaa0_9.jar")
;;           (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
;;       (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
;;       (unless (file-exists-p org-ditaa-jar-path)
;;         (sanityinc/grab-ditaa url jar-name)))))

;; (after-load 'ob-plantuml
;;   (let ((jar-name "plantuml.jar")
;;         (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
;;     (setq org-plantuml-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
;;     (unless (file-exists-p org-plantuml-jar-path)
;;       (url-copy-file url org-plantuml-jar-path))))


;; Re-align tags when window shape changes
(after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))


;; 

;; (maybe-require-package 'writeroom-mode)

;; (define-minor-mode prose-mode
;;   "Set up a buffer for prose editing.
;; This enables or modifies a number of settings so that the
;; experience of editing prose is a little more like that of a
;; typical word processor."
;;   nil " Prose" nil
;;   (if prose-mode
;;       (progn
;;         (when (fboundp 'writeroom-mode)
;;           (writeroom-mode 1))
;;         (setq truncate-lines nil)
;;         (setq word-wrap t)
;;         (setq cursor-type 'bar)
;;         (when (eq major-mode 'org)
;;           (kill-local-variable 'buffer-face-mode-face))
;;         (buffer-face-mode 1)
;;         ;;(delete-selection-mode 1)
;;         (setq-local blink-cursor-interval 0.6)
;;         (setq-local show-trailing-whitespace nil)
;;         (setq-local line-spacing 0.2)
;;         (setq-local electric-pair-mode nil)
;;         (ignore-errors (flyspell-mode 1))
;;         (visual-line-mode 1))
;;     (kill-local-variable 'truncate-lines)
;;     (kill-local-variable 'word-wrap)
;;     (kill-local-variable 'cursor-type)
;;     (kill-local-variable 'blink-cursor-interval)
;;     (kill-local-variable 'show-trailing-whitespace)
;;     (kill-local-variable 'line-spacing)
;;     (kill-local-variable 'electric-pair-mode)
;;     (buffer-face-mode -1)
;;     ;; (delete-selection-mode -1)
;;     (flyspell-mode -1)
;;     (visual-line-mode -1)
;;     (when (fboundp 'writeroom-mode)
;;       (writeroom-mode 0))))

;; ;;(add-hook 'org-mode-hook 'buffer-face-mode)


;; (setq org-support-shift-select t)
;; 
;; ;;; Capturing

;; (global-set-key (kbd "C-c c") 'org-capture)

;; (setq org-capture-templates
;;       `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
;;          "* NEXT %?\n%U\n" :clock-resume t)
;;         ("n" "note" entry (file "")
;;          "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
;;         ))


;; 
;;; Refiling

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))


;; 
;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday 1
        org-agenda-span 'week
        org-agenda-include-diary nil
        ))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)

(setq org-agenda-files
      (list "~/org/todos.org"))

;; 
;; ;;; Org clock

;; ;; Save the running clock and all clock history when exiting Emacs, load it on startup
;; (after-load 'org
;;   (org-clock-persistence-insinuate))
;; (setq org-clock-persist t)
;; (setq org-clock-in-resume t)

;; ;; Save clock data and notes in the LOGBOOK drawer
;; (setq org-clock-into-drawer t)
;; ;; Save state changes in the LOGBOOK drawer
;; (setq org-log-into-drawer t)
;; ;; Removes clocked tasks with 0:00 duration
;; (setq org-clock-out-remove-zero-time-clocks t)

;; ;; Show clock sums as hours and minutes, not "n days" etc.
;; (setq org-time-clocksum-format
;;       '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


;; 
;; ;;; Show the clocked-in task - if any - in the header line
;; (defun sanityinc/show-org-clock-in-header-line ()
;;   (setq-default header-line-format '((" " org-mode-line-string " "))))

;; (defun sanityinc/hide-org-clock-from-header-line ()
;;   (setq-default header-line-format nil))

;; (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
;; (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
;; (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

;; (after-load 'org-clock
;;   (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
;;   (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


;; 
;; (when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
;;   (add-hook 'org-clock-in-hook
;;             (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
;;                                 (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
;;   (add-hook 'org-clock-out-hook
;;             (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
;;                                 "tell application \"org-clock-statusbar\" to clock out"))))


;; 
;; ;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; ;; TODO: nested projects!


;; 
;; ;;; Archiving

;; (setq org-archive-mark-done nil)
;; (setq org-archive-location "%s_archive::* Archive")



;; 

;; (require-package 'org-pomodoro)
;; (setq org-pomodoro-keep-killed-pomodoro-time t)
;; (after-load 'org-agenda
;;   (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; ;; Show iCal calendars in the org agenda
;; ;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;; ;;   (setq org-agenda-include-diary t
;; ;;         org-agenda-custom-commands
;; ;;         '(("I" "Import diary from iCal" agenda ""
;; ;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;; ;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;; ;;             (lambda ()
;; ;;               (goto-char (point-min))
;; ;;               (save-excursion
;; ;;                 (while (re-search-forward "^[a-z]" nil t)
;; ;;                   (goto-char (match-beginning 0))
;; ;;                   (insert "0:00-24:00 ")))
;; ;;               (while (re-search-forward "^ [a-z]" nil t)
;; ;;                 (goto-char (match-beginning 0))
;; ;;                 (save-excursion
;; ;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;; ;;                 (insert (match-string 0))))))


;; (after-load 'org
;;   (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
;;   (when *is-a-mac*
;;     (define-key org-mode-map (kbd "M-h") nil)
;;     (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

;; (after-load 'org
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    `((R . t)
;;      (ditaa . t)
;;      (dot . t)
;;      (emacs-lisp . t)
;;      (gnuplot . t)
;;      (haskell . nil)
;;      (latex . t)
;;      (ledger . t)
;;      (ocaml . nil)
;;      (octave . t)
;;      (plantuml . t)
;;      (python . t)
;;      (ruby . t)
;;      (screen . nil)
;;      (,(if (locate-library "ob-sh") 'sh 'shell) . t)
;;      (sql . t)
;;      (sqlite . t))))


(defun insert-week-hours-table ()
  "Insert week table for the hours."
  (interactive)
  (end-of-buffer)
  (let* ((now (decode-time))
         (this-week (copy-sequence now))
         (next-week (copy-sequence now)))
    (cl-decf (nth 3 this-week) (mod (- (nth 6 this-week) 1) 7))
    (cl-incf (nth 3 next-week) (mod (- 7 (nth 6 next-week)) 7))
    (insert (format-time-string "* %G: Week => %B(%d) - " (apply #'encode-time this-week))
            (format-time-string "%B(%d)\n" (apply #'encode-time next-week))
            "   |   | ENTRADA | SALIDA | HORAS |\n"
            "   |---+---------+--------+-------|\n"
            "   | # |         |        |       |\n"
            "   | # |         |        |       |\n"
            "   | # |         |        |       |\n"
            "   | # |         |        |       |\n"
            "   | # |         |        |       |\n"
            "   |---+---------+--------+-------|\n"
            "   |   |         |        |       |\n"
            "   #+TBLFM: $4=dateDiffToHMS($3,$2,8)::@>$>=vsum(@I$>..@II$>)\n")))

(provide 'init-org)
;;; init-org.el ends here

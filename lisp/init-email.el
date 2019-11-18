;; IMAP

;; Let's have a chance to login against local IMAP services
;; (add-to-list 'auth-sources '(:source "~/.authinfo"))

(require 'smtpmail)
;; Bandeja primaria
(setq gnus-select-method
      '(nnimap "posteo"
               (nnimap-address "posteo.de")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; (setq gnus-secondary-select-methods
;;       '())

;; SMTP & other
(setq message-send-mail-function 'smtpmail-send-it
      auth-source-debug t
      auth-sources '((:source "~/.emacs.d/secrets/.authinfo.gpg"))
      gnus-message-archive-group '((".*" "nnimap:Sent"))
      message-signature-file "~/.emacs.d/.signature"
      user-mail-address "teorodrip@posteo.net"
      user-full-name "Mateo Rodriguez Ripolles"
      smtpmail-default-smtp-server "smtp.posteo.de"
      smtpmail-smtp-server  "posteo.de"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

;; Custom faces
(copy-face 'font-lock-variable-name-face 'gnus-face-6)
(setq gnus-face-6 'gnus-face-6)
(copy-face 'font-lock-constant-face 'gnus-face-7)
(setq gnus-face-7 'gnus-face-7)
(copy-face 'gnus-face-7 'gnus-summary-normal-unread)
(copy-face 'font-lock-constant-face 'gnus-face-8)
(set-face-foreground 'gnus-face-8 "gray50")
(setq gnus-face-8 'gnus-face-8)
(copy-face 'font-lock-constant-face 'gnus-face-9)
(set-face-foreground 'gnus-face-9 "gray70")
(setq gnus-face-9 'gnus-face-9)

;; Group interface
;; *Group* buffer: how to format each group entry.
(setq gnus-group-line-format "%M%m %8{│%} %6N (nl) %8{│%} %6t (tot) %8{│%} %(%-30,30g%) %8{│%} %(%-20,20G%)\n"
      ;;
      ;; %var details C-h i
      ;;`M' An asterisk if the group only has marked articles.
      ;;'N' Number of unread articles.
      ;;`t' Estimated total number of articles.
      ;;`G' Group name.
      ;;`D' Newsgroup description.
      ;;`m' `%' (`gnus-new-mail-mark') if there has arrived new mail to the
      ;;    group lately.
      ;;`D' Last time the group as been accessed.
      ;;
      ;; For the record, a default group line format
      ;;(setq gnus-group-line-format "%M\%S\%p\%P\%5y: %(%-40,40g%)%6,6~(cut 2)d\n")
      )

;; Summary interface
(setq gnus-summary-make-false-root 'dummy)
(setq gnus-summary-make-false-root-always nil)
(defun oxy-unicode-threads ()
  (interactive)
  (setq gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{│%}%) %6{□%}  %S\n"
        gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{│%} %6{%B%} %s\n"
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root "■ "
        gnus-sum-thread-tree-false-root "□ "
        gnus-sum-thread-tree-single-indent "▣ "
        gnus-sum-thread-tree-leaf-with-other "├─▶ "
        gnus-sum-thread-tree-vertical "│"
        gnus-sum-thread-tree-single-leaf "└─▶ "))

(defun oxy-unicode-threads-heavy ()
  (interactive)
  (setq gnus-summary-line-format "%(%-7,7d│%)%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
        gnus-summary-dummy-line-format "       %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root "┏● "
        gnus-sum-thread-tree-false-root " ○ "
        gnus-sum-thread-tree-single-indent " ● "
        gnus-sum-thread-tree-leaf-with-other "┣━━❯ "
        gnus-sum-thread-tree-vertical "┃"
        gnus-sum-thread-tree-single-leaf "┗━━❯ "))

(oxy-unicode-threads-heavy)


(defun gnus-show-all ()
  "show all, or t->50 to show 50 old mail"
  (gnus-summary-insert-old-articles 50))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

(setq gnus-use-cache t)

(add-hook 'gnus-summary-prepare-hook '(lambda () (run-with-idle-timer 0.1 nil 'gnus-show-all)))

(provide 'init-email)

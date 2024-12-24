;; -*- lexical-binding: t; -*-

(setf user-mail-address (getenv "MAIL_ADDRESS")
      user-full-name "luminous99"
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      calendar-date-style 'european)

(use-package gnus
  :ensure t
  :config
  (setq gnus-article-sort-functions
	    '(gnus-article-sort-by-date
	      gnus-article-sort-by-number gnus-article-sort-by-author
	      gnus-article-sort-by-subject gnus-article-sort-by-score)
	    gnus-select-method '(nnimap "gmail"
				                    (nnimap-address "imap.gmail.com")
				                    (nnimap-server-port 993)
				                    (nnimap-stream ssl)))
  (gnus-demon-add-handler 'gnus-demon-scan-news 10 10)
  (gnus-demon-init))

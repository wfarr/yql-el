;;; yql.el --- Emacs Lisp library for accessing YQL

;; Copyright (c) 2009 Will Farrington, Joshua Justice

;; Authors: Will Farrington, Joshua Justice
;; URL: http://github.com/wfarr/yql.el/tree/master
;; Version: 0.01pre
;; Keywords: yql, yahoo

;;; Commentary:

;; Use (require 'yql)

(require 'url)

(defvar yql-data-tables (yql-get-data-tables))

(defun yql-get-data-tables ()
  (yql show tables))

(defmacro yql (query)
  ;; Holy fuck this part will be hard.
)

(defun make-yql-request (string)
  "Takes in a string and makes it all HTML-friendly and such, and then sends it on to Yahoo!'s YQL servers.

It's neat bee tee dubz."
  (let ((yql-public-str "http://query.yahooapis.com/v1/public/yql?q=")
        (target (replace-regexp-in-string "\\ " "%20" string))
        (url-max-redirections 0)
        (url-request-method "GET"))
    (url-retrieve-synchronously (concat yql-public-str target)))
    )
  
(defun test-yql ()
  "Test cases that should work by 4pm tomorrow."
  (print
   (yql select (temp (condition item)) from (forecast weather) where (=: location 30313)))
  (print
   (yql select * from (places flickr) where (and: (=: query "north beach") (>: latitude 38))))
)

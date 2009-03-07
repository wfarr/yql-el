;;; yql.el --- Emacs Lisp library for accessing YQL

;; Copyright (c) 2009 Will Farrington, Joshua Justice

;; Authors: Will Farrington, Joshua Justice
;; URL: http://github.com/wfarr/yql.el/tree/master
;; Version: 0.01pre
;; Keywords: yql, yahoo

;;; Commentary:

;; Use (require 'yql)

(require 'json)
(require 'url)

(defvar yql-data-tables (yql-get-data-tables))

(defun yql-get-data-tables ()
  (let ((list (yql-make-request "show tables")))
    (yql-select-symbol 'table list)))

(defmacro yql (query)
  ;; Holy fuck this part will be hard.
)

(defun yql-select-symbol (symbol list)
  (let ((result (yql-find-symbol-val-in-list symbol list)))
    (if (or (typep result 'list)
            (typep result 'string)
            (typep result 'number))
        result
      (coerce result 'list))))

(defun yql-clean-up-query-string (string)
  (let* ((string (replace-regexp-in-string "\\ " "%20" string))
         (string (replace-regexp-in-string "\"" "%22" string))
         (string (replace-regexp-in-string "\'" "%27" string)))
    string))

(defun yql-make-request (string)
  "Takes in a string and makes it all HTML-friendly and such, and then sends it on to Yahoo!'s YQL servers.

It's neat bee tee dubz."
  (let ((yql-public-str "http://query.yahooapis.com/v1/public/yql?q=")
        (target (yql-clean-up-query-string string))
        (url-max-redirections 0)
        (url-request-method "GET"))
    (with-current-buffer
        (url-retrieve-synchronously (concat yql-public-str target "&format=json"))
        (let ((str (buffer-substring (point-min) (point-max))))
          (end-of-buffer)
          (re-search-backward "^\\(.*\\)$")
          (json-read-from-string (match-string 1))
          ))))

(defun yql-find-symbol-val-in-list (symbol list)
  (cond ((null list) nil)
        ((not (listp list)) nil)
        ((eq (car list) symbol) (cdr list))
        ((listp (car list)) (or (yql-find-symbol-val-in-list symbol (car list))
                                (yql-find-symbol-val-in-list symbol (cdr list))))
        (t
         (yql-find-symbol-val-in-list symbol (cdr list)))))

(defun test-yql ()
  "Test cases that should work by 4pm tomorrow."
  (print
   (yql-select-symbol 'temp (yql-make-request "select item.condition.temp from weather.forecast where location=30313")))
;;   (yql select (temp (condition item)) from (forecast weather) where (=: location 30313)))
  (print
   (yql-select-symbol
    '0
    (yql-select-symbol 'place (yql-make-request "select * from flickr.places where query=\"north beach\""))))
;;   (yql select * from (places flickr) where (and: (=: query "north beach") (>: latitude 38))))
)

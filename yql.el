;;; yql.el --- Emacs Lisp library for accessing YQL

;; Copyright (c) 2009 Will Farrington, Joshua Justice

;; Authors: Will Farrington, Joshua Justice
;; URL: http://github.com/wfarr/yql.el/tree/master
;; Version: 0.1
;; Keywords: yql, yahoo

;; This file is not (yet) part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Use (require 'yql)

;; See README for usage.

(require 'json)
(require 'url)

(defun yql-yahoo-search (query)
  "Perform a Yahoo! search with `query'."
  (interactive "sQuery string?: ")
  (let ((result (yql-filter 'result
                            (yql-select "title,abstract,url" "search.web"
                                        (concat (format "query=\"%s\"" query)
                                                "LIMIT 5")))))
    (save-excursion
      (set-buffer (get-buffer-create "*YQL Search Results*"))
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (dolist (item result)
        (let ((title (yql-filter 'title item))
              (abstract (yql-filter 'abstract item))
              (url (yql-filter 'url item)))
          (insert (concat title "\n  "
                          (if (not (string= abstract ""))
                              (concat abstract "\n  "))
                          url "\n"))))
      (local-set-key (kbd "q") 'kill-this-buffer)
      (goto-address)
      (define-key goto-address-highlight-keymap (kbd "RET") 'goto-address-at-point))
    (pop-to-buffer "*YQL Search Results*")))

(defun yql-yahoo-stocks (stocks)
  "Returns some basic information about whatever `stocks' you list (by abbreviation) in a
comma-separated list (with or without spaces)."
  (interactive "sStocks (Comma-separated)?: ")
  (let ((result
         (yql-filter 'row
                     (yql-select
                      "*" "csv"
                      (format "url='http://download.finance.yahoo.com/d/quotes.csv?s=%s&f=sl1d1c1&e=.csv' and columns='symbol,price,date,change'"
                              stocks)))))
    (save-excursion
      (set-buffer (get-buffer-create "*YQL Stock Results*"))
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (if (string-match "," stocks)
          (dolist (item result)
            (let ((symbol (yql-filter 'symbol item))
                  (price (yql-filter 'price item))
                  (change (yql-filter 'change item)))
              (insert (concat symbol "\n  Price: " price "\n  Change: " change "\n"))))
        (let ((symbol (yql-filter 'symbol result))
              (price (yql-filter 'price result))
              (change (yql-filter 'change result)))
          (insert (concat symbol "\n  Price: " price "\n  Change: " change "\n"))))
      (local-set-key (kbd "q") 'kill-this-buffer))
    (pop-to-buffer "*YQL Stock Results*")))

(defun yql-twitter-stream (user &optional limit)
  "Fetches a the twitter stream for `user' with public stream.
`limit' is 10 by default, and can go up to 20."
  (interactive "sWhich user?: ")
  (let ((result
         (yql-filter
          'item
          (yql-select
           "title,pubDate" "rss"
           (format "url='http://twitter.com/statuses/user_timeline/%s.rss' LIMIT %s" user (if limit limit 10))))))
    (save-excursion
      (set-buffer (get-buffer-create "*YQL Twitter Stream*"))
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (insert (concat "Tweets from " user ":\n\n"))
      (dolist (item result)
        (let ((tweet-time
               (eval (cons 'encode-time (parse-time-string (yql-filter 'pubDate item)))))
              (tweet-text
               (replace-regexp-in-string
                (format "^%s: " user) ""
                (yql-filter 'title item))))
          (insert
           (concat
            "  * ("
            (cond ((string= (format-time-string "%D")
                            (format-time-string "%D" tweet-time))
                   (format-time-string "%I:%M %p" tweet-time))
                  ((eq (cadddr (decode-time tweet-time))
                       (- (cadddr (decode-time (current-time))) 1))
                   (format-time-string "%I:%M %p, Yesterday" tweet-time))
                  (t
                   (format-time-string "%I:%M %p %D" tweet-time)))
            ") " tweet-text "\n"))))
      (local-set-key (kbd "q") 'kill-this-buffer))
    (pop-to-buffer "*YQL Twitter Stream*")))

(defmacro yql (query &rest args)
  "Constructs a function call based on `query', which should be one of
`show', `desc', `select', or `filter'."
  `(cond ((eq ,query 'show)
          (yql-show))
         ((eq ,query 'desc)
          ,(cons 'yql-desc args))
         ((eq ,query 'select)
          ,(cons 'yql-select args))
         ((eq ,query 'filter)
          ,(cons 'yql-filter args))
         (t
          (error "`query' must be one of '`show', '`desc', '`select', or '`filter'!"))))

(defun yql-show ()
  "Makes a GET request to YQL's public-facing api for 'show tables'.

NOTE: This is because 'tables' is the only valid argument for 'show' in YQL."
  (let ((list (yql-send-request "show tables")))
    (yql-filter 'table list)))

(defun yql-desc (table)
  "Makes a GET request to YQL's public-facing api for 'desc `table'', where
table is any item in `yql-show'."
  (yql-filter 'table (yql-send-request (concat "DESC " table))))

(defun yql-select (target table &optional qualifiers)
  "Makes a GET request to YQL's public-facing api for:
    'select `target' from `table' [where `qualifiers']'

`table' should be one of `yql-show' and `target' and `qualifiers'
should be valid for `table'."
  (let ((qualifiers (if qualifiers (concat " WHERE " (concatenate 'string qualifiers)) "")))
    (yql-send-request (concat "SELECT " target " FROM " table qualifiers))))

(defun yql-filter (symbol list)
  "Filters any JSON returned from a `yql-send-request' call for the value(s)
associated to `symbol', where the JSON is `list'."
  (if (eq (type-of symbol) 'string)
      (setq symbol (intern symbol)))
  (let ((result (yql-search-for-symbol symbol list)))
    (if (or (typep result 'list)
            (typep result 'string)
            (typep result 'number))
        result
      (coerce result 'list))))

(defun yql-escape-query-string (string)
  "Used internally. You probably won't want to use it."
  (let* ((string (replace-regexp-in-string "\\ " "%20" string))
         (string (replace-regexp-in-string "\"" "%22" string))
         (string (replace-regexp-in-string "\&" "%26" string))
         (string (replace-regexp-in-string "\'" "%27" string)))
    string))

(defun yql-search-for-symbol (symbol list)
  "Used internally. You probably won't want to use it."
  (cond ((null list) nil)
        ((not (listp list)) nil)
        ((eq (car list) symbol) (cdr list))
        ((listp (car list)) (or (yql-search-for-symbol symbol (car list))
                                (yql-search-for-symbol symbol (cdr list))))
        (t
         (yql-search-for-symbol symbol (cdr list)))))

(defun yql-send-request (string)
  "Performs a GET request based on a query string. The string is escaped
before performing the request.

Returns an S-expression representation of the JSON data returned."
  (let ((yql-public-str "http://query.yahooapis.com/v1/public/yql?q=")
        (target (yql-escape-query-string string))
        (url-max-redirections 0)
        (url-request-method "GET"))
    (with-current-buffer
        (url-retrieve-synchronously (concat yql-public-str target "&format=json"))
        (let ((str (buffer-substring (point-min) (point-max))))
          (end-of-buffer)
          (re-search-backward "^\\(.*\\)$")
          (json-read-from-string (match-string 1))))))

(provide 'yql)

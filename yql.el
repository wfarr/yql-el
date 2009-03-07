;;; yql.el --- Emacs Lisp library for accessing YQL

;; Copyright (c) 2009 Will Farrington, Joshua Justice

;; Authors: Will Farrington, Joshua Justice
;; URL: http://github.com/wfarr/yql.el/tree/master
;; Version: 0.01pre
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

(defvar yql-data-tables (yql-show)
  "The list of tables available in YQL.")

(defun yql-yahoo-search (query)
  (let ((result (yql-filter 'result
                            (yql-select "title,abstract,url" "search.web"
                                        (concat (format "query=\"%s\"" query)
                                                "LIMIT 5")))))
    (with-output-to-temp-buffer "*Search Results*"
      (dolist (item result)
        (let ((title (yql-filter 'title item))
              (abstract (yql-filter 'abstract item))
              (url (yql-filter 'url item)))
          (print (concat title "\n  " url "\n  " abstract)))))))

(defmacro yql (query &rest args)
  "Constructs a function call based on `query', which should be one of
`show', `desc', or `select'."
  `(cond ((eq ,query 'show)
          (yql-show))
         ((eq ,query 'desc)
          (yql-desc ,args))
         ((eq ,query 'select)
          (yql-select ,args))))

(defun yql-show ()
  "Makes a GET request to YQL's public-facing api for 'show tables'.

NOTE: This is because 'tables' is the only valid argument for 'show' in YQL."
  (let ((list (yql-send-request "show tables")))
    (yql-filter 'table list)))

(defun yql-desc (table)
  "Makes a GET request to YQL's public-facing api for 'desc `table'', where
table is any item in `yql-data-tables'."
  (if (memq table yql-data-tables)
      (yql-filter 'table (yql-send-request (concat "DESC " table)))
    (error "Should be one of `yql-data-tables'!")))

(defun yql-select (target table &optional qualifiers)
  "Makes a GET request to YQL's public-facing api for:
    'select `target' from `table' [where `qualifiers']'

`table' should be one of `yql-data-tables' and `target' and `qualifiers'
should be valid for `table'."
  (let ((qualifiers (if qualifiers (concat " WHERE " (concatenate 'string qualifiers)) "")))
    (yql-send-request (concat "SELECT " target " FROM " table qualifiers))))

(defun yql-filter (symbol list)
  "Filters any JSON returned from a `yql-send-request' call for the value(s)
associated to `symbol', where the JSON is `list'."
  (let ((result (yql-search-for-symbol symbol list)))
    (if (or (typep result 'list)
            (typep result 'string)
            (typep result 'number))
        result
      (coerce result 'list))))

(defun yql-escape-query-string (string)
  (let* ((string (replace-regexp-in-string "\\ " "%20" string))
         (string (replace-regexp-in-string "\"" "%22" string))
         (string (replace-regexp-in-string "\'" "%27" string)))
    string))

(defun yql-search-for-symbol (symbol list)
  (cond ((null list) nil)
        ((not (listp list)) nil)
        ((eq (car list) symbol) (cdr list))
        ((listp (car list)) (or (yql-search-for-symbol symbol (car list))
                                (yql-search-for-symbol symbol (cdr list))))
        (t
         (yql-search-for-symbol symbol (cdr list)))))

(defun yql-send-request (string)
  "Performs a GET request based on a query string. The string is escaped for spaces,
single-quotes, and double-quotes before performing the request.

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
          (json-read-from-string (match-string 1))
          ))))

;; Tests

(defun test-yql ()
  (print
   (yql-filter 'temp (yql-send-request "select item.condition.temp from weather.forecast where location=30313")))
  (print
   (yql-filter 'place (yql-send-request "select latitude from flickr.places where query=\"north beach\""))))


(provide 'yql)

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

(require 'json)
(require 'url)

(defvar yql-data-tables (yql-show))

(defmacro yql (query &rest args)
  `(cond ((eq ,query 'show)
          (yql-show))
         ((eq ,query 'desc)
          (yql-desc ,args))
         ((eq ,query 'select)
          (yql-select ,args))))

(defun yql-show ()
  (let ((list (yql-make-request "show tables")))
    (yql-select-symbol 'table list)))

(defun yql-desc (table)
  (yql-select-symbol 'table (yql-make-request (concat "DESC " table))))

(defun yql-select (selector target table &rest args)
  (yql-select-symbol
   selector
   (yql-make-request (concat "SELECT " target " FROM " table " " args))))

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

(defun yql-find-symbol-val-in-list (symbol list)
  (cond ((null list) nil)
        ((not (listp list)) nil)
        ((eq (car list) symbol) (cdr list))
        ((listp (car list)) (or (yql-find-symbol-val-in-list symbol (car list))
                                (yql-find-symbol-val-in-list symbol (cdr list))))
        (t
         (yql-find-symbol-val-in-list symbol (cdr list)))))

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

;; Tests

(defun test-yql ()
  "Test cases that should work by 4pm tomorrow."
  (print
   (yql-select-symbol 'temp (yql-make-request "select item.condition.temp from weather.forecast where location=30313")))
  (print
   (yql-select-symbol 'place (yql-make-request "select latitude from flickr.places where query=\"north beach\""))))


(provide 'yql)

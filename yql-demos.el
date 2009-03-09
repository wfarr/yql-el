;; Some demos

(defun yql-yahoo-search (query-string)
  "Perform a Yahoo! search with `query'."
  (interactive "sQuery string?: ")
  (let ((result (yql-filter
                 'result
                 (yql select title,abstract,url search.web
                      (concat (format "query=\"%s\"" query-string)
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
      (local-set-key (kbd "q") 'delete-window)
      (goto-address)
      (define-key goto-address-highlight-keymap (kbd "RET") 'goto-address-at-point))
    (pop-to-buffer "*YQL Search Results*")))


(defun yql-yahoo-stocks (stocks)
  "Returns some basic information about whatever `stocks' you list (by abbreviation) in a
comma-separated list (with or without spaces)."
  (interactive "sStocks (Comma-separated)?: ")
  (let ((result
         (yql-filter
          'row
          (yql select * csv
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
      (local-set-key (kbd "q") 'delete-window))
    (pop-to-buffer "*YQL Stock Results*")))

(defun yql-twitter-stream (user)
  "Fetches a the twitter stream for `user' with public stream.
`limit' is 10 by default, and can go up to 20."
  (interactive "sWhich user?: ")
  (let ((result
         (yql-filter 'item
                     (yql select title,pubDate rss
                          (format "url='http://twitter.com/statuses/user_timeline/%s.rss'" user)))))
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
      (local-set-key (kbd "q") 'delete-window))
    (pop-to-buffer "*YQL Twitter Stream*")))

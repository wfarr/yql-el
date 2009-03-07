;; Based on Phil Hagelberg's awesome stuff

(setq presentation-slides '("intro"
                            "intro-b"
                            "what-is-emacs"
                            "sales-pitch"
                            "sales-pitch-b"
                            "api"
                            "api-b"
                            "api-c"
                            "api-d"
                            "twitter-ex"
                            "twitter-ex-b"))

(defface presentation-link-face
  `((t (:foreground "blue"))) "Face for hyperlinks.")
(set-face-attribute 'presentation-link-face nil
                    :underline t)

(defvar presentation-minor-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "[") 'presentation-back)
    (define-key map (kbd "]") 'presentation-forward)
    map))

(defvar presentation-font-size "24"
  "Change to 24 when at 800x600.")

(define-minor-mode presentation-minor-mode
  "For presentations."
  :keymap presentation-minor-mode-keymap
  (ignore-errors
    (set-default-font
     (concat "-unknown-Inconsolata-normal-normal-normal-*-"
                            presentation-font-size "-*-*-*-m-0-*-*")))
  (scroll-bar-mode -1)
  (font-lock-add-keywords nil
                          '(("\\(http://[^ \t\n)]+\\)" .
                             'presentation-link-face)))
  (font-lock-add-keywords
   nil `(("\\(lambda\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil)))))
  (goto-char (point-min)))

(defun presentation-forward (&optional amt)
  (interactive)
  (let* ((pos (position (buffer-name) presentation-slides
                        :test 'string=))
         (new-pos (+ (or amt 1) pos))
         (new-file (nth new-pos presentation-slides)))
    (find-file (or new-file (car presentation-slides)))
    (presentation-minor-mode t)))

(defun presentation-back ()
  (interactive) (presentation-forward -1))

(find-file (car presentation-slides)) (presentation-minor-mode)
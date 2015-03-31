(require 'cl)

;;; Utilities
(defun flash-region (start end &optional timeout)
  "Borrowed from skewer.el. Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.35) nil 'delete-overlay overlay)))
(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10).
From http://xahlee.blogspot.com/2011/09/emacs-lisp-function-to-trim-string.html"
  (replace-regexp-in-string "\\`[ \t\n]*" ""
                            (replace-regexp-in-string "[ \t\n]*\\'" "" string)))
(defvar org-study-hour-day-begins 4)
(defun org-study-today ()
  "Like org-today, but start days later than midnight. So if the
user reviews cards after midnight it doesn't count as the next
day."
  (- (org-today) (if (<= (string-to-int (format-time-string "%H"))
                         org-study-hour-day-begins)
                     0
                   1)))

;;; Define a stamp that schedules a note for review
(require 'org-element)
(add-to-list 'org-element-all-objects    'studystamp)
(add-to-list 'org-element-all-successors 'studystamp)
(defvar org-studystamp-re
  (concat "STUDY{"
          org-ts-regexp-both ","
          "\\([0-9]*\\)"     ","   ; next interval
          "\\([0-9]*\\.?[0-9]*\\)" ; ease factor
          "}"))

(defvar org-study-multiple-choice-re "\\[[^]/]*/[^]/]*\\]")
(defvar org-study-answer-categories
  '((multiple-choice
     (looking-back (concat org-study-multiple-choice-re "\s*"))
     (cons (match-beginning 0) (match-end 0)))
    (explicit
     (looking-back "\\][\t ]*")
     (cons (match-beginning 0) (match-end 0)))
    (description
     (save-excursion (org-backward-element) (org-at-item-description-p))
     (cons 
      (save-excursion (org-beginning-of-item) (search-forward "::" nil t))
      (save-excursion (org-end-of-item) (point))))
    (subtree
     (or (org-at-heading-p) (org-at-item-p))
     (cons (progn (forward-line) (point))
           (progn (org-end-of-subtree) (point)))))

  "A list of card types recognizable by studystamps. Each type is
in the form NAME, PREDICATE, BOUNDS, where NAME is a symbol to
name the type, PREDICATE is an expression that will return
non-nil if point is at a studystamp of that kind, and BOUNDS is
an expression returning a cons cell of the boundaries of the
answer.")

(defun org-study-get-answer-category (&optional studystamp)
  " If studystamp is nil, assume we are at the beginning of the
studystamp. Can't parse it because this function is called from
`org-element-studystamp-parser"
  (when studystamp (goto-char (org-element-property :begin studystamp)))
  (or (save-excursion (loop for category in org-study-answer-categories
                            if (eval (nth 1 category))
                            return category))
      '(default
         t
         (cons (point) (point)))))

(defun org-study-lookup-answer-category (name)
  (loop for category in org-study-answer-categories
        if (eq name (nth 0 category)) return answer-category))

(defun org-study-answer-bounds (&optional studystamp)
  " If studystamp is nil, assume we are at the beginning of the
studystamp. Can't parse it because this function is called from
`org-element-studystamp-parser"
  (when studystamp (goto-char (org-element-property :begin studystamp)))  
  (save-excursion (eval (nth 2 (org-study-get-answer-category)))))


(defun org-element-studystamp-parser ()
  "Parse studystamp object at point.

Return a list whose CAR is `studystamp' and CDR is a plist with
`:begin', `:end', `:answer-begin', `:answer-end', `:review-day',
`:interval', and `:ease' keywords.

Assume point is at the beginning of 'study'." 
  (save-excursion
    (looking-at org-studystamp-re)
    (let ((begin (match-beginning 0))
          (end (match-end 0))
          (review-day
           (save-match-data
             (org-time-string-to-absolute
              (org-match-string-no-properties 1))))
          (interval (string-to-int (org-match-string-no-properties 2)))
          (ease (string-to-int (org-match-string-no-properties 3)))
          (answer-category (org-study-get-answer-category))
          (answer-bounds (org-study-answer-bounds)))
      (list 'studystamp
            (list :begin begin
                  :end end
                  :review-day review-day
                  :interval interval
                  :ease ease
                  :category-name (nth 0 answer-category)
                  :answer-end (car answer-bounds)
                  :answer-begin (cdr answer-bounds))))))

(defun org-study-create ()
  "Insert a studystamp at point"
  (interactive)
  (org-insert-time-stamp (current-time) nil 'inactive "STUDY{"
                         (format ",1,%.2f}" org-study-starting-ease))
  (save-excursion
    (backward-list) (goto-char (cdr (org-element-timestamp-successor))) ; go to timestamp
    (org-timestamp-up-day)
    (backward-char 6) ;; Go to beginning of studystamp TODO do this by rearranging save-excursions isntead
    (let ((e (org-element-studystamp-parser)))
      (flash-region (org-element-property :answer-begin e)
                    (org-element-property :answer-end e)))))

(defun org-element-studystamp-interpreter (studystamp contents)
  "interpret STUDYSTAMP object as org syntax.
contents is nil"
  (format "STUDY{%s,%d,%.2f}"
          (format-time-string (org-time-stamp-format nil 'inactive)
                              (org-time-from-absolute
                               (org-element-property :review-day
                                                     studystamp)))
          (org-element-property :interval studystamp)
          (org-element-property :ease     studystamp)))
(defun org-element-studystamp-successor ()
  "Search for the next studystamp object.

Return value is a cons cell whose CAR is `studystamp' and CDR is
beginning position."
  (save-excursion
    (when (search-forward-regexp org-studystamp-re nil t)
      (cons 'studystamp (match-beginning 0)))))

;;; Creating and reviewing flashcards
;; TODO: allow customization with properties in the org file
(defvar org-study-starting-ease 2.50)
(defvar org-study-easy-bonus 1.30)

(defun org-study-update (score)
  (save-excursion
    (let ((missed-it nil)
          (e (org-element-studystamp-parser))
          (late-bonus))
      (org-element-put-property e :ease
                                (truncate
                                 (let ((oldease (org-element-property :ease e)))
                                   (max 1.30 (cond ((= score 0)
                                                    (- oldease .20))
                                                   ((= score 1)
                                                    (- oldease .15))
                                                   ((= score 2)
                                                    oldease)
                                                   ((= score 3)
                                                    (* oldease org-study-easy-bonus)))))))
      ;; bonus for remembering overdue cards
      (setq late-bonus (/ (- (org-study-today) (org-element-property :review-day e))
                          (case score
                            (0 1)
                            (1 4) 
                            (2 2)
                            (3 1))))

      (if (= score 0)
          (progn
            (org-element-put-property e :interval 0)
            (org-element-put-property e :review-day (org-study-today)))
        (org-element-put-property e :interval
                                  (max 1
                                       (truncate (* (org-element-property :ease e)
                                                    (+ (org-element-property :interval e) 
                                                       late-bonus)))))
        (org-element-put-property e :review-day
                                  (+ (org-element-property :interval e)
                                     (org-study-today))))

      ;; Update the text
      (delete-region (org-element-property :begin e)
                     (org-element-property :end e))
      (insert (org-element-studystamp-interpreter e nil)))))

;;; Presenting reviewable notes to the user
(defun org-study-hide-answer (studystamp)
  (let* ((s (org-element-property :answer-begin studystamp))
         (e (org-element-property :answer-end studystamp))
         (ov (make-overlay s e)))
    (if (eq (org-element-property :category-name studystamp) 'multiple-choice)
        (overlay-put ov 'display (replace-in-string (buffer-substring-no-properties s e)
                                                    "*" "")
                     'face (list :underline (face-attribute 'default :foreground)))
      (overlay-put ov
                   'face (list :underline (face-attribute 'default :foreground)
                               :foreground (face-attribute 'default :background)))))) 

(defun org-study-reveal-answer (studystamp)
  (remove-overlays (org-element-property :answer-begin studystamp)
                   (org-element-property :answer-end studystamp)))

(defun org-study-due-for-review-p (&optional studystamp)
  (setq studystamp (or studystamp (org-element-studystamp-parser)))
  (<= (org-element-property :review-day studystamp) (org-study-today)))

(defun org-study-prepare-buffer ()
  (let ((e))
    (save-excursion
      (beginning-of-buffer)
      (while (setq e (org-study-next-for-review))
        (org-study-hide-answer e)))))

(defvar org-study-complements '(("true" "false")
                                ("upright" "inverted")
                                ("is" "isn't")
                                ("does" "doesn't")))
(defun org-study-create ()
  "Insert a studystamp at point"
  (interactive)
  (when (region-active-p)
    (let* ((s (region-beginning)) (e (region-end))
           (region-text (trim-string
                         (buffer-substring-no-properties s e)))
           (mult-choice-group (loop for l in org-study-complements
                                    if (member region-text l) 
                                    return (mapconcat (lambda (x)
                                                        (if (equal x region-text)
                                                            (concat "*" x)
                                                          x)) l "/"))))
      (if mult-choice-group
          (progn
            (delete-region s e)
            (insert (concat "[" mult-choice-group "]")))
        (goto-char s) (insert "[")
        (goto-char (1+ e)) (insert "]"))))
  (org-insert-time-stamp (current-time) nil 'inactive "STUDY{"
                         (format ",1,%.2f}" org-study-starting-ease))
  (save-excursion
    (backward-list) (goto-char (cdr (org-element-timestamp-successor))) ; go to timestamp
    (org-timestamp-up-day)
    (backward-char 6) ;; Go to beginning of studystamp TODO do this by rearranging save-excursions isntead
    (let ((e (org-element-studystamp-parser)))
      (flash-region (org-element-property :answer-begin e)
                    (org-element-property :answer-end e)))))

(defun org-study-next ()
  (interactive)
  (forward-char)
  (goto-char (cdr (org-element-studystamp-successor)))
  (org-show-entry))

(defun org-study-next-for-review (&optional idempotent)
  "Go to the next question that's due for review"
  (interactive)
  (unless idempotent (forward-char))
  (let ((e         nil)
        (successor nil))
    (while (and
            (setq successor (org-element-studystamp-successor))
            (goto-char (cdr successor))
            (setq e (org-element-studystamp-parser))
            (not (org-study-due-for-review-p e)))
      (forward-char))
    (when (called-interactively-p)
      (org-show-entry))
    e))
(define-key org-mode-map (kbd "C-c n") 'org-study-next-for-review)

(defun org-study-review ()
  (interactive)
  (let ((e (org-study-next-for-review 'idempotent))
        (score))
    (unless (org-study-due-for-review-p e)
      (error "Card not ready for review"))
    (org-study-reveal-answer e)
    (setq score      (case
                         (read-char-choice "Did you get it? 1,2,3,4 or space"
                                           '(?1 ?2 ?3 ?4 ? ))
                       (?1      0)
                       (?2      1)
                       ((?3 ? ) 2)
                       (?4      3)))
    (org-study-update score)
    (unless (> score 0)
      (org-study-hide-answer e))))

;;; Appearance
(defvar org-study-studystamp-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'org-study-next)
    (define-key map (kbd "N") 'org-study-next-for-review)
    (define-key map (kbd "a") 'org-study-review)
    map)
  "Keymap when cursor is on a studystamp for `org-study'")


(defun org-study-font-lock ()
  (font-lock-add-keywords nil
                          `(( ,org-studystamp-re 
                              (0 (progn
                                   (let ((s (match-beginning 0))
                                         (e (match-end 0)))
                                     (compose-region
                                      s e ?♠)
                                     (put-text-property
                                      s e 'face (if (save-match-data
                                                      (org-study-due-for-review-p))
                                                    'font-lock-string-face
                                                  'font-lock-comment-face))
                                     (put-text-property
                                      s e 'help-echo
                                      (substring-no-properties (match-string 0)))
                                     (put-text-property
                                      s e 'keymap org-study-studystamp-map
                                      ))
                                   nil)))
                            (,org-study-multiple-choice-re
                             (0 (save-match-data
                                  (save-excursion
                                    (let ((s (match-beginning 0))
                                          (e (match-end 0))
                                          (correct-answer))
                                      (goto-char s)
                                      (when (search-forward "*" e t)
                                        (setq all-options (buffer-substring s e))
                                        (setq correct-answer
                                              (buffer-substring (point)
                                                                (1- (save-excursion
                                                                      (search-forward-regexp
                                                                       "/\\|]" e t)))))
                                        ;; (compose-region s e (concat "\t" correct-answer))
                                        (put-text-property s e 'display correct-answer)
                                        (put-text-property s e 'face 'bold)
                                        (put-text-property s e 'help-echo all-options)
                                        ;; (put-text-property
                                        ;;  s e 'modification-hooks
                                        ;;  (list
                                        ;;   clear-line-properties))
                                        ))
                                    nil)
                                  ))))))

(defun org-remove-font-lock-display-properties (beg end)
  "Originally defined in org.el, overwritten to remove all
display properties. Maybe there is a hidden disadvantage to
this."
  (remove-text-properties beg end '(display t)))

;;; Hooks into org-mode etc
(add-hook 'org-mode-hook 'org-study-font-lock)
(add-hook 'org-mode-hook 'org-study-prepare-buffer)
(define-key org-mode-map (kbd "C-c s") 'org-study-create)


(provide 'org-study)

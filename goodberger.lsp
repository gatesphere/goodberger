#!/usr/bin/env newlisp
;; goodberger
;; automated gutenberg pager and emailer
;; sends out bits from a gutenberg text
;;
;; PeckJ
;;

(load "goodberger.conf")

;; functions
; build a url to grab a book
(define (fetch-url (id))
  (string "http://www.gutenberg.org/files/" id "/" id ".txt"))

; build a filename for a book
(define (book-file (id))
  (string bookdir "/" id ".txt"))

; grab the book and put it in the bookdir
(define (get-book (id))
  (write-file (book-file id) ((get-url (fetch-url id) "list") 1)))

; triplify the subs file
(define (triplify (my-list))
  (let (l '())
    (for (x 0 (- (length my-list) 1) 3)
      (set 'l (cons (list (int (my-list x)) (int (my-list (+ x 1))) (string (my-list (+ x 2)))) l)))
(reverse l)))

; process a subscription
(define (act-on-sub (subscr))
  (letex ((id (subscr 0)) (place (subscr 1)) (title (subscr 2)))
    ; check if book exists in bookdir, if not, grab it
    (if (not (member (string id ".txt") (directory bookdir)))
      (get-book id))
    ; grab a chunk from the book
    (set 'bookchunk (get-book-chunk (book-file id) place email-size))
    ; send email with bookchunk
    ;(send-email (list title (bookchunk 1) place (- (bookchunk 0) 1)))
    ; return updated subscription pair
    (set 'retval (list id (bookchunk 0) title))
retval))

; get a bookchunk
(define (get-book-chunk (bookfile) (place) (size))
  (let ((bs "") (line "") (x 0))
    (set 'filehandle (open bookfile "read"))
    (dotimes (x place) (read-line filehandle))
    (while (or (< x size) (and (>= x size) (!= line "\n")))
      (read-line filehandle)
      (set 'line (string (current-line) "\n"))
      ;(println x ": " (current-line))
      (set 'bs (append bs line))
      (inc x))
(list (+ place x) bs)))

; send an email
(define (send-email (title) (text) (begin-line) (end-line))
  (let (subject "") (command-line "")
    (set 'subject (string "From Gutenberger: " title " - lines " begin-line "-" end-line))
    (set 'command-line (string "echo \"" text "\" > mailx -s \"" subject "\"" email))
    (exec command-line)))

;; app logic
; create bookdir if it doesn't exist
(if (not (member bookdir (directory)))
  (make-dir bookdir))

; exit if subscriptions file doesn't exist
(if (not (member subscriptions (directory))) (exit))

; read and parse the subscription file
(set 'subs (triplify (parse (read-file subscriptions))))
 
; act on each subscription
(set 'new-subs (map act-on-sub subs))
(println new-subs)

; write new subscription data

(exit)

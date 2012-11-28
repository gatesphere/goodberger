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

; pairify the subs file
(define (pairify (my-list))
  (let (l '())
    (for (x 0 (- (length my-list) 1) 2)
      (setf l (cons (list (my-list x) (my-list (+ x 1))) l)))
(reverse l)))

; process a subscription
(define (act-on-sub (sub))
  (letex (id (sub 0)) (place (sub 1))
    ; check if book exists in bookdir, if not, grab it
    (if (not (member (string id ".txt") (directory bookdir)))
      (get-book id))
  ))

;; app logic
; create bookdir if it doesn't exist
(if (not (member bookdir (directory)))
  (make-dir bookdir))

; exit if subscriptions file doesn't exist
(if (not (member subscriptions (directory))) (exit))

; read and parse the subscription file
(setf subs (pairify (map int (parse (read-file subscriptions)))))
 
; act on each subscription
(setf new-subs (map act-on-sub subs))

; write new subscription data

(exit)

#lang racket

;This file encapsulates functions for resolving
;  queue names (like mailing addresses).
;Sending stuff to those addresses happes elsewhere.

;In short:
;  Queue names look like this
;  "lustrous-possible-37_TO_tubular-plenitude-3000"
;  Where the human readable names can be calculated from the
;    computer numbers.  And vice versa.
;  Note: That process uses happy-names lib

(require happy-names)

(provide reciever-name->queue-name
         sender-name->queue-name
         numbers->send-queue-name
         numbers->receive-queue-name
         queue-name->receiver-name
         queue-name->sender-name
         name?
         my-name
         number->name
         name-valid?)

(define (name? n)
  #;(name->number n) ;Tooo slow for a contract!!
  (and (string? n)
       (= 2 (length (string-split n "-")))))

(define (name-valid? s)
  (and
   (name? s)
   (name->number s)))

(define pair?
  (list/c symbol? symbol?))


(define/contract (try-file n)
  (-> string? (or/c number? #f))
  (and
   (file-exists? n)
   (file->value n)))

(define (my-id)
  (or
   (try-file "/home/thoughtstem/remote/cb_id")
   (try-file "/Users/thoughtstem/Dev/cb_id")
   1000))

(define (my-name)
  (string-join
   (map symbol->string (number->pair (my-id)))
   "-"))

;Call when you want to be the sender,
;  and receiver-name is the known recipient
(define/contract (reciever-name->queue-name receiver-name)
  (-> name? string?)
  (define receiver-number
    (name->number receiver-name))

  (numbers->send-queue-name
     (my-id)
     receiver-number))

;Call when you want to be the reciever,
;  and the sender-name is known
(define/contract (sender-name->queue-name sender-name)
  (-> name? string?)
  (define sender-number
    (name->number sender-name))

  (numbers->send-queue-name
     sender-number
     (my-id)))
  

;;EVERYTHING BELOW THINKS OF COMPUTERS AS #S...
;;  Functions for translating between names, pairs, numbers.


(define/contract (name->pair n)
  (-> name? pair?)
  (map string->symbol (string-split n "-")))

(define/contract (pair->name p)
  (-> pair? name?)
  (string-join
   (map symbol->string p)
   "-"))

(define/contract (number->name n)
  (-> number? name?)
  (pair->name (number->pair n)))

(define/contract (name->number n)
  (-> name? (or/c number? #f))
  (pair->number (name->pair n)))

(define/contract (numbers->send-queue-name subject object)
  (-> number? number? string?)
  (string-append
   (number->name subject)
   "-"
   (number->string subject)
   "_TO_"
   (number->name object)
   "-"
   (number->string object)))

(define/contract (numbers->receive-queue-name subject object)
  (-> number? number? string?)
  (string-append
   (number->name object)
   "-"
   (number->string object)
   "_TO_"
   (number->name subject)
   "-"
   (number->string subject)))

(define (extract-name n)
  (define a (string-split n "-"))

  (string-append
    (first a)
    "-"
    (second a)))

(define/contract (queue-name->sender-name q-name)
  (-> string? (or/c name? #f))

  (define p (string-split q-name "_TO_"))

  (cond [(< (length p) 2) #f]
        [else (extract-name (first p))]))

(define/contract (queue-name->receiver-name q-name)
  (-> string? (or/c name? #f))

  (define p (string-split q-name "_TO_"))

  (cond [(< (length p) 2 ) #f]
        [else (extract-name (second p))]))





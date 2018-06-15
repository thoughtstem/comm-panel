#lang racket

(require aws/keys
         aws/sqs
         aws/util)


(require "computer-queues.rkt")

;At this point, we'll only provide the safe stuff.
;   We'll hide the stuff people could use to troll.
;   That's enough security for me, for now.  Can add more
;   later at the IAM level.
(provide list-my-receive-queues
         list-my-send-queues
         receive-string-from
         send-string-to
         flush-my-receive-queues
         flush-my-send-queues
         
         name-valid?)


;Sending and receiving seems to be working.
;  Listing queues seems to be working.

;THIS LEVEL:
;  TODO: Flushing messages on a queue (my receive queues only...?)
;  TODO: Better AWS key management.  Better IAM profiles.

;NEXT LEVEL UP:
;  TODO: "Listen" for a message on all receive queues.
;         Execute a given function for each (first) message received...
;         Example: Teacher collects work from all students
;  TODO: "Broadcast" a message on all send queues.
;        Execute a function as messages are received
;        Example: Teacher passes out an assignment
;  TODO: Build an API that solves the actual problem: sending during class.
;

;NEXT NEXT LEVEL UP:
;  TODO: Build a widget that opens files...  Magic loader integration...
;         Can maybe wait until a future iteration?  Can test without.
;         Just string sending should work for the py-fizz stuff...



;Seems to be working??
(module+ test
  (require rackunit)
  
  (_enqueue (numbers->send-queue-name 10000 10001)
            "Hello world")

  (check-equal? 1
                (length (list-receive-queues-for (number->name 10001))))

  (check-equal? 1
                (length (list-send-queues-for (number->name 10000))))

  (check-equal? 0
                (length (list-receive-queues-for (number->name 10000))))

  (check-equal? 0
                (length (list-send-queues-for (number->name 10001))))
  
  (thread
   (λ()
     (sleep 1)
     (define m
       (_dequeue (numbers->send-queue-name 10000 10001)))

     (check-equal?
      "Hello world"
      (message-body m)))))


(define (extract-queue-name x)
  (last (string-split x "/")))

(define (filter-queues pred?)
  (map
   extract-queue-name
   (filter (λ(x)
             (define q-name (extract-queue-name x))
             (pred? q-name))
           (list-queues))))

(define (same-as? f s)
  (λ(q-name)
     (equal?
      s
      (f q-name))))

(define (list-my-receive-queues)
  (list-receive-queues-for (my-name)))

(define (list-my-send-queues)
  (list-send-queues-for (my-name)))

(define (list-send-queues-for other)
  (filter-queues
   (same-as? queue-name->sender-name
             other)))

(define (list-receive-queues-for other)
  (filter-queues
   (same-as? queue-name->receiver-name
             other)))

(define (send-string-to receiver-name s)
  (define q-name
    (reciever-name->queue-name receiver-name))

  (and (not (= 0 (string-length s)))
       (_enqueue q-name s)))


(define (receive-string-from sender-name)
  (define q-name
    (sender-name->queue-name sender-name))
  
  (_dequeue q-name))


(define (flush-my-send-queues)
  (map _flush (list-my-send-queues)))

(define (flush-my-receive-queues)
  (map _flush (list-my-send-queues)))

(define (_flush q-name)
  (define m (_dequeue q-name))
  (and m (_flush q-name)))

;Just for testing.  Should not be provided upward.
(define (_dequeue q-name)
  (define q
    (create-queue q-name))

  (with-handlers ([identity (thunk* #f)]) ;Return #f if no message received
    (define m (receive-message q))
    (delete-message q (message-receipt-handle m))

    (message-body m)))

(define (_enqueue q-name s)
  (define q
    (create-queue q-name))

  (send-message q s 0))






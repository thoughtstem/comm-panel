#lang racket

(provide listen
         broadcast)

(require "computer-queues.rkt")
(require "string-sending.rkt")

;This layer builds a "listen" and "broadcast" mechanism on top
; of the ability to send/receive strings on queues.


(define t #f) ;Global.  Want to makes sure there's only one ever.
;Singleton..

; "Listen" for a message on all receive queues.
;         Example: Teacher collects work from all students
;         Only gets one per queue
;         Clears queue first
(define (listen (on-each identity))

  (flush-my-receive-queues)

  (define ret '())

  (and t (kill-thread t)) ;Make sure only 1

  (set! t
    (thread
     (thunk*
      (for ([count (range 0 60)])
               (displayln (string-append "Listening " (number->string count)))
               (define current-list (list-my-receive-queues))
               (define senders (map queue-name->sender-name current-list))
               (define messages (filter identity (map receive-string-from senders)))

               (for ([m messages])
                 (on-each m))
               
               (set! ret (append ret messages))
    
               (sleep 1)))))



  (λ()
    (kill-thread t)

    ret))



;  TODO: "Broadcast" a message on all send queues.
;        Example: Teacher passes out an assignment
;        Only gets one per queue
;        Clears queue first
(define (broadcast message)
  ;Only send one per queue?
  ;Clear queue first?
  (flush-my-send-queues)

  (define sent-to-already '())

  (and t (kill-thread t)) ;Make sure only 1

  (set! t
        (thread
         (thunk*
          (for ([count (range 0 60)])
            (displayln (string-append "Broadcasting " (number->string count)))
            (define current-list (list-my-send-queues))
            (define receivers (map queue-name->receiver-name current-list))

            (define new-receivers
              (filter (λ(r) (not (member r sent-to-already)))
                      receivers))
        
            (for ([r new-receivers])
              (send-string-to r message)
              (set! sent-to-already
                    (cons r sent-to-already)))

            (sleep 1)))))

  (λ()
    (kill-thread t)))


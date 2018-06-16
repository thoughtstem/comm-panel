#lang racket

(provide comm-panel)

;Example usage...
(module+ test
  (define keys (file->value "keys"))

  (public-key  (first keys))
  (private-key (second keys))

  (define frame (new frame%
                     [label "Communication Panel"]
                     [height 500]
                     [width 300]))

  (define hidden (new frame%
                      [label "Hidden"]))
  
  (define cp (comm-panel frame))
  (send cp reparent hidden)

  (define listen-button
    (new button% [parent frame]
         [label "toggle"]
         [callback (lambda (button event)
                     (send cp reparent frame)
                     )]))

  (send frame show #t))


;TODOs:
;  Clear out old queues every day
;  Broadcast from slideshow 
;  Add to magic loader...
;  Figure out aws key management

(require "broadcast-listen.rkt"
         "string-sending.rkt"
         "computer-queues.rkt")
(require aws/keys)
(require racket/gui)
(require wxme)






(define (comm-panel (parent (new frame%
                                 [label "Hidden"])))
  
  (define frame (new vertical-panel%
                     #;[label "Communication Panel"]
                     [parent parent]
                     #;[height 500]
                     #;[width 300]))

  ;HEADER INFO!!!
  (define msg (new message% [parent frame]
                   [label (string-append
                           "My computer name:\n" (my-name))
                          ]))



  ;;EDITOR!!!
  (define editor-canvas (new editor-canvas%
                             [min-height 300]
                             (parent frame)
                             (label "Editor Canvas")))
  (define text (new text%))
  (send text insert "")
  (send editor-canvas set-editor text)
  (send text set-max-undo-history 100)

  ;A hidden editor for buffering incoming stuff
  (define hidden-frame (new frame%
                                 [label "Hidden"]))
  
  (define hidden-editor-canvas (new editor-canvas%
                                      (parent hidden-frame)
                                      [min-height 0]
                                      #;(shown #f)
                                      (label "Editor Canvas")))
    (define hidden-text (new text%))
    (send hidden-text insert "")
    (send hidden-editor-canvas set-editor hidden-text)
    (send hidden-text set-max-undo-history 100)



  

  (define mb (new menu-bar% [parent parent]))
  (define m-edit (new menu% [label "Edit"] [parent mb]))
  (define m-font (new menu% [label "Font"] [parent mb]))
  (append-editor-operation-menu-items m-edit #f)
  (append-editor-font-menu-items m-font)
  


  (define (editor-contents)
    (define e (send editor-canvas get-editor))
    (define s (send e get-text))
    
    (send e save-file (make-temporary-file))
    (define f (send e get-filename))

    (file->string f))


  
  (define (append-editor-contents t)
    (define e (send editor-canvas get-editor))
    (define s (send e get-text))
    
    (define temp (make-temporary-file))

    (with-output-to-file temp #:exists 'replace
      (lambda ()
        (printf (or t ""))))

    
    (send hidden-text load-file temp)
    (send hidden-text copy-self-to e))


  ;LISTENER BUTTON!!!

  (define listener #f)

  (define (start-listening)
    (send listen-button set-label "Stop Listening")
    (send broadcaster-button enable #f)
    (set! listener (listen append-editor-contents)))

  (define (stop-listening)
    #;(displayln (listener))
    (listener) ;;Stops the listener
    (send broadcaster-button enable #t)
    (send listen-button set-label "Start listening")
    (set! listener #f))

  (define listen-button
    (new button% [parent frame]
         [label "Start listening"]
         [callback (lambda (button event)
                     (if (not listener)
                         (start-listening)
                         (stop-listening)))]))



  ;BROADCASTER BUTTON!!

  (define broadcaster #f)

  (define (start-broadcasting)
    #;(displayln (string-append
                "Broadcasting..."
                (editor-contents)))
    (send listen-button enable #f)
    (send broadcaster-button set-label "Stop broadcasting")
    (set! broadcaster (broadcast
                       (editor-contents))))

  (define (stop-broadcasting)
    (broadcaster) ;stops the broadcaster...
    (send listen-button enable #t)
    (send broadcaster-button set-label "Start broadcasting")
    (set! broadcaster #f))

  (define broadcaster-button
    (new button% [parent frame]
         [label "Start broadcasting"]
         [callback (lambda (button event)
                     (if (not broadcaster)
                         (start-broadcasting)
                         (stop-broadcasting)))]))


  ;SENDER BUTTON!!

  (define t #f)

  (define (ensure-name-valid a b)
    (and t (kill-thread t))

    (set! t
          (thread
           (λ()
             (define valid (name-valid? (remote-name)))

             (send sender-button enable valid)
             (send receive-button enable valid)))))

  (define (remote-name)
    (send remote-name-field get-value))

  (define remote-name-field (new text-field%
                                 [callback ensure-name-valid]
                                 (label "Other computer name")
                                 (parent frame)
                                 (init-value "")))




  (define sender-button
    (new button% [parent frame]
         [label "Send to"]
         [enabled #f]
         [callback (lambda (button event)
                     (send-string-to
                      (remote-name)
                      (editor-contents)))]))

  (define receive-button
    (new button% [parent frame]
         [label "Receive from"]
         [enabled #f]
         [callback (lambda (button event)
                     (append-editor-contents
                      (receive-string-from
                       (remote-name))))]))

  frame
  )






#lang typed/racket

(require racket/tcp
         typed/racket/class
         typed/racket/async-channel
         typed/openssl
         "irc-message.rkt")

(provide (all-defined-out)) 

; A full rewrite of my old irc library
; a lot of code sharing will happen but, overall the goal here is to
; make the code more modular,  but also get rid of some silly issues
; that were caused by seperating the connection code into a different
; module (we still can't figure out why. Something type systems are weird



(define-type IRC-Handler (IRC-Message Output-Port IRC-Messages-Channel String -> Void))
(define-type IRC-Handler-HashTable (HashTable String IRC-Handler))
(define-type IRC-Messages-Channel (Async-Channelof IRC-Message))
(define-type Caps Symbol) ; temporarily just an alias for symbol
(define-type Semas (HashTable Symbol Semaphore))

(define-type IRC% (Class
    (init
     (host String)
     (port Positive-Integer)
     (nick String)
     (user String)

     (ssl Boolean #:optional)
     (connected Boolean #:optional)
     (realname String #:optional)

     (defaultmode Integer #:optional)
     (wanted-caps (Option (Listof Caps)) #:optional)
     (sasl Boolean #:optional)
     (sasl-username (Option String) #:optional)
     (sasl-password (Option String) #:optional)
     (sasl-method (U 'PLAIN) #:optional)
          (handlers IRC-Handler-HashTable #:optional)
     (ready-sema Semaphore #:optional))
    
    (field
     (nick String)
     (user String)
     (realname String)
     (handlers IRC-Handler-HashTable)
     (ready-sema Semaphore)
     (defaultmode Integer)
     (wanted-caps (Option (Listof Caps)))
     (sasl Boolean)
     (sasl-username (Option String))
     (sasl-password (Option String))
     (sasl-method (U 'PLAIN))
     (connected Boolean)
     (host String)
     (in (Option Input-Port))
     (out (Option Output-Port))
     (port Positive-Integer)
     (ssl Boolean))
     [connected? (-> Boolean)]
;    (hosepipe! (-> (Async-Channelof IRC-Messages-Channel)))
;    (raw (-> String (Listof String) Void))
;    (msg (String String -> Void))
;    (join (String -> Void))
;    (part (String String -> Void))
;    (notice (String String -> Void))
;    (ready? (-> Semaphore))
;    (set-nick (String -> Void))
    ;    (quit (String -> Void))
    [connect (-> Boolean)]
    ))

(define-type IRC (Instance IRC%))

(: irc% IRC%)
(define irc%
  (class object%
    (init-field
     ;;;w/o defaults
     ; connection related vars 
     [ host : String ]
     [ port : Positive-Integer ]
     ; irc related vars
     [ nick : String ]
     [ user : String ]
     ; other
     ;;; defaults
     ; connection
     [ ssl : Boolean #f]
     [ connected : Boolean #f]
     ; IRC related vars
     [realname : String "irc%"]
     [defaultmode : Integer 0]
     [ wanted-caps : (Option (Listof Caps)) #f ]
     [ sasl : Boolean #f ]
     [ sasl-username : (Option String) #f ]
     [ sasl-password : (Option String) #f ]
     [ sasl-method : (U 'PLAIN) 'PLAIN ]
     ; other
     [handlers : IRC-Handler-HashTable (make-hash)]
     [ready-sema : Semaphore (make-semaphore)]
     )
    ;
   (field
     [ in : (Option Input-Port) #f]
     [ out : (Option Output-Port) #f])
    (define in-ch : (Async-Channelof Any) (make-async-channel))
    (define hthread : (Option Thread) #f) 
    ; reports if the socket is currently connected.

    (: connected? (-> Boolean))
    (define/public (connected?) : Boolean
      connected)
    
    (: _connect : Boolean String Positive-Integer -> (Values Input-Port Output-Port))
    (define/private (_connect [ssl : Boolean]
                             [ host : String ]
                             [ port : Positive-Integer]) : (Values Input-Port Output-Port)
      (if ssl
          (ssl-connect host port)
          (tcp-connect host port)))

    (define/public (connect) : Boolean ; returns true... or an exception :3
      (cond
        [connected #f] ; return false if we're already connected
        [else
          (set!-values (in out) (_connect ssl host port))
          (set! hthread (spawnHandlerThread))
          #t]))
    
    ; thread management functions
    
    (define/private (spawnHandlerThread) : Thread
      (thread
       (lambda ()
         (let run ()
           (define ev : (Evtof Input-Port) (sync (assert in)))
           (cond
             [(eof-object? (peek-char (assert in))) (set! connected #f)]
             [else
              (define line : (U EOF String)
                (cond
                  [(port-closed? (assert in))
                   (set! connected #f)
                   eof]
                  [else
                   (set! connected #t)
                   (read-line (assert in))
                   ]))
              (cond
                [(eof-object? line) #f]
                [else
                 (lambda ([ out : Output-Port ]
                          [ in-ch : IRC-Messages-Channel]
                          [ line : String])
                   (define msg (new irc-message% [msg line]))
                   (async-channel-put (assert in-ch) msg)
                   (when (is-a? msg irc-message%)
                     (for ([kv (hash->list handlers)])
                       ((cdr kv) msg out in-ch (car kv))))
                   )])])
           (when connected
             (run))))))
    
    
    (super-new)))

; todo: design interfaces for handlers, capablility modules, and other modules


(define sasl-module%
  (class object%
    (init-field
     [username : (Option String)]
     [password : (Option String)]
     [method   : (U 'PLAIN) 'PLAIN])

    (define/public
      (unique?) : Boolean
      #t)
    
    (define/public
      (implements) : (Listof Symbol)
      (list 'sasl))

    (define/public
      (handler
       [ msg : IRC-Message ]
       [ out : Output-Port ]
       [ in-ch : IRC-Messages-Channel ]) : Void
       
       )

    (define/private (do-sasl [username : String ] [ password : String ] [ method : (U 'PLAIN) ]) : Void
      
      (letrec (
               [sa (lambda ([ x : String]) : Void
                           (raw "AUTHENTICATE" (list x)))]
               [authcmd : (-> Void) void])
        
        (hash-set! handlers "dosasl" (lambda ([ msg : IRC-Message]
                                              [ out : Output-Port]
                                              [ in-ch : IRC-Messages-Channel]
                                              [ key : String]) : Void
                                              (cond
                                                [(regexp-match? #rx"AUTHENTICATE \\+" (send msg raw))
                                                 (semaphore-post (hash-ref semas 'sasl-sema))
                                                 ]
                                                [(string=? (send msg verb) "903") (semaphore-post (hash-ref semas 'sasl-sema))(hash-remove! handlers key)])))
        
        (sa (symbol->string method))
        (sync (hash-ref semas 'sasl-sema))
        (cond
          [(eq? method 'PLAIN)
           (sa (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 (string-append username (string #\nul) username (string #\nul) password)))))
           ])
        
        ))
    
    
    (super-new)))
  

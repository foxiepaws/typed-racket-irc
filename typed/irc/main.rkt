#lang typed/racket

(require typed/racket/class
         typed/racket/async-channel
         "irc-message.rkt")
(require/typed net/base64
               [base64-encode (Bytes -> Bytes)])
(require/typed racket/string
               [string-contains? (String String -> Boolean)])

(provide (all-defined-out))


(require racket/tcp
         typed/racket/class
         typed/racket/async-channel
         typed/openssl)

(provide (all-defined-out))

; define some helper types our irc handlers.
(define-type IRC-Handler (IRC-Message Output-Port IRC-Messages-Channel String -> Void))
(define-type IRC-Handler-HashTable (HashTable String IRC-Handler))
(define-type IRC-Messages-Channel (Async-Channelof IRC-Message))
(define-type Caps Symbol) ; temporarily just an alias for symbol
(define-type Semas (HashTable Symbol Semaphore))
; IRC types
(define-type IRC% (Class
    (init
     (host String)
     (port Positive-Integer)
     (nick String)
     (user String)
     (ssl Boolean #:optional)
     (connected Boolean #:optional)
     (realname String #:optional)
     (handlers IRC-Handler-HashTable #:optional)
     (ready-sema Semaphore #:optional)
     (defaultmode Integer #:optional)
     (wanted-caps (Option (Listof Caps)) #:optional)
     (sasl Boolean #:optional)
     (sasl-username (Option String) #:optional)
     (sasl-password (Option String) #:optional)
     (sasl-method (U 'PLAIN) #:optional))
    
    (field
     (connected Boolean)
     (in (Option Input-Port))
     (out (Option Output-Port))
     (ssl Boolean)
     (host String)
     (port Positive-Integer)
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
     )
    (connected? (-> Boolean))
    (raw (-> String (Listof String) Void))
    (hosepipe! (-> IRC-Messages-Channel))
    (msg (String String -> Void))
    (join (String -> Void))
    (part (String String -> Void))
    (notice (String String -> Void))
    (ready? (-> Semaphore))
    (set-nick (String -> Void))
    (quit (String -> Void))
    (get-conthread (-> Thread))
    ))
(define-type IRC (Instance IRC%))    

(: irc% IRC%)
(define irc%
  (class object%
    (init-field
     [ host : String ]
     [ port : Positive-Integer ]
     [ nick : String ]
     [ user : String ]
     [ ssl : Boolean #f]
     [ connected : Boolean #f]
     [realname : String "irc%"]
     [handlers : IRC-Handler-HashTable (make-hash)]
     [ready-sema : Semaphore (make-semaphore)]
     [defaultmode : Integer 0]
     [ wanted-caps : (Option (Listof Caps)) #f ]
     [ sasl : Boolean #f ]
     [ sasl-username : (Option String) #f ]
     [ sasl-password : (Option String) #f ]
     [ sasl-method : (U 'PLAIN) 'PLAIN ]
     )

    (field
     [ in : (Option Input-Port) #f]
     [ out : (Option Output-Port) #f])
    (define in-ch : IRC-Messages-Channel (make-async-channel))
    ; reports if the socket is currently connected.
    (: connected? (-> Boolean))
    (define/public (connected?) : Boolean
      connected)
    ; returns an Async-Channel of Anything that the handler can add items to
    (: hosepipe! (-> IRC-Messages-Channel))
    (define/public (hosepipe!) : IRC-Messages-Channel
      in-ch)
    
    ; sends data to the connected side of the socket
    (: raw : String (Listof String) -> Void)
    (define/public (raw [ cmd : String ]
                        [ params : (Listof String) ]) : Void
      (fprintf (cast out Output-Port) "~a ~a\r\n" cmd (string-join params)))

    ; Internal function used to connect to sockets
    (: connect : Boolean String Positive-Integer -> (Values Input-Port Output-Port))
    (define/private (connect [ssl : Boolean]
                             [ host : String ]
                             [ port : Positive-Integer]) : (Values Input-Port Output-Port)
      (if ssl
          (ssl-connect host port)
          (tcp-connect host port)))


   (define/public (msg [ target : String ] [ msg : String ]) : Void
      (raw "PRIVMSG" (list target (string-append ":" msg))))

    
    (define/public (join [ channel : String ]) : Void
      (raw "JOIN" (list channel)))

    
    (define/public (part [ channel : String ] [arg : String]) : Void
        (raw "PART" (list channel (string-append ":" arg))))

    
    (define/public (notice [ target : String ] [ msg : String ] ) : Void
      (raw "NOTICE" (list target (string-append ":" msg))))

   
    (define/public (ready?) : Semaphore
      ready-sema)

    
    (define/public (set-nick [ n : String ]) : Void 
      (raw "NICK" (list n)))

    
    (define/private (set-user-info [ u : String ] [ m : Integer ] [ rn : String] ) : Void
      (raw "USER" (list u (format "~a" m) "*" (string-append ":" rn))))

    
    (define/public (quit [msg : String]) : Void
      (raw "QUIT" (list (string-append ":" msg))))
    
    (define #:forall(a) (allin [inlist : (Listof a)] [items : (Listof a)]) : (Listof a)
      (letrec
          ([hasitem? (lambda ([ inlist : (Listof a) ] [ rest : a ]) : Boolean
                       (not (boolean? (member rest inlist))))])
        (filter (lambda ([x : a]) (hasitem? inlist x)) items)))

    (define server-caps : (Option (Listof Symbol)) #f)
    (define/private (pre-cap) : Void
      (hash-set! handlers "docap" (lambda ([ msg : IRC-Message]
                                         [ out : Output-Port]
                                         [ in-ch : IRC-Messages-Channel]
                                         [ key : String]) : Void
                                    
                                    (cond
                                      [(send msg iscap?)
                                       (cond
                                         [(string=? (list-ref (send msg args) 1) "LS")
                                          (set! server-caps (map string->symbol (string-split (list-ref (send msg args) 2) " ")))
                                          (semaphore-post (hash-ref semas 'cap-sema))
                                          ]
                                         [(string=? (list-ref (send msg args) 1) "ACK")
                                          (semaphore-post (hash-ref semas 'cap-sema))
                                          ])])))
      (raw "CAP" (list "LS" "302")))
    (define/private (req-caps [caps : (Listof Caps)]) : Void
      (raw "CAP" (list "REQ" (string-append ":" (string-join (map symbol->string caps) " ")))))
    (define/private (cap-end) : Void
      (raw "CAP" (list "END")))

   
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


    
    (super-new)
    (set!-values (in out) (connect ssl host port))
    
    (file-stream-buffer-mode (assert out) 'line)

    ; This thread is how the socket is processed 
    (: conthread Thread)
    (define conthread
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
                 (define msg (new irc-message% [msg line]))
                 (async-channel-put (assert in-ch) msg)
                 (when (is-a? msg irc-message%)
                   (for ([kv (hash->list handlers)])
                     ((cdr kv) msg (assert out) in-ch (car kv))))])])
           (when connected
             (run))))))
    


    (define/public (get-conthread) : Thread
      conthread)
    
    ; enable sasl?
    (cond
      [(and sasl (not ssl)) (raise "cowardly failing because SASL is enabled WITHOUT SSL")]
      [(and sasl (boolean? wanted-caps)) (set! wanted-caps (list 'sasl))]
      ; in case user added sasl to wanted caps on their own.
      [(and sasl (and (list? wanted-caps) (not (boolean? (member 'sasl (assert wanted-caps)))))) (set! wanted-caps (append (assert wanted-caps) (list 'sasl)))]
      [else (void)])
    
    (define semas : Semas (make-hash (list 
                                      (cons 'cap-sema (make-semaphore))
                                      (cons 'sasl-sema (make-semaphore)))))

    (hash-set! handlers "ready" (lambda ([ msg : IRC-Message]
                                         [ out : Output-Port]
                                         [ in-ch : IRC-Messages-Channel]
                                         [ key : String])
                                  (cond
                                    [(string=? (send msg verb) "001") (semaphore-post ready-sema)(hash-remove! handlers key)])))

    (hash-set! handlers "ping" (lambda  ([ msg : IRC-Message]
                                         [ out : Output-Port]
                                         [ in-ch : IRC-Messages-Channel]
                                         [ key : String])
                                 (cond
                                   [(send msg isping?) (raw "PONG" (send msg args))])
                                 ))
    
    
    (cond
      [(not (boolean? wanted-caps))
       (pre-cap)
       (sync (hash-ref semas 'cap-sema))
       (req-caps (allin (assert wanted-caps) (assert server-caps)))
       (sync (hash-ref semas 'cap-sema))
       (cond
         [sasl
          (do-sasl (assert sasl-username) (assert sasl-password) sasl-method)
          (sync (hash-ref semas 'sasl-sema))
          (hash-remove! semas 'sasl-sema)])
       (cap-end)])
    
    (set-nick nick)
    (set-user-info user defaultmode realname)
    ))
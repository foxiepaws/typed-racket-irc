#lang typed/racket

(require typed/racket/base)

(provide (all-defined-out))

(require/typed racket/string
               [string-prefix? (String String -> Boolean)]
               [string-contains? (String String -> Boolean)])

(define-type Message-Tags (HashTable String String))
(define-type IRC-Message% (Class
     (init (msg String))
     (field (ars (Listof String))
            (msg String)
            (p? Boolean)
            (pfix String)
            (vrb String)
            (tgs Message-Tags)
            (c? Boolean))

     (args (-> (Listof String)))
     (isping? (-> Boolean))
     (prefix (-> String))
     (raw (-> String))
     (verb (-> String))
     (hastags? (-> Boolean))
     (get-tag-keys (-> (Option (Listof String))))
     (get-tag-value (String -> String))
     (tags (-> Message-Tags))
     (iscap? (-> Boolean))
     ))
(define-type IRC-Message (Instance IRC-Message%))

(: irc-message% IRC-Message%)
(define irc-message%
  (class object%
    (init-field
     [ msg : String ])
    (field [tgs : Message-Tags (make-hash)]) 
    (field [vrb : String ""])
    (field [pfix : String ""])
    (field [ars : (Listof String) (list "")])
    (field [p? : Boolean #f])
    (field [c? : Boolean #f])
    
    (: raw (-> String))
    (define/public (raw) : String
      msg)

    (: verb (-> String))
    (define/public (verb) : String
      vrb)

    (: prefix (-> String))
    (define/public (prefix) : String
      pfix)

    (: args (-> (Listof String)))
    (define/public (args) : (Listof String)
      ars)

    (: isping? (-> Boolean))
    (define/public (isping?) : Boolean
      p?)

    (: hastags? (-> Boolean))
    (define/public (hastags?) : Boolean
      (not (zero? (hash-count tgs))))
    
    (define/public (get-tag-keys) : (Option (Listof String))
      (if (hastags?)
          (hash-keys tgs)
          #f))

    (define/public (get-tag-value [k : String]) : String
      (hash-ref tgs k))

    (define/public (tags) : Message-Tags
      tgs)

    (define/public (iscap?) : Boolean
      c?)
    
    (define (removesep [str : String]) : String ; only substring if it starts with a :
      (match str
        [(pregexp "^:") (substring str 1)] 
        [_ str]))

    ; argsplit and split-tag both use similar splitting logic, so instead of repeating it, i just created this (private) function for doing the splitting tasks
    (define (split [sep : Regexp] [args : (Listof String)] #:joinhead? [ jhead : Boolean #f ] #:joiner [ jchr : String ""]) : (Listof String) ; create parameter list
      (letrec (
               [split-tail (lambda ([lst : (Listof String)]) : (Listof String) (assert (memf (lambda ([ x : String]) (regexp-match? sep (assert x string?))) lst)))]
               [split-head (lambda ( [lst : (Listof String)]) : (Listof String) (take lst (- (length lst) (length (if (split-tail lst) (split-tail lst) (list))))))]
               [removesep (lambda ([sep : Regexp] [str : String]) : String
                            (match str
                              [(regexp sep) (substring str 1)]
                              [_ str]))])
        (append 
         (if jhead
             (list (string-join (split-head args) jchr))
             (split-head args))
         (if (split-tail args) 
             (list (removesep sep (string-join (split-tail args) jchr)))
             (list)))))
    (define (split-tag [t : String]) : (Listof String)
      (match t
        [(regexp #rx"=") (split #rx"=" (map string (string->list t)) #:joinhead? #t)]
        [_ (list t "")]))
    
    (define (argsplit [args : (Listof String)]) : (Listof String)
      (if (string-contains? (string-join args " ") ":")
        (split #rx":" args #:joiner " ")
        args))
    
    (define (split-tags [t : String]) : (Listof String)
      (string-split t ";" #:trim? #f))
    
    (define (tag-replace-escapes [ts : String]) : String
      (string-replace
       (string-replace
        (string-replace
         (string-replace
          (string-replace ts "\\:" ";") "\\s" " ") "\\r" "\r") "\\n" "\n") "\\\\" "\\"))
    
    
    (define (ts->taghash [ts : String]) : Message-Tags
      (letrec (
               [list->unescaped-pair (lambda ([lst : (Listof String)]) : (Pairof String String)
                                       (cons (car lst) (tag-replace-escapes (car (cdr lst)))))])
        (make-hash (map (lambda ([ x : String ]) (list->unescaped-pair (split-tag x))) (split-tags ts)))))
    
    (super-new)
    (match (string-split msg " " #:trim? #f)
      [(list-rest "PING" a)
        (set! p? #t)
        (set! vrb "PING")
        (set! ars a)]
      [(list-rest p "CAP" a) (set! c? #t) (set! pfix (removesep p)) (set! vrb "CAP") (set! ars (argsplit a))] ; avoiding parses of capability messages.
      [(list-rest (? (lambda (x) (string-prefix? x "@")) t) p c a) (set! tgs (ts->taghash t))(set! pfix (removesep p)) (set! vrb c) (set! ars (argsplit a))]
      [(list-rest p c a)
       (set! pfix (removesep p))
       (set! vrb c)
       (set! ars (argsplit a))]
      
     )))

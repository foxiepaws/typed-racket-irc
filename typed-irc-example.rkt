#lang typed/racket


(require typed/irc
   	 typed/irc/irc-message)

(define i : IRC (new irc%
                [host "irc.anthrochat.org"]
                [port 6697]
                [nick "RachelRacket"]
                [user "irc"]
                [defaultmode 8]
                [ssl #t]
		[sasl #f]          ; to enable sasl, set this and the other sasl related fields.
		[sasl-username #f] ; set to a String containing your username 
		[sasl-password #f] ; set to a String containing your password
		))

; todo: figure out why we can't cast our Async-Channel.
(define ircmsgs (send i hosepipe!))
(void (sync (send i ready?)))
(send i join "#thezoo")
(let loop ()
  ; todo: Fix this nasty syntax crap. 
  (define msg : IRC-Message (cast (sync ircmsgs) IRC-Message)); type is Evtof Any data is always IRC-Message
  (display (send msg raw))
  (cond
    [(and (string=? (send msg verb) "PRIVMSG") (string=? (first
                                                          (string-split
                                                           (list-ref
                                                            (send msg args) 1))) "-hi" ))
     (send i msg (list-ref (send msg args) 0) "Hoi!")])  
  (loop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Simplest possible OSC example
;

;; start two osc servers
(io:osc:start-server 7009 "osc-receive-7009")
(io:osc:start-server 7019 "osc-receive-7019")

; Prints all messages to the log
(define (osc-receive-7009 timestamp address . args)
   (println 'port 7009 address '-> args))

(define (osc-receive-7019 timestamp address . args)
   (println 'port 7019 address '-> args))


;; define a sending address
(define addy1 (cons "localhost" 7009))
(define addy2 (cons "localhost" 7019))

;; Some a test message delayed by 4 seconds
(io:osc:send (+ (now) (* *second* 4)) addy "/test/msg" "Hello" 500 6.6 "World")
(io:osc:send (+ (now) (* *second* 4)) addy2 "/test/msg" "Hello" 500 6.6 "World")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Simplest possible OSC example
;

;; start osc server on port 7009
(io:osc:start-server 7009)

; Prints all messages to the log
(define (io:osc:receive timestamp address . args)
   (print address '-> args))


;; define a sending address
(define addy (cons "localhost" 7009))

;; Some a test message delayed by 4 seconds
(io:osc:send (+ (now) (* *second* 4)) addy "/test/msg" "Hello" 500 6.6 "World")

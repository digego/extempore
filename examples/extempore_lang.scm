;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  A basic introduction to the Extempore Language and Compiler
;;

;; multiple a * 5
;; note that type infercing works out the type 
;; of "a" and then using the inferred type
;; also works out the type of my-test-1
;; (i.e. argument type and return type)
;; 
;; integer literals default to 64 bit integers
(definec my-test-1
   (lambda (a)
      (* a 5)))

;; notice that the log view displays the type
;; of the closure we just compiled
;; [i64,i64]*
;; The square brackets define a closure type
;; The first type within the square braces is
;; the return type of the function (i64 for 64bit integer)
;; Any remaining types are function arguments 
;; in this case another i64 (for 64bit integer)
;; 
;; All closures are pointers.  Pointer types are
;; represented (as in "C") with a "*" which trails
;; the base type.
;; So an i64 pointer type would be "i64*"
;; A double pointer type would be "double*"
;; So a closure pointer type is "[...]*"

;; float literals default to doubles
(definec my-test-1f
   (lambda (a)
      (* a 5.0)))

;; Again note the closures type in the logview
;; [double,double]*


;; we can call these new closures like so
(println (my-test-1 6)) ;; 30
(println (my-test-1f 6.0)) ;; 30.0


;; you are free to recompile an existing compile
;; closures body to do something different whenever you like
;; so we can change my-test-1 to
(definec my-test-1
   (lambda (a)
      (/ a 5)))

(println (my-test-1 30)) ; 30 / 5 = 6

;; note that the closures signature is still the same
;; as it was before.  This is important because we are
;; NOT allowed to change an existing compiled closures
;; type signature.
;; 
;; So we CANNOT do this

;(definec my-test-1
;   (lambda (a)
;      (/ a 5.0)))

;; Just remember that you are not currently allowed to redefine an 
;; existing function to a new definition that requres a different type signature.  
;; This is to protect against the situation where you have allready compiled
;; code which requires the current signature.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Because we are working with closures
;; we can close over free variables
;; in this example we close over power
;; to maintain state between calls
;;
;; increment power each call
(definec my-test-2
   (let ((power 0))
      (lambda (x)
         (set! power (+ power 1)) ;; set! for closure mutation as per scheme
         (* x power))))

;; each modifies state
(println (my-test-2 2)) ;; should = 2
(println (my-test-2 2)) ;; should = 4
(println (my-test-2 2)) ;; etc

               
;; Closures can of course return closures.
;; notice the type signature of this function
;; as printed in the logview "[[i64,i64]*]*"
;; being a closure that returns a closure
(definec my-test-3
  (lambda ()
    (lambda (x)
      (* x 3))))


;; let's try to make a generic incrementor
;;
;; here we run into trouble
;; because the type inferencer cannot infer a valid type 
;; for i or inc and therefore also cannot infer
;; a type for my-inc-maker!
;;
;; THIS WILL CAUSE AN ERROR!

;(definec my-inc-maker
;  (lambda (i)
;    (lambda (inc)
;      (set! i (+ i inc))
;      i)))

;; This makes sense should "+" operate
;; on doubles or integers - who knows?
;; So the type inferencer complains justifiably complains
;;
;; What can we do about this ... 
;; we need to help the compiler out by proving some
;; explicit type goal posts
;;
;; We can do that by "typing" a variable.
;; Explicitly typing a variable means tagging
;; the symbol with a type separated by ":"
;;
;; Here are some examples
;; x:i64        = x is a 64 bit integer
;; y:double     = y is a double
;; z:i32*       = z is a pointer to a 32 bit integer
;; w:[i64,i64]* = w is a closure which takes an i64 and returns an i64
;;
;; Make sure there are no spaces in the expression
;;
;; Now we can explicitly type i
(definec my-inc-maker
   (lambda (i:i64)
      (lambda (inc)
         (set! i (+ i inc))
         i)))

;; this solves our problem as the compiler
;; can now use i's type to infer inc and
;; therefore my-inc-maker.


;; now we have a different problem.
;; if we call my-inc-maker we expect to be 
;; returned a closure.  But Scheme does not
;; know anything about ICR closure types and therefore
;; has no way of using the returned data.  Instead
;; it places the returned pointer (remember a closure is a pointer)
;; into a generic Scheme cptr type.
;;
;; We are free to then pass that cptr back into another
;; compiled function as an argument.  
;; 
;; So let's build a function that excepts a closure returned from 
;; my-inc-maker as an argument, as well as a suitable operand, and 
;; apply the closure.

;; f is our incoming closure
;; and x is our operand
;; THIS WILL CAUSE AN ERROR

;(definec my-inc-maker-wrappert
;   (lambda (f x) ; f and x are args
;      (f x)))

;; oops can't resolve the type of "f"
;; fair enough really.
;; even if we give a type for "x"
;; we still can't tell what "f"'s
;; return type should be?
;; This also causes an error!

;(definec my-inc-maker-wrappert
;   (lambda (f x:i64) ; f and x are args
;      (f x)))

;; so we need to type f properly
(definec my-inc-maker-wrapper
   (lambda (f:[i64,i64]* x)      
      (f x)))

;; ok so now we can call my-inc-maker
;; which will return a closure
;; which scheme stores as a generic cptr
(define myf (my-inc-maker 0))

;; and we can call my-in-maker-wrapper
;; to appy myf
(println (my-inc-maker-wrapper myf 1)) ; 1
(println (my-inc-maker-wrapper myf 1)) ; 2
(println (my-inc-maker-wrapper myf 1)) ; 3 etc..

;; of course the wrapper is only required if you 
;; need interaction with the scheme world.
;; otherwise you just call my-inc-maker directly

;; this avoids the wrapper completely
(definec my-inc-test
   (let ((f (my-inc-maker 0)))
      (lambda ()
         (f 1))))

(println (my-inc-test)) ; 1
(println (my-inc-test)) ; 2
(println (my-inc-test)) ; 3

;; hopefully you're getting the idea.
;; note that once we've compiled something
;; we can then use it any of our new
;; function definitions.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Closures can be recursive
;;

(definec my-test-4
  (lambda (a)
    (if (< a 1)
	(printf "done\n")
	(begin (printf "a: %lld\n" a)
	       (my-test-4 (- a 1))))))

(my-test-4 7)
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a simple tuple example
;; 
;; tuple types are represented as <type,type,type>*
;;

;; make and return a simple tuple
(definec my-test-6
  (lambda ()
    (make-tuple i64 double i32)))

;; logview shows [<i64,double,i32>*]*
;; i.e. a closure that takes no arguments
;; and returns the tuple <i64,double,i32>*
      

;; here's another tuple example
;; note that my-test-7's return type is inferred
;; by the tuple-reference index 
;; (i.e. i64 being tuple index 0)
(definec my-test-7 
  (lambda ()
    (let ((a (make-tuple i64 float)) ; type <i64,float>
	  (b 37)
	  (c 6.4))
      (tuple-set! a 0 b) ;; set i64 to 64
      (tuple-set! a 1 c) ;; set float to 6.4
      (tuple-ref a 0)))) ;; return first element which is i64

;; should be 64 as we return the 
;; first element of the tuple 
(println (my-test-7)) ; 37


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some array code with *casting*
;; this function returns void
(definec my-test-8
   (lambda ()
      (let ((v (make-array 5 float)))
         (dotimes (i 5)
            ;; random returns double so "truncate" to float
            ;; which is what v expects
            (array-set! v i (dtof (random))))
         (dotimes (k 5)
            ;; unfortunately printf doesn't like floats
            ;; so back to double for us :(
            (printf "val: %lld::%2f\n" k (ftod (array-ref v k)))))))

(my-test-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some crazy array code with closures
;; try to figure out what this does

(definec my-test-9 
   (lambda (v:i64*)
      (let ((f (lambda (x)
                  (* (array-ref v 2) x))))
         f)))

(definec my-test-10 
   (lambda (v:[i64,i64]**)
      (let ((ff (aref v 0))) ; aref alias for array-ref
         (ff 5))))

(definec my-test-11
   (lambda ()
      (let ((v (make-array 5 [i64,i64]*)) ;; make an array of closures!
            (vv (make-array 5 i64)))
         (array-set! vv 2 3)
         (aset! v 0 (my-test-9 vv)) ;; aset! alias for array-set!
         (my-test-10 v))))

;; try to guess the answer before you call this!!
(println (my-test-11))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some conditionals

(definec my-test-12
   (lambda (x:i64 y)
      (if (> x y)
          x
          y)))

(println (my-test-12 12 13))
(println (my-test-12 13 12))

;; returns boolean true
(definec my-test-13
   (lambda (x:i64)
      (cond ((= x 1) (printf "A\n"))
            ((= x 2) (printf "B\n"))
            ((= x 3) (printf "C\n"))
            ((= x 4) (printf "D\n"))
            (else (printf "E\n")))
      #t))
            
(my-test-13 1)
(my-test-13 3)
(my-test-13 100)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; making a linear envelop generator
;; for signal processing and alike

(definec envelope-segments
   (lambda (points:double* num-of-points:i64)
      (let ((lines (make-array num-of-points [double,double]*)))         
         (dotimes (k num-of-points)
             (let* ((idx (* k 2))
                    (x1 (aref points (+ idx 0)))
                    (y1 (aref points (+ idx 1)))
                    (x2 (aref points (+ idx 2)))
                    (y2 (aref points (+ idx 3)))
                    (m (if (= 0.0 (- x2 x1)) 0.0 (/ (- y2 y1) (- x2 x1))))
                    (c (- y2 (* m x2)))
                    (l (lambda (time) (+ (* m time) c))))
                (aset! lines k l)))
         lines)))

(definec make-envelope
   (lambda (points:double* num-of-points)
      (let ((klines:[double,double]** (envelope-segments points num-of-points))
            (line-length num-of-points))
         (lambda (time)
            (let ((res -1.0))
               (dotimes (k num-of-points)
                  (let ((line (aref klines k))
                        (time-point (aref points (* k 2))))
                     (if (or (= time time-point)
                             (< time-point time))
                         (set! res (line time)))))
               res)))))

;; make a convenience wrapper 
(definec env-wrap
   (let* ((points 3)
          (data (make-array (* points 2) double)))
      (aset! data 0 0.0) ;; point data
      (aset! data 1 0.0)      
      (aset! data 2 2.0)
      (aset! data 3 1.0)      
      (aset! data 4 4.0)
      (aset! data 5 0.0)
      (let ((f (make-envelope data points)))
         (lambda (time:double)
            (f time)))))

(println (env-wrap 0.0)) ;; time 0.0 should give us 0.0
(println (env-wrap 1.0)) ;; time 1.0 should give us 0.5
(println (env-wrap 2.0)) ;; time 2.0 should be 1.0
(println (env-wrap 2.5)) ;; going back down 0.75
(println (env-wrap 4.0)) ;; to zero



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; direct access to a closures environment
;;
;; it is possible to directly access a closures
;; environment in order to read or modify data
;; at runtime.
;;
;; You do this using a dot operator
;; To access an environment slot you use
;; closure.slot:type
;; So for example
;; (f.a:i32)
;; would return the 32bit integer symbol 'a'
;; from the closure 'f'
;; 
;; To set an environment slot you just
;; add a value of the correct type
;; for example
;; (f.a:i32 565)
;; would set 'a' in 'f' to 565
;; 
;; let's create a closure that capture's 'a'


(definec my-test14
  (let ((a:i32 6))
    (lambda ()
      (printf "a:%d\n" a)
      a)))

;; calling my-test14 prints the value of a
;; and returns the bind to a (i.e. 6)
(my-test14) ;  6


;; now let's create a new function
;; that calls my-test14 twice
;; once normally
;; then we directly set the closures 'a' binding
;; then call again
;; 
(definec my-test15
  (lambda (x:i32)
    (my-test14)
    (my-test14.a:i32 x)
    (my-test14)))

;; should print a:6 and a:9
(my-test15 9) ; 9

;; now what happens if we pass 101
;; should print a:9 and a:101
(my-test15 101) ; 101



;; of course this works just as well for
;; non-global closures
(definec my-test16
  (lambda (a:i32)
    (let ((f (lambda ()
	       (* 3 a))))
      f)))

(definec my-test17
  (lambda ()
    (let ((f (my-test16 5)))
      (f.a:i32 7)
      (f))))

(println (my-test17)) ;; 21



;; and you can get and set closures also!
(definec my-test18
  (let ((f (lambda (x:i64) x)))
    (lambda ()
      (lambda (z)
	(f z)))))


(definec my-test19
  (lambda ()
    (let ((t1 (my-test18))
	  (t2 (my-test18)))
      ;; identity of 5
      (printf "%lld:%lld\n" (t1 5) (t2 5))
      (t1.f:[i64,i64]* (lambda (x:i64) (* x x)))
      ;; square of 5
      (printf "%lld:%lld\n" (t1 5) (t2 5))      
      ;; cube of 5 
      (my-test18.f:[i64,i64]* (lambda (y:i64) (* y y y)))
      (printf "%lld:%lld\n" (t1 5) (t2 5)))))


(my-test19) ;; 5:5 > 25:25 > 125:125



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; named types

;; we can name our own types using bind-type
(bind-type mytype <i64,i64>*)

;; which we can then use in place
(definec my-test20
  (lambda (a:mytype)
    (tref a 0)))


;; named types support a single level of recursion
;; this is very useful for building data structures
;; linked lists for example!
(bind-type i64list <i64,i64list>*)

(definec cons-i64
  (lambda (a:i64 b:i64list)
    (let ((pair (make-tuple i64 i64list)))
      (tset! pair 0 a)
      (tset! pair 1 b)
      pair)))
          
(definec car-i64
  (lambda (a:i64list)
    (tref a 0)))

(definec cdr-i64
  (lambda (a:i64list)
    (tref a 1)))

;; print all i64's in list
(definec my-test25
  (lambda (a:i64list)
    (if (null? a)
	(begin (printf "done\n") 1)
	(begin (printf "%lld\n" (car-i64 a))
	       (my-test25 (cdr-i64 a))))))

;; build a list (using cons) and then call my-test25
(definec my-test26
  (lambda ()
    (let ((my-list (cons-i64 1 (cons-i64 2 (cons-i64 3 null)))))
      (my-test25 my-list))))

(my-test26) ;; 1 > 2 > 3 > done


(print)
(println 'finished)
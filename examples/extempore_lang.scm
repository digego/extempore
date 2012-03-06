;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  A basic introduction to the Extempore Language and Compiler
;;
;;  These examples are specific to Extempore lang
;;  for versions of LLVM v3.0+
;;
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
;; So a pointer to a 64 bit integer would be "i64*"
;; A double pointer type would be "double*"
;; So a closure pointer type is "[...]*"

;; float literals default to doubles
(definec my-test-1f
   (lambda (a)
      (* a 5.0)))

;; Again note the closures type in the logview
;; [double,double]*
;; a closure that returns a double and
;; taks a double as it's only argument


;; we can call these new closures like so
;; making sure we pass an integer for my-test-1
(println (my-test-1 6)) ;; 30
;; and a real number for my-test-1f
(println (my-test-1f 6.0)) ;; 30.0


;; you are free to recompile an existing closure
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
;; we can "close" over free variables
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
;; notice the type signature of this closure
;; as printed in the logview "[[i64,i64]*]*"
;; being a closure that returns a closure
;; the outer closure takes no arguments
;; and the return closure takes an i64 argument
(definec my-test-3
  (lambda ()
    (lambda (x)
      (* x 3))))


;; let's try to make a generic incrementor
;;
;; here we run into trouble
;; because the type inferencer cannot infer a valid type 
;; for i or inc because there are no numberic literals
;; to help in the validation process

;; THIS WOULD CAUSE AN ERROR!
;(definec my-inc-maker
;  (lambda (i)
;    (lambda (inc)
;      (set! i (+ i inc))
;      i)))

;; This makes sense - should "+" operate
;; on doubles or integers - who knows?
;; So the type inferencer justifiably complains
;;
;; What can we do about this ... 
;; we need to help the compiler out by providing
;; some explicit type information
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
;;                (remember that closures are ALWAYS pointers it is not
;;                 valid to have a closure type which is NOT a pointer)

;;
;; With this information in mind we can
;; fix the incrementor by explicitly typing 'i'
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
;; returned a closure.  However Scheme does not
;; know anything about Extempore Lang closure types and therefore
;; has no way of using the returned data.

;; Instead it places the returned pointer
;; (remember a closure is a pointer)
;; into a generic Scheme cptr type.
;; All pointer types moving from Extempore Lang -> Scheme
;; are converted into generic Scheme cptr types.  Scheme
;; knows that the type is a cptr but has no further information.
;;
;;
;; We are free to then pass that cptr back into another
;; compiled Extempore Lang function as an argument. When moving
;; from Scheme -> Extempore Lang the generic Scheme cptr is
;; automatically converted back into the explicit pointer type
;; required by Extempore Lang.
;; IMPORTANT!: This conversion is automatic and UNCHECKED so
;; it is your responsibility to ensure that Scheme cptr's point
;; to appropriate data (i.e. appropriate for the function be
;; called in Extempore Lang).

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
    (alloc <i64,double,i32>)))


;; logview shows [<i64,double,i32>*]*
;; i.e. a closure that takes no arguments
;; and returns the tuple <i64,double,i32>*
      

;; here's another tuple example
;; note that my-test-7's return type is inferred
;; by the tuple-reference index 
;; (i.e. i64 being tuple index 0)
(definec my-test-7 
  (lambda ()
    (let ((a (alloc <i64,double>)) ; returns pointer to type <i64,double>
	  (b 37)
	  (c 6.4))
      (tuple-set! a 0 b) ;; set i64 to 64
      (tset! a 1 c) ;; set double to 6.4 - tset! is an alias for tuple-set!
      (printf "tuple:1 %lld::%f\n" (tuple-ref a 0) (tref a 1))
      ;; we can fill a tuple in a single call by using tfill!
      (tfill! a 77 77.7)
      (printf "tuple:2 %lld::%f\n" (tuple-ref a 0) (tuple-ref a 1))
      (tuple-ref a 0)))) ;; return first element which is i64

;; should be 64 as we return the 
;; first element of the tuple 
(println (my-test-7)) ; 77


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some array code with *casting*
;; this function returns void
(definec my-test-8
   (lambda ()
      (let ((v1 (alloc |5,float|))
	    (v2 (alloc |5,float|))
	    (i 0)
	    (k 0))
         (dotimes (i 5)
            ;; random returns double so "truncate" to float
            ;; which is what v expects
            (array-set! v1 i (dtof (random))))
	 ;; we can use the afill! function to fill an array
	 (afill! v2 1.1 2.2 3.3 4.4 5.5)
         (dotimes (k 5)
            ;; unfortunately printf doesn't like floats
            ;; so back to double for us :(
            (printf "val: %lld::%f::%f\n" k
		    (ftod (array-ref v1 k))
		    (ftod (aref v2 k)))))))

(my-test-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some crazy array code with
;; closures and arrays
;; try to figure out what this all does
;;
;; this example uses the array type
;; the pretty print for this type is
;; |num,type| num elements of type
;; |5,i64| is an array of 5 x i64
;;
;; An array is not a pointer type
;; i.e. |5,i64| cannot be bitcast to i64*
;;
;; However an array can be a pointer
;; i.e. |5,i64|* can be bitcast to i64*
;; i.e. |5,i64|** to i64** etc..
;;
;; make-array returns a pointer to an array
;; i.e. (make-array 5 i64) returns type |5,i64|*
;;
;; aref (array-ref) and aset! (array-set!)
;; can operate with either pointers to arrays or
;; standard pointers.
;;
;; in other words aref and aset! are happy
;; to work with either i64* or |5,i64|*

(definec my-test-9
   (lambda (v:|5,i64|*)
      (let ((f (lambda (x)
                  (* (array-ref v 2) x))))
         f)))

(definec my-test-10
  (lambda (v:|5,[i64,i64]*|*)
    (let ((ff (aref v 0))) ; aref alias for array-ref
      (ff 5))))


(definec my-test-11
   (lambda ()
      (let ((v (alloc |5,[i64,i64]*|)) ;; make an array of closures!
            (vv (alloc |5,i64|)))
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
    (let ((lines (zone-alloc num-of-points [double,double]*))
	  (k 0))
      (dotimes (k num-of-points)
	(let* ((idx (* k 2))
	       (x1 (pointer-ref points (+ idx 0)))
	       (y1 (pointer-ref points (+ idx 1)))
	       (x2 (pointer-ref points (+ idx 2)))
	       (y2 (pointer-ref points (+ idx 3)))
	       (m (if (= 0.0 (- x2 x1)) 0.0 (/ (- y2 y1) (- x2 x1))))
	       (c (- y2 (* m x2)))
	       (l (lambda (time) (+ (* m time) c))))
	  (pointer-set! lines k l)))
      lines)))


(definec make-envelope
   (lambda (points:double* num-of-points)
      (let ((klines:[double,double]** (envelope-segments points num-of-points))
            (line-length num-of-points))
         (lambda (time)
            (let ((res -1.0)
		  (k:i64 0))
               (dotimes (k num-of-points)
                  (let ((line (pointer-ref klines k))
                        (time-point (pointer-ref points (* k 2))))
                     (if (or (= time time-point)
                             (< time-point time))
                         (set! res (line time)))))
               res)))))


;; make a convenience wrapper 
(definec env-wrap
   (let* ((points 3)
          (data (zone-alloc (* points 2) double)))
      (pointer-set! data 0 0.0) ;; point data
      (pset! data 1 0.0)      
      (pset! data 2 2.0)
      (pset! data 3 1.0)      
      (pset! data 4 4.0)
      (pset! data 5 0.0)
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
(bind-type mytype <i64,i64>)

;; which we can then use in place
(definec my-test20
  (lambda (a:mytype*)
    (tref a 0)))

;; named types support recursion
(bind-type i64list <i64,i64list*>)

;; Note the use of zone-alloc to allocate
;; enough zone memory to hold an i64list
;; zone-alloc returns a pointer to the
;; type that you ask it to allocate
;; pair is type i64list* in this case.
;;
;; You are responsible for cleaning up
;; this memory at some point in the future!
;; (i.e. cleaning up the memory zone that this
;; heap allocation was made into)
(definec cons-i64
  (lambda (a:i64 b:i64list*)
    (let ((pair (zone-alloc i64list)))
      (tset! pair 0 a)
      (tset! pair 1 b)
      pair)))
          
(definec car-i64
  (lambda (a:i64list*)
    (tref a 0)))

(definec cdr-i64
  (lambda (a:i64list*)
    (tref a 1)))

;; print all i64's in list
(definec my-test25
  (lambda (a:i64list*)
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


;; it can sometimes be helpful to allocate
;; a predefined tuple type on the stack
;; you can do this using allocate
(bind-type vec3 <double,double,double>)

;; note that point is deallocated at the
;; end of the function call.  You can
;; stack allocate (stack-alloc)
;; any valid type  (i64 for example)
(definec my-test27
  (lambda ()
    (let ((point (stack-alloc vec3)))
      (tset! point 0 0.0)
      (tset! point 1 -1.0)
      (tset! point 2 1.0)
      1)))

(println (my-test27)) ;; 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; aref-ptr and tref-ptr
;;

;; aref-ptr and tref-ptr return a pointer to an element
;; just as aref and tref return elements aref-ptr and
;; tref-ptr return a pointer to those elements.

;; This allows you to do things like create an array
;; with an offset
(definec my-test28
  (lambda ()
    (let ((arr (alloc |32,i64|))
	  (arroff (aref-ptr arr 16))
	  (i 0)
	  (k 0))
      ;; load arr
      (dotimes (i 32) (aset! arr i i))
      (dotimes (k 16)
	(printf "index: %lld\tarr: %lld\tarroff: %lld\n"
		k (aref arr k) (pref arroff k))))))
      
(my-test28) ;; print outs


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; arrays
;; Extempore lang supports arrays as for first class
;; aggregate types (in other words as distinct from
;; a pointer).
;;
;; an array is made up of a size and a type
;; |32,i64| is an array of 32 elements of type i64 
;;

(bind-type tuple-with-array <double,|32,|4,i32||,float>)

(definec my-test29
  (lambda ()
    (let ((tup (stack-alloc tuple-with-array))
	  (t2 (stack-alloc |32,i64|)))
      (aset! t2 0 9)      
      (tset! tup 2 5.5)
      (aset! (aref-ptr (tref-ptr tup 1) 0) 0 0)
      (aset! (aref-ptr (tref-ptr tup 1) 0) 1 1)
      (aset! (aref-ptr (tref-ptr tup 1) 0) 2 2)
      (printf "val: %lld %lld %f\n"
	      (aref (aref-ptr (tref-ptr tup 1) 0) 1)
	      (aref t2 0) (ftod (tref tup 2)))
      (aref (aref-ptr (tref-ptr tup 1) 0) 1))))

(my-test29) ;; val: 1 9 5.5


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global Variables
;;
;; You can allocate global variables using bind-val
;;

(bind-val g-var-a i32 5)

;; increment g-var-a by inc
;; and return new value of g-var-a
(definec my-test30
  (lambda (inc)
    (set! g-var-a (+ g-var-a inc))
    g-var-a))

(println (my-test30 3)) ;; 8

;; you can bind any primitive type
(bind-val g-var-b double 5.5)
(bind-val g-var-c i1 0)

;; you can bind array types
;; and choose to either
;; a) assign a value to each element
(bind-val g-var-a1 |5,i64| (list 1 2 3 4 5))
;; or b) assign a default value to all elements
;; for example initialize all 1024 double elements to 5.125
(bind-val g-var-a2 |1024,double| 5.125)

(definec test31
  (lambda ()
    (printf "a1[3]:%lld  a2[55]:%f\n" (aref g-var-a1 3) (aref g-var-a2 55))
    1))

(test31)

;; finally you can use sys:make-cptr to allocate
;; memory to any ptr type you like. It is up to
;; you to however to ensure that you allocate an
;; appropriate amount of space.
(bind-val g-var-d |4,i32|* (sys:make-cptr (* 4 4)))
(bind-val g-var-e tuple-with-array* (sys:make-cptr (+ 8 (* 32 (* 4 4)) 4)))

(definec test32
  (lambda ()
    (tset! g-var-e 0 11.0)
    (aset! g-var-d 0 55)
    (printf "%f :: %d\n" (tref g-var-e 0) (aref g-var-d 0))
    1))

(test32) ;; 11.000 :: 55


(bind-val gvar-array |5,double| 0.0)

(definec test33
  (lambda ()
    (aset! gvar-array 3 19.19)
    (aref gvar-array 3)))

(println (test33)) ;; -> 19.19


;; End Of Tutorial
(print)
(println 'finished)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Callbacks

(definec test34
  (lambda (time:i64 count:i64)
    (printf "time: %lld:%lld\n" time count)
    (callback (+ time 1000) test34 (+ time 22050) (+ count 1))))

(test34 (now) 0)


;; compiling this will stop the callbacks
;;
;; of course we need to keep the type
;; signature the same [void,i64,i64]*
;;
(definec test34
  (lambda (time:i64 count:i64)
    void))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; some memzone tests

(definec test35
  (lambda ()
    (let ((b (zalloc |5,double|)))
      (aset! b 0
	(memzone 1024
	   (let ((a (zalloc |10,double|)))
	     (aset! a 0 3.5)
	     (aref a 0))))
      (let ((c (zalloc |9,i32|)))
	(aset! c 0 99)
	(aref b 0)))))


(println (test35)) ;; 3.5


(definec test36
  (lambda ()
    (memzone 1024
      (let ((k (zalloc |15,double|))
	    (f (lambda (fa:|15,double|*)
	         (memzone 1024
		   (let ((a (zalloc |10,double|))
			 (i 0))
		     (dotimes (i 10)
		       (aset! a i (* (aref fa i) (random))))
		   a)))))
	(f k)))))

(definec test37
  (lambda ()
    (let ((v (test36))
	  (i 0))
      (dotimes (i 10) (printf "%lld:%f\n" i (aref v i))))))

;; should print all 0.0's  
(test37)


(definec test38
  (lambda ()
    (memzone 1024 (* 44100 10)
      (let ((a (alloc |5,double|)))
	(aset! a 0 5.5)
	(aref a 0)))))

(println (test38)) ;; 5.50000
	     

;;
;; Large allocation of memory on BUILD (i.e. when the closure is created)
;; requires an optional argument (i.e. an amount of memory to allocate
;; specifically for closure creation)
;;
;; This memory is automatically free'd whenever you recompile the closure
;; (it will be destroyed and replaced by a new allocation of the
;;  same amount or whatever new amount you have allocated for closure
;;  compilation)
;;
(definec test39 1000000
  (let ((k (zalloc |100000,double|)))
    (lambda ()
      (aset! k 0 1.0)
      (aref k 0))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some data structures examples


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FIFO Queue for positive 64bit integers
;;
;; i8* is value  --- list_t* is next
(bind-type list_t <i64,list_t*>)

;; remove value from front of list
(definec dequeue
  (lambda (queue:|2,list_t*|*)
    (let ((front (aref queue 0)))
      (if (null? front) -1
	  (let ((val (tref front 0))
		(back (aref queue 1)))
	    (aset! queue 0 (tref front 1))
	    (if (= back front) (aset! queue 1 null))
	    (free front)
	    val)))))

;; add to the back of the list
(definec enqueue
  (lambda (queue:|2,list_t*|* value:i64)
    (let ((tmp (halloc list_t))
	  (front (aref queue 0))
	  (back (aref queue 1)))
      (tset! tmp 0 value)
      (tset! tmp 1 null)
      (if (null? back) 1 (begin (tset! back 1 tmp) 1))
      (if (null? front) (aset! queue 0 tmp))
      (aset! queue 1 tmp) ;; set back to tmp
      1)))

(definec queue_test
  (lambda ()
    (let ((myqueue (salloc |2,list_t*|))
	  (stuff (salloc |8,i64|))
	  (i 0))
      ;; first we must set queue front and back to null
      (afill! myqueue null null)
      ;; initialize stuff array
      (dotimes (i 8) (aset! stuff i i))
      ;; what happens if we dequeue an empty queue (-1)
      (printf "dequeue 1: %lld\n" (dequeue myqueue))
      ;; add something to the queue
      (enqueue myqueue (aref stuff 1))
      ;; dequeue something
      (printf "dequeue 2: %lld\n" (dequeue myqueue))
      ;; back to nothing?
      (printf "dequeue 4: %lld\n" (dequeue myqueue))
      ;; etc..
      (enqueue myqueue (aref stuff 2))
      (printf "dequeue 5: %lld\n" (dequeue myqueue))
      (enqueue myqueue (aref stuff 3))
      (enqueue myqueue (aref stuff 4))
      (printf "dequeue 6: %lld\n" (dequeue myqueue))
      (printf "dequeue 7: %lld\n" (dequeue myqueue))
      (printf "dequeue 8: %lld\n" (dequeue myqueue))      
      1)))

(queue_test)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Distributed Processing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; do some work
(definec work
  (lambda (a:i64)
    (let ((i:i64 0))
      ;; 1 billion iterations
      (dotimes (i 1000000000)
	(* 2 3 4 5 6)))
    (printf "finished: %lld\n" a)
    ;; return a^2
    (* a a)))

;; start 5 new processes
;; ipc:definec work in each
(define procs
  (map (lambda (n p)
	 (ipc:new n p)
	 (ipc:definec n 'work)
	 n)
       (list "proc-a" "proc-b" "proc-c" "proc-d" "proc-e")
       (list 7097 7096 7095 7094 7093)))

;; call work using ipc:mapcall
;;
;; ipc:mapcall calls a given function on 'n'
;; number of processes and then blocks waiting
;; until it receives 'n' results.
(println 'result:
	 (ipc:mapcall 'work procs
		      '(1) '(2) '(3) '(4) '(5)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ad-Hoc Polymorphism
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extempore supports ad-hoc polymorphism
;; at some stage in the future this will
;; be implicit - but for the moment
;; it is explicitly defined using bind-poly

;; ad-hoc polymorphism allows you to provide
;; different specialisations depending on
;; type.  In other words, a single 'name'
;; can be bound to multiple function
;; implementations each with a uniqute
;; type.

;; for example to create the type variable
;; named 'cos' that ranges over the two
;; closure types [double,double]* and [float,float]*
;; you would call the two lines below

(bind-poly cos* cos)
(bind-poly cos* cosf)

;; the first argument is the polymorphic type name ('cos*')
;; the second argument is an implementation name ('cos' or 'cosf')
;; the third argument is the type ([double,double]* or [float,float]*) 

;; now you can let the compiler
;; decide whether to use cosf or cos

;; here cos defaults to float
;; this is because in this instance
;; both [double,double]* and [float,float]*
;; are acceptable.
;; In this instance the compiler chooses
;; the last poly bound -> [float,float]*
(definec test40
  (lambda (a)
    (cos* a)))

;; you could of course force the issue
;; to [double,double]* by adding a type to a
(definec test41
  (lambda (a:double)
    (cos* a)))

;; in a slightly more complex senario
;; floorf requires a float
(definec test42
  (lambda (a)
    (floorf (cos* a)))) 

;; note that forcing a to double in this case
;; fails because there is no 
;; "cos" poly with signature [float,double]*
(definec test43
  (lambda (a)
    (floorf (cos* a))))  ;; this fails although floor would work


;; poly variables can be for functions of
;; mixed argument lengths
;; 
;; so for example:
(definec my-func-1
  (lambda (a:i8*)
    (printf "%s\n" a)))

(definec my-func-2
  (lambda (a:i8* b:i8*)
    (printf "%s %s\n" a b)))

(definec my-func-3
  (lambda (a:i8* b:i8* c:i8*)
    (printf "%s %s %s\n" a b c)))

;; bind these three functions to poly 'print'
(bind-poly print my-func-1)
(bind-poly print my-func-2)
(bind-poly print my-func-3)

(definec test44
  (lambda (a b c)
    (print a)
    (print a b)
    (print a b c)))

(test44 "extempore's" "polymorphism" "rocks")


;; polys can also specialize
;; on the return type
(definec my-func-4
  (lambda (a:double)
    (* a a)))

(definec my-func-5
  (lambda (a:double)
    (dtoi64 (* a a))))

(bind-poly sqrd my-func-4)
(bind-poly sqrd my-func-5)

;; specialize on [i64,double]*
(definec test45
  (lambda (a:double)
    (+ 1.0 (sqrd a))))

;; specialize on [double,doube]*
(definec test46
  (lambda (a:double)
    (+ 1 (sqrd a))))

(println '-> (test45 5.0))
(println '-> (test46 5.0))


;; Memory Usage In Extempore Lang
;; -------------------------------

;; Extempore supports three types of memory allocation: stack allocation,
;; heap alloation and zone allocation.  The first two of these memory
;; allocation techniques should be familiar to anyone who has programmed
;; in C/C++.  The third allocation type presents a type of middle ground
;; between these two extremes.  Zone allocation in Extempore is in essence
;; a form of stack allocation whose scope is defined by the user.

;; Stack allocation in extempore is identical to stack allocation in C.
;; Stack allocation is made using the stack-alloc call (or salloc for
;; short).  Stack allocations, as in C, are available only for the
;; duration of the function call.  They are deallocated when the function
;; returns.

;; (definec ex1
;;   (lambda ()
;;     (let ((a (stack-alloc double)))
;;       (aset! a 0 5.5)
;;       (aref a 0))))

;; This example demonstrates a stack allocation of a single double (8
;; bytes) bound to the symbol a. The type returned by stack-alloc is
;; always a pointer to the memory allocated.  In ex1 the instance 'a'
;; will be of type double* (a:double*).  An optional integer argument
;; before the requested type results in a multiple allocation.

;; (bind-type vec3 <float,float,float>)

;; (definec ex2
;;   (lambda ()
     



;; ;; calls that draw memory from the current zone

;; make-string (literal strings are constant heap allocations)
;; closures (i.e. lambda)
;; make-array
;; make-tuple
;; zone-alloc

;; ;; call that draw memory from the stack
;; stack-alloc
;; just about everything else

;; ;; calls that draw memory from the heap
;; heap-alloc

;; 
;; Copyright (c) 2011, Andrew Sorensen
;; 
;; All rights reserved.
;; 
;; 
;; Redistribution and use in source and binary forms, with or without 
;; modification, are permitted provided that the following conditions are met:
;; 
;; 1. Redistributions of source code must retain the above copyright notice, 
;;    this list of conditions and the following disclaimer.
;; 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation 
;;    and/or other materials provided with the distribution.
;; 
;; Neither the name of the authors nor other contributors may be used to endorse
;; or promote products derived from this software without specific prior written 
;; permission.
;; 
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
;; ARE DISCLEXTD. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
;; POSSIBILITY OF SUCH DAMAGE.
;; 
;; 



(define *impc:ti:print-sub-checks* #f)
(define *impc:ti:print-main-check* #f)
(define *impc:ti:print-unifications* #f)

(define *impc:compile* #t)
(define *impc:compiler:print* #f)
(define *impc:compiler:print-types* #f)
(define *impc:compiler:verbose* #f)

(define *impc:compiler:print-raw-llvm* #f)

(define *impc:compiler:process* (ipc:get-process-name))
;(define *impc:compiler:process* "utility")

(define *impc:zone* (sys:default-mzone))

(define *impc:default-zone-size* (* 8 1024))

(define icr:new-zone
   (lambda args
     (if (null? args)
	 (sys:create-mzone *impc:default-zone-size*)
	 (sys:create-mzone (car args)))))

(define icr:destroy-zone
   (lambda (zone)
      (if (equal? *impc:zone* zone)
          (set! *impc:zone* (sys:default-mzone)))
      (if (equal? zone (sys:default-mzone))
          (print-notification "You are not allowed to destroy the default zone")
          (sys:destrop-mzone zone))))

(define icr:set-zone
   (lambda (zone)
      (set! *impc:zone* zone)))

(define icr:set-zone-default
   (lambda ()
      (set! *impc:zone* (sys:default-mzone))))

(define llvm:get-function-args-withoutzone
   (lambda (name)
     (if (llvm:get-function (string-append name "_getter"))
	 (let ((ftype (llvm:get-function-args name)))
	   (list* (car ftype) (cddr ftype)))
	 (llvm:get-function-args name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strips pretty-types from source code 
;; returns a cons of (the-new-ast any-explicit-types)
;; 
(define impc:ti:get-var-types
   (lambda (ast)
      (let* ((types '())
             (f (lambda (ast)
                   ;(print 'ast: ast 'types: types)
                   (cond ((null? ast) '())
                         ((atom? ast) ast)
                         ;; ((equal? (car ast) 'dotimes)
                         ;;  (list* 'dotimes
                         ;;         (list (if (regex:match? (symbol->string (caadr ast)) ":")
                         ;;                   (let ((t (regex:split (symbol->string (caadr ast)) ":")))
                         ;;                       (if (regex:match? (cadr t) "^\\<|\\[")
                         ;;                           (if (not (regex:match? (cadr t) "\\>|\\]"))
                         ;;                               (print-error 'Compiler 'Error: 'Syntax 'error: 'bad 'type (cadr t))))
                         ;;                      (set! types (cons (cons (string->symbol (car t)) (string->symbol (cadr t))) types))
                         ;;                      (string->symbol (car t)))
                         ;;                   (caadr ast))
                         ;;               (cadadr ast))
                         ;;         (f (cddr ast))))
                         ((equal? (car ast) 'lambda)
                          (list* 'lambda
                                (map (lambda (a)
                                        (if (regex:match? (symbol->string a) ":")
                                            (let ((t (regex:split (symbol->string a) ":")))                                                                                              
                                               (if (regex:match? (cadr t) "^\\<|\\[")
                                                   (if (not (regex:match? (cadr t) "\\>|\\]"))
                                                       (print-error 'Compiler 'Error: 'Syntax 'error: 'bad 'type (cadr t))))
                                               (set! types (cons (cons (string->symbol (car t)) (string->symbol (cadr t))) types))
                                               (string->symbol (car t)))
                                            a))
                                     (cadr ast))
                                (f (cddr ast))))
                         ((member (car ast) '(let let* letrec))
                          (list* (car ast)
                                 (map (lambda (a)
                                         (if (or (atom? a)                                                 
                                                 (null? (cdr a))
                                                 (> (length (cdr a)) 1))
                                             (print-error 'Compiler 'Error: 'syntax 'error: 'badly 'formed 'let 'expression (sexpr->string a)))
                                         (if (regex:match? (symbol->string (car a)) ":")
                                             (let ((t (regex:split (symbol->string (car a)) ":")))
                                                (if (regex:match? (cadr t) "^\\<|\\[")
                                                    (if (not (regex:match? (cadr t) "\\>|\\]"))
                                                        (print-error 'Compiler 'Error: 'Syntax 'error: 'bad 'type (cadr t))))                                               
                                                (set! types (cons (cons (string->symbol (car t)) (string->symbol (cadr t))) types))                                               
                                                (list (string->symbol (car t)) (car (f (cdr a)))))
                                             (list (car a) (car (f (cdr a))))))
                                      (cadr ast))
                                 (f (cddr ast))))
                         ((pair? ast) 
                          (cons (f (car ast))
                                (f (cdr ast))))))))
         (cons (f ast) types))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simple Compile Time Generics using type classes
;;

(define *impc:ti:vtypes* (list))

(define impc:ti:add-vtype
  (lambda (name type)
    (if (string? type)
	(set! type (impc:ir:get-type-from-pretty-str type)))
    (if (string? name)
	(set! name (string->symbol name)))
    (let ((v (assoc name *impc:ti:vtypes*)))
      (if v
	  (if (member type v) #t
	      (set-cdr! v (cons type (cdr v))))
	  (set! *impc:ti:vtypes* (cons (list name type) *impc:ti:vtypes*)))
      (ascii-print-color 0 7 10)
      (print "Successfull appended type variable ")
      (ascii-print-color 1 2 10)
      (print name)
      (ascii-print-color 0 7 10)
      (print " >>> ")
      (ascii-print-color 1 3 10)
      (print type)
      (ascii-print-color 0 7 10)
      (print)
      #t)))


(define impc:ti:vtypes
  (lambda (name)
    (let ((res (assoc name *impc:ti:vtypes*)))
      (if res (cdr res)
	  #f))))

(define impc:ti:vtype-match?
  (lambda (name type)
    (let ((res (assoc name *impc:ti:vtypes*)))
      (if (list? res) (if (member type res) #t #f)
	  #f))))


(define impc:ti:check-for-vtypes
  (lambda (ast)
    (let* ((atypes (cdr (impc:ti:get-var-types ast)))
	   (types (map (lambda (v) (impc:ti:vtypes (cdr v))) atypes))
	   (rest (cl:remove #f types)))
      (if (> (length rest) 0) #t #f))))

(define impc:ti:force-vtypes
  (lambda (ast)
    (replace-all ast '((y:num . y:double)))))



(define impc:ti:vtype-transforms
  (lambda (name ast)
    (if (symbol? name) (set! name (symbol->string name)))
    (let* ((stripped (impc:ti:get-var-types ast))
	   (args (cadr (caddr (car stripped))))
	   (atypes (reverse (cdr stripped))) ;; statically bound types
	   (tvar-lists (map (lambda (v) (impc:ti:vtypes (cdr v))) atypes))
	   (tlists (cl:remove #f tvar-lists))
	   (combinations (if (= (length tlists) 1)
			     (map (lambda (x) (list x)) (car tlists))
			     (apply multi-list-combination tlists)))
	   (fixedtype-lists (map (lambda (comb)
				   (map (lambda (at c)
					  (let ((findstr (string-append (symbol->string (car at))
									":"
									(symbol->string (cdr at))))
						(replacestr (string-append (symbol->string (car at))
									   ":"
									   (symbol->string c))))
					    (cons (string->symbol findstr)
						  (string->symbol replacestr))))
					atypes
					comb))
				 combinations))
	   (fixedtype-lists2 (map (lambda (comb)
				    (map (lambda (at c)
					   (let ((findstr (string-append ":" (symbol->string (cdr at))))
						 (replacestr (impc:ir:get-base-type (symbol->string c))))
					     (cons (string->symbol findstr)
						   (string->symbol replacestr))))
					 atypes
					 comb))
				 combinations))
	   (newnames (map (lambda (r)
			    (let* ((hash (string-hash (apply string-append
							    (map (lambda (t)
								   (symbol->string t))
								 r))))
				   (nameandhash (string-append name (number->string hash))))
			      (cons (string->symbol name) (string->symbol nameandhash))))
			  combinations)))
      (list (map (lambda (n) (cdr n)) newnames)
	    (map (lambda (n r r2)
		   ;(println '-> (cons n (append r r2)))
		   (replace-all (cl:copy-list ast) (cons n (append r r2))))
		 newnames
		 fixedtype-lists
		 fixedtype-lists2)))))


(define impc:ti:parametric-poly-pass
  (lambda (ast)
    (if (not (impc:ti:check-for-vtypes ast))
	(eval ast (interaction-environment)) ;; if not generic func compile normally
	(let* ((storeprint print) ;; set print to nothing but store original as storeprint
	       (name (cadr ast)) ;; otherwise compile all required templates
	       (dat (impc:ti:vtype-transforms name ast))
	       (_ (set! print (lambda args (storeprint "")))) ;;setting print to suppress compiler output
	       (res (map (lambda (fname nast)
			   ;(println 'fname: fname)
			   (let ((val (call/cc (lambda (k)
						 (set! print-error (lambda args
								     ;(println "andrew world")
								     ;(apply pprint-error args)
								     (k #f)))
						 #t))))
			     ;(println 'bingo: val 'fname: fname)
			     (if (equal? val #t)
				 (let* ((rr (eval nast (interaction-environment)))
					(type (impc:ir:get-function-type (symbol->string fname)))
					(strtype (impc:ir:pretty-print-type type)))
				   (if (null? type)
				       #f
				       (begin					
					 (eval `(bind-poly ,name ,fname ,(string->symbol strtype))
					       (interaction-environment))
					 type))))))
			 (car dat)
			 (cadr dat))))
	  ;; restore print-error and print
	  (set! print-error (lambda args (apply pprint-error args) (error "")))
	  (set! print storeprint)
	  (print)
	  (if (> (length (cl:remove '() (cl:remove #f res))) 0)
	      (begin
		(ascii-print-color 0 7 10)
		(print 'Successfully 'compiled 'generic 'function )
		(ascii-print-color 1 2 10)
		(print "" name)
		(ascii-print-color 0 7 10)
		(print)
		(ascii-print-color 1 3 10)
		(for-each (lambda (k)
			    (print)
			    (print "\t" (impc:ir:pretty-print-type k)))
			  (cl:remove '() (cl:remove #f res)))
		(ascii-print-color 0 7 10)
		(print)
		(print)
		(print ".... ")
		(ascii-print-color 1 2 10)
		(print name)
		(ascii-print-color 0 7 10)
		(print) (print)
		#t)
	      (begin (print-error 'Compile 'Error: 'failed 'to 'find 'any 'valid 'forms 'for 'generic 'function: name)
		     #f))))))

;; (define impc:ti:parametric-poly-pass
;;   (lambda (ast)
;;     (if (not (impc:ti:check-for-vtypes ast))
;; 	(eval ast (interaction-environment)) ;; if not generic func compile normally
;; 	(let* ((name (cadr ast)) ;; otherwise compile all required templates
;; 	      (dat (impc:ti:vtype-transforms name ast))
;; 	      (res (map (lambda (fname nast)
;; 			  (eval nast (interaction-environment))
;; 			  (let* ((type (impc:ir:get-function-type (symbol->string fname)))
;; 				 (strtype (impc:ir:pretty-print-type type)))
;; 			    (if (null? type)
;; 				#f
;; 				(begin					
;; 				  (eval `(bind-poly ,name ,fname ,(string->symbol strtype))
;; 					(interaction-environment))
;; 				  #t))))
;; 			(car dat)
;; 			(cadr dat))))
;; 	  (if (= (length (cl:remove #t res)) 0)
;; 	      (begin
;; 		(ascii-print-color 1 6 10)
;; 		(print)
;; 		(print 'Successfully 'compiled 'generic 'function '-> )
;; 		(ascii-print-color 1 2 10)
;; 		(print "" name)
;; 		(ascii-print-color 0 7 10)		
;; 		(print) (print)
;; 		#t)
;; 	      (begin (print-error 'Compile 'Error: 'failed 'to 'build 'generic 'function name)
;; 		     #f))))))

	

;;
;; TRANSFORM CODE
;;
;; Transform straight R5RS code into
;; a simpler but still valid R5RS scheme code
;;

(define impc:ti:and
   (lambda (ast)
      (if (pair? ast)
          (list 'if (car ast)
                (if (null? (cdr ast))
                    (car ast)
                    (impc:ti:and (cdr ast)))
                #f))))

(define impc:ti:or
   (lambda (ast)
      (if (pair? ast)
          (list 'if (car ast)
                (car ast)
                (if (null? (cdr ast))
                    #f
                    (impc:ti:or (cdr ast)))))))

(define impc:ti:cond
   (lambda (ast)
      (if (null? ast) '()          
	      (list 'if (caar ast) 
                (if (null? (cdar ast))
                    '()
                    (apply list 'begin (cdar ast)))
                (impc:ti:cond (cdr ast))))))

(define impc:ti:map   
   (lambda (ast)
      (list 'let 'maplloop (append (map (lambda (p l)
                                       (cons l (list p)))
                                    (cdr ast)
                                    (list 'l1 'l2 'l3 'l4 'l5 'l6 'l7 'l8 'l9))
                               (list (list 'll '(list))))
            (list 'if '(null? l1) '(reverse ll)
                  (append '(maplloop)
                        (map (lambda (p l)
                                (list 'cdr l))
                             (cdr ast)
                             (list 'l1 'l2 'l3 'l4 'l5 'l6 'l7 'l8 'l9))
                        (list (list 'cons (append (list (car ast))
                                                  (map (lambda (p l)
                                                          (list 'car l))
                                                       (cdr ast)
                                                       (list 'l1 'l2 'l3 'l4 'l5 'l6 'l7 'l8 'l9)))
                                    'll)))))))

(define impc:ti:not
   (lambda (ast)
      (list 'if ast #f #t)))

(define impc:ti:case
   (lambda (expr body)
      (if (null? body)
          '(list)
          `(if ,(if (eq? 'else (caar body))
                    #t
                    (list 'member expr (cons 'list (caar body))))
               ,@(cdar body)
               ,(impc:ti:case expr (cdr body))))))
                             

(define impc:ti:quote
   (lambda (ast)
      (cond ((null? ast) '(impc_null)) ;(list))
            ((symbol? ast) `(llvm_make_symbol ,(symbol->string ast)))
            ((list? ast) 
             (cons 'list (map (lambda (a) 
                                 (if (or (eq? 'NIL a)
                                         (null? a))
                                     '(list)
                                     a))
                              ast)))
            (else ast))))


(define impc:ti:random
   (lambda (ast)
      (case (length ast)
            ((0) (append (list 'imp_rand) ast))
            ((1) (append (list 'imp_rand1) ast))
            ((2) (append (list 'imp_rand2) ast)))))


;; (define impc:ti:array
;;    (lambda (ast)
;;       (let* ((sym (string->symbol (string-append "v" (number->string (llvm:count++))))))
;;          (append '(let) (list (list (list sym (list 'make-array (- (length ast) 1)
;;                                                     (if (integer? (cadr ast)) 0 0.0)))))
;;                  (make-list-with-proc (- (length ast) 1)
;;                                       (lambda (i)
;;                                          (list 'array-set! sym i (list-ref ast (+ i 1)))))
;;                  (list sym)))))

;; (define impc:ti:tuple
;;    (lambda (ast)
;;       (let* ((sym (string->symbol (string-append "v" (number->string (llvm:count++))))))
;;          (append '(let) (list (list (list sym (list 'make-tuple (- (length ast) 1)
;;                                                     (if (integer? (cadr ast)) 0 0.0)))))
;;                  (make-list-with-proc (- (length ast) 1)
;;                                       (lambda (i)
;;                                          (list 'tuple-set! sym i (list-ref ast (+ i 1)))))
;;                  (list sym)))))


;; no anonymous lambdas !!!
(define impc:ti:lambda
   (lambda (ast)
      (let* ((fname (gensym))
             (rest (cons (impc:ti:first-transform (cadr ast) #t)
                         (list (cons 'begin (impc:ti:first-transform (cddr ast) #t)))))
             (expr (cons 'lambda rest)))
         `(let ((,fname ,expr))
             (begin ,fname)))))


;; replace (* 2 3 4 5) or (+ 2 3 4 5)
;; with (* 2 (* 3 (* 4 5))) etc..
(define impc:ti:binary-arity
   (lambda (ast inbody?)
      (let ((op (car ast))
            (inlst (reverse (cdr ast))))
         (let loop ((rest (cdr inlst))
                    (lst (car inlst)))
            (if (null? rest) lst
                (loop (cdr rest) (cons op (cons (impc:ti:first-transform (car rest) inbody?) (list lst)))))))))
				
				
(define impc:ti:binary-arity
   (lambda (ast inbody?)
      (let ((op (car ast))
            (inlst (cdr ast)))
         (if (< (length inlst) 2)
                (print-error 'Compiler 'Error: 'Bad 'arity 'in 'math 'expr ast))
         (let loop ((rest (cddr inlst))
                    (lst (list op 
                               (impc:ti:first-transform (car inlst) inbody?) 
                               (impc:ti:first-transform (cadr inlst) inbody?))))
            (if (null? rest) lst
                (loop (cdr rest) (list op lst (impc:ti:first-transform (car rest) inbody?))))))))


(define impc:ti:bitwise-not-to-eor
  (lambda (ast inbody?)
    (list 'bitwise-eor (cadr ast) -1)))


(define impc:ti:afill!
  (lambda (ast)
    (append '(begin)
	    (map (lambda (arg idx)
		   (list 'aset! (car ast) idx arg))
		 (cdr ast)
		 (make-list-with-proc (length ast) (lambda (i) i))))))


(define impc:ti:pfill!
  (lambda (ast)
    (append '(begin)
	    (map (lambda (arg idx)
		   (list 'pset! (car ast) idx arg))
		 (cdr ast)
		 (make-list-with-proc (length ast) (lambda (i) i))))))


(define impc:ti:tfill!
  (lambda (ast)
    (append '(begin)
	    (map (lambda (arg idx)
		   (list 'tset! (car ast) idx arg))
		 (cdr ast)
		 (make-list-with-proc (length ast) (lambda (i) i))))))

(define *unique-polynum* 0)

(define impc:ti:first-transform
   (lambda (ast inbody?)
      ;(println inbody? 'transforming-ast: ast)
      (if (null? ast) '()
          (cond ((list? ast)
                 (cond ((impc:ir:poly-types (car ast))
			(set! *unique-polynum* (+ 1 *unique-polynum*))
			(cons (string->symbol (string-append (symbol->string (car ast))
							     "$$$"
							     (number->string *unique-polynum*)))
			      (impc:ti:first-transform (cdr ast) inbody?)))
		       ((eq? (car ast) 'and) 
                        (impc:ti:first-transform (impc:ti:and (cdr ast)) inbody?))
                       ((eq? (car ast) 'random)
                        (impc:ti:first-transform (impc:ti:random (cdr ast)) inbody?))
                       ((eq? (car ast) 'quote)
                        (impc:ti:first-transform (impc:ti:quote (cadr ast)) inbody?))
                       ((member (car ast) '(map for-each))
                        (impc:ti:first-transform (impc:ti:map (cdr ast)) inbody?))
                       ((eq? (car ast) 'case) 
                        (impc:ti:first-transform (impc:ti:case (cadr ast) (cddr ast)) inbody?))
		       ((eq? (car ast) 'afill!)
			(impc:ti:first-transform (impc:ti:afill! (cdr ast)) inbody?))
		       ((eq? (car ast) 'pfill!)
			(impc:ti:first-transform (impc:ti:pfill! (cdr ast)) inbody?))
		       ((eq? (car ast) 'tfill!)
			(impc:ti:first-transform (impc:ti:tfill! (cdr ast)) inbody?))
                       ((eq? (car ast) 'or) 
                        (impc:ti:first-transform (impc:ti:or (cdr ast)) inbody?))
                       ((eq? (car ast) 'free) 
                        (list 'free (list 'bitcast (impc:ti:first-transform (cadr ast) inbody?)
                                          'i8*)))
                       ((member (car ast) '(array))
                        (impc:ti:first-transform (impc:ti:array ast) inbody?))
                       ((member (car ast) '(tuple))
                        (impc:ti:first-transform (impc:ti:tuple ast) inbody?))     
                       ;((eq? (car ast) 'null?)
                       ; (impc:ti:first-transform `(impc_isnull ,(cadr ast)) inbody?))
                       ((eq? (car ast) 'not) 
                        (impc:ti:first-transform (impc:ti:not (cadr ast)) inbody?))
                       ;; ((eq? (car ast) 'list)
                       ;;  (impc:ti:first-transform (impc:ti:binary-arity (cons 'mcons (append (cdr ast) '(nilnil))) inbody?) inbody?))
		       ((and (member (car ast) '(* - / + bitwise-and bitwise-or bitwise-eor bitwise-shift-left bitwise-shift-right))
                             (<> (length ast) 3))
                        (impc:ti:first-transform (impc:ti:binary-arity ast inbody?) inbody?))
		       ((eq? (car ast) 'bitwise-not)
			(impc:ti:bitwise-not-to-eor ast inbody?))
                       ((eq? (car ast) 'lambda)
                        (if inbody?
                            (impc:ti:lambda ast)
                            (cons (impc:ti:first-transform (car ast) inbody?)
                                  (cons (impc:ti:first-transform (cadr ast) #t)
                                        (list (cons 'begin (impc:ti:first-transform (cddr ast) #t)))))))
                       ;; ((eq? (car ast) 'cons)
		       ;; 	(println 'ast: ast)
                       ;;  (impc:ti:first-transform (impc:ti:cons ast) inbody?))                       
                       ((eq? (car ast) 'cond)
                        (impc:ti:first-transform (impc:ti:cond (cdr ast)) inbody?))
                       ((eq? (car ast) 'cset!)
                        (list 'closure-set! 
                              (impc:ti:first-transform (cadr ast) inbody?)
                              (symbol->string (caddr ast))
                              (impc:ti:first-transform (cadddr ast) inbody?)
			      (if (not (null? (cddddr ast)))
				  (impc:ir:get-type-str (impc:ir:convert-from-pretty-types (car (cddddr ast)))))))
                       ((eq? (car ast) 'cref)
                        (list 'closure-ref 
                              (impc:ti:first-transform (cadr ast) inbody?)
                              (symbol->string (caddr ast))
                              (if (not (null? (cdddr ast)))
				  (impc:ir:get-type-str (impc:ir:convert-from-pretty-types (cadddr ast))))))
                       ((eq? (car ast) 'dotimes)
			(list 'dotimes 
			      (impc:ti:first-transform (cadr ast) inbody?)
			      (cons 'begin (impc:ti:first-transform (cddr ast) inbody?))))
                       ((member (car ast) '(let let* letrec))
                        (cons (impc:ti:first-transform (car ast) inbody?)
                              (cons (impc:ti:first-transform (cadr ast) #f)
                                    (list (cons 'begin (impc:ti:first-transform (cddr ast) #t))))))
                       ((and (symbol? (car ast))
                             (regex:match? (symbol->string (car ast)) ".*\\..*"))
                        (if (regex:match? (symbol->string (car ast)) ".*\\..*:.*")                            
                            (let* ((subs (regex:split (symbol->string (car ast)) "\\."))
                                   (a (string->symbol (car subs)))
                                   (subs2 (regex:split (cadr subs) ":"))
                                   (b (string->symbol (car subs2)))
                                   (c (string->symbol (cadr subs2))))
			      (if (= (length ast) 1)
				  (impc:ti:first-transform (list 'cref a b c) inbody?)
				  (impc:ti:first-transform (list 'cset! a b (cadr ast) c) inbody?)))
                            (let* ((subs (regex:split (symbol->string (car ast)) "\\."))
                                   (a (string->symbol (car subs)))
				   (b (string->symbol (cadr subs))))
			      (if (= (length ast) 1)
				  (impc:ti:first-transform (list 'cref a b) inbody?)
				  (impc:ti:first-transform (list 'cset! a b (cadr ast)) inbody?)))))
                       ((and (atom? (car ast))
                             (symbol? (car ast))
                             (not (eq? 'dotimes (car ast)))
                             (defined? (car ast))
                             (macro? (eval (car ast))))
                        (macro-expand ast))
                       (else (cons (impc:ti:first-transform (car ast) inbody?)
                                   (impc:ti:first-transform (cdr ast) inbody?)))))
                (else (cond ((eq? ast #f) '(impc_false))
                            ((eq? ast #t) '(impc_true))
			    ((eq? ast 'cast) 'bitcast)
                            ((eq? ast 'else) '(impc_true))
                            ((eq? ast '*samplerate*) '(llvm_samplerate))
                            ((eq? ast 'printf) 'llvm_printf)
                            ((eq? ast 'sprintf) 'llvm_sprintf)			    
			    ((eq? ast 'null) '(impc_null))
			    ((eq? ast 'now) 'llvm_now)
			    ((eq? ast 'pset!) 'pointer-set!)
			    ((eq? ast 'pref) 'pointer-ref)
			    ((eq? ast 'pref-ptr) 'pointer-ref-ptr)			    
			    ((eq? ast 'aset!) 'array-set!)
			    ((eq? ast 'aref) 'array-ref)
			    ((eq? ast 'aref-ptr) 'array-ref-ptr)
			    ((eq? ast 'tset!) 'tuple-set!)
			    ((eq? ast 'tref) 'tuple-ref)
			    ((eq? ast 'tref-ptr) 'tuple-ref-ptr)
			    ((eq? ast 'salloc) 'stack-alloc)
			    ((eq? ast 'halloc) 'heap-alloc)
			    ((eq? ast 'zalloc) 'zone-alloc)
			    ((eq? ast 'alloc) 'zone-alloc)
			    ((eq? ast 'schedule) 'callback)
			    ((eq? ast 'void) '(void))
                            (else ast)))))))


;;
;; TYPE INFERENCE CODE
;;
;; request? can be a type - or a symbol if it's a symbol it must be a free variable available in vars
;;
;;

;; takes types with symbols and expands them
;; using types associated with symbols in vars
;; if a particular var doesn't have a type yet
;; then we try to reverse expand 
;; (i.e. look at other closure options that may include type values
;; and assign those values into vars)
(define impc:ti:symbol-expand
   (lambda (vs vars all-vs)
      (if (atom? vs)
          (if (symbol? vs)
              (if (not (assoc vs vars)) 
                  (print-error 'Compiler 'Error: 'variable 'not 'marked 'as 'free! vs)
                  ;; check to see a type has been defined
                  ;; otherwise return null
                  (let ((t (cdr (assoc vs vars)))) 
                     ;; first check to see if he symbol vs has a value                     
                     (if (null? t) ;; if it doesn't we might need to reverse match!
                         (let* ((positions (map (lambda (x)
                                                   (if (atom? x)
                                                       (print-error 'Compiler 'Error: 'severe 'type 'error: 'have 'you 'specified 'an 'incorrect 'type?)
                                                       (cl:position vs x)))
                                                all-vs))
                                (position (cl:find-if number? positions))
                                (values (if position
                                            (map (lambda (x)
                                                    (list-ref x position))
                                                 all-vs)
                                            '()))
                                (value (cl:find-if impc:ir:type? values)))
                            ;; if we found a value force it into vars
                            (if value (impc:ti:force-var vs vars '() value))
                            ;(if value (print-notification 'backward 'assigning value 'to vs))
                            (if value
                                value
                                t))
                         t))) ;; else if symbol does have a value then return it
              vs)
          (cons (impc:ti:symbol-expand (car vs) vars all-vs)
                (impc:ti:symbol-expand (cdr vs) vars all-vs)))))



;; impc:ti:intersection* is cl:intersection for 
;; an arbirary number of sets (i.e. args)
;; also handles *impc:ir:other* which we want
;; to match against anything.
(define impc:ti:intersection*   
   (lambda args
      (let loop ((a args)
                 (res '()))
         (if (null? a) res
             (loop (cdr a) 
                   (if (null? res)
                       (car a)
                       (if (null? (car a))
                           res
                           (cl:intersection (car a) res))))))))


(define impc:ti:complex-unify
   (lambda (sym types)
      ;; first a sanity check
      (if (cl:find-if (lambda (x) (not (list? x))) types)
          (apply print-error 'Compiler 'Error: 'bad 'type: (symbol->string sym) 'invalid 'mixed 'type 'definitions: types))
      ;; first check that all complex types
      ;; are the same length
      ;; otherwise we have a problem
      (if (and (> (length types) 1)
               (not (apply = (map length types))))
          (print-error 'Compiler 'Error: 'un-even 'type 'error 'for 'sym: sym 'types: types)
          (map (lambda (ts)
                  ;(print 'ts: ts)
                  (let ((ttt (if (list? ts)
                                 (apply impc:ti:intersection* ts)
                                 (impc:ti:intersection* ts))))
                     (if (and (list? ttt)
                              (= (length ttt) 1))
                         (car ttt)
                         ttt)))
               (map (lambda (i)
                       (map (lambda (t)
                               (let ((tt (list-ref t i)))
                                  (if (list? tt) 
                                      ;; check to see if tt need further unification
                                      (cond ((null? tt) '())
                                            ((list? (car tt))
                                             (impc:ti:complex-unify sym tt))
                                            (else tt))
                                      ;; need to make atoms into lists 
                                      ;; so we can pass them through cl:intersection
                                      (list tt))))
                            types))
                    (make-list-with-proc (length (car types)) (lambda (i) i)))))))


;; return simple types without change
;; pass complex types through to impc:ti:complex-unify
(define impc:ti:type-unify
   (lambda (sym types)
      ;(print 'types: types)
      (if (not (cl:find-if list? types))
          (if (= (length types) 1)
              (car types)
              types) ;; if not complex just return it
          (impc:ti:complex-unify sym types))))


;;
;; takes un-unified types and returns unified ones (hopefully!)
;; 
(define impc:ti:unify
   (lambda (vars)
      (map (lambda (v)              
              (let* ((sym (car v))
                     ;; expand any symbols and do reverse symbol checks
                     (types-expanded (map (lambda (t)                                   
                                             (if (or (symbol? t)
                                                     (list? t))
                                                 (impc:ti:symbol-expand t vars (cdr v))
                                                 t))
                                          (cdr v)))
                     (types-unified (impc:ti:type-unify sym types-expanded)))  
                 ;(print 'un-expanded (cdr v) 'un-unified types-expanded 'unified types-unified)
                 (cons sym types-unified)))
           vars)))

;; checks to see if a type system is completely unified
(define impc:ti:unity?
   (lambda (vars)
      (map (lambda (x)
              (if (impc:ir:type? (cdr x)) #t #f))
           vars)))
              

;; join elements into a list (without including nulls)
(define impc:ti:join
   (lambda args
      (cl:remove-if null? args)))

;; probably should use a state monad for this but ...
;; vars must be a list of lists
;; NOT an assoc list with pairs
;;
;; don't allow update to add to kts values
(define impc:ti:update-var
   (lambda (sym vars kts t)
      ;(println 'update-var:> sym 'in: vars 'with: t 'kts: kts)
      (if (member sym kts) ;; if in known types don't do anything
          '()
          (if (and (not (assoc sym vars))
		   (not (llvm:get-function (symbol->string sym)))
		   (not (llvm:get-globalvar (symbol->string sym))))
              (print-error 'Compiler 'Error: 'var (symbol->string sym) 'does 'not 'exist)
              (let ((pair (assoc sym vars)))
		(if pair
		    (if (impc:ir:type? t)
			(set-cdr! pair (cl:remove-duplicates (append (list t) (cdr pair))))
			(set-cdr! pair (cl:remove-duplicates (append t (cdr pair)))))
		    '()))))))


;; force a var to a particular type
;; (i.e. wipe out other choices)
;;
;; do allow force-var to overwrite kts values
(define impc:ti:force-var
   (lambda (sym vars kts t)
      ;(println 'force-var:> sym 'in: vars 'with: t 'kts: kts)            
      (if (and (not (assoc sym vars))
               (not (llvm:get-globalvar (symbol->string sym))))
          (print-error 'Compiler 'Error: 'var (symbol->string sym) 'does 'not 'exist)
          (let ((pair (assoc sym vars)))             
             (if pair
                 (if (impc:ir:type? t)
                     (set-cdr! pair (list t))
                     (set-cdr! pair t))
                 '())))))

(define impc:ti:get-var
   (lambda (sym vars)
      (if (not (assoc sym vars))
          (print-error 'Compiler 'Error: 'var (symbol->string sym) 'does 'not 'exist)
          (assoc sym vars))))
		  
(define impc:ti:get-var
   (lambda (sym vars)
      (if (not (assoc sym vars))
          (if (llvm:get-global-variable-type (symbol->string sym))
              (cons sym (- (impc:ir:get-type-from-str (llvm:get-global-variable-type (symbol->string sym)))
                           *impc:ir:pointer*))
              (print-error 'Compiler 'Error: 'var (symbol->string sym) 'does 'not 'exist))
          (assoc sym vars))))
		  

;; clear all vars
(define impc:ti:clear-all-vars
   (lambda (vars)
      (map (lambda (x)
              (set-cdr! x '()))
           vars)))

(define impc:ti:numeric-check
   (lambda (ast vars kts request?)      
      (if *impc:ti:print-sub-checks* (println 'num:> 'ast: ast 'request? request?))
      (if (and request?
               (not (null? request?)))
          (cond ((symbol? request?) 
                 (let* ((t1 (impc:ti:symbol-check request? vars kts #f))
                        (t2 (impc:ti:numeric-check ast vars kts #f))
                        (t3 (cl:intersection t1 t2)))
                    t3))
                ((list? request?)
                 (let* ((t1 (impc:ti:numeric-check ast vars kts #f))
                        (t2 (cl:intersection request? t1)))
                    t2))
                ((number? request?)
                 (let* ((t1 (impc:ti:numeric-check ast vars kts #f))
                        (t2 (cl:intersection (list request?) t1)))
                    t2))
                (else (print-error 'Compiler 'Error: 'shouldn't 'reach 'here 'in 'numeric 'check 'request? request?)
		      (print-error '-> 'You 'might 'be 'using 'a 'pref 'where 'you 'should 'be 'using 'a 'tref?)))
          (if (integer? ast)  ;; preference goes to start of list
	      (if (or (= 1 ast) (= 0 ast))
	      	  (list *impc:ir:si64* *impc:ir:si32* *impc:ir:ui8* *impc:ir:i1*)
		  (if (< ast 256)
		      (list *impc:ir:si64* *impc:ir:si32* *impc:ir:ui8*)
		      (list *impc:ir:si64* *impc:ir:si32*)))  ;*impc:ir:double* *impc:ir:float*))
              (list *impc:ir:double* *impc:ir:float*)))))


(define impc:ti:symbol-check
   (lambda (ast vars kts request?)
      (if *impc:ti:print-sub-checks* (println 'sym:> 'ast: ast 'request? request?))
      ;; if a request is made - assume it's forced
      ;; find the intersection between the request
      ;; and the current values and force that intersection
      (if (and (not (assoc ast vars))
	       (not (llvm:get-function (symbol->string ast)))
               (not (llvm:get-globalvar (symbol->string ast))))
          (print-error 'Compiler 'Error: 'unbound 'symbol: ast))
      (let ((type (if (assoc ast vars)
                      (cdr (assoc ast vars))
		      (if (llvm:get-function (symbol->string ast))
			  (list (cons (+ *impc:ir:closure* *impc:ir:pointer* *impc:ir:pointer*) (map impc:ir:get-type-from-str (llvm:get-function-args-withoutzone (symbol->string ast)))))
                          (list (impc:ir:pointer-- (impc:ir:get-type-from-str (llvm:get-global-variable-type (symbol->string ast)))))))))
         (if (and request?
                  (not (null? request?)))             
             (let ((intersection (cl:intersection (if (null? type) ;; if type is null then force request
                                                      (if (atom? request?) (list request?) request?)
                                                      (if (atom? type) (list type) type))
                                                  ;; if request is not a set make it one
                                                  (if (atom? request?) (list request?) request?))))
                (if (not (null? intersection))
                    (begin (impc:ti:force-var ast vars kts intersection)
                           intersection)
                    type))
             type))))



(define impc:ti:math-check
   (lambda (ast vars kts request?)
      (let* ((a (impc:ti:type-check (cadr ast) vars kts request?))
             (b (impc:ti:type-check (caddr ast) vars kts request?))
             (t (cl:intersection (if (atom? a) (list a) a) 
                                 (if (atom? b) (list b) b))))
         ;(println 'math: a b 'vars: vars)
         (if *impc:ti:print-sub-checks* (println 'math:> 'ast: ast 'a: a 'b: b 't: t 'request? request?))
         (if (not (null? t))
             (begin (if (symbol? (cadr ast)) (impc:ti:force-var (cadr ast) vars kts t))
                    (if (symbol? (caddr ast)) (impc:ti:force-var (caddr ast) vars kts t))
                    t)
             (cond ((not (cl:find-if symbol? (cdr ast))) t) ;; return t
                   ((and (symbol? (cadr ast)) 
                         (symbol? (caddr ast))                         
                         (not (null? (cdr (impc:ti:get-var (cadr ast) vars))))
                         (not (null? (cdr (impc:ti:get-var (caddr ast) vars)))))
                    ;; if both are symbols and their types cannot unify on anything
                    ;; then we have a problem!  So force both types to NULL
                    (impc:ti:force-var (cadr ast) vars kts '())
                    (impc:ti:force-var (caddr ast) vars kts '())
                    t) ;; and return t (which should be NULL)                   
                   ((and (symbol? (cadr ast)) (not (null? b)))
                    (impc:ti:update-var (cadr ast) vars kts b) b) ;; return b
                   ((and (symbol? (caddr ast)) (not (null? a)))
                    (impc:ti:update-var (caddr ast) vars kts a) a) ;; return a
                   (else t))))))
				   


(define impc:ti:compare-check
   (lambda (ast vars kts request?)
      (let* ((n1 (if (number? (cadr ast)) (caddr ast) (cadr ast)))
	     (n2 (if (number? (cadr ast)) (cadr ast) (caddr ast)))
	     (a (impc:ti:type-check n1 vars kts request?))
             (b (impc:ti:type-check n2 vars kts request?))
             (t (cl:intersection (if (atom? a) (list a) a) 
                                 (if (atom? b) (list b) b))))
         (if *impc:ti:print-sub-checks* (println 'compare:> 'ast: ast 'a: a 'b: b 't: t 'request? request?))
         (if (not (null? t)) 
             (list *impc:ir:i1*)
             (cond ((not (cl:find-if symbol? (cdr ast))) (list *impc:ir:i1*)) ;; return t
                   ((and (symbol? n1) 
                         (symbol? n2)                         
                         (not (null? (cdr (impc:ti:get-var n1 vars))))
                         (not (null? (cdr (impc:ti:get-var n2 vars)))))
                    ;; if both are symbols and their types cannot unify on anything
                    ;; then we have a problem!  So force both types to NULL
                    (impc:ti:force-var n1 vars kts '())
                    (impc:ti:force-var n2 vars kts '())
                    (list *impc:ir:i1*)) ;; and return t (which should be NULL)
                   ((and (symbol? n1) (not (null? b)))
                    (impc:ti:update-var n1 vars kts b)
                    (list *impc:ir:i1*)) ;; return b
                   ((and (symbol? n2) (not (null? a)))
                    (impc:ti:update-var n2 vars kts a) 
                    (list *impc:ir:i1*)) ;; return a
                   (else (list *impc:ir:i1*)))))))


(define impc:ti:nativef-check
   (lambda (ast vars kts request?)
      ;(println 'native-check 'ast: ast 'vars: vars 'request: request?)
      (let ((ftype (map impc:ir:get-type-from-str
                        (llvm:get-function-args-withoutzone (symbol->string (car ast))))))
	 ;(println 'ftype:> 'ast: ast 'type: ftype)
         (if *impc:ti:print-sub-checks* (println 'ftype:> 'ast: ast 'type: ftype))

         (if (<> (length ftype) 
                 (length ast))
             (print-error 'Compiler 'Error: 'bad 'arity 'in 'call ast))

         ;; we don't care what we get back because we already know the return type
         (for-each (lambda (a t)
                      ;; if a is a symbol then add type t to a
                      ;; we also know that for native functions there 
                      ;; is no choice about the type so we should
                      ;; force it to the type not update it
                      ;(if (symbol? a) (impc:ti:force-var a vars kts t))
		      (if (symbol? a) (impc:ti:update-var a vars kts t))
                      (impc:ti:type-check a vars kts t))
                   (cdr ast)
                   (cdr ftype))
         (list (car ftype)))))


;; polymorphic version
(define impc:ti:nativef-poly-check
   (lambda (ast vars kts request?)
      ;(println 'poly-check 'ast: ast 'vars: vars 'request: request?)     
      (let* ((polyf (string->symbol (car (regex:split (symbol->string (car ast)) "\\$\\$\\$"))))
 	     (ftypes (impc:ir:poly-types polyf))
	     (valid-lgth (map (lambda (type)
				(if (<> (length (cdr type))
					(length ast)) #f #t))
			      ftypes))
	     (valid-args (map (lambda (type valid)
				;(println 'type: type valid)
				(if valid
				    (let* ((checked-types
					    (map (lambda (a t)
						   ;(println 'a: a 't: t)
						   (impc:ti:type-check a vars kts t))
						 (cdr ast)
						 (cddr type)))
					   (ct2 (map (lambda (ct ft) ;; checked type against poly type
						       ;(println 'ct: ct 'ft: ft)
						       (if (number? ct)
							   (if (= ct ft) #t #f)
							   (if (string? ct)
							       (if (string=? ct ft) #t #f)
							       (if (list? ct)
								   (if (member ft ct) #t #f)
								   #f))))
						     checked-types
						     (cddr type))))
				      ;(println 'ct: ct2 'checked: checked-types)
				      ;(println '__checked-types checked-types (cdr (cdadr type)) ct2)
				      (if (member #f ct2) #f #t))
				    #f))
			      ftypes
			      valid-lgth))
	     (valid-polys (remove '()
				  (map (lambda (type valid)
					 (if valid type '()))
				       ftypes
				       valid-args))))
	
	(if (null? valid-polys)
	    (print-error 'Compiler 'Error: 'no 'valid 'polymorphic 'options 'for: ast))

	;(println ast 'valid-polys: valid-polys 'request: request?)
	
	(let ((returns (map (lambda (t)
			      (cadr t))
			    valid-polys)))
	  (if request?
	      (begin (if (atom? request?)
			 (set! request? (list request?)))
		     (set! returns (impc:ti:intersection* returns request?))))

	  (set! valid-polys (cl:remove #f
			       (map (lambda (v)
				      (if (member (cadr v) returns)
					  v
					  #f))
				    valid-polys)))
	  ;; update valid-polys to reflect return types (from request?)
	  ;(println 'valid-polys: valid-polys 'request? request? 'ast: ast) 
	  (impc:ti:update-var (car ast) vars kts valid-polys)

	  ;(println 'returns: returns)
	  returns))))

	  
      


(define impc:ti:callback-check
   (lambda (ast vars kts request?)
      ;(println 'callback-check 'ast: ast 'vars: vars 'request: request?)
      (let ((ftype (map impc:ir:get-type-from-str
                        (let ((ags (llvm:get-function-args-withoutzone (symbol->string (caddr ast)))))
			  (if ags ags '())))))
	;(println 'ftype: ftype)
	(if *impc:ti:print-sub-checks* (println 'ftype:> 'ast: ast 'type: ftype))
	(if (null? ftype)
	    (begin (impc:ti:type-check (cadr ast) vars kts *impc:ir:si64*)
		   (impc:ti:type-check (cddr ast) vars kts '())
		   (list *impc:ir:void*))
	    (begin (if (<> (+ 2 (length ftype))
			   (length ast))
		       (print-error 'Compiler 'Error: 'bad 'arity 'in 'call ast))
		   ;; we don't care what we get back
		   (for-each (lambda (a t)
			       (if (symbol? a) (impc:ti:update-var a vars kts t))
			       (impc:ti:type-check a vars kts t))
			     (cdddr ast)
			     (cdr ftype))
		   ;; callback returns void	 
		   (list *impc:ir:void*))))))


(define impc:ti:memzone-check
  (lambda (ast vars kts request?)
    (if (or (> (length ast) 4)
	    (< (length ast) 3))
	(print-error 'Compiler 'Error: 'memzone 'must 'be 'of 'the 'form '(memzone size [delay] body) 'where "delay" 'is 'optional 'and "body" 'is 'a 'let, 'begin, 'lambda 'etc..))
    ;(println 'memzonecheck ast (list? (cadr ast)))
    (if (= (length ast) 3)
	(begin (impc:ti:type-check (cadr ast) vars kts *impc:ir:si64*)
	       (impc:ti:type-check (caddr ast) vars kts request?))
	(begin (impc:ti:type-check (cadr ast) vars kts *impc:ir:si64*)
	       (impc:ti:type-check (caddr ast) vars kts *impc:ir:si64*)
	       (impc:ti:type-check (cadddr ast) vars kts request?)))))



(define impc:ti:let-check
   (lambda (ast vars kts request?)
      ;; for the symbols we want to set each return type
      (for-each (lambda (e)
		  (let ((a (impc:ti:type-check (cadr e) vars kts
					       (if (member (car e) kts)
						   (cadr (assoc (car e) vars))
						   request?))))
		    ;(println 'update: (car e) 'with: a)  
		    (impc:ti:update-var (car e) vars kts a)))
                (cadr ast))
      ;; then return the return type for the whole let
      ;; which should have a begin body! so caddr should work
      (impc:ti:type-check (caddr ast) vars kts request?)))
	  

(define impc:ti:null?-check
   (lambda (ast vars kts request?)
      (let ((a (impc:ti:type-check (cadr ast) vars kts request?)))
	;(println 'a: a 'ast: ast 'car: (impc:ir:pointer? (car a)))
	(if (if (not (impc:ir:type? a))
		(impc:ir:pointer? (car a))
		(impc:ir:pointer? a))
	    (list *impc:ir:i1*)
	    (print-error 'Compiler 'Error: 'null? 'must 'take 'a 'pointer 'type (sexpr->string ast))))))


(define impc:ti:null-check
   (lambda (ast vars kts request?)
     ;(println 'null-check 'ast: ast 'request? request?)
     (if (and request?
	      (impc:ir:pointer? request?))
	 request?
	 (list *impc:ir:i8*))))


(define impc:ti:ret-check
   (lambda (ast vars kts request?)
      ;; grab function name from ret->
      (let* ((sym (if (equal? (caddr ast) (cadr ast))
                      '()
                      (impc:ti:get-var (cadr ast) vars)))
             (t (if (null? sym) #f 
                    (if (null? (cdr sym))
			#f
			(if (impc:ir:type? (cdr sym))
			    (cdr sym)
			    (car (cdr sym))))))
                        ;(car (cdr sym)))))
             ;; if closure has a return type set
             ;; pass it as a request
             (a (impc:ti:type-check (caddr ast) vars kts 
                                    (if (and t
                                             (impc:ir:type? t)
                                             (impc:ir:closure? t))
					(if (list? t) (cadr t) #f)
                                        #f)))) ;; or else pass #f
         ;; if t is not a closure type we have a problem!
         (if (and t
                  (or (not (list? t));(not (impc:ir:type? t))
                      (not (impc:ir:closure? t))))
             (print-error 'Compiler 'Error: 'type 'error 'calculating 'return 'type: ast 
                          'have 'you 'specified 'an 'incorrect 'closure 'type?))
         (if *impc:ti:print-sub-checks* (println 'ret:> 'ast: ast 'a: a 'sym: sym))
         (if t
             ;; if the return value is a symbol then it should be
             ;; give then return type of 't             
             (if (symbol? (caddr ast))
                 (impc:ti:update-var (caddr ast) vars kts (list (cadr t)))
                 ;; else the return value is not a symbol
                 ;; and we should use it's value to update the lambda's type
                 (impc:ti:update-var (car sym) vars kts
                                     (list (impc:ir:pointer++ (impc:ir:pointer++ (list* *impc:ir:closure* a (cddr t))))))))
         a)))


(define impc:ti:begin-check
   (lambda (ast vars kts request?)
      (let ((a (car (reverse (map (lambda (e)
                                     (impc:ti:type-check e vars kts request?))
                                  (cdr ast))))))
         (if *impc:ti:print-sub-checks* (println 'begin:> 'ast: ast 'a: a))
         a)))
		 

(define impc:ti:bitcast-check
   (lambda (ast vars kts request?)
      ;; for the symbols we want to set each return type
      ;(impc:ti:update-var (cadr ast) vars kts (impc:ir:convert-from-pretty-types (caddr ast)))
      (list (impc:ir:convert-from-pretty-types (caddr ast)))))
		 

(define impc:ti:if-check
   (lambda (ast vars kts request?)
      (let* ((a (impc:ti:type-check (cadr ast) vars kts request?))
             (b (impc:ti:type-check (caddr ast) vars kts request?))
             (c (if (null? (cdddr ast))
		    '()
		    (impc:ti:type-check (cadddr ast) vars kts request?)))
             (t (cl:intersection (if (atom? b) (list b) b) (if (atom? c) (list c) c))))
         (if *impc:ti:print-sub-checks* (println 'if:> 'a: a 'b: b 'c: c 't: t))
         (if (null? b)
             (set! t c))
         (if (null? c)
             (set! t b))
         ;; return intersection of b and c
         (if (null? t) 
             (print-error 'Compiler 'Error: 'cannot 'unify 'then (map (lambda (v) (impc:ir:get-type-str v)) b) 'and 'else (map (lambda (v) (impc:ir:get-type-str v)) c) 'clauses 'in ast)
             t))))

;; ;; make-array should be of the form
;; ;; (make-array num type)
;; ;; where num is fixed point and type is a valid type
;; (define impc:ti:make-array-check
;;    (lambda (ast vars kts request?)      
;;       ;; make-array should have a type
;;       (let ((a (list *impc:ir:array*
;; 		     (cadr ast)
;; 		     (impc:ir:convert-from-pretty-types (caddr ast))))
;;             ;; this should be fixed point
;;             (b (impc:ti:type-check (cadr ast) vars kts (list *impc:ir:si64* *impc:ir:si32*))))
;; 	;; returns a pointer of type 'a'
;; 	(if (null? a) a
;; 	    (list (impc:ir:pointer++ a))))))


(define impc:ti:void-check
  (lambda (ast vars kts request?)
    (if (> (length ast) 1)
	(print-error 'Compiler 'Error: 'void 'does 'not 'take 'any 'arguments)
	(list *impc:ir:void*))))


(define impc:ti:array-set-check
   (lambda (ast vars kts request?)
     (if (<> (length ast) 4)
	 (print-error 'Compiler 'Error: 'error 'parsing (atom->string (car ast)) 'incorrect 'number 'of 'arguments))
     (let* ((a (impc:ti:type-check (cadr ast) vars kts request?))
	    ;; b should be fixed point types
	    (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si64* *impc:ir:si32*)))
	    ;; c should be of type a*
	    (c (impc:ti:type-check (cadddr ast) vars kts (if (null? a) #f
							     (if (and (not (impc:ir:type? a))
								      (impc:ir:array? (car a)))
								 (list (caddr (car a)))
								 (list (impc:ir:pointer-- (car a))))))))
       (if (or (and (not (null? a))
		    (not (impc:ir:array? (car a))))
	       (> (impc:ir:get-ptr-depth (car a)) 1))	   
	   (print-error 'Compiler 'Error: 'invalid 'aset! 'type (impc:ir:get-type-str (car a))))
       ;; array set check will return the value set
       c)))


(define impc:ti:array-ref-ptr-check
   (lambda (ast vars kts request?)
      (let ((a (impc:ti:type-check (cadr ast) vars kts request?))
            ;; b should be fixed point
            (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si64* *impc:ir:si32*))))
	(if (impc:ir:type? a) (set! a (list a)))	
	(if (null? a) 
	    a
	    (if (or (not (impc:ir:array? (car a)))
		    (> (impc:ir:get-ptr-depth (car a)) 1))
		(print-error 'Compiler 'Error: 'invalid 'array-ref-ptr 'type (impc:ir:get-type-str (car a)) 'in ast)
		(list (impc:ir:pointer++ (caddr (car a)))))))))


(define impc:ti:array-ref-check
   (lambda (ast vars kts request?)
      ;(println 'array-ref-check: 'ast: ast 'vars: vars 'kts: kts)
      (let ((a (impc:ti:type-check (cadr ast) vars kts request?))
            ;; b should be fixed point
            (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si64* *impc:ir:si32*))))
	(if (impc:ir:type? a) (set! a (list a)))
	(if (null? a) 
             a
	     (if (or (not (impc:ir:array? (car a)))
		     (> (impc:ir:get-ptr-depth (car a)) 1))
		 (print-error 'Compiler 'Error: 'invalid 'array-ref 'type (impc:ir:get-type-str (car a)) 'in ast)
		 (list (caddr (car a))))))))


(define impc:ti:pointer-set-check
   (lambda (ast vars kts request?)
     (if (<> (length ast) 4)
	 (print-error 'Compiler 'Error: 'error 'parsing (atom->string (car ast)) 'incorrect 'number 'of 'arguments ': ast))
     (let* ((a (impc:ti:type-check (cadr ast) vars kts request?))
	    ;; b should be fixed point types
	    (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si64* *impc:ir:si32*)))
	    ;; c should be of type a*
	    (c (impc:ti:type-check (cadddr ast) vars kts (if (null? a) #f
							     (if (and (not (impc:ir:type? a))
								      (impc:ir:array? (car a)))
								 (list (caddr (car a)))
								 (list (impc:ir:pointer-- (car a))))))))
       ;; array set check will return the type of the value set
       c)))

(define impc:ti:pointer-ref-ptr-check
   (lambda (ast vars kts request?)
      (let ((a (impc:ti:type-check (cadr ast) vars kts request?))
            ;; b should be fixed point
            (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si64* *impc:ir:si32*))))
	(if (impc:ir:type? a) (set! a (list a)))	
	(if (null? a) 
	    a
	    (if (impc:ir:array? (car a))
		(list (impc:ir:pointer++ (caddr (car a))))
		(list (car a)))))))


(define impc:ti:pointer-ref-check
   (lambda (ast vars kts request?)
      ;(println 'array-ref-check: 'ast: ast 'vars: vars 'kts: kts)
      (let ((a (impc:ti:type-check (cadr ast) vars kts request?))
            ;; b should be fixed point
            (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si64* *impc:ir:si32*))))
	(if (impc:ir:type? a) (set! a (list a)))
	(if (null? a) 
             a
             (if (impc:ir:array? (car a))
		 (list (caddr (car a)))
		 (list (impc:ir:pointer-- (car a))))))))



;; make should be of the form
;; (make type)
;; where type is a valid type
;; (make i64)
;; memory is allocated on the head 
(define impc:ti:heap-alloc-check
   (lambda (ast vars kts request?)
      ;; make should return a ptr to type a
      (let ((a (impc:ir:convert-from-pretty-types (if (< (length ast) 3) (cadr ast) (caddr ast)))))
         ;; returns a pointer of tuple type 'a'
	(if (null? a) a
	    (impc:ir:pointer++ a)))))


;; make should be of the form
;; (make type)
;; where type is a valid type
;; (make i64)
;; memory is allocated on the head 
(define impc:ti:zone-alloc-check
   (lambda (ast vars kts request?)
      ;; make should return a ptr to type a
      (let ((a (impc:ir:convert-from-pretty-types (if (< (length ast) 3) (cadr ast) (caddr ast)))))
         ;; returns a pointer of tuple type 'a'
	(if (null? a) a
	    (impc:ir:pointer++ a)))))


;; make should be of the form
;; (make type)
;; where type is a valid type
;; (make i64)
;; memory is allocated on the stack 
(define impc:ti:stack-alloc-check
   (lambda (ast vars kts request?)
      ;; alloc should return a ptr to type a
      (let ((a (impc:ir:convert-from-pretty-types (if (< (length ast) 3) (cadr ast) (caddr ast)))))
         ;; returns a pointer of tuple type 'a'
	(if (null? a) a
	    (impc:ir:pointer++ a)))))


;; ;; make-tuple should be of the form
;; ;; (make-tuple type type type ...)
;; ;; where types are valid types
;; ;; (make-tuple i64 i8* i32)
;; (define impc:ti:make-tuple-check
;;    (lambda (ast vars kts request?)
;;       ;; make-tuple should return the tuple type a
;;       (let ((a (cons *impc:ir:tuple* (impc:ir:convert-from-pretty-types (cdr ast)))))
;;          ;; returns a pointer of tuple type 'a'
;; 	(if (null? a) a
;; 	    (impc:ir:pointer++ a)))))

(define impc:ti:get-tuple-type-from-name
  (lambda (str)
    (let ((t (llvm:get-named-type (substring (impc:ir:get-base-type (car res))					     
					     1
					     (string-length (impc:ir:get-base-type (car res)))))))
      (dotimes (i (impc:ir:get-ptr-depth str)) (set! t (impc:ir:pointer++ t)))
      t)))

(define impc:ti:tuple-set-check
   (lambda (ast vars kts request?)            
      (if (< (length ast) 4)
          (print-error 'Compiler 'Error: 'missing 'operands 'in (sexpr->string ast)))
      ;; (caddr ast) must be an integer 
      (if (not (integer? (caddr ast))) 
          (print-error 'Compiler 'Error: 'tuple-set! 'must 'use 'a 'literal 'integer 'index! ast))
      (let* (;; a should be a tuple of some kind
             (a (let ((res (impc:ti:type-check (cadr ast) vars kts request?)))
		  (if (and (string? (car res))
			   (char=? (string-ref (car res) 0) #\%))
		      (let ((t (llvm:get-named-type (substring (impc:ir:get-base-type (car res))
							       1
							       (string-length (impc:ir:get-base-type (car res)))))))
			(dotimes (i (impc:ir:get-ptr-depth (car res))) (set! t (impc:ir:pointer++ t)))
			(list (impc:ir:get-type-from-str t)))
		      res)))  
             ;; b should be 32bit fixed point type -- llvm structs only support 32bit indexes
             (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si32*)))
             ;; c should be an element of a tuple
             (c (impc:ti:type-check (cadddr ast) vars kts
                                    (if (and (not (null? a))
                                             (list? a))
                                        (if (impc:ir:tuple? (car a))
                                            (list-ref (car a) (+ 1 (caddr ast)))
                                            #f)
                                        #f))))
	(if (not (null? a))
	    (and (not (impc:ir:tuple? (car a)))
		 (print-error 'Compiler 'Error: 'invalid 'tuple-set-check 'type (impc:ir:get-type-str (car a)))))
	;; tuple set check will return the type of the value set
	c)))


(define impc:ti:tuple-ref-ptr-check
   (lambda (ast vars kts request?)
      ;; (caddr ast) must be an integer    
      (if (not (integer? (caddr ast))) 
          (print-error 'Compiler 'Error: 'tuple-ref 'must 'use 'a 'literal 'integer 'index! ast))
      (let* (; a should be a tuple of some kind!
            (a (impc:ti:type-check (cadr ast) vars kts (if (impc:ir:type? request?)
							   (impc:ir:tuple? request?)
							   request? 
							   #f))) ;request?))
            ;; b should be fixed point -- llvm structs only support 32bit indexes
            (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si32*))))
	(if (impc:ir:type? a)
	    (set! a (list a)))
	;; check for named types
	(if (and (not (null? a)) (string? (car a)))
	    (let ((t (impc:ir:get-type-from-str (llvm:get-named-type (impc:ir:clean-named-type (car a)))))
		  (ptr-level (impc:ir:get-ptr-depth (car a))))
	      (dotimes (i ptr-level) (set! t (impc:ir:pointer++ t)))
	      (set! a (list t))))	
	;(println 'tupref-check 'a: a 'ast: ast (list-ref (car a) (+ 1 (caddr ast))))
	(if (and (not (null? a))
		 (list? a)
		 (impc:ir:tuple? (car a)))
	    (list (impc:ir:pointer++ (list-ref (car a) (+ 1 (caddr ast)))))
	    ;;'()))))
	    (if (null? a)
		'()
		(print-error 'Compiler 'Error: 'invalid 'tuple-ref-ptr 'type (impc:ir:get-type-str (car a))))))))


(define impc:ti:tuple-ref-check
   (lambda (ast vars kts request?)
     ;(println 'ref-check ast kts vars)
      ;; (caddr ast) must be an integer    
      (if (not (integer? (caddr ast))) 
          (print-error 'Compiler 'Error: 'tuple-ref 'must 'use 'a 'literal 'integer 'index! ast))
      (let* (; a should be a tuple of some kind!
            (a (impc:ti:type-check (cadr ast) vars kts (if (impc:ir:type? request?)
							   (impc:ir:tuple? request?)
							   request? 
							   #f))) ;request?))
            ;; b should be fixed point -- llvm structs only support 32bit indexes
            (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si32*))))
	(if (impc:ir:type? a)
	    (set! a (list a)))
	;; check for named types
	(if (and (not (null? a))
		 (string? (car a)))
	    (let ((t (impc:ir:get-type-from-str (llvm:get-named-type (impc:ir:clean-named-type (car a)))))
		  (ptr-level (impc:ir:get-ptr-depth (car a))))
	      (dotimes (i ptr-level) (set! t (impc:ir:pointer++ t)))
	      (set! a (list t))))
	;(println 'tupref-check 'a: a 'ast: ast (list-ref (car a) (+ 1 (caddr ast))))
	(if (and (not (null? a))
		 (list? a)
		 (impc:ir:tuple? (car a)))
	    (begin (if (>= (caddr ast) (- (length (car a)) 1))
		       (print-error 'Compiler 'Error: 'tuple 'index 'beyond 'type 'boundary ast))	    
		   (list-ref (car a) (+ 1 (caddr ast))))
	    (if (null? a)
		'()
		(print-error 'Compiler 'Error: 'invalid 'tuple-ref 'type (impc:ir:get-type-str (car a))))))))
;	    '()))))


;;(closure-set! closure a i32 5)
(define impc:ti:closure-set-check
   (lambda (ast vars kts request?)   
     (println 'ast: ast)
      (if (< (length ast) 5)
          (print-error 'Compiler 'Error: 'missing 'operands 'in (sexpr->string ast)))
      (let* (;; a should be a closure of some kind
             (a (if (and (symbol? (cadr ast))
                         (llvm:get-function (symbol->string (cadr ast))))
                    #t ; // yes (cadr ast) is a globally defined closure
                    (impc:ti:type-check (cadr ast) vars kts request?)))
             ;; b should be a string (the var's name)
             (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si8*)))
             ;; c should be a string (the type of b)
             (c (impc:ti:type-check (cadddr ast) vars kts (list *impc:ir:si8*)))
             ;; d should be a value of type c
             (d (impc:ti:type-check (car (cddddr ast)) vars kts (impc:ir:get-type-from-str (cadddr ast)))))
         ;; should return the type requested (i.e. c)
         (impc:ir:get-type-from-str (cadddr ast)))))

;;(closure-ref closure a i32)
(define impc:ti:closure-ref-check
   (lambda (ast vars kts request?)     
      (if (< (length ast) 4)
          (print-error 'Compiler 'Error: 'missing 'operands 'in (sexpr->string ast)))
      (let* (;; a should be a closure of some kind
             (a (if (and (symbol? (cadr ast))
                         (llvm:get-function (symbol->string (cadr ast))))
                    #t ; // yes (cadr ast) is a globally defined closure
                    (impc:ti:type-check (cadr ast) vars kts request?)))
             ;; b should be a string (the var's name)
             (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si8*)))
             ;; c should be a string (the type of b)
             (c (impc:ti:type-check (cadddr ast) vars kts (list *impc:ir:si8*))))
         ;; should return the type requested (i.e. c)
         (impc:ir:get-type-from-str (cadddr ast)))))


;;(closure-set! closure a i32 5)
(define impc:ti:closure-set-check
   (lambda (ast vars kts request?)   
      ;(println 'cset 'ast: ast 'request? request?)
      (if (< (length ast) 4)
          (print-error 'Compiler 'Error: 'missing 'operands 'in (sexpr->string ast)))
      (let* (;; a should be a closure of some kind
             (a (if (and (symbol? (cadr ast))
                         (llvm:get-function (symbol->string (cadr ast))))
                    #t ; // yes (cadr ast) is a globally defined closure
                    (impc:ti:type-check (cadr ast) vars kts request?)))
             ;; b should be a string (the var's name)
             (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si8*)))
	     ;; c should be a value for var's name
	     (c (impc:ti:type-check (cadddr ast) vars kts 
				    (if (null? (car (cddddr ast)))
					request?
					(impc:ir:get-type-from-str (car (cddddr ast)))))))
	c)))

;;(closure-ref closure a i32)
(define impc:ti:closure-ref-check
   (lambda (ast vars kts request?)
     ;(println 'cls 'ref 'check: ast 'request? request?)
      (if (< (length ast) 3)
          (print-error 'Compiler 'Error: 'missing 'operands 'in (sexpr->string ast)))
      (let* (;; a should be a closure of some kind
             (a (if (and (symbol? (cadr ast))
                         (llvm:get-function (symbol->string (cadr ast))))
                    #t ; // yes (cadr ast) is a globally defined closure
                    (impc:ti:type-check (cadr ast) vars kts request?)))
             ;; b should be a string (the var's name)
             (b (impc:ti:type-check (caddr ast) vars kts (list *impc:ir:si8*))))
	(if (null? (cadddr ast))
	    (if request?
		request?
		'())
	    (impc:ir:get-type-from-str (cadddr ast))))))


(define impc:ti:set-check
   (lambda (ast vars kts request?)      
      (let* ((sym (impc:ti:get-var (cadr ast) vars))
             (a (impc:ti:type-check (caddr ast) vars kts (cdr sym))))
         (if *impc:ti:print-sub-checks* (println 'set!:> 'ast: ast 'a: a))
	 ;; if sym is not a global var then add return type to sym
	 (if (assoc (car sym) vars) 
	     (impc:ti:update-var (car sym) vars kts a))
         a)))

(define impc:ti:pdref-check
   (lambda (ast vars kts request?)      
      (let* ((a (impc:ti:type-check (cadr ast) vars kts request?)))
         (if *impc:ti:print-sub-checks* (println 'ptrref:> 'ast: ast 'a: a))
	 ;; return type of ptrref is 'a' dereferenced
	 (if (list? a)
	     (set! a (car a)))
	 (if (and (impc:ir:type? a)
		  (impc:ir:pointer? a))
	     (impc:ir:pointer-- a)
	     (print-error 'Compiler 'Error: 'ptrref 'takes 'a 'pointer 'argument 'not a)))))
      

(define impc:ti:pref-check
  (lambda (ast vars kts request?)      
    (let* ((a (impc:ti:type-check (cadr ast) vars kts request?)))
      (if *impc:ti:print-sub-checks* (println 'ptrref:> 'ast: ast 'a: a))
      ;; return type of ptrref is 'a' referenced
      (if (list? a)
	  (set! a (car a)))
      (if (and (impc:ir:type? a)
	       (impc:ir:pointer? a))
	  (impc:ir:pointer++ a)
	  (print-error 'Compiler 'Error: 'ptrref 'takes 'a 'pointer 'argument 'not a)))))


(define impc:ti:lambda-check
   (lambda (ast vars kts request?)
     ;(println 'lcheck: 'ast: ast 'request? request? 'vars: vars 'kts: kts)
     ;; first we check if a type request has been made
     (if (and (impc:ir:type? request?)
	      (impc:ir:closure? request?))
	        ;; if there is a request then cycle through 
	        ;; and set lambda arg symbols
	 (begin (map (lambda (sym req)
		       (if (symbol? sym)
			   (impc:ti:update-var sym vars kts req)))
		     (cadr ast)
		     (cddr request?))
		;; finally set request? to the return type
		(set! request? (cadr request?))))
      ;; run body for type coverage     
      ;; grab the last result as return type
      (let ((res (impc:ti:type-check (caddr ast) vars kts request?)))
         ;; if we have a choice between numeric options we force one!
         (if (and (list? res)
		  (> (length res) 1)
		  (not (member #f (map (lambda (t) (impc:ir:floating-point? t)) res))))
             (set! res (list *impc:ir:double*))) ;; force doubles
         (if (and (list? res)
		  (> (length res) 1)
		  (not (member #f (map (lambda (t) (impc:ir:fixed-point? t)) res))))
             (set! res (list *impc:ir:si64*))) ;; force i64
         ;; return lambda type which is made up of
         ;; argument symbols plus return type from last body expression         
         (let ((ret (list (impc:ir:pointer++ (impc:ir:pointer++ (list* *impc:ir:closure* res (cadr ast)))))))
            ;(print 'return 'ret: ret 'from 'ast ast) 
            ret))))
			

;; whenever a closure is called we calculate a type for it
;; at the end these possibly multiple views should unify!
(define impc:ti:closure-call-check
   (lambda (ast vars kts request?)
      ;(println 'cchint 'ast: ast 'vars: vars 'request: request?)      
      ;; otherwise we need to try to find a type definition for the closure      
      (let* ((ctype (if (assoc (car ast) vars)
                        (cdr (assoc (car ast) vars))
                        (if (llvm:get-globalvar (symbol->string (car ast)))
                            (list (impc:ir:get-type-from-str (llvm:get-global-variable-type (symbol->string (car ast)))))
                            (print-error 'Compiler 'Error: 'no 'closure 'named: (car ast)))))
             ;; get argument expression types
             (res (map (lambda (e t)
                          (let ((res (impc:ti:type-check e vars kts
                                                         (if (symbol? t) 
                                                             (impc:ti:symbol-check t vars kts #f)
                                                             t))))
                             ;; if t is a symbol then add res to t
                             (if (symbol? t) (impc:ti:update-var t vars kts res))
                             res))
                       (cdr ast)
                       (if (or (null? ctype) 
                               (not (impc:ir:closure? (car ctype))))
                           (make-list (length (cdr ast)) #f)
                           ;; if we are using an existing definition then check arity
                           (if (<> (length (cddr (car ctype)))
                                   (length (cdr ast)))
                               (print-error 'Compiler 'Error: 'bad 'arity 'for ast)
                               (cddr (car ctype))))))
             ;; if there was a request that will be the return type
             ;; otherwise if we already have a type defined we can use it's return type
             ;; otherwise we cannot know it
             (ret (if (and request?
                           (not (null? request?)))
                      request?
                      (if (or (null? ctype) 
                              (not (impc:ir:closure? (car ctype))))
                          '()
                          (cadr (car ctype))))))         
         (if *impc:ti:print-sub-checks* (println 'closure:> 'ast: ast 'res: res 'ret: ret))
         ; set the closure type for the symbol (if not a global var)
         (if (assoc (car ast) vars)
             (impc:ti:update-var (car ast) vars kts
                                 (list (impc:ir:pointer++ (impc:ir:pointer++ (list* *impc:ir:closure* ret res))))))
         ; and return the new closure's return type
         (if (list? ret) ret
             (list ret)))))
			 

(define impc:ti:dotimes-check
   (lambda (ast vars kts request?)
      (let ((a (impc:ti:type-check (cadr (cadr ast)) vars kts 
                                   (list *impc:ir:double* *impc:ir:float* *impc:ir:si64* *impc:ir:si32* *impc:ir:ui8*))))
         ;; if numeric? and multiple choice - then force a type!
         (if (number? (cadr (cadr ast)))
             (cond ((equal? (cl:sort a <)
                            (cl:sort (list *impc:ir:double* *impc:ir:float* *impc:ir:si64* *impc:ir:si32* *impc:ir:ui8*) <))
                    (set! a (list *impc:ir:si64*)))
                   ((equal? (cl:sort a <) 
                            (cl:sort (list *impc:ir:double* *impc:ir:float*) <))
                    (set! a (list *impc:ir:double*)))
                   (else 'leave-a-alone)))                                   
         ;; (car (cadr ast)) should be a symbol that we want to update with a
         (if (not (symbol? (car (cadr ast)))) (print-error 'Compiler 'Error: 'bad 'form 'for 'dotimes. 'Needs 'a 'symbol ast))
         (impc:ti:update-var (car (cadr ast)) vars kts a) 
         ;; stretch over body code but don't worry about return types
         (impc:ti:type-check (caddr ast) vars kts #f)
         ;; dotimes returns void
         (list *impc:ir:void*))))


(define impc:ti:printf-check
   (lambda (ast vars kts request?)
      (let ((a (impc:ti:type-check (cadr ast) vars kts (list (+ *impc:ir:si8* *impc:ir:pointer*)))))
         ;; run through everything else for completeness but don't care about the results
         (for-each (lambda (x) (impc:ti:type-check x vars kts #f)) (cddr ast))
         ;; printf returns i32
         (list *impc:ir:si32*))))

(define impc:ti:sprintf-check
   (lambda (ast vars kts request?)
      (let ((a (impc:ti:type-check (cadr ast) vars kts (list (+ *impc:ir:si8* *impc:ir:pointer*))))
	    (b (impc:ti:type-check (caddr ast) vars kts (list (+ *impc:ir:si8* *impc:ir:pointer*)))))
         ;; run through everything else for completeness but don't care about the results
         (for-each (lambda (x) (impc:ti:type-check x vars kts #f)) (cdddr ast))
         ;; printf returns i32
         (list *impc:ir:si32*))))

(define impc:ti:string-check
   (lambda (ast vars kts request?)
      (if (string? ast)
          (list (+ *impc:ir:si8* *impc:ir:pointer*))
          '())))


(define impc:ti:carcdr-check
   (lambda (ast vars kts request?)
      ;; check that we are getter a pair as an argument
      (impc:ti:type-check (cadr ast) vars kts (list (impc:ir:pointer++ *impc:ir:pair*)))
      ;; don't do anything about return type yet
      '()))

(define impc:ti:coerce-check
   (lambda (ast vars kts request?)
      (impc:ti:type-check (cadr ast) vars kts #f)
      (list (caddr ast))))

(define impc:ti:closure-in-first-position
   (lambda (ast vars kts request?)
      ;; first check return type of car ast (which will be a closure)
      ;; then check against it's arg types
      (let ((type (impc:ti:type-check (car ast) vars kts request?)))
        ;(println 'closure-in-first-pos: ast 'type: type)
	(if (not (impc:ir:type? type))
	    (set! type (car type)))
	(if (<> (+ *impc:ir:closure* *impc:ir:pointer* *impc:ir:pointer*) (car type))
	    (begin (print-error 'Invalid 'Expression ast) (error ""))
	    (begin (map (lambda (a b) 
			  (impc:ti:type-check b vars kts a))
			(cddr type)
			(cdr ast))
		   (cadr type))))))	  

;; vars is statefull and will be modified in place
(define impc:ti:type-check
   (lambda (ast vars kts request?)
      ;(println 'type-check: ast  'vars: vars  'kts: kts)
      (if *impc:ti:print-main-check* (println 'type-check: ast 'kts: kts 'request? request?))
      (if *impc:ti:print-main-check* (println 'vars------: vars))
      (cond ((null? ast) '())
            ((and (atom? ast) (number? ast)) (impc:ti:numeric-check ast vars kts request?))
            ((and (atom? ast) (symbol? ast)) (impc:ti:symbol-check ast vars kts request?)) ;#f)) ;request?))
            ((and (atom? ast) (string? ast)) (impc:ti:string-check ast vars kts request?))
            ((atom? ast) (print-error 'Compiler 'Error: 'internal 'error 'unhandled 'atom: ast))
            ((and (list? ast) (member (car ast) '(let let* letrec))) (impc:ti:let-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(lambda))) (impc:ti:lambda-check ast vars kts request?))
	    ((and (list? ast) (member (car ast) '(* / + - modulo bitwise-and bitwise-or bitwise-eor bitwise-shift-left bitwise-shift-right bitwise-not))) (impc:ti:math-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(< > = <>))) (impc:ti:compare-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(dotimes))) (impc:ti:dotimes-check ast vars kts request?))            
            ((and (list? ast) (member (car ast) '(llvm_printf))) (impc:ti:printf-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(memzone))) (impc:ti:memzone-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(callback))) (impc:ti:callback-check ast vars kts request?))	    
            ((and (list? ast) (member (car ast) '(llvm_sprintf))) (impc:ti:sprintf-check ast vars kts request?))
            ;((and (list? ast) (member (car ast) '(make-array))) (impc:ti:make-array-check ast vars kts request?))  
            ((and (list? ast) (member (car ast) '(array-set!))) (impc:ti:array-set-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(array-ref))) (impc:ti:array-ref-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(array-ref-ptr))) (impc:ti:array-ref-ptr-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(pointer-set!))) (impc:ti:pointer-set-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(pointer-ref))) (impc:ti:pointer-ref-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(pointer-ref-ptr))) (impc:ti:pointer-ref-ptr-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(stack-alloc))) (impc:ti:stack-alloc-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(heap-alloc))) (impc:ti:heap-alloc-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(zone-alloc))) (impc:ti:zone-alloc-check ast vars kts request?))	    	    
            ;((and (list? ast) (member (car ast) '(make-tuple))) (impc:ti:make-tuple-check ast vars kts request?)) 
            ((and (list? ast) (member (car ast) '(tuple-set!))) (impc:ti:tuple-set-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(tuple-ref))) (impc:ti:tuple-ref-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(tuple-ref-ptr))) (impc:ti:tuple-ref-ptr-check ast vars kts request?))	    
            ((and (list? ast) (member (car ast) '(closure-set!))) (impc:ti:closure-set-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(closure-ref))) (impc:ti:closure-ref-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(pref))) (impc:ti:pref-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(pdref))) (impc:ti:pdref-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(null?))) (impc:ti:null?-check ast vars kts request?))
	    ((and (list? ast) (member (car ast) '(impc_null))) (impc:ti:null-check ast vars kts request?))
	    ((and (list? ast) (member (car ast) '(bitcast))) (impc:ti:bitcast-check ast vars kts request?))
	    ((and (list? ast) (member (car ast) '(void))) (impc:ti:void-check ast vars kts request?))
            ((and (list? ast) ;; polymorphic function
                  (symbol? (car ast))
		  (regex:match? (symbol->string (car ast)) "\\$\\$\\$")
                  (impc:ir:poly-types (string->symbol (car (regex:split (symbol->string (car ast)) "\\$\\$\\$")))))
             (let ((reses (impc:ti:nativef-poly-check ast vars kts request?)))
	       ;(println 'ast: ast reses)
	       reses))
            ((and (list? ast) ;; native function
                  (symbol? (car ast))
                  (llvm:get-function (symbol->string (car ast))))
             (impc:ti:nativef-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(begin))) (impc:ti:begin-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(if ifret))) (impc:ti:if-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(set!))) (impc:ti:set-check ast vars kts request?))
            ((and (list? ast) (member (car ast) '(ret->))) (impc:ti:ret-check ast vars kts request?))
            ((and (list? ast) (assoc (car ast) vars)) (impc:ti:closure-call-check ast vars kts request?))    
            ((and (list? ast) (list? (car ast))) (impc:ti:closure-in-first-position ast vars kts request?))
            ((and (list? ast)  ;; this is here to check against closures as global vars (i.e. not in local environment)
                  (symbol? (car ast))
		  (llvm:get-globalvar (symbol->string (car ast)))
                  (impc:ir:closure? (impc:ir:get-type-from-str (llvm:get-global-variable-type (symbol->string (car ast))))))
             (impc:ti:closure-call-check ast vars kts request?))
            (else (impc:ti:join (impc:ti:type-check (car ast) vars kts request?)
                                (impc:ti:type-check (cdr ast) vars kts request?))))))


(define impc:ti:find-unresolved-simple-types
   (lambda (union)
      (let ((unresolved (cl:remove #f (map (lambda (x) ;; return the first bad variable that is not a closure
                                              (if (null? (cdr x)) #f
                                                  (if (and (list? (cdr x)) ;; check there are multiple choices
                                                           (not (member (modulo (cadr x) *impc:ir:pointer*) 
                                                                        (list *impc:ir:tuple* *impc:ir:closure* *impc:ir:array*))) ;; make sure it's a base type (not closure or tuple)
                                                           (cl:every impc:ir:type? (cdr x))) ;; check that it's choices are valid (not null)
                                                      x #f)))
                                           union))))
         (if (null? unresolved) #f
             unresolved))))


;; run the type checker
;; if we fail to unify completely the first time
;; try some possible substitutions!
(define impc:ti:run-type-check
   (lambda (vars forced-types ast . cnt)
      ;(println '------------------------------------)
      ;(println 'forced-types forced-types)
      ;(println 'vars: vars)
      ;(println 'ast: ast)
      ;(if (null? cnt) (sys:clear-log-view))
      (let* ((fvars (map (lambda (t) ;; add any forced-type values to vars
                            (if (assoc (car t) forced-types)
                                (let ((tt (cdr (assoc (car t) forced-types))))
                                   (cons (car t) (list tt)))
                                t))
                         vars))
             (kts (map (lambda (x) (car x)) forced-types)) ;; just need symbols from forced-types
             (ret (impc:ti:type-check ast fvars kts #f))
             (u (impc:ti:unify fvars))
             (t (impc:ti:unity? u))
             (tt (cl:every (lambda (x) x) t))
             (a (if tt #t
                    (impc:ti:find-unresolved-simple-types u))))
         (if *impc:ti:print-unifications* (println 'tirun:> a '-> u))
         ;; if we have unified types then return them otherwise run through options!
         (if tt 
             u
             (if (or (not (null? cnt)) ;; else if we're not on the first run die
                     (not a))
                 (if a a u)
                 ;; if we are on the first run then attempt substitutions
                 ;; we grab the first unresolved simple type and start
                 ;; testing each of it's options in turn
                 ;; also attempting to unify that choice with all other
                 ;; unresolved simple types before each run
                 (let ((res (map (lambda (x)
                                    ;; first clear vars
                                    (impc:ti:clear-all-vars fvars)
                                    (impc:ti:run-type-check fvars 
                                                            ;; as well as all simple types which have resolved fully
                                                            (append (cl:remove-if-not (lambda (z) (and (not (list? z))
                                                                                                       (pair? z))) u)
                                                                    ;; and any simple types that unify on x
                                                                    (cl:remove 'failed 
                                                                               (map (lambda (k)
                                                                                       (if (null? (cl:intersection (list x) (cdr k)))
                                                                                           'failed
                                                                                           (cons (car k) x)))
                                                                                    (impc:ti:find-unresolved-simple-types u)))
                                                                    forced-types)
                                                            ast 1))
                                 (cdr (car a)))))
                    (let ((r (cl:find-if (lambda (x) 
                                            (cl:every (lambda (x) x) (impc:ti:unity? x)))
                                         res)))
		      ;; old version below as backup
                      (if (not r) 
                          (apply print-error 'Compiler 'Error: 'could 'not 'resolve 'types:
				 (map (lambda (x) (symbol->string (car x)))
				      (cl:remove-if (lambda (x)
						      (impc:ir:type? (cdr x)))
						    (car res))))
                          (car res))
                      r)))))))

            
;;
;; 
;; Other utility code
;;
;;


;; add types to source
;; also add clrun for closure application
;; and inject polymorphic functions
(define impc:ti:add-types-to-source
   (lambda (symname ast types envvars . prev)
      ;(println 'symname: symname 'envvars: envvars 'ast: ast 'prev: prev)
      (if (atom? ast) ast
          (cond ((equal? (car ast) 'make-closure)
                 (list (car ast)
		       (cadr ast)
                       ;; global name
                       (string-append (symbol->string symname) "__" (number->string (llvm:count++)))
                       (if (null? prev) ;; this adds return type
                           *impc:ir:other*
                           (caddr (assoc (car prev) types))) 
                       (map (lambda (v) ;; environment types
                               (if (member v envvars)
                                   (let ((p (assoc v types)))
                                      (cons (string->symbol (string-append (symbol->string (car p)) "__sub"))
                                            (cdr p)))
                                   (assoc v types)))
                            (cons symname (caddr ast)))
                       (map (lambda (v) ;; argument types
                               (assoc v types))
                            (cadddr ast))
                       (impc:ti:add-types-to-source symname (car (cddddr ast)) types (append envvars (caddr ast)))))
                ((equal? (car ast) 'clrun->)
                 (list* (car ast)
                        (cadr ast)
                        (map (lambda (arg type)
                                ;(print 'clrunargs-> arg type)
                                (let ((a (impc:ti:add-types-to-source symname arg types envvars ast)))
                                   (if (null? type) 
                                       (print-error 'Compiler 'Error: 'cannot 'infer 'closure 'type 'for 
                                                    (symbol->string (cadr ast)))
                                       a)))
                             (cddr ast)
                             (cdddr (if (not (assoc (cadr ast) types)) ;; if not in local env then get types from global var
                                        (cons (cadr ast) (impc:ir:get-type-from-str (llvm:get-global-variable-type (symbol->string (cadr ast)))))
                                        (assoc (cadr ast) types))))))
		;; ;; inject ad-hoc polymorphism
		;; ((impc:ir:poly-types (car ast))
		;;  ;(println 'bingo: ast)
		;;  (let* ((vars (cl:tree-copy types))
		;; 	;(llll (println 'vars: vars))
		;; 	(kts (map (lambda (t) (car t)) types))
		;; 	;(lllll (println 'kts: kts))
		;; 	(polyargs (map (lambda (arg)
		;; 			 ;(println 'arg: arg)
		;; 			 (let ((res (impc:ti:type-check arg vars kts #f)))
		;; 			   ;(println 'res: res)
		;; 			   (if (and (list? res)
		;; 				    (> (length res) 1))
		;; 			       res
		;; 			       ;(print-error 'Compiler 'Error: 'unable 'to 'resolve 'polymorphic 'function: ast)
		;; 			       (if (atom? res) res (car res)))))
		;; 		       (cdr ast)))
		;; 	;(lllllll (println 'polyartgs: polyargs))
		;; 	(retargs (impc:ti:type-check ast vars kts #f))
		;;         ;(lllllllllll (println 'retargs: retargs 'prev: (cdr (car prev))))
		;; 	(rets (if (null? prev)
		;; 		  retargs
		;; 		  retargs))) ;(impc:ti:type-check (cdr (car prev)) vars kts retargs))))
		;;    ;;(println 'polyargs: polyargs 'retargs: retargs 'rets: rets 'prev: prev)
		;;    (let* ((polys (map (lambda (pret)
		;; 			(let* ((polyf (cons (+ *impc:ir:closure*
		;; 					       *impc:ir:pointer*
		;; 					       *impc:ir:pointer*)
		;; 					    (cons pret polyargs)))
		;; 			       (polyfunc (impc:ir:check-poly (car ast) polyf)))
		;; 			  ;(println 'polyf: polyf 'polyfunc: polyfunc)
		;; 			  polyfunc))
		;; 		      rets))
		;; 	  (validpolys (remove #f polys)))
		;;      ;(println 'all: polys 'polys: validpolys symname (cdr ast))
		;;      (let ((rr (impc:ti:add-types-to-source symname (cdr ast) types envvars)))
		;;        ;; take the first valid polymorphic dispatch
		;;        (cons (car validpolys) rr)))))
		;; inject polymorphic functions
		((and (symbol? (car ast))
		      (regex:match? (symbol->string (car ast)) "\\$\\$\\$"))
		 (let* ((pname (string->symbol (car (regex:split (symbol->string (car ast)) "\\$\\$\\$"))))
			(type (cdr (assoc (car ast) types)))
			(polyname (impc:ir:check-poly pname type)))
		   ;(println 'pname: pname 'type: type 'polyname: polyname)
		   (cons polyname (impc:ti:add-types-to-source symname (cdr ast) types envvars))))
                ((member (car ast) '(make-env make-env-zone))
                 (list (car ast)
		       (cadr ast)
                       (map (lambda (p)
                               (list (assoc (car p) types)
                                     (impc:ti:add-types-to-source symname (cadr p) types envvars (car p))))
                            (caddr ast))
                       (impc:ti:add-types-to-source symname (cadddr ast) types envvars)))
                ((or (and (assoc (car ast) types)
                          (impc:ir:closure? (cdr (assoc (car ast) types))))
                     (and (not (list? (car ast)))
			  (symbol? (car ast))
                          (llvm:get-globalvar (symbol->string (car ast)))
                          (impc:ir:closure? (llvm:get-global-variable-type (symbol->string (car ast))))))
                 (impc:ti:add-types-to-source symname (cons 'clrun-> ast) types envvars))   
                ((list? ast)
                 (map (lambda (x)
                         (impc:ti:add-types-to-source symname x types envvars ast))
                      ast))
                (else (cons (apply impc:ti:add-types-to-source symname (car ast) types envvars)
                            (apply impc:ti:add-types-to-source symname (cdr ast) types envvars)))))))


;; this is uggglly and needs to be redone!!!!!!!
;; adds ret tags
(define impc:ti:mark-returns
   (lambda (ast name in-body? last-pair? blocked?)
      (cond ((atom? ast) 
             (if (and in-body? last-pair?) 
                 (if blocked? ast (list 'ret-> name ast)) 
                 ast))
            ((pair? ast)
             (cond ((equal? (car ast) 'if)
                    ;; if statement need special syntax adjustments for returns
                    (append (if blocked? (list 'if) (list 'ifret)) (list (cadr ast))
                            (list (impc:ti:mark-returns (caddr ast) name in-body? last-pair? blocked?))
                            (if (not (null? (cdddr ast)))
                                (list (impc:ti:mark-returns (cadddr ast) name in-body? last-pair? blocked?)))))
                   ((member (car ast) '(let* let letrec))
                    (append (list (car ast))
                            (list (map (lambda (a)
                                          ;; let assigns always block (lambda can override but nothing else)
                                          (list (car a) (impc:ti:mark-returns (cadr a) (car a) #f #f #t)))
                                       (cadr ast)))
                            (impc:ti:mark-returns (cddr ast) name #t #f blocked?)))
                   ((member (car ast) '(lambda))
                    (append '(lambda) (list (cadr ast))
                            ;; lambda always unblocks because lambdas always need a return
                            (impc:ti:mark-returns (cddr ast) name #t #f #f)))
                   ;((equal? (car ast) 'dotimes)
                   ; (append '(dotimes) (list (cadr ast)) (impc:ti:mark-returns (cddr ast) name #t #f blocked?)))                   
                   ((equal? (car ast) 'begin) 
                    (let* ((rev (reverse (cdr ast)))
                           (last (car rev))
                           (rest (reverse (cdr rev)))
                           (newast (append '(begin) 
                                           (append (map (lambda (a)
                                                           ;; block everything except ...
                                                           (impc:ti:mark-returns a name in-body? #f #t))
                                                       rest)
                                                   ;; the last one which we let through
                                                   ;; ONLY if it hasn't been blocked higher up!
                                                   (list (impc:ti:mark-returns last name in-body? 
                                                                               (if blocked? #f #t) 
                                                                               blocked?))))))
                       newast))
                   ((equal? (car ast) 'begin) 
                    (append '(begin) (impc:ti:mark-returns (cdr ast) name in-body? #f blocked?)))      
                   ((and in-body? last-pair? (not blocked?)) ;; if everything is good add a return!
                    (list 'ret-> name (cons (car ast) (impc:ti:mark-returns (cdr ast) name in-body? #f #t))))
		    ;(list 'ret-> name ast))                                 
                   (else (cons (impc:ti:mark-returns (car ast) name in-body? #f blocked?)
                               (impc:ti:mark-returns (cdr ast) name in-body? #f blocked?))))))))


;; this is a dodgy flatten :(
(define impc:ti:flatten-1
   (lambda (lst)
      (cond ((null? lst) '())
            ((list? (car lst))
             (append (car lst) (impc:ti:flatten-1 (cdr lst))))
            (else (list lst)))))


;; find all free vars
;; currently we don't allow shadow vars
(define impc:ti:find-all-vars
   (lambda (full-ast syms)
     (letrec ((f (lambda (ast)
		   (cond ((pair? ast)
			  (cond ((and (symbol? (car ast))
				      (regex:match? (symbol->string (car ast)) "\\$\\$\\$")
				      (impc:ir:poly-types (string->symbol (car (regex:split (symbol->string (car ast)) "\\$\\$\\$")))))
				 (set! syms (append (list (car ast)) syms))
				 (f (cdr ast)))
				((equal? (car ast) 'make-closure)
				 (if (not (null? (cl:intersection (cadddr ast) syms)))
				     (print-error 'Compiler 'Error: 'Sorry 'single 'definition 'variables 'only! 'caught 'trying 'to 'redefine (symbol->string (car (cl:intersection (caddr ast) syms))) 'as 'a 'shadow 'variable))			      
				 (set! syms (cl:remove-duplicates (append (caddr ast) (cadddr ast) syms)))
				 (f (car (cddddr ast))))
				((member (car ast) '(make-env make-env-zone))
				 (set! syms
				       (append (map (lambda (p)
						      (if (member (car p) syms)
							  (print-error 'Compiler 'Error: 'Sorry 'single 'definition 'variables 'only! 'caught 'trying 'to 'redefine (symbol->string (car p)) p 'as 'a 'shadow 'variable))
						      (car p))
						    (caddr ast))
					       syms))
				 (for-each (lambda (p)
					     (f (cadr p)))
					   (caddr ast))					     
				 (f (cadddr ast)))
				(else (f (car ast))
				      (f (cdr ast)))))
			 ((atom? ast) '())))))
       (f full-ast)
       syms)))



(define impc:ti:block:check-for-free-syms
   (lambda (ast esyms)
      ;(print 'check: 'ast: ast 'esyms: esyms) 
      (cl:remove-duplicates (let loop ((lst ast))
                               (cond ((pair? lst)
                                      (append (loop (car lst))
                                              (loop (cdr lst))))
                                     ((atom? lst)
                                      (if (member lst esyms)
                                          (list lst)
                                          '())))))))

;;
;; adds make-closure and make-env tags
;;

(define impc:ti:allocate-var?
  (lambda (ast)
    (cond ((null? ast) #f)
	  ((eq? ast 'lambda) #t)
	  ((pair? ast)
	   (or (impc:ti:allocate-var? (car ast))
	       (impc:ti:allocate-var? (cdr ast))))
	  (else #f))))

;; adds make-closure and make-env tags
(define impc:ti:closure:convert
   (lambda (ast esyms)
      (cond ((pair? ast)
             (if (equal? (car ast) 'lambda)
                 (let (;(env (impc:ti:block:check-for-free-syms ast esyms))
		       (allocate-mem-for-vars? (impc:ti:allocate-var? (cdr ast))))
		   (list 'make-closure allocate-mem-for-vars? 
			 ;; name of compiled function is always last 
			 ;; so we can remove it by dropping it off the end
			 (cdr (reverse (cl:remove-duplicates esyms))) ;env 
			 (cadr ast)
			 (impc:ti:closure:convert (caddr ast) (append (cadr ast) esyms))))
                 (if (member (car ast) '(let let* letrec))
                     (let* ((allocate-mem-for-vars? (impc:ti:allocate-var? ast))
                            (bindings (map (lambda (binding) 
					     (car binding))
					   (cadr ast))))
		       ;(free-syms (impc:ti:block:check-for-free-syms (cddr ast) (append bindings esyms))))
		       (cons 'make-env
			     (cons allocate-mem-for-vars?
				   (list (impc:ti:closure:convert (cadr ast) (append bindings esyms))
					 (impc:ti:closure:convert (caddr ast) (append bindings esyms))))))
                     (cons (impc:ti:closure:convert (car ast) esyms)
                           (impc:ti:closure:convert (cdr ast) esyms)))))
            ((atom? ast) ast))))





;; expects t1 (i.e. original untransformed code)
(define impc:ti:get-closure-arg-symbols
   (lambda (closure-sym ast)
      ;(print 'ast: ast)
      (cond ((null? ast) '())
            ((atom? ast) '())
            ((vector? ast) '())
            ((and (pair? ast)
                  (eq? (car ast) closure-sym))
             (if (and (not (null? (cdr ast)))
                      (list? (cadr ast))
                      (eq? (caadr ast) 'lambda))
                 (cadr (cadr ast))
                 '()))
            (else (append (impc:ti:get-closure-arg-symbols closure-sym (car ast))
                          (impc:ti:get-closure-arg-symbols closure-sym (cdr ast)))))))    


(define impc:ti:handle-forced-types
   (lambda (t1 . args)
      (if (null? args) '()
          (let* ((forced-types (map (lambda (t)
                                       (map (lambda (tt)
                                               (if (not (or (symbol? tt)
                                                            (list? tt)))
                                                   (print-error 'Compiler 'Error: 'Bad 'fixed 'type t)))
                                            (if (list? t) (cdr t) (list (cdr t))))
                                       (cons (car t) (impc:ir:convert-from-pretty-types (cdr t)))) 
                                    args))
                 (forced-types-updated (apply append (list) 
                                              (map (lambda (t)
                                                      (if (impc:ir:closure? (cdr t))
                                                          (append (map (lambda (sym type)
                                                                          (cons sym type))
                                                                       (impc:ti:get-closure-arg-symbols (car t) t1)
                                                                       (cdddr t))
                                                                  (list t))
                                                          (list t)))
                                                   forced-types)))
                 (checked-for-duplicates (let loop ((types forced-types-updated))
                                            (if (null? types) (cl:remove-duplicates forced-types-updated)
                                                (if (and (assoc (caar types) (cdr types))
                                                         (not (equal? (cdr (assoc (caar types) (cdr types)))
                                                                      (cdr (car types)))))
                                                    (print-error 'Compiler 'Error: 'Type 'mismatch 'with 'fixed 'types
                                                                 (assoc (caar types) (cdr types))
                                                                 'and (car types) '- 'do 'you 'have 'a 'conflicing 'closure 'type?)
                                                    (loop (cdr types)))))))
             ;(print 'checked checked-for-duplicates 'forced-types-udpated forced-types-updated 'forced: forced-types)
             checked-for-duplicates))))
			 
			 

(define impc:ti:get-closure-names
   (lambda (ast . args)
      (let ((blst '()))
         (let loop ((alst ast))
            (cond ((null? alst) '())
                  ((atom? alst) '())
                  ((pair? alst)		   
                   (if (equal? (car alst) 'make-closure)
		       (set! blst (cons (caddr alst) blst)))
                   (loop (car alst))
                   (loop (cdr alst)))))
         blst)))


(define impc:ti:numeric-cast-operator
  (lambda (a b)
    (let* ((lowest (if (< a b) a b))
	   (highest (if (= a lowest) b a))
	   (caststr (string-append (if (impc:ir:floating-point? highest)
				       (if (= highest 0) "d" "f")
				       (impc:ir:pretty-print-type highest))
				   "to"
				   (if (impc:ir:floating-point? lowest)
				       (if (= lowest 0) "d" "f")
				       (impc:ir:pretty-print-type lowest)))))
      caststr)))


;; an optional compiler stage to support some basic type coercions
;; particular math coercions of forced types
(define impc:ti:coercion-run
  (lambda (ast forced-types)
    (if (pair? ast)
	(cond ((member (car ast) '(< > * / = + - <>))
	       (let ((a (assoc (cadr ast) forced-types))
		     (b (assoc (caddr ast) forced-types)))
		 (if (and (and a b)
			  (<> (cdr a) (cdr b)))
		     (let ((ret (string->symbol (impc:ti:numeric-cast-operator (cdr a) (cdr b)))))
		       (if (> (cdr a) (cdr b))
			   `(,(car ast) (,ret ,(cadr ast)) ,(caddr ast))
			   `(,(car ast) ,(cadr ast) (,ret ,(caddr ast)))))
		     (if (and a (number? (caddr ast)))
			 (if (and (impc:ir:floating-point? (cdr a))
				  (integer? (caddr ast)))
			     `(,(car ast) ,(cadr ast) ,(integer->real (caddr ast)))
			     (if (and (impc:ir:fixed-point? (cdr a))
				      (real? (caddr ast)))
				 `(,(car ast) ,(cadr ast) ,(real->integer (caddr ast)))
				 ast))
			 (if (and b (number? (cadr ast)))
			     (if (and (impc:ir:floating-point? (cdr b))
				      (integer? (cadr ast)))
				 `(,(car ast) ,(integer->real (cadr ast)) ,(caddr ast))
				 (if (and (impc:ir:fixed-point? (cdr b))
					  (real? (cadr ast)))
				     `(,(car ast) ,(real->integer (cadr ast)) ,(caddr ast))
				     ast))
			     ast)))))
	      (else (cons (impc:ti:coercion-run (car ast) forced-types)
			  (impc:ti:coercion-run (cdr ast) forced-types))))
	ast)))

			 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define impc:ti:run
   (lambda (symname code . args)
     ;(println 'impc:ti:run: symname)
     ;(println 'code: code)
     ;(println 'args: args)
      (set! *impc:ir:ls_var* '())
      ;; don't want type checking to find existing native versions!
      (if *impc:compile*
          (begin ;(llvm:remove-globalvar (string-append (symbol->string symname) "_var"))
                 ;(llvm:erase-function (symbol->string symname))
                 (llvm:erase-function (string-append (symbol->string symname) "_setter"))
                 (llvm:erase-function (string-append (symbol->string symname) "_maker"))))  
      (let* ((c code)
	     ;(ct1 (now))
             (c1 (impc:ti:get-var-types c)) ;; this is a cons pair of (ast . types)
	     ;(ct2 (now))
             (t1 (impc:ti:first-transform (car c1) #t)) ;; car is ast
	     ;(lllll (println 't1: t1))
	     ;(ct3 (now))	     
             (t2 (impc:ti:mark-returns t1 symname #f #f #f))
	     ;(llllllll (println 't2: t2))	     
	     ;(ct4 (now))	     
             (t3 (impc:ti:closure:convert t2 (list symname)))
	     ;(llllllllll (println 't3: t3))  
	     ;(ct5 (now))	     
             (vars (map (lambda (x) (list x)) (impc:ti:find-all-vars t3 '())))
	     ;(llllllllllll (println 'vars: vars))	     
	     ;(ct6 (now))	     
             (forced-types (apply impc:ti:handle-forced-types t1 (append (cdr c1) args)))
	     ;(lllllllllllll (println 'forced-types: forced-types))
	     ;(ct7 (now))
	     (t4 (impc:ti:coercion-run t2 forced-types)) ;; t4 and t5 are optional
	     ;(ct8 (now))	     
	     (t5 (impc:ti:coercion-run t3 forced-types)) ;; also there is doubling dipping here :(
	     ;(ct9 (now))
             (types (impc:ti:run-type-check vars forced-types t4))
	     ;(lllllllllllllll (println 'types: types))
	     ;(ct10 (now))
             (newast (impc:ti:add-types-to-source symname t5 (cl:tree-copy types) (list)))
	     ;(ct11 (now))
	     )
	 ;(println 'types: types)
	 ;(println 'run: (impc:ti:unity? types))
	 ;(println 'newast: newast)
	 ;(println 'forced: forced-types)
	 ;(println 'times: (- ct2 ct1) (- ct3 ct2) (- ct4 ct3) (- ct5 ct4) (- ct6 ct5) (- ct7 ct6) (- ct8 ct7) (- ct9 ct8) (- ct10 ct9) (- ct11 ct10))	 
         ;; if we didn't unify print error and bomb out!
         (if (not (cl:every (lambda (x) x) (impc:ti:unity? types)))
	     (begin ;(println 'types: types)
		    (print-error 'Compiler 'Error: 'could 'not 'resolve
				 (cl:remove 'good (map (lambda (x y) (if y 'good (symbol->string (car x)))) 
						       types (impc:ti:unity? types)))
				 'you 'could 'try 'forcing 'the 'type 'of 'one 'or 'more 'of 'these 'symbols)))
         ;; if this function has been defined before make sure we aren't changing its signature!!
         (if (and (llvm:get-function (symbol->string symname))
                  (or (<> (length (llvm:get-function-args-withoutzone (symbol->string symname)))
                          (length (cddr (assoc symname types)))) 
                      (cl:position #f (map (lambda (a b)
					     (equal? a b))
                                           (cons (+ *impc:ir:closure*
						    *impc:ir:pointer*
                                                    *impc:ir:pointer*)
                                                 (map (lambda (x) (impc:ir:get-type-from-str x))
                                                      (llvm:get-function-args-withoutzone (symbol->string symname))))
                                           (cdr (assoc symname types))))))
             (print-error 'Compiler 'Error: 'sorry 'the 'compiler 'does 'not 'currently
			              'allow 'you 'to 'redefine 'or 'overload 'the 'type 'signature 'of 'existing 'functions. 
						  'in 'this 'case (symbol->string symname) 'to: 
                          (impc:ir:pptype (cdr (assoc symname types))) 'from:
                          (impc:ir:pptype (cons (+ *impc:ir:closure*
						   *impc:ir:pointer*
                                                   *impc:ir:pointer*)
                                                 (map (lambda (x) (impc:ir:get-type-from-str x))
                                                      (llvm:get-function-args-withoutzone (symbol->string symname)))))))
	 ;(print-error "stop")
         (if *impc:compiler:print-types* (println '---------------------------------))
         (if *impc:compiler:print-types* (println 'types: types))
         ;(println 'ctypes: converted-types)
         (if *impc:compiler:print-types* (println 'newast: newast))

         ;; check for unfound types
         (for-each (lambda (t)
		     (if (not (impc:ir:type? (cdr t)))
			 (print-error 'Compiler 'Error: 'unresolved 'type 'error 'for 'symbol (car t))))
		   types)
		      ;; ;(println 't: t)
                      ;; (cond ((and (list? t)
                      ;;             (member *impc:ir:other* t))
		      ;; 	     (println 'kkkkkkkkk t *impc:ir:other*)
                      ;;        (print-error 'Compiler 'Error: 'unresolved 'type 'error 'for 'symbol (car t)))
                      ;;       ((and (not (list? t))
                      ;;             (pair? t)
                      ;;             (equal? *impc:ir:other* (cdr t)))
                      ;;        (print-error 'Compiler 'Error: 'unresolved 'type 'error 'for 'symbol (car t)))))
                   ;; types)

	 
	 ;; ;; forward declare _callback (this is a bit of a hack :-(
	 ;; (if (llvm:get-globalvar (string-append (symbol->string symname) "_var"))
	 ;;     'donothing
	 ;;     (llvm:compile (string-append "declare linkonce void @" (symbol->string symname) "_callback(i8*)\n"))) ;;{\nentry:\nret void\n}")))
         ;; compile to ir	 
         (define fstr (impc:ir:compiler newast types))
         ;; compile to x86 - i.e. call jit on any new ir functions to force jit compilation	
         (for-each (lambda (fn) (llvm:jit-compile-function fn)) (impc:ti:get-closure-names newast))

         ;;
         ;; now compile ir to x86 and make stub code
         (let* ((closure-type (cadr (impc:ir:gname)))
		(closure-type-- (impc:ir:get-type-str (impc:ir:pointer-- (impc:ir:get-type-from-str closure-type))))
                (compile-stub? (if (llvm:get-globalvar (string-append (symbol->string symname) "_var")) #f #t))
		;(forward-declaration-of-callback (if compile-stub? (llvm:compile (string-append "declare void @" (symbol->string symname) "_callback(i8*)")) #f))
                (fs (string-append "define ccc " closure-type " @" (string-append (symbol->string symname) "_maker")
                                   "(i8* %_impz){\nentry:\n"				   
                                   ;; "%_zone = bitcast i8* %_impz to %mzone*\n"
				   ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				   ;; ;; new lines for impz
				   "%_impzPtr = alloca i8*\n"
				   "store i8* %_impz, i8** %_impzPtr\n"
				   ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                   fstr "}\n"))
                (fssetter (string-append (if (llvm:get-globalvar (string-append (symbol->string symname) "_var"))
                                             "" ;; if global var alread exists do nothing
                                             (string-append "@" (symbol->string symname) "_var = global [1 x i8*] [ i8* null ]\n\n"
                                                            "@" (symbol->string symname) "_var_zone = global [1 x i8*] [ i8* null ]\n\n"))
                                         "define ccc void @" (string-append (symbol->string symname) "_setter")
                                         "(i8* %_impz){\nentry:\n" 
                                         "%oldzone1 = getelementptr [1 x i8*]* @" (symbol->string symname) "_var_zone, i32 0, i32 0\n"
                                         "%oldzone2 = load i8** %oldzone1\n"
                                         "%oldzone3 = bitcast i8* %oldzone2 to %mzone*\n"
                                         "store i8* %_impz, i8** %oldzone1\n"
                                         ; existing code
                                         "%closure = call ccc " (cadr (impc:ir:gname))
                                         " @" (string-append (symbol->string symname) "_maker") "(i8* %_impz)\n"
                                         "%ptr = bitcast " (cadr (impc:ir:gname)) " %closure to i8*\n"
                                         "%varptr = bitcast [1 x i8*]* @" (symbol->string symname) "_var to i8**\n"
                                         "store i8* %ptr, i8** %varptr\n"
                                         ;; new code
                                         "; destroy oldzone if not null\n"
                                         "%test = icmp ne %mzone* %oldzone3, null\n"
                                         "br i1 %test, label %then, label %cont\n"
                                         ;"then:\ncall ccc void @llvm_zone_destroy(%mzone* %oldzone3)\nbr label %cont\n"
					 "then:\ncall ccc void @llvm_destroy_zone_after_delay(%mzone* %oldzone3, i64 441000)\nbr label %cont\n"
                                         "cont:\n"
                                         "ret void\n}\n"))
                ;(stub-type (string->sexpr (impc:ti:string-to-type closure-type)))
                (stub-type (impc:ir:get-closure-type-from-str closure-type))
                (fsgetter (string-append "define ccc i8* @" (symbol->string symname) "_getter(){\n"
                                         "entry:\n"
                                         "%ptr = getelementptr [1 x i8*]* @" (symbol->string symname) "_var, i32 0, i32 0\n"
                                         "%func = load i8** %ptr\n"
                                         "ret i8* %func\n}\n"))
		(cb-struct-type (if (null? (cdr stub-type))
				    '()
				    (string-append ;"{void(i8*)*"
				               "{"
					       (impc:ir:get-type-str (cadr stub-type))
					       (apply string-append
						      (map (lambda (a)
							     (string-append ", " (impc:ir:get-type-str a)))
							   (cddr stub-type)))
					       "}*")))
                (fstub (string-append "define ccc " (impc:ir:get-type-str (car stub-type))
                                      " @" (string-append (symbol->string symname) "(i8* %_impz")
                                      (apply string-append (map (lambda (t n c)
                                                                   (string-append c (impc:ir:get-type-str t) " "
                                                                                  n))
                                                                (cdr stub-type)                                           
                                                                '("%a" "%b" "%c" "%d" "%e" "%f" "%g" "%h" "%i" "%j" "%k" "%l" "%m" "%n" "%o" "%p" "%q" "%r" "%s" "%t")
                                                                '("," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ",")))
                                      ")\n"
                                      "{\nentry:\n"
                                      ;"%_zone = call ccc %mzone* @malloc_create_zone(0,0)\n"
                                      ;"%_zone = call ccc %mzone* @malloc_default_zone()\n"
                                      ;"%_impz = bitcast %mzone* %_zone to i8*\n"
                                      "%ptr = getelementptr [1 x i8*]* @" (symbol->string symname) "_var, i32 0, i32 0\n"
                                      "%ptrvar = load i8** %ptr\n"
                                      "%closure_tmp = bitcast i8* %ptrvar to " closure-type "\n"				      
                                      "%closure = load " closure-type " %closure_tmp \n"
                                      "%fPtr = getelementptr " closure-type-- " %closure, i32 0, i32 2\n"
                                      "%ePtr = getelementptr " closure-type-- " %closure, i32 0, i32 1\n"
                                      "%ff = load "
                                      (regex:replace closure-type-- "<\\{ ?i8\\*, ?i8\\*,(.*)\\}>\\*" "$1")
                                      "* %fPtr\n"
                                      "%ee = load i8** %ePtr\n"
                                      (if (impc:ir:void? (car stub-type)) "" "%result = ")
                                      "tail call fastcc " (impc:ir:get-type-str (car stub-type)) " %ff(i8* %_impz, i8* %ee"
                                      (apply string-append (map (lambda (t n)
                                                                   (string-append ", " 
                                                                                  (impc:ir:get-type-str t) 
                                                                                  " " n))
                                                                (cdr stub-type)
								'("%a" "%b" "%c" "%d" "%e" "%f" "%g" "%h" "%i" "%j" "%k" "%l" "%m" "%n" "%o" "%p" "%q" "%r" "%s" "%t")))
                                      ")\nret " (impc:ir:get-type-str (car stub-type)) 
                                      (if (impc:ir:void? (car stub-type)) "\n" " %result\n")
                                      "}"))
		(fscallback (string-append "define ccc void @" (string-append (symbol->string symname) "_callback(i8* %dat){\n"
					   "entry:\n"
					   (if (null? cb-struct-type)
					       "%fstruct = select i1 true, i8* %dat, i8* %dat\n"
					       (string-append "%fstruct = bitcast i8* %dat to " cb-struct-type "\n"))
					   ;;"%fPtr = getelementptr " cb-struct-type " %fstruct ,i32 0, i32 0\n"
					   (apply string-append (map (lambda (n t ap a)
								       (string-append ap " = getelementptr " cb-struct-type " %fstruct, i32 0, i32 " (number->string n) "\n"
										      a " = load " (impc:ir:get-type-str t) "* " ap "\n"))
								     (make-list-with-proc (- (length stub-type) 1) (lambda (i) i)) ;(+ i 1)))
								     (cdr stub-type)
								     '("%ap" "%bp" "%cp" "%dp" "%ep" "%fp" "%gp" "%hp" "%ip" "%jp" "%kp" "%lp" "%mp" "%np" "%op" "%pp" "%qp" "%rp" "%sp" "%tp")
								     '("%a" "%b" "%c" "%d" "%e" "%f" "%g" "%h" "%i" "%j" "%k" "%l" "%m" "%n" "%o" "%p" "%q" "%r" "%s" "%t")))
					   "%_zone = call ccc %mzone* @llvm_peek_zone_stack()\n"
					   "%_impz = bitcast %mzone* %_zone to i8*\n"
					   "%ptr = getelementptr [1 x i8*]* @" (symbol->string symname) "_var, i32 0, i32 0\n"
					   "%ptrvar = load i8** %ptr\n"
					   "%closure_tmp = bitcast i8* %ptrvar to " closure-type "\n"
					   "%closure = load " closure-type " %closure_tmp \n"
					   "%fPtr = getelementptr " closure-type-- " %closure, i32 0, i32 2\n"
					   "%ePtr = getelementptr " closure-type-- " %closure, i32 0, i32 1\n"
					   "%ff = load "
					   (regex:replace closure-type-- "<\\{ ?i8\\*, ?i8\\*,(.*)\\}>\\*" "$1")
					   "* %fPtr\n"
					   "%ee = load i8** %ePtr\n"
					   (if (impc:ir:void? (car stub-type)) "" "%result = ")
					   "tail call fastcc " (impc:ir:get-type-str (car stub-type)) " %ff(i8* %_impz, i8* %ee"
					   (apply string-append (map (lambda (t n)
								       (string-append ", " 
										      (impc:ir:get-type-str t) 
										      " " n))
								     (cdr stub-type)
								     '("%a" "%b" "%c" "%d" "%e" "%f" "%g" "%h" "%i" "%j" "%k" "%l" "%m" "%n" "%o" "%p" "%q" "%r" "%s" "%t")))
					   ")\nret void\n" ; (impc:ir:get-type-str (car stub-type)) 
					   ;(if (impc:ir:void? (car stub-type)) "\n" " %result\n")
					   "}")))
		(fstub_scheme_valid #t) ;; this will be set to false if no valid conversion type to scheme is available (structs and arrays are not supported yet for example)
                (fstub_scheme (string-append "define ccc i8* " ;(impc:ir:get-type-str (car stub-type))
					     " @" (string-append (symbol->string symname) "_scheme(i8* %_sc, i8* %args)\n"
					     "{\nentry:\n"
					     ;"%_zone = call ccc %mzone* @llvm_zone_create(i64 2048)\n"
					     ;"%_zone = call ccc %mzone* @malloc_default_zone()\n"
					     "%_zone = call ccc %mzone* @llvm_peek_zone_stack()\n"
					     "%_impz = bitcast %mzone* %_zone to i8*\n"

					     ;(begin (println 'aaaaa) "")

					     (apply string-append
						    (map (lambda (t n idx)
							   ;(println 't: t 'n: n 'idx: idx)
							   (string-append n "_val = call ccc i8* @list_ref(i8* %_sc, i32 " (number->string idx) ",i8* %args)\n"
                                                                            (cond ((and (not (number? t))
											(not (impc:ir:pointer? t)))
										   (set! fstub_scheme_valid #f)
										   "")
										  ((or (not (number? t))
										       (> t 9))
										   (string-append "%ttv_" (number->string idx) " = call ccc i8* @cptr_value(i8* " n "_val)\n"
												  n " = bitcast i8* %ttv_" (number->string idx) " to " (impc:ir:get-type-str t) "\n"))
										  ((= t 0) (string-append n " = call ccc double @r64value(i8* " n "_val)\n"))
										  ((= t 1) (string-append n " = call ccc float  @r32value(i8* " n "_val)\n"))
										  ((= t 2) (string-append n " = call ccc i64  @i64value(i8* " n "_val)\n"))
										  ((= t 3) (string-append n " = call ccc i64  @i64value(i8* " n "_val)\n"))
										  ((= t 4) (string-append n " = call ccc i32  @i32value(i8* " n "_val)\n"))
										  ((= t 5) (string-append n " = call ccc i32  @i32value(i8* " n "_val)\n"))
										  ((= t 6) (string-append n " = call ccc i8  @i8value(i8* " n "_val)\n"))
										  ((= t 7) (string-append n " = call ccc i8  @i8value(i8* " n "_val)\n"))
										  ((= t 8) (string-append n " = call ccc i1  @i1value(i8* " n "_val)\n"))
										  ((= t 9) (string-append n " = call ccc i8*  @string_value(i8* " n "_val)\n"))
										  (else (error "Compiler Error: 'bad 'type 'in 'scheme 'stub")))))
							 (cdr stub-type)
							 '("%a" "%b" "%c" "%d" "%e" "%f" "%g" "%h" "%i" "%j" "%k" "%l" "%m" "%n" "%o" "%p" "%q" "%r" "%s" "%t")
							 (make-list-with-proc 20 (lambda (i) i))))

					     ;(begin (println 'bbbbb) "")					     
					     
					     ;"call ccc void @llvm_destroy_zone_after_delay(i8* %_impz, double 88200.0)\n"
					     "%ptr = getelementptr [1 x i8*]* @" (symbol->string symname) "_var, i32 0, i32 0\n"
					     "%ptrvar = load i8** %ptr\n"
					     "%closure_tmp = bitcast i8* %ptrvar to " closure-type "\n"
					     "%closure = load " closure-type " %closure_tmp \n"
					     "%fPtr = getelementptr " closure-type-- " %closure, i32 0, i32 2\n"
					     "%ePtr = getelementptr " closure-type-- " %closure, i32 0, i32 1\n"
					     "%ff = load "
					     (regex:replace closure-type-- "<\\{ ?i8\\*, ?i8\\*,(.*)\\}>\\*" "$1")
					     "* %fPtr\n"
					     "%ee = load i8** %ePtr\n"
					     (if (impc:ir:void? (car stub-type)) "" "%result = ")
					     "tail call fastcc " (impc:ir:get-type-str (car stub-type)) " %ff(i8* %_impz, i8* %ee"

					     (apply string-append
						    (map (lambda (t n)
							   (string-append ", " (impc:ir:get-type-str t) " " n))
							 (cdr stub-type)
							 '("%a" "%b" "%c" "%d" "%e" "%f" "%g" "%h" "%i" "%j" "%k" "%l" "%m" "%n" "%o" "%p" "%q" "%r" "%s" "%t")))
					     ")\n"

					     ;(begin (println 'ccccc) "")

					     (let* ((t (car stub-type)))
					       (cond ((and (not (number? t))
							   (not (impc:ir:pointer? t)))
						      (set! fstub_scheme_valid #f)
						      "")
						     ((or (not (number? t))
							  (> t 9))
						      (string-append "%tmpres = bitcast " (impc:ir:get-type-str t) " %result to i8*\n"
								     "%res = call ccc i8* @mk_cptr(i8* %_sc, i8* %tmpres)\n"))
						     ((= t -1) "%res = call ccc i8* @mk_i1(i8* %_sc, i1 1)\n") ;; don't do anything for void
						     ((= t 0) "%res = call ccc i8* @mk_double(i8* %_sc, double %result)\n")
						     ((= t 1) "%res = call ccc i8* @mk_float(i8* %_sc, float %result)\n")
						     ((= t 2) "%res = call ccc i8* @mk_i64(i8* %_sc, i64 %result)\n")
						     ((= t 3) "%res = call ccc i8* @mk_i64(i8* %_sc, i64 %result)\n") 
						     ((= t 4) "%res = call ccc i8* @mk_i32(i8* %_sc, i32 %result)\n")
						     ((= t 5) "%res = call ccc i8* @mk_i32(i8* %_sc, i32 %result)\n")  
						     ((= t 6) "%res = call ccc i8* @mk_i8(i8* %_sc, i8 %result)\n")
						     ((= t 7) "%res = call ccc i8* @mk_i8(i8* %_sc, i8 %result)\n")	 
						     ((= t 8) "%res = call ccc i8* @mk_i1(i8* %_sc, i1 %result)\n")
						     ((= t 9) "%res = call ccc i8* @mk_string(i8* %_sc, i8* %result\n")
						     (else (error 'Compiler 'Error: 'return 'type 'error 'in 'scheme 'stub))))
					     
					     "ret i8* %res\n"
					     "}")))
		;;(fffffffff (println fstub_scheme))
                (fstub_native (string-append "define ccc " (impc:ir:get-type-str (car stub-type))
					     " @" (string-append (symbol->string symname) "_native("
								 (apply string-append (map (lambda (t n c)
											     (string-append c (impc:ir:get-type-str t) " "
													    n))
											   (cdr stub-type)                                
											   '("%a" "%b" "%c" "%d" "%e" "%f" "%g" "%h" "%i" "%j" "%k" "%l" "%m" "%n" "%o" "%p" "%q" "%r" "%s" "%t")
											   '("" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ",")))
					     ")\n"
					     "{\nentry:\n"
					     ;"%_zone = call ccc %mzone* @llvm_zone_create(i64 2048)\n"
					     ;"%_zone = call ccc %mzone* @malloc_default_zone()\n"
					     "%_zone = call ccc %mzone* @llvm_peek_zone_stack()\n"
					     "%_impz = bitcast %mzone* %_zone to i8*\n"
					     ;"call ccc void @llvm_destroy_zone_after_delay(i8* %_impz, double 88200.0)\n"
					     "%ptr = getelementptr [1 x i8*]* @" (symbol->string symname) "_var, i32 0, i32 0\n"
					     "%ptrvar = load i8** %ptr\n"
					     "%closure_tmp = bitcast i8* %ptrvar to " closure-type "\n"
					     "%closure = load " closure-type " %closure_tmp \n"
					     "%fPtr = getelementptr " closure-type-- " %closure, i32 0, i32 2\n"
					     "%ePtr = getelementptr " closure-type-- " %closure, i32 0, i32 1\n"
					     "%ff = load "
					     (regex:replace closure-type-- "<\\{ ?i8\\*, ?i8\\*,(.*)\\}>\\*" "$1")
					     "* %fPtr\n"
					     "%ee = load i8** %ePtr\n"
					     (if (impc:ir:void? (car stub-type)) "" "%result = ")
					     "tail call fastcc " (impc:ir:get-type-str (car stub-type)) " %ff(i8* %_impz, i8* %ee"
					     (apply string-append (map (lambda (t n)
									 (string-append ", " 
											(impc:ir:get-type-str t) 
											" " n))
								       (cdr stub-type)
								       '("%a" "%b" "%c" "%d" "%e" "%f" "%g" "%h" "%i" "%j" "%k" "%l" "%m" "%n" "%o" "%p" "%q" "%r" "%s" "%t")))
					     ")\nret " (impc:ir:get-type-str (car stub-type)) 
					     (if (impc:ir:void? (car stub-type)) "\n" " %result\n")
					     "}"))))

	   ;;(println fsgetter)
	   ;;(println fstub)
            (if *impc:compiler:print* (println '------------------------------compiling 'maker----------------------------------->))
            (if *impc:compiler:print* (println fs))
            (if *impc:compiler:print-raw-llvm* (print-full-nq fs)) 
            (if *impc:compile*
                (begin ;(llvm:remove-function (string-append (symbol->string symname) "_maker"))
                       (if (not (llvm:compile fs))
                           (begin (print-error "Compiler Failed")
                                  (error "")))
                       (if *impc:compiler:print* (print-notification "compiled maker"))))
            (if *impc:compiler:print* (println '--------------------------------compiling 'setter----------------------------------->))            
            (if *impc:compiler:print* (println fssetter))
            (if *impc:compiler:print-raw-llvm* (print-full-nq fssetter))
            (if *impc:compile*
                (begin ;(llvm:remove-function (string-append (symbol->string symname) "_setter"))
                       ;(llvm:remove-globalvar (string-append (symbol->string symname) "_var"))
                       (if (not (llvm:compile fssetter))
                           (begin (print-error "Compiler Failed")
                                  (error "")))
                       (if *impc:compiler:print* (print-notification "compiled setter"))))
            ;(if *impc:compile*
            ;    (let ((res1 (llvm:get-function (string-append (symbol->string symname) "_setter"))))                   
            ;       (if (cptr? res1)
            ;           (llvm:run res1 (sys:create-mzone))
            ;           (begin (print-error 'Compiler 'Error '- 'error 'creating 'setter 'function)
            ;                  (error "")))))
            (if *impc:compiler:print* (println '--------------------------------compiling 'getter----------------------------------->))            
            (if *impc:compiler:print* (println fsgetter))            
            (if *impc:compiler:print-raw-llvm* (print-full-nq fsgetter)) 
            (if (and *impc:compile* compile-stub?) ;; only compile stub first time around!!!
                (begin ;(llvm:remove-function (string-append (symbol->string symname) "_stub"))
                       (if (not (llvm:compile fsgetter))
                           (begin (print-error "Compiler Failed")
                                  (error ""))) 
                       (if *impc:compiler:print* (print-notification "compiled stub"))))
            (if *impc:compiler:print* (println '--------------------------------compiling 'stubs----------------------------------->))            
            (if *impc:compiler:print* (println fstub))            
            (if *impc:compiler:print-raw-llvm* (print-full-nq fstub))
            (if (and *impc:compile* compile-stub?) ;; only compile stub first time around!!!
                (begin ;(llvm:remove-function (string-append (symbol->string symname) "_stub"))
                       (if (or (not (llvm:compile fstub))
			       (not (llvm:compile fstub_native))
			       (not (if fstub_scheme_valid (llvm:compile fstub_scheme) #t)))
                           (begin (print-error "Compiler Failed building stubs!")
                                  (error "")))
		       ;; bind fstub_scheme
		       ;(add-foreign-func (symbol->string symname) (llvm:get-scheme-function (symbol->string name)))
                       (if *impc:compiler:print*
			   (print-notification "compiled stubs"))))
	   (if *impc:compiler:print* (println '----------------------------compiling 'callback----------------------------------->))
	   (if *impc:compiler:print* (println fscallback))
	   (if *impc:compiler:print-raw-llvm* (print-full-nq fscallback))
	   (if (and *impc:compile* compile-stub?) ;; only compile callback first time around!!!
	       (begin ;(llvm:remove-function (string-append (symbol->string symname) "_stub"))
	   	 (if (not (llvm:compile fscallback))
	   	     (begin (print-error 'Compiler 'Failed:)
	   		    (error "")))
	   	 (if *impc:compiler:print* (print-notification "compiled callback"))))
	   
	    

            (if *impc:compile*
                (let ((ftype (llvm:get-function-args-withoutzone (symbol->string symname)))) 		  
                   (if (not *impc:compiler:print-raw-llvm*)
		       (begin ;(println 'Successfully 'compiled symname '>>> 
				   ;    (string->sexpr (impc:ir:pretty-print-type (cons (+ *impc:ir:closure* *impc:ir:pointer*)
					;					       ftype))))
			      (ascii-print-color 0 7 10)
			      (print "Successfully compiled ")
			      (ascii-print-color 1 2 10)
			      (print symname)
			      (ascii-print-color 0 7 10)
			      (print " >>> ")
			      (ascii-print-color 1 3 10)
			      (print (string->symbol (impc:ir:pretty-print-type (impc:ir:get-type-str (cons (+ *impc:ir:closure* *impc:ir:pointer* *impc:ir:pointer*) ftype)))))
			      (ascii-print-color 0 7 10)
			      (print)))))
            (cadr (impc:ir:gname))))))


;; definec takes optional type arguments after symname
(define-macro (definec symname . args)
  (let ((zone-size *impc:default-zone-size*))
    (if (number? (car args))
	(begin (set! zone-size (car args))
	       (set! args (cdr args))))
    (let ((types (cdr (reverse args)))
	  (expr (car (reverse args))))
      (if (regex:match? (symbol->string symname) ":")
	  (let ((sres (regex:split (symbol->string symname) ":")))
	    (set! symname (string->symbol (car sres)))
	    (set! types (cons (cons symname (string->symbol (cadr sres))) types))))
      ;(print-full symname 'types: types 'e: expr 'args: args)
      `(define ,symname
	 (let* ((res1 (impc:ti:run ',symname
				   '(let ((,symname ,expr)) ,symname)
				   ,@(if (null? types) 
					 '()
					 (map (lambda (k) (list 'quote k)) types))))
		(setter (llvm:get-function (string-append (symbol->string ',symname) "_setter")))
		(func (llvm:get-function (symbol->string ',symname))))
	   (if setter
	       (llvm:run setter (sys:create-mzone ,zone-size))
	       (begin (print-error 'no 'compiled 'function ',symname 'setter  '... 'turn 'on 'compilation?)
		      (error "")))
	   (if func
	       ;(lambda args (apply llvm:run func (sys:peek-memzone) args))
	       (if (llvm:get-scheme-function ,(symbol->string symname))
		   (mk-ff (llvm:get-scheme-function ,(symbol->string symname)))
		   (begin (ascii-print-color 0 7 10)
			  (print "There is no scheme stub available for ")
			  (ascii-print-color 0 6 10)
			  (println ,(symbol->string symname))
			  (ascii-print-color 0 7 10)))
	       (begin (print-error 'no 'compiled 'function ',symname  '... 'turn 'on 'compilation?)
		      (error ""))))))))

;; alias for definec (should replace definec)
(define-macro (bind-func . args)
  `(impc:ti:parametric-poly-pass '(definec ,@args)))

(define-macro (bind-typevar name . args)
  (if (string? name) (set! name (string->symbol name)))
  `(map (lambda (a)
	  (impc:ti:add-vtype ',name a))
	',args))

;; Definec-precomp is for setting up precompiled ir functions only
(define definec-precomp
  (lambda (symname)
    (let ((zone-size *impc:default-zone-size*))
      (eval
       `(define ,symname
	  (let* ((setter (llvm:get-function (string-append (symbol->string ',symname) "_setter")))
		 (func (llvm:get-function (symbol->string ',symname))))
	    (println 'setter: setter 'func: func)
	    (if setter
		(llvm:run setter (sys:create-mzone ,zone-size))
		(begin (print-error 'no 'compiled 'function ',symname 'setter  '... 'turn 'on 'compilation?)
		       (error "")))
	    ;(println 'scheme_func: (llvm:get-scheme-function ,(symbol->string symname)))
	    (if func
		(mk-ff (llvm:get-scheme-function ,(symbol->string symname)))
		;(lambda args (apply llvm:run func (sys:peek-memzone) args))
		(begin (print-error 'no 'compiled 'function ',symname  '... 'turn 'on 'compilation?)
		       (error "")))))
       (interaction-environment)))))


;; definec-ipc is for passing an already compiled (and setter'd) native function
;; acroos to a non-primary process
(define definec-ipc
  (lambda (symname)
    (let ((zone-size *impc:default-zone-size*))
      (eval
       `(define ,symname
	  (let* ((setter (llvm:get-function (string-append (symbol->string ',symname) "_setter")))
		 (func (llvm:get-function (symbol->string ',symname))))
	    ;(println 'scmfunc: (llvm:get-scheme-function ,(symbol->string symname)))
	    (if func
		(mk-ff (llvm:get-scheme-function ,(symbol->string symname)))
		;(lambda args (apply llvm:run func (sys:peek-memzone) args))
		(begin (print-error 'no 'compiled 'function ',symname  '... 'turn 'on 'compilation?)
		       (error "")))))
       (interaction-environment)))))

(define ipc:definec
  (lambda (procname symname)
    (ipc:call procname 'definec-ipc symname)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					
;; macro helper for fx code au's							
(define-macro (definec:dsp . args)
   `(definec ,(car args)
       (,(car args) . [double,double,double,double,double*]*)
       ,(cadr args)))


(define-macro (bind-alias symbol type)
  `(begin (impc:ir:add-type-alias ,(symbol->string symbol) ,(symbol->string type))
          (ascii-print-color 0 7 10)
	  (print "Successfull aliased ")
	  (ascii-print-color 1 2 10)
	  (print ',symbol)
	  (ascii-print-color 0 7 10)
	  (print " >>> ")
	  (ascii-print-color 1 3 10)
	  ;(print ',type)
	  (print (impc:ir:pretty-print-type ,(symbol->string symbol))) ; (llvm:get-named-type ,(symbol->string symbol))))
	  (ascii-print-color 0 7 10)
	  (print)))
  

(define-macro (bind-type symbol type)
  `(begin (if (char=? (string-ref ,(symbol->string type) 0) #\<) ; (impc:ir:tuple? (impc:ir:get-type-from-pretty-str ,(symbol->string type)))
	      (llvm:compile (string-append "%" ,(symbol->string symbol) " = type "
					   ,(impc:ir:get-type-str (impc:ir:get-type-from-pretty-str (symbol->string type) (symbol->string symbol)))))
	      (print-error 'Compiler 'Error: 'only 'named 'tuple 'types 'are 'supported))
	  (ascii-print-color 0 7 10)
	  (print "Successfull bound ")
	  (ascii-print-color 1 2 10)
	  (print ',symbol)
	  (ascii-print-color 0 7 10)
	  (print " >>> ")
	  (ascii-print-color 1 3 10)
	  ;(print ',type)
	  (print (impc:ir:pretty-print-type (llvm:get-named-type ,(symbol->string symbol))))
	  (ascii-print-color 0 7 10)
	  (print)))


(define-macro (bind-tvar symbol . types)
  `(begin (if (char=? (string-ref ,(symbol->string type) 0) #\<) ; (impc:ir:tuple? (impc:ir:get-type-from-pretty-str ,(symbol->string type)))
	      (llvm:compile (string-append "%" ,(symbol->string symbol) " = type "
					   ,(impc:ir:get-type-str (impc:ir:get-type-from-pretty-str (symbol->string type) (symbol->string symbol)))))
	      (print-error 'Compiler 'Error: 'only 'named 'tuple 'types 'are 'supported))
	  (ascii-print-color 0 7 10)
	  (print "Successfull bound ")
	  (ascii-print-color 1 2 10)
	  (print ',symbol)
	  (ascii-print-color 0 7 10)
	  (print " >>> ")
	  (ascii-print-color 1 3 10)
	  ;(print ',type)
	  (print (impc:ir:pretty-print-type (llvm:get-named-type ,(symbol->string symbol))))
	  (ascii-print-color 0 7 10)
	  (print)))
  


;;
;; bindc should not really be called directly and
;; is really now only here for legacy reasons
;;
;; although bind-val does currently call into bindc
;; so don't go deleteing it!!
;;
;; end users should use bind-val instead
;;
(define-macro (bindc symbol type value)
   (if (cptr? (eval value))
       `(begin (if (not (llvm:get-globalvar ,(symbol->string symbol)))
                   (llvm:compile (string-append "@" ,(symbol->string symbol)
                                                " = external global "
                                                ,(impc:ir:get-type-str (impc:ir:convert-from-pretty-types type)))))
               ;(ipc:call ,*impc:compiler:process* 'llvm:bind-global-var ,(symbol->string symbol) ,value)
	       (llvm:bind-global-var ,(symbol->string symbol) ,value)
	       (ascii-print-color 0 7 10)
	       (print "Successfully bound ")
	       (ascii-print-color 1 2 10)
	       (print ',symbol)
	       (ascii-print-color 0 7 10)
	       (print " >>> ")
	       (ascii-print-color 1 3 10)
	       (print ',type)
	       (ascii-print-color 0 7 10)
	       (print))	       
       (print-error 'Compiler 'Error: 'bindc 'only 'accepts 'cptr 'values!  'Try 'bind-val 'for 'numeric 'binds)))


;; (define-macro (bind-val symbol type value)
;;    (if (number? (eval value))
;;        `(begin (if (not (llvm:get-globalvar ,(symbol->string symbol)))
;;                    (llvm:compile (string-append "@" ,(symbol->string symbol)
;;                                                 " = global "
;;                                                 ,(impc:ir:get-type-str (impc:ir:convert-from-pretty-types type))
;; 						" "
;; 						(atom->string ,value))))
;;                ;(ipc:call ,*impc:compiler:process* 'llvm:bind-global-var ,(symbol->string symbol) ,value)
;; 	       ;(llvm:bind-global-var ,(symbol->string symbol) ,value)
;; 	       (ascii-print-color 0 7 10)
;; 	       (print "Successfully bound ")
;; 	       (ascii-print-color 1 2 10)
;; 	       (print ',symbol)
;; 	       (ascii-print-color 0 7 10)
;; 	       (print " >>> ")
;; 	       (ascii-print-color 1 3 10)
;; 	       (print ',type)
;; 	       (ascii-print-color 0 7 10)
;; 	       (print))
;;        (if (cptr? (eval value))
;; 	   `(bindc ,symbol ,type ,value)
;; 	   (print-error 'Compiler 'Error: 'bind-val 'only 'accepts 'numeric 'and 'cptr 'values!))))


(define-macro (bind-val symbol type value)
  (if (and (not (impc:ir:pointer? (impc:ir:convert-from-pretty-types type)))
	   (or (impc:ir:number? (impc:ir:convert-from-pretty-types type)) ;(eval value))
	       (impc:ir:boolean? (impc:ir:convert-from-pretty-types type)))) ;(eval value))
       `(begin (if (not (llvm:get-globalvar ,(symbol->string symbol)))
                   (llvm:compile (string-append "@" ,(symbol->string symbol)
                                                " = global "
                                                ,(impc:ir:get-type-str (impc:ir:convert-from-pretty-types type))
						" "
						(atom->string ,value))))
               ;(ipc:call ,*impc:compiler:process* 'llvm:bind-global-var ,(symbol->string symbol) ,value)
	       ;(llvm:bind-global-var ,(symbol->string symbol) ,value)
	       (ascii-print-color 0 7 10)
	       (print "Successfully bound ")
	       (ascii-print-color 1 2 10)
	       (print ',symbol)
	       (ascii-print-color 0 7 10)
	       (print " >>> ")
	       (ascii-print-color 1 3 10)
	       (print ',type)
	       (ascii-print-color 0 7 10)
	       (print))
       (if (and (impc:ir:array? (impc:ir:convert-from-pretty-types type))
		(not (impc:ir:pointer? (impc:ir:convert-from-pretty-types type))))
	   (let* ((array-type (impc:ir:convert-from-pretty-types type))
		  (sub-type (caddr array-type))
		  (num (cadr array-type))
		  (atypestr (impc:ir:get-type-str array-type))
		  (subtypestr (impc:ir:get-type-str sub-type)))
	     `(begin (if (not (llvm:get-globalvar ,(symbol->string symbol)))
			 (llvm:compile (string-append "@" ,(symbol->string symbol)
						      " = global "
						      ,atypestr
						      " [" ,subtypestr " " ,(if (list? value)
										(atom->string (cadr value))
										(atom->string value))
						      ,(apply string-append (map (lambda (v)
										   (string-append ", " subtypestr
												  " " (atom->string v)))
										 (if (list? value)
										     (cddr value)
										     (make-list-with-proc (- num 1)
													  (lambda (i) value)))))
						      "]"))
			 (print-error 'Compiler 'Error: ',symbol 'is 'already 'bound))
		     (ascii-print-color 0 7 10)
		     (print "Successfully bound ")
		     (ascii-print-color 1 2 10)
		     (print ',symbol)
		     (ascii-print-color 0 7 10)
		     (print " >>> ")
		     (ascii-print-color 1 3 10)
		     (print ',type)
		     (ascii-print-color 0 7 10)
		     (print)))
	   (if (cptr? (eval value))
	       `(bindc ,symbol ,type ,value)
	       (print-error 'Compiler 'Error: 'bind-val 'only 'accepts 'numeric 'array 'and 'cptr 'values!)))))


(define impc:ir:get-function-type
  (lambda (name)
    (if (symbol? name)
	(set! name (symbol->string name)))
    (cons (+ *impc:ir:closure* *impc:ir:pointer* *impc:ir:pointer*)
	  (map (lambda (x) (impc:ir:get-type-from-str x))
	       (llvm:get-function-args-withoutzone name)))))


(define-macro (bind-poly poly_sym implementation_sym)
  (let ((type (impc:ir:get-function-type implementation_sym)))
    ;(set! type (impc:ir:get-type-from-pretty-str (symbol->string type)))
    `(begin (impc:ir:add-poly ',poly_sym ',implementation_sym ',type)
	    (ascii-print-color 0 7 10)
	    (print "Successfully poly'd ")
	    (ascii-print-color 1 2 10)
	    (print ',poly_sym)
	    (ascii-print-color 0 7 10)
	    (print " >>> ")
	    (ascii-print-color 1 3 10)
	    (print ,(impc:ir:pretty-print-type type))
	    (ascii-print-color 0 7 10)
	    (print))))
	  

(define-macro (bind-scm symbol type value)
   (if (cptr? (eval value))
       `(begin (if (not (llvm:get-globalvar ,(symbol->string symbol)))
                   (llvm:compile (string-append "@" ,(symbol->string symbol)
                                                " = external global "
                                                ,(impc:ir:get-type-str (impc:ir:convert-from-pretty-types type)))))
						;,(impc:ir:get-type-str (impc:ir:get-type-from-pretty-str type)))))
               ;(ipc:call ,*impc:compiler:process* 'llvm:bind-global-var ,(symbol->string symbol) ,value)
	       (llvm:bind-global-var ,(symbol->string symbol) ,value)
	       (ascii-print-color 0 7 10)
	       (print "Successfully bound ")
	       (ascii-print-color 1 2 10)
	       (print ',symbol)
	       (ascii-print-color 0 7 10)
	       (print " >>> ")
	       (ascii-print-color 1 3 10)
	       (print ',type)
	       (ascii-print-color 0 7 10)
	       ;(print " from scheme:" ,value)
	       (print))
       (if (number? (eval value))
	   `(bind-val ,symbol ,type ,value)
	   (print-error 'Compiler 'Error: 'bind-scm 'only 'accepts 'cptr 'or 'numeric 'values!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this here for wrapping llvm dynamic binds
(define-macro (bind-lib library symname type)
  `(__dynamic-bind ,library ',symname ',type))

(define __dynamic-bind
  (lambda (library symname type)
    (let* ((ctype (cdr (impc:ir:get-type-from-pretty-str (symbol->string type))))
           (ircode (string-append "declare "
                                   (impc:ir:get-type-str (car ctype))
                                   " @"
                                   (symbol->string symname)
                                   "("
                                   (if (null? (cdr ctype))
                                       ""
                                       (apply string-append
                                              (impc:ir:get-type-str (cadr ctype))
                                              (map (lambda (v)
                                                     (string-append "," (impc:ir:get-type-str v)))
                                                   (cddr ctype))))
                                   ")")))
      (if (and (llvm:compile ircode)
	       (llvm:bind-symbol library (symbol->string symname)))
	  (begin (ascii-print-color 0 9 10)
		 (print "Successfully bound ")
		 (ascii-print-color 1 2 10)
		 (print (symbol->string symname))
		 (ascii-print-color 0 9 10)
		 (print " >>> ")
		 (ascii-print-color 1 3 10)
		 (print type)
		 (ascii-print-color 0 9 10)
					;(print " from lib: " library)
		 (print))
	  (print-error 'Compiler 'Error: 'could 'not 'bind! symname)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; a helper for returning a scheme closure native closure (if one exists!)
(define llvm:get-native-closure
  (lambda (name)
    (let ((f (llvm:get-function (string-append name "_getter"))))
      (if f (llvm:run f)
	  '()))))

;; a helper for returning a scheme closure native closure (if one exists!)
(define llvm:get-native-function
  (lambda (name)
    (llvm:get-function-pointer (string-append name "_native"))))

;; a helper for returning a scheme closure native closure (if one exists!)
(define llvm:get-scheme-function
  (lambda (name)
    (llvm:get-function-pointer (string-append name "_scheme"))))

;; Wrap a native, bound C function, allowing it to be called from scheme
(define-macro (define-wrapper local-sym native-sym)
  (let* ((types (cdr (llvm:get-function-args (symbol->string native-sym))))
	 (args (map (lambda (t) (gensym)) types)))
    `(definec ,local-sym
       (lambda ,args
	 ,(cons native-sym args)))))

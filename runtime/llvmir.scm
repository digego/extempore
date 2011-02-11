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

;; base types
(define *impc:ir:void* -1)
(define *impc:ir:double* 0)
(define *impc:ir:float* 1)
(define *impc:ir:si64* 2)
(define *impc:ir:ui64* 3)
(define *impc:ir:si32* 4)
(define *impc:ir:ui32* 5)
(define *impc:ir:si8* 6)
(define *impc:ir:ui8* 7)
(define *impc:ir:i1* 8)
(define *impc:ir:char* 9)
(define *impc:ir:null* 10)
(define *impc:ir:closure* 11)
(define *impc:ir:tuple* 12)
;; this should be incremented to represent the lowest native type
(define *impc:ir:lowest-base-type* 13)

;; and a non-type
(define *impc:ir:other* 1000)

;; pointer offset
(define *impc:ir:pointer* 100)

(define impc:ir:get-base-type
   (lambda (str)
      (let* ((r1 (impc:ir:get-ptr-depth str))
             (r2 (string-append "^(.*)" (apply string-append (make-list r1 "\\*")) "$")))
         (regex:replace str r2 "$1"))))

(define impc:ir:get-ptr-depth
   (lambda (t)
      (if (string? t)
          (string-length (car (regex:matched t "([*]*)$")))
          (floor (/ (impc:ir:str-list-check t) *impc:ir:pointer*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This stuff is all here just for pretty printing

(define impc:ir:regex-tc-or-a "((\\[|\\<)(?<struct>[^<>\\[\\]]|(\\[|\\<)\\g<struct>*(\\]|\\>)\\**)*(\\]|\\>)\\**)|(?:([%0-9a-zA-Z]\\**)+)")


(define impc:ir:get-type-from-pretty-tuple
   (lambda (string-type)
      (let* ((s1 (regex:replace string-type "\\<(.*)\\>?.*" "$1"))
             (t1 (cl:remove-if (lambda (x) (string=? x "")) 
                               (regex:match-all s1 impc:ir:regex-tc-or-a)))
             (t2 (map (lambda (x) (impc:ir:get-type-from-pretty-str x)) t1)))
         t2)))

(define impc:ir:get-type-from-pretty-closure
   (lambda (string-type)
      (let* ((s1 (regex:replace string-type "\\[(.*)\\]?.*" "$1"))
             (t1 (cl:remove-if (lambda (x) (string=? x "")) 
                               (regex:match-all s1 impc:ir:regex-tc-or-a)))
             (t2 (map (lambda (x) (impc:ir:get-type-from-pretty-str x)) t1)))
         t2)))
		 
		 
(define impc:ir:pretty-print-type
   (lambda (t)
      (if (string? t)
          (impc:ir:pretty-print-type (impc:ir:get-type-from-pretty-str t))      
          (if (null? t) '()
              (cond ((atom? t) (impc:ir:get-type-str t))
                    ((impc:ir:tuple? t) 
                     (string-append "<" (string-join (map (lambda (k) (impc:ir:pretty-print-type k)) (cdr t)) ",")
                                    ">" (apply string-append (make-list-with-proc (impc:ir:get-ptr-depth t) (lambda (k) "*")))))
                    ((impc:ir:closure? t)
                     (string-append "[" (string-join (map (lambda (k) (impc:ir:pretty-print-type k)) (cdr t)) ",")
                                    "]" (apply string-append (make-list-with-proc (impc:ir:get-ptr-depth t) (lambda (k) "*"))))))))))


(define impc:ir:pptype impc:ir:pretty-print-type)


;; now with pretty print support
(define impc:ir:get-type-from-pretty-str
   (lambda (string-type . args)
      (if (or (not (string? string-type))
              (string=? "" string-type))
          (print-error 'Compiler 'Error: 'Internal 'error 'impc:ir:get-type-from-str 'must 'take 'a 'string 'not string-type))
      (let* ((ptr-depth (impc:ir:get-ptr-depth string-type))
             (offset (* ptr-depth 100))
             (expand-closures? (if (null? args) #f (car args)))
             (base (impc:ir:get-base-type string-type)))
         ;(print 'base: base 'ptr-depth: ptr-depth)
         (cond ((string=? base "void") *impc:ir:void*)
               ((string=? base "closure") (+ *impc:ir:closure* offset))
               ((string=? base "tuple") (+ *impc:ir:tuple* offset))
               ((regex:match? base "^\\[.*\\]$") 
                (cons (+ offset *impc:ir:closure*) (impc:ir:get-type-from-pretty-closure string-type)))               
               ((regex:match? base "\\<\\{\\s?i8\\*,\\s?i8\\*.*") 
                (cons (+ offset *impc:ir:closure*) (impc:ir:get-closure-type-from-str string-type)))
               ((regex:match? base "^\\<[^{].*[^}]\\>$") 
                (cons (+ offset *impc:ir:tuple*) (impc:ir:get-type-from-pretty-tuple string-type)))               
               ((regex:match? base "\\<?\\{.*\\}\\>?\\**")
                (cons (+ offset *impc:ir:tuple*) (impc:ir:get-tuple-type-from-str string-type)))               
               (else (let loop ((i -1))
                        (if (string=? base (impc:ir:get-type-str i))
                            (+ i offset)
                            (if (< i *impc:ir:lowest-base-type*)
                                (loop (+ i 1))
                                (print-error 'Compiler 'Error: 'cannot 'find 'type 'for 'string string-type)))))))))
								
								
(define impc:ir:convert-from-pretty-types
   (lambda (t)
      ;(print 't: t)
      (if (null? t) '()
          (if (atom? t)
              (impc:ir:get-type-from-pretty-str (sexpr->string t))
              (map (lambda (t)
                      (impc:ir:convert-from-pretty-types t))
                   t)))))								


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define impc:ir:regex-structs-or-atoms "(\\<?\\{(?<struct>[^<{}>]|\\<?\\{\\g<struct>*\\}\\>?\\**)*\\}\\>?\\**)|(?:([%0-9a-zA-Z]\\*?)+)")
(define impc:ir:regex-structs-or-atoms "(\\<?\\{(?<struct>[^<{}>]|\\<?\\{\\g<struct>*\\}\\>?\\**)*\\}\\>?\\**)|(?:([%0-9a-zA-Z]\\**)+)")


(define impc:ir:get-tuple-type-from-str
   (lambda (string-type)
      (let* ((s1 (regex:replace string-type "\\<?\\{(.*)\\}\\>?.*" "$1"))
             (t1 (cl:remove-if (lambda (x) (string=? x "")) 
                               (regex:match-all s1 impc:ir:regex-structs-or-atoms)))
             (t2 (map (lambda (x) (impc:ir:get-type-from-str x)) t1)))
         t2)))


(define impc:ir:get-closure-type-from-str
   (lambda (string-type)
      (impc:ir:get-function-type-from-str (regex:replace string-type "\\<\\{\\s?i8\\*,\\s?i8\\*,(.*)\\}\\>" "$1"))))


(define impc:ir:get-function-type-from-str   
   (lambda (string-type)
      (let* ((sk (regex:match-all string-type impc:ir:regex-structs-or-atoms))
             (ss (string-append "(" (car sk) " " (string-join (cdddr sk) " ")))
             (s2 (cl:remove-if (lambda (x) (string=? x "")) 
                               (regex:match-all ss impc:ir:regex-structs-or-atoms)))
             (s3 (map (lambda (x)
                         (impc:ir:get-type-from-str x))
                      s2)))
         s3)))


(define impc:ir:get-type-from-str
   (lambda (string-type . args)
      (if (or (not (string? string-type))
              (string=? "" string-type))
          (print-error 'Compiler 'Error: 'Internal 'error 'impc:ir:get-type-from-str 'must 'take 'a 'string 'not string-type))
      ;(print 'string-type: string-type)
      (let* ((ptr-depth (impc:ir:get-ptr-depth string-type))
             (offset (* ptr-depth 100))
             (expand-closures? (if (null? args) #f (car args)))
             (base (impc:ir:get-base-type string-type)))
         (cond ((string=? base "void") *impc:ir:void*)
               ((string=? base "closure") (+ *impc:ir:closure* offset))
               ((string=? base "tuple") (+ *impc:ir:tuple* offset))               
               ((regex:match? base "\\<\\{\\s?i8\\*,\\s?i8\\*.*") 
                (cons (+ offset *impc:ir:closure*) (impc:ir:get-closure-type-from-str string-type)))
               ((regex:match? base "\\<?\\{.*\\}\\>?\\**")
                (cons (+ offset *impc:ir:tuple*) (impc:ir:get-tuple-type-from-str string-type)))
               (else (let loop ((i -1))
                        (if (string=? base (impc:ir:get-type-str i))
                            (+ i offset)
                            (if (< i *impc:ir:lowest-base-type*)
                                (loop (+ i 1))
                                (print-error 'Compiler 'Error: 'cannot 'find 'type 'for 'string string-type)))))))))
																


(define impc:ir:get-type-str
   (lambda (type)
      (if (string? type) type
          (cond ((list? type) ;; must be a complex type
                 (cond ((impc:ir:closure? (car type))
                        (apply string-append "<{i8*, i8*, " (impc:ir:make-function-str (cdr type) #t) "*}>"
                               (make-list (impc:ir:get-ptr-depth (car type)) "*")))
                       ((impc:ir:tuple? (car type))
                        (apply string-append "<{" (string-join (map (lambda (x) (impc:ir:get-type-str x)) (cdr type)) ",") "}>"
                               (make-list (impc:ir:get-ptr-depth (car type)) "*")))
                       (else (print-error 'Compiler 'Error: 'bad 'complex 'type! type))))
                ((= type -1) "void")
                (else (let ((base (modulo type 100))
                            (ptr-depth (floor (/ type 100))))
                         (string-append (cond ((= base *impc:ir:double*) "double")
                                              ((= base *impc:ir:float*) "float")
                                              ((member base (list *impc:ir:si64* *impc:ir:ui64*)) "i64")
                                              ((member base (list *impc:ir:si32* *impc:ir:ui32*)) "i32")
                                              ((member base (list *impc:ir:si8* *impc:ir:ui8* *impc:ir:char*)) "i8")
                                              ((= base *impc:ir:i1*) "i1")
                                              (else (print-error 'Compiler 'Error: 'bad 'type 'getting 'type 'str type)))
                                        (apply string-append (make-list ptr-depth "*")))))))))

(define impc:ir:convert-types
   (lambda (t)
      ;(print 't: t)
      (if (null? t) '()
          (if (atom? t)
              (impc:ir:get-type-from-str (sexpr->string t))
              (map (lambda (t)
                      (impc:ir:convert-types t))
                   t)))))
                     

(define impc:ir:str-list-check
   (lambda (type)
      (cond ((string? type)
             (impc:ir:str-list-check (impc:ir:get-type-from-str type))) 
            ((list? type) ; complex type
             (cond ((impc:ir:closure? (car type)) (car type))
                   ((impc:ir:tuple? (car type)) (car type))
                   (else (print-error 'Compiler 'Error: 'Unknown 'complex 'type type))))
            (else type))))


(define impc:ir:get-type-size
   (lambda (type)
      (let ((t (impc:ir:str-list-check type)))
         (if (impc:ir:pointer? t) (/ (sys:pointer-size) 8) ;; in bytes not bits
             (cond ((member t (list *impc:ir:double* *impc:ir:si64* *impc:ir:ui64*)) 8) ; 8 byte stuff
                   ((member t (list *impc:ir:float* *impc:ir:si32* *impc:ir:ui32*)) 4) ; 4 byte stuff
                   ((member t (list *impc:ir:char* *impc:ir:si8* *impc:ir:ui8* *impc:ir:i1*)) 1) ; 1 bytes stuff
                   ((= t *impc:ir:closure*) (* (impc:ir:get-type-size "i8*") 3))
                   ((= t *impc:ir:tuple*) 
                    (apply + (map (lambda (x) (impc:ir:get-type-size x)) (cdr type))))
                   (else (print-error 'Compiler 'Error: 'bad 'type 'in 'get-size)))))))


(define impc:ir:pointer--
   (lambda (type)
      (if (impc:ir:closure? type)         
          (let ((nl (cl:copy-list (if (string? type) (impc:ir:get-type-from-str type) type))))
             (set-car! nl (- (impc:ir:str-list-check type) *impc:ir:pointer*))
             nl)
          (- (impc:ir:str-list-check type) *impc:ir:pointer*))))

(define impc:ir:pointer++
   (lambda (type)
      (if (or (impc:ir:closure? type)
              (impc:ir:tuple? type))
          (let ((nl (cl:copy-list (if (string? type) (impc:ir:get-type-from-str type) type))))
             (set-car! nl (+ (impc:ir:str-list-check type) *impc:ir:pointer*))
             nl)
          (+ (impc:ir:str-list-check type) *impc:ir:pointer*))))

(define impc:ir:type?
   (lambda (type)
      (if (string? type)          
          (impc:ir:type? (impc:ir:get-type-from-str type))
          (cond ((null? type) #f)
                ((and (number? type) (< type 1001)) #t)
                ((and (list? type)
                      (number? (car type))
                      (or (impc:ir:closure? type)
                          (impc:ir:tuple? type))
                      (cl:every (lambda (x) x) 
                                (map (lambda (a) (impc:ir:type? a)) (cdr type))))
                 #t)
                (else #f))))) 

(define impc:ir:other?
   (lambda (type)
      (let ((t (impc:ir:str-list-check type)))
         (if (= t *impc:ir:other*) #t #f))))

(define impc:ir:pointer?
   (lambda (type)
      (let ((t (impc:ir:str-list-check type)))
         (if (>= t *impc:ir:other*) #f
             (if (>= t *impc:ir:pointer*) #t #f)))))

(define impc:ir:scalar?
   (lambda (type)
      (not (impc:ir:pointer? type))))


(define impc:ir:tuple?
   (lambda (type)
      (if (list? type)
          (if (< (length type) 2) #f
              (if (impc:ir:tuple? (car type)) #t #f))
          (if (= (modulo (impc:ir:str-list-check type) *impc:ir:pointer*) *impc:ir:tuple*) #t #f))))


(define impc:ir:closure?
   (lambda (type)
      (if (list? type)
          (if (< (length type) 2) #f
              (if (impc:ir:closure? (car type)) #t #f))
          (if (= (modulo (impc:ir:str-list-check type) *impc:ir:pointer*) *impc:ir:closure*) #t #f))))

(define impc:ir:signed?
   (lambda (type)      
      (let* ((t (impc:ir:str-list-check type)))
         (if (>= t *impc:ir:other*) #f
             (if (member (modulo t *impc:ir:pointer*) 
                         (list *impc:ir:si64* *impc:ir:si32* *impc:ir:si8*)) 
                 #t #f)))))

(define impc:ir:floating-point?
   (lambda (type)
      (let ((t (impc:ir:str-list-check type)))
         (if (>= t *impc:ir:other*) #f
             (if (member (modulo t *impc:ir:pointer*) 
                         (list *impc:ir:double* *impc:ir:float*)) 
                 #t #f)))))

(define impc:ir:fixed-point?
   (lambda (type)
      (let ((t (modulo (impc:ir:str-list-check type) *impc:ir:pointer*)))
         (if (member t (list *impc:ir:si64* *impc:ir:si32* *impc:ir:si8*
                             *impc:ir:ui64* *impc:ir:ui32* *impc:ir:ui8*))
             #t #f))))

(define impc:ir:number?
   (lambda (type)
      (or (impc:ir:floating-point? type)
          (impc:ir:fixed-point? type))))

(define impc:ir:pointer?
   (lambda (type)
      (let ((t (impc:ir:str-list-check type)))
         (if (>= t *impc:ir:pointer*) #t #f))))

(define impc:ir:void?
   (lambda (type)
      (if (= (impc:ir:str-list-check type) *impc:ir:void*) #t #f)))

(define impc:ir:strip-space
   (lambda (str)
      (string-strip str)))


(define impc:ir:make-function-str
   (lambda (t . with-env-added?)
      (let* ((os (make-string 0))
             (args (cdr t)))
	(if (car with-env-added?) 
	    (emit (string-append (impc:ir:get-type-str (car t)) 
				 " (i8*, i8*"
				 (if (null? args) 
				     "" 
				     ", "))
		  os)
	    (emit (string-append (impc:ir:get-type-str (car t)) " (") os))
	(dotimes (i (length args))
		 (if (> i 0) (emit ", " os))
		 (if (symbol? (list-ref args i))
		     (print-error 'Compiler 'Error: 'Unresolved 'Type: (list-ref args i)))
		 (emit (impc:ir:get-type-str (list-ref args i)) os))
	(emit ")" os) 	 
	(impc:ir:strip-space os))))


(define impc:ir:make-struct-str
   (lambda (t)
      (let* ((os (make-string 0))
             (args t))
         (emit "<{" os)
         (dotimes (i (length args))
            (if (> i 0) (emit ", " os))
            (emit (impc:ir:get-type-str (cdr (list-ref args i))) os))
         (emit "}>" os) 
         (impc:ir:strip-space os))))
		 
		 
(define impc:ir:make-struct-str-env
   (lambda (t)
      (let* ((os (make-string 0))
             (args t))
         (emit "<{" os)
         (dotimes (i (length args))
            (if (> i 0) (emit ", " os))
            (emit (string-append (impc:ir:get-type-str (cdr (list-ref args i))) "*") os))
         (emit "}>" os) 
         (impc:ir:strip-space os))))		 
		 

(define impc:ir:make-arglist-str
   (lambda (args . with-symbol?)
      (let* ((os (make-string 0)))
         (dotimes (i (length args))
            (if (> i 0) (emit ", " os))
            (let ((arg (list-ref args i)))
               ;(print 'arg: arg)
               (emit (impc:ir:get-type-str (cdr arg)) os)
               (if (car with-symbol?)
                   (emit (string-append " %" (symbol->string (car arg))) os))))
         (impc:ir:strip-space os))))


(define impc:ir:make-string
   (lambda (ast)
      (let* ((os (make-string 0))
             (cnt 0))
         (emit (string-append (impc:ir:gname "string" "i8*") " = call i8* @llvm_zone_malloc(%mzone* %_zone, i64 "
                                 (number->string (+ 1 (string-length ast))) ")\n") os)         
         (define strname (car (impc:ir:gname)))
         (for-each (lambda (char)
                      (emit (string-append (impc:ir:gname "val" "i8*") " = "
                                              "getelementptr i8* " strname
                                              ", i32 " (number->string cnt) "\n") os)
                      (emit (string-append "store i8 " (number->string (char->integer char)) 
                                              ", i8* " (car (impc:ir:gname)) "\n") os)
                      (set! cnt (+ cnt 1)))
                   (append (string->list ast) (list (integer->char 0))))
         (impc:ir:gname (- (impc:ir:gname "string")))         
         (impc:ir:strip-space os)))) 

			   
			   
;; this looks scary but it's basically all just
;; making and filling an environment structure
;; for a particular closure
(define impc:ir:compile:make-closureenv
   (lambda (ast types)
      ;(print 'make-closure-env 'ast: ast 'types: types)
      (let* ((os2 (make-string 0))
             (os1 (make-string 0))
             (name (list-ref ast 1)) 
             (rettype (list-ref ast 2))
             (env (list-ref ast 3))
             (args (list-ref ast 4))
             (code (list-ref ast 5)))
         
         (define func-type-str (impc:ir:make-function-str (list* rettype (map (lambda (x) (cdr x)) args)) #t))
         
         (define closure-struct-str
            (string-append "<{ i8*, i8*, " func-type-str "*}>"))
         
         ;; malloc closure structure
         (emit "; malloc closure structure\n" os2)
         (define cstruct closure-struct-str)
         (emit (string-append (impc:ir:gname "val" "i8*") " = getelementptr " cstruct "* null, i32 1\n") os2)
         (emit (string-append (impc:ir:gname "size" "i64") " = ptrtoint " cstruct "* " (car (impc:ir:gname 1)) " to i64\n") os2)         
         (emit (string-append (impc:ir:gname "clsptr" "i8*") " = call i8* @llvm_zone_malloc("
                                 "%mzone* %_zone, i64 " (car (impc:ir:gname (impc:ir:gname "size"))) ")\n") os2)         
         (emit (string-append (impc:ir:gname "closure" (string-append cstruct "*")) 
                                 " = bitcast i8* " (car (impc:ir:gname (impc:ir:gname "clsptr"))) 
                                 " to " cstruct "*\n") os2)
         
         ;; malloc evironment structure 
         (emit "\n; malloc environment structure\n" os2)
         (define estruct (impc:ir:make-struct-str-env env))
         (emit (string-append (impc:ir:gname "val" "i8*") " = getelementptr " estruct "* null, i32 1\n") os2)
         (emit (string-append (impc:ir:gname "size" "i64") " = ptrtoint " estruct "* " (car (impc:ir:gname 1)) " to i64\n") os2)
         (emit (string-append (impc:ir:gname "envptr" "i8*") " = call i8* @llvm_zone_malloc("
                                 "%mzone* %_zone, i64 " (car (impc:ir:gname 1)) ")\n") os2)         
         (emit (string-append (impc:ir:gname "environment" (string-append estruct "*")) 
                                 " = bitcast i8* " (car (impc:ir:gname (impc:ir:gname "envptr"))) 
                                 " to " estruct "*\n") os2)
         
         ;; make new closure_address_table
         (emit "\n; malloc closure address table\n" os2)
         (emit (string-append (impc:ir:gname "addytable" "%clsvar*") " = call %clsvar* @new_address_table()\n") os2)
         (define table (impc:ir:gname))
         (define ptridx 0)
         (dotimes (i (length env))
            (let* ((e (list-ref env i))
                   (name-str (impc:ir:make-string (symbol->string (car e))))
                   (name (impc:ir:gname))
                   (type-str (impc:ir:make-string (impc:ir:get-type-str (cdr e))))
                   (type (impc:ir:gname)))
               (emit name-str os2)
               (emit type-str os2)
               (emit (string-append (impc:ir:gname "addytable" "%clsvar*") 
                                       " = call %clsvar* @add_address_table("
                                       (cadr name) " " (car name) ", "
                                       "i32 " (number->string ptridx) ", "
                                       (cadr type) " " (car type) ", "
                                       "%clsvar* " (car table) ")\n")
                        os2)
               (set! table (impc:ir:gname))
               (set! ptridx (+ ptridx (/ (sys:pointer-size) 8))))) ; need it as bytes
         (emit (string-append (impc:ir:gname "address-table" "i8*") " = bitcast %clsvar* " (car table) " to i8*\n") os2)
         
         ;; add data to environment structure
         (emit "; add data to environment\n" os1)
         (dotimes (i (length env))
            (let* ((e (list-ref env i))
                   (alloc? (not (regex:match? (symbol->string (car e)) "__sub$"))))
               ;(print 'e: (car e) 'alloc: alloc? (regex:match? (symbol->string (car e)) "__sub$"))                 
               ;; first fixup mangled name if not allocing
               (if (not alloc?) (set! e (cons (string->symbol (car (regex:split (symbol->string (car e)) "__sub$")))
                                              (cdr e))))
               (define t (begin (impc:ir:gname (string-append (symbol->string (car e)) "EnvPtr")
                                               (string-append (impc:ir:get-type-str (cdr e)) "*"))
                                (impc:ir:gname)))
               ;; if allocating then assign heap memory
               (if alloc?
                   (begin (emit (string-append ";need to alloc memory for this env var " (symbol->string (car e)) "\n"
                                                  (impc:ir:gname "mem" "i8*") " = call i8* @llvm_zone_malloc("
                                                  "%mzone* %_zone, i64 " 
                                                  (number->string (impc:ir:get-type-size (cdr e)))
                                                  ")\n") os1)
                          (emit (string-append "; and store value in new memory\n"
                                                  ;(impc:ir:gname "memstore" (cadr t))
                                                  (car t)
                                                  " = bitcast i8* " (car (impc:ir:gname (impc:ir:gname "mem"))) " to " (cadr t) "\n") os1)
                          (emit (string-append  (impc:ir:gname "val" (impc:ir:get-type-str (cdr e)))
                                                   " = load " (impc:ir:get-type-str (cdr e)) "* %"
                                                   (symbol->string (car e)) "Ptr\n") os1)
                          (emit (string-append "store " (impc:ir:get-type-str (cdr e)) " " (car (impc:ir:gname))
                                                  ", " ;(impc:ir:get-type-str (cdr e))
                                                  (cadr t) " " (car t) "\n") os1)
                          (emit (string-append (impc:ir:gname "tmp_envptr" (string-append (cadr t) "*")) " = getelementptr "
                                                  estruct "* " (car (impc:ir:gname (impc:ir:gname "environment"))) ", "
                                                  "i32 0, i32 " (number->string i) "\n") os1)
                          (emit (string-append "store " (cadr t) " " (car t) " "
                                                  ", " (cadr t) "* " (car (impc:ir:gname (impc:ir:gname "tmp_envptr"))) "\n\n") os1))
                   (begin (emit (string-append "; don't need to alloc for env var " (symbol->string (car e)) "\n") os1)                          
                          (emit (string-append (impc:ir:gname "tmp_envptr" (string-append (cadr t) "*")) " = getelementptr "
                                                  estruct "* " (car (impc:ir:gname (impc:ir:gname "environment"))) ", "
                                                  "i32 0, i32 " (number->string i) "\n") os1)
                          (emit (string-append "store " (cadr t) " %" (symbol->string (car e)) "Ptr"
                                                  ", "
                                                  (cadr t) "* " (car (impc:ir:gname (impc:ir:gname "tmp_envptr"))) "\n\n") os1)))))
         (emit "\n" os1)
         
         ;(emit (string-append "call void @testtest(i8* " (car (impc:ir:gname (impc:ir:gname "envptr"))) ")\n") os1)
         
         ;; add ftype string to provide type info to scheme world.
         ;(emit (impc:ir:make-string name) os2)
         
         ;; insert stuff into closure
         (emit "\n; insert table, function and environment into closure struct\n" os2)
         (emit (string-append (impc:ir:gname "closure.table" "i8**") " = getelementptr " cstruct "* "
                                 (car (impc:ir:gname (impc:ir:gname "closure"))) ", i32 0, i32 0\n") os2)
         (emit (string-append "store i8* " (car (impc:ir:gname (impc:ir:gname "address-table"))) ", i8** " 
                                 (car (impc:ir:gname)) "\n") os2)
         (emit (string-append (impc:ir:gname "closure.env" "i8**") " = getelementptr " cstruct "* " 
                                 (car (impc:ir:gname (impc:ir:gname "closure"))) ", i32 0, i32 1\n") os2)
         (emit (string-append "store i8* " (car (impc:ir:gname (impc:ir:gname "envptr"))) ", i8** " 
                                 (car (impc:ir:gname)) "\n") os2)
         (emit (string-append (impc:ir:gname "closure.func" (string-append func-type-str "**")) 
                                 " = getelementptr " cstruct "* " (car (impc:ir:gname (impc:ir:gname "closure"))) 
                                 ", i32 0, i32 2\n") os2)
         (emit (string-append "store " func-type-str "* @" name ", " func-type-str "** " (car (impc:ir:gname)) "\n") os2)
         (impc:ir:gname (- (impc:ir:gname "closure")))
         (cons (impc:ir:strip-space os2)
               (impc:ir:strip-space os1)))))
			   

(define impc:ir:compile:make-closure   
   (lambda (ast types)
      (let* ((os (make-string 0))
             (name (list-ref ast 1)) 
             (rettype (list-ref ast 2))
             (env (list-ref ast 3))
             (args (list-ref ast 4))
             (code (list-ref ast 5)))
         ;(print 'making-closure--------------------------------------> )
         ;(print 'name: name)
         ;(print 'rettype: rettype)
         ;(print 'env: env)
         ;(print 'args: args)
         ;(print 'code: code)
         ;; first we make the function code
         ;; define fastcc function with return type
         (emit (string-append "define fastcc " (impc:ir:get-type-str rettype) " @" name "(") os)
         ;(if (not (null? env)) (emit "i8* %_impenv" os))
         (emit "i8* %_impz," os)
         (emit "i8* %_impenv" os)         
         (if (not (null? args)) (emit (string-append ", " (impc:ir:make-arglist-str args #t)) os))
         ;; close off function opening         
         (emit ") {\n" os)
         (emit "entry:\n" os)
         (emit "; setup zone\n" os)
         (emit "%_zone = bitcast i8* %_impz to %mzone*\n" os)
         ;; first we need to pull evironment values
         (if (not (null? env))
             (begin (emit "; setup environment\n" os)
                    (emit (string-append "%impenv = bitcast i8* %_impenv to "
                                            (impc:ir:make-struct-str-env env) "*\n") os)
                    (dotimes (i (length env))
                       (let ((e (list-ref env i)))
                          ;; need to strip __sub's
                          (if (regex:match? (symbol->string (car e)) "__sub$")
                              (set! e (cons (string->symbol (car (regex:split (symbol->string (car e)) "__sub$")))
                                            (cdr e))))
                          ;(print 'e: e)
                          (emit (string-append "%" (symbol->string (car e)) "Ptr_ = getelementptr "
                                                  (impc:ir:make-struct-str-env env) "* %impenv, "
                                                  "i32 0, i32 " (number->string i) "\n") os)
                          (emit (string-append "%" (symbol->string (car e)) "Ptr = load "
                                                  (impc:ir:get-type-str (cdr e)) "** %"
                                                  (symbol->string (car e)) "Ptr_\n") os)))))                       
                       
         ;; next we pull the function arguments
         (emit "\n; setup arguments\n" os)          
         (dotimes (i (length args))
            (let* ((a (list-ref args i)))
               (emit (string-append "%" (symbol->string (car a)) "Ptr = alloca " (impc:ir:get-type-str (cdr a)) "\n") os)
               (emit (string-append "store " (impc:ir:get-type-str (cdr a)) " %" (symbol->string (car a))
                                       ", " (impc:ir:get-type-str (cdr a)) "* %" (symbol->string (car a)) "Ptr\n") os)))
         (emit "\n" os)
         ;; compile body
         (emit (impc:ir:compiler code types) os)         
         ;(emit (string-append "ret " (impc:ir:get-type-str rettype) " " (car (impc:ir:gname)) "\n") os)
         (emit "}" os) 
         (impc:ir:strip-space os))))


(define impc:ir:compile:make-env
   (lambda (ast types)
      (let* ((os (make-string 0))
             (env-zone-tmp '()))
         (map (lambda (p)   
                 ;(print 'p: p 'ast: ast)
                 (let* ((symstr (symbol->string (caar p)))
                        (symtype (cdr (assoc (caar p) types)))
                        (value (impc:ir:compiler (cadr p) types symtype) os)                                                
                        (typestr (cadr (impc:ir:gname))))
                    (if (and (number? (cadr p)) ;; if numeric constant force to type of symbol
                             (impc:ir:number? (cdr (assoc (caar p) types)))
                             (impc:ir:number? (impc:ir:get-type-from-str typestr)))
                        (set! typestr (impc:ir:get-type-str (cdr (assoc (caar p) types)))))
                    ;; type check       
                    ;(print typestr 'a: (impc:ir:get-type-from-str typestr) 'b: (cdr (assoc (caar p) types)))                    
                    (if (not (equal? (impc:ir:get-type-from-str typestr) ;; check to see if the two types are equal?
                                     (cdr (assoc (caar p) types))))
                        (print-error 'Compiler 'Error:  'Type 'Mismatch 'for 'symbol 'in (cadr p) 'symbol: (symbol->string (caar p))
                                     (string->symbol (impc:ir:get-type-str (cdr (assoc (caar p) types)))) 
                                     'does 'not 'match 'use 'of 
                                     (string->symbol typestr)))
                    ;(print 'value: value 'cadrp (cadr p))
                    ;(print 'ts: typestr 'ss: symstr)
                    (emit (string-append "\n; let assign value to symbol " symstr "\n") os)
                    ;(emit (impc:ir:compiler (cadr p) types) os)
                    (if (pair? value)
                        (emit (car value) os)
                        (emit value os))
                    ;; this bitcast should be the same type on both sides                    
                    (emit "\n; let value assignment\n" os)
                    (emit (string-append "%" symstr " = bitcast " typestr " " 
                                            (if (and (number? (cadr p))
                                                     (= *impc:ir:float* (impc:ir:get-type-from-str typestr)))
                                                (llvm:convert-float (car (impc:ir:gname)))
                                                (car (impc:ir:gname)))
                                            " to " typestr "\n") os)
                    (emit (string-append "%" symstr "Ptr = alloca " typestr "\n") os)
                    (emit (string-append "store " typestr " %" symstr ", " 
                                            typestr "* %" symstr "Ptr\n\n") os)
                    (if (pair? value)
                        (emit (cdr value) os))))                 
              (cadr ast))
         (emit (impc:ir:compiler (cddr ast) types) os)
         (impc:ir:strip-space os))))


(define impc:ir:compiler:closure-from-getter
   (lambda (name)
      (if (not (llvm:get-function (string-append name "_getter")))
          (print-error 'Compiler 'Error: 'no 'global 'closure 'named name)
          (let* ((os (make-string 0))
                 (type (cons (+ *impc:ir:pointer* *impc:ir:closure*) 
                             (map (lambda (x) (impc:ir:get-type-from-str x))
                                  (llvm:get-function-args-withoutzone name)))))
             (emit (string-append (impc:ir:gname "closure" "i8*") " = call i8* @" name "_getter()\n") os)
             (emit (string-append (impc:ir:gname "closure" (impc:ir:get-type-str type)) " = bitcast i8* " 
                                     (car (impc:ir:gname 1)) " to " (impc:ir:get-type-str type) "\n") os)
             (impc:ir:strip-space os)))))


(define impc:ir:compiler:closure-ref
   (lambda (ast types)
      ;; arg 1 must be a closure
      ;; arg 2 must be a string
      ;; arg 3 must be a string
      (let* ((os (make-string 0))
             ;(closure-str (impc:ir:compiler (cadr ast) types))
             (closure-str (if (and (symbol? (cadr ast))
                                   (llvm:get-function (symbol->string (cadr ast))))
                              (impc:ir:compiler:closure-from-getter (symbol->string (cadr ast)))
                              (impc:ir:compiler (cadr ast) types)))             
             (closure (impc:ir:gname))
             (name-str (impc:ir:compiler (caddr ast) types))
             (name (impc:ir:gname))
             (type-str (impc:ir:compiler (cadddr ast) types))
             (type (impc:ir:gname)))
         (emit "\n; closure ref \n" os)         
         (emit closure-str os)
         (emit name-str os)
         (emit type-str os)
         (emit (string-append (impc:ir:gname "tablePtr" (cadr closure)) " = getelementptr "
                                 (cadr closure) " " (car closure) ", i32 0, i32 0\n") os)         
         (emit (string-append (impc:ir:gname "tmp" "%clsvar**") " = bitcast i8** " 
                                 (car (impc:ir:gname 1)) " to %clsvar**\n") os)
         (emit (string-append (impc:ir:gname "table" "%clsvar*")
                                 " = load %clsvar** " (car (impc:ir:gname 1)) "\n") os)
         (emit (string-append (impc:ir:gname "ePtr" "i8**") " = getelementptr "
                                 (cadr closure) " " (car closure) ", i32 0, i32 1\n") os)
         (define ePtr (impc:ir:gname))         
         (emit (string-append (impc:ir:gname "e" "i8*")
                                 " = load i8** " (car (impc:ir:gname (impc:ir:gname "ePtr"))) "\n") os)
         (define e (impc:ir:gname))
         (emit (string-append (impc:ir:gname "offset" "i32")
                                 " = call i32 @get_address_offset(i8* "
                                 (car name) ", %clsvar* " (car (impc:ir:gname (impc:ir:gname "table"))) ")\n") os)
         (define offset (impc:ir:gname))
         (emit (string-append (impc:ir:gname "valPtr" "i8*") " = getelementptr " 
                                 (cadr e) " " (car e) ", i32 " (car offset) "\n") os)
         (emit (string-append (impc:ir:gname "val" "i8**") " = bitcast i8* " (car (impc:ir:gname 1)) " to i8**\n") os)
         (emit (string-append (impc:ir:gname "val" "i8*") " = load i8** " (car (impc:ir:gname 1)) "\n") os)
         (emit (string-append (impc:ir:gname "val" (string-append (cadddr ast) "*")) " = bitcast i8* " (car (impc:ir:gname 1)) " to "
                                 (string-append (cadddr ast) "*\n")) os)
         (emit (string-append (impc:ir:gname "val" (cadddr ast)) " = load " (cadddr ast) "* " (car (impc:ir:gname 1)) "\n") os)
         (impc:ir:strip-space os))))


(define impc:ir:compiler:closure-set
   (lambda (ast types)
      ;; arg 1 must be a closure
      ;; arg 2 must be a string
      ;; arg 3 must be a string
      ;; arg 4 must be of type!      
      (let* ((os (make-string 0))
             (closure-str (if (and (symbol? (cadr ast))
                                   (llvm:get-function (symbol->string (cadr ast))))
                              (impc:ir:compiler:closure-from-getter (symbol->string (cadr ast)))
                              (impc:ir:compiler (cadr ast) types)))
             (closure (impc:ir:gname))
             (name-str (impc:ir:compiler (caddr ast) types))
             (name (impc:ir:gname))
             (type-str (impc:ir:compiler (cadddr ast) types))
             (type (impc:ir:gname))
             (val-str (impc:ir:compiler (car (cddddr ast)) types (impc:ir:get-type-from-str (cadddr ast))))
             (val (impc:ir:gname)))
         (emit "\n; closure set! \n" os)
         (emit closure-str os)
         (emit name-str os)
         (emit type-str os)     
         (emit val-str os)    
         (emit (string-append (impc:ir:gname "tablePtr" (cadr closure)) " = getelementptr "
                                 (cadr closure) " " (car closure) ", i32 0, i32 0\n") os)         
         (emit (string-append (impc:ir:gname "tmp" "%clsvar**") " = bitcast i8** " 
                                 (car (impc:ir:gname 1)) " to %clsvar**\n") os)
         (emit (string-append (impc:ir:gname "table" "%clsvar*")
                                 " = load %clsvar** " (car (impc:ir:gname 1)) "\n") os)
         (emit (string-append (impc:ir:gname "ePtr" "i8**") " = getelementptr "
                                 (cadr closure) " " (car closure) ", i32 0, i32 1\n") os)
         (define ePtr (impc:ir:gname))         
         (emit (string-append (impc:ir:gname "e" "i8*")
                                 " = load i8** " (car (impc:ir:gname (impc:ir:gname "ePtr"))) "\n") os)
         (define e (impc:ir:gname))
         (emit (string-append (impc:ir:gname "offset" "i32")
                                 " = call i32 @get_address_offset(i8* "
                                 (car name) ", %clsvar* " (car (impc:ir:gname (impc:ir:gname "table"))) ")\n") os)
         (define offset (impc:ir:gname))
         (emit (string-append (impc:ir:gname "valPtr" "i8*") " = getelementptr " 
                                 (cadr e) " " (car e) ", i32 " (car offset) "\n") os)
         (emit (string-append (impc:ir:gname "val" "i8**") " = bitcast i8* " (car (impc:ir:gname 1)) " to i8**\n") os)
         (emit (string-append (impc:ir:gname "val" "i8*") " = load i8** " (car (impc:ir:gname 1)) "\n") os)
         (emit (string-append (impc:ir:gname "val" (string-append (cadddr ast) "*")) " = bitcast i8* " (car (impc:ir:gname 1)) " to "
                                 (string-append (cadddr ast) "*\n")) os)
         (emit (string-append "store " (cadr val) " " (car val) ", " (cadr (impc:ir:gname)) " " (car (impc:ir:gname)) "\n") os)
         (emit (string-append (impc:ir:gname "result" (cadr val)) " = load " (cadr (impc:ir:gname 1)) " " (car (impc:ir:gname 1)) "\n") os)  
         (impc:ir:strip-space os))))


;; (impc:ir:gcnt [increment])
;; (impc:ir:gcnt) ;; get current cnt
;; (impc:ir:gcnt 2) ;; increment current cnt by 2
(define impc:ir:gcnt
   (let ((cnt 0)) 
      (lambda args
         (if (not (null? args))
             (set! cnt (+ (car args) cnt)))
         cnt)))


;; impc:ir:gname
;; usage:
;; (gname [previous name]) ;; find distance to this
;; (gname "fred" type) ;; set current name
;; (gname) ;; get current name
;; (gname 2) ;; get name 2 previous
;; (gname -2) ;; revert to name 2 previous
(define impc:ir:gname
   (let ((prev '()))
      (lambda args
         (if (null? args)
             (cdr (list-ref prev 0))
             (if (> (length args) 1)
                 (let ((str (if (or (number? (string->atom (car args)))
                                    (regex:match? (car args) "^0x"))
                                (car args)
                                (string-append "%" (car args)
                                               (number->string (llvm:count++))))))
                    (begin (set! prev (cons (list (car args) str (cadr args)) prev))
                           (cadr (list-ref prev 0))))
                 (if (number? (car args))
                     (if (< (car args) 0)
                         (begin (set! prev (cons (list-ref prev (abs (car args))) prev))
                                (cdr (list-ref prev 0)))
                         (cdr (list-ref prev (car args))))
                     (if (assoc (car args) prev)
                         (cl:position (assoc (car args) prev) prev)
                         '())))))))


(define impc:ir:compile:eval-var
   (lambda (var t)
      (let* ((os (make-string 0)))
         (let ((typestr (impc:ir:get-type-str t)))
            (emit (string-append (impc:ir:gname "val" typestr) 
                                    " = load " typestr 
                                    "* %" (symbol->string var) "Ptr\n") os)
            (impc:ir:strip-space os)))))


(define impc:ir:compile:eval-gvar
   (lambda (var)
      (let* ((os (make-string 0))
             (type (impc:ir:get-type-from-str (llvm:get-global-variable-type (symbol->string var))))
             (type2 (impc:ir:pointer-- type))
             (typestr (impc:ir:get-type-str type2)))
         (emit (string-append (impc:ir:gname "val" typestr) 
                                 " = load " typestr 
                                 "* @" (symbol->string var) "\n") os)
         (impc:ir:strip-space os))))




(define impc:ir:compile:apply-closure
   (lambda (ast types ftype-provided?)
      ;(print 'apply-closure ast types ftype-provided?)
      (let* (             (functiontype (if ftype-provided? 
                               (cdr (impc:ir:get-type-from-str (cadar ast)))
                               (if (and (assoc (car ast) types)
                                        (impc:ir:closure? (cdr (assoc (car ast) types)))
                                        (= 1 (impc:ir:get-ptr-depth (cdr (assoc (car ast) types)))))
                                   (cddr (assoc (car ast) types))
                                   (print-error "Compiler error: Bad type for closure!" (car ast)))))
             (os (make-string 0))
             (ftype (impc:ir:make-function-str functiontype #t))
             (clstype (string-append "<{i8*, i8*, " ftype "*}>*"))
             (vars (map (lambda (arg hint)
                           (emit (impc:ir:compiler arg types hint) os)
                           (impc:ir:gname))
                        (cdr ast)
                        (cdr functiontype))))
         (emit "\n; apply closure \n" os)
         (define v '())
         (if ftype-provided?
             (set! v (caar ast))
             (begin (emit (string-append (impc:ir:gname "val" clstype) " = load " clstype "* %"
                                            (symbol->string (car ast)) "Ptr\n") os)
                    (set! v (car (impc:ir:gname (impc:ir:gname "val"))))))
         (emit (string-append (impc:ir:gname "fPtr" clstype)
                                 " = getelementptr " clstype " " v ", i32 0, i32 2\n") os)
         (emit (string-append (impc:ir:gname "ePtr" clstype) " = getelementptr "
                                 clstype " " v ", i32 0, i32 1\n") os)
         (emit (string-append (impc:ir:gname "f" (string-append ftype "*"))
                                 " = load " ftype "** " (car (impc:ir:gname (impc:ir:gname "fPtr"))) "\n") os)
         (emit (string-append (impc:ir:gname "e" "i8*")
                                 " = load i8** " (car (impc:ir:gname (impc:ir:gname "ePtr"))) "\n") os)
         (emit (string-append (impc:ir:gname "z" "i8*") " = bitcast %mzone* %_zone to i8*\n") os)
         (define zone (car (impc:ir:gname)))
         (emit (string-append (if (impc:ir:void? (car functiontype))
                                     (begin (impc:ir:gname "result" "void") "")
                                     (string-append (impc:ir:gname "result" (impc:ir:get-type-str (car functiontype))) " = "))
                                 "tail call fastcc "
                                 (impc:ir:get-type-str (car functiontype)) " "
                                 (car (impc:ir:gname (impc:ir:gname "f"))) "(i8* " zone ", i8* "
                                 (car (impc:ir:gname (impc:ir:gname "e"))) 
                                 (apply string-append (map (lambda (var)
                                                              (string-append ", " (cadr var) " " (car var)))
                                                           vars))
                                 ")\n") os)
         (impc:ir:strip-space os))))


(define impc:ir:compiler:loop
   (lambda (ast types)
      (let* ((os (make-string 0))
             (loop-num (llvm:count++))
             (loop-label (string-append "loop" (number->string loop-num) ":"))
             (closeloop-label (string-append "closeloop" (number->string loop-num) ":"))
             (after-label (string-append "after" (number->string loop-num) ":"))
             (loop (string-append "%loop" (number->string loop-num)))
             (closeloop (string-append "%closeloop" (number->string loop-num)))
             (cmp (string-append "%cmp" (number->string loop-num)))
             (after (string-append "%after" (number->string  loop-num)))
             (iterator (string-append "%" (symbol->string (caar ast)))) ; "Loop" (number->string loop-num)))
             (iterator-type (impc:ir:get-type-str (cdr (assoc (caar ast) types))))
             (numstr (impc:ir:compiler (cadar ast) types (cdr (assoc (caar ast) types))))                     
             (num (impc:ir:gname)) ;(ir:eval (cadar ast) os stack sym-table))
             (bodystr (impc:ir:compiler (cdr ast) types)))
         (emit "; setup loop\n" os)
         ;(print num 'numstr numstr)
         (emit numstr os)         
         (emit (string-append iterator "Ptr = alloca " iterator-type "\n") os)
         (emit (string-append "store " iterator-type (if (impc:ir:fixed-point? (cadr num))
							    " 0, "
							    " 0.0, ")
				 iterator-type "* " iterator "Ptr\n") os)
         (emit (string-append "br label " loop "\n") os)
         (emit (string-append "\n" loop-label "\n") os)
         (emit bodystr os)
         (emit (string-append "%loop_cnt" (number->string loop-num) 
                                 " = load " iterator-type "* " iterator "Ptr\n") os)
         (emit (string-append "%next" (number->string loop-num)
                                 (if (impc:ir:fixed-point? (cadr num))
				     " = add " 
				     " = fadd ")
				 iterator-type " %loop_cnt" (number->string loop-num) 
				 (if (impc:ir:fixed-point? (cadr num))
				     ", 1\n"
				     ", 1.0\n"))
		  os)
         (emit (string-append "store " iterator-type " %next" (number->string loop-num) 
                                     ", " iterator-type "* " iterator "Ptr\n") os) 
         (emit (string-append cmp " = "
                                 (if (impc:ir:fixed-point? (cadr num))
                                     "icmp ult "
                                     "fcmp ult ")
                                 (cadr num) " "
                                 "%next" (number->string loop-num) ", " (car num) "\n") os)
         (emit (string-append "br i1 " cmp ", label " loop ", label " after "\n") os)         
         (emit (string-append "\n" after-label "\n") os)
         (impc:ir:gname "voidmark" (impc:ir:get-type-str *impc:ir:void*))		 
         (impc:ir:strip-space os))))


(define impc:ir:compiler:set!
   (lambda (ast types)
      (let* ((os (make-string 0))
             (s2 (impc:ir:compiler (caddr ast) types))
             (vv (impc:ir:gname)))
         (emit s2 os)
         (emit (string-append "store " (cadr vv) " " (car vv) ", " (cadr vv) "* %" 
                                 (symbol->string (cadr ast)) "Ptr\n") os)
         (impc:ir:strip-space os))))          


(define impc:ir:compiler:make-array
   (lambda (ast types)
      (let* ((os (make-string 0)))
         (let* ((idx-str (impc:ir:compiler (cadr ast) types))
                (idx (impc:ir:gname))
                ;(t (impc:ir:get-type-from-str (cadr (impc:ir:gname))))
                (t (impc:ir:get-type-from-str (cadr idx)))
                (tt (impc:ir:convert-from-pretty-types (caddr ast))))
            (if (not (impc:ir:fixed-point? t))
                (print-error 'Compiler 'Error: 'Type 'Mismatch: ast 'size 'must 'be 'fixed-point 'not (impc:ir:get-type-str t)))                         
            (emit idx-str os)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (if (not (member t (list *impc:ir:si64* *impc:ir:ui64*)))
                (begin (emit (string-append (impc:ir:gname "tmp" "i64") " = zext " (impc:ir:get-type-str t) " "
                                               (car idx) " to i64\n") os)
                       (set! idx (impc:ir:gname))))
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (emit (string-append (impc:ir:gname "size" "i64") " = mul i64 "
                                    (number->string (impc:ir:get-type-size tt)) ", " 
                                    (car idx) "\n") os)
            (emit (string-append (impc:ir:gname "dat" "i8*") " = call i8* @llvm_zone_malloc(%mzone* %_zone, i64 "
                                    (car (impc:ir:gname (impc:ir:gname "size")))
                                    ")\n") os)                
            (emit (string-append (impc:ir:gname "val" (string-append (impc:ir:get-type-str tt) "*"))
                                    " = bitcast i8* " (car (impc:ir:gname (impc:ir:gname "dat"))) 
                                    " to " (impc:ir:get-type-str tt) "*\n") os)
            (impc:ir:strip-space os)))))


(define impc:ir:compiler:array-ref
   (lambda (ast types)
      (let* ((os (make-string 0))
             (index-str (impc:ir:compiler (caddr ast) types))
             (idx (impc:ir:gname))
             (var-str (impc:ir:compiler (cadr ast) types))
             (var (impc:ir:gname)))
         ;; type tests
         (if (not (impc:ir:pointer? (impc:ir:get-type-from-str (cadr var))))
             (print-error 'Compiler 'Error: 'Type 'Mismatch: ast 'array 'must 'be 'pointer 'not (cadr var)))
         (if (not (impc:ir:fixed-point? (impc:ir:get-type-from-str (cadr idx))))
             (print-error 'Compiler 'Error: 'Type 'Mismatch: ast 'index 'must 'be 'fixed-point 'not (cadr idx)))         
         (emit index-str os)
         (emit var-str os)
         (emit "; array ref\n" os)
         (emit (string-append (impc:ir:gname "val" (cadr var)) " = getelementptr " 
                                 (cadr var) " " (car var) ", " (cadr idx) " " (car idx) "\n") os)
         (define tt (impc:ir:get-type-str (impc:ir:pointer-- (impc:ir:get-type-from-str (cadr var)))))
         (emit (string-append (impc:ir:gname "val" tt) " = load " (cadr var) " "
                                 (car (impc:ir:gname 1)) "\n") os)
         (impc:ir:strip-space os))))


(define impc:ir:compiler:array-set
   (lambda (ast types)
      (let* ((os (make-string 0))
             (index-str (impc:ir:compiler (caddr ast) types))
             (idx (impc:ir:gname))
             (var-str (impc:ir:compiler (cadr ast) types))
             (var (impc:ir:gname))
             (val-str (impc:ir:compiler (cadddr ast) types (impc:ir:pointer-- (cadr var))))
             (val (impc:ir:gname)))
         ;; type tests
         (if (not (impc:ir:pointer? (impc:ir:get-type-from-str (cadr var))))
             (print-error 'Compiler 'Error: 'Type 'Mismatch: ast 'array 'must 'be 'pointer 'not (cadr var)))
         (if (not (impc:ir:fixed-point? (impc:ir:get-type-from-str (cadr idx))))
             (print-error 'Compiler 'Error: 'Type 'Mismatch: ast 'index 'must 'be 'fixed-point 'not (cadr idx)))
         (if (not (equal? (impc:ir:get-type-from-str (cadr var))
                          (impc:ir:pointer++ (impc:ir:get-type-from-str (cadr val)))))
             (print-error 'Compiler 'Error: 'Type 'Mismatch: ast '- 'setting (cadr val) 'into (cadr var)))         
         ;; type tests done
         (emit index-str os)
         (emit var-str os)
         (emit val-str os)
         (emit "; set array\n" os)
         (emit (string-append (impc:ir:gname "val" (cadr var)) " = getelementptr " 
                                 (cadr var) " " (car var) ", " (cadr idx) " " (car idx) "\n") os)
         ;(define tt (impc:ir:get-type-str (impc:ir:pointer-to-scalar (impc:ir:get-type-from-str (cadr var)))))
         (emit (string-append "store " (cadr val) " " (car val) ", " 
                                 (cadr (impc:ir:gname)) " " (car (impc:ir:gname)) "\n") os)
         (impc:ir:strip-space os))))


(define impc:ir:compiler:make-tuple
   (lambda (ast types)
      (let* ((os (make-string 0)))
         (let* ((t (cons *impc:ir:tuple* (impc:ir:convert-from-pretty-types (cdr ast)))))
            (emit (string-append (impc:ir:gname "dat" "i8*") " = call i8* @llvm_zone_malloc(%mzone* %_zone, i64 "
                                    (number->string (impc:ir:get-type-size t))
                                    ")\n") os)            
            (emit (string-append (impc:ir:gname "val" (string-append (impc:ir:get-type-str t) "*"))
                                    " = bitcast i8* " (car (impc:ir:gname (impc:ir:gname "dat"))) 
                                    " to " (impc:ir:get-type-str t) "*\n") os)
            (impc:ir:strip-space os)))))


(define impc:ir:compiler:tuple-ref
   (lambda (ast types)
      ;; arg 1 for tuples must be a symbol
      ;; arg 2 for typles must be a number
      ;; this should make it easy for us!
      (let* ((os (make-string 0))
             (index-str (impc:ir:compiler (caddr ast) types))
             (idx (impc:ir:gname))
             (var-str (impc:ir:compiler (cadr ast) types))
             (var (impc:ir:gname))
             (tuple-type (impc:ir:get-type-from-str (cadr var)))
             (element-type (list-ref (cdr tuple-type) (caddr ast)))) 
         ;; type tests
         (if (not (impc:ir:tuple? (impc:ir:get-type-from-str (cadr var))))
             (print-error 'Compiler 'Error: 'Type 'Mismatch: ast 'tuple 'must 'be 'tuple 'not (cadr var)))
         (if (not (integer? (caddr ast)))
             (print-error 'Compiler 'Error: 'tuple-ref 'must 'have 'a 'static 'integer 'as 'its 'index 'not: (caddr ast)))
         (if (not (impc:ir:fixed-point? (impc:ir:get-type-from-str (cadr idx))))
             (print-error 'Compiler 'Error: 'Type 'Mismatch: ast 'index 'must 'be 'fixed-point 'not (cadr idx)))
         ;(emit index-str os)
         (emit var-str os)
         (emit "; tuple ref\n" os)
         (define ttstr (impc:ir:get-type-str element-type))
         (emit (string-append (impc:ir:gname "val" (string-append ttstr "*")) " = getelementptr " 
                                 (cadr var) " " (car var) ", i64 0, i32 " (car idx) "\n") os)
         (emit (string-append (impc:ir:gname "val" ttstr) " = load " ttstr "* "
                                 (car (impc:ir:gname 1)) "\n") os)
         (impc:ir:strip-space os))))


;; indexing into structures is limited to i32 indexes!!
(define impc:ir:compiler:tuple-set   
   (lambda (ast types)
      ;; arg 1 for tuples must be a symbol
      ;; arg 2 for typles must be a number
      ;; this should make it easy for us!      
      (let* ((os (make-string 0))
             (var-str (impc:ir:compiler (cadr ast) types))
             (var (impc:ir:gname))             
             (tuple-type (impc:ir:get-type-from-str (cadr var)))
             (element-type (list-ref (cdr tuple-type) (caddr ast)))             
             (index-str (impc:ir:compiler (caddr ast) types))
             (idx (impc:ir:gname))
             (val-str (impc:ir:compiler (cadddr ast) types element-type))
             (val (impc:ir:gname)))
         ;(print 'var: var 'idx: idx 'val: val)
         ;; type tests
         (if (not (impc:ir:tuple? (impc:ir:get-type-from-str (cadr var))))
             (print-error 'Compiler 'Error: 'Type 'Mismatch: ast 'tuple 'must 'be 'pointer 'not (cadr var)))
         (if (not (impc:ir:fixed-point? (impc:ir:get-type-from-str (cadr idx))))
             (print-error 'Compiler 'Error: 'Type 'Mismatch: ast 'index 'must 'be 'fixed-point 'not (cadr idx)))
         (if (not (equal? element-type
                          (impc:ir:get-type-from-str (cadr val))))
             (print-error 'Compiler 'Error: 'Type 'Mismatch: ast '- 'setting (cadr val) 'into 'index 
                          (string->symbol (number->string (caddr ast))) 'of (cadr var)))
         ;; type tests done
         (emit index-str os)
         (emit var-str os)
         (emit val-str os)
         (emit "; set tuple\n" os)
         (define ttstr (impc:ir:get-type-str element-type))         
         (emit (string-append (impc:ir:gname "val" (string-append ttstr "*")) " = getelementptr " 
                                 (cadr var) " " (car var) ", i64 0, i32 " (car idx) "\n") os)
         ;(define tt (impc:ir:get-type-str (impc:ir:pointer-to-scalar (impc:ir:get-type-from-str (cadr var)))))
         (emit (string-append "store " (cadr val) " " (car val) ", " 
                                 (cadr (impc:ir:gname)) " " (car (impc:ir:gname)) "\n") os)
         (impc:ir:gname -1)
         (impc:ir:strip-space os))))


(define impc:ir:compiler:cmp
   (let ((fcmps '("ugt" "ult" "une" "ueq"))
         (icmps '("sgt" "slt" "ne" "eq")))
      (lambda (v ast types . hint?)
         (let* ((type-hint (let ((value (assoc (cl:find-if symbol? (cdr ast)) types))) 
                              (if value 
                                  (cdr value) 
                                  (if (null? hint?)
                                      '()
                                      (car hint?)))))
                (a (if (null? type-hint)
                       (impc:ir:compiler (cadr ast) types)
                       (impc:ir:compiler (cadr ast) types type-hint)))
                (aval (impc:ir:gname))                
                (b (if (null? type-hint)
                       (impc:ir:compiler (caddr ast) types)
                       (impc:ir:compiler (caddr ast) types type-hint)))                       
                (bval (impc:ir:gname))
                                (os (make-string 0))
                (type (if (null? type-hint) 
                          (cadr aval)
                          (impc:ir:get-type-str type-hint))))
            (emit a os)
            (emit b os)
            ;; do llvm float constant check
            (if (= *impc:ir:float* (impc:ir:get-type-from-str type))
                (begin (if (number? (cadr ast)) (set-car! aval (llvm:convert-float (car aval))))
                       (if (number? (caddr ast)) (set-car! bval (llvm:convert-float (car bval))))))
            (if (impc:ir:fixed-point? type)
                (emit (string-append (impc:ir:gname "cmp" "i1") " = icmp " (list-ref icmps v) 
                                        " " type " " (car aval) 
                                        ", " (car bval) "\n") os)
                (emit (string-append (impc:ir:gname "cmp" "i1") " = fcmp " (list-ref fcmps v)
                                        " " type " " (car aval) 
                                        ", " (car bval) "\n") os))
            (impc:ir:strip-space os)))))


(define impc:ir:compiler:modulo
   (lambda (ast types . hint?)
      ;(print 'modulo: 'ast: ast 'hint: hint?)
      (let* ((type-hint (let ((value (assoc (cl:find-if symbol? (cdr ast)) types))) 
                           (if value 
                               (cdr value) 
                               (if (null? hint?)
                                   '()
                                   (car hint?)))))
             (a (if (null? type-hint)
                    (impc:ir:compiler (cadr ast) types)
                    (impc:ir:compiler (cadr ast) types type-hint)))
             (aval (impc:ir:gname))                
             (b (if (null? type-hint)
                    (impc:ir:compiler (caddr ast) types)
                    (impc:ir:compiler (caddr ast) types type-hint)))                       
             (bval (impc:ir:gname))
                          (os (make-string 0))
             (type (if (null? type-hint) 
                       (cadr aval)
                       (impc:ir:get-type-str type-hint))))
         ;; sanity check
         (if (not (and (impc:ir:number? (cadr aval))
                       (impc:ir:number? (cadr bval))))
             (print-error 'Compiler 'Error: 'Bad 'type 'in 'modulo 'expression: ast  '>>> (cadr aval) 'or (cadr bval)))
         (emit a os)
         (emit b os)            
         ;(print (car aval) '>> (car bval) '>> type)
         ;; do llvm float constant check
         (if (= *impc:ir:float* (impc:ir:get-type-from-str type))
             (begin (if (number? (cadr ast)) (set-car! aval (llvm:convert-float (car aval))))
                    (if (number? (caddr ast)) (set-car! bval (llvm:convert-float (car bval))))))     
         ;; do llvm double constant check       
         (if (= *impc:ir:double* (impc:ir:get-type-from-str type))
             (begin (if (number? (cadr ast)) (set-car! aval (number->string (* 1.0 (cadr ast)))))
                    (if (number? (caddr ast)) (set-car! bval (number->string (* 1.0 (caddr ast)))))))
            
         ;; need this to transpose float values            
         (if (impc:ir:fixed-point? (impc:ir:get-type-from-str type))
             (emit (string-append (impc:ir:gname "val" type) " = srem" 
                                     " " type " " (car aval) 
                                     ", " (car bval) "\n") os)
             (emit (string-append (impc:ir:gname "val" type) " = frem"
                                     " " type " " (car aval) 
                                     ", " (car bval) "\n") os))
         (impc:ir:strip-space os))))


(define impc:ir:compiler:math
   (let ((fcmps '("fadd" "fsub" "fmul" "fdiv" "frem"))
         (icmps '("add" "sub" "mul" "sdiv" "srem")))
      (lambda (v ast types . hint?)
         ;(print 'math: 'ast: ast 'hint: hint?)
         (let* ((type-hint (let ((value (assoc (cl:find-if symbol? (cdr ast)) types))) 
                              (if value 
                                  (cdr value) 
                                  (if (null? hint?)
                                      '()
                                      (car hint?)))))
                (a (if (null? type-hint)
                       (impc:ir:compiler (cadr ast) types)
                       (impc:ir:compiler (cadr ast) types type-hint)))
                (aval (impc:ir:gname))                
                (b (if (null? type-hint)
                       (impc:ir:compiler (caddr ast) types)
                       (impc:ir:compiler (caddr ast) types type-hint)))                       
                (bval (impc:ir:gname))
                                (os (make-string 0))
                (type (if (null? type-hint) 
                          (cadr aval)
                          (impc:ir:get-type-str type-hint))))
            ;; sanity check
            (if (not (and (impc:ir:number? (cadr aval))
                          (impc:ir:number? (cadr bval))))
                (print-error 'Compiler 'Error: 'Bad 'type 'in 'math 'expression: ast  '>>> (cadr aval) 'or (cadr bval)))
            (emit a os)
            (emit b os)            
            ;(print (car aval) '>> (car bval) '>> type)
            ;; do llvm float constant check
            (if (= *impc:ir:float* (impc:ir:get-type-from-str type))
                (begin (if (number? (cadr ast)) (set-car! aval (llvm:convert-float (car aval))))
                       (if (number? (caddr ast)) (set-car! bval (llvm:convert-float (car bval))))))     
            ;; do llvm double constant check       
            (if (= *impc:ir:double* (impc:ir:get-type-from-str type))
                (begin (if (number? (cadr ast)) (set-car! aval (number->string (* 1.0 (cadr ast)))))
                       (if (number? (caddr ast)) (set-car! bval (number->string (* 1.0 (caddr ast)))))))
            
            ;; need this to transpose float values            
            (if (impc:ir:fixed-point? (impc:ir:get-type-from-str type))
                (emit (string-append (impc:ir:gname "val" type) " = " (list-ref icmps v) 
                                        " " type " " (car aval) 
                                        ", " (car bval) "\n") os)
                (emit (string-append (impc:ir:gname "val" type) " = " (list-ref fcmps v)
                                        " " type " " (car aval) 
                                        ", " (car bval) "\n") os))
            (impc:ir:strip-space os)))))


			

(define impc:ir:compiler:bitcast
   (lambda (ast types)
      (let* ((os (make-string 0))
             (a (impc:ir:compiler (cadr ast) types))             
             (at (impc:ir:gname))
             (type-str (impc:ir:get-type-str (impc:ir:convert-from-pretty-types (caddr ast)))))
         (emit a os)
         (emit (string-append (impc:ir:gname "val" type-str) 
                                 " = bitcast " (cadr at) " " (car at) " to " type-str "\n") os)
         (impc:ir:strip-space os))))			
		 
(define impc:ir:compiler:null
   (lambda (ast types)
      (let* ((os (make-string 0))
             (a (impc:ir:compiler (cadr ast) types))             
             (at (impc:ir:gname)))
         (emit a os)
         (emit (string-append (impc:ir:gname "val" "i1") " = icmp eq " (cadr at) " " (car at) ", null\n") os)
         (impc:ir:strip-space os))))		 
  
  
;; if that finishes with ret clauses (i.e. doesn't return a value)		 
(define impc:ir:compiler:ifret
   (lambda (ast types)
      (let* ((os (make-string 0))
             (elset (if (null? (cdddr ast)) #f #t))  
             (num (number->string (llvm:count++))))
         ;; first do compare expression
         (emit (impc:ir:compiler (cadr ast) types) os)
         ;; then break on compare
         (emit (string-append "br i1 " (car (impc:ir:gname)) ", label %then" num ", "
                                 (if elset "label %else" "label %then") 
                                 num "\n") os)
         ;; do then
         (emit (string-append "\nthen" num ":\n") os)
         (emit (impc:ir:compiler (caddr ast) types) os)
         
         (define a (impc:ir:gname))
         ;; do else if requried
         (if elset             
             (begin (emit (string-append "\nelse" num ":\n") os)
                    (emit (impc:ir:compiler (cadddr ast) types) os)))
         ;; finally consolidate return type of if statement
         (define b (if elset (impc:ir:gname) #f))
         (if (and elset 
                  (not (equal? (impc:ir:get-type-from-str (cadr a)) 
                               (impc:ir:get-type-from-str (cadr b)))))                               
             (print-error 'Compiler 'error: ast 'type 'conflict 'in 
                          'between 'then (cadr a) 'and 'else (cadr b)))
         (impc:ir:strip-space os))))


;; if that might need to return a value
(define impc:ir:compiler:if
   (lambda (ast types)
      (let* ((os (make-string 0))
             (elset (if (or (null? (cdddr ast))
                            (null? (cadddr ast)))
                        #f #t))  
             (num (number->string (llvm:count++))))
         ;; first do compare expression
         (emit (impc:ir:compiler (cadr ast) types) os)
         ;; then break on compare
         (emit (string-append "br i1 " (car (impc:ir:gname)) ", label %then" num ", "
                                 "label %else" num "\n") os)
         ;; do then
         (emit (string-append "\nthen" num ":\n") os)
         (emit (impc:ir:compiler (caddr ast) types) os)
         (emit (string-append "store " (cadr (impc:ir:gname)) " " (car (impc:ir:gname))
                                 ", " (cadr (impc:ir:gname)) "* %ifptr" num "\n") os)
         (emit (string-append "br label %ifcont" num "\n") os)
         
         (define a (impc:ir:gname))
         ;; do else if requried
         (if elset
             (begin (emit (string-append "\nelse" num ":\n") os)
                    (emit (impc:ir:compiler (cadddr ast) types) os)
                    (emit (string-append "store " (cadr (impc:ir:gname)) " " (car (impc:ir:gname))
                                 ", " (cadr (impc:ir:gname)) "* %ifptr" num "\n") os)         
                    (emit (string-append "br label %ifcont" num "\n") os))
             (begin (emit (string-append "\nelse" num ":\n") os)
                    (emit (string-append "br label %ifcont" num "\n") os)))
         
         ;; finally consolidate return type of if statement
         (define b (if elset (impc:ir:gname) #f))
         (if (and elset 
                  (not (equal? (impc:ir:get-type-from-str (cadr a)) 
                               (impc:ir:get-type-from-str (cadr b)))))                               
             (print-error 'Compiler 'error: ast 'type 'conflict 'in 'between 'then (cadr a) 'and 'else (cadr b)))
         
         (emit (string-append "\nifcont" num ":\n") os)
         (emit (string-append (impc:ir:gname "ifres" (cadr a)) " = load " (cadr a) "* %ifptr" num "\n\n") os) 
         ;; finally append %ifptr alloca to front of string
         (string-append "\n; alloca if pointer\n"
                        "%ifptr" num " = alloca " (cadr a) "\n"
                        (impc:ir:strip-space os)))))		 

(define impc:ir:compiler:native-call
   (lambda (ast types) 
      ;(print 'native: 'ast ast)     
      (let* ((os (make-string 0)) 
             (fname (symbol->string (car ast)))
             (calling-conv (llvm:get-function-calling-conv fname))
             (ftypes (llvm:get-function-args-withoutzone fname))
             (closurecall (llvm:get-function (string-append fname "_getter")))
             (args (map (lambda (a hint)
                           (cons (impc:ir:compiler a types (impc:ir:get-type-from-str hint)) (impc:ir:gname)))
                        (cdr ast)
                        (cdr ftypes))))
         (if (<> (length args) (length (cdr ftypes)))
             (print-error 'Compiler 'Error: ast 'Wrong 'number 'of 'arguments))
         (emit (apply string-append (map (lambda (p) (car p)) args)) os)
         (emit (string-append (if (impc:ir:void? (car ftypes))
                                     (begin (impc:ir:gname "res" "void") "")
                                     (string-append (impc:ir:gname "res" (car ftypes)) " = "))
                                 "tail call cc " (number->string calling-conv)
                                 " " (car ftypes)
                                 " @" fname "("
                                 (if closurecall 
                                     (string-append "i8* %_impz" (if (null? (cdr ftypes)) "" ","))
                                     "")
                                 (apply string-append 
                                        (map (lambda (p ft i)
                                                (let ((atype (cadr (cdr p)))
                                                      (aname (car (cdr p))))
                                                   ;(print p ft i)
                                                   ;(if (not (string=? atype ft))
                                                   (if (not (equal? (impc:ir:get-type-from-str atype)
                                                                    (impc:ir:get-type-from-str ft)))
                                                       (print-error 'Compiler 'Error: 'Type 'Error: ast 'function 'argument 'does 'not 'match. 'Expected ft 'but 'got atype))
                                                   (string-append (if (> i 0) ", " "")
                                                                  atype " " aname)))
                                             args (cdr ftypes) (make-list-with-proc (length args) (lambda (i) i))))
                                 ")\n") os)
         (impc:ir:strip-space os))))


(define impc:ir:compiler
   (lambda (ast types . hint?)
      (cond ((null? ast) "")
            ((atom? ast)
             (cond ((symbol? ast) 
                    (if (or (assoc ast types)
                            (llvm:get-globalvar (symbol->string ast)))
                        (if (assoc ast types) 
                            (impc:ir:compile:eval-var ast (cdr (assoc ast types)))
                            (impc:ir:compile:eval-gvar ast))
                        (print-error 'Compiler 'Error: 'Unbound 'symbol ast)))
                   ((number? ast)
                    ;(print 'number: ast hint?)
                    (if (and (not (null? hint?))
                             (impc:ir:number? (car hint?)))
                        (if (= *impc:ir:float* (car hint?))
                            (impc:ir:gname (llvm:convert-float (number->string ast)) (impc:ir:get-type-str (car hint?)))
                            (impc:ir:gname (number->string ast) (impc:ir:get-type-str (car hint?))))
                        (impc:ir:gname (number->string ast) (if (integer? ast) "i64" "double")))
                    "")
                   ((string? ast) (impc:ir:make-string ast))
                   (else (print-error "bad or unsupported atom type -> " ast) (error ""))))
            ((list? ast)
             (cond ((member (car ast) '(make-env make-env-zone))
                    (impc:ir:compile:make-env ast types))
                   ((equal? (car ast) 'make-closure) 
                    (let* (;(str-pair (impc:ir:compile:make-closure ast types))
                           ;(fstr (car str-pair))
                           ;(lstr (cdr str-pair)))
                            (fstr (impc:ir:compile:make-closure ast types))
                            (lstr (impc:ir:compile:make-closureenv ast types)))
                       ;; compile function
                       (if *impc:compiler:print* (print '------------------------------compiling---------------------------->))
                       (if *impc:compiler:print* (print fstr))
                       (if *impc:compiler:print-raw-llvm* (print-full-nq fstr))
                       (if *impc:compile*
                           (begin (llvm:remove-function (cadr ast))
                                  (if (not (llvm:compile fstr))
                                      (begin (if *impc:compiler:verbose*
                                                 (print-error "Compiler Failed On: " fstr)
                                                 (print-error "Compiler Failed"))
                                             (error "")))))
                       lstr))
                   ((member (car ast) '(clrun->)) ;; apply function  
                    (if (llvm:get-globalvar (symbol->string (cadr ast))) ;; if closure is a global var? (i.e. global env)
                        (string-append (impc:ir:compiler (cadr ast) types)
                                       (impc:ir:compile:apply-closure (cons (impc:ir:gname) (cddr ast)) types #t))
                        (impc:ir:compile:apply-closure (cdr ast) types #f))) ;; else closure is in local env
                   ((equal? (car ast) 'set!)
                    (impc:ir:compiler:set! ast types))
                   ((equal? (car ast) 'bitcast)
                    (impc:ir:compiler:bitcast ast types))
                   ((equal? (car ast) 'null?)
                    (impc:ir:compiler:null ast types))					
                   ((equal? (car ast) 'dotimes)
                    (impc:ir:compiler:loop (cdr ast) types))
                   ((equal? (car ast) 'make-array)
                    (impc:ir:compiler:make-array ast types))
                   ((equal? (car ast) 'array-ref)
                    (impc:ir:compiler:array-ref ast types))
                   ((equal? (car ast) 'array-set!)
                    (impc:ir:compiler:array-set ast types))
                   ((equal? (car ast) 'make-tuple)
                    (impc:ir:compiler:make-tuple ast types))
                   ((equal? (car ast) 'tuple-ref)
                    (impc:ir:compiler:tuple-ref ast types))
                   ((equal? (car ast) 'tuple-set!)
                    (impc:ir:compiler:tuple-set ast types))                   
                   ((equal? (car ast) 'closure-ref)
                    (impc:ir:compiler:closure-ref ast types))
                   ((equal? (car ast) 'closure-set!)
                    (impc:ir:compiler:closure-set ast types))                                      
                   ((equal? (car ast) 'coerce->)
                    (impc:ir:compiler:coerce (cdr ast) types))
				   ((equal? (car ast) 'modulo)
				    (impc:ir:compiler:modulo ast types))
                   ((member (car ast) '(> < <> =)) 
                    (if (<> (length ast) 3)
                        (print-error 'Compiler 'Error: ast 'bad 'arity))                    
                    (if (not (null? hint?))                        
                        (impc:ir:compiler:cmp (cl:position (car ast) '(> < <> =)) ast types (car hint?))
                        (impc:ir:compiler:cmp (cl:position (car ast) '(> < <> =)) ast types)))
                   ((member (car ast) '(+ - * /))
                    (if (<> (length ast) 3)
                        (print-error 'Compiler 'Error: ast 'bad 'arity))
                    (if (not (null? hint?))                    
                        (impc:ir:compiler:math (cl:position (car ast) '(+ - * /)) ast types (car hint?))
                        (impc:ir:compiler:math (cl:position (car ast) '(+ - * /)) ast types)))
                   ((equal? (car ast) 'if)
                    (impc:ir:compiler:if ast types))
                   ((equal? (car ast) 'ifret)
                    (impc:ir:compiler:ifret ast types))					
                   ((and (symbol? (car ast))
                         (llvm:get-function (symbol->string (car ast))))
                    (if (equal? (car ast) 'llvm_printf) ;; this guff all here for llvm_printf
                        (let ((args (map (lambda (a)
                                            (cons (impc:ir:compiler a types)
                                                  (impc:ir:gname)))
                                         (cdr ast)))
                              (va (impc:ir:gname "val" "i32")))
                           (if (<> (impc:ir:get-type-from-str (car (cdr (cdr (car args))))) (impc:ir:pointer++ *impc:ir:si8*))
                               (print-error 'Compiler 'Error: 'Type 'Mismatch ast 'First 'argument 'must 'be 'a 'format 'string))
                           (string-append (apply string-append (map (lambda (a) (car a)) args))
                                          "\n" va " = call i32 (i8*, ...)* @llvm_printf("
                                          (caddr (car args)) " " (cadr (car args))
                                          (apply string-append (map (lambda (a)
                                                                       (string-append ", " 
                                                                                      (caddr a) 
                                                                                      " " (cadr a)))
                                                                    (cdr args)))
                                          ")\n"))                                                                  
                        (impc:ir:compiler:native-call ast types)))
                   ((equal? (car ast) 'ret->) ;; return from function
                    (let ((str (impc:ir:compiler (caddr ast) types)))
                       (if (impc:ir:void? (cadr (impc:ir:gname)))
                           (string-append str "ret void\n")
                           (string-append str "ret " 
                                          (cadr (impc:ir:gname)) " "
                                          (car (impc:ir:gname))
                                          "\n"))))
                   ((equal? (car ast) 'if)
                    (impc:ir:compiler:if ast types))
                   ((equal? (car ast) 'begin)
                    (let ((ll (map (lambda (expr) (impc:ir:compiler expr types)) (cdr ast))))
                       (apply string-append ll)))
                   ((and (list? (car ast))
                         (equal? (caar ast) 'begin))
                    (string-append (impc:ir:compiler (car ast) types)
                                   (impc:ir:compiler (cdr ast) types)))
                   ((list? (car ast))
                    (string-append (impc:ir:compiler (car ast) types)                    
                                   (impc:ir:compile:apply-closure (cons (impc:ir:gname) (cdr ast)) types #t)))
                   (else (string-append (impc:ir:compiler (car ast) types)
                                        (impc:ir:compiler (cdr ast) types)))))
            ((pair? ast)
             (let ((a (impc:ir:compiler (car ast) types))
                   (b (impc:ir:compiler (cdr ast) types)))
                (string-append a b))))))


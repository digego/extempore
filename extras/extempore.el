;;; extempore.el

;; Author: Ben Swift <ben.swift@anu.edu.au>
;; Adapted from: scheme.el by Bill Rozas and Dave Love
;; Also includes some work done by Hector Levesque and Andrew Sorensen

;; TODO add licence info

;; Commentary:

;; The major mode for editing Extempore code, which is a mixture of
;; Scheme and xtlang---a strongly-typed, scheme-like language.  See
;; the Extempore project page at http://github.com/digego/extempore
;; for more details.

;; To set up Emacs to automatically load this major mode for any .xtm
;; files, add the following lines to your .emacs

;; (autoload 'extempore-mode "/path/to/extempore/extras/extempore.el" "" t)
;; (add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

;;; Code:

(require 'lisp-mode)

(defvar extempore-mode-syntax-table
  (let ((st (make-syntax-table))
	(i 0))
    ;; Symbol constituents
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    (modify-syntax-entry ?\| "\" 23bn" st)
    ;; Guile allows #! ... !# comments.
    ;; But SRFI-22 defines the comment as #!...\n instead.
    ;; Also Guile says that the !# should be on a line of its own.
    ;; It's too difficult to get it right, for too little benefit.
    ;; (modify-syntax-entry ?! "_ 2" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
    (modify-syntax-entry ?\; "< 2 " st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar extempore-mode-abbrev-table nil)
(define-abbrev-table 'extempore-mode-abbrev-table ())

;; imenu stuff goes here in scheme-mode - not implemented for
;; extempore mode yet.

(defun extempore-mode-variables ()
  (set-syntax-table extempore-mode-syntax-table)
  (setq local-abbrev-table extempore-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-paragraph-function) 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'outline-regexp) ";;; \\|(....")
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-add) 1)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (set (make-local-variable 'font-lock-comment-start-skip) ";+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'lisp-indent-function) 'extempore-indent-function)
  (setq mode-line-process '("" extempore-mode-line-process))
  ;; (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'font-lock-defaults)
       '(extempore-font-lock-keywords
	 nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
	 beginning-of-defun
	 (font-lock-mark-block-function . mark-defun)
	 (font-lock-syntactic-face-function
	  . extempore-font-lock-syntactic-face-function)
	 (parse-sexp-lookup-properties . t)
	 (font-lock-extra-managed-props syntax-table)))
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'extempore-doc-string-elt))

(defvar extempore-mode-line-process "")

(defvar extempore-mode-map
  (let ((smap (make-sparse-keymap))
	(map (make-sparse-keymap "Extempore")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar extempore] (cons "Extempore" map))
    ;; (define-key map [run-extempore] '("Run Inferior Extempore" . run-extempore))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
				   (interactive "r")
				   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for Extempore mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;;;###autoload
(define-derived-mode extempore-mode prog-mode "Extempore"
  "Major mode for editing Extempore code.
This mode has been adapted from `scheme-mode'.

In addition, if an Extempore process is running in a shell
buffer, some additional commands will be defined, for evaluating
expressions and controlling the extempore process.

Entry to this mode calls the value of `extempore-mode-hook'."
  (extempore-mode-variables)
  (make-variable-buffer-local 'extempore-process)
  (setq extempore-process nil))

(defgroup extempore nil
  "Editing Extempore code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom extempore-mode-hook nil
  "Normal hook run when entering `extempore-mode'.
See `run-hooks'."
  :type 'hook
  :group 'extempore)

(defcustom extempore-default-host "localhost"
  "Default host where the extempore process is running."
  :type 'string
  :group 'extempore)

(defcustom extempore-default-port 7099
  "Default port where the extempore process is running."
  :type 'integer
  :group 'extempore)

(defcustom extempore-use-pretty-lambdas t
  "Use pretty (greek symbol) lambdas in buffer?"
  :type 'boolean
  :group 'extempore)

;; from emacs-starter-kit

(defface extempore-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses in extempore."
  :group 'extempore)

(defun extempore-keybindings (keymap)
  (define-key keymap (kbd "C-x C-j") 'extempore-connect)
  (define-key keymap (kbd "C-x C-x") 'extempore-send-definition)
  (define-key keymap (kbd "C-x C-r") 'extempore-send-region))

(extempore-keybindings extempore-mode-map)

(defconst extempore-font-lock-keywords-scheme
  (eval-when-compile
    (list
     ;; built-ins
     (list
      (concat
       "(" (regexp-opt
            '("begin" "call-with-current-continuation" "call/cc"
              "call-with-input-file" "call-with-output-file" "case" "cond"
              "do" "dotimes" "else" "for-each" "if" "lambda"
              "let" "let*" "let-syntax" "letrec" "letrec-syntax"
              "and" "or"
              "map" "syntax" "syntax-rules"
              "print" "println") t) "\\>")
      '(1 font-lock-keyword-face))
     ;; It wouldn't be Scheme w/o named-let.
     '("(let\\s-+\\(\\sw+\\)"
       (1 font-lock-function-name-face))
     ;; definitions
     (list (concat
	    "(\\(define\\(\\|-syntax\\|-macro\\|-instrument\\|-sampler\\)\\)"
	    ;; Any whitespace and declared object.
	    "[ \t]*(?"
	    "\\(\\sw+\\)?")
	   '(1 font-lock-keyword-face)
	   '(3 font-lock-function-name-face))
     )))

(defconst extempore-font-lock-keywords-xtlang
  (eval-when-compile
    (list
     ;; definitions
     (list (concat
            "(\\(bind-\\(func\\|val\\|type\\|alias\\|poly\\|lib\\)\\)"
	    ;; Any whitespace and declared object.
	    "[ \t]*(?"
	    "\\(\\sw+\\)?")
	   '(1 font-lock-keyword-face)
	   '(3 font-lock-function-name-face))
     ;; important xtlang functions
     (list
      (concat
       "(" (regexp-opt
            '("begin" "cond" "dotimes" "if" "else"  "lambda"
              "let" "and" "or" "callback" "printf" "cast"
              "aref" "aset!" "afill!" "aref-ptr"
              "array-ref" "array-set!" "array-fill!" "array-ref-ptr"
              "tref" "tset!" "tfill!" "tref-ptr"
              "tuple-ref" "tuple-set!" "tuple-fill!" "tuple-ref-ptr"
              "pref" "pset!" "pfill!" "pref-ptr"
              "pointer-ref" "pointer-set!" "pointer-fill!" "pointer-ref-ptr"
              "alloc" "salloc" "halloc" "zalloc"
              "stack-alloc" "heap-alloc" "zone-alloc")
            t) "\\>")
      '(1 font-lock-keyword-face))
     ;; closure type annotations (i.e. specified with a colon)
     '("(bind-func\\s-+\\S-+\\(:\\S-+\\)\\>"
       (1 font-lock-type-face t))
     ;; bind-type/alias
     '("(bind-\\(type\\|alias\\)\\s-+\\S-+\\s-+\\(\\S-+\\))"
       (2 font-lock-type-face))
     ;; bind-lib
     '("(bind-lib\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\))"
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face)
       (3 font-lock-type-face))
     ;; bind-val
     '("(bind-val\\s-+\\S-+\\s-+\\(\\S-+\\)\\>"
       (1 font-lock-type-face))
     ;; cast
     '("(cast\\s-+\\S-+\\s-+\\(\\S-+\\)\\_>"
       (1 font-lock-type-face))
     ;; other type annotations
     '(":\\S-+\\>"
       (0 font-lock-type-face))
     ;; float and int literals
     '("\\_<[-+]?[/.[:digit:]]+?\\_>"
       (0 font-lock-constant-face))
     ;; type coercion stuff
     (list
      (concat
       "(" (regexp-opt
            (let ((types '("i1" "i8" "i16" "i32" "i64" "f" "d")))
              (apply 'append (mapcar (lambda (a)
                                       (mapcar (lambda (b)
                                                 (concat a "to" b))
                                               (remove a types)))
                                     types))) t) "\\>")
      '(1 font-lock-type-face))
     )))

(font-lock-add-keywords 'extempore-mode
                        '(("(\\|)" . 'extempore-paren-face)))

(defvar extempore-font-lock-keywords
  (append extempore-font-lock-keywords-scheme
          extempore-font-lock-keywords-xtlang)
  "Expressions to highlight in extempore-mode.")

(defconst extempore-sexp-comment-syntax-table
  (let ((st (make-syntax-table extempore-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

(put 'lambda 'extempore-doc-string-elt 2)
;; Docstring's pos in a `define' depends on whether it's a var or fun def.
(put 'define 'extempore-doc-string-elt
     (lambda ()
       ;; The function is called with point right after "define".
       (forward-comment (point-max))
       (if (eq (char-after) ?\() 2 0)))

(defun extempore-font-lock-syntactic-face-function (state)
  (when (and (null (nth 3 state))
	     (eq (char-after (nth 8 state)) ?#)
	     (eq (char-after (1+ (nth 8 state))) ?\;))
    ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
    (save-excursion
      (let ((pos (point))
	    (end
	     (condition-case err
		 (let ((parse-sexp-lookup-properties nil))
		   (goto-char (+ 2 (nth 8 state)))
		   ;; FIXME: this doesn't handle the case where the sexp
		   ;; itself contains a #; comment.
		   (forward-sexp 1)
		   (point))
	       (scan-error (nth 2 err)))))
	(when (< pos (- end 2))
	  (put-text-property pos (- end 2)
			     'syntax-table extempore-sexp-comment-syntax-table))
	(put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

(defvar calculate-lisp-indent-last-sexp)

;; FIXME this duplicates almost all of lisp-indent-function.
;; Extract common code to a subroutine.
(defun extempore-indent-function (indent-point state)
  "Extempore mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `extempore-indent-function'
\(or the deprecated `extempore-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
	     (not (looking-at "\\sw\\|\\s_")))
	;; car of form doesn't seem to be a symbol
	(progn
	  (if (not (> (save-excursion (forward-line 1) (point))
		      calculate-lisp-indent-last-sexp))
	      (progn (goto-char calculate-lisp-indent-last-sexp)
		     (beginning-of-line)
		     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
	  ;; Indent under the list or under the first sexp on the same
	  ;; line as calculate-lisp-indent-last-sexp.  Note that first
	  ;; thing on that line has to be complete sexp since we are
	  ;; inside the innermost containing sexp.
	  (backward-prefix-chars)
	  (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'extempore-indent-function)
			 (get (intern-soft function) 'extempore-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method state indent-point normal-indent)))))))


;;; 'let' is different in Scheme/xtlang

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that it is protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun extempore-let-indent (state indent-point)
;;  (if (would-be-symbol (next-sexp-as-string))
;;      (extempore-indent-specform 2 state indent-point)
;;      (extempore-indent-specform 1 state indent-point)))

(defun extempore-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'extempore-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'extempore-indent-function 0)
(put 'case 'extempore-indent-function 1)
(put 'delay 'extempore-indent-function 0)
(put 'do 'extempore-indent-function 2)
(put 'lambda 'extempore-indent-function 1)
(put 'let 'extempore-indent-function 'extempore-let-indent)
(put 'let* 'extempore-indent-function 1)
(put 'letrec 'extempore-indent-function 1)
(put 'let-values 'extempore-indent-function 1) ; SRFI 11
(put 'let*-values 'extempore-indent-function 1) ; SRFI 11
(put 'sequence 'extempore-indent-function 0) ; SICP, not r4rs
(put 'let-syntax 'extempore-indent-function 1)
(put 'letrec-syntax 'extempore-indent-function 1)
(put 'syntax-rules 'extempore-indent-function 1)
(put 'syntax-case 'extempore-indent-function 2) ; not r5rs

(put 'call-with-input-file 'extempore-indent-function 1)
(put 'with-input-from-file 'extempore-indent-function 1)
(put 'with-input-from-port 'extempore-indent-function 1)
(put 'call-with-output-file 'extempore-indent-function 1)
(put 'with-output-to-file 'extempore-indent-function 1)
(put 'with-output-to-port 'extempore-indent-function 1)
(put 'call-with-values 'extempore-indent-function 1) ; r5rs?
(put 'dynamic-wind 'extempore-indent-function 3) ; r5rs?


;; dealing with the (external) extempore process

(defun extempore-connect (host port)
  "Connect to the running extempore process, which must
be running in another (shell-like) buffer."
  (interactive (let ((read-host (read-from-minibuffer
				  (concat "Hostname (default "
					  extempore-default-host
					  "):")))
		     (read-port (read-from-minibuffer
				  (concat "Port (default "
					  (number-to-string extempore-default-port)
					  "):"))))
		 (list (if (string-equal read-host "")
			   extempore-default-host
			 read-host)
		       (if (string-equal read-port "")
			   extempore-default-port
			 (string-to-number read-port)))))
  (if (not (null extempore-process))
      (delete-process extempore-process))
  (setq extempore-process
	(open-network-stream "extempore" nil
			     host
			     port))
  (set-process-filter extempore-process
		      '(lambda (proc str) (message (substring str 0 -1)))))

(defun extempore-stop ()
  "Terminate connection to the Extempore process"
  (interactive)
  (delete-process extempore-process)
  (setq extempore-process nil))

(defun extempore-send-definition ()
  "Send the enclosing top-level def to Extempore server for evaluation"
  (interactive)
  (save-excursion
    (mark-defun)
    (let ((str (concat (buffer-substring (point) (mark))
		       "\r\n")))
      (process-send-string extempore-process str)
      (redisplay) ; flash the def like Extempore
      (sleep-for .25))))

(defun extempore-send-region ()
  "Send the current region (or all the buffer) to Extempore for evaluation"
  (interactive)
  (save-excursion
    (if mark-active
	(unless (= (point) (region-beginning)) (exchange-point-and-mark))
      (progn (goto-char (point-min)) (set-mark (point-max))))
    (let ((start (region-beginning)) (end (region-end)))
      (while (re-search-forward "^[^\n;]*(" end t)
	(extempore-send-definition)
	(end-of-defun)))))

;; misc bits and pieces

(defun xpb1 (name duration)
  (interactive "sName: \nsDuration: ")
  (insert (concat "(define " name
		  "\n  (lambda (beat dur)\n    "
		  "(callback (*metro* (+ beat (* .5 " duration "))) '"
		  name " (+ beat " duration ") " duration ")))\n\n"
		  "(" name " (*metro* 'get-beat 4) " duration ")")))

;; for greek symbol lambdas: from emacs-starter-kit

(if extempore-use-pretty-lambdas
    (font-lock-add-keywords
     nil `(("(?\\(lambda\\>\\)"
	    (0 (progn (compose-region (match-beginning 1) (match-end 1)
				      ,(make-char 'greek-iso8859-7 107))
		      nil))))))

(provide 'extempore)

;;; extempore.el ends here

;;; extempore-repl.el --- interaction REPL for Extempore

;; Author: Ben Swift <maa036@lancaster.ac.uk>
;; Created: 9 October 2014

;; *heavily* based on ielm.el by David Smith

;; TODO insert GPLv3

;;; Commentary:

;; To start: M-x extempore-repl.  Type C-h m in the *extempore-repl* buffer for more info.

;;; Code:

(require 'comint)
(require 'extempore)

;;; User variables

(defgroup extempore-repl nil
  "Interaction mode for Extempore."
  :group 'lisp)


(defcustom extempore-repl-noisy t
  "If non-nil, extempore-repl will beep on error."
  :type 'boolean
  :group 'extempore-repl)

(defcustom extempore-repl-prompt-read-only t
  "If non-nil, the extempore-repl prompt is read only.
The read only region includes the newline before the prompt.
Setting this variable does not affect existing extempore-repl runs.
This works by setting the buffer-local value of `comint-prompt-read-only'.
Setting that value directly affects new prompts in the current buffer.

If this option is enabled, then the safe way to temporarily
override the read-only-ness of extempore-repl prompts is to call
`comint-kill-whole-line' or `comint-kill-region' with no
narrowing in effect.  This way you will be certain that none of
the remaining prompts will be accidentally messed up.  You may
wish to put something like the following in your init file:

\(add-hook 'extempore-repl-mode-hook
	  (lambda ()
	     (define-key extempore-repl-map \"\\C-w\" 'comint-kill-region)
	     (define-key extempore-repl-map [C-S-backspace]
	       'comint-kill-whole-line)))

If you set `comint-prompt-read-only' to t, you might wish to use
`comint-mode-hook' and `comint-mode-map' instead of
`extempore-repl-mode-hook' and `extempore-repl-map'.  That will affect all comint
buffers, including extempore-repl buffers.  If you sometimes use extempore-repl on
text-only terminals or with `emacs -nw', you might wish to use
another binding for `comint-kill-whole-line'."
  :type 'boolean
  :group 'extempore-repl
  :version "22.1")

(defcustom extempore-repl-prompt "xtm> "
  "Prompt used in extempore-repl.
Setting this variable does not affect existing extempore-repl runs.

Interrupting the extempore-repl process with \\<extempore-repl-map>\\[comint-interrupt-subjob],
and then restarting it using \\[extempore-repl], makes the then current
default value affect _new_ prompts.  Unless the new prompt
differs only in text properties from the old one, extempore-repl will no
longer recognize the old prompts.  However, executing \\[extempore-repl]
does not update the prompt of an *extempore-repl* buffer with a running process.
For extempore-repl buffers that are not called `*extempore-repl*', you can execute
\\[inferior-extempore-mode] in that extempore-repl buffer to update the value,
for new prompts.  This works even if the buffer has a running process."
  :type 'string
  :group 'extempore-repl)

(defvar extempore-repl-prompt-internal "xtm> "
  "Stored value of `extempore-repl-prompt' in the current extempore-repl buffer.
This is an internal variable used by extempore-repl.  Its purpose is to
prevent a running extempore-repl process from being messed up when the user
customizes `extempore-repl-prompt'.")

(defcustom extempore-repl-dynamic-return t
  "Controls whether \\<extempore-repl-map>\\[extempore-repl-return] has intelligent behavior in extempore-repl.
If non-nil, \\[extempore-repl-return] evaluates input for complete sexps, or inserts a newline
and indents for incomplete sexps.  If nil, always inserts newlines."
  :type 'boolean
  :group 'extempore-repl)

(defcustom extempore-repl-dynamic-multiline-inputs t
  "Force multiline inputs to start from column zero?
If non-nil, after entering the first line of an incomplete sexp, a newline
will be inserted after the prompt, moving the input to the next line.
This gives more frame width for large indented sexps, and allows functions
such as `edebug-defun' to work with such inputs."
  :type 'boolean
  :group 'extempore-repl)

(defcustom extempore-repl-mode-hook nil
  "Hooks to be run when extempore-repl (`inferior-extempore-mode') is started."
  :options '(turn-on-eldoc-mode)
  :type 'hook
  :group 'extempore-repl)
(defvaralias 'inferior-extempore-mode-hook 'extempore-repl-mode-hook)

(defvar * nil
  "Most recent value evaluated in extempore-repl.")

(defvar ** nil
  "Second-most-recent value evaluated in extempore-repl.")

(defvar *** nil
  "Third-most-recent value evaluated in extempore-repl.")

(defvar extempore-repl-match-data nil
  "Match data saved at the end of last command.")

(defvar *1 nil
  "During extempore-repl evaluation, most recent value evaluated in extempore-repl.
Normally identical to `*'.  However, if the working buffer is an extempore-repl
buffer, distinct from the process buffer, then `*' gives the value in
the working buffer, `*1' the value in the process buffer.
The intended value is only accessible during extempore-repl evaluation.")

(defvar *2 nil
  "During extempore-repl evaluation, second-most-recent value evaluated in extempore-repl.
Normally identical to `**'.  However, if the working buffer is an extempore-repl
buffer, distinct from the process buffer, then `**' gives the value in
the working buffer, `*2' the value in the process buffer.
The intended value is only accessible during extempore-repl evaluation.")

(defvar *3 nil
  "During extempore-repl evaluation, third-most-recent value evaluated in extempore-repl.
Normally identical to `***'.  However, if the working buffer is an extempore-repl
buffer, distinct from the process buffer, then `***' gives the value in
the working buffer, `*3' the value in the process buffer.
The intended value is only accessible during extempore-repl evaluation.")

;;; System variables

(defvar extempore-repl-working-buffer nil
  "Buffer in which extempore-repl sexps will be evaluated.
This variable is buffer-local.")

(defvar extempore-repl-header
  "*** Welcome to extempore-repl ***  Type (describe-mode) for help.\n"
  "Message to display when extempore-repl is started.")

(defvar extempore-repl-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\C-m" 'extempore-repl-return)
    (define-key map "\C-j" 'extempore-repl-send-input)
    (define-key map "\e\C-x" 'eval-defun)         ; for consistency with
    (define-key map "\e\t" 'completion-at-point)  ; lisp-interaction-mode
    ;; These bindings are from `lisp-mode-shared-map' -- can you inherit
    ;; from more than one keymap??
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\177" 'backward-delete-char-untabify)
    ;; Some convenience bindings for setting the working buffer
    (define-key map "\C-c\C-b" 'extempore-repl-change-working-buffer)
    (define-key map "\C-c\C-f" 'extempore-repl-display-working-buffer)
    (define-key map "\C-c\C-v" 'extempore-repl-print-working-buffer)
    map)
  "Keymap for extempore-repl mode.")
(defvaralias 'inferior-extempore-mode-map 'extempore-repl-map)

(defvar extempore-repl-font-lock-keywords
  '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
     (1 font-lock-comment-face)
     (2 font-lock-constant-face)))
  "Additional expressions to highlight in extempore-repl buffers.")

;;; Completion stuff

(defun extempore-repl-tab nil
  "Possibly indent the current line as Lisp code."
  (interactive)
  (when (or (eq (preceding-char) ?\n)
	    (eq (char-syntax (preceding-char)) ?\s))
    (extempore-repl-indent-line)
    t))

(defun extempore-repl-complete-symbol nil
  "Complete the Lisp symbol before point."
  ;; A wrapper for lisp-complete symbol that returns non-nil if
  ;; completion has occurred
  (let* ((btick (buffer-modified-tick))
	 (cbuffer (get-buffer "*Completions*"))
	 (ctick (and cbuffer (buffer-modified-tick cbuffer))))
    (lisp-complete-symbol)
     ;; completion has occurred if:
    (or
     ;; the buffer has been modified
     (not (= btick (buffer-modified-tick)))
     ;; a completions buffer has been modified or created
     (if cbuffer
	 (not (= ctick (buffer-modified-tick cbuffer)))
       (get-buffer "*Completions*")))))

(defun extempore-repl-complete-filename nil
  "Dynamically complete filename before point, if in a string."
  (when (nth 3 (parse-partial-sexp comint-last-input-start (point)))
    (comint-dynamic-complete-filename)))

(defun extempore-repl-indent-line nil
  "Indent the current line as Lisp code if it is not a prompt line."
  (when (save-excursion (comint-bol) (bolp))
    (lisp-indent-line)))

;;; Working buffer manipulation

(defun extempore-repl-print-working-buffer nil
  "Print the current extempore-repl working buffer's name in the echo area."
  (interactive)
  (message "The current working buffer is: %s" (buffer-name extempore-repl-working-buffer)))

(defun extempore-repl-display-working-buffer nil
  "Display the current extempore-repl working buffer.
Don't forget that selecting that buffer will change its value of `point'
to its value of `window-point'!"
  (interactive)
  (display-buffer extempore-repl-working-buffer)
  (extempore-repl-print-working-buffer))

(defun extempore-repl-change-working-buffer (buf)
  "Change the current extempore-repl working buffer to BUF.
This is the buffer in which all sexps entered at the extempore-repl prompt are
evaluated.  You can achieve the same effect with a call to
`set-buffer' at the extempore-repl prompt."
  (interactive "bSet working buffer to: ")
  (let ((buffer (get-buffer buf)))
    (if (and buffer (buffer-live-p buffer))
  	(setq extempore-repl-working-buffer buffer)
      (error "No such buffer: %S" buf)))
  (extempore-repl-print-working-buffer))

;;; Other bindings

(defun extempore-repl-return nil
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.  If however `extempore-repl-dynamic-return' is nil, this always
simply inserts a newline."
  (interactive)
  (if extempore-repl-dynamic-return
      (let ((state
	     (save-excursion
	       (end-of-line)
	       (parse-partial-sexp (extempore-repl-pm)
				   (point)))))
	(if (and (< (car state) 1) (not (nth 3 state)))
	    (extempore-repl-send-input)
	  (when (and extempore-repl-dynamic-multiline-inputs
		     (save-excursion
		       (beginning-of-line)
		       (looking-at-p comint-prompt-regexp)))
	    (save-excursion
	      (goto-char (extempore-repl-pm))
	      (newline 1)))
	  (newline-and-indent)))
    (newline)))

(defvar extempore-repl-input)

(defun extempore-repl-input-sender (_proc input)
  ;; Just sets the variable extempore-repl-input, which is in the scope of
  ;; `extempore-repl-send-input's call.
  (setq extempore-repl-input input))

(defun extempore-repl-send-input nil
  "Evaluate the Extempore expression after the prompt."
  (interactive)
  (let (extempore-repl-input)			; set by extempore-repl-input-sender
    (comint-send-input)			; update history, markers etc.
    (extempore-repl-eval-input extempore-repl-input)))

;;; Utility functions

(defun extempore-repl-is-whitespace-or-comment (string)
  "Return non-nil if STRING is all whitespace or a comment."
  (or (string= string "")
      (string-match-p "\\`[ \t\n]*\\(?:;.*\\)*\\'" string)))

;;; Evaluation

(defvar extempore-repl-string)
(defvar extempore-repl-form)
(defvar extempore-repl-pos)
(defvar extempore-repl-result)
(defvar extempore-repl-error-type)
(defvar extempore-repl-output)
(defvar extempore-repl-wbuf)
(defvar extempore-repl-pmark)

(defun extempore-repl-eval-input (input-string)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  ;; This is the function that actually `sends' the input to the
  ;; `inferior Lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.  A better way of doing this might have
  ;; been to actually send the output to the `cat' process, and write
  ;; this as in output filter that converted sexps in the output
  ;; stream to their evaluated value.  But that would have involved
  ;; more process coordination than I was happy to deal with.
  ;;
  ;; NOTE: all temporary variables in this function will be in scope
  ;; during the eval, and so need to have non-clashing names.
  (let ((extempore-repl-string input-string)      ; input expression, as a string
        extempore-repl-form			; form to evaluate
	extempore-repl-pos			; End posn of parse in string
	extempore-repl-result			; Result, or error message
	extempore-repl-error-type			; string, nil if no error
	(extempore-repl-output "")		; result to display
	(extempore-repl-wbuf extempore-repl-working-buffer)	; current buffer after evaluation
	(extempore-repl-pmark (extempore-repl-pm)))
    (unless (extempore-repl-is-whitespace-or-comment extempore-repl-string)
      (condition-case err
	  (let ((rout (read-from-string extempore-repl-string)))
	    (setq extempore-repl-form (car rout)
		  extempore-repl-pos (cdr rout)))
	(error (setq extempore-repl-result (error-message-string err))
	       (setq extempore-repl-error-type "Read error")))
      (unless extempore-repl-error-type
	;; Make sure working buffer has not been killed
	(if (not (buffer-name extempore-repl-working-buffer))
	    (setq extempore-repl-result "Working buffer has been killed"
		  extempore-repl-error-type "extempore-repl Error"
		  extempore-repl-wbuf (current-buffer))
	  (if (extempore-repl-is-whitespace-or-comment (substring extempore-repl-string extempore-repl-pos))
	      ;; To correctly handle the extempore-repl-local variables *,
	      ;; ** and ***, we need a temporary buffer to be
	      ;; current at entry to the inner of the next two let
	      ;; forms.  We need another temporary buffer to exit
	      ;; that same let.  To avoid problems, neither of
	      ;; these buffers should be alive during the
	      ;; evaluation of extempore-repl-form.
	      (let ((*1 *)
		    (*2 **)
		    (*3 ***)
		    extempore-repl-temp-buffer)
		(set-match-data extempore-repl-match-data)
		(save-excursion
		  (with-temp-buffer
		    (condition-case err
			(unwind-protect
			     ;; The next let form creates default
			     ;; bindings for *, ** and ***.  But
			     ;; these default bindings are
			     ;; identical to the extempore-repl-local
			     ;; bindings.  Hence, during the
			     ;; evaluation of extempore-repl-form, the
			     ;; extempore-repl-local values are going to be
			     ;; used in all buffers except for
			     ;; other extempore-repl buffers, which override
			     ;; them.  Normally, the variables *1,
			     ;; *2 and *3 also have default
			     ;; bindings, which are not overridden.
			     (let ((* *1)
				   (** *2)
				   (*** *3))
			       (kill-buffer (current-buffer))
			       (set-buffer extempore-repl-wbuf)
			       (setq extempore-repl-result
                                     (eval extempore-repl-form lexical-binding))
			       (setq extempore-repl-wbuf (current-buffer))
			       (setq
				extempore-repl-temp-buffer
				(generate-new-buffer " *extempore-repl-temp*"))
			       (set-buffer extempore-repl-temp-buffer))
			  (when extempore-repl-temp-buffer
			    (kill-buffer extempore-repl-temp-buffer)))
		      (error (setq extempore-repl-result (error-message-string err))
			     (setq extempore-repl-error-type "Eval error"))
		      (quit (setq extempore-repl-result "Quit during evaluation")
			    (setq extempore-repl-error-type "Eval error")))))
		(setq extempore-repl-match-data (match-data)))
	    (setq extempore-repl-error-type "extempore-repl error")
	    (setq extempore-repl-result "More than one sexp in input"))))

      ;; If the eval changed the current buffer, mention it here
      (unless (eq extempore-repl-wbuf extempore-repl-working-buffer)
	(message "current buffer is now: %s" extempore-repl-wbuf)
	(setq extempore-repl-working-buffer extempore-repl-wbuf))

      (goto-char extempore-repl-pmark)
      (unless extempore-repl-error-type
	(condition-case nil
	    ;; Self-referential objects cause loops in the printer, so
	    ;; trap quits here. May as well do errors, too
	    (setq extempore-repl-output (concat extempore-repl-output (pp-to-string extempore-repl-result)))
	  (error (setq extempore-repl-error-type "extempore-repl Error")
		 (setq extempore-repl-result "Error during pretty-printing (bug in pp)"))
	  (quit  (setq extempore-repl-error-type "extempore-repl Error")
		 (setq extempore-repl-result "Quit during pretty-printing"))))
      (if extempore-repl-error-type
	  (progn
	    (when extempore-repl-noisy (ding))
	    (setq extempore-repl-output (concat extempore-repl-output "*** " extempore-repl-error-type " ***  "))
	    (setq extempore-repl-output (concat extempore-repl-output extempore-repl-result)))
	;; There was no error, so shift the *** values
	(setq *** **)
	(setq ** *)
	(setq * extempore-repl-result))
      (setq extempore-repl-output (concat extempore-repl-output "\n")))
    (setq extempore-repl-output (concat extempore-repl-output extempore-repl-prompt-internal))
    (comint-output-filter (extempore-repl-process) extempore-repl-output)))

;;; Process and marker utilities

(defun extempore-repl-process nil
  ;; Return the current buffer's process.
  (get-buffer-process (current-buffer)))

(defun extempore-repl-pm nil
  ;; Return the process mark of the current buffer.
  (process-mark (get-buffer-process (current-buffer))))

(defun extempore-repl-set-pm (pos)
  ;; Set the process mark in the current buffer to POS.
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

;;; Major mode

(define-derived-mode inferior-extempore-mode comint-mode "extempore-repl"
  "Major mode for interactively evaluating Extempore expressions.
Uses the interface provided by `comint-mode' (which see).

* \\<extempore-repl-map>\\[extempore-repl-send-input] evaluates the sexp following the prompt.  There must be at most
  one top level sexp per prompt.

* \\[extempore-repl-return] inserts a newline and indents, or evaluates a
  complete expression (but see variable `extempore-repl-dynamic-return').
  Inputs longer than one line are moved to the line following the
  prompt (but see variable `extempore-repl-dynamic-multiline-inputs').

* \\[comint-dynamic-complete] completes Lisp symbols (or filenames, within strings),
  or indents the line if there is nothing to complete.

The current working buffer may be changed (with a call to `set-buffer',
or with \\[extempore-repl-change-working-buffer]), and its value is preserved between successive
evaluations.  In this way, expressions may be evaluated in a different
buffer than the *extempore-repl* buffer.  By default, its name is shown on the
mode line; you can always display it with \\[extempore-repl-print-working-buffer], or the buffer itself
with \\[extempore-repl-display-working-buffer].

During evaluations, the values of the variables `*', `**', and `***'
are the results of the previous, second previous and third previous
evaluations respectively.  If the working buffer is another extempore-repl
buffer, then the values in the working buffer are used.  The variables
`*1', `*2' and `*3', yield the process buffer values.

Expressions evaluated by extempore-repl are not subject to `debug-on-quit' or
`debug-on-error'.

The behavior of extempore-repl may be customized with the following variables:
* To stop beeping on error, set `extempore-repl-noisy' to nil.
* If you don't like the prompt, you can change it by setting `extempore-repl-prompt'.
* If you do not like that the prompt is (by default) read-only, set
  `extempore-repl-prompt-read-only' to nil.
* Set `extempore-repl-dynamic-return' to nil for bindings like `lisp-interaction-mode'.
* Entry to this mode runs `comint-mode-hook' and `extempore-repl-mode-hook'
 (in that order).

Customized bindings may be defined in `extempore-repl-map', which currently contains:
\\{extempore-repl-map}"
  :syntax-table extempore-mode-syntax-table

  (setq comint-prompt-regexp (concat "^" (regexp-quote extempore-repl-prompt)))
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (setq comint-input-sender 'extempore-repl-input-sender)
  (setq comint-process-echoes nil)
  (set (make-local-variable 'comint-dynamic-complete-functions)
       '(extempore-repl-tab comint-replace-by-expanded-history
	 extempore-repl-complete-filename extempore-repl-complete-symbol))
  (set (make-local-variable 'extempore-repl-prompt-internal) extempore-repl-prompt)
  (set (make-local-variable 'comint-prompt-read-only) extempore-repl-prompt-read-only)
  (setq comint-get-old-input 'extempore-repl-get-old-input)
  (set (make-local-variable 'comint-completion-addsuffix) '("/" . ""))
  (setq mode-line-process '(":%s on " (:eval (buffer-name extempore-repl-working-buffer))))

  (set (make-local-variable 'indent-line-function) 'extempore-repl-indent-line)
  (set (make-local-variable 'extempore-repl-working-buffer) (current-buffer))
  (set (make-local-variable 'fill-paragraph-function) 'lisp-fill-paragraph)
  (add-hook 'completion-at-point-functions
            'lisp-completion-at-point nil 'local)

  ;; Value holders
  (set (make-local-variable '*) nil)
  (set (make-local-variable '**) nil)
  (set (make-local-variable '***) nil)
  (set (make-local-variable 'extempore-repl-match-data) nil)

  ;; font-lock support
  (set (make-local-variable 'font-lock-defaults)
       '(extempore-repl-font-lock-keywords nil nil ((?: . "w") (?- . "w") (?* . "w"))))

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
	(start-process "extempore-repl" (current-buffer) "hexl")
      (file-error (start-process "extempore-repl" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (extempore-repl-process) nil)
    (goto-char (point-max))

    ;; Lisp output can include raw characters that confuse comint's
    ;; carriage control code.
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)

    ;; Add a silly header
    (insert extempore-repl-header)
    (extempore-repl-set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (extempore-repl-process) extempore-repl-prompt-internal)
    (set-marker comint-last-input-start (extempore-repl-pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)))

(defun extempore-repl-get-old-input nil
  ;; Return the previous input surrounding point
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

;;; User command

;;;###autoload
(defun extempore-repl nil
  "Interactively evaluate Extempore expressions.
Switches to the buffer `*extempore-repl*', or creates it if it does not exist."
  (interactive)
  (let (old-point)
    (unless (comint-check-proc "*extempore-repl*")
      (with-current-buffer (get-buffer-create "*extempore-repl*")
	(unless (zerop (buffer-size)) (setq old-point (point)))
	(inferior-extempore-mode)))
    (switch-to-buffer "*extempore-repl*")
    (when old-point (push-mark old-point))))

(provide 'extempore-repl)

;;; extempore-repl.el ends here

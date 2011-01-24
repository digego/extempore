;; Put (autoload 'extempore-mode "/path/to/thisfile" "" t) in .emacs.
;;
;; This mode uses the normal Scheme key bindings except that the sequence
;;    control-x control-x sends a top level def to Extempore
;;    control-x control-r sends a region of defs to Extempore
;;
;; Future work: get TAB completion to work!

(require 'scheme)

(defvar extempore-keydef "\C-x\C-x")     ; key sequence to send a definition
(defvar extempore-keyreg "\C-x\C-r")     ; key sequence to send a region
(defvar extempore-port 7099)             ; TCP port to Extempore
(defvar extempore-process nil)           ; process during TCP connection

(define-minor-mode extempore-mode 
   "Toggle the mode for interacting with Scheme in Extempore over TCP"
   :init-value nil :lighter " Extempore" :keymap scheme-mode-map
   (if extempore-mode (extempore-go) (extempore-stop)))

(defun extempore-go ()                   ; start connection to Extempore
  (define-key scheme-mode-map extempore-keydef 'extempore-send-definition)
  (define-key scheme-mode-map extempore-keyreg 'extempore-send-region)
  (setq extempore-process
	(open-network-stream "extempore" nil "localhost" extempore-port))
  (set-process-filter extempore-process 
	'(lambda (proc str) (message (substring str 0 -1)))))

(defun extempore-stop ()                 ; terminate connection to Extempore
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
      ; (process-send-region extempore-process (point) (mark))
      (redisplay)                          ; flash the def like Extempore
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

;;;;;;;;;;;;;;;;;;;;;;
;; extempore logger ;;
;;;;;;;;;;;;;;;;;;;;;;

;; WARNING: highly experimental, will almost certainly set your hair
;; on fire.

(define-minor-mode exlog-mode
  "This minor mode automatically logs all keystrokes (and
  extempore code sent for evaluation) in all Extempore buffers."
  :global t
  :init-value nil
  :lighter " Log"
  :keymap nil
  :group 'extempore

  (if exlog-mode
      (exlog-start-logging)
    (exlog-stop-logging)))

(defun exlog-start-logging ()
  (add-hook 'pre-command-hook 'exlog-pre-command-hook)
  (call-interactively 'exlog-log-comment)
  (exlog-advise-functions exlog-special-functions))

(defun exlog-stop-logging ()
  (remove-hook 'pre-command-hook 'exlog-pre-command-hook)
  (call-interactively 'exlog-log-comment)
  (exlog-write-log-buffer)
  (exlog-unadvise-functions exlog-special-functions))

(defun exlog-new-session ()
  (interactive)
  (if (get-buffer "*exlog*")
      (call-interactively 'exlog-stop-logging))
  (exlog-start-logging))

;; advise functions for logging

(defun exlog-advise-functions (func-list)
  "Advise (via defadvice) the key extempore-mode functions"
  (mapc (lambda (function)
          (ad-add-advice
           function
           '(exlog-advice
             nil t
             (advice . (lambda ()
                         (let ((args (ad-get-args 0)))
                           (exlog-log-command
                            real-this-command
                            current-prefix-arg
                            (if (member real-this-command
                                        '(extempore-send-definition
                                          extempore-send-region
                                          extempore-send-buffer))
                                (buffer-substring-no-properties
                                 (car args) (cadr args))
                              args))))))
           'after 'first)
          (ad-activate function))
        func-list))

(defun exlog-unadvise-functions (func-list)
  "Remove advice from special extempore-mode functions"
  (mapc (lambda (function)
          (ad-remove-advice function 'after 'exlog-advice))
        func-list))

(defvar exlog-special-functions
  '(extempore-send-region
    extempore-connect
    extempore-disconnect)
  "A list of extempore-mode functions to specifically instrument for logging")

(defun exlog-yasnippet-hook ()
  (exlog-log-command 'yas-expand
                     nil
                     (buffer-substring-no-properties yas-snippet-beg yas-snippet-end)))

(add-hook 'yas-after-exit-snippet-hook
          'exlog-yasnippet-hook)

(defun exlog-log-comment (comment)
  (interactive "sAny comments about this particular session? ")
  (exlog-write-log-entry (buffer-name) 'user-comment nil comment))

(defun exlog-write-log-entry (bname command event args)
  (with-current-buffer (get-buffer-create "*exlog*")
    (insert
     (format "(#inst \"%s\" %s %s %s %s)\n"
             (format-time-string "%Y-%m-%dT%T.%3N")
             bname
             command
             event
             (if (stringp args) (prin1-to-string args) args)))))

(defun exlog-log-command (command event args)
  (if (and (equal major-mode 'extempore-mode)
           (symbolp command))
    (exlog-write-log-entry (buffer-name)
                           (symbol-name command)
                           event
                           args)))

(defun exlog-pre-command-hook ()
  (let (deactivate-mark)
    (exlog-log-command real-this-command last-input-event current-prefix-arg)))

;; writing command list to file

(defun exlog-write-log-buffer ()
  (let ((logfile-name (concat extempore-share-directory
                              "keylogs/"
                              (format-time-string "%Y%m%dT%H%M%S-")
                              user-login-name
                              ".keylog"))
        (log-buffer (get-buffer "*exlog*")))
    (if log-buffer
        (with-current-buffer log-buffer
          (write-region nil nil logfile-name nil nil nil t)
          (kill-buffer log-buffer)
          (async-shell-command (format "gzip %s" logfile-name))
          (message "Writing Extempore keylog file to %s" logfile-name)
          (run-with-timer 1 nil 'kill-buffer "*Async Shell Command*"))
      (message "No log buffer found."))))

(global-set-key (kbd "C-c C-l") 'exlog-mode)

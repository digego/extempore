;;; .emacs -- a starter Emacs config for working with Extempore
;; Author: You! (with a little help from Ben Swift to get your started...)

;;; Commentary:

;; If you're new to Emacs...

;; This file is a 'starter' .emacs ('dot emacs') file, which might
;; come in handy if you're new to Emacs. When Emacs starts, it looks
;; in your home directory for a file called .emacs, and if it finds
;; one it loads it up. This is where your own Emacs configuration and
;; personalisations go.

;; This file is intended for people who are new to emacs. It has a few
;; handy defaults, and also sets up Emacs to work nicely with
;; Extempore.

;; Feel free to mess with it as much as you like: tweaking
;; one's .emacs is something of a right of passage for Emacs users :)

;; Installation:

;; To 'install' this file, just copy it to your home directory, e.g.
;; 
;;   $ cp /path/to/extempore/extras/.emacs ~
;; 
;; after that, Emacs will load the file on startup.

;; You'll also need to change the `extempore-share-directory' variable
;; (further down in this file) to point to your Extempore source
;; directory.

;; If you're already an Emacs user...

;; You'll have your own .emacs with its own tweaks. Just grab
;; extempore-mode.el from MELPA:

;; M-x `package-install' RET `extempore-mode' RET

;; and see the commentary at the top of that file for usage
;; instructions.

;; Further info:

;; https://github.com/digego/extempore
;; http://benswift.me/extempore-docs/

;;;;;;;;;;
;; elpa ;;
;;;;;;;;;;

;; this is the Emacs package manager

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(unless package--initialized
  (package-initialize))

(when (null package-archive-contents)
  (package-refresh-contents))

;; add/remove any packages you like here
(dolist (package
         '(ido-ubiquitous
           magit
           markdown-mode
           smex
           yasnippet
           extempore-mode))
  (if (not (package-installed-p package))
      (package-install package)))

;;;;;;;;;;
;; smex ;;
;;;;;;;;;;

(smex-initialize)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(ido-mode 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display & appearance ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq visible-bell t)
(setq inhibit-startup-message t)
(setq color-theme-is-global t)

(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 2)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(show-paren-mode 1)
(column-number-mode 1)
(hl-line-mode t)

;; show time and battery status in mode line

(display-time-mode 1)
(setq display-time-format "%H:%M")
(display-battery-mode 1)

;; whitespace

(setq sentence-end-double-space nil)
(setq shift-select-mode nil)
(setq whitespace-style '(face trailing lines-tail tabs))
(setq whitespace-line-column 80)
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; mark region commands as safe

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; text mode tweaks

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(remove-hook 'text-mode-hook 'smart-spacing-mode)

;; file visiting stuff

(setq save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq recentf-max-saved-items 100)

(global-auto-revert-mode t)

;; other niceties

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq diff-switches "-u")

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defalias 'yes-or-no-p 'y-or-n-p)

;; pretty lambdas

(add-hook 'prog-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              nil `(("(?\\(lambda\\>\\)"
                     (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                               ,(make-char 'greek-iso8859-7 107))
                               nil)))))))

;;;;;;;;;;;;;;;;;
;; color theme ;;
;;;;;;;;;;;;;;;;;

;; uncomment these lines when you've found a theme you like (replace
;; `your-theme') and want to load it on startup.

;; (if (display-graphic-p)
;;     (load-theme 'your-theme t))

;;;;;;;;;;;
;; faces ;;
;;;;;;;;;;;

;; to choose a font (and size), you can use this code

;; ;; font size
;; (setq base-face-height 200)
;; ;; monospace font
;; (set-face-attribute 'default nil :height base-face-height :family "Ubuntu Mono")
;; ;; variable-width font
;; (set-face-attribute 'variable-pitch nil :height base-face-height :family "Ubuntu")

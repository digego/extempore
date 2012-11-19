;; This file is a 'starter' .emacs ('dot emacs') file if you're new to
;; Emacs. When Emacs starts, it looks in your home directory for a
;; file called .emacs, and if it finds one it loads it up. This is
;; where your own Emacs configuration and personalisations go.

;; This file has a few handy defaults, and also sets up Emacs to work
;; nicely with Extempore. Feel free to mess with it as much as you
;; like: tweaking one's .emacs is something of a right of passage for
;; Emacs users :)

;; To 'install' this file, just copy it to your home directory, e.g.
;; 
;;   $ cp /path/to/Extempore/extras/.emacs ~
;; 
;; after that, Emacs will load the file on startup.

;; The only other thing you have to change manually is the
;; extempore-path variable (further down in this file).

;;;;;;;;;;
;; elpa ;;
;;;;;;;;;;

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(package-refresh-contents)

;; add/remove any packages you like here

(dolist (p '(markdown-mode
             yasnippet
             yasnippet-bundle
             smex
             ido-ubiquitous
             magit
             org))
  (when (not (package-installed-p p))
    (package-install p)))

;; (package-initialize)

;;;;;;;;;;
;; smex ;;
;;;;;;;;;;

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;;;;;;;;;;;;;
;; from ESK ;;
;;;;;;;;;;;;;;

(setq save-place t)
(hl-line-mode t)
(global-auto-revert-mode t)
(remove-hook 'text-mode-hook 'smart-spacing-mode)

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Set this to whatever browser you use
;; (setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-default-macosx-browser)
;; (setq browse-url-browser-function 'browse-default-windows-browser)
;; (setq browse-url-browser-function 'browse-default-kde)
;; (setq browse-url-browser-function 'browse-default-epiphany)
;; (setq browse-url-browser-function 'browse-default-w3m)
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "~/src/conkeror/conkeror")

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;
;; appearance ;;
;;;;;;;;;;;;;;;;

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; pretty lambdas

(add-hook 'prog-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              nil `(("(?\\(lambda\\>\\)"
                     (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                               ,(make-char 'greek-iso8859-7 107))
                               nil)))))))

;;;;;;;;;;;
;; faces ;;
;;;;;;;;;;;

;; choose whichever fonts you like here

;; font size
(setq base-face-height 200)
;; monospace font
(set-face-attribute 'default nil :height base-face-height :family "Ubuntu Mono")
;; variable-width font
(set-face-attribute 'variable-pitch nil :height base-face-height :family "Ubuntu")

;;;;;;;;;;;;;;;
;; extempore ;;
;;;;;;;;;;;;;;;

;; set this to wherever you put the extempore source directory
(setq extempore-path "/path/to/extempore")

(autoload 'extempore-mode (concat extempore-path "/extras/extempore.el") "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

;;;;;;;;;
;; git ;;
;;;;;;;;;

(add-to-list 'auto-mode-alist '(".*gitconfig$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '(".*gitignore$" . conf-unix-mode))

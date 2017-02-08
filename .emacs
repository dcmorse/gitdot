;; bindings I miss:
;; M-/ - complete an identifier using textual search
;; C-/j and C-jj - drop marker jump marker
;; Caps-M j and Caps-M k - scroll up and down
;; Caps-C-M j and Caps-C-M k - scroll up and down other pane

;;; my old font family (in terms of ~customize>faces>basic faces) was
;;; b&h-lucidatypewriter with height 136

(setq max-specpdl-size 10000);; keep fonts search from b*rking out.

(defun eemacs () (interactive) (find-file "~/gitdot/.emacs"))
(defun ebash  () (interactive) (find-file "~/.bashrc"))
(defun ehelp  () (interactive) (find-file "~/help.txt"))

(load "~/.emacs-mail-aliases.el" t)

(defun insert-blame-pointer ()
  (interactive)
  (require 'time-stamp)
  (insert (time-stamp-string "-%u %02m-%02d-%:y")))

(defun dave-unbury-buffer ()
  (interactive)
  ;; source yanked from mouse-unbury-buffer in xemacs-21.something
  (let* ((bufs (buffer-list))
         (entry (1- (length bufs)))
         val)
    (while (not (setq val (nth entry bufs)
                      val (and (/= (aref (buffer-name val) 0)
                                   ? )
                               val)))
      (setq entry (1- entry)))
    (switch-to-buffer val)))


(defvar pushed-stack-markers nil "(internal) see push-stack-marker")
(defvar popped-stack-markers nil "(internal) see push-stack-marker")

(defun push-stack-marker ()
  (interactive)
  "Push point onto list of stack markers.  Recall with pop-stack-marker.
See also: unpop-stack-marker."
  (push (point-marker) pushed-stack-markers))

(defun pop-stack-marker ()
  (interactive)
  "Recall marker pushed by push-stack-marker.  See also: unpop-stack-marker."
  (let ((marker (pop pushed-stack-markers)))
    (if marker
        (progn
          (goto-char marker)
          (push marker popped-stack-markers))
        (message "No markers left on stack"))))

(defun unpop-stack-marker ()
  (interactive)
  "Recall a marker that has been previously visited by pop-stack-marker."
  (let ((marker (pop popped-stack-markers)))
    (if marker
        (progn
          (goto-char marker)
          (push marker pushed-stack-markers))
        (message "No markers left on stack"))))


;; super       (charwise/linewise)
(global-set-key [?\s-j] 'next-line)
(global-set-key [?\s-k] 'previous-line)
(global-set-key [?\s-l] 'forward-char)
;; (customize gest confused by the simple approach)
;; (global-set-key (vector (read-from-string "?\\s-;")) 'forward-char)
;; nah, be like vi:
(global-set-key [?\s-h] 'backward-char)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-decrease) ; can't use C-minus because it's bound to negative-argument

;; super meta  (wordwise/pagewise)
(global-set-key [?\s-\M-j] 'scroll-up)
(global-set-key [?\s-\M-k] 'scroll-down)
(global-set-key [?\s-\M-l] 'forward-word)
(global-set-key [?\s-\M-h] 'backward-word)

;; super shift (sexp-wise/definition-wise)
(global-set-key [?\s-J] 'end-of-defun)
(global-set-key [?\s-K] 'beginning-of-defun)
(global-set-key [?\s-L] 'forward-sexp)
(global-set-key [?\s-H] 'backward-sexp)

;; super upper-row
;; super-p is masked by gnome for horrible reasons, easier to pick a new key for these
;; (global-set-key [?\s-o] 'bury-buffer)        ; replace with modeline voodoo
;; (global-set-key [?\s-p] 'dave-unbury-buffer) ; replace with modeline voodoo
(global-set-key [?\s-i] 'push-stack-marker)
(global-set-key [?\s-u] 'pop-stack-marker)
(global-set-key [?\s-y] 'unpop-stack-marker)

;; super left-hand
(global-set-key [?\s-d] 'bury-buffer)
(global-set-key [?\s-f] 'unbury-buffer)
(global-set-key [s-tab] 'indent-code-rigidly)
(global-set-key [?\M-\C-%] 'query-replace-regexp)



(defmacro bind-balanced-inserter (str key1 &optional keysetter)
  (if (not keysetter)
      (setq keysetter 'global-set-key))
  `(,keysetter ,key1 (lambda () (interactive)
		       (insert ,str)
		       (backward-char ,(/ (length str) 2)))))

(bind-balanced-inserter "\"\"" [?\M-\"])
(bind-balanced-inserter "''" [?\M-\'])
(bind-balanced-inserter "[]" [?\M-\[])
(bind-balanced-inserter "{}" [?\M-\{])
(bind-balanced-inserter "<>" [?\M-\C-<])

(global-set-key [?\[] '(lambda () (interactive) (insert "()") (backward-char)))
(global-set-key [?\]] 'up-list)

;; suck underlying line onto current line.
(global-set-key [?\M-\C-^] (lambda () 
			     (interactive) (forward-line)
			     (delete-indentation)))

(global-set-key [?\M-+] (lambda ()
			  (interactive)
			  (insert "(in-package :)")
			  (backward-char)))

(defun launchcode-grep (regexp)
  "grep launchcode source tree"
  (interactive "Mregexp: ") 
  (grep-apply-setting 'grep-command  (format "grep --color -nirH -e %s /home/dm/launch_code/app /home/dm/launch_code/spec" regexp))
  (call-interactively 'grep))

(defun contra-grep (regexp)
  "grep launchcode source tree"
  (interactive "Mregexp: ") 
  (grep-apply-setting 'grep-command  (format "grep --color -nirH -e %s /home/dm/contra/app /home/dm/contra/spec" regexp))
  (call-interactively 'grep))

(global-set-key [?\C-x ?\C-k] 'kill-this-buffer)
(global-set-key [f8] 'launchcode-grep)
(global-set-key [f7] 'contra-grep)
(global-set-key [f4] 'next-error)
(global-set-key '[C-f8] 'grep)
(global-set-key '[C-tab] 'other-window)
(global-set-key [?\M-g] 'goto-line)


(defvar *lisp-buffer-switcharoo-src-buffer* nil)
(defun lisp-buffer-switcharoo ()
    (interactive)
    (let* ((lisp-buff "*inferior-lisp*")
	   (buff (current-buffer))
	   (buff-name (buffer-name buff)))
      (if (string= buff-name lisp-buff)
	  (if *lisp-buffer-switcharoo-src-buffer*
	      (switch-to-buffer *lisp-buffer-switcharoo-src-buffer*)
	      (bury-buffer))
	  (progn (switch-to-buffer lisp-buff)
		 (setq *lisp-buffer-switcharoo-src-buffer* buff)))))




(defun insert-blame-pointer ()
  (interactive)
  (require 'time-stamp)
  (insert (time-stamp-string "-%u %02m-%02d-%:y")))
(global-set-key "\C-cu" 'insert-blame-pointer)

;; This function surely already exists somewhere, but I coudn't find it.
(defun beginning-of-linep () (interactive)
  (equal (point)
         (save-excursion (beginning-of-line) (point))))

(defun end-of-linep () (interactive)
  (equal (point)
         (save-excursion (end-of-line) (point))))

;; create a new face, for functions, varaibles, etc
;; that take a name as first argument.
(make-face 'defmumble)

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(compilation-scroll-output t)
 '(compilation-window-height 10)
 '(confirm-kill-emacs nil)
 '(current-language-environment "ASCII")
 '(default-frame-alist
    (quote
     ((foreground-color . "black")
      (background-color . "#d5dadf")
      (menu-bar-lines . 1))))
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(face-font-family-alternatives
   (quote
    (("bitstream-bitstream vera sans mono" "courier" "fixed")
     ("helv" "helvetica" "arial" "fixed"))))
 '(fancy-splash-image nil)
 '(flx-ido-mode t)
 '(font-lock-maximum-decoration nil)
 '(global-font-lock-mode t nil (font-lock))
 '(ido-everywhere t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "
")
 '(ispell-extra-args (quote ("-W" "2")))
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(mouse-yank-at-point t)
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace t)
 '(tab-width 2)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode (quote (only . t))))
;; bitstream font :face is spelled "bitstream-bitstream vera sans mono"
;; it works with font height 122
;; also "b&h-lucidatypewriter" at 136
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#d5dadf" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 128 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "Gray30"))))
 '(font-lock-function-name-face ((((class color) (background light)) (:foreground "Blue" :weight bold))))
 '(trailing-whitespace ((t nil))))




(put 'if 'lisp-indent-function nil)
(put 'labels 'lisp-indent-function 1)
(put 'macrolet 'lisp-indent-function 1)
(put 'multiple-value-bind 'lisp-indent-function 2)
(put 'with-slots 'lisp-indent-function 2)
(put 'mvbind 'lisp-indent-function 2)
(put 'mv-let* 'lisp-indent-function 1)
(put 'symbol-macrolet 'lisp-indent-function 1)
(put 'with-open-file 'lisp-indent-function 1)
(put 'destructuring-bind 'lisp-indent-function 2)
(mapc (lambda (x) (put x 'lisp-indent-function 1))
      '(case ecase ccase icase typecase etypecase ctypecase))
(mapc (lambda (x) (put x 'lisp-indent-function 1))
      '(do-vertexes do-vertex-infos))
(put 'let1 'lisp-indent-function 2)


(setq inferior-lisp-program "clisp -I")
;;(setq inferior-lisp-program "sbcl --userinit /dev/null") ; "clisp -I" "lisp"
;;(setq inferior-lisp-program "sbcl")


(put 'downcase-region 'disabled nil)



(defun boardgamegeek-bracketize (c)
  (interactive "c")
  (insert (format "[%c][/%c]" c c))
  (backward-char 4))

(defun angle-bracketize (c)
  (interactive "c")
  (insert (format "<%c></%c>" c c))
  (backward-char 4))

(setq auto-mode-alist
      (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

;; (load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent) ; doens't work beans!
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode) 
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent) 
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci) ; puts prime in words!

;; was "ghci"
(setq-default haskell-program-name "ghci -package HaXml") ;; or (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci


;; load problems
;; (define-key haskell-mode-map '[?\C-\M-\;] '(lambda () (interactive)
;;   (insert "{-  -}")
;;   (backward-char 3)))

(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\C-k" 'kill-this-buffer)
(global-set-key "\C-z" 'undo)
(global-set-key "\M-/" 'undo)

(put 'erase-buffer 'disabled nil)

(put 'set-goal-column 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(setq-default indent-tabs-mode nil)

(global-set-key "\M-/" 'dabbrev-expand)

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

(global-set-key (kbd "s-r") 'point-to-register)
(global-set-key (kbd "s-R") 'jump-to-register)
(global-set-key (kbd "s-c") 'copy-to-register)
(global-set-key (kbd "s-v") 'insert-register)

(put 'erase-buffer 'disabled nil)




(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; is this actually working?
;; source: https://www.emacswiki.org/emacs/BackupDirectory
(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs-autosave"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)                    ; M-x package-list-packages, M-x package-install
; (require 'evil)
; (require 'coffee-mode)
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(setq ruby-deep-indent-paren nil)
(require 'inf-ruby)
(require 'projectile)
(projectile-global-mode)
(require 'ido)
(require 'flx-ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)



;; (add-to-list 'default-frame-alist '(font. "-PfEd-DejaVu Sans-normal-normal-normal-*-25-*-*-*-*-0-iso10646-1"))
;; (set-face-attribute 'default t :font "-PfEd-DejaVu Sans-normal-normal-normal-*-25-*-*-*-*-0-iso10646-1" )
;; (set-face-attribute 'default nil :font "-PfEd-DejaVu Sans-normal-normal-normal-*-25-*-*-*-*-0-iso10646-1" )
;; (set-frame-font "-PfEd-DejaVu Sans-normal-normal-normal-*-25-*-*-*-*-0-iso10646-1" nil t)

;; (set-default-font “Terminus-9”)
(setq js-indent-level 2
      css-indent-offset 2)

(defun insert-dash () (interactive) (insert "-"))
(defun insert-underscore () (interactive) (insert "_"))
(defun swap-dash-with-underscore ()
  (interactive)
  (local-set-key [?_] (if (eql 'self-insert-command (key-binding [?_])) 'insert-dash       'self-insert-command))
  (local-set-key [?-] (if (eql 'self-insert-command (key-binding [?-])) 'insert-underscore 'self-insert-command)))
(defun restore-dash-underscore ()
  (interactive)
  (local-set-key [?_] 'self-insert-command)
  (local-set-key [?-] 'self-insert-command))

;; (add-hook 'enh-ruby-mode-hook 'swap-dash-with-underscore)
;; (add-hook 'ruby-mode-hook 'swap-dash-with-underscore)


(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(setq js-indent-level 2)

;; -*- mode: Emacs-Lisp; truncate-lines: t; -*-

;; Copyright (C) 1998 - 2012  Jo Odland
;; All rights reserved.
;;
;; Filename:    .emacs
;; Description: GNUEmacs 24.x
;; Author:      Jo Odland <jo.odland@gmail.com>
;;
;;

(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)


;; add user local site-lisp to load-path
(if (file-exists-p (expand-file-name "~/site-lisp"))
    (add-to-list 'load-path (expand-file-name "~/site-lisp")))

;; built-ins
(require 'css-mode)
(require 'bs)                           ; a better show-buffer C-x C-b
(require 'uniquify)                     ; Unique buffer names
(require 'sql)

;; package.el
(require 'mic-paren)                    ; better paren matching
(require 'magit)                        ; git mode
(require 'markdown-mode)                ; markdown mode
(require 'clojure-mode)                 ; clojure major-mode
(require 'js2-mode)                     ; a better javascript major- mode
(require 'mustache-mode)                ; mustache major-mode
(require 'scala-mode2)                  ; scala major-mode
(require 'restclient)                   ; REST client
(require 'expand-region)                ; expand region on scope at the time
(require 'python-mode)                  ; python mode
(require 'enh-ruby-mode)                ; ruby mode

;; site-lisp
(require 'thrift-mode)                  ; thrift major-mode
(require 'bm)                           ; visual bookmarks
(require 'uniq)                         ; unix uniq tool on emacs buffers

;;(require 'confluence)                   ; confluence editor

;; whitespace mode
(when (require 'ethan-wspace nil 'noerror)
  ;; make whitespace stand out
  (global-ethan-wspace-mode 1))

;; enable diff marks in fringe
(if (boundp 'global-diff-hl-mode)
    (global-diff-hl-mode))

;; follow links to version controlled files
(setq vc-follow-symlinks t)

;; enable File->Open Recent-> menu item
(recentf-mode)

;; set ruby path
(setq enh-ruby-program "/Users/fijoodla/.rvm/rubies/ruby-1.9.3-p327/bin/ruby")

;; interpreter-mode-alist
(add-to-list 'interpreter-mode-alist (cons "perl" 'cperl-mode))

;; custom
(setq custom-file "~/.emacs.d/.custom")

;; set backup directory
(add-to-list 'backup-directory-alist (cons "." (expand-file-name "~/.emacs.d/.backups")))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; enable paren matching
(paren-activate)

;; configure *scratch* buffer
(setq inhibit-startup-message t         ; drop emacs-info
      initial-major-mode 'text-mode)    ; use text-mode as initial mode


;;; Mode triggers
(setq auto-mode-alist
       (append '(("\\.\\([pP][Llm]\\|al\\)$" . cperl-mode)
                 ("\\.js$" . js2-mode)
                 ("\\.json$" . js2-mode)
                 ("\\.sql$" . sql-mode)
                 ("\\.thrift$" . thrift-mode)
                 ("\\.xml$" . nxml-mode)
                 ("\\.md$" . markdown-mode)
                 ("\\.rb$" . enh-ruby-mode)
                 ("\\.css$" . css-mode))
               auto-mode-alist))

;; set unified diff mode
(setq diff-switches "-u")

;; quiet, please! No dinging!
(setq visible-bell nil)
(setq ring-bell-function `ignore)


;; ensure utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; preserve point when scrolling
(setq scroll-preserve-screen-position t)

;; avoid recentering point when scrolling
(setq scroll-conservatively 3)

;; disable tab indent
(setq-default indent-tabs-mode nil)

;; always truncate lines
(setq-default truncate-lines t)

;; scroll to bottom on output
(setq comint-scroll-to-bottom-on-output t)

;; enable temp-buffer-resize-mode
(temp-buffer-resize-mode)

;; enable mouse wheel
(mouse-wheel-mode)

;; enable transient mark
(transient-mark-mode)

;; disable auto fill
(auto-fill-mode 0)

;; turn on font lock
(global-font-lock-mode)

;; prompt when quiting emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; change yes/no confirmation to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; column number in modeline
(setq column-number-mode t)

;; yank at point
(setq mouse-yank-at-point t)

;; real Norwegians don't use double space after sentences!
(setq sentence-end-double-space nil)

;; stay at end-of-line when moving vertically
(setq track-eol t)

;; enable disabled functions
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer     'disabled nil)

;; disable toolbar
(tool-bar-mode -1)

;; disable scrollbar
(scroll-bar-mode -1)

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; restore window configuration, C-c left, C-c right
(winner-mode 1)

;; disable blinking cursor
(blink-cursor-mode -1)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


;;
;; Faces
;;

;; Set proper face looks if colour window-system
(if (and window-system (> (x-display-planes) 1))
    (progn

      ;; Set background and foreground color
      (set-foreground-color "gainsboro")
      (set-background-color "black")

      ;; Set fringe face
      (if (facep 'fringe)
          (progn
            (set-face-foreground 'fringe "green")
            (set-face-background 'fringe "black")))


      ;; Set proper modeline look
      (if (facep 'modeline)
          (progn
            (set-face-foreground 'modeline "green")
            (set-face-background 'modeline "black")))

      ))



;;;
;;; Functions
;;;


(defun url-decode-region (start end)
  "URL decode a region."
  (interactive "r")
  (save-excursion
    (let ((text (url-unhex-string (buffer-substring start end))))
      (delete-region start end)
      (insert text))))


(defun url-encode-region (start end)
  "URL encode a region."
  (interactive "r")
  (save-excursion
    (let ((text (url-hexify-string (buffer-substring start end))))
      (delete-region start end)
      (insert text))))


(defun goto-column (&optional arg)
  "Goto column."
  (interactive "nGoto column: ")
  (move-to-column arg))


(defun shift-region-right (start end arg)
  "Shift region right one character."
  (interactive "r\np")
  (indent-rigidly start end (if (null arg) 1 arg))

  ;; do not deactivate mark
  (setq deactivate-mark nil))


(defun shift-region-left (start end arg)
  "Shift region left one character."
  (interactive "r\np")
  (indent-rigidly start end (- (if (null arg) 1 arg)))

  ;; do not deactivate mark
  (setq deactivate-mark nil))


;; Set `sql-set-product' to something other than 'ansi
;; to avoid error when calling `sql-connect'.
(sql-set-product 'postgres)

;; Sybase
(setenv "SYBASE" "/Applications/Sybase/System")
(setq sql-sybase-program "/Applications/Sybase/System/OCS-15_0/bin/isql")
(setq sql-sybase-options '("-e" "-n" "-w" "2048" "-I" "/Applications/Sybase/System/interfaces")) ; isql --help for description of options

;; MySQL
(setq sql-mysql-program "/usr/local/bin/mysql")

;; PostgreSQL
(setq sql-postgres-program "/usr/local/bin/psql")

;; sql connection settings (connect to a database with (sql-connect))
(if (file-exists-p (expand-file-name "~/site-lisp/sql-connections.el"))
    (load-file (expand-file-name "~/site-lisp/sql-connections.el")))

(defun sql-postgres-mode nil
  "Enable `sql-mode' and set SQL dialect to PostgreSQL."
  (interactive)

  (sql-mode)
  (sql-set-product 'postgres)
  (sql-highlight-postgres-keywords)

  (sql-set-sqli-buffer))

(defun sql-sybase-mode nil
  "Enable `sql-mode' and set SQL dialect to Sybase."
  (interactive)

  (sql-mode)
  (sql-set-product 'sybase)
  (sql-highlight-sybase-keywords)

  (sql-set-sqli-buffer))




;;
;; HOOKS
;;


;; SQL mode hook
(add-hook 'sql-mode-hook
          (lambda ()
            ;; customize comments
            (setq comment-start-skip "-- *")
            (setq comment-column 40)

            ;; enable sybase syntaxs highlighting
            ;; (sql-highlight-sybase-keywords)

            (define-key sql-mode-map "\t" 'indent-relative-maybe)))


;; sql-interactive
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            ;; don't wrap output lines
            (setq truncate-lines t)

            ;; turn of font lock
            (font-lock-mode nil)

            ;; rename output buffer
            (setq sql-alternate-buffer-name (concat sql-user "@" sql-server "(" sql-database ")"))
            (sql-rename-buffer)))



;; make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;;
;;; Keys
;;;

;; bs
(define-key global-map "\C-x\C-b" 'bs-show)

;; undo
(global-unset-key (kbd "M-z"))          ; unbind M-z
(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "M-z") 'undo)

;; other-window
(define-key global-map (kbd "<C-tab>") 'other-window)

;; bookmark (bm.el)
(define-key global-map [f2] 'bm-next)
(define-key global-map [S-f2] 'bm-previous)
(define-key global-map [C-f2] 'bm-toggle)
(define-key global-map (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
(define-key global-map (kbd "<right-fringe> <mouse-1>") 'bm-toggle-mouse)

;; Font size
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "M-0") (lambda ()
                              (interactive)
                              (text-scale-set 0))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; expand-region
(global-set-key (kbd "s-<up>") 'er/expand-region)
(global-set-key (kbd "s-<down>") 'er/contract-region)


(global-set-key (kbd "C-<") 'shift-region-left)
(global-set-key (kbd "C->") 'shift-region-right)


;;;
;;; Custom
;;;

;; set-variables
(custom-set-variables

 ;; bs.el
 '(bs-must-always-show-regexp "\\(^\\*scratch\\*\\|^\\*SQL\\)")
 '(bs-default-sort-name "by name")

 ;; confluence customization
 ;; '(confluence-url "http://confluence.finn.no/rpc/xmlrpc")
 ;; '(confluence-default-space-alist (list (cons confluence-url "DEV")))

 ;; mac osx
 ;; '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(ns-function-modifier (quote control))
 '(ns-option-modifier nil)


 ;;
 ;; cperl
 '(cperl-hairy t)
 '(cperl-highlight-variables-indiscriminately t)
)

;; set-faces
(custom-set-faces

 ;; font: -apple-Monaco-Medium-normal-normal-*-11-*-*-*-m-0-iso10646-1
 '(default ((t (:height 110 :family "Monaco"))))
 ;; empty
 )


;; start emacs server (to support emacsclient)
(server-start)

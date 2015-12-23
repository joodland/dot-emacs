;; -*- mode: Emacs-Lisp; truncate-lines: t; -*-

;; Copyright (C) 1998 - 2015  Jo Odland
;; All rights reserved.
;;
;; Filename:    .emacs
;; Description: GNUEmacs 24.x
;; Author:      Jo Odland <jo.odland@gmail.com>
;;
;;

(require 'package)

;:; http://stackoverflow.com/questions/14836958/updating-packages-in-emacs
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(package-initialize)


;; add user local site-lisp to load-path
(if (file-exists-p (expand-file-name "~/site-lisp"))
    (add-to-list 'load-path (expand-file-name "~/site-lisp")))

;; built-ins
(require 'css-mode)
(require 'bs)                           ; a better show-buffer C-x C-b
(require 'uniquify)                     ; Unique buffer names
(require 'sql)
;(require 'auto-complete)

;; package.el
(require 'mic-paren)                    ; better paren matching
(require 'markdown-mode)                ; markdown mode
(require 'clojure-mode)                 ; clojure major-mode
(require 'js2-mode)                     ; a better javascript major- mode
(require 'mustache-mode)                ; mustache major-mode
(require 'scala-mode2)                  ; scala major-mode
(require 'restclient)                   ; REST client
(require 'expand-region)                ; expand region on scope at the time
(require 'python-mode)                  ; python mode
(require 'enh-ruby-mode)                ; ruby mode
(require 'cider)                        ; clojure mode
(require 'exec-path-from-shell)         ; fix PATH issues on Mac OSX
(require 'git-gutter-fringe)            ; git changes in the gutter

;; site-lisp
(require 'thrift-mode)                  ; thrift major-mode
(require 'bm)                           ; visual bookmarks
(require 'uniq)                         ; unix uniq tool on emacs buffers


;; mac osx, import environment
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; set default frame size
;; (setq default-frame-alist
;;       '((top . 1)
;;         (left . 45)
;;         (width . 90)
;;         (height . 70)
;;         ))

;; make cursor visible after scrolling
(beacon-mode 1)

;; turn on git-gutter
(global-git-gutter-mode t)
(setq git-gutter-fr:side 'right-fringe)

(when (window-system)
  (set-face-foreground 'git-gutter:added "#0c0")
  (set-face-foreground 'git-gutter:deleted "#c00")
  (set-face-foreground 'git-gutter:modified "#c0c"))




;; whitespace mode
(when (require 'ethan-wspace nil 'noerror)
  ;; make whitespace stand out
  (global-ethan-wspace-mode 1))

;; turn of final newline. Conflicts with ethan-wspace-mode
(setq mode-require-final-newline nil)

;; enable diff marks in fringe
(if (boundp 'global-diff-hl-mode)
    (global-diff-hl-mode))

;; follow links to version controlled files
(setq vc-follow-symlinks t)

;; enable File->Open Recent-> menu item
(recentf-mode t)
(setq recentf-max-saved-items 50)

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

;; uniq buffer names
(setq uniquify-buffer-name-style 'post-forward)

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

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; ensure utf-8
(set-language-environment "UTF-8")
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


;; set `default-directory' to ~/
(setq-default default-directory (expand-file-name "~/"))
(setq default-directory (expand-file-name "~/"))


(setq magit-last-seen-setup-instructions "1.4.0")



;; python
;(elpy-enable)
;(setq py-pyflakes-command "/usr/local/bin/pyflakes")

;;
(ido-mode)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-extensions t)



;;
;; Auto complete
;;
(global-auto-complete-mode t)

(setq-default ac-expand-on-auto-complete nil)
(setq-default ac-auto-start 2)
(setq-default ac-dwim nil)

; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
(setq tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)


;;
;; Faces
;;
(load-theme 'spolsky t) ;; https://github.com/owainlewis/emacs-color-themes




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



(defun ido-recentf-open ()
  "Use `ido-completing-readd' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " (mapcar 'abbreviate-file-name recentf-list)))
      (message "Opening file...")
    (message "No recent file list.")))


;;
;; HOOKS
;;

;; GO mode
(add-hook 'go-mode-hook
          (lambda ()
            ;; Customize compile command to run go build
            (set  (make-local-variable 'compile-command)
                   "go generate && go build -v && go test -v && go vet")

            (setq gofmt-command "goimports")
            (setq tab-width 4)

            (add-hook 'before-save-hook 'gofmt-before-save)))


;; SQL mode hook
(add-hook 'sql-mode-hook
          (lambda ()
            ;; customize comments
            (setq comment-start-skip "-- *")
            (setq comment-column 40)

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

;;
(add-hook 'cider-mode-hook
          (lambda ()
            (cider-turn-on-eldoc-mode)

            (setq cider-repl-pop-to-buffer-on-connect nil)
            (setq cider-popup-stacktraces nil)
            (setq cider-repl-popup-stacktraces t)
            (setq cider-repl-popup-stacktraces t)
            (setq nrepl-buffer-name-separator "-")
            (setq nrepl-buffer-name-show-port t)
            (setq cider-repl-display-in-current-window t)
            (setq cider-repl-print-length 100) ; the default is nil, no limit
            (setq cider-prompt-save-file-on-load nil)
            (setq cider-repl-result-prefix ";; => ")
            (setq cider-interactive-eval-result-prefix ";; => ")
            (setq cider-repl-use-clojure-font-lock t)
            (setq cider-switch-to-repl-command 'cider-switch-to-current-repl-buffer)

          ))




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
                              (text-scale-set 0)))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; expand-region
(global-set-key (kbd "s-<up>") 'er/expand-region)
(global-set-key (kbd "s-<down>") 'er/contract-region)


(global-set-key (kbd "C-<") 'shift-region-left)
(global-set-key (kbd "C->") 'shift-region-right)

;; find recent files, ido mode
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable ido style on M-x (smex.el)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; bind magit
(global-set-key (kbd "C-x g") 'magit-status)


;;;
;;; Custom
;;;

;; set-variables
(custom-set-variables

 ;; bs.el
 '(bs-must-always-show-regexp "\\(^\\*scratch\\*\\|^\\*SQL\\)")
 '(bs-default-sort-name "by name")

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
 '(default ((t (:height 120 :family "Hack"))))
 ;; empty
 )


;; start emacs server (to support emacsclient)
(server-start)

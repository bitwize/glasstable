;; This is Glass Table, an IDE (Interactive Development Environment)
;; for Gambit. Put quite simply, Glass Table is a REPL that lets you
;; save your work.

;; Glass Table maintains a "workspace" consisting of the symbols you
;; have defined at the top level and the forms you used to define
;; them. This workspace may then be saved into a Scheme file and later
;; recalled back into the workspace for future work. You may also add
;; other kinds of expressions to the workspace with a special setting
;; (see below). What's more, redefinitions of the same identifier will
;; clobber their old definitions in the workspace. (This only works
;; correctly when you redefine things the same way you defined them in
;; the first place; e.g., a symbol defined by `define-macro' should be
;; redefined with another `define-macro'.)

;; To begin using Glass Table simply enter './glass table' in this
;; directory. To use Glass Table from within a running Gambit instance
;; say:

;;   (load "glasstable.scm")
;;   (gt#repl)

;; This latter option is very handy for writing more complex Scheme
;; programs in Gambit for Android!

;; Various REPL commands are available:

;;   * ,(new-workspace)

;;     Clears all definitions in the workspace so you can start
;;     afresh. Beware; currently this does not affect the bindings in
;;     the environment, so any definitions you made in the REPL can
;;     still be referenced in future expressions, even though there's
;;     no workspace entry for them!

;;   * ,(save-workspace filename)

;;     Saves the current workspace as a Scheme source program into the
;;     file named by `filename'.

;;   * ,(load-workspace filename)

;;     Loads the Scheme program named by `filename' into the
;;     workspace, and also evaluates each definition or expression in
;;     the file.

;;   * ,(workspace-defs-only)

;;     Further forms entered into the REPL will only be committed to
;;     the workspace if they are definitions. (Currently, a
;;     definition, for Glass Table's purposes, is any pair form whose
;;     car begins with `define'. So `define', `define-macro',
;;     `define-syntax', `define-record-type', etc. forms all apply.)
;;     This is the default when you start GT.

;;   * ,(workspace-defs-and-exprs)

;;     Any further form entered into the REPL will be committed to the
;;     workspace.

;;   * ,(edit sym)

;;     Start an editor (by default $EDITOR, or 'vi' if not set) and
;;     lets the user edit the workspace definiton of `sym`. When the
;;     editor is saved and closed, the edited definition replaces the
;;     old one in the workspace and is evaled. It won't work if the
;;     saved file is not a valid definition form.

;; Because I wrote a rudimentary REPL for GT for the time being, the
;; Gambit REPL commands don't work inside the GT REPL. I want to
;; change this!

;; FUTURE WORK

;;   * Being smarter about what counts as a define and what it defines.
;;     (A macro that expands to a bunch of `define' forms should be a
;;     definition form!)

;;   * Other schemes. Maybe other programming languages.

(include "~~/lib/_repl#.scm")

(define gt#def-table (make-table size: 500 init: #f))

(define (gt#cmd-table channel old-read)
		      (list->table
		       (list
			(cons 'new-workspace
			      (lambda (cmd) (gt#new-workspace)))
			(cons 'eval-workspace
			      (lambda (cmd) (gt#eval-workspace)))
			(cons 'quit
			      (lambda (cmd)
				(macro-repl-channel-read-command-set!
				 channel old-read)))
			(cons 'save-workspace
			      (lambda (cmd) (if (not (and (pair? (cdr cmd)) (string? (cadr cmd))))
					     (error "file name for save-workspace must be a string")
					     (gt#save-workspace (cadr cmd)))))
			(cons 'load-workspace
			      (lambda (cmd) (if (not (and (pair? (cdr cmd)) (string? (cadr cmd))))
					     (error "file name for load-workspace must be a string")
					     (gt#load-workspace (cadr cmd)))))
			(cons 'workspace-defs-only
			      (lambda (cmd) (set! gt#remembers-expressions #f)))
			(cons 'workspace-defs-and-exprs
			      (lambda (cmd) (set! gt#remembers-expressions #t)))
			(cons 'workspace-eval-dynamic
			      (lambda (cmd) (set! gt#always-evals-workspace #t)))
			(cons 'edit
			      (lambda (cmd) (if (not (and (pair? (cdr cmd)) (symbol? (cadr cmd))))
					     (error "must name a symbol to edit")
					     (gt#edit (cadr cmd)))))
			(cons 'workspace-eval-explicit
			      (lambda (cmd) (set! gt#always-evals-workspace #f))))))

(define (gt#command? form tbl)
  (and (pair? form)
       (pair? (cdr form))
       (eq? (car form) 'unquote)
       (pair? (cadr form))
       (table-ref tbl (caadr form))))

(define gt#workspace '())

(define gt#remembers-expressions #f)

(define gt#always-evals-workspace #f)

(define (gt#definition? form)
  (and
   (list? form)
   (not (null? form))
   (symbol? (car form))
   (let ((s (symbol->string (car form))))
     (and (>= (string-length s) 6)
	  (string=? (substring s 0 6) "define")))))

(define (gt#defined-identifier form)
  (if (and (list? form)
	   (not (null? form))
	   (not (null? (cdr form))))
      (if (pair? (cadr form))
	  (car (cadr form))
	  (cadr form))
      '()))

(define (gt#add-to-workspace! form)
  (if (gt#definition? form)
      (let ((i (gt#defined-identifier form)))
	(if (not (table-ref gt#def-table i))
	    (begin
	      (set! gt#workspace (cons form gt#workspace))
	      (table-set! gt#def-table i gt#workspace))

	    (set-car! (table-ref gt#def-table i) form)))
        (if (or gt#remembers-expressions (gt#definition? form))
	    (set! gt#workspace (cons form gt#workspace)))))

(define (gt#save-workspace fn)
  (let ((p (open-output-file fn)))
    (with-exception-catcher
     (lambda (x) (close-port p))
     (lambda ()
       (for-each
	(lambda (x)
	  (pretty-print x p)
	  (newline p))
	(reverse gt#workspace))
       (close-port p)))))

(define (gt#new-workspace)
  (set! gt#workspace '())
  (set! gt#def-table (make-table size: 500 init: #f)))

(define (gt#eval-workspace)
  (eval (cons 'begin (reverse gt#workspace))))

(define (gt#load-workspace fn)
  (let ((p (open-input-file fn)))
    (with-exception-catcher
     (lambda (x) (close-port p))
     (lambda ()
       (let loop ((item (read p)))
	 (cond ((eof-object? item)
		(gt#eval-workspace)
		(close-port p))
	       (else
		(gt#add-to-workspace! item)
		(loop (read p)))))))))


(define (gt#editor)
  (getenv "EDITOR" "vi"))

(define (gt#edit1 obj)
  (let* ((temp-file (string-append "/tmp/gt-"
				   (number->string (time->seconds (current-time))))))
    (with-exception-catcher
     (lambda (x) (if (file-exists? temp-file) (delete-file temp-file)) (raise x))
     (lambda ()
       (let ((p (open-output-file temp-file)))
	 (with-exception-catcher
	  (lambda (y) (close-output-port p) (raise y))
	  (lambda ()
	    (write obj p)
	    (close-output-port p))))
       (shell-command (string-append (gt#editor) " " temp-file))
       (let ((p (open-input-file temp-file)))
	 (with-exception-catcher
	  (lambda (y) (close-input-port p) (raise y))
	  (lambda () (let ((r (read p))) (delete-file temp-file) r))))))))

(define (gt#edit name)
  (let ((slot (table-ref gt#def-table name)))
    (if slot
	(let ((replacement (gt#edit1 (car slot))))
	  (gt#add-to-workspace! replacement)
	  (if gt#always-evals-workspace
	      (gt#eval-workspace)
	      (eval replacement)))
	(error "symbol not in workspace: " name))))

(define (gt#prompt) "gt> ")

(define (gt#gt-wrap x)
  (lambda (channel level depth)
    (let ((cmd-table (gt#cmd-table channel x)))
      (begin
	(display "gt|")
	(let* ((src (x channel level depth))
	       (usrc (##desourcify src)))
	  (if (gt#command? usrc cmd-table)
	      (begin
		((table-ref cmd-table (caadr usrc)) (cadr usrc))
		(##make-source #!void (##source-locat src)))
	      (begin
		(if (or
		     gt#remembers-expressions
		     (gt#definition? usrc))
		    (gt#add-to-workspace! usrc))
		src)))))))

(define (gt#repl #!optional (channel (##thread-repl-channel-get! (current-thread))))
  (let ((new-read (gt#gt-wrap (macro-repl-channel-read-command channel))))
    (macro-repl-channel-read-command-set! channel new-read)))


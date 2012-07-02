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

;; Because I wrote a rudimentary REPL for GT for the time being, the
;; Gambit REPL commands don't work inside the GT REPL. I want to
;; change this!

;; FUTURE WORK

;;   * An `edit' command that pops open your $EDITOR with a single
;;     definition and commits the new definition back to the workspace

;;   * Integration with Gambit's powerful REPL, and enabling the use
;;     of Gambit's REPL commands inside GT

;;   * Being smarter about what counts as a define and what it defines.
;;     (A macro that expands to a bunch of `define' forms should be a
;;     definition form!)

;;   * Other schemes. Maybe other programming languages. (How does GT
;;     for Python sound?)

(define gt#def-table (make-table size: 500 init: #f))

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

(define (gt#prompt) "gt> ")

(define (gt#repl #!optional (in-port (repl-input-port)) (out-port (repl-output-port)))
  (call-with-current-continuation
   (lambda (k1)
     (let loop ()
       (display (gt#prompt) out-port)
       (force-output out-port)
       (let ((form (read in-port)))
	 (##continuation-capture
	  (lambda (k2)
	    (with-exception-catcher
	     (lambda (e)
	       (##display-exception-in-context
		e k2 out-port))
	     (lambda () 
	       (cond
		((eof-object? form) (k1 #!void))
		((and (pair? form)
		      (eq? (car form) 'unquote))
		 (gt#do-command (cadr form) k1))
		(else (let ((result (eval form)))
			(if (not (eq? result #!void)) (begin (pretty-print result)))
			(gt#add-to-workspace! form)))))))))
       (loop)))))

(define (gt#do-command cmd gt-exit)
  (if (pair? cmd)
      (cond
       ((eq? (car cmd) 'new-workspace)
	(gt#new-workspace))
       ((eq? (car cmd) 'eval-workspace)
	(gt#eval-workspace))
       ((eq? (car cmd) 'quit)
	(gt-exit #!void))
       ((eq? (car cmd) 'save-workspace)
	(if (not (and (pair? (cdr cmd)) (string? (cadr cmd))))
	    (error "file name for save-workspace must be a string")
	    (gt#save-workspace (cadr cmd))))
       ((eq? (car cmd) 'load-workspace)
	(if (not (and (pair? (cdr cmd)) (string? (cadr cmd))))
	    (error "file name for load-workspace must be a string")
	    (gt#load-workspace (cadr cmd))))
       ((eq? (car cmd) 'workspace-defs-only)
	(set! gt#remembers-expressions #f))
       ((eq? (car cmd) 'workspace-defs-and-exprs)
	(set! gt#remembers-expressions #t)))))
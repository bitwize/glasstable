(define gt#def-table (make-table size: 500 init: #f))

(define gt#workspace '())

(define gt#remembers-expressions #f)

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
  (let loop ()
    (display (gt#prompt) out-port)
    (force-output out-port)
    (let ((form (read in-port)))
      (##continuation-capture
       (lambda (k)
	 (with-exception-catcher
	  (lambda (e)
	    (##display-exception-in-context
	     e k out-port))
	  (lambda () 
	    (if (and (pair? form)
		     (eq? (car form) 'unquote))
		(gt#do-command (cadr form))
		(let ((result (eval form)))
		  (if (not (eq? result #!void)) (begin (pretty-print result)))
		  (gt#add-to-workspace! form)))))))
      (loop))))

(define (gt#do-command cmd)
  (if (pair? cmd)
      (cond
       ((eq? (car cmd) 'new-workspace)
	(gt#new-workspace))
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
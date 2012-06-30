(define gt#def-table (make-table size: 500 init: #f))

(define gt#workspace '())

(define gt#remembers-commands #f)

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
        (if (or gt#remembers-commands (gt#definition? form))
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
      (if (and (pair? form)
	       (eq? (car form) 'unquote))
	  (gt#do-directive (cdr form))
	  (##continuation-capture
	   (lambda (k)
	     (with-exception-catcher
	      (lambda (e)
		(##display-exception-in-context
		 e k out-port))
	      (lambda () (let ((result (eval form)))
			   (if (not (eq? result #!void)) (begin (pretty-print result))))
		      (gt#add-to-workspace! form))))))
      (loop))))
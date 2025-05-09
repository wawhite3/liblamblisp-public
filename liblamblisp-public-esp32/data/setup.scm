(syslog "LambLisp has control\n")

(define LambLisp?   (eqv? (if #f #t) #f))
(define TinyScheme? (eqv? (if #f #t) '()))
(define ChezScheme? (eqv? (if #f #t) (printf "")))

(define (caddr args) (car (cddr args)))
(define (cdddr args) (cdr (cddr args)))

(load "/Terminal.scm" 0)
(load "/rxrs.scm" 0)
(load "/Timers.scm" 0)
(load "/Led.scm" 0)

(define uptime_ms
  (let ((t (Stopwatch_ms)))
    (lambda () (t))
    )
  )

(define (is-idle idle_threshold) (< (lamb-loop-elapsed-ms) idle_threshold))

(define (make-idler idle-threshold interval)
  (let ((t (Stopwatch_ms))
	)
    (lambda ()
      (let ((tf (and (>= (t) interval) (is-idle idle-threshold)))
	    )
	(if tf (set! t (Stopwatch_ms)))
	tf
	)
      )
    )
  )

(define idler (make-idler 3 10000))

(define (keys->alist keys)
  (letrec ((construct-alist
	    (lambda (keys alist)
	      (if (null? keys) alist
		  (let ((pair (cons (car keys) nil))
			)
		    (construct->alist (cdr keys) (cons pair alist))
		    )
		  )
	      )
	    )
	   )
    (construct-alist keys nil)
    )
  )

;;;;;;;;
;;;
;;;(loop) start here
;;;
;;;;;;;;
(define loop
  (let ((me "(loop)")
	(t_loop (Stopwatch_ms))
	(hist (make-vector 100 0))
	)
    (lambda ()
      (let ((elapsed (t_loop)))
	(set! t_loop (Stopwatch_ms))
	
	(pattern_led)
	
	(if (>=i elapsed 100) (set! elapsed 99))
	(vector-set! hist elapsed (+i 1 (vector-ref hist elapsed)))
        (if (idler) (news "~a (loop-stats ~a)\n" me (vector->sparsevec hist 0)))
	)
      )
    )
  )

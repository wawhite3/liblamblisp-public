(syslog "Loading Timers\n")

;;;Stopwatch returns a procedure.  When called, the procedure returns the time (in ms) between the creation of the Stopwatch and the call.
#|
Usage:
(define elapsed (Stopwatch))
(code)
(more code)
(syslog "That took ~a time units\n" (elapsed))
|#

(define (Stopwatch_ms)
  (let ((t_start (millis)))
    (lambda ()
      (-i (millis) t_start))
    )
  )

(define (Stopwatch_us)
  (let ((t_start (micros)))
    (lambda ()
      (-i (micros) t_start))
    )
  )

;;;KitchenTimer accepts a duration in milliseconds, returns a countdown procedure.
;;;The counter automatically restarts and returns #f when expired, otherwise returns the remaining time.
;;;The procedure is created in the "expired" state.  To start it running, call it immediately, as in (let ((t (KitchenTimer_ms 1000))) (until (t) 'not-running))
;;;When the specified interval is 0, the timer will always return #f.
;;;
(define (KitchenTimer_ms intv)
  (let* ((me "(KitchenTimer_ms)")
	(interval (floor intv))
	(t_end (+i interval (millis)))	;not expired, created running
	)
    (lambda ()
      (let* ((now (millis))		;get current time
	     (t_rem (-i t_end now)))	;compute remaining time
	(if (positive? t_rem)		;still running?
	    t_rem			;yes, return remaining time
	    (begin			;not running, restart and return #f
	      (set! t_end (+i now interval))
	      #f
	      )
	    )
	)
      )
    )
  )

;;;Oneshot-tf returns a procedure that returns #t once and #f thereafter.
#|
Usage:
This example will print "oshot 1" but not "oshot 2".

(define oshot (Oneshot-tf))
(if (oshot) (syslog "oshot 1\n"))
(if (oshot) (syslog "oshot 2\n"))

|#
(define (Oneshot-tf)
  (let ((shot #f))
    (lambda ()
      (if shot #f
	  (begin (set! shot #t) #t)
	  )
      )
    )
  )

(define Oneshot Oneshot-tf)

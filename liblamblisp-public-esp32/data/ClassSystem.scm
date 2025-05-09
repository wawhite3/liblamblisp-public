;;;Class system for LambLisp

(define (make-struct . fields)
  (let ((res nil)
	(f (lambda (field res)
	     (cons item res)))
	)
    (if (null? fields) res (f ))
    )
  )

(define (hashtbl-lookup-symbol htbl symbol)
  (let* ((mask (-i 1 (hashtbl-length htbl)))
	 (index (& mask (symbol-hash symbol)))
	 (bucket (hashtbl-ref htbl index))
	 )
    (assq symbol bucket)
    )
  )

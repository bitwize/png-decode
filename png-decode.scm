(define (png-check-magic port)
  (let ((magic (read-u8vector-checked 8 port))
        (ref-magic (u8vector 137 80 78 71 13 10 26 10)))
    (if (not (equal? magic ref-magic)) (error "not a PNG file on " port))))

(define (png-fourcc->integer str)
  (let ((v (list->u8vector (map char->integer (string->list str)))))
    (png-be32-decode v)))

(define (png-integer->fourcc i)
  (string
   (integer->char (bitwise-and (arithmetic-shift i -24) 255))
   (integer->char (bitwise-and (arithmetic-shift i -16) 255))
   (integer->char (bitwise-and (arithmetic-shift i -8) 255))
   (integer->char (bitwise-and i 255))))
   

(define (png-be32-decode v)
  (bitwise-ior
   (arithmetic-shift (u8vector-ref v 0) 24)
   (arithmetic-shift (u8vector-ref v 1) 16)
   (arithmetic-shift (u8vector-ref v 2) 8)
   (u8vector-ref v 3)))

(define (png-read-be32 port)
  (let ((v (read-u8vector-checked 4 port))) (png-be32-decode v)))

(define-record-type
 png-chunk
 (make-png-chunk type data crc)
 png-chunk?
 (type png-chunk-type)
 (data png-chunk-data)
 (crc png-chunk-crc))

(define (read-u8vector-checked len port)
  (let* ((v (make-u8vector len)) (bytes (read-subu8vector v 0 len port)))
    (if (< bytes len) (error "unexpected EOF on " port))
    v))

(define (png-read-chunk port)
  (let* ((len (png-read-be32 port))
         (type (png-read-be32 port))
         (data (read-u8vector-checked len port))
         (crc (png-read-be32 port)))
    (make-png-chunk type data crc)))

(define (png-read-chunks port)
  (let loop ((l '()))
    (let ((c (png-read-chunk port)))
      (if (= (png-chunk-type c) (png-fourcc-to-integer "IEND"))
	(reverse (cons c l))
	(loop (cons c l))))))

(define (png-read port)
  (png-check-magic port)
  (png-read-chunks port))

(define *crc-table* (make-u32vector 256 0))

(define (fill-crc-table! poly)
  (do ((i 0 (+ i 1)))
      ((>= i 256))
    (u32vector-set!
     *crc-table*
     i
     (do ((bit 0 (+ bit 1))
          (v i
             (if (zero? (bitwise-and v 1))
                 (arithmetic-shift v -1)
                 (bitwise-xor poly (arithmetic-shift v -1)))))
         ((>= bit 8) v)))))

(fill-crc-table! #xedb88320)

(define (crc data #!optional (start 4294967295))
  (let ((len (u8vector-length data)))
    (do ((i 0 (+ i 1))
         (c start
            (bitwise-xor
             (u32vector-ref
              *crc-table*
              (bitwise-and (bitwise-xor c (u8vector-ref data i)) 255))
             (arithmetic-shift c -8))))
        ((>= i len) (bitwise-xor c 4294967295)))))


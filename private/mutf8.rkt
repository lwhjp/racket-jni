#lang racket/base

(require racket/match
         racket/port)

(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (bytes->string/modified-utf-8 bstr [err-char #f] [start 0] [end (bytes-length bstr)])
  (with-output-to-string
    (λ ()
      (let loop ([rest (bytes->list (subbytes bstr start end))]
                 [hi #f])
        (match rest
          [(list) (void)]
          [(list-rest x y z rest)
           (=> next)
           (unless (and (eqv? #xE0 (bitwise-and x #xF0))
                        (eqv? #x80 (bitwise-and y #xC0))
                        (eqv? #x80 (bitwise-and z #xC0)))
             (next))
           (cond
             [(and hi (eqv? #xED x) (eqv? #xB0 (bitwise-and y #xF0)))
              (write-char (integer->char (+ #x10000
                                            (arithmetic-shift hi 10)
                                            (arithmetic-shift (bitwise-and y #x0F) 6)
                                            (bitwise-and z #x3F))))
              (loop rest #f)]
             [(and (not hi) (eqv? #xED x) (eqv? #xA0 (bitwise-and y #xF0)))
              (loop rest (bitwise-ior (arithmetic-shift (bitwise-and y #x0F) 6)
                                      (bitwise-and z #x3F)))]
             [(and (not hi) (not (eqv? #xED x)) (not (eqv? #xA0 (bitwise-and y #xE0))))
              (write-char (integer->char
                           (bitwise-ior (arithmetic-shift (bitwise-and x #x0F) 12)
                                        (arithmetic-shift (bitwise-and y #x3F) 6)
                                        (bitwise-and z #x3F))))
              (loop rest #f)]
             [else (next)])]
          [(list-rest x y rest)
           #:when (and (not hi)
                       (eqv? #xC0 (bitwise-and x #xE0))
                       (eqv? #x80 (bitwise-and y #x80)))
           (write-char (integer->char
                        (bitwise-ior (arithmetic-shift (bitwise-and x #x0F) 6)
                                     (bitwise-and y #x3F))))
           (loop rest #f)]
          [(list-rest x rest)
           #:when (and (not hi)
                       (eqv? #x00 (bitwise-and x #x80)))
           (write-char (integer->char x))
           (loop rest #f)]
          [else
           (if err-char
               (write-char err-char)
               (error 'bytes->string/modified-utf-8 "invalid encoding"))])))))

(module+ test
  (check-equal? (bytes->string/modified-utf-8 (bytes)) "")
  (check-equal? (bytes->string/modified-utf-8 (bytes #xC0 #x80)) "\u0000")
  (check-equal? (bytes->string/modified-utf-8 (bytes #xED #xA0 #xB4 #xED #xB4 #x9E)) "\U1D11E")
  (check-equal? (bytes->string/modified-utf-8 #"hello") "hello")
  (check-equal? (bytes->string/modified-utf-8
                 (bytes #xE3 #x81 #x93 #xE3 #x82 #x93 #xE3 #x81 #xAB #xE3 #x81 #xA1 #xE3 #x81 #xAF))
                "こんにちは")
  (check-equal? (bytes->string/modified-utf-8
                 (bytes #x67 #x6C #xED #xA0 #xBC #xED #xBC #x90 #x62 #x65))
                "gl\U0001F310be"))

(define (string->bytes/modified-utf-8 str [start 0] [end (string-length str)])
  (with-output-to-bytes
    (λ ()
      (for ([chr (in-string str start end)])
        (define cp (char->integer chr))
        (cond
          [(<= #x0001 cp #x007F)
           (write-byte cp)]
          [(<= cp #x07FF) ; includes null
           (write-byte (bitwise-ior #xC0 (arithmetic-shift cp -6)))
           (write-byte (bitwise-ior #x80 (bitwise-and cp #x3F)))]
          [(<= cp #xFFFF)
           (write-byte (bitwise-ior #xE0 (arithmetic-shift cp -12)))
           (write-byte (bitwise-ior #x80 (bitwise-bit-field cp 6 12)))
           (write-byte (bitwise-ior #x80 (bitwise-and cp #x3F)))]
          [else
           (let ([z (- cp #x10000)])
             (write-byte #xED)
             (write-byte (bitwise-ior #xA0 (bitwise-bit-field z 16 20)))
             (write-byte (bitwise-ior #x80 (bitwise-bit-field z 10 16)))
             (write-byte #xED)
             (write-byte (bitwise-ior #xB0 (bitwise-bit-field z 6 10)))
             (write-byte (bitwise-ior #x80 (bitwise-and z #x3F))))])))))

(module+ test
  (check-equal? (string->bytes/modified-utf-8 "") (bytes))
  (check-equal? (string->bytes/modified-utf-8 "\u0000") (bytes #xC0 #x80))
  (check-equal? (string->bytes/modified-utf-8 "\U1D11E") (bytes #xED #xA0 #xB4 #xED #xB4 #x9E))
  (check-equal? (string->bytes/modified-utf-8 "hello") #"hello")
  (check-equal? (string->bytes/modified-utf-8 "こんにちは")
                (bytes #xE3 #x81 #x93 #xE3 #x82 #x93 #xE3 #x81 #xAB #xE3 #x81 #xA1 #xE3 #x81 #xAF))
  (check-equal? (string->bytes/modified-utf-8 "gl\U0001F310be")
                (bytes #x67 #x6C #xED #xA0 #xBC #xED #xBC #x90 #x62 #x65)))

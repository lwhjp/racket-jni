#lang racket/base

(provide (all-defined-out))

(struct exn:fail:jni exn:fail ())

(struct exn:fail:jni:error exn:fail:jni (code))

(struct exn:fail:jni:throw exn:fail:jni (object))

(define (raise-jni-error code)
  (raise
   (exn:fail:jni:error
    (case code
      [(-2) "thread detached from VM"]
      [(-3) "JNI version error"]
      [(-4) "not enough memory"]
      [(-5) "VM already created"]
      [(-6) "invalid arguments"]
      [else "unknown error"])
    (current-continuation-marks)
    code)))

(define (check-jni-return-code r)
  (unless (zero? r) (raise-jni-error r)))

(define-syntax-rule
  (return/check r e)
  (begin (check-jni-return-code r) e))

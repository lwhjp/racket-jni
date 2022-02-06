#lang racket/base

(require racket/class
         "jni.rkt")

(provide (except-out (all-defined-out)
                     local-jni-env))

(define local-jni-env (make-thread-cell #f #f))

(define current-jni-env
  (case-lambda
    [()
     (thread-cell-ref local-jni-env)]
    [(env)
     (unless (or (not env) (is-a? env JNIEnv<%>))
       (raise-argument-error 'current-jni-env "instance of JNIEnv<%> or #f" env))
     (thread-cell-set! local-jni-env env)]))

(define (require-jni-env)
  (or (current-jni-env)
      (error "attempted to use JNI on an unattached thread")))

(define (with-jni-env env thunk)
  (define old-env #f)
  (dynamic-wind
   (λ ()
     (set! old-env (current-jni-env))
     (current-jni-env env))
   thunk
   (λ () (current-jni-env old-env))))

(define-syntax-rule
  (let-jni-env env body0 body ...)
  (with-jni-env env (λ () body0 body ...)))

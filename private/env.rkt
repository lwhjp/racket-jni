#lang racket/base

(require racket/class
         ffi/unsafe
         "jni.rkt")

(provide (all-defined-out))

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

; scope

(struct jni-scope (refs))

(define (make-jni-scope)
  (jni-scope (make-ephemeron-hasheq)))

(define (jni-scope-register scope v proc)
  (hash-set! (jni-scope-refs scope) v proc))

(define (exit-jni-scope scope)
  (for ([proc (in-hash-values (jni-scope-refs scope))])
    (proc)))

(define global-jni-scope (make-jni-scope))

(define reference-cleanup-executor (make-will-executor))

(define local-jni-scope
  (make-thread-cell global-jni-scope #f))

(define (current-jni-scope)
  (or (thread-cell-ref local-jni-scope)
      (error "used outside of JNI scope")))

(define (with-jni-scope thunk)
  (define old-scope #f)
  (dynamic-wind
   (λ ()
     (set! old-scope (thread-cell-ref local-jni-scope))
     (thread-cell-set! local-jni-scope (make-jni-scope)))
   thunk
   (λ ()
     (exit-jni-scope (thread-cell-ref local-jni-scope))
     (thread-cell-set! local-jni-scope old-scope)
     (will-try-execute reference-cleanup-executor (void)))))

(define (with-jni-frame capacity thunk)
  (define-values (old-ref new-ptr)
    (with-jni-scope
     (λ ()
       (send (require-jni-env) PushLocalFrame capacity)
       (define result (thunk))
       (values
        result
        (send (require-jni-env) PopLocalFrame (and result (send result get-pointer)))))))
  (and new-ptr (send old-ref clone-with-new-pointer new-ptr)))

; references

(define reference<%>
  (interface ()
    ->weak-reference
    ->local-reference
    ->global-reference
    delete
    get-pointer
    clone-with-new-pointer))

(define reference%
  (class* object% (reference<%>)
    (init-field _type)
    (init pointer)
    (super-new)
    (define ptr (box pointer))
    (define (make-ref ref% p)
      (and p
           (new ref%
                [_type _type]
                [pointer (cast p _type)])))
    (define/public (->weak-reference)
      (make-ref weak-reference%
                (send (require-jni-env) NewWeakGlobalRef (get-pointer))))
    (define/public (->local-reference)
      (make-ref local-reference%
                (send (require-jni-env) NewLocalRef (get-pointer))))
    (define/public (->global-reference)
      (make-ref global-reference%
                (send (require-jni-env) NewGlobalRef (get-pointer))))
    (define/public (get-pointer)
      (or (unbox ptr)
          (error "invalid reference")))
    (define/public (clone-with-new-pointer p)
      (new this%
           [_type _type]
           [pointer p]))
    (define/public (clear)
      (set-box! ptr #f))
    (define/pubment (delete)
      (when (unbox ptr)
        (inner (void) delete)
        (clear)))))

(define weak-reference%
  (class* reference% ()
    (super-new)
    (define/override (->weak-reference) this)
    (define/augment (delete)
      (send (require-jni-env) DeleteWeakGlobalRef (super get-pointer)))
    (define/override (get-pointer)
      (error "access through weak reference"))))

(define local-reference%
  (class* reference% ()
    (inherit get-pointer)
    (super-new)
    (jni-scope-register (current-jni-scope) this (λ () (send this clear)))
    (define/override (->local-reference) this)
    (define/augment (delete)
      (send (require-jni-env) DeleteLocalRef (get-pointer)))))

(define global-reference%
  (class* reference% ()
    (inherit get-pointer)
    (super-new)
    (will-register reference-cleanup-executor this (λ () (send this delete)))
    (define/override (->global-reference) this)
    (define/augment (delete)
      (send (require-jni-env) DeleteGlobalRef (get-pointer)))))

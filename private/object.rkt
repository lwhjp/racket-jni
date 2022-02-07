#lang racket/base

(require racket/class
         "env.rkt"
         "types.rkt")

(provide (all-defined-out))

(define jobject%
  (class* object% (reference<%>)
    (init-field ref)
    (super-new)
    ; FIXME: should we use mixins or surrogates instead?
    (define/public (->weak-reference)
      (new this% [ref (send ref ->weak-reference)]))
    (define/public (->local-reference)
      (new this% [ref (send ref ->local-reference)]))
    (define/public (->global-reference)
      (new this% [ref (send ref ->global-reference)]))
    (define/public (delete)
      (send ref delete))
    (define/public (get-pointer)
      (send ref get-pointer))
    (define/public (clone-with-new-pointer p)
      (new this% [ref (send ref clone-with-new-pointer p)]))

    (define/public (get-class)
      (new jclass%
           [ref (new local-reference%
                     [_type _jclass]
                     [pointer (send (require-jni-env) GetObjectClass (get-pointer))])]))
    (define/public (get-ref-type)
      (send (require-jni-env) GetObjectRefType (get-pointer)))
    (define/public (instance-of? clazz)
      (send (require-jni-env) IsInstanceOf (get-pointer) (send clazz get-pointer)))
    (define/public (same-object? obj)
      (send (require-jni-env) IsSameObject (get-pointer) (send obj get-pointer)))
    ; TODO: get/set field
    ; TODO: call method
    ))

(define jclass%
  (class jobject%
    (inherit get-pointer)
    (super-new)
    (define/public (get-superclass)
      (define ptr (send (require-jni-env) GetSuperclass (get-pointer)))
      (and ptr
           (new jclass%
                [ref (new local-reference%
                          [_type _jclass]
                          [pointer ptr])])))
    (define/public (assignable-from? cls)
      (send (require-jni-env) IsAssignableFrom (send cls get-pointer) (get-pointer)))
    ; TODO: static fields, methods
    ))

(define jstring%
  (class jobject%
    (inherit get-pointer)
    (super-new)
    (define/public (get-length)
      (send (require-jni-env) GetStringLength (get-pointer)))
    ; TODO: get chars; get region
    ))

; TODO: arrays
; TODO: throwable

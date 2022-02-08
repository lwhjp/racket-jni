#lang racket/base

(require racket/class
         racket/match
         racket/string
         "env.rkt"
         "signature.rkt"
         "types.rkt")

(provide field-id?
         method-id?
         jobject%
         alloc-jobject
         new-jobject
         jclass%
         define-jclass
         find-jclass
         jstring%)

(define (signature-return-type sig)
  (match sig
    [`(method ,_ ,r) r]
    [_ sig]))

(define (object-signature? sig)
  (match (signature-return-type sig)
    [`(array ,_) #t]
    [`(object ,_) #t]
    [_ #f]))

(define (signature->method-name sig template)
  (define t (signature-return-type sig))
  (string->symbol
   (string-replace
    template
    "_T_"
    (if (symbol? t)
        (string-titlecase
         (symbol->string t))
        "Object"))))

(define (wrap/sig sig v)
  ; TODO: arrays
  (if (object-signature? sig)
      (wrap-object/local v)
      v))

(define (wrap/local c% _t p)
  (and p
       (new c%
            [ref (new local-reference%
                      [_type _t]
                      [pointer p])])))

(define (wrap-object/local ptr)
  (wrap/local jobject% _jobject ptr))

(define (wrap-class/local ptr)
  (wrap/local jclass% _jclass ptr))

(struct field-id (ptr sig))
(struct method-id (ptr sig))

(define (unwrap-args args)
  (map (λ (v)
         (if (is-a? v jobject%)
             (send v get-pointer)
             v))
       args))

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
      (wrap-class/local (send (require-jni-env) GetObjectClass (get-pointer))))
    (define/public (get-ref-type)
      (send (require-jni-env) GetObjectRefType (get-pointer)))
    (define/public (instance-of? clazz)
      (send (require-jni-env) IsInstanceOf (get-pointer) (send clazz get-pointer)))
    (define/public (same-object? obj)
      (send (require-jni-env) IsSameObject (get-pointer) (send obj get-pointer)))
    (define/public (get-field-id name sig)
      (field-id (send (require-jni-env) GetFieldID (get-pointer) name sig)
                (parse-signature sig)))
    (define/public (get-field f)
      (wrap/sig
       (field-id-sig f)
       (dynamic-send (require-jni-env)
                     (signature->method-name (field-id-sig f) "Get_T_Field")
                     (get-pointer)
                     (field-id-ptr f))))
    (define/public (set-field! f v)
      (dynamic-send (require-jni-env)
                    (signature->method-name (field-id-sig f) "Set_T_Field")
                    (get-pointer)
                    (field-id-ptr f)
                    (if (object-signature? (field-id-sig f))
                        (send v get-pointer)
                        v)))
    (define/public (get-method-id name sig)
      (method-id (send (require-jni-env) GetMethodID (get-pointer) name sig)
                 (parse-signature sig)))
    (define/public (call-method m . args)
      (wrap/sig
       (method-id-sig m)
       (dynamic-send (require-jni-env)
                     (signature->method-name (method-id-sig m) "Call_T_MethodA")
                     (get-pointer)
                     (method-id-ptr m)
                     (unwrap-args args))))
    (define/public (call-nonvirtual-method m c . args)
      (wrap/sig
       (method-id-sig m)
       (dynamic-send (require-jni-env)
                     (signature->method-name (method-id-sig m) "CallNonvirtual_T_MethodA")
                     (get-pointer)
                     (send c get-pointer)
                     (method-id-ptr m)
                     (unwrap-args args))))))

(define (alloc-jobject clazz)
  (wrap-object/local (send (require-jni-env) AllocObject (send clazz get-pointer))))

(define (new-jobject clazz ctor . args)
  (wrap-object/local
   (send (require-jni-env)
         NewObjectA
         (send clazz get-pointer)
         (method-id-ptr ctor)
         (unwrap-args args))))

(define jclass%
  (class jobject%
    (inherit get-pointer)
    (super-new)
    (define/public (get-superclass)
      (wrap-class/local (send (require-jni-env) GetSuperclass (get-pointer))))
    (define/public (assignable-from? cls)
      (send (require-jni-env) IsAssignableFrom (send cls get-pointer) (get-pointer)))
    (define/public (get-static-field-id name sig)
      (field-id (send (require-jni-env) GetFieldID (get-pointer) name sig)
                (parse-signature sig)))
    (define/public (get-static-field f)
      (wrap/sig
       (field-id-sig f)
       (dynamic-send (require-jni-env)
                     (signature->method-name (field-id-sig f) "GetStatic_T_Field")
                     (get-pointer)
                     (field-id-ptr f))))
    (define/public (set-static-field! f v)
      (dynamic-send (require-jni-env)
                    (signature->method-name (field-id-sig f) "SetStatic_T_Field")
                    (get-pointer)
                    (field-id-ptr f)
                    (if (object-signature? (field-id-sig f))
                        (send v get-pointer)
                        v)))
    (define/public (get-static-method-id name sig)
      (method-id (send (require-jni-env) GetStaticMethodID (get-pointer) name sig)
                 (parse-signature sig)))
    (define/public (call-method m . args)
      (wrap/sig
       (method-id-sig m)
       (dynamic-send (require-jni-env)
                     (signature->method-name (method-id-sig m) "CallStatic_T_MethodA")
                     (get-pointer)
                     (method-id-ptr m)
                     (unwrap-args args))))))

(define (define-jclass name buf [loader #f])
  (wrap-class/local
   (send (require-jni-env)
         DefineClass
         name
         (and loader (send loader get-pointer))
         buf)))

(define (find-jclass name)
  (wrap-class/local (send (require-jni-env) FindClass name)))

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

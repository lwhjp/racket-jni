#lang racket/base

(require racket/class
         racket/match
         racket/string
         ffi/unsafe
         "env.rkt"
         "error.rkt"
         "signature.rkt"
         "types.rkt")

(provide field-id?
         method-id?
         jobject%
         jni-alloc-object
         jni-new-object
         jclass%
         jni-define-class
         jni-find-class
         jstring%
         jni-new-string
         jarray%
         jarray/object%
         jni-new-object-array
         jarray/primitive%
         jarray/boolean%
         jarray/byte%
         jarray/char%
         jarray/short%
         jarray/int%
         jarray/long%
         jarray/float%
         jarray/double%
         jni-new-primitive-array
         jthrowable%
         jni-throw
         jni-throw/new
         jni-exception-pending?
         jni-get-exception
         jni-describe-exception
         jni-clear-exception!
         jni-fatal-error!
         current-jni-dump-exceptions?
         jni-check-exception
         jobject-cast)

(define (signature-return-type sig)
  (match sig
    [`(method ,_ ,r) r]
    [_ sig]))

(define (array-signature? sig)
  (match (signature-return-type sig)
    [`(array ,_) #t]
    [_ #f]))

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

(define (array-types type)
  (case type
      [(boolean) (values jarray/boolean% _jbooleanArray)]
      [(byte) (values jarray/byte% _jbyteArray)]
      [(char) (values jarray/char% _jcharArray)]
      [(short) (values jarray/short% _jshortArray)]
      [(int) (values jarray/int% _jintArray)]
      [(long) (values jarray/long% _jlongArray)]
      [(float) (values jarray/float% _jfloatArray)]
      [(double) (values jarray/double% _jdoubleArray)]
      [else (raise-argument-error 'jni-new-primitive-array "valid type" type)]))

(define (wrap/sig sig v)
  (define r (signature-return-type sig))
  (match r
    [`(array ,_) (wrap-array/local r v)]
    [`(object "java.lang.String") (wrap-string/local (cast v _jobject _jstring))]
    [`(object ,_) (wrap-object/local v)]
    [else v]))

(define (wrap/local c% _t p)
  (and p
       (new c%
            [ref (new local-reference%
                      [_type _t]
                      [pointer p])])))

(define (wrap-object/local ptr)
  (wrap/local jobject% _jobject ptr))

(define (wrap-string/local ptr)
  (wrap/local jstring% _jstring ptr))

(define (wrap-class/local ptr)
  (wrap/local jclass% _jclass ptr))

(define (wrap-array/local sig ptr)
  (match-define `(array ,t) sig)
  (and ptr
       (if (symbol? t)
           (let-values ([(array% _array) (array-types t)])
             (new array%
                  [ref (new local-reference%
                            [_type _array]
                            [pointer ptr])]))
           (wrap/local jarray/object% _jobjectArray ptr))))

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
      (field-id (return/throw (send (require-jni-env) GetFieldID (get-pointer) name sig))
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
      (method-id (return/throw (send (require-jni-env) GetMethodID (get-pointer) name sig))
                 (parse-signature sig)))
    (define/public (call-method m . args)
      (wrap/sig
       (method-id-sig m)
       (return/throw
        (dynamic-send (require-jni-env)
                      (signature->method-name (method-id-sig m) "Call_T_MethodA")
                      (get-pointer)
                      (method-id-ptr m)
                      (unwrap-args args)))))
    (define/public (call-nonvirtual-method m c . args)
      (wrap/sig
       (method-id-sig m)
       (return/throw
        (dynamic-send (require-jni-env)
                      (signature->method-name (method-id-sig m) "CallNonvirtual_T_MethodA")
                      (get-pointer)
                      (send c get-pointer)
                      (method-id-ptr m)
                      (unwrap-args args)))))
    (define/public (monitor-enter)
      (send (require-jni-env) MonitorEnter (get-pointer)))
    (define/public (monitor-exit)
      (send (require-jni-env) MonitorExit (get-pointer)))))

(define (jni-alloc-object class)
  (wrap-object/local
   (return/throw
    (send (require-jni-env) AllocObject (send class get-pointer)))))

(define (jni-new-object class ctor . args)
  (wrap-object/local
   (return/throw
    (send (require-jni-env)
          NewObjectA
          (send class get-pointer)
          (method-id-ptr ctor)
          (unwrap-args args)))))

(define jclass%
  (class jobject%
    (inherit get-pointer)
    (super-new)
    (define/public (get-superclass)
      (wrap-class/local (send (require-jni-env) GetSuperclass (get-pointer))))
    (define/public (assignable-from? cls)
      (send (require-jni-env) IsAssignableFrom (send cls get-pointer) (get-pointer)))
    (define/public (get-static-field-id name sig)
      (field-id (return/throw (send (require-jni-env) GetFieldID (get-pointer) name sig))
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
      (method-id (return/throw (send (require-jni-env) GetStaticMethodID (get-pointer) name sig))
                 (parse-signature sig)))
    (define/public (call-static-method m . args)
      (wrap/sig
       (method-id-sig m)
       (return/throw
        (dynamic-send (require-jni-env)
                      (signature->method-name (method-id-sig m) "CallStatic_T_MethodA")
                      (get-pointer)
                      (method-id-ptr m)
                      (unwrap-args args)))))))

(define (jni-define-class name buf [loader #f])
  (wrap-class/local
   (return/throw
    (send (require-jni-env)
          DefineClass
          name
          (and loader (send loader get-pointer))
          buf))))

(define (jni-find-class name)
  (wrap-class/local
   (return/throw
    (send (require-jni-env) FindClass name))))

(define jstring%
  (class jobject%
    (inherit get-pointer)
    (super-new)
    (define/public (get-length)
      (send (require-jni-env) GetStringLength (get-pointer)))
    (define/public (get-chars)
      (define env (require-jni-env))
      (let-values ([(p copy?) (send env GetStringUTFChars (get-pointer))])
        (begin0
          (cast p _pointer _string/modified-utf-8)
          (send env ReleaseStringUTFChars (get-pointer) p))))
    (define/public (get-region start len)
      (send (require-jni-env)
            GetStringUTFRegion
            (get-pointer)
            start
            len))))

(define (jni-new-string str)
  (wrap/local jstring%
              _jstring
              (return/throw (send (require-jni-env) NewStringUTF str))))

(define jarray%
  (class jobject%
    (inherit get-pointer)
    (super-new)
    (define/public (get-length)
      (send (require-jni-env) GetArrayLength (get-pointer)))))

(define jarray/object%
  (class jarray%
    (inherit get-pointer)
    (super-new)
    (define/public (get-element index)
      (wrap-object/local
       (send (require-jni-env) GetObjectArrayElement (get-pointer) index)))
    (define/public (set-element! index value)
      (send (require-jni-env) SetObjectArrayElement (get-pointer) index (send value get-pointer)))))

(define (jni-new-object-array length element-class [initial-element #f])
  (wrap/local jarray/object%
              _jobjectArray
              (return/throw
               (send (require-jni-env)
                     NewObjectArray
                     length
                     (send element-class get-pointer)
                     (and initial-element (send initial-element get-pointer))))))

(define jarray/primitive%
  (class jarray%
    (init-field _element)
    (inherit get-pointer)
    (super-new)
    (define (call-with-elements/critical mode proc)
      (define p
        (return/throw
         (let-values ([(p copy?) (send (require-jni-env) GetPrimitiveArrayCritical (get-pointer))])
           p)))
      (begin0
        (proc p)
        (send (require-jni-env) ReleasePrimitiveArrayCritical (get-pointer) p mode)))
    (define/public (copy-to-vector! start end dest [dest-start 0])
      (call-with-elements/critical JNI_ABORT
        (λ (p)
          (for ([i (in-range (- end start))])
            (vector-set! dest (+ dest-start i) (ptr-ref p _element (+ start i)))))))
    (define/public (copy-from-vector! start end src [src-start 0])
      (call-with-elements/critical JNI_COMMIT
        (λ (p)
          (for ([i (in-range (- end start))])
            (ptr-set! p _element (+ start i) (vector-ref src (+ src-start i)))))))
    (define/public (get-region start end)
      (let ([v (make-vector (- end start) #f)])
        (copy-to-vector! start end v)
        v))))

(define-values (jarray/boolean% jarray/byte% jarray/char% jarray/short%
                jarray/int% jarray/long% jarray/float% jarray/double%)
  (apply
   values
   (map (λ (_element)
          (class jarray/primitive%
            (super-new [_element _element])))
        (list _jboolean _jbyte _jchar _jshort
              _jint _jlong _jfloat _jdouble))))

(define (jni-new-primitive-array length type)
  (wrap-array/local
   type
   (return/throw
    (dynamic-send (require-jni-env)
                  (signature->method-name type "New_T_Array")
                  length))))

(define jthrowable%
  (class jobject%
    (inherit get-pointer)
    (super-new)))

(define (jni-throw t)
  (send (require-jni-env) Throw (send t get-pointer)))

(define (jni-throw/new jclass [message #f])
  (send (require-jni-env) ThrowNew (send jclass get-pointer) message))

(define (jni-exception-pending?)
  (send (require-jni-env) ExceptionCheck))

(define (jni-get-exception)
  (wrap/local jthrowable%
              _jthrowable
              (send (require-jni-env) ExceptionOccurred)))

(define (jni-describe-exception)
  (send (require-jni-env) ExceptionDescribe))

(define (jni-clear-exception!)
  (send (require-jni-env) ExceptionClear))

(define (jni-fatal-error! [msg #f])
  (send (require-jni-env) FatalError msg))

(define current-jni-dump-exceptions?
  (make-parameter #f))

(define (jni-check-exception)
  (let ([t (jni-get-exception)])
    (when t
      (when (current-jni-dump-exceptions?)
        (jni-describe-exception))
      (raise (exn:fail:jni:throw "Java exception"
                                 (current-continuation-marks)
                                 t)))))

(define (return/throw v)
  (jni-check-exception)
  v)

; cast

(define jobject-type-map
  (hash
   jobject% _jobject
   jclass% _jclass
   jstring% _jstring
   jarray/object% _jobjectArray
   jarray/boolean% _jbooleanArray
   jarray/byte% _jbyteArray
   jarray/char% _jcharArray
   jarray/short% _jshortArray
   jarray/int% _jintArray
   jarray/long% _jlongArray
   jarray/float% _jfloatArray
   jarray/double% _jdoubleArray
   jthrowable% _jthrowable))

(define (jobject-cast obj type%)
  (define old-ref (get-field ref obj))
  (define ptr (get-field pointer old-ref))
  (define new-type (hash-ref jobject-type-map type%))
  (define new-ref (send old-ref clone-with-pointer (cast ptr _pointer new-type)))
  (set-field! _type new-ref new-type)
  (new type% [ref new-ref]))

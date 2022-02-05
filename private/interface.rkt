#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/string
                     racket/syntax
                     syntax/parse)
         racket/class
         racket/splicing
         racket/stxparam
         ffi/unsafe
         "types.rkt")

(provide (all-defined-out))

(define-syntax-parameter _this-pointer #f)

(define-syntax define-jni-interface
  (syntax-parser
    [(_ id<%>:id
        #:interface-type _if
        #:reserved reserved:nat
        ([method-id:id method-type] ...))
     (define id-base
       (let ([s (symbol->string (syntax-e #'id<%>))])
         (unless (string-suffix? s "<%>")
           (raise-syntax-error #f "expected: identifier ending in <%>" this-syntax #'id<%>))
         (substring s 0 (- (string-length s) 3))))
     (define if-base
       (let ([s (symbol->string (syntax-e #'_if))])
         (unless (string-prefix? s "_")
           (raise-syntax-error #f "expected: identifier starting with _" this-syntax #'_if))
         (substring s 1)))
     (with-syntax* ([(id _id _id-pointer id-pointer? id%)
                     (map (λ (fmt) (format-id #'id<%> fmt id-base))
                          '("~a" "_~a" "_~a-pointer" "~a-pointer?" "~a%"))]
                    [_if-pointer (format-id #'_if "_~a-pointer" if-base)]
                    [(if-method ...)
                     (map (λ (id) (format-id #'_if "~a-~a" if-base (syntax-e id)))
                          (syntax->list #'(method-id ...)))]
                    [(reserved-id ...) (build-list (syntax-e #'reserved) (λ (id) (gensym 'reserved)))])
       #'(splicing-syntax-parameterize ([_this-pointer (make-rename-transformer #'_id-pointer)])
           (define-cpointer-type _id-pointer #:tag 'id)
           (define-cpointer-type _id _id-pointer
             (λ (obj) (get-field this-pointer obj))
             (λ (ptr) (new id% [this-pointer ptr])))
           (define-cstruct _if
             ([reserved-id _pointer] ...
              [method-id method-type] ...))
           (define id<%> (interface () method-id ...))
           (define id%
             (class* object% (id<%>)
               (init-field this-pointer)
               (unless (id-pointer? this-pointer)
                 (raise-argument-error 'id% (symbol->string 'id-pointer?) this-pointer))
               (super-new)
               (define/public (method-id . args)
                 (apply (if-method (ptr-ref this-pointer _if-pointer))
                        this-pointer
                        args)) ...))))]))

(define-syntax-parameter _T #f)
(define-syntax-parameter _Tarray #f)
(define-syntax-parameter _Tarray/null #f)

(define-syntax (define-jni-interface* stx)
  (define-splicing-syntax-class clause
    #:attributes ([expanded 1])
    (pattern
     (~seq #:for-types (types:id) ([method-id:id method-type] ...))
     #:attr [expanded 1]
     (append*
      (for/list ([t (in-string (symbol->string (syntax-e #'types)))])
        (define-values (type-name _type _typeArray _typeArray/null)
          (case t
            [(#\V) (values "Void" #'_void #f #f)]
            [(#\L) (values "Object" #'_jobject/null #'_jobjectArray #'_jobjectArray/null)]
            [(#\Z) (values "Boolean" #'_jboolean #'_jbooleanArray #'_jbooleanArray/null)]
            [(#\B) (values "Byte" #'_jbyte #'_jbyteArray #'_jbyteArray/null)]
            [(#\C) (values "Char" #'_jchar #'_jcharArray #'_jcharArray/null)]
            [(#\S) (values "Short" #'_jshort #'_jshortArray #'_jshortArray/null)]
            [(#\I) (values "Int" #'_jint #'_jintArray #'_jintArray/null)]
            [(#\J) (values "Long" #'_jlong #'_jlongArray #'_jlongArray/null)]
            [(#\F) (values "Float" #'_jfloat #'_jfloatArray #'_jfloatArray/null)]
            [(#\D) (values "Double" #'_jdouble #'_jdoubleArray #'_jdoubleArray/null)]
            [else (raise-syntax-error #f
                                      (format "invalid type specifier: ~e" t)
                                      this-syntax
                                      #'types)]))
        (with-syntax ([(method-id* ...)
                       (map (λ (id)
                              (datum->syntax
                               id
                               (string->symbol
                                (string-replace
                                 (symbol->string (syntax-e id))
                                 "_T_"
                                 type-name))))
                            (syntax->list #'(method-id ...)))])
          (syntax->list
           #`([method-id*
               (syntax-parameterize
                   ([_T (make-rename-transformer #'#,_type)]
                    [_Tarray #,(and _typeArray #`(make-rename-transformer #'#,_typeArray))]
                    [_Tarray/null #,(and _typeArray/null #`(make-rename-transformer #'#,_typeArray/null))])
                 method-type)]
              ...))))))
    (pattern
     (~seq any)
     #:attr [expanded 1]
     (list #'any)))
  (syntax-parse stx
    [(_ id<%>:id
        #:interface-type _if
        #:reserved reserved:nat
        (c:clause ...))
     #'(define-jni-interface id<%>
         #:interface-type _if
         #:reserved reserved
         (c.expanded ... ...))]))

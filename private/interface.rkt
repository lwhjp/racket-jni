#lang racket/base

(require (for-syntax racket/base
                     racket/string
                     racket/syntax
                     syntax/parse)
         racket/class
         racket/splicing
         racket/stxparam
         ffi/unsafe)

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

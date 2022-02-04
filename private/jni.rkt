#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/class
         ffi/unsafe
         ffi/unsafe/define
         "env.rkt"
         "error.rkt"
         "types.rkt"
         "vm.rkt")

(provide (all-defined-out))

(define-cstruct _JavaVMOption
  ([optionString _string/locale]
   [extraInfo (_or-null _pointer)]))

(define-cstruct _JavaVMInitArgs
  ([version _jint]
   [nOptions _jint]
   [options _JavaVMOption-pointer/null]
   [ignoreUnrecognized _jboolean]))

(define JNI%
  (class object%
    (super-new)    
    (define jni-lib
      (ffi-lib
       "libjvm"
       #f
       #:get-lib-dirs
       (λ () (list "/usr/lib/jvm/default-java/lib/server"))))
    (define-ffi-definer define-jni jni-lib)
    (define-syntax (define-jni/public stx)
      (syntax-case stx ()
        [(_ id type)
         (with-syntax ([JNI_id (format-id #'id "JNI_~a" (syntax-e #'id))])
           #'(begin
               (define-jni JNI_id type)
               (define/public (id . args) (apply JNI_id args))))]))
    (define-jni/public GetDefaultJavaVMInitArgs
      (_fun (version) ::
            [vm_args : (_ptr io _JavaVMInitArgs)
                     = (make-JavaVMInitArgs version 0 #f #f)]
            -> [r : _jint]
            -> (return/check r vm_args)))
    (define-jni/public GetCreatedJavaVMs
      (_fun ([buffer-size 10]) ::
            [vmBuf : _pointer = (malloc (_array _pointer buffer-size) 'atomic)]
            [bufLen : _jsize = buffer-size]
            [nVMs : (_ptr o _jsize)]
            -> [r : _jint]
            -> (return/check r (build-list nVMs (λ (i) (ptr-ref vmBuf _JavaVM i))))))
    (define-jni/public CreateJavaVM
      (_fun (vm : (_ptr o _JavaVM/null))
            (env : (_ptr o _JNIEnv/null))
            _JavaVMInitArgs-pointer
            -> (r : _jint)
            -> (return/check r (values vm env))))))

; hacky function for testing
(define (get-jni-env [version JNI_VERSION_10])
  (define jni (new JNI%))
  (define vms (send jni GetCreatedJavaVMs 1))
  (if (null? vms)
      (let ([args (send jni GetDefaultJavaVMInitArgs version)])
        (let-values ([(vm env) (send jni CreateJavaVM args)])
          env))
      (send (car vms) GetEnv version)))

#lang racket/base

(require racket/list
         ffi/unsafe)

(provide (all-defined-out))

; primitive types
(define _jboolean (make-ctype _uint8 (λ (v) (if v 1 0)) (λ (v) (not (eqv? 0 v)))))
(define _jbyte _sint8)
(define _jchar _uint16)
(define _jshort _sint16)
(define _jint _sint32)
(define _jlong _sint64)
(define _jfloat _float)
(define _jdouble _double)
(define _jsize _jint)

; reference types
(define-cpointer-type _jobject)
(define-cpointer-type _jclass _jobject)
(define-cpointer-type _jstring _jobject)
(define-cpointer-type _jarray _jobject)
(define-cpointer-type _jobjectArray _jarray)
(define-cpointer-type _jbooleanArray _jarray)
(define-cpointer-type _jbyteArray _jarray)
(define-cpointer-type _jcharArray _jarray)
(define-cpointer-type _jshortArray _jarray)
(define-cpointer-type _jintArray _jarray)
(define-cpointer-type _jlongArray _jarray)
(define-cpointer-type _jfloatArray _jarray)
(define-cpointer-type _jdoubleArray _jarray)
(define-cpointer-type _jthrowable _jobject)

; ids
(define-cpointer-type _jfieldID)
(define-cpointer-type _jmethodID)

; value
; TODO: define-cunion
#;(define-cunion _jvalue
  ([z _jboolean]
   [b _jbyte]
   [c _jchar]
   [s _jshort]
   [i _jint]
   [j _jlong]
   [f _jfloat]
   [d _jdouble]
   [l _jobject]))
(define _jvalue
  (_union _jboolean _jbyte _jchar _jshort _jint
          _jlong _jfloat _jdouble _jobject))

(define (jvalue-ref jv which)
  (union-ref
   jv
   (or (index-of '(z b c s i j f d l) which)
       (error 'jvalue-ref "invalid member specifier: ~a" which))))

(define (jvalue-set! jv which e)
  (union-set!
   jv
   (or (index-of '(z b c s i j f d l) which)
       (error 'jvalue-ref "invalid member specifier: ~a" which))
   e))

; version codes
(define JNI_VERSION_1_1 #x00010001)
(define JNI_VERSION_1_2 #x00010002)
(define JNI_VERSION_1_4 #x00010004)
(define JNI_VERSION_1_6 #x00010006)
(define JNI_VERSION_1_8 #x00010008)
(define JNI_VERSION_9   #x00090000)
(define JNI_VERSION_10  #x000a0000)

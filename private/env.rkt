#lang racket/base

(require ffi/unsafe
         "error.rkt"
         "interface.rkt"
         "types.rkt")

(provide (all-defined-out))

(define-jni-interface JNIEnv<%>
  #:interface-type _JNINativeInterface
  #:reserved 4
  ([GetVersion (_fun _this-pointer -> _jint)]))

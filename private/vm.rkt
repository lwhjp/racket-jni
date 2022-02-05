#lang racket/base

(require ffi/unsafe
         "env.rkt"
         "error.rkt"
         "interface.rkt"
         "types.rkt")

(provide (all-defined-out))

(define-cstruct _JavaVMAttachArgs
  ([version _jint]
   [name _string/modified-utf-8]
   [group _jobject/null]))

(define-jni-interface JavaVM<%>
  #:interface-type _JNIInvokeInterface
  #:reserved 3
  ([DestroyJavaVM
    (_fun _this-pointer
          -> [r : _jint]
          -> (return/check r (void)))]
   [AttachCurrentThread
    (_fun _this-pointer
          [env : (_ptr o _JNIEnv/null)]
          [thr_args : _JavaVMAttachArgs-pointer/null]
          -> [r : _jint]
          -> (return/check r env))]
   [DetachCurrentThread
    (_fun _this-pointer
          -> [r : _jint]
          -> (return/check r (void)))]
   [GetEnv
    (_fun _this-pointer
          [env : (_ptr o _JNIEnv/null)]
          [version : _jint]
          -> [r : _jint]
          -> (return/check r env))]
   [AttachCurrentThreadAsDaemon
    (_fun _this-pointer
          [env : (_ptr o _JNIEnv/null)]
          [args : _JavaVMAttachArgs-pointer/null]
          -> [r : _jint]
          -> (return/check r env))]))

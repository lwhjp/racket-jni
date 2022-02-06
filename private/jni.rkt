#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/class
         ffi/unsafe
         ffi/unsafe/define
         "error.rkt"
         "interface.rkt"
         "types.rkt")

(provide (all-defined-out))

;;
;; JNI library
;;

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

;;
;; Java VM invokation
;;

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

;;
;; Native interface
;;

(define-cstruct _JNINativeMethod
  ([name _string/modified-utf-8]
   [signature _string/modified-utf-8]
   [fnPtr _fpointer]))

(define-jni-interface* JNIEnv<%>
  #:interface-type _JNINativeInterface
  #:reserved 4
  ([GetVersion
    (_fun _this-pointer -> _jint)]
   [DefineClass
    (_fun _this-pointer
          [name : _string/modified-utf-8]
          [loader : _jobject/null]
          [buf : _bytes]
          [bufLen : _jsize = (bytes-length buf)]
          -> _jclass/null)]
   [FindClass
    (_fun _this-pointer
          [name : _string/modified-utf-8]
          -> _jclass/null)]
   [FromReflectedMethod
    (_fun _this-pointer
          [method : _jobject]
          -> _jmethodID/null)]
   [FromReflectedField
    (_fun _this-pointer
          [field : _jobject]
          -> _jfieldID/null)]
   [ToReflectedMethod
    (_fun _this-pointer
          [cls : _jclass]
          [methodID : _jmethodID]
          [isStatic : _jboolean]
          -> _jobject/null)]
   [GetSuperclass
    (_fun _this-pointer
          [clazz : _jclass]
          -> _jclass/null)]
   [IsAssignableFrom
    (_fun _this-pointer
          [clazz1 : _jclass]
          [clazz2 : _jclass]
          -> _jboolean)]
   [ToReflectedField
    (_fun _this-pointer
          [cls : _jclass]
          [fieldID : _jfieldID]
          [isStatic : _jboolean]
          -> _jobject/null)]
   [Throw
    (_fun _this-pointer
          [obj : _jthrowable]
          -> [r : _jint]
          -> (return/check r (void)))]
   [ThrowNew
    (_fun _this-pointer
          [clazz : _jclass]
          [message : _string/modified-utf-8]
          -> [r : _jint]
          -> (return/check r (void)))]
   [ExceptionOccurred
    (_fun _this-pointer
          -> _jthrowable/null)]
   [ExceptionDescribe
    (_fun _this-pointer
          -> _void)]
   [ExceptionClear
    (_fun _this-pointer
          -> _void)]
   [FatalError
    (_fun _this-pointer
          [msg : _string/modified-utf-8]
          -> _void)]
   [PushLocalFrame
    (_fun _this-pointer
          [capacity : _jint]
          -> [r : _jint]
          -> (return/check r (void)))]
   [PopLocalFrame
    (_fun _this-pointer
          [result : _jobject/null]
          -> _jobject/null)]
   [NewGlobalRef
    (_fun _this-pointer
          [ref : _jobject/null]
          -> _jobject/null)]
   [DeleteGlobalRef
    (_fun _this-pointer
          [globalRef : _jobject/null]
          -> _void)]
   [DeleteLocalRef
    (_fun _this-pointer
          [localRef : _jobject/null]
          -> _void)]
   [IsSameObject
    (_fun _this-pointer
          [ref1 : _jobject/null]
          [ref2 : _jobject/null]
          -> _jboolean)]
   [NewLocalRef
    (_fun _this-pointer
          [ref : _jobject/null]
          -> _jobject/null)]
   [EnsureLocalCapacity
    (_fun _this-pointer
          [capacity : _jint]
          -> [r : _jint]
          -> (return/check r (void)))]
   [AllocObject
    (_fun _this-pointer
          [clazz : _jclass]
          -> _jobject/null)]
   [NewObject
    (_fun #:varargs-after 3
          _this-pointer
          [clazz : _jclass]
          [methodID : _jmethodID]
          -> _jobject/null)]
   [NewObjectV _fpointer]
   [NewObjectA
    (_fun _this-pointer
          [clazz : _jclass]
          [methodID : _jmethodID]
          [args : (_list i _jvalue)]
          -> _jobject/null)]
   [GetObjectClass
    (_fun _this-pointer
          [obj : _jobject]
          -> _jclass)]
   [IsInstanceOf
    (_fun _this-pointer
          [obj : _jobject/null]
          [clazz : _jclass]
          -> _jboolean)]
   [GetMethodID
    (_fun _this-pointer
          [clazz : _jclass]
          [name : _string/modified-utf-8]
          [sig : _string/modified-utf-8]
          -> _jmethodID/null)]
   #:for-types (LZBCSIJFDV)
   ([Call_T_Method
     (_fun #:varargs-after 3
           _this-pointer
           [obj : _jobject]
           [methodID : _jmethodID]
           -> _T)]
    [Call_T_MethodV _fpointer]
    [Call_T_MethodA
     (_fun _this-pointer
           [obj : _jobject]
           [methodID : _jmethodID]
           [args : (_list i _jvalue)]
           -> _T)])
   #:for-types (LZBCSIJFDV)
   ([CallNonvirtual_T_Method
     (_fun #:varargs-after 4
           _this-pointer
           [obj : _jobject]
           [clazz : _jclass]
           [methodID : _jmethodID]
           -> _T)]
    [CallNonvirtual_T_MethodV _fpointer]
    [CallNonvirtual_T_MethodA
     (_fun _this-pointer
           [obj : _jobject]
           [clazz : _jclass]
           [methodID : _jmethodID]
           [args : (_list i _jvalue)]
           -> _T)])
   [GetFieldID
    (_fun _this-pointer
          [clazz : _jclass]
          [name : _string/modified-utf-8]
          [sig : _string/modified-utf-8]
          -> _jfieldID/null)]
   #:for-types (LZBCSIJFD)
   ([Get_T_Field
     (_fun _this-pointer
           [obj : _jobject]
           [fieldID : _jfieldID]
           -> _T)])
   #:for-types (LZBCSIJFD)
   ([Set_T_Field
     (_fun _this-pointer
           [obj : _jobject]
           [fieldID : _jfieldID]
           [value : _T]
           -> _void)])
   [GetStaticMethodID
    (_fun _this-pointer
          [clazz : _jclass]
          [name : _string/modified-utf-8]
          [sig : _string/modified-utf-8]
          -> _jmethodID/null)]
   #:for-types (LZBCSIJFDV)
   ([CallStatic_T_Method
     (_fun #:varargs-after 3
           _this-pointer
           [clazz : _jclass]
           [methodID : _jmethodID]
           -> _T)]
    [CallStatic_T_MethodV _fpointer]
    [CallStatic_T_MethodA
     (_fun _this-pointer
           [clazz : _jclass]
           [methodID : _jmethodID]
           [args : (_list i _jvalue)]
           -> _T)])
   [GetStaticFieldID
    (_fun _this-pointer
          [clazz : _jclass]
          [name : _string/modified-utf-8]
          [sig : _string/modified-utf-8]
          -> _jfieldID/null)]
   #:for-types (LZBCSIJFD)
   ([GetStatic_T_Field
     (_fun _this-pointer
           [clazz : _jclass]
           [fieldID : _jfieldID]
           -> _T)])
   #:for-types (LZBCSIJFD)
   ([SetStatic_T_Field
     (_fun _this-pointer
           [clazz : _jclass]
           [fieldID : _jfieldID]
           [value : _T]
           -> _void)])
   [NewString
    (_fun _this-pointer
          [unicodeChars : _string/utf-8]
          [len : _jsize = (string-length unicodeChars)]
          -> _jstring/null)]
   [GetStringLength
    (_fun _this-pointer
          [string : _jstring]
          -> _jsize)]
   [GetStringChars
    (_fun _this-pointer
          [string : _jstring]
          [isCopy : (_ptr o _jboolean)]
          -> [chars : _pointer]
          -> (values chars isCopy))]
   [ReleaseStringChars
    (_fun _this-pointer
          [string : _jstring]
          [chars : _pointer]
          -> _void)]
   [NewStringUTF
    (_fun _this-pointer
          [bytes : _string/modified-utf-8]
          -> _jstring/null)]
   [GetStringUTFLength
    (_fun _this-pointer
          [string : _jstring]
          -> _jsize)]
   [GetStringUTFChars
    (_fun _this-pointer
          [string : _jstring]
          [isCopy : (_ptr o _jboolean)]
          -> [utf : _pointer]
          -> (values utf isCopy))]
   [ReleaseStringUTFChars
    (_fun _this-pointer
          [string : _jstring]
          [utf : _pointer]
          -> _void)]
   [GetArrayLength
    (_fun _this-pointer
          [array : _jarray]
          -> _jsize)]
   [NewObjectArray
    (_fun _this-pointer
          [length : _jsize]
          [elementClass : _jclass]
          [initialElement : _jobject/null]
          -> _jobjectArray/null)]
   [GetObjectArrayElement
    (_fun _this-pointer
          [array : _jobjectArray]
          [index : _jsize]
          -> _jobject/null)]
   [SetObjectArrayElement
    (_fun _this-pointer
          [array : _jobjectArray]
          [index : _jsize]
          [value : _jobject/null]
          -> _void)]
   #:for-types (ZBCSIJFD)
   ([New_T_Array
     (_fun _this-pointer
           [length : _jsize]
           -> _Tarray/null)])
   #:for-types (ZBCSIJFD)
   ([Get_T_ArrayElements
     (_fun _this-pointer
           [array : _Tarray]
           [isCopy : (_ptr o _jboolean)]
           -> [elems : _pointer]
           -> (values elems isCopy))])
   #:for-types (ZBCSIJFD)
   ([Release_T_ArrayElements
     (_fun _this-pointer
           [array : _Tarray]
           [elems : _pointer]
           [mode : _jint]
           -> _void)])
   #:for-types (ZBCSIJFD)
   ([Get_T_ArrayRegion
     (_fun _this-pointer
           [array : _Tarray]
           [start : _jsize]
           [len : _jsize]
           [buf : _pointer]
           -> _void)])
   #:for-types (ZBCSIJFD)
   ([Set_T_ArrayRegion
     (_fun _this-pointer
           [array : _Tarray]
           [start : _jsize]
           [len : _jsize]
           [buf : _pointer]
           -> _void)])
   [RegisterNatives
    (_fun _this-pointer
          [clazz : _jclass]
          [methods : (_list i _JNINativeMethod)]
          [nMethods : _jint = (length methods)]
          -> [r : _jint]
          -> (return/check r (void)))]
   [UnregisterNatives
    (_fun _this-pointer
          [clazz : _jclass]
          -> [r : _jint]
          -> (return/check r (void)))]
   [MonitorEnter
    (_fun #:blocking? #t
          _this-pointer
          [obj : _jobject]
          -> [r : _jint]
          -> (return/check r (void)))]
   [MonitorExit
    (_fun _this-pointer
          [obj : _jobject]
          -> [r : _jint]
          -> (return/check r (void)))]
   [GetJavaVM
    (_fun _this-pointer
          [vm : (_ptr o _JavaVM)]
          -> [r : _jint]
          -> (return/check r vm))]
   [GetStringRegion
    (_fun _this-pointer
          [str : _jstring]
          [start : _jsize]
          [len : _jsize]
          [buf : _pointer]
          -> _void)]
   [GetStringUTFRegion
    (_fun _this-pointer
          [str : _jstring]
          [start : _jsize]
          [len : _jsize]
          [buf : _pointer]
          -> _void)]
   [GetPrimitiveArrayCritical
    (_fun _this-pointer
          [array : _jarray]
          [isCopy : (_ptr o _jboolean)]
          -> [carray : _pointer]
          -> (values carray isCopy))]
   [ReleasePrimitiveArrayCritical
    (_fun _this-pointer
          [array : _jarray]
          [carray : _pointer]
          [mode : _jint]
          -> _void)]
   [GetStringCritical
    (_fun _this-pointer
          [string : _jstring]
          [isCopy : (_ptr o _jboolean)]
          -> [cstring : _pointer]
          -> (values cstring isCopy))]
   [ReleaseStringCritical
    (_fun _this-pointer
          [string : _jstring]
          [carray : _pointer]
          -> _void)]
   [NewWeakGlobalRef
    (_fun _this-pointer
          [obj : _jobject/null]
          -> _jweak/null)]
   [DeleteWeakGlobalRef
    (_fun _this-pointer
          [obj : _jweak/null]
          -> _void)]
   [ExceptionCheck
    (_fun _this-pointer
          -> _jboolean)]
   [NewDirectByteBuffer
    (_fun _this-pointer
          [address : _pointer]
          [capacity : _jlong]
          -> _jobject/null)]
   [GetDirectBufferAddress
    (_fun _this-pointer
          [buf : _jobject]
          -> _pointer)]
   [GetDirectBufferCapacity
    (_fun _this-pointer
          [buf : _jobject]
          -> _jlong)]
   [GetObjectRefType
    (_fun _this-pointer
          [obj : _jobject]
          -> _jobjectRefType)]
   [GetModule
    (_fun _this-pointer
          [clazz : _jclass]
          -> _jobject)]))

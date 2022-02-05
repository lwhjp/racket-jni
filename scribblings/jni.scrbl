#lang scribble/manual
@title{Java Native Interface}

@(require (for-label racket/base
                     racket/class
                     racket/contract/base
                     (except-in ffi/unsafe ->)
                     jni))

@defmodule[jni]

@section{Low Level Interface}

These interfaces provide access to the raw C API with minimal wrapping.
Needless to say, it is easy to cause memory corruption and crashes if
used incorrectly.

@subsection{Types}

@defthing*[([_jboolean ctype?]
            [_jbyte ctype?]
            [_jchar ctype?]
            [_jshort ctype?]
            [_jint ctype?]
            [_jlong ctype?]
            [_jfloat ctype?]
            [_jdouble ctype?]
            [_jsize ctype?])]{
  JNI primitive types.
}

@defthing*[([_jobject ctype?]
            [_jclass ctype?]
            [_jstring ctype?]
            [_jarray ctype?]
            [_jthrowable ctype?])]{
  JNI reference types.

  These are defined as tagged pointer types using @racket[define-cpointer-type],
  so predicate functions and @tt{/null} variants are also available.
}

@defthing*[([_jobjectArray ctype?]
            [_jbooleanArray ctype?]
            [_jbyteArray ctype?]
            [_jcharArray ctype?]
            [_jshortArray ctype?]
            [_jintArray ctype?]
            [_jlongArray ctype?]
            [_jfloatArray ctype?]
            [_jdoubleArray ctype?])]{
  JNI array types.

  These all derive from @racket[_jarray].
}

@defthing[_jweak ctype?]{
  JNI weak reference, derived from @racket[_jobject].
}

@defthing[_jobjectRefType ctype?]{
  Enumeration of @racket['JNIInvalidRefType], @racket['JNILocalRefType], @racket['JNIGlobalRefType]
  and @racket['JNIWeakGlobalRefType].
}

@defthing*[([_jfieldID ctype?]
            [_jmethodID ctype?])]{
  Field and method IDs, represented as tagged pointers.
}

@defthing[_jvalue ctype?]{
  A union type.
}

@defthing[_string/modified-utf-8 ctype?]{
  A type for modified UTF-8 strings, similar to @racket[_string/utf-8].
}

@defthing*[([JNI_VERSION_1_1 exact-nonnegative-integer? #:value #x00010001]
            [JNI_VERSION_1_2 exact-nonnegative-integer? #:value #x00010002]
            [JNI_VERSION_1_4 exact-nonnegative-integer? #:value #x00010004]
            [JNI_VERSION_1_6 exact-nonnegative-integer? #:value #x00010006]
            [JNI_VERSION_1_8 exact-nonnegative-integer? #:value #x00010008]
            [JNI_VERSION_9 exact-nonnegative-integer? #:value #x00090000]
            [JNI_VERSION_10 exact-nonnegative-integer? #:value #x000a0000])]{
  JNI version constants.
}

@defthing*[([JNI_COMMIT exact-nonnegative-integer? #:value 1]
            [JNI_ABORT exact-nonnegative-integer? #:value 2])]{
  Modes for releasing array contents.
}

@subsection{Modified UTF-8}

@defproc[(bytes->string/modified-utf-8 [bstr bytes?]
                                       [err-char (or/c #f char?) #f]
                                       [start exact-nonnegative-integer? 0]
                                       [end exact-nonnegative-integer? (bytes-length bstr)])
         string?]{
  Interprets @racket[bstr] as modified UTF-8 and returns the resulting string.

  Invalid byte sequences will be decoded as @racket[err-char] if it is a character; an
  exception will be raised otherwise.
}

@defproc[(string->bytes/modified-utf-8 [str string?]
                                       [start exact-nonnegative-integer? 0]
                                       [end exact-nonnegative-integer? (string-length str)])
         bytes?]{
  Produces the modified UTF-8 encoding of @racket[str].
}

@subsection{JNI Library}

@defclass[JNI% object% ()]{
  A class for initializing the JNI library and instantiating a Java VM.
  See the JNI documentation for details.
  @defmethod[(GetDefaultJavaVMInitArgs [version exact-integer?]) JavaVMInitArgs?]
  @defmethod[(GetCreatedJavaVMs) (listof (is-a?/c JavaVM<%>))]
  @defmethod[(CreateJavaVM [args JavaVMInitArgs?]) (values (is-a?/c JavaVM<%>) (is-a?/c JNIEnv<%>))]
}

@defthing[_JavaVMInitArgs ctype?]{
  A structure used when initializing the JNI.
  @racketblock[
    (define-cstruct _JavaVMInitArgs
      ([version _jint]
       [nOptions _jint]
       [options _JavaVMOption-pointer/null]
       [ignoreUnrecognized _jboolean]))
  ]
}

@defthing[_JavaVMOption ctype?]{
  A structure used when initializing the JNI.
  @racketblock[
    (define-cstruct _JavaVMOption
      ([optionString _string/locale]
       [extraInfo (_or-null _pointer)]))
  ]
}

@defproc[(get-jni-env [version exact-integer? JNI_VERSION_10])
         (is-a?/c JNIEnv<%>)]{
  A utility function to get hold of a @racket[JNIEnv<%>], creating a Java VM if necessary.
  This is likely be removed in the future.
}

@subsection{Invocation interface}

@definterface[JavaVM<%> ()]{
  Methods for invoking and attaching to the Java VM, corresponding to the JNI C functions.
  @defmethod[(DestroyJavaVM) void?]
  @defmethod[(AttachCurrentThread [thr_args (or/c #f JavaVMAttachArgs?)]) (is-a?/c JNIEnv<%>)]
  @defmethod[(DetachCurrentThread) void?]
  @defmethod[(GetEnv [version exact-nonnegative-integer?]) (is-a?/c JNIEnv<%>)]
  @defmethod[(AttachCurrentThreadAsDaemon [args (or/c #f JavaVMAttachArgs?)]) (is-a?/c JNIEnv<%>)]
}

@defthing[_JavaVM ctype?]{
  A type equivalent to @tt{JavaVM *} in C. Produces an instance of @racket[JavaVM<%>] when
  converted to a Racket value.
}

@defthing[_JavaVMAttachArgs ctype?]{
  A structure used when attaching threads to the VM. Defined as:
  @racketblock[
    (define-cstruct _JavaVMAttachArgs
      ([version _jint]
       [name _string/modified-utf-8]
       [group _jobject/null]))
  ]
}

@subsection{Native interface}

@definterface[JNIEnv<%> ()]{
  Methods for interacting with the Java environment, corresponding to the JNI C functions.

  There are many functions here, so please refer to the source code and JNI documentation for details.
}

@defthing[_JNIEnv ctype?]{
  A type equivalent to @tt{JNIEnv *} in C. Produces an instance of @racket[JNIEnv<%>] when
  converted to a Racket value.
}

@defthing[_JNINativeMethod ctype?]{
  A structure used when registering native methods. Defined as:
  @racketblock[    
    (define-cstruct _JNINativeMethod
      ([name _string/modified-utf-8]
       [signature _string/modified-utf-8]
       [fnPtr _fpointer]))
  ]
}

@subsection{Errors}

Certain methods in @racket[JavaVM<%>] and @racket[JNIEnv<%>] signal an error in their return code.
In this case, the wrapper will raise an instance of @racket[exn:fail:jni]. Note that this does not
correspond to a Java exception---these need to be handled explicity.

@defstruct[(exn:fail:jni exn:fail) ([code exact-integer?])]{
  Represents an error return code from a JNI function.
 See the JNI documentation for the interpretation of @racket[code].
}

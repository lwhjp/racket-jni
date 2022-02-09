#lang scribble/manual
@title{Java Native Interface}

@(require (for-label racket/base
                     racket/class
                     racket/contract/base
                     jni
                     jni/unsafe))

@defmodule[jni]

This library provides interoperability with a Java Runtime Environment
via the C JNI interface. Although the wrappers provide a Racket-like interface,
it is possible for unexpected usage patterns to corrupt or crash the
Racket process. This whole library should be considered unsafe, and not used
with untrusted code.

In particular, there are likely to be bugs. Save your work before running.

@section{Getting Started}

This is a work-in-progress, and only suitable for experimentation at this point.
Stay tuned!

If you're familiar with JNI and the Racket FFI, try this to get started:

@codeblock|{
#lang racket

(require racket/class
         jni
         jni/unsafe)

(let-jni-env (get-jni-env)
  (define env (require-jni-env))
  (println (send env GetVersion))
  (with-jni-scope
    (λ ()
      (define sptr (send env NewString "hello, java"))
      (define s (new jstring% [ref (new local-reference% [_type _jstring] [pointer sptr])]))
      (println (send s get-length))
      (send s delete))))
}|

@section{High-level wrappers}

@subsection{Object wrappers}

@defproc[(field-id? [v any/c]) boolean?]{
  Predicate for (wrapped) JNI field IDs.
}

@defproc[(method-id? [v any/c]) boolean?]{
  Predicate for (wrapped) JNI method IDs.
}

@subsubsection{@tt{Object}}

@defclass[jobject% object% (reference<%>)]{
  Represents an instance of Java @tt{Object}. All other object wrappers inherit
  from this class.

  @defmethod[(get-class) (is-a?/c jclass%)]
  @defmethod[(get-ref-type) exact-integer?]
  @defmethod[(instance-of? [clazz (is-a?/c jclass%)]) boolean?]
  @defmethod[(same-object? [obj (is-a?/c jobject%)]) boolean?]
  @defmethod[(get-field-id [name string?] [sig string?]) field-id?]
  @defmethod[(get-field [f field-id?]) any/c]
  @defmethod[(set-field! [f field-id?] [v any/c]) void?]
  @defmethod[(get-method-id [name string?] [sig string?]) method-id?]
  @defmethod[(call-method [m method-id?] [arg any/c] ...) any/c]
  @defmethod[(call-nonvirtual-method [m method-id?] [c (is-a?/c jclass%)] [arg any/c] ...) any/c]
  @defmethod[(monitor-enter) void?]
  @defmethod[(monitor-exit) void?]
}

@defproc[(jni-alloc-object [class (is-a?/c jclass%)]) (is-a?/c jobject%)]{
  Allocates (but does not initialize) a Java object. You probably don't want to use this.
}

@defproc[(jni-new-object [class (is-a?/c jclass%)]
                         [ctor method-id?]
                         [arg any/c] ...)
         (is-a?/c jobject%)]{
  Creates a new Java object of class @racket[class], calling the specified constructor
  @racket[ctor].
}

@subsubsection{@tt{Class}}

@defclass[jclass% jobject% ()]{
  Represents a Java class.

  @defmethod[(get-superclass) (or/c (is-a?/c jclass%) #f)]{
    Returns the superclass of this class, or @racket[#f] if this class is
    @tt{Object} or an interface.
  }
  @defmethod[(assignable-from? [cls (is-a?/c jclass%)]) boolean?]{
    Returns @racket[#t] if @racket[cls] is a subclass of (or the same as) this class.
  }
  @defmethod[(get-static-field-id [name string?] [sig string?]) field-id?]
  @defmethod[(get-static-field [f field-id?]) any/c]
  @defmethod[(set-static-field! [f field-id?] [v any/c]) void?]
  @defmethod[(get-static-method-id [name string?] [sig string?]) method-id?]
  @defmethod[(call-static-method [m method-id?] [arg any/c] ...) any/c]
}

@defproc[(jni-define-class [name string?]
                           [buf bytes?]
                           [loader (or/c (is-a?/c jobject%) #f) #f])
         (is-a?/c jclass%)]{
  Defines a new Java class from bytecode.
}

@defproc[(jni-find-class [name string?]) (is-a?/c jclass%)]{
  Find an existing Java class by name or throws an exception if not found.
}

@subsubsection{@tt{String}}

@defclass[jstring% jobject% ()]{
  Represents a Java string.

  @defmethod[(get-length) exact-nonnegative-integer?]
  @defmethod[(get-chars) string?]
  @defmethod[(get-region [start exact-nonnegative-integer?]
                         [len exact-positive-integer?])
             string?]
}

@defproc[(jni-new-string [str string?]) (is-a?/c jstring%)]{
  Creates a new Java string.
}

@subsubsection{Exceptions (@tt{Throwable})}

@defclass[jthrowable% jobject% ()]{
  Represents an instance of a Java @tt{Throwable}.
}

@defproc[(jni-throw [t (is-a?/c jthrowable%)]) void?]{
  Throws @racket[t] as a Java exception. Note that this does @emph{not} result
  in a Racket exception: you should return normally without calling any other
  JNI functions to allow propagation to continue on the Java side.
}

@defproc[(jni-throw/new [jclass (is-a?/c jclass%)]
                          [message (or/c string? #f) #f])
           void?]{
  Throws a new instance of @racket[jclass] (which must be a subclass of @tt{Throwable}).
  See @racket[jni-throw] for semantics.
}

@defproc[(jni-exception-thrown?) boolean?]{
  Check whether a Java exception is pending.
}

@defproc[(jni-get-exception) (or/c (is-a?/c jthrowable%) #f)]{
  Returns the pending exception, if any, or @racket[#f] if all is well.
}

@defproc[(jni-describe-exception) void?]{
  Causes the JVM to dump debug information about the pending exception to stderr.
}

@defproc[(jni-clear-exception!) void?]{
  Clear any pending Java exception.
}

@defproc[(jni-fatal-error! [msg (or/c string? #f) #f]) void?]{
  Causes the JVM to exit immediately, taking your Racket process with it.
}

@defproc[(jni-check-exception) void?]{
  Checks whether a Java exception is pending, and throws an instance of
  @racket[exn:fail:jni:throw] if so.
}

@subsection{References}
@definterface[reference<%> ()]{
  This interface represents a reference to a Java object.

  @defmethod[(->weak-reference) (is-a?/c reference<%>)]{
    Returns a new weak reference for the same object. Weak references may
    not be dereferenced: convert to a local or global reference first.
  }
  @defmethod[(->local-reference) (or/c (is-a?/c reference<%>) #f)]{
    Returns a new local reference for the same object. If the current reference
    is a weak reference which has already been collected, this method returns @racket[#f].
  }
  @defmethod[(->global-reference) (or/c (is-a?/c reference<%>) #f)]{
    Returns a new global reference for the same object. If the current reference
    is a weak reference which has already been collected, this method returns @racket[#f].
  }
  @defmethod[(delete) void?]{
    Releases the reference. It is an error to use the reference after calling this method.
  }
  @defmethod[(get-pointer) jobject?]{
    Returns a pointer to the referenced object. Best not to use this unless you know what you're doing.
  }
}

@section{Errors}

Certain JNI methods signal an error in their return code. If the error is due to a thrown Java
exception, the wrapper will raise an instance of @racket[exn:fail:jni:throw]. In other cases, a
numeric error code is returned and the wrapper raises @racket[exn:fail:jni:error].

Not all JNI functions check for exceptions. You can call @racket[jni-check-exception] to explicitly
check and raise a Racket-side exception if a Java exception is pending.

Java exceptions remain "active" until they are cancelled. If you catch and handle an instance of
@racket[exn:fail:jni:throw], you should also call @racket[jni-clear-exception!] to prevent the
exception from propagating when control returns to the Java side.

@defstruct[(exn:fail:jni exn:fail) ()]{
  Supertype for all JNI errors.
}

@defstruct[(exn:fail:jni:error exn:fail:jni) ([code exact-integer?])]{
  Represents an error return code from a JNI function.
  See the JNI documentation for the interpretation of @racket[code].
}

@defstruct[(exn:fail:jni:throw exn:fail:jni) ([object (is-a?/c jthrowable%)])]{
  Represents a thrown Java exception. The exception will be propagated back to the Java side
  unless @racket[jni-clear-exception!] is called.
}

@section{Attaching to the JVM}

These functions are dangerous, and may change or be removed.

@defproc*[([(current-jni-env) (or/c (is-a?/c JNIEnv<%>) #f)]
           [(current-jni-env [env (or/c (is-a?/c JNIEnv<%>) #f)]) void?])]{
  Get or set the current thread-local JNI environment interface.
}

@defproc[(require-jni-env) (is-a?/c JNIEnv<%>)]{
  Same as @racket[(current-jni-env)], but raises an error if the value is @racket[#f].
}

@defproc[(with-jni-env [env (is-a?/c JNIEnv<%>)] [thunk (-> any)]) any]{
  Evaluates @racket[(thunk)] with @racket[(current-jni-env)] set to return @racket[env]
  during the dynamic scope of @racket[thunk].
}

@defform[(let-jni-env env body ...+)
         #:contracts ([env (is-a?/c JNIEnv<%>)])]{
  Convenience wrapper for @racket[with-jni-env].
}

@defproc[(with-jni-scope [thunk (-> any)]) any]{
  Wraps a JNI scope: local references associated with the scope will be marked as invalid
  when @racket[thunk] exits.
}

@defproc[(with-jni-frame [thunk (-> (or/c (is-a? reference<%>) #f))]) (or/c (is-a? reference<%>) #f)]{
  Creates a new local frame. You can return a local reference from @racket[thunk], which will
  be converted to a local reference in the parent frame.
}

@section{Low Level Interface}

@defmodule[jni/unsafe]
@(require (for-label (except-in ffi/unsafe ->)))

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

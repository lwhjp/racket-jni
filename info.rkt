#lang info
(define collection "jni")
(define deps '("base"))
(define build-deps '("racket-doc" "rackunit-lib" "scribble-lib"))
(define scribblings '(("scribblings/jni.scrbl" () (interop))))

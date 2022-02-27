#lang info
(define collection "jni")
(define deps '("base" "parser-tools-lib"))
(define build-deps '("racket-doc" "rackunit-lib" "scribble-lib"))
(define scribblings '(("scribblings/jni.scrbl" () (interop))))
(define test-omit-paths '("example/"))

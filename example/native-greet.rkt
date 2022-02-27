#lang racket
(require jni)

;; This is our native method which will be called from Java
(define (get-name obj)
  (jni-new-string "Racket"))

;; Set up the JVM
(current-jni-dump-exceptions? #t)
(define env (get-jni-env))
(current-jni-env env)

;; Register the method
(define greeter-class (jni-find-class "example/NativeGreet"))
(send greeter-class
      register-natives!
      (list (list "getName" "()Ljava/lang/String;" get-name)))

;; Create an instance of the Java object
(define greeter-ctor (send greeter-class get-method-id "<init>" "()V"))
(define greeter (jni-new-object greeter-class greeter-ctor))

;; Have it call our native method
(define greet-mid (send greeter-class get-method-id "greet" "()Ljava/lang/String;"))
(define greeting (send greeter call-method greet-mid))
(printf "Java said: ~s\n" (send greeting get-chars))

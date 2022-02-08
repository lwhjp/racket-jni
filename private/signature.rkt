#lang racket/base

(require racket/port
         racket/string
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide parse-signature)

(define-tokens type-tokens (PRIMITIVE CLASS))
(define-empty-tokens type-delimiters (ARRAY LPAREN RPAREN END))

(define signature-lexer
  (lexer
   [(eof) (token-END)]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\[ (token-ARRAY)]
   [(char-set "BCDFIJSVZ")
    (token-PRIMITIVE (string-ref lexeme 0))]
   [(:: #\L (:* (:~ #\;)) #\;)
    (token-CLASS (substring lexeme 1 (sub1 (string-length lexeme))))]))

(define signature-parser
  (parser
   (tokens type-tokens type-delimiters)
   (start type)
   (end END)
   (error (λ (ok? name value)
            (error "invalid type descriptor")))
   (grammar
    [type
     [(LPAREN method-args type)
      (list 'method $2 $3)]
     [(ARRAY type)
      (list 'array $2)]
     [(PRIMITIVE)
      (case $1
        [(#\B) 'byte]
        [(#\C) 'char]
        [(#\D) 'double]
        [(#\F) 'float]
        [(#\I) 'int]
        [(#\J) 'long]
        [(#\S) 'short]
        [(#\V) 'void]
        [(#\Z) 'boolean])]
     [(CLASS)
      (list 'object (string-replace $1 "/" "."))]]
    [method-args
     [(RPAREN) '()]
     [(type method-args) (cons $1 $2)]])))

(define (parse-signature str)
  (call-with-input-string str
    (λ (in)
      (signature-parser
       (λ () (signature-lexer in))))))

(module+ test
  (require rackunit)
  (check-equal? (parse-signature "I")
                'int)
  (check-equal? (parse-signature "Ljava/lang/String;")
                '(object "java.lang.String"))
  (check-equal? (parse-signature "[[Z")
                '(array (array boolean)))
  (check-equal? (parse-signature "(Ljava/lang/String;J)[Ljava/util/List;")
                '(method ((object "java.lang.String") long)
                         (array (object "java.util.List")))))

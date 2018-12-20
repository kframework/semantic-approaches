#lang racket

; The reader and tokenizer
; See the grammar in imp-parser.rkt

(require "imp-parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module imp-module "imp-expander.rkt" ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)

(require br-parser-tools/lex)
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define imp-lexer
      (lexer
       [(union "int" "if" "else" "while" "<=" "&&" "++" "read" "print" "halt" "join" "spawn") lexeme]
       [(char-set ",();{}=+/!") lexeme]
       ["false" (token 'BOOL #f)]
       ["true" (token 'BOOL #t)]
       [(repetition 1 +inf.0 alphabetic) (token 'ID lexeme)]
       [(repetition 1 +inf.0 numeric) (token 'INT (string->number lexeme))]
       [whitespace (next-token)]))
    (imp-lexer port))
  next-token)


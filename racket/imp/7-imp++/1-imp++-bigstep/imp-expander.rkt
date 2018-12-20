#lang racket

(require racket/match)
(require "imp-sigma.rkt")
(require "imp-stmt-expander.rkt")
(require "imp-expr-expander.rkt")

; Top level expander
(define-syntax-rule (#%module-begin PARSE-TREE)
  (#%plain-module-begin (begin (display 'PARSE-TREE)
                               (display #\newline)
                               (display-sigma (PARSE-TREE (make-sigma (list 0 1 2 3 4 5 6 7 8 9 10))))
                               )))

(define prog
  (lambda ast (match ast
                [(list s) (lambda (S) (s S))])))

(provide #%module-begin
         #%datum
         #%app
         prog
         id_list
         stmt
         aexp
         block
         bexp
         seq
)
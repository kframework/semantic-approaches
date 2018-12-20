#lang racket

(require racket/match)
(require "imp-sigma.rkt")

; Conditionally executes an expression without-halt with the configuration S
; if S is in a halting state, do nothing and return that same S
; otherwise, runs without-halt with the configuration S
(define expr-with-halt
  (lambda (without-halt)
    (lambda (S)
      (if (halt? S) (values (void) S) (without-halt S)))))

(define-syntax-rule
  (run-expr (S) body)
  (expr-with-halt (lambda (S) body)))

; Given choice1 and choice2 returns a tuple of values where
; the two choices are randomly ordered
(define (non-deterministic choice1 choice2)
  (begin (define shuffled (shuffle (list choice1 choice2)))
         (values (car shuffled) (cadr shuffled))))

(define (run-exprs-non-deterministic S a1 a2)
  (let*-values ([(l1 l2) (non-deterministic a1 a2)]
                [(r1 S+) (l1 S)]
                [(r2 S++) (l2 S+)])
    (begin
      (if (eq? l1 a1)
          (values S++ r1 r2)
          (values S++ r2 r1)))))


(define aexp
  (lambda ast
    (match ast
      [(list var-name/int)
       (run-expr (S)
                 (values (if (integer? var-name/int)
                             var-name/int
                             (lookup-var S var-name/int))
                         S))]
      [(list "(" a ")")
       (run-expr (S) (a S))]
      [(list "read" "(" ")")
       (run-expr (S)
         (values (peek-input S) (pop-front-input S)))]
      [(list a1 "+" a2)
       (run-expr (S)
                 (let-values ([(S+ r1 r2) (run-exprs-non-deterministic S a1 a2)])
                   (if (halt? S+)
                       (values (void) S+)
                       (values (+ r1 r2) S+))))]
      [(list a1 "/" a2)
       (run-expr (S)
                 (let-values ([(S+ r1 r2) (run-exprs-non-deterministic S a1 a2)])
                   (if (halt? S+)
                       (values (void) S+)
                       (if (equal? r2 0)
                           (values (void) (halt! S+))
                           (values (/ r1 r2) S+)))))]
      [(list "++" var-name)
       (run-expr (S)
         (begin
           (define val++ (+ (lookup-var S var-name) 1))
           (define S+ (update-var S var-name val++))
           (values val++ S+)))]
      )))

(define bexp
  (lambda ast
    (match ast
      [(list boolean-val)
       (lambda (S)
         (values boolean-val S))]
      [(list "!" b)
       (lambda (S)
         (let-values ([(bval S+) (b S)])
           (values (not bval) S+)))]
      [(list "(" b ")")
       (run-expr (S) (b S))]
      [(list a1 "<=" a2)
       (lambda (S)
         (let*-values ([(i1 S+) (a1 S)]
                       [(i2 S++) (a2 S+)])
           (values (<= i1 i2) S++)))]
      [(list b1 "&&" b2)
       (lambda (S)
         (let-values ([(bval1 S+) (b1 S)])
           (if bval1 (values #f S+) (b2 S+))))]
    )))

(provide aexp
         bexp
         )
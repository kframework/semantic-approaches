#lang racket

(require racket/match)
(require "imp-sigma.rkt")

; Conditionally executes a statement without-halt with the configuration S
; if S is in a halting state, do nothing and return that same S
; otherwise, runs without-halt with the configuration S
(define stmt-with-halt
  (lambda (without-halt)
    (lambda (S)
      (if (halt? S)
          S
          (without-halt S)))))

; Given choice1 and choice2 returns a tuple of values where
; the two choices are randomly ordered
(define (non-deterministic choice1 choice2)
  (begin (define shuffled (shuffle (list choice1 choice2)))
         (values (car shuffled) (cadr shuffled))))

(define accepts-next-stmt?
  (lambda (proc)
    (let-values ([(_ keywords) (procedure-keywords proc)])
      (not (empty? keywords)))))

(require racket/undefined)

(define-syntax-rule
  (run-stmt (S) body)
  (stmt-with-halt (lambda (S) body)))

(define id_list
  (lambda ast
    (match ast
      [(list id) (run-stmt (S) (add-var S id))]
      [(list id "," rest) (run-stmt (S) (rest (add-var S id)))])))

(define stmt
  (lambda ast
    (match ast
      [(list "int" ids ";") (run-stmt (S) (ids S))]
      [(list "spawn" b)
       (lambda (S #:next-stmt [next-stmt undefined])
         (if (halt? S)
             S
             (if (eq? next-stmt undefined)
                 (b S)
                 (let-values ([(s1 s2) (non-deterministic b next-stmt)])
                   (s2 (s1 S))))))]
      [(list id "=" a ";")
       (run-stmt (S)
                 (let-values ([(a-result S+) (a S)])
                   (if (halt? S+)
                       S+
                       (update-var S+ id a-result))))]
      [(list "if" "(" c ")" block1 "else" block2)
       (run-stmt (S)
                 (let-values ([(bval S+) (c S)])
                   (if (halt? S+)
                       S+
                       (if bval (block1 S+) (block2 S+)))))]
      [(list "while" "(" c ")" body)
       (run-stmt (S)
                 (let-values ([(bval S+) (c S)])
                   (if (halt? S+)
                       S+
                       (if bval
                           ((stmt "while" "(" c ")" body) (body S+))
                           S+))))]
      [(list "print" "(" a ")" ";")
       (run-stmt (S)
                 (let-values ([(a-result S+) (a S)])
                   (if (halt? S+)
                       S+
                       (append-output S+ (list a-result)))))]
      [(list "halt" ";") (run-stmt (S) (halt! S))]
      [(list b) (run-stmt (S) (b S))])))

(define seq
  (lambda ast
    (match ast
      [(list s) (run-stmt (S) (s S))]
      [(list s ss) (run-stmt (S)
                              (if (accepts-next-stmt? s)
                                  (s S #:next-stmt ss)
                                  (ss (s S))))]
      )))

(define block
  (lambda ast
    (match ast
      [(list "{" "}") (lambda (S) S)]
      [(list "{" body "}")
       (run-stmt (S)
                 (exit-block (body (enter-block S))))]
      )))

(provide id_list
         stmt
         block
         seq
)
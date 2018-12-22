#lang racket

(define (pdb s)
  (display s)
  (display "\n"))

(define-syntax-rule (#%module-begin PARSE-TREE)
  (#%plain-module-begin (begin
                               PARSE-TREE
                               )))



(define (add-var-mem mem var-name val)
  (dict-set mem var-name val))

(define (add-var-loc loc var-name)
  (set-add loc var-name))


(define (lookup-var mem loc var-name)
  (if (set-member? loc var-name)
      (dict-ref mem var-name)
      (begin (display "Undefined variable: ")
             (pdb var-name)
             (exit 1))))


(define (order-shuffle lambda1 lambda2)
  (begin (define shuffled (shuffle (list lambda1 lambda2)))
         (values (car shuffled) (cadr shuffled))))

(define prog
  (lambda ast
    (match ast
      [(list s)
       (begin (define mem '())
              (define loc (list->set '()))
              (pdb "Start executing")
              (pdb "Finish executing")
              (pdb "Output:")
              (define-values (mm ll) (s mem loc))
              (display "\n")
              (pdb "Memory:")
              (pdb mm)
              (pdb "Location:")
              (pdb ll)
              (display "\n"))]
      )))

(define id_list
  (lambda ast
    (match ast
      [(list id)
       (lambda (mem loc)
        (begin (define mem+ (add-var-mem mem id 0))
               (define loc+ (add-var-loc loc id))
               (values mem+ loc+)))]

      [(list id "," rest)
       (lambda (mem loc)
        (begin (define mem+ (add-var-mem mem id 0))
               (define loc+ (add-var-loc loc id))
               (rest mem+ loc+)))]
      )))

(define stmt
  (lambda ast
    (match ast
      [(list "print" "(" a ")" ";")
       (lambda (mem loc)
         (begin
                (define-values (a-result mem+ loc+) (a mem loc))
                (display a-result)
                (display "\n")
                (values mem+ loc+)))]
      [(list "int" ids ";")
       (lambda (mem loc)
         (begin (define-values (mem+ loc+) (ids mem loc))
                (values mem+ loc+)))]
      [(list "int" ids ";" s)
       (lambda (mem loc)
         (begin (define-values (mem+ loc+) (ids mem loc))
                ((stmt "let" ids "in" s) mem+ loc+)))]
      [(list "let" ids "in" s)
       (lambda (mem loc)
         (begin (define-values (mem+ loc+) (s mem loc))
                (values mem+ loc)))]
      [(list id "=" a ";")
       (lambda (mem loc)
         (begin (define-values (a-result mem+ loc+) (a mem loc))
                (values (add-var-mem mem+ id a-result) loc+)))]
      [(list b)
       (lambda (mem loc) (b mem loc))]
      [(list s1 s2)
       (lambda (mem loc)
         (begin (define-values (mem+ loc+) (s1 mem loc))
                (s2 mem+ loc+)))]
      [(list "if" "(" b ")" true_s "else" false_s)
       (lambda (mem loc)
         (begin
           (define-values (b_val mem+ loc+) (b mem loc))
           (if b_val
               (true_s mem+ loc+)
               (false_s mem+ loc+))))]
      [(list "while" "(" b ")" s)
       (lambda (mem loc)
         (begin
           (define-values (b_val mem+ loc+) (b mem loc))
           (if b_val
               (begin (let-values ([(mem++ loc++) (s mem+ loc+)])
                      ((stmt "while" "(" b ")" s) mem++ loc++)))
               (values mem+ loc+))))]
      )))

(define aexp
  (lambda ast
    (match ast
      [(list var-name/int)
       (lambda (mem loc)
         (values (if (integer? var-name/int)
                     var-name/int
                     (lookup-var mem loc var-name/int))
                 mem
                 loc))]
      [(list a1 "+" a2)
       (lambda (mem loc)
         (begin
           (define-values (l1 l2) (order-shuffle a1 a2))
           (define-values (r1 mem+ loc+) (l1 mem loc))
           (define-values (r2 mem++ loc++) (l2 mem+ loc+))
           (values (+ r1 r2) mem++ loc++)))]
      [(list a1 "/" a2)
       (lambda (mem loc)
         (begin
           (define-values (l1 l2) (order-shuffle a1 a2))
           (define-values (r1 mem+ loc+) (l1 mem loc))
           (define-values (r2 mem++ loc++) (l2 mem+ loc+))
           (if (eq? l1 a1)
               (values (/ r1 r2) mem++ loc++)
               (values (/ r2 r1) mem++ loc++))))]
      [(list "++" id)
       (lambda (mem loc)
         (begin
           (define val (lookup-var mem loc id))
           (define mem+ (add-var-mem mem id (+ val 1)))
           (values (+ val 1) mem+ loc)))]
      )))


(define bexp
  (lambda ast
    (match ast
      [(list #t)
       (lambda (mem loc)
         (values #t mem loc))]
      [(list #f)
       (lambda (mem loc)
         (values #f mem loc))]
      [(list a1 "<=" a2)
        (lambda (mem loc)
         (begin
           (define-values (l1 l2) (order-shuffle a1 a2))
           (define-values (r1 mem+ loc+) (l1 mem loc))
           (define-values (r2 mem++ loc++) (l2 mem+ loc+))
           (if (eq? l1 a1)
               (values (<= r1 r2) mem++ loc++)
               (values (<= r2 r1) mem++ loc++))))]
      [(list a1 "&&" a2)
        (lambda (mem loc)
         (begin
           (define-values (r1 mem+ loc+) (a1 mem loc))
           (if (not r1)
               (values #f mem+ loc+)
               (a2 mem+ loc+))))]
      [(list "!" b)
       (lambda (mem loc)
         (begin
           (define-values (b_val mem+ loc+) (b mem loc))
           (values (not b_val) mem+ loc+)))]
      )))
(define block
  (lambda ast
    (match ast
      [(list "{" s "}")
       (lambda (mem loc)
          (begin (define-values (mem+ loc+) (s mem loc))
                 (values mem+ loc)))]
      [(list "{" "}")
       (lambda (mem loc)
          (values mem loc))]
      )))
(provide #%module-begin
         #%datum
         #%app
         prog
         id_list
         stmt
         aexp
         bexp
         block)

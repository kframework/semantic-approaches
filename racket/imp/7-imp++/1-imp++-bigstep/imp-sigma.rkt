#lang racket

; The Sigma here represents the configuration that goes with the syntax object
; in the big-step rules
; Sigma contains
; (1) the variable environment (see imp-env.rkt)
; (2) the input buffer
; (3) the output buffer
; (4) whether the configuration is in a halting state

(require "imp-env.rkt")

(struct sigma (env input output halt?))
(define default-sigma (lambda () (sigma (make-env) '() '() #f)))
(define make-sigma (lambda (input) (sigma (make-env) input '() #f)))

(define get-env sigma-env)

(define halt? sigma-halt?)
(define (halt! S)
  (let ([env (sigma-env S)]
        [input (sigma-input S)]
        [output (sigma-output S)])
    (sigma env input output #t)))

(define (update-env S env)
  (let ([input (sigma-input S)]
        [output (sigma-output S)]
        [halt (sigma-halt? S)])
    (sigma env input output halt)))

(define (update-input S input)
  (let ([env (sigma-env S)]
        [output (sigma-output S)]
        [halt (sigma-halt? S)])
    (sigma env input output halt)))

(define (update-output S output)
  (let ([env (sigma-env S)]
        [input (sigma-input S)]
        [halt (sigma-halt? S)])
    (sigma env input output halt)))

(define (update-var S var-name value)
  (update-env S (env-update-var (sigma-env S) var-name value)))

(define (add-var S var-name)
  (update-env S (env-add-var (sigma-env S) var-name)))

(define (lookup-var S var-name)
  (env-lookup-var (sigma-env S) var-name))

(define (enter-block S)
  (update-env S (env-enter-block (sigma-env S))))

(define (exit-block S)
  (update-env S (env-exit-block (sigma-env S))))

(define (peek-input S)
  (car (sigma-input S)))

(define (pop-front-input S)
  (let ([input (sigma-input S)])
    (update-input S (cdr input))))

(define (append-output S v)
  (let ([output (sigma-output S)])
    (update-output S (append output v))))

(define (display-sigma S)
  (display (~a "Env: " (env-get-var-map (sigma-env S)) #\newline
               "Input: " (sigma-input S) #\newline
               "Output: " (sigma-output S) #\newline
               "Halted?: " (sigma-halt? S) #\newline)))

(provide sigma default-sigma make-sigma
         get-env update-env update-var lookup-var add-var
         display-sigma
         peek-input pop-front-input append-output
         enter-block exit-block
         halt? halt!)
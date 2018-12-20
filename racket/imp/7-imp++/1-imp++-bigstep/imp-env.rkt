#lang racket

; Helper functions for "pushing" and "popping" variables to the environment
; when entering and exiting blocks.

; An environment is represented as a pair (varmap, restore_functions)
; where
; (1) varmap is the variable->value assignment in the current block and
; (2) restore_functions is a stack of functions that are called to restore
;     the previous varmap in the enclosing block
(define make-env (lambda () (list (list) (list identity))))

(define env-enter-block
  (lambda (env)
    (cons (car env) (cons identity (cdr env)))))

(define env-get-var-map car)
(define env-get-restore-stack car)
(define env-peek-restore cadr)
(define env-pop-restore cddr)
(define env-update
  (lambda (env
           [new-var-map (env-get-var-map env)]
           [new-restore (env-peek-restore env)])
    (cons new-var-map (cons new-restore (env-pop-restore env)))))

(define env-exit-block
  (lambda (env)
    (let ([var-map (env-get-var-map env)]
          [restore (env-peek-restore env)])
      (cons (restore var-map) (env-pop-restore env)))))

; Chains a restore function together.
; This happens when a new variable is declared in a block.
; We would need to extend the current restore function to restore
; this variable's value, when the block exits.
(define chain-restore
  (lambda (current-var-map var-name prev-restore)
    (lambda (var-map)
      (prev-restore (if (dict-has-key? current-var-map var-name)
                        (dict-set var-map var-name (dict-ref current-var-map var-name))
                        (dict-remove var-map var-name))))))

(define env-add-var
  (lambda (env var-name)
    (let* ([var-map (env-get-var-map env)]
           [prev-restore (env-peek-restore env)]
           [new-restore (chain-restore var-map var-name prev-restore)]
           [new-var-map (dict-set var-map var-name 0)])
      (env-update env new-var-map new-restore))))

(define env-update-var
  (lambda (env var-name value)
    (let* ([var-map (env-get-var-map env)]
           [new-var-map (if (dict-has-key? var-map var-name)
                            (dict-set var-map var-name value)
                            (error (~a "Variable " var-name " is not declared.")))])
      (env-update env new-var-map))))

(define env-lookup-var
  (lambda (env var-name)
    (let ([var-map (env-get-var-map env)])
      (if (dict-has-key? var-map var-name)
          (dict-ref var-map var-name)
          (error (~a "Variable " var-name " is not declared."))))))

(provide make-env
         env-enter-block
         env-exit-block
         env-add-var
         env-update-var
         env-lookup-var
         env-get-var-map)
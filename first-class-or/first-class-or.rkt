#lang racket
(provide first-class-or)
(require (for-syntax racket/base syntax/parse))

(define (or-function . args)
  (cond [(empty? args)
         #false]
        [(first args)] ; answer = question if not false
        [else
        (apply or-function (rest args))]))
        
(define-syntax (first-class-or stx)
  (syntax-parse stx
   [(_)
    #'#false]
   [(_ ?a . ?b)
    #'(let ([a-val ?a])
        (if a-val a-val (first-class-or . ?b)))]
   [_:id
    #'or-function]))

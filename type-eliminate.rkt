#lang racket

(provide (rename-out [ast:pgm type-eliminate]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi . ,def+)
     `(program ,pi . ,(map ast:def def+))]))

(define ast:def
  (match-lambda
    [`(define ,p ,e)
     `(define ,p ,(ast:exp e))]))

(define ast:exp
  (lambda (expr)
    (match expr
      [`(typed ,e ,t)
        (ast:exp e)]
      [`(vector . ,e*)
        (let ([t* (map typed->type e*)])
         `(vector-tag ,(vec-elem-type*->vec-tag t*) . ,(map ast:exp e*)))]
      [`(fun-ref ,l) expr]
      [`(let ([,v ,e]) ,b)
       `(let ([,v ,(ast:exp e)]) ,(ast:exp b))]
      [`(,op . ,e*)
       `(,op . ,(map ast:exp e*))]
      [_(% expr)])))

(define vec-elem-type*->vec-tag
  (lambda (t*)
    (let ([len (length t*)])
      (let loop ([t* (reverse t*)] [pm 0])
        (if (empty? t*)
          (bitwise-ior 0
            (arithmetic-shift len 1)
            (arithmetic-shift pm 7))
          (loop (cdr t*)
            (bitwise-ior
              (arithmetic-shift pm 1)
              (if (vector-type? (car t*)) 1 0))))))))


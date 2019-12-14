#lang racket

(provide (rename-out [ast:pgm limit-define]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi . ,def+)
     `(program ,pi . ,(map ast:def def+))]))

(define ast:def
  (match-lambda
    [`(define (,f . ,v*) ,e)
      (let* ([vecp (gensym 'vecp)] [e (ast:exp v* vecp e)])
        (if (< (length v*) 6)
         `(define (,f . ,v*) ,e)
         `(define (,f ,@(take v* 5) ,vecp) ,e)))]))

(define ast:exp
  (lambda (v* vecp expr)
    (let ([recur (curry ast:exp v* vecp)])
      (match expr
        [`(typed ,(? symbol? v) ,t)
          (let ([idx (index-of v* v)])
            (if (or (not idx) (< idx 5))
              (% expr)
             `(vector-ref (typed ,vecp (Vector ...)) ,(- idx 5))))]
        [`(let ([,v ,e1]) ,e2)
         `(let ([,v ,(recur e1)]) ,(recur e2))]
        [`(app ,e1 . ,e*)
          (let ([e1 (recur e1)] [e* (map recur e*)])
            (if (< (length e*) 6)
             `(app ,e1 . ,e*)
             `(app ,e1 ,@(take e* 5) 
                (typed (vector . ,(drop e* 5)) (Vector ...)))))]
        [`(,op . ,e*)
         `(,op . ,(map recur e*))]
        [_(% expr)]))))


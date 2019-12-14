#lang racket

(provide (rename-out [ast:pgm limit-define]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi . ,def+)
      (let ([f+ (map define->name def+)])
       `(program ,pi . ,(map (curry ast:def f+) def+)))]))

(define ast:def
  (lambda (f+ def)
    (match def
      [`(define (,f . ,v*) ,e)
        (let* ([vecp (gensym 'vecp)] [e (ast:exp f+ v* vecp e)])
          (if (< (length v*) 6)
           `(define (,f . ,v*) ,e)
           `(define (,f ,@(take v* 5) ,vecp) ,e)))])))

(define ast:exp
  (lambda (f+ v* vecp expr)
    (let ([recur (curry ast:exp f+ v* vecp)])
      (match expr
        [`(typed ,e ,t)
         `(typed ,(recur e) ,t)]
        [`(let ([,v ,e1]) ,e2)
         `(let ([,v ,(recur e1)]) ,(recur e2))]
        [`(app ,e1 . ,e*)
          (let ([e1 (recur e1)] [e* (map recur e*)])
            (if (< (length e*) 6)
             `(app ,e1 . ,e*)
             `(app ,e1 ,@(take e* 5)
               ,(let ([vec-e* (drop e* 5)])
                 `(typed 
                    (vector . ,vec-e*)
                    (Vector . ,(map typed->type vec-e*)))))))]
        [`(,op . ,e*)
         `(,op . ,(map recur e*))]
        [_(ast:arg f+ v* vecp expr)]))))

(define ast:arg
  (lambda (f+ v* vecp arg)
    (match arg
      [(? symbol?)
       (if (memq arg f+)
        `(fun-ref ,arg)
         (let ([idx (index-of v* arg)])
           (if (or (not idx) (< idx 5)) arg
            `(vector-ref
               (typed ,vecp Vector)
               (typed ,(- idx 5) Integer)))))]
      [(else) arg])))


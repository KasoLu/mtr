#lang racket

(provide (rename-out [ast:pgm uniquify]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi ,def* ... ,expr)
      (let ([def/u* (map ast:def/dec def*)])
        (let ([f* (map define->name def*)] [f/u* (map define->name def/u*)])
          (let ([v.u* (map make-pair f* f/u*)])
           `(program ,pi ,@(map (curry ast:def/exp v.u*) def/u*)
             ,(ast:exp v.u* expr)))))]))

(define ast:def/dec
  (match-lambda
    [`(define (,f . ,v*) ,e)
     `(define (,(gensym f) . ,v*) ,e)]))

(define ast:def/exp
  (lambda (v.u* def)
    (match def
      [`(define (,f . ,v*) ,e)
        (let ([vu* (map gensym v*)])
         `(define (,f . ,vu*)
           ,(ast:exp (assoc-add v.u* (map make-pair v* vu*)) e)))])))

(define ast:exp
  (lambda (v.u* expr)
    (let ([recur (curry ast:exp v.u*)])
      (match expr
        [`(typed ,e ,t)
         `(typed ,(recur e) ,t)]
        [`(let ([,v ,e1]) ,e2)
          (let ([v/u (gensym v)])
           `(let ([,v/u ,(recur e1)])
             ,(ast:exp (assoc-add v.u* v v/u) e2)))]
        [`(lambda ,v* ,e1)
          (let ([v/u* (map gensym v*)])
           `(lambda ,v/u* ,(ast:exp (assoc-add v.u* (map make-pair v* v/u*)) e1)))]
        [`(,op . ,e*)
         `(,op . ,(map recur e*))]
        [_(ast:arg v.u* expr)]))))

(define ast:arg
  (lambda (v.u* arg)
    (match arg
      [(? symbol?) (assoc-ref v.u* arg)]
      [(else) arg])))


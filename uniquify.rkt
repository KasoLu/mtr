#lang racket

(provide (rename-out [ast:pgm uniquify]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi . ,def+)
      (let ([def/u+ (map ast:def/dec def+)])
        (let ([f+ (map define->name def+)] [f/u+ (map define->name def/u+)])
          (let ([env (env-add (make-env) f+ f/u+)])
           `(program ,pi . ,(map (curry ast:def/exp env) def/u+)))))]))

(define ast:def/dec
  (match-lambda
    [`(define (,f . ,v*) : ,r ,fi ,e)
     `(define (,(gensym f) . ,v*) : ,r ,fi ,e)]))

(define ast:def/exp
  (lambda (env def)
    (match def
      [`(define (,f [,v* : ,t*]...) : ,r ,fi ,e)
        (let ([vu* (map gensym v*)])
         `(define (,f . ,(map type-anoc vu* t*)) : ,r ,fi
           ,(ast:exp (env-add env v* vu*) e)))])))

(define ast:exp
  (lambda (env expr)
    (let ([recur (curry ast:exp env)])
      (match expr
        [`(bind [,v ,e1] ,e2)
          (let ([v/u (gensym v)])
           `(bind [,v/u ,(recur e1)]
             ,(ast:exp (env-add env v v/u) e2)))]
        [`(lambda ([,v* : ,t*]...) : ,r ,e1)
          (let ([v/u* (map gensym v*)])
           `(lambda ,(map type-anoc v/u* t*) : ,r 
             ,(ast:exp (env-add env v* v/u*) e1)))]
        [`(,op . ,e*)
         `(,op . ,(map recur e*))]
        [_(ast:arg env expr)]))))

(define ast:arg
  (lambda (env arg)
    (match arg
      [(? symbol?) (env-ref env arg)]
      [(else) arg])))


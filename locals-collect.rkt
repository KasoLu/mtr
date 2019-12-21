#lang racket

(provide (rename-out [ast:pgm locals-collect]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi . ,def+)
     `(program ,pi . ,(map ast:def def+))]))

(define ast:def
  (match-lambda
    [`(define (,f . ,v*) ,fi ([,lb* ,tail*]...))
      (parameterize ([$var-rcd/cur (list)])
        (for ([tail tail*]) (ast:tail tail))
       `(define (,f . ,v*)
         ,(assoc-add fi 'var* ($var-rcd/cur))
         ,(map make-pair lb* tail*)))]))

(define ast:tail
  (match-lambda
    [`(seq ,s ,t)
      (ast:tail t)
      (ast:stmt s)]
    [_(void)]))

(define ast:stmt
  (match-lambda
    [`(assign ,v ,_)
      (unless (eq? v '_) ($var-rcd/add v))]
    [_(void)]))

(define $var-rcd/cur (make-parameter (void)))
(define $var-rcd/add 
  (lambda (var) 
    ($var-rcd/cur (set-union ($var-rcd/cur) `(,var)))))


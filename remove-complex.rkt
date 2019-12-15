#lang racket

(provide (rename-out [ast:pgm remove-complex]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi . ,def+)
     `(program ,pi . ,(map ast:def def+))]))

(define ast:def
  (match-lambda
    [`(define ,p ,fi ,e)
     `(define ,p ,fi ,(ast:exp e))]))

(define ast:exp
  (lambda (expr)
    (match expr
      [`(let ([,v ,e]) ,b)
       `(let ([,v ,(ast:exp e)]) ,(ast:exp b))]
      [`(if ,e1 ,e2 ,e3)
       `(if . ,(map ast:exp `(,e1 ,e2 ,e3)))]
      [`(,op . ,e*) #:when (to-smp-op? op)
        (let loop ([e* e*] [v* (list)] [v:e (make-assoc)])
          (if (empty? e*)
            (expand-bind v:e `(,op . ,(reverse v*)))
            (let ([v (gensym 'rcv)] [e (ast:exp (car e*))])
              (if (smp-exp? e)
                (loop (cdr e*) (cons e v*) v:e)
                (loop (cdr e*) (cons v v*) (assoc-add v:e v e))))))]
      [_(% expr)])))

(define expand-bind
  (lambda (v:e expr)
    (for/fold ([acc expr]) ([v.e v:e])
     `(let (,v.e) ,acc))))

(define to-smp-op?
  (curry set-member? 
    (set-union ath-op lgc-op cmp-op '(vector-ref vector-set! app))))


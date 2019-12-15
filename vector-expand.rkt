#lang racket

(provide (rename-out [ast:pgm vector-expand]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi . ,def+)
     `(program ,pi . ,(map ast:def def+))]))

(define ast:def
  (match-lambda
    [`(define (,f . ,v*) ,e)
      (parameterize ([$vec-rcd/cur (list)])
        ($vec-rcd/add (first v*))
        (unless (< (length v*) 6)
          ($vec-rcd/add (last v*)))
        (let ([e (ast:exp e)])
         `(define (,f . ,v*) ([vec* ,($vec-rcd/cur)]) ,e)))]))

(define ast:exp
  (lambda (expr)
    (match expr
      [`(vector-tag ,tag . ,e*)
        (let ([vec (gensym 'vec)] [e* (map/reverse ast:exp e*)])
          ($vec-rcd/add vec)
          (vec-collect vec (length e*) tag
            (vec-set vec e*)))]
      [`(let ([,v ,e1]) ,e2)
       `(let ([,v ,(ast:exp e1)]) ,(ast:exp e2))]
      [`(fun-ref ,l) expr]
      [`(,op . ,e*)
       `(,op . ,(map ast:exp e*))]
      [_(% expr)])))

(define $vec-rcd/cur (make-parameter (void)))
(define $vec-rcd/add
  (lambda (vec)
    ($vec-rcd/cur (cons vec ($vec-rcd/cur)))))

(define vec-set
  (lambda (vec e*)
    (let loop ([e* e*] [acc vec])
      (if (empty? e*)
        (% acc)
        (loop (cdr e*)
         `(let ([_ (vector-set! ,vec ,(sub1 (length e*)) ,(car e*))]) ,acc))))))

(define vec-collect
  (lambda (vec len tag vse)
    (let ([siz (* 8 (add1 len))])
     `(let ([_ (if (< (+ ,g-fp ,siz) ,g-fe) (void) (collect ,siz))])
        (let ([,vec (allocate ,len ,tag)]) ,vse)))))


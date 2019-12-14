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

;(require "interp.rkt")
;(require "parse.rkt")
;(test MTR/interp
; `(,parse ,ast:pgm)

; `(program () (let ([x 10]) x))
; `(program () (let ([x (let ([x 20]) x)]) x))
; `(program () (let ([x (let ([x 20]) x)]) (let ([x 30]) x)))
; `(program () (let ([x (lambda ([a1 : Integer]) : Integer (+ a1 10))]) (x 20)))
; `(program ()
;    (define (aaa [a1 : Integer]) : Integer (+ 10 a1))
;    (define (bbb [b1 : Integer]) : Integer (+ (aaa 10) b1)) 
;    (define (ccc [c1 : Integer]) : Integer (+ 20 c1))
;    (define (ddd [d1 : (Integer -> Integer)]
;                 [d2 : (Integer -> Integer)])
;            : (Vector Integer Integer)
;      (vector (d1 30) (d2 40)))
;    (vector-ref (ddd bbb ccc) 0))
; `(program ()
;    (define (map-vec [f : (Integer -> Integer)]
;                     [v : (Vector Integer Integer)])
;            : (Vector Integer Integer)
;      (vector (f (vector-ref v 0)) (f (vector-ref v 1))))
;    (define (add1 [x : Integer]) : Integer
;      (+ x 1))
;    (vector-ref (map-vec add1 (vector 0 41)) 1))
; `(program ()
;    (let ([x 10])
;      (let ([f (lambda () : Integer (+ x (+ 20 20)))])
;        (let ([x 30])
;          (f)))))
; )


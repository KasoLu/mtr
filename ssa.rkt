#lang racket

(provide (rename-out [@pgm ssa]))
(require "helper.rkt")

(define @pgm
  (match-lambda
    [`(program ,pi . ,def+)
      (let ([def-u+ (map @def/dec def+)])
        (let ([f* (map define->name def+)] [fu* (map define->name def-u+)])
         `(program ,(assoc-add pi 'func-ssa (map make-pair fu* f*)) .
           ,(map (curry @def/exp (map make-pair f* fu*)) def-u+))))]))

(define @def/dec
  (lambda (def)
    (match def
      [`(define (,f . ,v*) ,e)
       `(define (,(gensym f) . ,v*) ,e)])))

(define @def/exp
  (lambda (venv def)
    (match def
      [`(define (,f . ,v*) ,e)
        (let ([vu* (map gensym v*)])
         `(define (,f . ,vu*)
           ,(@exp (assoc-add venv (map make-pair v* vu*)) e)))])))

(define @exp
  (lambda (venv expr)
    (let ([recur (curry @exp venv)])
      (match expr
        [`(typed ,e ,t)
         `(typed ,(recur e) ,t)]
        [`(let ([,v ,e1]) ,e2)
          (let ([vu (gensym v)])
           `(let ([,vu ,(recur e1)])
             ,(@exp (assoc-add venv v vu) e2)))]
        [`(lambda ,v* ,e1)
          (let ([vu* (map gensym v*)])
           `(lambda ,vu* ,(@exp (assoc-add venv (map make-pair v* vu*)) e1)))]
        [`(,op . ,e*)
         `(,op . ,(map recur e*))]
        [_(@arg venv expr)]))))

(define @arg
  (lambda (venv arg)
    (match arg
      [(? symbol?) (assoc-ref venv arg)]
      [(else) arg])))

;(require "interp.rkt")
;(require "parse.rkt")
;(test MTR/interp
; `(,parse ,\@pgm)

; `(program () (let ([x 10]) x))
; `(program () (let ([x (let ([x 20]) x)]) x))
; `(program () (let ([x (let ([x 20]) x)]) (let ([x 30]) x)))
; `(program () (lambda ([a1 : Integer]) : Integer (+ a1 10)))
; `(program () (let ([x (lambda ([a1 : Integer]) : Integer (+ a1 10))]) (x 20)))
; `(program ()
;    (define (aaa [a1 : Integer]) : Integer (+ 10 a1))
;    (define (bbb [b1 : Integer]) : Integer (+ (aaa 10) b1)) 
;    (define (ccc [c1 : Integer]) : Integer (+ 20 c1))
;    (define (ddd [d1 : (Integer -> Integer)]
;                 [d2 : (Integer -> Integer)])
;            : (Vector Integer Integer)
;      (vector (d1 30) (d2 40)))
;    (ddd bbb ccc))
; )


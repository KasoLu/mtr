#lang racket

(provide (rename-out [@pgm anf]))
(require "helper.rkt")

(define @pgm
  (match-lambda
    [`(program ,pi . ,def+)
     `(program ,pi . ,(map @def def+))]))

(define @def
  (match-lambda
    [`(define (,f . ,v*) ,fi ,e)
     `(define (,f . ,v*) ,fi ,(@exp e))]))

(define @exp
  (lambda (expr)
    (match expr
      [`(let ([,v ,e]) ,b)
       `(let ([,v ,(@exp e)]) ,(@exp b))]
      [`(if ,e1 ,e2 ,e3)
       `(if . ,(map @exp `(,e1 ,e2 ,e3)))]
      [`(,op . ,e*) #:when (anf-op? op)
        (let loop ([e* e*] [v* (list)] [v:e (make-assoc)])
          (if (empty? e*)
            (expand-bind v:e `(,op . ,(reverse v*)))
            (let ([v (gensym 'anf)] [e (@exp (car e*))])
              (if (smp-exp? e)
                (loop (cdr e*) (cons e v*) v:e)
                (loop (cdr e*) (cons v v*) (assoc-add v:e v e))))))]
      [_(% expr)])))

(define expand-bind
  (lambda (v:e expr)
    (for/fold ([acc expr]) ([v.e v:e])
     `(let (,v.e) ,acc))))

(define anf-op?
  (curry set-member? 
    (set-union ath-op lgc-op cmp-op '(vector-ref vector-set! app))))

;(require "interp.rkt")
;(require "parse.rkt" "ssa.rkt" "c2f.rkt" "limit-proc.rkt" "type-eliminate.rkt" "v2a.rkt")
;(test MTR/interp
; `(,parse ,ssa ,c2f ,limit-proc ,type-eliminate ,vector-to-alloc ,\@pgm)

; `(program ()
;    (define (aaa [a1 : Integer]) : Integer (+ 10 a1))
;    (define (bbb [b1 : Integer]) : Integer (+ (aaa 10) b1)) 
;    (define (ccc [c1 : Integer]) : Integer (+ 20 c1))
;    (define (ddd [d1 : (Integer -> Integer)]
;                 [d2 : (Integer -> Integer)])
;            : (Vector Integer Integer)
;      (vector (d1 30) (d2 40)))
;    (ddd bbb ccc))
; `(program ()
;    (define (foo) : Integer 42)
;    (define (bar [arg1 : Integer]) : Boolean (< arg1 42))
;    (define (aaa [arg1 : Integer] [arg2 : Boolean]) : Integer
;      (if (and (bar 10) arg2) (foo) 20))
;    (aaa 24 #t))
; `(program ()
;    (define (map-vec [f : (Integer -> Integer)]
;                     [v : (Vector Integer Integer)])
;            : (Vector Integer Integer)
;      (vector (f (vector-ref v 0)) (f (vector-ref v 1))))
;    (define (add1 [x : Integer]) : Integer
;      (+ x 1))
;    (vector-ref (map-vec add1 (vector 0 41)) 1))
; `(program ()
;    (define (f [x : Integer]) : (Integer -> Integer)
;      (let ([y 4])
;        (lambda ([z : Integer]) : Integer
;          (+ x (+ y z)))))
;    (let ([g (f 5)])
;      (let ([h (f 3)])
;        (+ (g 11) (h 15)))))
; `(program ()
;    (define (b [z : (Integer -> (Integer -> Integer))]) : Integer
;      (let ([c (z 10)])
;        (c 20)))
;    (let ([f (lambda ([a : Integer]) : (Integer -> Integer)
;               (let ([x (lambda ([y : Integer]) : Integer y)])
;                 (lambda ([d : Integer]) : Integer
;                   (+ a (+ d (x 30))))))])
;      (b f)))
; `(program ()
;    (define (aaa [a1 : Integer] [a2 : Boolean]
;                 [a3 : Integer] [a4 : Boolean]
;                 [a5 : Integer] [a6 : Boolean]
;                 [a7 : Integer] [a8 : Boolean]) : Boolean
;      (eq? 10
;        ((lambda ([l1 : Integer] [l2 : Boolean]
;                  [l3 : Integer] [l4 : Boolean]
;                  [l5 : Integer] [l6 : Boolean]
;                  [l7 : Integer] [l8 : Boolean]) : Integer
;           (+ a1 (+ a3 (+ l5 l7))))
;         10 #t 20 #f 30 #t 40 #f)))
;    (aaa 50 #f 60 #t 70 #f 80 #t))
; )

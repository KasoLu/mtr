#lang racket

(provide (rename-out [@pgm vector-to-alloc]))
(require "helper.rkt")

(define @pgm
  (match-lambda
    [`(program ,pi . ,def+)
     `(program ,pi . ,(map @def def+))]))

(define @def
  (match-lambda
    [`(define (,f . ,v*) ,e)
      (parameterize ([$vec-rcd/cur (list)])
        ($vec-rcd/add (first v*))
        (unless (< (length v*) 6)
          ($vec-rcd/add (last v*)))
        (let ([e (@exp e)])
         `(define (,f . ,v*) ([vec* ,($vec-rcd/cur)]) ,e)))]))

(define @exp
  (lambda (expr)
    (match expr
      [`(vector-tag ,tag . ,e*)
        (let ([vec (gensym 'vec)] [e* (map/reverse @exp e*)])
          ($vec-rcd/add vec)
          (vec-collect vec (length e*) tag
            (vec-set vec e*)))]
      [`(let ([,v ,e1]) ,e2)
       `(let ([,v ,(@exp e1)]) ,(@exp e2))]
      [`(fun-ref ,l) expr]
      [`(,op . ,e*)
       `(,op . ,(map @exp e*))]
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

;(require "interp.rkt")
;(require "parse.rkt" "ssa.rkt" "c2f.rkt" "limit-proc.rkt" "type-eliminate.rkt")
;(test MTR/interp
; `(,parse ,ssa ,c2f ,limit-proc ,type-eliminate ,\@pgm)

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

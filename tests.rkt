#lang racket

(require "helper.rkt" "interp.rkt")
(require "parse.rkt" "uniquify.rkt" "closure-to-define.rkt" "limit-define.rkt"
         "type-eliminate.rkt" "vector-expand.rkt")

(test MTR/interp 
 `(
   ,identity ,parse ,uniquify ,closure-to-define ,limit-define ,type-eliminate
   ,vector-expand
  )

 `(program () 42)
 `(program () #t)
 `(program () #f)
 `(program () (void))
 `(program () (let ([x 10]) x))
 `(program () (let ([x (let ([x 20]) x)]) x))
 `(program () (let ([x (let ([x 20]) x)]) (let ([x 30]) x)))
 `(program () (let ([x (lambda ([a1 : Integer]) : Integer (+ a1 10))]) (x 20)))
 `(program () (if #t 42 24))
 `(program () (- 42))
 `(program () (+ 42 24))
 `(program () (- 42 24))
 `(program () (* 42 24))
 `(program () (/ 42 24))
 `(program () (and #t #f))
 `(program () (or #t #f))
 `(program () (not #t))
 `(program () (eq? 42 24))
 `(program () (eq? #t #f))
 `(program () (< 42 24))
 `(program () (> 39 24))
 `(program () (<= 42 24))
 `(program () (>= 42 24))
 `(program () (vector 42 #t))
 `(program () (vector (vector 1 2) (vector #t #f)))
 `(program () (vector-ref (vector 42 #t) 0))
 `(program () (vector-set! (vector 42 #t) 1 24))
 `(program () (let ([x (lambda ([a1 : Integer]) : Integer (+ a1 10))]) (x 20)))
 `(program ()
    (define (aaa [a1 : Integer]) : Integer (+ 10 a1))
    (define (bbb [b1 : Integer]) : Integer (+ (aaa 10) b1)) 
    (define (ccc [c1 : Integer]) : Integer (+ 20 c1))
    (define (ddd [d1 : (Integer -> Integer)]
                 [d2 : (Integer -> Integer)])
            : (Vector Integer Integer)
      (vector (d1 30) (d2 40)))
    (vector-ref (ddd bbb ccc) 0))
 `(program ()
    (define (map-vec [f : (Integer -> Integer)]
                     [v : (Vector Integer Integer)])
            : (Vector Integer Integer)
      (vector (f (vector-ref v 0)) (f (vector-ref v 1))))
    (define (add1 [x : Integer]) : Integer
      (+ x 1))
    (vector-ref (map-vec add1 (vector 0 41)) 1))
 `(program ()
    (let ([x 10])
      (let ([f (lambda () : Integer (+ x (+ 20 20)))])
        (let ([x 30])
          (f)))))
 `(program ()
    (define (foo) : Integer 42)
    (define (bar [arg1 : Integer]) : Boolean (< arg1 42))
    (define (aaa [arg1 : Integer] [arg2 : Boolean]) : Integer
      (if (and (bar 10) arg2) (foo) 20))
    (aaa 24 #t))
 `(program ()
    (define (f [x : Integer]) : (Integer -> Integer)
      (let ([y 4])
        (lambda ([z : Integer]) : Integer
          (+ x (+ y z)))))
    (let ([g (f 5)])
      (let ([h (f 3)])
        (+ (g 11) (h 15)))))
 `(program ()
    (define (b [z : (Integer -> (Integer -> Integer))]) : Integer
      (let ([c (z 10)])
        (c 20)))
    (let ([f (lambda ([a : Integer]) : (Integer -> Integer)
               (let ([x (lambda ([y : Integer]) : Integer y)])
                 (lambda ([d : Integer]) : Integer
                   (+ a (+ d (x 30))))))])
      (b f)))
 `(program ()
    (define (aaa [a1 : Integer] [a2 : Boolean]
                 [a3 : Integer] [a4 : Boolean]
                 [a5 : Integer] [a6 : Boolean]
                 [a7 : Integer] [a8 : Boolean]) : Boolean
      (eq? 10
        ((lambda ([l1 : Integer] [l2 : Boolean]
                  [l3 : Integer] [l4 : Boolean]
                  [l5 : Integer] [l6 : Boolean]
                  [l7 : Integer] [l8 : Boolean]) : Integer
           (+ a1 (+ a3 (+ l5 l7))))
         10 #t 20 #f 30 #t 40 #f)))
    (aaa 50 #f 60 #t 70 #f 80 #t))
 )


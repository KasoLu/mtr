#lang racket

(provide (rename-out [@pgm limit-proc]))
(require "helper.rkt")

(define @pgm
  (match-lambda
    [`(program ,pi . ,def+)
      (let ([f* (map define->name def+)])
       `(program ,pi . ,(map (curry @def f*) def+)))]))

(define @def
  (lambda (f* def)
    (match def
      [`(define (,f . ,v*) ,e)
        (let* ([vecp (gensym 'vecp)] [e (@exp f* v* vecp e)])
          (if (< (length v*) 6)
           `(define (,f . ,v*) ,e)
           `(define (,f ,@(take v* 5) ,vecp) ,e)))])))

(define @exp
  (lambda (f* v* vecp expr)
    (let ([recur (curry @exp f* v* vecp)])
      (match expr
        [`(typed ,e ,t)
         `(typed ,(recur e) ,t)]
        [`(let ([,v ,e1]) ,e2)
         `(let ([,v ,(recur e1)]) ,(recur e2))]
        [`(app ,e1 . ,e*)
          (let ([e1 (recur e1)] [e* (map recur e*)])
            (if (< (length e*) 6)
             `(app ,e1 . ,e*)
             `(app ,e1 ,@(take e* 5)
               ,(let ([vec-e* (drop e* 5)])
                 `(typed 
                    (vector . ,vec-e*)
                    (Vector . ,(map typed->type vec-e*)))))))]
        [`(,op . ,e*)
         `(,op . ,(map recur e*))]
        [_(@arg f* v* vecp expr)]))))

(define @arg
  (lambda (f* v* vecp arg)
    (match arg
      [(? symbol?)
       (if (memq arg f*)
        `(fun-ref ,arg)
         (let ([idx (index-of v* arg)])
           (if (or (not idx) (< idx 5)) arg
            `(vector-ref
               (typed ,vecp Vector)
               (typed ,(- idx 5) Integer)))))]
      [(else) arg])))

;(require "interp.rkt")
;(require "parse.rkt" "ssa.rkt" "c2f.rkt")
;(test MTR/interp
; `(,parse ,ssa ,c2f ,\@pgm)

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


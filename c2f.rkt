#lang racket

(provide (rename-out [@pgm c2f]))
(require "helper.rkt")

(define @pgm
  (match-lambda
    [`(program ,pi . ,def+)
      (parameterize ([$def-rcd/cur (list)] [$fv*-rcd/cur (list)])
        (let ([def+ (map (curry @def (map define->name def+)) def+)])
         `(program ,pi .
           ,(append ($def-rcd/cur) def+))))]))

(define @def
  (lambda (f* def)
    (match def
      [`(define (,f . ,v*) ,e)
        (let ([clsp (gensym 'clsp)])
         `(define (,f ,clsp . ,v*)
           ,(@exp f* v* clsp e)))])))

(define @exp
  (lambda (f* v* clsp expr)
    (let ([recur (curry @exp f* v* clsp)])
      (match expr
        [`(typed (lambda ,v* ,e1) ,t)
          (parameterize ([$fv*-rcd/cur (list)])
            (let* ([clsp (gensym 'clsp)] [e1 (@exp f* v* clsp e1)])
              (let ([lmd (gensym 'lmd)] [fv* ($fv*-rcd/cur)])
                ($def-rcd/add 
                 `(define (,lmd ,clsp . ,v*) ,(expand-fv* fv* clsp e1)))
                (let* ([fv*-r (reverse fv*)] [ft*-r (map typed->type fv*-r)])
                 `(typed (vector (typed ,lmd _) . ,fv*-r) (Vector _ . ,ft*-r))))))]
        [`(typed (let ([,v ,e1]) ,e2) ,t)
          (let ([e1 (recur e1)] [e2 (@exp f* (cons v v*) clsp e2)])
           `(typed (let ([,v ,e1]) ,e2) ,(typed->type e2)))]
        [`(typed ,(? symbol? v) ,t) #:when (memq v f*)
         `(typed (vector (typed ,v _)) (Vector _))]
        [`(typed ,(? symbol? v) ,t)
          (let ([expr (if (proc-type? t) `(typed ,v Vector) expr)])
            (begin (unless (memq v v*) ($fv*-rcd/add expr)) expr))]
        [`(typed ,e ,t)
          (let ([t (if (proc-type? t) 'Vector t)])
           `(typed
             ,(match e
                [`(app ,e1 . ,e*) (expand-app (recur e1) (map recur e*) t)]
                [`(,op . ,e*) `(,op . ,(map recur e*))]
                [_(% e)])
             ,(% t)))]))))

(define $fv*-rcd/cur (make-parameter (void)))
(define $fv*-rcd/add 
  (lambda (fv) 
    ($fv*-rcd/cur (cons fv ($fv*-rcd/cur)))))

(define $def-rcd/cur (make-parameter (void)))
(define $def-rcd/add
  (lambda (def)
    ($def-rcd/cur (cons def ($def-rcd/cur)))))

(define expand-fv*
  (lambda (fv* clsp expr)
    (let ([te (typed->type expr)])
      (let loop ([fv* fv*] [expr expr])
        (if (empty? fv*) expr
          (let ([fv (car fv*)])
            (loop (cdr fv*)
             `(typed
                (let 
                  ([,(typed->expr fv) 
                     (typed
                       (vector-ref
                         (typed ,(% clsp) Vector)
                         (typed ,(length fv*) Integer)) 
                      ,(typed->type fv))])
                 ,(% expr))
               ,(% te)))))))))

(define expand-app
  (lambda (f e* t)
    (let ([fun (gensym 'fun)])
     `(let ([,fun ,f])
        (typed
          (app
            (typed (vector-ref (typed ,fun Vector) (typed 0 Integer)) _)
            (typed ,fun Vector) .
           ,(% e*))
         ,(% t))))))

;(require "interp.rkt")
;(require "parse.rkt" "ssa.rkt")
;(test MTR/interp
; `(,parse ,ssa ,\@pgm)

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
; )


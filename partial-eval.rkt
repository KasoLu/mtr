#lang racket

(require "helper.rkt")
;(require "trace.rkt")

(define @pgm
  (match-lambda
    [`(program ,pi ,def* ... (define (,_) ,e))
      (let ([senv (map @def def*)])
        (for ([k.v senv]) (set-mcdr! k.v (set-closure-env (mcdr k.v) senv)))
       `(program ,pi ,(closure->lambda (@exp senv e))))]))

(define @def
  (match-lambda
    [`(define (,f . ,v*) ,e)
      (mcons f `(closure ,v* ,e #f))]))

(define @exp
  (lambda (senv expr)
    (let ([recur (curry @exp senv)])
      (match expr
        [`(typed ,e ,t)
         `(typed ,(recur e) ,t)]
        [`(read) expr]
        [`(void) expr]
        [`(,op . ,e*) #:when (ath-op? op)
          (let* ([e* (map recur e*)] [e-v* (map typed->expr e*)])
            (if (andmap integer? e-v*)
              (apply (ath-op->proc op) e-v*)
             `(,op . ,e*)))]
        [`(,op . ,e*) #:when (lgc-op? op)
          (let* ([e* (map recur e*)] [e-v* (map typed->expr e*)])
            (if (andmap boolean? e-v*)
              (apply (lgc-op->proc op) e-v*)
             `(,op . ,e*)))]
        [`(eq? . ,e*)
          (let* ([e* (map recur e*)] [e-v* (map typed->expr e*)])
            (if (or (andmap integer? e-v*) (andmap boolean? e-v*)) 
              (apply eq? e-v*)
             `(eq? . ,e*)))]
        [`(,op . ,e*) #:when (cmp-op? op)
          (let* ([e* (map recur e*)] [e-v* (map typed->expr e*)])
            (if (andmap integer? e-v*)
              (apply (cmp-op->proc op) e-v*)
             `(,op . ,e*)))]
        [`(let ([,v ,e]) ,b)
          (let* ([e (recur e)] [e-v (typed->expr e)])
            (if (val-exp? e-v)
              (typed->expr (@exp (env-add senv v e-v) b))
             `(let ([,v ,e]) ,(recur b))))]
        [`(if ,e1 ,e2 ,e3)
          (let* ([e1 (recur e1)] [e1-v (typed->expr e1)])
            (if (boolean? e1-v)
              (if e1-v (typed->expr (recur e2)) (typed->expr (recur e3)))
             `(if ,e1 ,(recur e2) ,(recur e3))))]
        [`(vector . ,e*)
         `(vector . ,(map recur e*))]
        [`(vector-ref ,ev ,ei)
          (let ([ev (recur ev)])
            (match ev
              [`(typed (vector . ,e*) ,_) 
                (typed->expr (list-ref e* (typed->expr ei)))]
              [_
               `(vector-ref ,ev ,ei)]))]
        [`(vector-set! ,ev ,ei ,e1)
         `(vector-set! ,(recur ev) ,ei ,(recur e1))]
        [`(lambda ,v* ,be)
         `(closure ,v* ,be ,senv)]
        [`(app ,e1 . ,e*)
          (let ([e1 (recur e1)] [e* (map recur e*)])
            (match e1 
              [`(typed (closure ,v* ,be ,lenv) ,_) 
                (typed->expr (@exp (env-add lenv (map make-pair v* (map typed->expr e*))) be))]
              [_
               `(app ,e1 . ,e*)]))]
        [_(@arg senv expr)]))))

(define @arg
  (lambda (senv arg)
    (match arg
      [(or (? fixnum?) (? boolean?)) arg]
      [(? symbol?) (env-ref senv arg (lambda () arg))]
      [(else) arg])))

(define closure->lambda
  (lambda (expr)
    (match expr
      [`(typed ,e ,t)
       `(typed ,(closure->lambda e) ,t)]
      [`(closure ,v* ,be ,_)
       `(lambda ,v* ,be)]
      [_(% expr)])))

(define val-exp?
  (match-lambda 
    [(or `(closure ,_ ,_ ,_) (? smp-exp?)) #t]
    [_ #f]))

;(require "interp.rkt")
;(require "parse.rkt")
;(test void ;MTR/interp
; `(,parse ,\@pgm)

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
;    (let ([x 10])
;      (let ([f (lambda () : Integer (+ x (+ 20 20)))])
;        (let ([x 30])
;          (f)))))
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
; `(program ()
;    (define (aaa [a1 : Integer]) : Integer
;      (+ (if (< (read) 10) 20 30) a1))
;    (vector 
;      (aaa (read))
;      (if (< (read) 40) 50 60)))
; )


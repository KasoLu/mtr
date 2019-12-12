#lang racket

(provide (rename-out [@pgm parse]))
(require "helper.rkt" racket/fixnum)

(define @pgm
  (match-lambda
    [`(program ,pi ,def* ... ,expr)
      (let ([tenv (map define->func.type def*)])
        (parameterize ([$define-record/cur '(main)])
         `(program ,pi ,@(map (curry @def tenv) def*)
            (define (main) ,(@exp tenv expr)))))]))

(define @def
  (lambda (tenv def)
    (match def
      [`(define (,f [,v* : ,t*]...) : ,rt ,e)
        (if (memq f ($define-record/cur))
          (report-error/dup f)
          (begin
            ($define-record/add f)
            (let*
              ([e (@exp (assoc-add tenv (map make-pair v* t*)) e)]
               [t (typed->type e)])
              (if (equal? rt (typed->type e))
               `(define (,f . ,v*) ,e)
                (report-error/typ `(define (,f . ,t*) : ,rt ,e))))))])))

(define @exp
  (lambda (tenv expr)
    (let ([recur (curry @exp tenv)])
      (match expr
        [`(read)
         `(typed ,expr Integer)]
        [`(void)
         `(typed ,expr Void)]
        [`(- ,e1)
          (let* ([e1 (recur e1)] [t1 (typed->type e1)])
            (if (eq? t1 'Integer)
             `(typed (- ,e1) Integer)
              (report-error/typ `(- ,t1))))]
        [`(,(? ath-op? op) ,e1 ,e2)
          (let* ([e* (map recur `(,e1 ,e2))] [t* (map typed->type e*)])
            (if (all-Integer? t*)
             `(typed (,op . ,e*) Integer)
              (report-error/typ `(,op . ,t*))))]
        [`(and ,e1 ,e2)
          (recur `(if ,e1 (if ,e2 #t #f) #f))]
        [`(or ,e1 ,e2)
          (recur `(if ,e1 #t (if ,e2 #t #f)))]
        [`(not ,e1)
          (let* ([e1 (recur e1)] [t1 (typed->type e1)])
            (if (eq? t1 'Boolean)
             `(typed (not ,e1) Boolean)
              (report-error/typ `(not ,t1))))]
        [`(eq? ,e1 ,e2)
          (let* ([e* (map recur `(,e1 ,e2))] [t* (map typed->type e*)])
            (if ((disjoin all-Integer? all-Boolean?) t*)
             `(typed (eq? . ,e*) Boolean)
              (report-error/typ `(eq? . ,t*))))]
        [`(,(? cmp-op? op) ,e1 ,e2)
          (let* ([e* (map recur `(,e1 ,e2))] [t* (map typed->type e*)])
            (if (all-Integer? t*)
             `(typed (,op . ,e*) Boolean)
              (report-error/typ `(,op . ,t*))))]
        [`(let ([,v ,e1]) ,e2)
          (let* 
            ([e1 (recur e1)] [t1 (typed->type e1)]
             [e2 (@exp (assoc-add tenv v t1) e2)] [t2 (typed->type e2)])
           `(typed (let ([,v ,e1]) ,e2) ,t2))]
        [`(if ,e1 ,e2 ,e3)
          (let*
            ([e1 (recur e1)] [t1 (typed->type e1)]
             [e2 (recur e2)] [t2 (typed->type e2)]
             [e3 (recur e3)] [t3 (typed->type e3)])
           (if (and (eq? t1 'Boolean) (equal? t2 t3))
            `(typed (if ,e1 ,e2 ,e3) ,t2)
             (report-error/typ `(if ,t1 ,t2 ,t3))))]
        [`(vector . ,e*)
          (let* ([e* (map recur e*)] [t* (map typed->type e*)])
           `(typed (vector . ,e*) (Vector . ,t*)))]
        [`(vector-ref ,ev ,(? fixnum? i))
          (let* ([ev (recur ev)] [tv (typed->type ev)])
            (match tv
              [`(Vector . ,t*)
                (if (< i (length t*))
                 `(typed (vector-ref ,ev ,i) ,(list-ref t* i))
                  (report-error/oor `(vector-ref ,tv ,i)))]
              [_(report-error/typ `(vector-ref ,tv ,i))]))]
        [`(vector-set! ,ev ,(? fixnum? i) ,e1)
          (let* 
            ([ev (recur ev)] [tv (typed->type ev)]
             [e1 (recur e1)] [t1 (typed->type e1)])
            (match tv
              [`(Vector . ,t*)
                (if (< i (length t*))
                 `(typed (vector-set! ,ev ,i ,e1) Void)
                  (report-error/oor `(vector-set! ,tv ,i ,t1)))]
              [_(report-error/typ `(vector-set! ,tv ,i ,t1))]))]
        [`(lambda ([,v* : ,t*]...) : ,rt ,e1)
          (let*
            ([e1 (@exp (assoc-add tenv (map make-pair v* t*)) e1)]
             [t1 (typed->type e1)])
            (if (equal? rt t1)
             `(typed (lambda ,v* ,e1) (,@t* -> ,rt))
              (report-error/typ `(lambda ,t* : ,rt ,t1))))]
        [`(,e1 . ,e*)
          (let* 
            ([e1 (recur e1)] [t1 (typed->type e1)]
             [e* (map recur e*)] [t* (map typed->type e*)])
            (match t1
              [`(,v-t* ... -> ,rt)
                (if (equal? v-t* t*)
                 `(typed (app ,e1 . ,e*) ,rt)
                  (report-error/typ `(,t1 . ,t*)))]
              [_(report-error/typ `(,t1 . ,t*))]))]
        [_(@arg tenv expr)]))))

(define @arg
  (lambda (tenv arg)
    (match arg
      [(? fixnum?)
      `(typed ,arg Integer)]
      [(? boolean?)
      `(typed ,arg Boolean)]
      [(? symbol?)
      `(typed ,arg ,(assoc-ref tenv arg))]
      [(else)
       (report-error/exp arg)])))

(define $define-record/cur 
  (make-parameter (void)))
(define $define-record/add
  (lambda (def)
    ($define-record/cur (cons def ($define-record/cur)))))

(define all-Integer?
  (curry andmap (curry eq? 'Integer)))

(define all-Boolean?
  (curry andmap (curry eq? 'Boolean)))

(define define->func.type
  (match-lambda
    [`(define (,f [,v* : ,t*]...) : ,rt ,e)
      (make-pair f `(,@t* -> ,rt))]))

(define report-error/exp
  (lambda (expr)
    (error 'parse "invalid expr: ~a" expr)))

(define report-error/typ
  (lambda (type)
    (error 'parse "invalid type: ~a" type)))

(define report-error/dup
  (lambda (def)
    (error 'parse "already defined: ~a" def)))

(define report-error/oor
  (lambda (expr)
    (error 'parse "index out of range: ~a" expr)))

;(require "interp.rkt")
;(test
; `([,identity ,MTR/interp ,identity]
;   [,\@pgm    ,MTR/interp ,identity])

; `(program () 42)
; `(program () #t)
; `(program () #f)
; `(program () (read))
; `(program () (void))
; `(program () (let ([x 42]) x))
; `(program () (if #t 42 24))
; `(program () (- 42))
; `(program () (+ 42 24))
; `(program () (- 42 24))
; `(program () (* 42 24))
; `(program () (/ 42 24))
; `(program () (and #t #f))
; `(program () (or #t #f))
; `(program () (not #t))
; `(program () (eq? 42 24))
; `(program () (eq? #t #f))
; `(program () (< 42 24))
; `(program () (> 42 24))
; `(program () (<= 42 24))
; `(program () (>= 42 24))
; `(program () (vector 42 #t))
; `(program () (vector-ref (vector 42 #t) 0))
; `(program () (vector-set! (vector 42 #t) 1 24))
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


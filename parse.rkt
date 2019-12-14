#lang racket

(provide (rename-out [ast:pgm parse]))
(require "helper.rkt" racket/fixnum)

(define ast:pgm
  (match-lambda
    [`(program ,pi ,def* ... ,expr)
      (let ([v.t* (map def->f.t def*)])
        (parameterize ([$def-rcd/cur '(main begin)])
         `(program ,pi ,@(map (curry ast:def v.t*) def*)
           ,(let-values ([(e t) (ast:exp v.t* expr)]) e))))]))

(define ast:def
  (lambda (v.t* def)
    (match def
      [`(define (,f [,v* : ,t*]...) : ,rt ,e)
        (if (memq f ($def-rcd/cur))
          (rpt-err/dup f)
          (begin
            ($def-rcd/add f)
            (let-values ([(e t) (ast:exp (assoc-add v.t* (map make-pair v* t*)) e)])
              (if (equal? rt t)
               `(define (,f . ,v*) ,e)
                (rpt-err/typ `(define (,f . ,t*) : ,rt ,t))))))])))

(define ast:exp
  (lambda (v.t* expr)
    (let ([recur (curry ast:exp v.t*)])
      (match expr
        [`(read) (values expr 'Integer)]
        [`(void) (values expr 'Void)]
        [`(- ,e1)
          (let-values ([(e1 t1) (recur e1)])
            (if (eq? t1 'Integer)
              (values `(- ,e1) 'Integer)
              (rpt-err/typ `(- ,t1))))]
        [`(,op ,e1 ,e2) #:when (ath-op? op)
          (let-values ([(e* t*) (map/values recur `(,e1 ,e2))])
            (if (all-Integer? t*)
              (values `(,op . ,e*) 'Integer)
              (rpt-err/typ `(,op . ,t*))))]
        [`(and ,e1 ,e2)
          (let-values ([(e* t*) (map/values recur `(,e1 ,e2))])
            (if (all-Boolean? t*)
              (match-let ([`(,e1 ,e2) e*])
                (values `(if ,e1 (if ,e2 #t #f) #f) 'Boolean))
              (rpt-err/typ `(and . ,t*))))]
        [`(or ,e1 ,e2)
          (let-values ([(e* t*) (map/values recur `(,e1 ,e2))])
            (if (all-Boolean? t*)
              (match-let ([`(,e1 ,e2) e*])
                (values `(if ,e1 #t (if ,e2 #t #f)) 'Boolean))
              (rpt-err/typ `(or . ,t*))))]
        [`(not ,e1)
          (let-values ([(e1 t1) (recur e1)])
            (if (eq? t1 'Boolean)
              (values `(not ,e1) 'Boolean)
              (rpt-err/typ `(not ,t1))))]
        [`(eq? ,e1 ,e2)
          (let-values ([(e* t*) (map/values recur `(,e1 ,e2))])
            (if ((disjoin all-Integer? all-Boolean?) t*)
              (values `(eq? . ,e*) 'Boolean)
              (rpt-err/typ `(eq? . ,t*))))]
        [`(,op ,e1 ,e2) #:when (cmp-op? op)
          (let-values ([(e* t*) (map/values recur `(,e1 ,e2))])
            (if (all-Integer? t*)
              (values `(,op . ,e*) 'Boolean)
              (rpt-err/typ `(,op . ,t*))))]
        [`(let ([,v ,ee]) ,eb)
          (let*-values
            ([(ee te) (recur ee)] [(eb tb) (ast:exp (assoc-add v.t* v te) eb)])
            (values `(let ([,v ,ee]) ,eb) tb))]
        [`(if ,e1 ,e2 ,e3)
          (let-values
            ([(e1 t1) (recur e1)]
             [(e2 t2) (recur e2)]
             [(e3 t3) (recur e3)])
            (if (and (eq? t1 'Boolean) (equal? t2 t3))
              (values `(if ,e1 ,e2 ,e3) t2)
              (rpt-err/typ `(if ,t1 ,t2 ,t3))))]
        [`(vector . ,e*)
          (let-values ([(e* t*) (map/values recur e* dft-res/val)])
            (let ([t `(Vector . ,t*)])
              (values `(vector . ,(map expr->typed e* t*)) t)))]
        [`(vector-ref ,ev ,(? fixnum? ei))
          (let-values ([(ev tv) (recur ev)])
            (match tv
              [`(Vector . ,t*)
                (if (< ei (length t*))
                  (values `(vector-ref ,ev ,ei) (list-ref t* ei))
                  (rpt-err/oor `(vector-ref ,tv 'Integer)))]
              [_(rpt-err/typ `(vector-ref ,tv 'Integer))]))]
        [`(vector-set! ,ev ,(? fixnum? ei) ,e1)
          (let-values ([(ev tv) (recur ev)] [(e1 t1) (recur e1)])
            (match tv
              [`(Vector . ,t*)
                (if (< ei (length t*))
                  (values `(vector-set! ,ev ,ei ,e1) 'Void)
                  (rpt-err/oor `(vector-set! ,tv ,ei ,t1)))]
              [_(rpt-err/typ `(vector-set! ,tv ,ei ,t1))]))]
        [`(lambda ([,v* : ,t*]...) : ,rt ,eb)
          (let-values ([(eb tb) (ast:exp (assoc-add v.t* (map make-pair v* t*)) eb)])
            (if (equal? rt tb)
              (values `(lambda ,v* ,eb) `(,@t* -> ,rt))
              (rpt-err/typ `(lambda ,t* : ,rt ,tb))))]
        [`(,e1 . ,e*)
          (let-values 
            ([(e1 t1) (recur e1)] [(e* t*) (map/values recur e* dft-res/val)])
            (match t1
              [`(,vt* ... -> ,rt)
                (if (equal? vt* t*)
                  (values `(app ,e1 . ,(map expr->typed e* t*)) rt)
                  (rpt-err/typ `(,t1 . ,t*)))]
              [_(rpt-err/typ `(,t1 . ,t*))]))]
        [_(ast:arg v.t* expr)]))))

(define ast:arg
  (lambda (v.t* arg)
    (match arg
      [(? fixnum?)
       (values arg 'Integer)]
      [(? boolean?)
       (values arg 'Boolean)]
      [(? symbol?)
       (let ([t (assoc-ref v.t* arg)])
         (values `(typed ,arg ,t) t))]
      [(else)
       (rpt-err/exp arg)])))

(define $def-rcd/cur
  (make-parameter (void)))

(define $def-rcd/add
  (lambda (def) 
    ($def-rcd/cur (cons def ($def-rcd/cur)))))

(define def->f.t
  (match-lambda
    [`(define (,f [,v* : ,t*]...) : ,rt ,e)
      (make-pair f `(,@t* -> ,rt))]))

(define expr->typed
  (lambda (expr type)
    (if (typed? expr) expr `(typed ,expr ,type))))

(define dft-res/val
  (lambda () (values '() '())))

(define all-Integer?
  (curry andmap (curry eq? 'Integer)))

(define all-Boolean?
  (curry andmap (curry eq? 'Boolean)))

(define rpt-err/exp
  (lambda (expr)
    (error 'parse "invalid expr: ~a" expr)))

(define rpt-err/typ
  (lambda (type)
    (error 'parse "invalid type: ~a" type)))

(define rpt-err/dup
  (lambda (def)
    (error 'parse "already defined: ~a" def)))

(define rpt-err/oor
  (lambda (expr)
    (error 'parse "index out of range: ~a" expr)))

;(require "interp.rkt")
;(test MTR/interp
; `(,identity ,ast:pgm)

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
; `(program () (vector (vector 1 2) (vector #t #f)))
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


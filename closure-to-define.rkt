#lang racket

(provide (rename-out [ast:pgm closure-to-define]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi . ,def+)
      (parameterize ([$def-rcd/cur (list)] [$fv*-rcd/cur (list)])
        (let ([f+ (map define->name def+)])
          (let ([def+ (map (curry ast:def f+) def+)])
           `(program ,pi ,@(append ($def-rcd/cur) def+)))))]))

(define ast:def
  (lambda (f+ def)
    (match def
      [`(define (,f . ,v*) ,e)
        (let ([clsp (gensym 'clsp)])
         `(define (,f ,clsp . ,v*)
           ,(ast:exp f+ v* clsp e)))])))

(define ast:exp
  (lambda (f+ v* clsp expr)
    (let ([recur (curry ast:exp f+ v* clsp)])
      (match expr
        [`(typed ,(? symbol? v) ,t)
          (if (memq v f+)
           `(vector (typed (fun-ref ,v) _))
            (let ([expr (if (proc-type? t) `(typed ,v (Vector ...)) expr)])
              (begin (unless (memq v v*) ($fv*-rcd/add expr)) expr)))]
        [`(lambda ,v* ,eb)
          (parameterize ([$fv*-rcd/cur (list)])
            (let ([lmd (gensym 'lmd)] [clsp (gensym 'clsp)])
              (let ([eb (ast:exp f+ v* clsp eb)] [fv* ($fv*-rcd/cur)])
                ($def-rcd/add
                 `(define (,lmd ,clsp . ,v*) ,(expand-fv* fv* clsp eb)))
               `(vector (typed (fun-ref ,lmd) _) . ,(reverse fv*)))))]
        [`(let ([,v ,ee]) ,eb)
         `(let ([,v ,(recur ee)]) ,(ast:exp f+ (cons v v*) clsp eb))]
        [`(app ,e1 . ,e*)
          (let ([fun (gensym 'fun)])
           `(let ([,fun ,(recur e1)])
              (app (vector-ref (typed ,fun (Vector ...)) 0)
                   (typed ,fun (Vector ...)) .
                  ,(map recur e*))))]
        [`(,op . ,e*)
         `(,op . ,(map recur e*))]
        [_(% expr)]))))

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
    (if (empty? fv*) expr
      (expand-fv* (cdr fv*) clsp
       `(let ([,(typed->expr (car fv*)) (vector-ref ,clsp ,(length fv*))]) 
         ,(% expr))))))


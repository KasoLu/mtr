#lang racket

(provide (rename-out [ast:pgm flatten-control]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi . ,def+)
     `(program ,pi . ,(map ast:def def+))]))

(define ast:def
  (match-lambda
    [`(define (,f . ,v*) ,fi ,e)
      (parameterize ([$tail-rcd/cur (make-assoc)])
       (let ([lb (symbol-append f '_start)] [tail (ast:exp e dft-cont)])
        `(define (,f . ,v*) ,fi
          ,(assoc-add ($tail-rcd/cur) lb tail))))]))

(define ast:exp
  (lambda (expr cont)
    (match expr
      [`(collect ,i)
       `(seq ,expr ,(cont `(void)))]
      [`(app ,e1 . ,e*)
        (cont `(call ,e1 . ,e*))]
      [`(let ([,v ,ee]) ,eb)
        (ast:exp ee (lambda (ee) `(seq (assign ,v ,ee) ,(ast:exp eb cont))))]
      [`(if ,e1 ,e2 ,e3)
        (let 
          ([ifv (gensym 'ifv)] [lbc (gensym 'L_)]
           [lb3 (gensym 'L_)] [lb2 (gensym 'L_)])
          ($tail-rcd/add lbc (cont ifv))
          ($tail-rcd/add lb3
            (ast:exp e3 (lambda (e3) `(seq (assign ,ifv ,e3) (goto ,lbc)))))
          ($tail-rcd/add lb2 
            (ast:exp e2 (lambda (e2) `(seq (assign ,ifv ,e2) (goto ,lbc)))))
          (ast:exp e1
            (match-lambda
              [(? cmp-exp? e1)
              `(if ,e1 (goto ,lb2) (goto ,lb3))]
              [(? smp-exp? e1)
              `(if (eq? ,e1 #t) (goto ,lb2) (goto ,lb3))]
              [(? any? e1)
               (let ([res (gensym 'res)])
                `(seq (assign ,res ,e1)
                      (if (eq? ,res #t) (goto ,lb2) (goto ,lb3))))])))]
      [_(cont expr)])))

(define $tail-rcd/cur 
  (make-parameter (void)))
(define $tail-rcd/add 
  (lambda (lb tail)
    ($tail-rcd/cur (assoc-add ($tail-rcd/cur) lb tail))))

(define dft-cont
  (match-lambda
    [`(call ,e1 . ,e*)
     `(tailcall ,e1 . ,e*)]
    [ (var e)
     `(return ,e)]))


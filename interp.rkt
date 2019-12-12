#lang racket

(provide (rename-out [MTR/pgm MTR/interp]))
(require "helper.rkt" racket/fixnum)

(define MTR/pgm
  (match-lambda
    [(or `(program ,pi ,def* ... (define (,_ ...) ,e))
         `(program ,pi ,def* ... ,e))
     (let ([env (map MTR/def def*)])
       (for ([k.v env]) (set-mcdr! k.v (set-closure-env (mcdr k.v) env)))
       (MTR/exp env e))]))

(define MTR/def
  (lambda (def)
    (match def
      [(or `(define (,f  [,v* : ,_] ...) : ,_ ,e)
           `(define (,f . ,v*)  ,_  ,e)
           `(define (,f . ,v*)  ,e))
       (mcons f `(closure ,v* ,e #f))])))

(define MTR/exp
  (lambda (env expr)
    (let ([recur (curry MTR/exp env)])
      (match expr
        [(or `(typed ,e ,t)) (recur e)]
        [(or `(read)) (read)]
        [(or `(void)) (void)]
        [(or `(,(? ath-op? op) . ,e*))
         (apply (ath-op->proc op) (map recur e*))]
        [(or `(not ,e1))
         (not (recur e1))]
        [(or `(and ,e1 ,e2))
         (if (recur e1) (if (recur e2) #t #f) #f)]
        [(or `(or ,e1 ,e2))
         (if (recur e1) #t (if (recur e2) #t #f))]
        [(or `(,(? cmp-op? op) . ,e*))
         (apply (cmp-op->proc op) (map recur e*))]
        [(or `(let ([,v ,e1]) ,e2))
         (MTR/exp (env-add env v (recur e1)) e2)]
        [(or `(if ,e1 ,e2 ,e3))
         (if (recur e1) (recur e2) (recur e3))]
        [(or `(vector . ,e*))
         (apply vector (map recur e*))]
        [(or `(vector-ref ,ev ,ei))
         (vector-ref (recur ev) (recur ei))]
        [(or `(vector-set! ,ev ,ei ,e1))
         (vector-set! (recur ev) (recur ei) (recur e1))]
        [(or `(lambda ([,v* : ,_]...) : ,_ ,e1)
             `(lambda ,v* ,e1))
        `(closure ,v* ,e1 ,env)]
        [(or `(app ,e1 . ,e*)
             `(,e1 . ,e*))
         (match (recur e1)
           [`(closure ,v* ,body ,senv)
             (MTR/exp (env-add senv (map make-pair v* (map recur e*))) body)])]
        [(else)
         (MTR/arg env expr)]
        ))))

(define MTR/arg
  (lambda (env arg)
    (match arg
      [(? fixnum?) arg]
      [(? boolean?) arg]
      [(? symbol?) (env-ref env arg)])))

(define ath-op->proc
  (match-lambda ['+ fx+] ['- fx-] ['* fx*] ['/ fxquotient]))

(define cmp-op->proc
  (match-lambda ['eq? eq?] ['< fx<] ['> fx>] ['<= fx<=] ['>= fx>=]))

(define env-cre
  (lambda () (list)))

(define env-ref
  (lambda (env key)
    (let loop ([env env])
      (if (empty? env)
        (error 'env "couldn't find '~a' in env" key)
        (match (car env)
          [(mcons k v)
           (if (eq? key k) v
             (loop (cdr env)))])))))

(define env-add
  (case-lambda
    [(env key val)
     (cons (mcons key val) env)]
    [(env ass)
     (for/fold ([env env]) ([k.v ass])
       (match k.v [`(,k ,v) (env-add env k v)]))]))

(define set-closure-env
  (lambda (closure nenv)
    (match closure
      [`(closure ,v* ,e ,senv)
       `(closure ,v* ,e ,nenv)])))


#lang racket

(module env-utils racket
  (provide (all-defined-out) (all-from-out "helper.rkt"))
  (require "helper.rkt")

  (define env$/cur (make-parameter (void)))
  (define env$/ref (lambda (k) (env-ref (env$/cur) k)))
  (define env$/add
    (case-lambda
      [(k v) (env$/cur (env-add (env$/cur) k v))]
      [(ass) (env$/cur (env-add (env$/cur) ass))]))

  (define env-cre
    (lambda () (list)))

  (define env-ref
    (lambda (env key [handle #f])
      (let loop ([env env])
        (if (empty? env)
          (if (not handle)
            (error 'env "couldn't find '~a' in env" key)
            (handle))
          (match (car env)
            [(mcons k v) 
             (if (eq? key k) v (loop (cdr env)))])))))

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
  )

(module MS racket
  (provide (rename-out [itp:pgm MS:interp]))
  (require (submod ".." env-utils))

  (define itp:pgm
    (match-lambda
      [(or `(program ,pi ,def* ... (define (,_ ...) ,_ ,e)) 
           `(program ,pi ,def* ... (define (,_ ...) ,e))
           `(program ,pi ,def* ... ,e))
       (let ([env (map itp:def def*)])
         (for ([k.v env]) (set-mcdr! k.v (set-closure-env (mcdr k.v) env)))
         (parameterize ([env$/cur env])
           (itp:exp e)))]))

  (define itp:def
    (match-lambda
      [(or `(define (,f  [,v* : ,_] ...) : ,_ ,e)
           `(define (,f . ,v*)  ,_  ,e)
           `(define (,f . ,v*)  ,e))
       (mcons f `(closure ,v* ,e #f))]))

  (define itp:exp
    (lambda (expr)
      (match expr
        [(or `(typed ,e ,t))
         (itp:exp e)]
        [(or `(read))
         (read)]
        [(or `(void))
         (void)]
        [(or `(,op . ,e*)) #:when ((disjoin ath-op? cmp-op?) op)
         (let ([e* (map itp:exp e*)])
           (cond
             [(ath-op? op) (apply (ath-op->proc op) e*)]
             [(cmp-op? op) (apply (cmp-op->proc op) e*)]))]
        [(or `(not ,e1))
         (not (itp:exp e1))]
        [(or `(and ,e1 ,e2))
         (if (itp:exp e1) (if (itp:exp e2) #t #f) #f)]
        [(or `(or ,e1 ,e2))
         (if (itp:exp e1) #t (if (itp:exp e2) #t #f))]
        [(or `(let ([,v ,e1]) ,e2))
         (let ([e1 (itp:exp e1)])
           (if (eq? v '_)
             (itp:exp e2)
             (parameterize ([env$/cur (env-add (env$/cur) v e1)])
               (itp:exp e2))))]
        [(or `(if ,e1 ,e2 ,e3))
         (if (itp:exp e1) (itp:exp e2) (itp:exp e3))]
        [(or `(vector . ,e*) `(vector-tag ,_ . ,e*))
         (apply vector (map itp:exp e*))]
        [(or `(vector-ref ,ev ,ei))
         (vector-ref (itp:exp ev) (itp:exp ei))]
        [(or `(vector-set! ,ev ,ei ,e1))
         (vector-set! (itp:exp ev) (itp:exp ei) (itp:exp e1))]
        [(or `(lambda ([,v* : ,_]...) : ,_ ,e1)
             `(lambda ,v* ,e1))
         `(closure ,v* ,e1 ,(env$/cur))]
        [(or `(global-value ,v))
         (% 0)]
        [(or `(collect ,i))
         (void)]
        [(or `(allocate ,len ,tag))
         (make-vector len)]
        [(or `(fun-ref ,l))
         (env$/ref l)]
        [(or `(app ,e1 . ,e*)
             `(,e1 . ,e*))
         (let ([e* (map itp:exp e*)])
           (match (itp:exp e1)
             [`(closure ,v* ,body ,senv)
               (parameterize ([env$/cur (env-add senv (map make-pair v* e*))])
                 (itp:exp body))]))]
        [(else)
         (itp:arg expr)])))

  (define itp:arg
    (lambda (args)
      (match args
        [(or (? fixnum?) (? boolean?)) args]
        [(or (? symbol? v)) (env$/ref v)])))
  )

(module MC racket
  (provide (rename-out [itp:pgm MC:interp]))
  (require (submod ".." env-utils))

  (define itp:pgm
    (match-lambda
      [`(program ,pi . ,def+)
        (parameterize ([env$/cur (map itp:def def+)])
          (itp:tail `(tailcall ,(def+->entry def+) #f)))]))

  (define itp:def
    (match-lambda
      [`(define (,f . ,v*) ,fi ,lb.tail*)
        (mcons f `(proc ,f ,v* ,lb.tail*))]))
  
  (define itp:tail
    (lambda (tail)
      (match tail
        [`(return ,e)
          (itp:exp e)]
        [`(seq ,s ,t)
          (itp:stmt s)
          (itp:tail t)]
        [`(goto ,l)
          (itp:tail (env$/ref l))]
        [`(if (,op ,a1 ,a2) (goto ,l1) (goto ,l2))
          (if ((cmp-op->proc op) (itp:arg a1) (itp:arg a2))
            (itp:tail (env$/ref l1))
            (itp:tail (env$/ref l2)))]
        [`(tailcall ,a1 . ,a*)
          (match (itp:arg a1)
            [`(proc ,f ,v* ,lb.tail*)
              (let* 
                ([nenv (env-add (env$/cur) lb.tail*)]
                 [nenv (env-add (% nenv) (map make-pair v* (map itp:arg a*)))])
                (parameterize ([env$/cur nenv])
                  (itp:tail (assoc-ref lb.tail* (symbol-append f '_start)))))])])))

  (define itp:stmt
    (lambda (stmt)
      (match stmt
        [`(assign ,v ,e)
          (let ([e (itp:exp e)])
            (unless (eq? v '_) (env$/add v e)))]
        [`(collect ,i)
          (void)])))

  (define itp:exp
    (lambda (expr)
      (match expr
        [`(read)
          (read)]
        [`(void)
          (void)]
        [`(fun-ref ,v)
          (env$/ref v)]
        [`(,op . ,a*) #:when ((disjoin ath-op? lgc-op? cmp-op?) op)
          (let ([a* (map itp:arg a*)])
            (cond
              [(ath-op? op) (apply (ath-op->proc op) a*)]
              [(lgc-op? op) (apply (lgc-op->proc op) a*)]
              [(cmp-op? op) (apply (cmp-op->proc op) a*)]))]
        [`(allocate ,i ,t)
          (make-vector i)]
        [`(vector-ref ,a ,i)
          (vector-ref (itp:arg a) i)]
        [`(vector-set! ,v ,i ,a)
          (vector-set! (itp:arg v) i (itp:arg a))]
        [`(call ,a1 . ,a*)
          (itp:tail `(tailcall ,a1 . ,a*))]
        [_(itp:arg expr)])))

  (define itp:arg
    (lambda (arg)
      (match arg
        [(or (? fixnum?) (? boolean?)) arg]
        [(or (? symbol? v)) (env$/ref v)]
        [(or`(global-value ,_)) 0])))

  (define def+->entry
    (match-lambda
      [`(,_ ... (define (,f . ,_) ,_ ,_)) f]))
  )


(require 'MS 'MC)
(provide (all-from-out 'MS 'MC))


#lang racket

(module MR racket
  (provide (rename-out [itp:pgm MR:interp]))
  (require "helper.rkt")

  (define itp:pgm
    (match-lambda
      [(or `(program ,pi ,def* ... (define (,_ ...) ,_ ,e))
           `(program ,pi ,def* ... (define (,_ ...) ,e))
           `(program ,pi ,def* ... ,e))
       (let ([env (map def->f.c def*)])
         (for ([k.v env])
           (set-mcdr! k.v 
             (match (mcdr k.v)
               [`(closure ,v* ,e ,_)
                `(closure ,v* ,e ,env)])))
         (itp:exp env e identity))]))

  (define itp:exp
    (lambda (env expr cont)
      (let ([rec (curry itp:exp env)])
        (match expr
          [(or `(typed ,e ,t))
           (rec e cont)]
          [(or `(read))
           (cont (read))]
          [(or `(void))
           (cont (void))]
          [(or `(,op . ,e*)) #:when ((disjoin ath-op? cmp-op?) op)
           (cps-map rec e*
             (lambda (v*)
               (cond
                 [(ath-op? op) (cont (apply (ath-op->proc op) v*))]
                 [(cmp-op? op) (cont (apply (cmp-op->proc op) v*))])))]
          [(or `(not ,e1))
           (rec e1 (lambda (v1) (cont (not v1))))]
          [(or `(and ,e1 ,e2))
           (rec e1 (lambda (v1) (if v1 (rec e2 cont) (cont #f))))]
          [(or `(or ,e1 ,e2))
           (rec e1 (lambda (v1) (if v1 (cont #t) (rec e2 cont))))]
          [(or `(let ([,re ,ee]) ,eb))
           (rec ee (lambda (ve) (itp:exp (env-add env re ve) eb cont)))]
          [(or `(if ,e1 ,e2 ,e3))
           (rec e1 (lambda (v1) (if v1 (rec e2 cont) (rec e3 cont))))]
          [(or `(vector . ,e*) `(vector-tag ,_ . ,e*))
           (cps-map rec e* (lambda (v*) (cont (list->vector v*))))]
          [(or `(vector-ref ,ev ,ei))
           (cps-map rec `(,ev ,ei) (lambda (v*) (cont (apply vector-ref v*))))]
          [(or `(vector-set! ,ev ,ei ,e1))
           (cps-map rec `(,ev ,ei ,e1) (lambda (v*) (cont (apply vector-set! v*))))]
          [(or `(lambda ([,r* : ,_]...) : ,_ ,e1)
               `(lambda ,r* ,e1))
           (cont `(closure ,r* ,e1 ,env))]
          [(or `(global-value ,v))
           (cont 0)]
          [(or `(collect ,i))
           (cont (void))]
          [(or `(allocate ,len ,tag))
           (cont (make-vector len))]
          [(or `(fun-ref ,l))
           (cont (env-ref env l))]
          [(or `(app ,e1 . ,e*)
               `(,e1 . ,e*))
           (cps-map rec `(,e1 . ,e*)
             (lambda/match (`(,v1 . ,v*))
               (match v1
                 [`(closure ,r* ,body ,senv)
                   (let ([nenv (env-add senv (map make-pair r* v*))])
                     (itp:exp nenv body cont))])))]
          [(else)
           (itp:arg env expr cont)]))
      
      ))

  (define itp:arg
    (lambda (env arg cont)
      (match arg
        [(or (? fixnum?) (? boolean?))
         (cont arg)]
        [(or (? symbol?))
         (cont (env-ref env arg))])))

  (define def->f.c
    (match-lambda
      [(or `(define (,f  [,v* : ,_] ...) : ,_ ,e)
           `(define (,f . ,v*)  ,_  ,e)
           `(define (,f . ,v*)  ,e))
       (mcons f `(closure ,v* ,e #f))]))

  )

(module MC racket
  (provide (rename-out [itp:pgm MC:interp]))
  (require "helper.rkt")

  (define itp:pgm
    (match-lambda
      [`(program ,pi . ,def+)
        (let ([env (map def->f.p def+)])
          (itp:tail env `(tailcall ,(assoc-ref pi 'entry) #f) identity))]))
  
  (define itp:tail
    (lambda (env tail cont)
      (let ([rec-arg (curry itp:arg env)])
        (match tail
          [`(return ,e)
            (itp:exp env e cont)]
          [`(seq ,s ,t)
            (itp:stmt env s (lambda (env) (itp:tail env t cont)))]
          [`(goto ,l)
            (itp:tail env (env-ref env l) cont)]
          [`(if (,cmp ,a1 ,a2) ,go-l1 ,go-l2)
            (cps-map rec-arg `(,a1 ,a2)
              (lambda (v*)
                (if (apply (cmp-op->proc cmp) v*)
                  (itp:tail env go-l1 cont)
                  (itp:tail env go-l2 cont))))]
          [`(tailcall ,a1 . ,a*)
            (cps-map rec-arg `(,a1 . ,a*)
              (lambda/match (`(,v1 . ,v*))
                (match v1
                  [`(proc ,f ,r* ,lb.tail*)
                    (let ([nenv (env-con env lb.tail* (map make-pair r* v*))])
                      (let ([entry (assoc-ref lb.tail* (symbol-append f '_start))])
                        (itp:tail nenv entry cont)))])))]))))

  (define itp:stmt
    (lambda (env stmt cont)
      (match stmt
        [`(assign ,r ,e)
          (itp:exp env e (lambda (v) (cont (env-add env r v))))]
        [`(collect ,i)
          (cont env)])))

  (define itp:exp
    (lambda (env expr cont)
      (let ([rec-arg (curry itp:arg env)])
        (match expr
          [`(read)
            (cont (read))]
          [`(void)
            (cont (void))]
          [`(fun-ref ,v)
            (cont (env-ref env v))]
          [`(,op . ,a*) #:when ((disjoin ath-op? lgc-op? cmp-op?) op)
            (cps-map rec-arg a*
              (lambda (v*)
                (cond
                  [(ath-op? op) (cont (apply (ath-op->proc op) v*))]
                  [(lgc-op? op) (cont (apply (lgc-op->proc op) v*))]
                  [(cmp-op? op) (cont (apply (cmp-op->proc op) v*))])))]
          [`(allocate ,i ,t)
            (cont (make-vector i))]
          [`(vector-ref ,a ,i)
            (cps-map rec-arg `(,a ,i)
              (lambda (v*) (cont (apply vector-ref v*))))]
          [`(vector-set! ,v ,i ,a)
            (cps-map rec-arg `(,v ,i ,a)
              (lambda (v*) (cont (apply vector-set! v*))))]
          [`(call ,a1 . ,a*)
            (cps-map rec-arg `(,a1 . ,a*)
              (lambda/match (`((proc ,f ,r* ,lb.tail*) . ,v*))
                (let ([nenv (env-con env lb.tail* (map make-pair r* v*))])
                  (let ([entry (assoc-ref lb.tail* (symbol-append f '_start))])
                    (itp:tail nenv entry (lambda (v) (cont v)))))))]
          [_(itp:arg env expr cont)]))))

  (define itp:arg
    (lambda (env arg cont)
      (match arg
        [(or (? fixnum?) (? boolean?))
         (cont arg)]
        [(or (? symbol? v))
         (cont (env-ref env v))]
        [(or`(global-value ,_))
         (cont 0)])))

  (define def->f.p
    (match-lambda
      [`(define (,f . ,v*) ,fi ,lb.tail*)
        (mcons f `(proc ,f ,v* ,lb.tail*))]))

  )

(module MA racket
  (provide (rename-out [itp:pgm MA:interp]))
  (require "helper.rkt" racket/fixnum)

  (define itp:pgm
    (match-lambda
      [`(program ,pi . ,def+)
        (let ([bgn `(callq ,(assoc-ref pi 'entry))])
          (let loop ([env (def+->env def+)] [ins+ (list bgn)] [stk (list)])
            (itp:ins env ins+ stk
              (lambda (env ins+ stk)
                (cond
                  [(not (empty? ins+))
                   (loop env ins+ stk)]
                  [(not (empty? stk))
                   (loop env (car stk) (cdr stk))]
                  [(owise)
                   (env-ref env `(reg rax) (lambda () 0))])))))]))
  
  (define itp:ins
    (lambda (env ins+ stk cont)
      (match (car ins+)
        [`(,op ,a1 ,a2) #:when (set-member? '(addq subq) op)
          (cps-map itp:arg env `(,a1 ,a2)
            (lambda/match (env `(,v1 ,v2))
              (set-mcdr! v2 ((ath-ins->proc op) (mcdr v1) (mcdr v2)))
              (cont env (cdr ins+) stk)))]
        [`(,op ,a1) #:when (set-member? '(imulq idivq) op)
          (cps-map itp:arg env `(,a1 (reg rax))
            (lambda/match (env `(,v1 ,v2))
              (set-mcdr! v2 ((ath-ins->proc op) (mcdr v1) (mcdr v2)))
              (cont env (cdr ins+) stk)))]
        [`(negq ,a1)
          (itp:arg env a1
            (lambda (env v1)
              (set-mcdr! v1 (- (mcdr v1)))
              (cont env (cdr ins+) stk)))]
        [ (or `(movq ,a1 ,a2) `(movzbq ,a1 ,a2))
          (cps-map itp:arg env `(,a1 ,a2)
            (lambda/match (env `(,v1 ,v2))
              (set-mcdr! v2 (mcdr v1))
              (cont env (cdr ins+) stk)))]
        [`(callq ,l)
          (cond
            [(eq? l 'collect)
             (cont env (cdr ins+) stk)]
            [(eq? l 'read_int)
             (itp:arg env `(reg rax)
               (lambda (env vax)
                 (set-mcdr! vax (read))
                 (cont env (cdr ins+) stk)))]
            [(owise)
             (cont env (env-ref env (symbol-append l '_pre)) (cons (cdr ins+) stk))])]
        [`(retq)
          (cont env (car stk) (cdr stk))]
        [`(pushq ,a1)
          (cps-map itp:arg env `(,a1 (reg rsp))
            (lambda/match (env `(,v1 ,vsp))
              (set-mcdr! vsp (- (mcdr vsp) 8))
              (itp:arg env `(adr ,(mcdr vsp))
                (lambda (env vdr)
                  (set-mcdr! vdr (mcdr v1))
                  (cont env (cdr ins+) stk)))))]
        [`(popq ,a1)
          (cps-map itp:arg env `(,a1 (reg rsp))
            (lambda/match (env `(,v1 ,vsp))
              (itp:arg env `(adr ,(mcdr vsp))
                (lambda (env vdr)
                  (set-mcdr! v1  (mcdr vdr))
                  (set-mcdr! vsp (+ (mcdr vsp) 8))
                  (cont env (cdr ins+) stk)))))]
        [`(xorq ,a1 ,a2)
          (cps-map itp:arg env `(,a1 ,a2)
            (lambda/match (env `(,v1 ,v2))
              (set-mcdr! v2 (fxxor (mcdr v1) (mcdr v2)))
              (cont env (cdr ins+) stk)))]
        [`(cmpq ,a1 ,a2)
          (cps-map itp:arg env `(,a1 ,a2)
            (lambda/match (env `(,v1 ,v2))
              (let ([v1/v (mcdr v1)] [v2/v (mcdr v2)])
                (cps-map itp:arg env `((cc l) (cc le) (cc g) (cc ge) (cc e))
                  (lambda/match (env `(,vl ,vle ,vg ,vge ,ve))
                    (set-mcdr! vl  (if (fx<  v2/v v1/v) 1 0))
                    (set-mcdr! vle (if (fx<= v2/v v1/v) 1 0))
                    (set-mcdr! vg  (if (fx>  v2/v v1/v) 1 0))
                    (set-mcdr! vge (if (fx>= v2/v v1/v) 1 0))
                    (set-mcdr! ve  (if (fx=  v2/v v1/v) 1 0))
                    (cont env (cdr ins+) stk))))))]
        [`(set ,cc ,a1)
          (cps-map itp:arg env `((cc ,cc) ,a1)
            (lambda/match (env `(,vc ,v1))
              (set-mcdr! v1 (mcdr vc))
              (cont env (cdr ins+) stk)))]
        [`(jmp ,l)
          (cont env (env-ref env l) stk)]
        [`(jmp-if ,cc ,l)
          (if (zero? (env-ref env `(cc ,cc)))
            (cont env (cdr ins+) stk)
            (cont env (env-ref env l) stk))]
        [`(jmp-post ,post)
          (cont env (car stk) (cdr stk))]
        [`(indirect-callq ,a1)
          (itp:arg env a1
            (lambda (env v1)
              (cont env (mcdr v1) (cons (cdr ins+) stk))))]
        [`(tail-jmp ,a1)
          (itp:arg env a1
            (lambda (env v1)
              (cont env (mcdr v1) stk)))]
        [`(leaq (fun-ref ,l) ,a2)
          (itp:arg env a2
            (lambda (env v2)
              (set-mcdr! v2 (env-ref env (symbol-append l '_pre)))
              (cont env (cdr ins+) stk)))])))

  (define itp:arg
    (lambda (env arg cont)
      (match arg
        [`(int ,int)
          (cont env (mcons arg int))]
        [`(deref ,reg ,ofs)
          (itp:arg env `(reg ,reg)
            (lambda (env veg)
              (itp:arg env `(adr ,(+ (mcdr veg) ofs)) cont)))]
        [_
          (let ([fnd (env-ass env arg (lambda () #f))])
            (if (not fnd)
              (let ([fnd (mcons arg 0)]) (cont (env-add env fnd) fnd))
              (cont env fnd)))])))

  (define def+->env
    (lambda (def+)
      (let ([stk-top 16384])
        (apply env-con (env-cre)
          (mcons '(reg rsp) (sub1 stk-top))
          (mcons  (% g-fp)  (% 0))
          (mcons  (% g-fe)  (% stk-top))
          (for/fold ([ass (env-cre)]) ([def def+])
            (match def
              [`(define (,f) ,fi ([,l+ (block ,bi . ,i++)]...))
                (for/fold ([ass ass]) ([l l+] [i+ i++])
                  (env-add ass l i+))]))))))

  )

(require 'MR 'MC 'MA)
(provide (all-from-out 'MR 'MC 'MA))


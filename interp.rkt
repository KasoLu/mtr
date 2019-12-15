#lang racket

(require racket/fixnum "helper.rkt")

(module+ MTR
  (provide (rename-out [itp:prgm MTR:interp]))
  (define itp:prgm
    (match-lambda
      [(or `(program ,pi ,defn* ... (define (,_ ...) ,_ ,e)) 
           `(program ,pi ,defn* ... (define (,_ ...) ,e))
           `(program ,pi ,defn* ... ,e))
       (let ([venv (map itp:defn defn*)])
         (for ([k.v venv]) (set-mcdr! k.v (set-closure-env (mcdr k.v) venv)))
         (itp:expr venv e))]))

  (define itp:defn
    (match-lambda
      [(or `(define (,f  [,v* : ,_] ...) : ,_ ,e)
           `(define (,f . ,v*)  ,_  ,e)
           `(define (,f . ,v*)  ,e))
       (mcons f `(closure ,v* ,e #f))]))

  (define itp:expr
    (lambda (venv expr)
      (let ([recur (curry itp:expr venv)])
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
           (let ([e1 (recur e1)])
             (if (eq? v '_)
               (recur e2)
               (itp:expr (env-add venv v e1) e2)))]
          [(or `(if ,e1 ,e2 ,e3))
           (if (recur e1) (recur e2) (recur e3))]
          [(or `(vector . ,e*) `(vector-tag ,_ . ,e*))
           (apply vector (map recur e*))]
          [(or `(vector-ref ,ev ,ei))
           (vector-ref (recur ev) (recur ei))]
          [(or `(vector-set! ,ev ,ei ,e1))
           (vector-set! (recur ev) (recur ei) (recur e1))]
          [(or `(lambda ([,v* : ,_]...) : ,_ ,e1)
               `(lambda ,v* ,e1))
           `(closure ,v* ,e1 ,venv)]
          [(or `(global-value ,v))
           (% 0)]
          [(or `(collect ,i))
           (void)]
          [(or `(allocate ,len ,tag))
           (make-vector len)]
          [(or `(fun-ref ,l))
           (env-ref venv l)]
          [(or `(app ,e1 . ,e*)
               `(,e1 . ,e*))
           (match (recur e1)
             [`(closure ,v* ,body ,senv)
               (itp:expr (env-add senv (map make-pair v* (map recur e*))) body)])]
          [(else)
           (itp:args venv expr)]))))

  (define itp:args
    (lambda (venv args)
      (match args
        [(or (? fixnum?) (? boolean?)) args]
        [(or (? symbol? v)) (env-ref venv v)]))))

(module+ MC
  (provide (rename-out [itp:prgm MC:interp]))

  (define itp:prgm
    (match-lambda
      [`(program ,pi . ,def+)
        (parameterize ([$venv/cur (map itp:defn def+)])
          (itp:tail `(tailcall (fun-ref ,(defn+->entry def+)) #f)))]))

  (define itp:defn
    (match-lambda
      [`(define (,f . ,v*) ,fi ,lb.tail*)
        (mcons f `(proc ,f ,v* ,lb.tail*))]))
  
  (define itp:tail
    (lambda (tail)
      (match tail
        [`(return ,e)
          (itp:expr e)]
        [`(seq ,s ,t)
          (itp:stmt s)
          (itp:tail t)]
        [`(goto ,l)
          (itp:tail ($venv/ref l))]
        [`(if (,op ,a1 ,a2) (goto ,l1) (goto ,l2))
          (if ((cmp-op->proc op) (itp:args a1) (itp:args a2))
            (itp:tail ($venv/ref l1))
            (itp:tail ($venv/ref l2)))]
        [`(tailcall ,a1 . ,a*)
          (match (itp:args a1)
            [`(proc ,f ,v* ,lb.tail*)
              (let* 
                ([nenv (env-add ($venv/cur) lb.tail*)]
                 [nenv (env-add (% nenv) (map make-pair v* (map itp:args a*)))])
                (parameterize ([$venv/cur nenv])
                  (itp:tail (assoc-ref lb.tail* (symbol-append f '_start)))))])])))

  (define itp:stmt
    (lambda (stmt)
      (match stmt
        [`(assign ,v ,e)
          (let ([e (itp:expr e)])
            (unless (eq? v '_) ($venv/add v e)))]
        [`(collect ,i)
          (void)])))

  (define itp:expr
    (lambda (expr)
      (match expr
        [`(read) (read)]
        [`(void) (void)]
        [`(,op . ,a*) #:when (ath-op? op)
          (apply (ath-op->proc op) (map itp:args a*))]
        [`(,op . ,a*) #:when (lgc-op? op)
          (apply (lgc-op->proc op) (map itp:args a*))]
        [`(,op . ,a*) #:when (cmp-op? op)
          (apply (cmp-op->proc op) (map itp:args a*))]
        [`(allocate ,i ,t)
          (make-vector i)]
        [`(vector-ref ,a ,i)
          (vector-ref (itp:args a) i)]
        [`(vector-set! ,v ,i ,a)
          (vector-set! (itp:args v) i (itp:args a))]
        [`(call ,a1 . ,a*)
          (itp:tail `(tailcall ,a1 . ,a*))]
        [_(itp:args expr)])))

  (define itp:args
    (lambda (args)
      (match args
        [(or (? fixnum?) (? boolean?)) args]
        [(or `(fun-ref ,v) (? symbol? v)) ($venv/ref v)]
        [(or `(global-value ,_)) 0])))

  (define $venv/cur (make-parameter (void)))
  (define $venv/ref (lambda (k) (env-ref ($venv/cur) k)))
  (define $venv/add 
    (case-lambda
      [(k v) ($venv/cur (env-add ($venv/cur) k v))]
      [(ass) ($venv/cur (env-add ($venv/cur) ass))]))

  (define defn+->entry
    (match-lambda
      [`(,_ ... (define (,f . ,_) ,_ ,_)) f]))
  )

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


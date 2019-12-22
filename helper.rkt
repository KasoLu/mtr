#lang racket

(provide (all-defined-out) %)
(require racket/control racket/fixnum racket/trace)

;; ---- language ---- ;;
(define g-fp
 '(global-value free_ptr))

(define g-fe
 '(global-value fromspace_end))

(define reg?
  (curry set-member? 
    `(rsp rbp rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)))

(define glb?
  (lambda (v)
    (match? `(global-value ,_) v)))

(define ath-op '(+ - * /))
(define ath-op? (curry set-member? ath-op))

(define cmp-op '(eq? < <= > >=))
(define cmp-op? (curry set-member? cmp-op))

(define lgc-op '(and or not))
(define lgc-op? (curry set-member? lgc-op))

(define call-reg*
 `(rdi rsi rdx rcx r8 r9))

(define typed->type
  (match-lambda
    [`(typed ,expr ,type) type]))

(define typed->expr
  (match-lambda
    [`(typed ,expr ,type) expr]))

(define typed?
  (lambda (expr)
    (match? `(typed ,_ ,_) expr)))

(define define->name
  (match-lambda
    [`(define (,f . ,p*) : ,rt ,e) f]
    [`(define (,f . ,p*) ,fi ,e) f]
    [`(define (,f . ,p*) ,e) f]))

(define proc-type?
  (lambda (type)
    (match? `(,_ ... -> ,_) type)))

(define vector-type?
  (lambda (type)
    (match? `(Vector . ,_) type)))

(define smp-exp?
  (match-lambda
    [`(global-value ,_) #t]
    [ (or (? integer?) (? boolean?) (? symbol?)) #t]
    [_(% #f)]))

(define cmp-exp?
  (match-lambda [`(,(? cmp-exp?) ,_ ,_) #t] [_ #f]))

(define ath-op->proc
  (match-lambda ['+ fx+] ['- fx-] ['* fx*] ['/ fxquotient]))

(define ath-ins->proc
  (match-lambda ['addq fx+] ['subq fx-] ['imulq fx*] ['idivq fxquotient]))

(define lgc-op->proc
  (match-lambda ['not not]))

(define cmp-op->proc
  (match-lambda ['eq? eq?] ['< fx<] ['> fx>] ['<= fx<=] ['>= fx>=]))

;; ----- utils ----- ;;
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
           (if (equal? key k) v (loop (cdr env)))])))))

(define env-ass
  (lambda (env key [handle #f])
    (let loop ([env env])
      (if (empty? env)
        (if (not handle)
          (error 'env "couldn't find '~a' in env" key)
          (handle))
        (let ([mpair (car env)])
          (match mpair
            [(mcons k v) 
             (if (equal? key k) mpair (loop (cdr env)))]))))))

(define env-add
  (case-lambda
    [(env key val)
     (cons (mcons key val) env)]
    [(env ass)
     (if (mpair? ass)
       (cons ass env)
       (for/fold ([env env]) ([k.v ass])
         (match k.v [`(,k ,v) (env-add env k v)])))]))

(define env-con
  (lambda (env . ass*)
    (for/fold ([env env]) ([ass ass*])
      (env-add env ass))))

(define make-assoc
  (lambda () (list)))

(define assoc-ref
  (lambda (ass key [handle #f])
    (let ([found (assoc key ass)])
      (if (pair? found)
        (pair->val found)
        (if (not handle)
          (error 'assoc-ref "not found ~a" key)
          (handle))))))

(define assoc-add
  (case-lambda
    [(ass key val)
     (cons `(,key ,val) (assoc-del ass key))]
    [(ass ass-new)
     (append ass-new
       (foldl (lambda (k.v ass) (assoc-del ass (pair->key k.v))) ass ass-new))]))

(define assoc-del
  (lambda (ass key)
    (filter (lambda (k.v) (not (eq? key (pair->key k.v)))) ass)))

(define make-pair
  (lambda (key val) (list key val)))

(define pair->key
  (lambda (k.v) (first k.v)))

(define pair->val
  (lambda (k.v) (second k.v)))

(define symbol-append
  (lambda (sym1 sym2)
    (string->symbol
      (string-append
        (symbol->string sym1)
        (symbol->string sym2)))))

(define map/reverse
  (lambda (proc . ls*)
    (if (empty? ls*) (list)
      (let loop ([z* (apply map list ls*)] [acc (list)])
        (if (empty? z*) acc
          (loop (cdr z*) (cons (apply proc (car z*)) acc)))))))

(define map/values
  (lambda (proc ls [handle #f])
    (cond
      [(not (empty? ls))
       (let ([wrap (lambda (e) (call-with-values (lambda () (proc e)) list))])
         (apply values (apply map list (map wrap ls))))]
      [(not handle) (error 'map/values "list is empty")]
      [(owise) (handle)])))

(define any?
  (lambda (any) #t))

(define cps-map
  (match-lambda*
    [`(,proc ,a* ... ,e* ,cont)
      (let ([a*-len (length a*)])
        (let loop ([e* e*] [a* a*] [v* (list)])
          (if (empty? e*)
            (apply (apply (curry cont) a*) (apply map list (reverse v*)))
            (curry (apply (curry proc) a*) (car e*)
              (lambda r*
                (loop (cdr e*) (take r* a*-len) (cons (drop r* a*-len) v*)))))))]))

;; ----- syntax ----- ;;
(define-syntax lambda/match
  (syntax-rules ()
    [(_ pat body ...)
     (match-lambda** [pat body ...])]))

(define-syntax apply/values
  (syntax-rules ()
    [(_ func vals)
     (call-with-values (lambda () vals) func)]))

(define-syntax match?
  (syntax-rules ()
    [(_ pat val) (match val [pat #t] [_ #f])]))

(define-match-expander else
  (lambda (stx)
    (syntax-case stx ()
      [(_) #'(var _)])))

(define-syntax owise
  (syntax-rules ()
    [(_) #t]))

;; ----- test ----- ;;
(define test
  (lambda (pass* . case*)
    (for ([kase case*])
      (newline)
      (pretty-display
        (let loop ([res '()] [kase kase] [pass* pass*])
          (if (empty? pass*)
           `((execute ,res) ,kase)
            (match-let* ([`(,pass ,interp) (car pass*)] [kase (pass kase)])
              (loop (cons (interp kase) res) kase (cdr pass*)))))))))


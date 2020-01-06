#lang racket

(provide (all-defined-out) %)
(require racket/control racket/fixnum racket/trace racket/hash)

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

(define void-type?
  (curry eq? 'Void))

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

(define type-anoc
  (lambda (var type) `(,var : ,type)))

;; ----- utils ----- ;;
(define make-pair cons)
(define pair->key car)
(define pair->val cdr)

(define env-cre
  (case-lambda
    [()
     (list)]
    [(bnd*)
     (for/fold ([env (list)]) ([k.v bnd*])
       (match k.v [`(,k . ,v) (env-add env k v)]))]
    [(key* val*)
     (for/fold ([env (list)]) ([k key*] [v val*])
       (env-add env k v))]))
(define env-add
  (case-lambda
    [(env key val)
     (cons (mcons key val) env)]
    [(env new-env)
     (append new-env env)]))
(define env-set
  (lambda (env key val)
    (let ([fnd (env-get env key (lambda () #f))])
      (if (not fnd)
        (error 'env-set "not found '~a'" key)
        (begin (set-mcdr! fnd val) env)))))
(define env-ref
  (lambda (env key [handle #f])
    (let ([fnd (env-get env key (lambda () #f))])
      (if fnd (mcdr fnd)
        (if handle (handle)
          (error 'env "couldn't find '~a' in env" key))))))
(define env-get
  (lambda (env key [handle #f])
    (let loop ([env env])
      (if (empty? env)
        (if handle (handle)
          (error 'env "couldn't find '~a' in env" key))
        (match (car env)
          [(@ bnd (mcons k v))
           (if (equal? key k) bnd
             (loop (cdr env)))])))))

(define assoc-cre
  (case-lambda
    [()
     (make-immutable-hash)]
    [(bnd*)
     (make-immutable-hash bnd*)]
    [(key* val*)
     (make-immutable-hash (map make-pair key* val*))]))
(define assoc-add
  (case-lambda
    [(ass key val)
     (hash-set ass key val)]
    [(ass new-ass)
     (hash-union ass new-ass)]))
(define assoc-ref
  (lambda (ass key [handle #f])
    (let ([fnd (hash-ref ass key #f)])
      (if fnd fnd
        (if (not handle)
          (error 'assoc-ref "not found ~a" key)
          (handle))))))

(define symbol-append
  (lambda sym* 
    (string->symbol
      (apply string-append
        (map symbol->string sym*)))))

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
      (let ([len (length a*)])
        (let loop ([e* e*] [a* a*] [v* (list)])
          (if (empty? e*)
            (if (empty? v*)
              (curry (apply (curry cont) a*) (% v*))
              (apply (apply (curry cont) a*) (apply map list (reverse v*))))
            (curry (apply (curry proc) a*) (car e*)
              (lambda r* (loop (cdr e*) (take r* len) (cons (drop r* len) v*)))))))]))

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

(define-syntax owise
  (syntax-rules ()
    [(_) #t]))

(define-syntax define-store
  (lambda (stx)
    (define symbol-append
      (lambda sym* 
        (string->symbol
          (apply string-append (map symbol->string sym*)))))
    (define build-name
      (lambda (name sfx)
        (datum->syntax
          (begin name)
          (symbol-append (syntax-e name) sfx)
          (begin name))))
    (syntax-case stx ()
      [(_ name type)
       (let ([name-s #'name] [type-e (syntax-e #'type)])
       #`(define-values
           (#,(build-name name-s '/cur$)
            #,(build-name name-s '/cre$)
            #,(build-name name-s '/add$))
           (let ([param (make-parameter (void))])
           #,(cond
               [(eq? '#:list type-e)
                #`(values param list
                    (lambda (v) (param (cons v (param)))))]
               [(eq? '#:assoc type-e)
                #`(values param assoc-cre
                    (lambda (k v) (param (assoc-add (param) k v))))]
               [(eq? '#:set type-e)
                #`(values param set
                    (lambda (v) (param (set-union (set v) (param)))))]))))])))

(define-match-expander else
  (lambda (stx)
    (syntax-case stx ()
      [(_) #'(var _)])))

(define-match-expander @
  (lambda (stx)
    (syntax-case stx ()
      [(_ id pat) #'(and id pat)])))

(define-match-expander pair
  (lambda (stx)
    (syntax-case stx ()
      [(_ k v) #'(cons k v)])))

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


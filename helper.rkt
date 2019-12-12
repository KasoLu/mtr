#lang racket

(provide (all-defined-out) %)
(require racket/control)

;; ---- language ---- ;;
(define g-fp
  '(global-value free_ptr))

(define g-fe
  '(global-value fromspace_end))

(define ath-op?
  (curry set-member? '(+ - * /)))

(define cmp-op?
  (curry set-member? '(eq? < <= > >=)))

(define lgc-op?
  (curry set-member? '(and or not)))

(define typed->type
  (match-lambda
    [`(typed ,expr ,type) type]))

(define typed->expr
  (match-lambda
    [`(typed ,expr ,type) expr]))

(define define->name
  (match-lambda
    [`(define (,f . ,p*) : ,rt ,e) f]
    [`(define (,f . ,p*) ,fi ,e) f]
    [`(define (,f . ,p*) ,e) f]))

(define proc-type?
  (lambda (type)
    (match? `(,_ ... -> ,_) type)))

(define vector-type?
  (match-lambda
    [`(Vector . ,_) #t]
    ['Vector #t]
    [_ #f]))

;; ----- utils ----- ;;
(define make-assoc
  (lambda () (list)))

(define assoc-ref
  (lambda (ass key [handle #f])
    (let ([found (assq key ass)])
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

;; ----- syntax ----- ;;
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

;; ----- test ----- ;;
(define test
  (lambda (handle pass* . case*)
    (for ([kase case*])
      (newline)
      (pretty-display
        (let loop ([res '()] [kase kase] [pass* pass*])
          (if (empty? pass*)
           `((execute ,(reverse res)) ,kase)
            (let* ([pass (car pass*)] [kase (pass kase)])
              (loop (cons (handle kase) res) kase (cdr pass*)))))))))


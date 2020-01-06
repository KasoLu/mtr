#lang racket

(provide (rename-out [ast:pgm parse]))
(require "helper.rkt" racket/fixnum)

(define-store def-rcd #:list)

(define ast:pgm
  (match-lambda
    [`(program ,pi ,def* ... ,expr)
      (let ([env (env-cre (map def->f.t def*))])
        (parameterize ([def-rcd/cur$ '(main begin)])
         `(program ,pi .
           ,(cons
              (let-values ([(e t) (ast:exp env expr)])
               `(define (begin) : ,t ,(assoc-cre) ,e))
              (map (curry ast:def env) def*)))))]))

(define ast:def
  (lambda (env def)
    (match def
      [`(define (,f [,v* : ,t*]...) : ,rt ,e)
        (if (memq f (def-rcd/cur$))
          (error 'parse "already defined: ~a" f)
          (begin
            (def-rcd/add$ f)
            (let-values ([(e t) (ast:exp (env-add env (env-cre v* t*)) e)])
              (if (equal? rt t)
               `(define (,f . ,(map type-anoc v* t*)) : ,(assoc-cre) ,rt ,e)
                (report-error/type `(define (,f . ,t*) : ,rt ,t))))))])))

(define ast:exp
  (lambda (env expr)
    (let ([rec (curry ast:exp env)])
      (match expr
        [`(read) (values expr 'Integer)]
        [`(void) (values expr 'Void)]
        [`(,op ,e1 ,e2) #:when (ath-op? op)
          (let-values ([(e* t*) (map/values rec `(,e1 ,e2))])
            (if (all-Integer? t*)
              (values `(,op . ,e*) 'Integer)
              (report-error/type `(,op . ,t*))))]
        [`(and ,e1 ,e2)
          (match-let-values ([(`(,e1 ,e2) t*) (map/values rec `(,e1 ,e2))])
            (if (all-Boolean? t*)
              (values `(if ,e1 (if ,e2 #t #f) #f) 'Boolean)
              (report-error/type `(and . ,t*))))]
        [`(or ,e1 ,e2)
          (match-let-values ([(`(,e1 ,e2) t*) (map/values rec `(,e1 ,e2))])
            (if (all-Boolean? t*)
              (values `(if ,e1 #t (if ,e2 #t #f)) 'Boolean)
              (report-error/type `(or . ,t*))))]
        [`(not ,e1)
          (let-values ([(e1 t1) (rec e1)])
            (if (eq? t1 'Boolean)
              (values `(not ,e1) 'Boolean)
              (report-error/type `(not ,t1))))]
        [`(eq? ,e1 ,e2)
          (let-values ([(e* t*) (map/values rec `(,e1 ,e2))])
            (if ((disjoin all-Integer? all-Boolean?) t*)
              (values `(eq? . ,e*) 'Boolean)
              (report-error/type `(eq? . ,t*))))]
        [`(,op ,e1 ,e2) #:when (cmp-op? op)
          (let-values ([(e* t*) (map/values rec `(,e1 ,e2))])
            (if (all-Integer? t*)
              (values `(,op . ,e*) 'Boolean)
              (report-error/type `(,op . ,t*))))]
        [`(let ([,v* ,e*]...) ,eb)
          (match/values
            (for/fold ([e* (list)] [env env]) ([v v*] [e e*])
              (let-values ([(e t) (ast:exp env e)])
                (values (cons e e*) (env-add env v t))))
            [(e* env)
             (let-values ([(eb tb) (ast:exp env eb)])
               (values (foldr (lambda (v e a) `(bind [,v ,e] ,a)) eb v* (reverse e*)) tb))])]
        [`(if ,e1 ,e2 ,e3)
          (match-let-values ([(e* `(,t1 ,t2 ,t3)) (map/values rec `(,e1 ,e2 ,e3))])
            (if (and (eq? t1 'Boolean) (equal? t2 t3))
              (values `(if . ,e*) t2)
              (report-error/type `(if ,t1 ,t2 ,t3))))]
        [`(begin ,e* ... ,eb)
          (let-values ([(e* t*) (map/values rec e* dft-vals)] [(eb tb) (rec eb)])
            (if (and (all-Void? t*) (not (void-type? tb))) 
              (values `(begin ,@e* ,eb) tb)
              (report-error/type `(begin ,@t* ,tb))))]
        [`(vector . ,e*)
          (let-values ([(e* t*) (map/values rec e* dft-vals)])
            (if (any-Void? t*)
              (report-error/type `(vector . ,t*))
              (values `(vector . ,e*) `(Vector . ,t*))))]
        [`(vector-ref ,ev ,(? fixnum? i))
          (let-values ([(ev tv) (rec ev)])
            (match tv
              [`(Vector . ,t*)
                (if (< i (length t*))
                  (values `(vector-ref ,ev ,i) (list-ref t* i))
                  (report-error/ioor `(vector-ref ,tv Integer)))]
              [_(report-error/type `(vector-ref ,tv Integer))]))]
        [`(vector-set! ,ev ,(? fixnum? i) ,e1)
          (let-values ([(ev tv) (rec ev)] [(e1 t1) (rec e1)])
            (match tv
              [`(Vector . ,t*)
                (cond
                  [(not (< i (length t*))) 
                   (report-error/ioor `(vector-set! ,tv ,i ,t1))]
                  [(void-type? t1)
                   (report-error/type `(vector-set! ,tv Integer ,t1))]
                  [(owise)
                   (values `(vector-set! ,ev ,i ,e1) 'Void)])]
              [_(report-error/type `(vector-set! ,tv Integer ,t1))]))]
        [`(lambda ([,v* : ,t*]...) : ,rt ,eb)
          (if (any-Void? t*)
            (report-error/type `(lambda ,t* : ,rt T))
            (let-values ([(eb tb) (ast:exp (env-add env (env-cre v* t*)) eb)])
              (if (equal? rt tb)
                (values `(lambda ,(map type-anoc v* t*) : ,tb ,eb) `(,@t* -> ,tb))
                (report-error/type `(lambda ,t* : ,rt ,tb)))))]
        [`(,ep . ,e*)
          (let-values ([(ep tp) (rec ep)] [(e* t*) (map/values rec e* dft-vals)])
            (match tp
              [`(,vt* ... -> ,rt)
                (if (equal? vt* t*)
                  (values `(app ,ep . ,e*) rt)
                  (report-error/type `(,tp . ,t*)))]
              [_(report-error/type `(,tp . ,t*))]))]
        [_(ast:arg env expr)]))))

(define ast:arg
  (lambda (env arg)
    (match arg
      [(? fixnum?)
       (values arg 'Integer)]
      [(? boolean?)
       (values arg 'Boolean)]
      [(? symbol?)
       (values arg (env-ref env arg))]
      [(else)
       (error 'parse "invalid expr: ~a" arg)])))

(define def->f.t
  (match-lambda
    [`(define (,f [,v* : ,t*]...) : ,rt ,e)
      (make-pair f `(,@t* -> ,rt))]))

(define dft-vals
  (lambda () (values '() '())))

(define all-Integer?
  (curry andmap (curry eq? 'Integer)))

(define all-Boolean?
  (curry andmap (curry eq? 'Boolean)))

(define all-Void?
  (curry andmap void-type?))

(define any-Void?
  (curry ormap void-type?))

(define report-error/type
  (lambda (type)
    (error 'parse "invalid type: ~a" type)))

(define report-error/ioor
  (lambda (expr)
    (error 'parse "index out of range: ~a" expr)))


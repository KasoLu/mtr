#lang racket

(provide (rename-out [ast:pgm target-select]))
(require "helper.rkt")

(define ast:pgm
  (match-lambda
    [`(program ,pi . ,def+)
     `(program ,pi . ,(map ast:def def+))]))

(define ast:def
  (match-lambda
    [`(define (,f . ,v*) ,fi ([,lb* ,tail*]...))
     `(define (,f) ,fi
       ,(assoc-add
          (for/list ([lb lb*] [tail tail*])
            (make-pair lb `(block () ,@(ast:tail f tail))))
          (symbol-append f '_pre)
          (make-pre-block f v*)))]))

(define ast:tail
  (lambda (f tail)
    (match tail
      [`(return ,e)
       `(,@(ast:stmt `(assign rax ,e)) (jmp-post ,(symbol-append f '_post)))]
      [`(seq ,s ,t)
        (append (ast:stmt s) (ast:tail f t))]
      [`(goto ,l)
       `((jmp ,l))]
      [`(if (,cmp ,a1 ,a2) (goto ,l1) (goto ,l2))
        (let ([a1 (ast:arg a1)] [a2 (ast:arg a2)])
         `((cmpq ,a2 ,a1)
           (jmp-if ,(cmp->cc cmp) ,l1)
           (jmp ,l2)))]
      [`(tailcall ,a1 ,a* ...)
        (append
          (for/list ([a a*] [r call-reg*])
           `(movq ,(ast:arg a) (reg ,r)))
         `((tail-jmp ,(ast:arg a1))))])))

(define ast:stmt
  (match-lambda
    [`(collect ,i)
     `((movq (reg r15) (reg rdi))
       (movq ,(ast:arg i) (reg rsi))
       (callq collect))]
    [`(assign ,v (void))
     `((movq (int 0) ,(ast:arg v)))]
    [`(assign ,v (read))
     `((callq read_int)
       (movq (reg rax) ,(ast:arg v)))]
    [`(assign ,v (fun-ref ,f))
     `((leaq (fun-ref ,f) ,(ast:arg v)))]
    [`(assign ,v (- ,a1))
      (let ([v/a (ast:arg v)])
       `((movq ,(ast:arg a1) ,v/a)
         (negq ,v/a)))]
    [`(assign ,v (+ ,a1 ,a2))
      (let ([v/a (ast:arg v)])
       `((movq ,(ast:arg a2) ,v/a)
         (addq ,(ast:arg a1) ,v/a)))]
    [`(assign ,v (- ,a1 ,a2))
      (let ([v/a (ast:arg v)])
       `((movq ,(ast:arg a2) ,v/a)
         (subq ,(ast:arg a1) ,v/a)))]
    [`(assign ,v (* ,a1 ,a2))
     `((movq ,(ast:arg a2) (reg rax))
       (imulq ,(ast:arg a1))
       (movq (reg rax) ,(ast:arg v)))]
    [`(assign ,v (/ ,a1 ,a2))
     `((movq ,(ast:arg a2) (reg rax))
       (idivq ,(ast:arg a1))
       (movq (reg rax) ,(ast:arg v)))]
    [`(assign ,v (not ,a1))
      (let ([v/a (ast:arg v)])
       `((movq ,(ast:arg a1) ,v/a)
         (xorq (int 1) ,v/a)))]
    [`(assign ,v (,cmp ,a1 ,a2)) #:when (cmp-op? cmp)
      (let ([v/a (ast:arg v)])
       `((cmpq ,(ast:arg a2) ,(ast:arg a1))
         (set ,(cmp->cc cmp) (byte-reg al))
         (movzbq (byte-reg al) ,v/a)))]
    [`(assign ,v (allocate ,i ,t))
      (let ([v/a (ast:arg v)])
       `((movq ,g-fp ,v/a)
         (addq (int ,(* 8 (add1 i))) ,g-fp)
         (movq ,v/a (reg r11))
         (movq (int ,t) (deref r11 0))))]
    [`(assign ,v (vector-ref ,a ,i))
      (let ([v/a (ast:arg v)])
       `((movq ,(ast:arg a) (reg r11))
         (movq (deref r11 ,(* 8 (add1 i))) ,v/a)))]
    [`(assign ,v (vector-set! ,av ,i ,aa))
      (let ([v/a (ast:arg v)])
       `((movq ,(ast:arg av) (reg r11))
         (movq ,(ast:arg aa) (deref r11 ,(* 8 (add1 i))))
         (movq (int 0) ,v/a)))]
    [`(assign ,v (call ,a1 . ,a*))
      (append
        (for/list ([a a*] [r call-reg*])
         `(movq ,(ast:arg a) (reg ,r)))
       `((indirect-callq ,(ast:arg a1))
         (movq (reg rax) ,(ast:arg v))))]
    [`(assign ,v ,a)
     `((movq ,(ast:arg a) ,(ast:arg v)))]))

(define ast:arg
  (match-lambda
    [(? reg? reg)
    `(reg ,reg)]
    [(? glb? glb)
     (% glb)]
    [(? integer? int)
    `(int ,int)]
    [(? boolean? bool)
    `(int ,(if bool 1 0))]
    [(? symbol? var)
    `(var ,var)]))

(define cmp->cc
  (match-lambda ['eq? 'e] ['< 'l] ['<= 'le] ['> 'g] ['>= 'ge]))

(define make-pre-block
  (lambda (f v*)
    (let ([start-ins `(jmp ,(symbol-append f '_start))])
      (let loop ([v* v*] [r* call-reg*] [ins+ (list start-ins)])
        (if (empty? v*)
         `(block () ,@ins+)
          (loop (cdr v*) (cdr r*)
            (cons `(movq (reg ,(car r*)) (var ,(car v*))) ins+)))))))


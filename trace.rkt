#lang racket

(provide lambda match-lambda)
(require racket/trace)

(define-syntax-rule
  (lambda kw body ...)
  (trace-lambda kw body ...))

(define-syntax-rule
  (match-lambda cls ...)
  (trace-lambda (id) (match id cls ...)))


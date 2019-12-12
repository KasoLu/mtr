### Mini Typed Racket

- grammer:
  ```
  Prgm : (program Info Def* Expr)
  Defi : (define (Var [Var : Type]*) : Type Expr)
  Expr : Argu | (read) | (void) | (- Expr) | (not Expr)
       | (let ([Var Expr]) Expr) | (if Expr Expr Expr)
       | (Arth Expr Expr) | (Logi Expr Expr) | (Comp Expr Expr)
       | (vector Expr*) | (vector-ref Expr Int) | (vector-set! Expr Int Expr)
       | (lambda ([Var : Type]*) : Type Expr) | (Expr Expr*)
  Comp : < | <= | > | >= | eq?
  Logi : and | or
  Arth : + | - | * | /
  Argu : Int64 | #t | #f
  Type : Integer | Boolean | Void | Vector | -> 
       | (Vector Type*) | (Type* -> Type)
  ```

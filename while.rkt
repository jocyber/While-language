#lang racket/base

(require racket/match
         racket/function
         )

; BNF grammar
; a ::= n | x | a1 + a2 | a1 * a2 | a1 - a2
; b ::= ~b | true | false | a1 = a2 | a1 <= a2 | b1 && b2
; S ::= S ; S | x := a | skip | while b do S | if b then S1 else S2

(struct Numeral (n) #:transparent)
(struct Boolean (b) #:transparent)
(struct Identifier (x) #:transparent)
(struct BinaryExpr (op a1 a2) #:transparent)
(struct UnaryExpr (op a) #:transparent)
(struct While (b s) #:transparent)
(struct Begin (ss) #:transparent)
(struct If (b s1 s2) #:transparent)
(struct Skip () #:transparent)

; semantic function for WHILE 
(define ((interpret expr) env)
  (match expr
    [(Numeral n) n]
    [(Boolean b) b]
    [(Skip) (void)]
    [(Identifier x) 
     (hash-ref env x (thunk (error (format "Undefined variable in scope [~s]" x))))]
    [(BinaryExpr ':= id e)
     (when (hash-has-key? env id)
       (error (format "~s is already a bound identifer" id)))

     (hash-set! env id ((interpret e) env))]
    [(BinaryExpr op a1 a2) 
     (let ([v1 ((interpret a1) env)]
           [v2 ((interpret a2) env)])
       (apply op (list v1 v2)))]
    [(UnaryExpr op a) (apply op (list ((interpret a) env)))]
    [(If condition consequent alternate)
     (if ((interpret condition) env)
         ((interpret consequent) env)
         ((interpret alternate) env))]))
 
(module+ test
  (require rackunit)

  (check-eq? 3 ((interpret (Numeral 3)) (make-hash)) "numerals are self-evaluated")
  (check-eq? 4
             ((interpret 
               (BinaryExpr + (Numeral 1) (Identifier 'x)))
               (make-hash '([x . 3])))
             "addition produces correct sum")
  (check-exn exn:fail? (thunk ((interpret (Identifier 'x)) (make-hash))) "error on uninitialized variable access")
  )


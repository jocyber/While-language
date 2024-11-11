#lang racket

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
(struct If (b s1 s2) #:transparent)

; semantic function for WHILE 
(define ((interpret expr) env)
  (match expr
    [(Numeral n) n]
    [(Boolean b) b]
    [(Identifier x) 
     (hash-ref env x (thunk (error (format "Undefined variable in scope [~s]" x))))]
    [(BinaryExpr op a1 a2) 
     (let ([v1 ((interpret a1) env)]
           [v2 ((interpret a2) env)])
       (apply op (list v1 v2)))]))
 
(module+ test
  (require rackunit)

  (check-eq? ((interpret (Numeral 3)) (make-hash)) 3 "numerals are self-evaluated")
  (check-eq? ((interpret 
                (BinaryExpr + (Numeral 1) (Identifier 'x)))
                (make-hash '([x . 3])))
             4
             "addition produces correct sum")
  (check-exn exn:fail? (thunk ((interpret (Identifier 'x)) (make-hash))) "error on uninitialized variable access")
  )


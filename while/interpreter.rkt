#lang racket/base

(require racket/match
         racket/function
         "ast-nodes.rkt"
         )

; BNF grammar
; a ::= n | x | a1 + a2 | a1 * a2 | a1 - a2
; b ::= ~b | true | false | a1 = a2 | a1 <= a2 | b1 && b2
; S ::= S ; S | x := a | skip | while b do S | if b then S1 else S2

; semantic function for WHILE 
(define ((interpret expr) [env (make-hash)])
  (match expr
    [(Numeral n) n]
    [(Boolean b) b]
    [(Skip) (void)]
    [(Identifier x) 
     (hash-ref env x (thunk (error (format "Undefined variable in scope: ~s" x))))]
    [(BinaryExpr ':= id e)
     (hash-set! env id ((interpret e) env))]
    [(BinaryExpr op a1 a2) 
     (let ([v1 ((interpret a1) env)]
           [v2 ((interpret a2) env)])
       (apply op (list v1 v2)))]
    [(UnaryExpr op a) (apply op (list ((interpret a) env)))]
    [(If b s1 s2)
     (if ((interpret b) env)
         ((interpret s1) env)
         ((interpret s2) env))]
    [(Begin ss) (for/last ([s ss]) ((interpret s) env))]
    [(While b s)
     (let while ()
       (cond 
         [((interpret b) env) 
          ((interpret s) env)
          (while)]
         [else (void)]))]))

(module+ test
  (require rackunit)

  ; arithmetic and variable access
  (check-eq? 3 ((interpret (Numeral 3))) "numerals are self-evaluated")
  (check-eq? 4
             ((interpret 
               (BinaryExpr + (Numeral 1) (Identifier 'x)))
               (make-hash '([x . 3])))
             "addition produces correct sum")
  (check-exn exn:fail? (thunk ((interpret (Identifier 'x)))) "error on uninitialized variable access")

  ; while 
  (check-eq? 4 
             ((interpret 
                (Begin (list (BinaryExpr ':= 'x (Numeral 3))
                             (While (BinaryExpr < (Identifier 'x) (Numeral 4))
                                    (BinaryExpr ':= 'x (BinaryExpr + (Identifier 'x) (Numeral 1))))
                             (Identifier 'x)))))
             "variables are not shadowed in while block")
  )


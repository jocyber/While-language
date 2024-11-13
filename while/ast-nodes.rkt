#lang racket/base

(provide (all-defined-out))

(struct Numeral (n) #:transparent)
(struct Boolean (b) #:transparent)
(struct Identifier (x) #:transparent)
(struct BinaryExpr (op a1 a2) #:transparent)
(struct UnaryExpr (op a) #:transparent)
(struct While (b s) #:transparent)
(struct Begin (ss) #:transparent)
(struct If (b s1 s2) #:transparent)
(struct Skip () #:transparent)


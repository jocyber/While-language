#lang racket

; an implementation of the WHILE language presented in "Semantics with Applications: An Appetizer"

; formal grammar
; a ::= n | x | a1 + a2 | a1 * a2 | a1 - a2
; b ::= ~b | true | false | a1 = a2 | a1 <= a2 | b1 && b2
; S ::= S ; S | x := a | skip | while b do S | if b then S1 else S2




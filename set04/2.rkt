;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide expr-fold)
(provide value-of)
(provide make-sum-exp)
(provide make-mult-exp)
(provide operator-count)
(provide operand-count)
(provide expr-to-image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct sum-exp (exprs))
(define-struct mult-exp (exprs))

;;  An Expr is one of 
;; -- Number
;; --(make-sum-exp LOExpr)
;; --(make-mult-exp LOExpr)

;; interp:
;; A Number represents a operand in the expr
;; a sum-exp represents a sum of the given LOExpr
;; a mult-exp represents a multiplication of the given LOExpr

;; template
;; expr-fn ; Expr -> ?
;(define (expr-fn expr)
;    (cond
;        [(number? expr) ...]
;        [(sum-exp? expr) ... (loexpr-fn (sum-exp-exprs expr))]
;        [(mult-exp? expr) ...(loexpr-fn (mult-exp-exprs expr))]))

;; A LOExpr is one of 
;; --empty     (interp: there is no more expression)
;; --(cons Expr LOExpr)  (interp: a Expr folloewd by  
;;                                      a sequence of Expr (LOExpr))
;; template :
;; loexpr-fn : LOExpr -> ?
;(define (loexpr-fn loexpr)
;    (cond
;        [(empty? loexpr) ...]
;        [else (...
;                 (expr-fn (first loexpr))
;                 (loexpr-fn (rest loexpr)))]))

; [X] (X -> Boolean) [List-of X] -> [List-of X]
; produce a list from all those items on list for which p holds
; (define (filter p list) ...)

; [X Y] (X Y -> Y) Y [List-of X] -> Y
; compute the result of applying f from right to left to all of
; alox and base, that is, apply f to
;    the last item in alox and base,
;    the penultimate item and the result of the first step,
;    and so on up to the first item
;    (foldr f base (list x-1 ... x-n)) = (f x-1 ... (f x-n base))
; (define (foldr f base alox) ...)

; [X Y] (X -> Y) [List-of X] -> [List-of Y]
; construct a list by applying f to each item on alox
;    (map f (list x-1 ... x-n)) = (list (f x-1) ... (f x-n))
;(define (map f alox) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANT VARIABLES FOR TESTS
;; 1-LEVEL OPERAND
(define NUM-LIST-IMAGE
  (above (text (number->string 1) 20 "black" )
         (text (number->string 2) 20 "black" )
         (text (number->string 3) 20 "black" )
         (text (number->string 4) 20 "black" )
         (empty-scene 0 0)))
(define CONSTANT 5)
(define CONSTANT-IMAGE (text (number->string CONSTANT) 20 "black" ))
(define EMPTY-SUM (make-sum-exp empty));;0
(define EMPTY-SUM-IMAGE (beside/align "top" (text "(+ " 20 "black" )
                                   (beside/align "bottom"  
                                     (empty-scene 0 0)
                                     (text ")" 20 "black" ))))
(define EMPTY-MULT (make-mult-exp empty));;1
(define EMPTY-MULT-IMAGE (beside/align "top" (text "(* " 20 "black" )
                                   (beside/align "bottom"  
                                     (empty-scene 0 0)
                                     (text ")" 20 "black" ))))
(define NUM-SUM (make-sum-exp (list 1 2 3 4)));;=10
(define NUM-SUM-IMAGE (beside/align "top" (text "(+ " 20 "black" )
                                   (beside/align "bottom"  
                                     NUM-LIST-IMAGE
                                     (text ")" 20 "black" ))))
(define NUM-MULT (make-mult-exp (list 1 2 3 4)));;=24
(define NUM-MULT-IMAGE (beside/align "top" (text "(* " 20 "black" )
                                   (beside/align "bottom"  
                                     NUM-LIST-IMAGE
                                     (text ")" 20 "black" ))))
;; 2-LEVEL OPERAND
(define SUM-OF-NON-EMPTY (make-sum-exp (list NUM-SUM NUM-MULT 1 2 )));;=37
(define SUM-WITH-EMPTY-SUM 
  (make-sum-exp (list EMPTY-SUM NUM-SUM NUM-MULT 1 2 )));;=37
(define SUM-WITH-EMPTY-MULT 
  (make-sum-exp (list EMPTY-MULT NUM-SUM NUM-MULT 1 2)));;=38
(define SUM-WITH-EMPTY-SUM-EMPTY-MULT 
  (make-sum-exp (list EMPTY-SUM EMPTY-MULT)));;=1
(define SUM-EMPTY-SUM (make-sum-exp (list EMPTY-SUM)));;=0
(define SUM-EMPTY-MULT (make-sum-exp (list EMPTY-MULT)));;=1

(define SUM-OF-NON-EMPTY-IMAGE 
  (beside/align "top" (text "(+ " 20 "black" )
                (beside/align "bottom"  
                              (above NUM-SUM-IMAGE
                                     NUM-MULT-IMAGE
                                     (text (number->string 1) 20 "black" )
                                     (text (number->string 2) 20 "black" )
                                     (empty-scene 0 0))
                              (text ")" 20 "black" ))))
(define SUM-WITH-EMPTY-SUM-IMAGE 
  (beside/align "top" (text "(+ " 20 "black" )
                (beside/align "bottom"  
                              (above EMPTY-SUM-IMAGE
                                     NUM-SUM-IMAGE
                                     NUM-MULT-IMAGE
                                     (text (number->string 1) 20 "black" )
                                     (text (number->string 2) 20 "black" )
                                     (empty-scene 0 0))
                              (text ")" 20 "black" ))))
(define SUM-WITH-EMPTY-MULT-IMAGE 
  (beside/align "top" (text "(+ " 20 "black" )
                (beside/align "bottom"  
                              (above EMPTY-MULT-IMAGE
                                     NUM-SUM-IMAGE
                                     NUM-MULT-IMAGE
                                     (text (number->string 1) 20 "black" )
                                     (text (number->string 2) 20 "black" )
                                     (empty-scene 0 0))
                              (text ")" 20 "black" ))))
(define SUM-WITH-EMPTY-SUM-EMPTY-MULT-IMAGE 
  (beside/align "top" (text "(+ " 20 "black" )
                (beside/align "bottom"  
                              (above EMPTY-SUM-IMAGE
                                     EMPTY-MULT-IMAGE
                                     (empty-scene 0 0))
                              (text ")" 20 "black" ))))
(define SUM-EMPTY-SUM-IMAGE 
  (beside/align "top" (text "(+ " 20 "black" )
                (beside/align "bottom"  
                              (above EMPTY-SUM-IMAGE
                                     (empty-scene 0 0))
                              (text ")" 20 "black" ))))
(define SUM-EMPTY-MULT-IMAGE (beside/align "top" (text "(+ " 20 "black" )
                                   (beside/align "bottom"  
                                     (above EMPTY-MULT-IMAGE
                                            (empty-scene 0 0))
                                     (text ")" 20 "black" ))))

(define MULT-OF-NON-EMPTY 
  (make-mult-exp (list NUM-SUM NUM-MULT 1 2)));;=480
(define MULT-WITH-EMPTY-SUM 
  (make-mult-exp (list EMPTY-SUM NUM-SUM NUM-MULT 1 2)));;=0
(define MULT-WITH-EMPTY-MULT 
  (make-mult-exp (list EMPTY-MULT NUM-SUM NUM-MULT 1 2)));;=480
(define MULT-WITH-EMPTY-SUM-EMPTY-MULT 
  (make-mult-exp (list EMPTY-SUM EMPTY-MULT)));;=0
(define MULT-EMPTY-SUM (make-mult-exp (list EMPTY-SUM)));;=0
(define MULT-EMPTY-MULT (make-mult-exp (list EMPTY-MULT)));;=1

(define MULT-OF-NON-EMPTY-IMAGE 
  (beside/align "top" (text "(* " 20 "black" )
                (beside/align "bottom"  
                              (above NUM-SUM-IMAGE
                                     NUM-MULT-IMAGE
                                     (text (number->string 1) 20 "black" )
                                     (text (number->string 2) 20 "black" )
                                     (empty-scene 0 0))
                              (text ")" 20 "black" ))))
(define MULT-WITH-EMPTY-SUM-IMAGE 
  (beside/align "top" (text "(* " 20 "black" )
                (beside/align "bottom"  
                              (above EMPTY-SUM-IMAGE
                                     NUM-SUM-IMAGE
                                     NUM-MULT-IMAGE
                                     (text (number->string 1) 20 "black" )
                                     (text (number->string 2) 20 "black" )
                                     (empty-scene 0 0))
                              (text ")" 20 "black" ))))
(define MULT-WITH-EMPTY-MULT-IMAGE 
  (beside/align "top" (text "(* " 20 "black" )
                (beside/align "bottom"  
                        (above EMPTY-MULT-IMAGE
                               NUM-SUM-IMAGE
                               NUM-MULT-IMAGE
                               (text (number->string 1) 20 "black" )
                                     (text (number->string 2) 20 "black" )
                                      (empty-scene 0 0))
                        (text ")" 20 "black" ))))
(define MULT-WITH-EMPTY-SUM-EMPTY-MULT-IMAGE 
  (beside/align "top" (text "(* " 20 "black" )
                (beside/align "bottom"
                              (above EMPTY-SUM-IMAGE
                                     EMPTY-MULT-IMAGE
                                     (empty-scene 0 0))
                              (text ")" 20 "black" ))))
(define MULT-EMPTY-SUM-IMAGE (beside/align "top" (text "(* " 20 "black" )
                                   (beside/align "bottom"  
                                     (above EMPTY-SUM-IMAGE
                                            (empty-scene 0 0))
                                     (text ")" 20 "black" ))))
(define MULT-EMPTY-MULT-IMAGE (beside/align "top" (text "(* " 20 "black" )
                                   (beside/align "bottom"  
                                     (above EMPTY-MULT-IMAGE
                                            (empty-scene 0 0))
                                     (text ")" 20 "black" ))))

;; 3-LEVEL OPERAND
(define SUM-NON-EMPTY-3 
  (make-sum-exp (list SUM-OF-NON-EMPTY MULT-OF-NON-EMPTY)));;517
(define SUM-WITH-EMPTY-3 
  (make-sum-exp (list SUM-WITH-EMPTY-MULT MULT-WITH-EMPTY-SUM)));;38
(define SUM-WITH-EMPTY-SUM-EMPTY-MULT-3 
  (make-sum-exp (list SUM-WITH-EMPTY-SUM-EMPTY-MULT 
                      MULT-WITH-EMPTY-SUM-EMPTY-MULT)));;1
(define SUM-EMPTY-SUM-3 
  (make-sum-exp (list SUM-EMPTY-SUM)));;0
;;(define SUM-EMPTY-MULT-3 (make-sum-exp (list SUM-EMPTY-MULT)));;2

(define SUM-NON-EMPTY-3-IMAGE (beside/align "top" (text "(+ " 20 "black" )
                                   (beside/align "bottom"  
                                     (above SUM-OF-NON-EMPTY-IMAGE
                                            MULT-OF-NON-EMPTY-IMAGE
                                            (empty-scene 0 0))
                                     (text ")" 20 "black" ))))
(define SUM-WITH-EMPTY-3-IMAGE (beside/align "top" (text "(+ " 20 "black" )
                                   (beside/align "bottom"  
                                     (above SUM-WITH-EMPTY-MULT-IMAGE
                                            MULT-WITH-EMPTY-SUM-IMAGE
                                            (empty-scene 0 0))
                                     (text ")" 20 "black" ))))
(define SUM-WITH-EMPTY-SUM-EMPTY-MULT-3-IMAGE 
        (beside/align "top" (text "(+ " 20 "black" )
                                  (beside/align "bottom"  
                                    (above SUM-WITH-EMPTY-SUM-EMPTY-MULT-IMAGE
                                          MULT-WITH-EMPTY-SUM-EMPTY-MULT-IMAGE
                                          (empty-scene 0 0))
                                    (text ")" 20 "black" ))))
(define SUM-EMPTY-SUM-3-IMAGE (beside/align "top" (text "(+ " 20 "black" )
                                   (beside/align "bottom"  
                                     (above SUM-EMPTY-SUM-IMAGE
                                            (empty-scene 0 0))
                                     (text ")" 20 "black" ))))

(define MULT-NON-EMPTY-3 (make-mult-exp 
                          (list SUM-OF-NON-EMPTY MULT-OF-NON-EMPTY)))
(define MULT-WITH-EMPTY-3 (make-mult-exp 
                           (list SUM-WITH-EMPTY-MULT MULT-WITH-EMPTY-SUM)))
(define MULT-WITH-EMPTY-SUM-EMPTY-MULT-3 
  (make-mult-exp (list SUM-WITH-EMPTY-SUM-EMPTY-MULT 
                       MULT-WITH-EMPTY-SUM-EMPTY-MULT)));;1
;;(define MULT-EMPTY-SUM-3 (make-mult-exp (list SUM-EMPTY-SUM)));;0
(define MULT-EMPTY-MULT-3 (make-mult-exp (list SUM-EMPTY-MULT)));;2

(define MULT-NON-EMPTY-3-IMAGE (beside/align "top" (text "(* " 20 "black" )
                                   (beside/align "bottom"  
                                     (above SUM-OF-NON-EMPTY-IMAGE
                                            MULT-OF-NON-EMPTY-IMAGE
                                            (empty-scene 0 0))
                                     (text ")" 20 "black" ))))
(define MULT-WITH-EMPTY-3-IMAGE (beside/align "top" (text "(* " 20 "black" )
                                   (beside/align "bottom"  
                                     (above SUM-WITH-EMPTY-MULT-IMAGE
                                            MULT-WITH-EMPTY-SUM-IMAGE
                                            (empty-scene 0 0))
                                     (text ")" 20 "black" ))))
(define MULT-WITH-EMPTY-SUM-EMPTY-MULT-3-IMAGE 
          (beside/align "top" (text "(* " 20 "black" )
                                   (beside/align "bottom"  
                                     (above SUM-WITH-EMPTY-SUM-EMPTY-MULT-IMAGE
                                            MULT-WITH-EMPTY-SUM-EMPTY-MULT-IMAGE
                                            (empty-scene 0 0))
                                     (text ")" 20 "black" ))))
(define MULT-EMPTY-MULT-3-IMAGE (beside/align "top" (text "(* " 20 "black" )
                                   (beside/align "bottom"  
                                     (above SUM-EMPTY-MULT-IMAGE
                                            (empty-scene 0 0))
                                     (text ")" 20 "black" ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expr-fold : (Number -> X) (ListOf<X> -> X) 
;;                           (ListOf<X> -> X) Expr -> X
;; given a Expr, if it's Number, use first function to transfer to X
;; if it's sum-exp, use second function
;; if it's mult-exp, use third function
;; example: 
;; (expr-fold (lambda (n) n) sum product 5) = 5
;; strategy: structural decomposition on expr [Expr]
(define (expr-fold base combiner1 combiner2 expr)
    (cond
        [(number? expr) (base expr)]
        [(sum-exp? expr) (combiner1 (sum-exp-exprs expr))]
        [(mult-exp? expr) (combiner2 (mult-exp-exprs expr))]))

;; value-of : Expr -> Number
;; return the value of the expression as a Number after calculation
;; example;(value-of MULT-OF-NON-EMPTY)=480
;; strategy: structural decomposition on expr [Expr]
#(define (value-of expr)
    (cond
        [(number? expr) expr]
        [(sum-exp? expr) (sum (sum-exp-exprs expr))]
        [(mult-exp? expr) (product (mult-exp-exprs expr))]))
;; strategy: higher order function composition
(define (value-of expr)
    (expr-fold (;; lambda : Number->Number
                ;; return what is given
                lambda (n) n) sum product expr))

;; sum : LOExpr -> Number
;; given a LOExpr, sum the expression inside it can return a number
;; example; (sum 1)=1
;; strategy: structural decomposition on loexpr [LOExpr]
#(define (sum loexpr)
    (cond
        [(empty? loexpr) 0]
        [else (+
                 (value-of (first loexpr))
                 (get-loexpr-sum (rest loexpr)))]))
;; strategy: higher order function composition
(define (sum loexpr)
    (foldr + 0 (map value-of loexpr)))

;; product : LOExpr -> Number
;; given a LOExpr, multiple the expression inside it can return a number
;; example;(product empty)=1
;; strategy: structural decomposition on loexpr [LOExpr]
#(define (product loexpr)
    (cond
        [(empty? loexpr) 1]
        [else (*
                 (value-of (first loexpr))
                 (get-loexpr-mult (rest loexpr)))]))
;; strategy: higher order function composition
(define (product loexpr)
    (foldr * 1 (map value-of loexpr)))

;;
(define-test-suite value-of-tests
  ;; 1-LEVEL OPERAND
  (check-equal?
     (value-of CONSTANT)
     5
     "test for expr with only number")
  (check-equal?
     (value-of EMPTY-SUM)
     0
     "test for sum-expr without anything")
  (check-equal?
     (value-of EMPTY-MULT)
     1
     "test for mult-expr without anything")
  (check-equal?
     (value-of NUM-SUM)
     10
     "test for sum-expr with a list of numbers")
  (check-equal?
     (value-of NUM-MULT)
     24
     "test for mult-expr with a list of numbers")
;;level-2
  (check-equal?
     (value-of SUM-OF-NON-EMPTY)
     37
     "test for sum-expr with a list of sum and mult and numbers")
  (check-equal?
     (value-of SUM-WITH-EMPTY-SUM)
     37
     "test for sum-expr with empty-sum")
  (check-equal?
     (value-of SUM-WITH-EMPTY-MULT)
     38
     "test for sum-expr with empty-mult")
  (check-equal?
     (value-of SUM-WITH-EMPTY-SUM-EMPTY-MULT)
     1
     "test for sum-expr with a list of empty-sum and empty-mult")
  (check-equal?
     (value-of SUM-EMPTY-SUM)
     0
     "test for sum-expr with only ")
  (check-equal?
     (value-of SUM-EMPTY-MULT)
     1
     "test for sum-expr with a list of numbers") 
  
;; for mult with 2 level
  (check-equal?
     (value-of MULT-OF-NON-EMPTY)
     480
     "test for mult-expr with a list of sum and mult and numbers")
  (check-equal?
     (value-of MULT-WITH-EMPTY-SUM)
     0
     "test for mult-expr with empty-sum")
  (check-equal?
     (value-of MULT-WITH-EMPTY-MULT)
     480
     "test for mult-expr with empty-mult")
  (check-equal?
     (value-of MULT-WITH-EMPTY-SUM-EMPTY-MULT)
     0
     "test for mult-expr with a list of empty-sum and empty-mult")
  (check-equal?
     (value-of MULT-EMPTY-SUM)
     0
     "test for mult-expr with only ")
  (check-equal?
     (value-of MULT-EMPTY-MULT)
     1
     "test for mult-expr with a list of numbers") 
  
;; for three-level sum
  (check-equal?
     (value-of SUM-NON-EMPTY-3)
     517
     "test for 3-level sum-expr with a list of sum and mult and numbers")
  (check-equal?
     (value-of SUM-WITH-EMPTY-3)
     38
     "test for 3-level sum-expr with some empty-mult or empty-sum")
  (check-equal?
     (value-of SUM-WITH-EMPTY-SUM-EMPTY-MULT-3)
     1
     "test for 3-level sum-expr with all empty-mult or empty-sum")
  (check-equal?
     (value-of SUM-EMPTY-SUM-3)
     0
     "test for 3-level sum-expr with empty-sum only")

  (check-equal?
     (value-of MULT-NON-EMPTY-3)
     17760
     "test for 3-level mult-expr with a list of sum and mult and numbers")
  (check-equal?
     (value-of MULT-WITH-EMPTY-3)
     0
     "test for 3-level mult-expr with some empty-mult or empty-sum")
  (check-equal?
     (value-of MULT-WITH-EMPTY-SUM-EMPTY-MULT-3)
     0
     "test for 3-level mult-expr with all empty-mult or empty-sum")
  (check-equal?
     (value-of MULT-EMPTY-MULT-3)
     1
     "test for 3-level mult-expr with empty-sum only")
)
(run-tests value-of-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operator-count : Expr -> Number
;; return a Number which count the number of arithmetic 
;; operations in the given expression
;; example: (operator-count EMPTY-MULT)=1
;; strategy: structural decomposition on expr [Expr]
;#(define (operator-count expr)
;    (cond
;      [(number? expr) 0]
;      [(sum-exp? expr) (add1-to-loexp-operator-count (sum-exp-exprs expr))]
;      [(mult-exp? expr) (add1-to-loexp-operator-count (mult-exp-exprs expr))]))
;; strategy: higher order function composition
(define (operator-count expr)
    (expr-fold (;; Number -> Number
                ;; ignore the input return 0
                lambda (n) 0) add1-to-loexp-operator-count 
               add1-to-loexp-operator-count expr))

;; add1-to-loexp-operator-count : LOExpr -> Number
;; add 1 to the total operator number of the given LOExp
;; example: (add1-to-loexp-operator-count EMPTY-MULT) =2
;; strategy: function composition
(define (add1-to-loexp-operator-count loexp)
   (+ 1 (operator-count-loexpr loexp)))

;; operator-count-loexpr : LOExpr -> Number
;; count the number of operators in the given LOExpr
;; exmaple; (operator-count-loexpr EMPTY-MULT) =0
;; strategy: structural decomposition on loexpr [LOExpr]
;#(define (operator-count-loexpr loexpr)
;    (cond
;        [(empty? loexpr) 0]
;        [else (+
;                 (operator-count (first loexpr))
;                 (operator-count-loexpr (rest loexpr)))]))
;; strategy: higher order function composition
(define (operator-count-loexpr loexpr)
    (foldr + 0 (map operator-count loexpr)))


(define-test-suite operator-count-tests
  ;; 1-LEVEL OPERAND
  (check-equal?
     (operator-count CONSTANT)
     0
     "operator-count-tests for expr with only number")
  (check-equal?
     (operator-count EMPTY-SUM)
     1
     "operator-count-tests for sum-expr without anything")
  (check-equal?
     (operator-count EMPTY-MULT)
     1
     "operator-count-tests for mult-expr without anything")
  (check-equal?
     (operator-count NUM-SUM)
     1
     "operator-count-tests for sum-expr with a list of numbers")
  (check-equal?
     (operator-count NUM-MULT)
     1
     "operator-count-tests for mult-expr with a list of numbers")
;;level-2
  (check-equal?
     (operator-count SUM-OF-NON-EMPTY)
     3
     "operator-count-tests for sum-expr with 
                                     a list of sum and mult and numbers")
  (check-equal?
     (operator-count SUM-WITH-EMPTY-SUM)
     4
     "operator-count-tests for sum-expr with empty-sum")
  (check-equal?
     (operator-count SUM-WITH-EMPTY-MULT)
     4
     "operator-count-tests for sum-expr with empty-mult")
  (check-equal?
     (operator-count SUM-WITH-EMPTY-SUM-EMPTY-MULT)
     3
     "operator-count-tests for sum-expr with a list 
                                  of empty-sum and empty-mult")
  (check-equal?
     (operator-count SUM-EMPTY-SUM)
     2
     "operator-count-tests for sum-expr with only ")
  (check-equal?
     (operator-count SUM-EMPTY-MULT)
     2
     "operator-count-tests for sum-expr with a list of numbers") 
  
;; for mult with 2 level
  (check-equal?
     (operator-count MULT-OF-NON-EMPTY)
     3
     "operator-count-tests for mult-expr with a list of 
                  sum and mult and numbers")
  (check-equal?
     (operator-count MULT-WITH-EMPTY-SUM)
     4
     "operator-count-tests for mult-expr with empty-sum")
  (check-equal?
     (operator-count MULT-WITH-EMPTY-MULT)
     4
     "operator-count-tests for mult-expr with empty-mult")
  (check-equal?
     (operator-count MULT-WITH-EMPTY-SUM-EMPTY-MULT)
     3
     "operator-count-tests for mult-expr with a list 
                    of empty-sum and empty-mult")
  (check-equal?
     (operator-count MULT-EMPTY-SUM)
     2
     "operator-count-tests for mult-expr with only ")
  (check-equal?
     (operator-count MULT-EMPTY-MULT)
     2
     "operator-count-tests for mult-expr with a list of numbers") 
  
;; for three-level sum
  (check-equal?
     (operator-count SUM-NON-EMPTY-3)
     7
     "operator-count-tests for 3-level sum-expr with 
                              a list of sum and mult and numbers")
  (check-equal?
     (operator-count SUM-WITH-EMPTY-3)
     9
     "operator-count-tests for 3-level sum-expr 
                      with some empty-mult or empty-sum")
  (check-equal?
     (operator-count SUM-WITH-EMPTY-SUM-EMPTY-MULT-3)
     7
     "operator-count-tests for 3-level
                          sum-expr with all empty-mult or empty-sum")
  (check-equal?
     (operator-count SUM-EMPTY-SUM-3)
     3
     "operator-count-tests for 3-level sum-expr with empty-sum only")

  (check-equal?
     (operator-count MULT-NON-EMPTY-3)
     7
     "operator-count-tests for 3-level mult-expr with 
                 a list of sum and mult and numbers")
  (check-equal?
     (operator-count MULT-WITH-EMPTY-3)
     9
     "operator-count-tests for 3-level mult-expr 
              with some empty-mult or empty-sum")
  (check-equal?
     (operator-count MULT-WITH-EMPTY-SUM-EMPTY-MULT-3)
     7
     "operator-count-tests for 3-level
                      mult-expr with all empty-mult or empty-sum")
  (check-equal?
     (operator-count MULT-EMPTY-MULT-3)
     3
     "operator-count-tests for 3-level mult-expr with empty-sum only")
)
(run-tests operator-count-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operand-count : Expr -> Number
;; return a Number which count the number of arithmetic 
;; constans in the given expression
;; example:(operand-count NUM-MULT)=4
;; strategy: structural decomposition on expr [Expr]
;#(define (operand-count expr)
;    (cond
;        [(number? expr) 1]
;        [(sum-exp? expr) (operand-count-loexpr (sum-exp-exprs expr))]
;        [(mult-exp? expr) (operand-count-loexpr (mult-exp-exprs expr))]))

;; strategy: higher order function composition
(define (operand-count expr)
    (expr-fold (;; Number -> Number
                ;; ignore input and return 1
                lambda (n) 1) operand-count-loexpr operand-count-loexpr expr))

;; operand-count-loexpr : LOExpr -> Number
;; count the number of operators in the given LOExpr
;; exmaple; (operand-count (list 1 2 3 4))=4
;; strategy: structural decomposition on loexpr [LOExpr]
#(define (operand-count-loexpr loexpr)
    (cond
        [(empty? loexpr) 0]
        [else (+
                 (operand-count (first loexpr))
                 (operand-count-loexpr (rest loexpr)))]))

;; strategy: higher order function composition
(define (operand-count-loexpr loexpr)
    (foldr + 0 (map operand-count loexpr)))


(define-test-suite operand-count-tests
  ;; 1-LEVEL OPERAND
  (check-equal?
     (operand-count CONSTANT)
     1
     "operand-count-tests for expr with only number")
  (check-equal?
     (operand-count EMPTY-SUM)
     0
     "operand-count-tests for sum-expr without anything")
  (check-equal?
     (operand-count EMPTY-MULT)
     0
     "operand-count-tests for mult-expr without anything")
  (check-equal?
     (operand-count NUM-SUM)
     4
     "operand-count-tests for sum-expr with a list of numbers")
  (check-equal?
     (operand-count NUM-MULT)
     4
     "operand-count-tests for mult-expr with a list of numbers")
;;level-2
  (check-equal?
     (operand-count SUM-OF-NON-EMPTY)
     10
     "operand-count-tests for sum-expr with a list of sum and mult and numbers")
  (check-equal?
     (operand-count SUM-WITH-EMPTY-SUM)
     10
     "operand-count-tests for sum-expr with empty-sum")
  (check-equal?
     (operand-count SUM-WITH-EMPTY-MULT)
     10
     "operand-count-tests for sum-expr with empty-mult")
  (check-equal?
     (operand-count SUM-WITH-EMPTY-SUM-EMPTY-MULT)
     0
     "operand-count-tests for sum-expr with a list of empty-sum and empty-mult")
  (check-equal?
     (operand-count SUM-EMPTY-SUM)
     0
     "operand-count-tests for sum-expr with only ")
  (check-equal?
     (operand-count SUM-EMPTY-MULT)
     0
     "operand-count-tests for sum-expr with a list of numbers") 
  
;; for mult with 2 level
  (check-equal?
     (operand-count MULT-OF-NON-EMPTY)
     10
     "operand-count-tests for mult-expr with a list of sum 
                      and mult and numbers")
  (check-equal?
     (operand-count MULT-WITH-EMPTY-SUM)
     10
     "operand-count-tests for mult-expr with empty-sum")
  (check-equal?
     (operand-count MULT-WITH-EMPTY-MULT)
     10
     "operand-count-tests for mult-expr with empty-mult")
  (check-equal?
     (operand-count MULT-WITH-EMPTY-SUM-EMPTY-MULT)
     0
     "operand-count-tests for mult-expr with a list 
                   of empty-sum and empty-mult")
  (check-equal?
     (operand-count MULT-EMPTY-SUM)
     0
     "operand-count-tests for mult-expr with only ")
  (check-equal?
     (operand-count MULT-EMPTY-MULT)
     0
     "operand-count-tests for mult-expr with a list of numbers") 
  
;; for three-level sum
  (check-equal?
     (operand-count SUM-NON-EMPTY-3)
     20
     "operand-count-tests for 3-level sum-expr with
                                         a list of sum and mult and numbers")
  (check-equal?
     (operand-count SUM-WITH-EMPTY-3)
     20
     "operand-count-tests for 3-level sum-expr with some 
                     empty-mult or empty-sum")
  (check-equal?
     (operand-count SUM-WITH-EMPTY-SUM-EMPTY-MULT-3)
     0
     "operand-count-tests for 3-level sum-expr with  
                 all empty-mult or empty-sum")
  (check-equal?
     (operand-count SUM-EMPTY-SUM-3)
     0
     "operand-count-tests for 3-level sum-expr with empty-sum only")

  (check-equal?
     (operand-count MULT-NON-EMPTY-3)
     20
     "operand-count-tests for 3-level mult-expr 
                  with a list of sum and mult and numbers")
  (check-equal?
     (operand-count MULT-WITH-EMPTY-3)
     20
     "operand-count-tests for 3-level mult-expr 
                      with some empty-mult or empty-sum")
  (check-equal?
     (operand-count MULT-WITH-EMPTY-SUM-EMPTY-MULT-3)
     0
     "operand-count-tests for 3-level mult-expr with 
                      all empty-mult or empty-sum")
  (check-equal?
     (operand-count MULT-EMPTY-MULT-3)
     0
     "operand-count-tests for 3-level mult-expr with empty-sum only")
)
(run-tests operand-count-tests)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expr-to-image : Expr -> Image
;; render the givn expression as an Image
;; example:(expr-to-image MULT-EMPTY-MULT-3)=MULT-EMPTY-MULT-3-IMAGE
;; strategy: structural decomposition on expr [Expr]
;(define (expr-to-image expr)
;    (cond
;        [(number? expr) (render-number expr)]
;        [(sum-exp? expr) (render-sum-loexpr (sum-exp-exprs expr))]
;        [(mult-exp? expr) (render-mult-loexpr (mult-exp-exprs expr))]))

;; strategy: higher order function composition
(define (expr-to-image expr)
   (expr-fold render-number render-sum-loexpr render-mult-loexpr expr))

;; render-number : Number -> Image
;; convent given Number to a Test Image
;; example: (render-number 5)=(text (number->string n) 20 "black")
;; strategy: domian knowledge
(define (render-number n)
    (text (number->string n) 20 "black" ))

;; render-sum-loexpr : LOExpr -> Image
;; render aLoExpr from sum-expr struct
;; example: (render-sum-loexpr (make-mul-expr 1 2)
;; strategy: function composition
(define (render-sum-loexpr loexpr)
            (beside/align "top" (text "(+ " 20 "black" ) 
                  (beside/align "bottom"  
                       (render-loexpr loexpr)
                       (text ")" 20 "black" ))))

;; render-mult-loexpr : LOExpr -> Image
;; render aLoExpr from sum-expr struct
;; example:(render-mult-loexpr (make-mul-expr 1 2))=
;; strategy: function composition
(define (render-mult-loexpr loexpr)
            (beside/align "top" (text "(* " 20 "black" ) 
                  (beside/align "bottom"  
                       (render-loexpr loexpr)
                       (text ")" 20 "black" ))))
;; render-loexpr : LOExpr -> Image
;; given the list of Expr, return a new image that combin
;; all elements inside the list
;; example: (render-loexpr (LIST 1 2 3 4))=NUM-LIST-IMAGE
;; strategy: structural decomposition on loexpr [LOExpr]
;(define (render-loexpr loexpr)
;    (cond
;        [(empty? loexpr) (empty-scene 0 0)]
;        [else 
;               (above
;                 (expr-to-image (first loexpr))
;                 (render-loexpr (rest loexpr)))]))
;; strategy: higher order function composition
(define (render-loexpr loexpr)
    (foldr above (empty-scene 0 0) (map expr-to-image loexpr)))

(define-test-suite expr-to-image-tests
  ;; 1-LEVEL OPERAND
  (check-equal?
     (expr-to-image CONSTANT)
     CONSTANT-IMAGE
     "expr-to-image test for expr with only number")
  (check-equal?
     (expr-to-image EMPTY-SUM)
     EMPTY-SUM-IMAGE
     "expr-to-image test for sum-expr without anything")
  (check-equal?
     (expr-to-image EMPTY-MULT)
     EMPTY-MULT-IMAGE
     "expr-to-image test for mult-expr without anything")
  (check-equal?
     (expr-to-image NUM-SUM)
     NUM-SUM-IMAGE
     "expr-to-image test for sum-expr with a list of numbers")
  (check-equal?
     (expr-to-image NUM-MULT)
     NUM-MULT-IMAGE
     "expr-to-image test for mult-expr with a list of numbers")
;;level-2
  (check-equal?
     (expr-to-image SUM-OF-NON-EMPTY)
     SUM-OF-NON-EMPTY-IMAGE
     "expr-to-image test for sum-expr with a list of sum and mult and numbers")
  (check-equal?
     (expr-to-image SUM-WITH-EMPTY-SUM)
     SUM-WITH-EMPTY-SUM-IMAGE
     "expr-to-image test for sum-expr with empty-sum")
  (check-equal?
     (expr-to-image SUM-WITH-EMPTY-MULT)
     SUM-WITH-EMPTY-MULT-IMAGE
     "expr-to-image test for sum-expr with empty-mult")
  (check-equal?
     (expr-to-image SUM-WITH-EMPTY-SUM-EMPTY-MULT)
     SUM-WITH-EMPTY-SUM-EMPTY-MULT-IMAGE
     "expr-to-image test for sum-expr with a list of empty-sum and empty-mult")
  (check-equal?
     (expr-to-image SUM-EMPTY-SUM)
     SUM-EMPTY-SUM-IMAGE
     "expr-to-image test for sum-expr with only ")
  (check-equal?
     (expr-to-image SUM-EMPTY-MULT)
     SUM-EMPTY-MULT-IMAGE
     "expr-to-image test for sum-expr with a list of numbers") 
  
;; for mult with 2 level
  (check-equal?
     (expr-to-image MULT-OF-NON-EMPTY)
     MULT-OF-NON-EMPTY-IMAGE
     "test for mult-expr with a list of sum and mult and numbers")
  (check-equal?
     (expr-to-image MULT-WITH-EMPTY-SUM)
     MULT-WITH-EMPTY-SUM-IMAGE
     "test for mult-expr with empty-sum")
  (check-equal?
     (expr-to-image MULT-WITH-EMPTY-MULT)
     MULT-WITH-EMPTY-MULT-IMAGE
     "test for mult-expr with empty-mult")
  (check-equal?
     (expr-to-image MULT-WITH-EMPTY-SUM-EMPTY-MULT)
     MULT-WITH-EMPTY-SUM-EMPTY-MULT-IMAGE
     "test for mult-expr with a list of empty-sum and empty-mult")
  (check-equal?
     (expr-to-image MULT-EMPTY-SUM)
     MULT-EMPTY-SUM-IMAGE
     "test for mult-expr with only ")
  (check-equal?
     (expr-to-image MULT-EMPTY-MULT)
     MULT-EMPTY-MULT-IMAGE
     "test for mult-expr with a list of numbers") 
;  
;; for three-level sum
  (check-equal?
     (expr-to-image SUM-NON-EMPTY-3)
     SUM-NON-EMPTY-3-IMAGE
     "expr-to-image test for 3-level sum-expr with a  
                          list of sum and mult and numbers")
  (check-equal?
     (expr-to-image SUM-WITH-EMPTY-3)
     SUM-WITH-EMPTY-3-IMAGE
     "expr-to-image test for 3-level sum-expr with some 
                           empty-mult or empty-sum")
  (check-equal?
     (expr-to-image SUM-WITH-EMPTY-SUM-EMPTY-MULT-3)
     SUM-WITH-EMPTY-SUM-EMPTY-MULT-3-IMAGE
     "expr-to-image test for 3-level sum-expr with all 
                         empty-mult or empty-sum")
  (check-equal?
     (expr-to-image SUM-EMPTY-SUM-3)
     SUM-EMPTY-SUM-3-IMAGE
     "expr-to-image test for 3-level sum-expr with empty-sum only")

  (check-equal?
     (expr-to-image MULT-NON-EMPTY-3)
     MULT-NON-EMPTY-3-IMAGE
     "expr-to-image test for 3-level mult-expr with a list of 
                              sum and mult and numbers")
  (check-equal?
     (expr-to-image MULT-WITH-EMPTY-3)
     MULT-WITH-EMPTY-3-IMAGE
     "expr-to-image test for 3-level mult-expr with
                            some empty-mult or empty-sum")
  (check-equal?
     (expr-to-image MULT-WITH-EMPTY-SUM-EMPTY-MULT-3)
     MULT-WITH-EMPTY-SUM-EMPTY-MULT-3-IMAGE
     "expr-to-image test for 3-level mult-expr with 
                         all empty-mult or empty-sum")
  (check-equal?
     (expr-to-image MULT-EMPTY-MULT-3)
     MULT-EMPTY-MULT-3-IMAGE
     "expr-to-image test for 3-level mult-expr with empty-sum only")
)
(run-tests expr-to-image-tests)



















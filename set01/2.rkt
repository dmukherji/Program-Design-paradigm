;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;;Write your solution for the second problem
;;of the first problem set in this file

(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")
(require 2htdp/image)

(provide make-diff-exp)
(provide make-mult-exp)
(provide diff-exp-rand1)
(provide diff-exp-rand2)
(provide mult-exp-rand1)
(provide mult-exp-rand2)
(provide expr-to-image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA DEFINITION
(define-struct diff-exp (rand1 rand2))
(define-struct mult-exp (rand1 rand2))
;;A Exp is one of
;;--(make-diff-exp Number Number)
;;--(make-mult-exp Number Number)
;;Interp.
;;a diff-exp represents a difference between the two inputs
;;a mult-exp represents a multiplication of the two inputs

;;TEMPLATE
;;diff-fn : diff-exp->??
;(define (diff-fn expr)
;  (...
;   (diff-exp-rand1 expr)
;   (diff-exp-rand2 expr)))

;;mult-fn : mult-exp->??
;(define (mult-fn expr)
;  (...
;   (mult-exp-rand1 expr)
;   (mult-exp-rand2 expr)))

;;expr-to-image : Expr Boolean->Image
;;This function renders a expr to an image 
;;the boolean value determines whether the expr should be
;;rendered as prefix or an infix expression
;;EXAMPLES:
;;(expr-to-image (make-diff-exp 12 30) true)= (12 - 33)
;;(expr-to-image (make-diff-exp 12 30) false)= (- 12 33)
;;(expr-to-image (make-mult-exp 12 30) true)= (12 * 33)
;;(expr-to-image (make-mult-exp 12 30) false)= (* 12 33)
;;STRATEGY : function composition

(define (expr-to-image expr infix?)
  (cond 
    [(diff-exp? expr)
     (if (false? infix?)
         (prefix-render (diff-exp-rand1 expr) (diff-exp-rand2 expr) "-")
         (infix-render (diff-exp-rand1 expr) (diff-exp-rand2 expr) "-"))]
    [(mult-exp? expr)
      (if (false? infix?)
          (prefix-render (mult-exp-rand1 expr) (mult-exp-rand2 expr) "*")
          (infix-render (mult-exp-rand1 expr) (mult-exp-rand2 expr) "*"))]))


;;Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;infix-render : Number Number String -> Image
;;produce an exp in a infix manner using the two numbers and the operand string
;;then, produce a image of the exp
;;Example:
;; (infix-render 3 4 "-")=(3 - 4)
;; (infix-render 3 4 "*")=(3 * 4)
;;STRATEGY : domain knowledge
(define (infix-render first second operand)
  (if (and (number? first) (number? second))
       (text (string-append "(" (number->string first) 
                            " " operand " "  
                            (number->string second) ")") 
             11 
             "black")
      "please enter number"))

;;prefix-render : Number Number String -> Image
;;produce an exp in a prefix manner using the two numbers and the operand string
;;then, produce a image of the exp
;;Example:
;;--(prefix-render 3 4 "-")=(- 3 4)
;;--(prefix-render 3 4 "*")=(* 3 4)
;;STRATEGY : domain knowledge
(define (prefix-render first second operand)
  (if (and (number? first) (number? second))
       (text (string-append 
                 "(" operand " " 
                 (number->string first) " " 
                 (number->string second) ")") 
             11 
             "black")
        "please enter number"))


;;TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite render-test
   (check-equal?
      (infix-render 3 4 "-")
      (text "(3 - 4)" 11 "black")
      "make infix exp")
   (check-equal?
      (infix-render -3 4 "-")
      (text "(-3 - 4)" 11 "black")
      "make infix exp negtive num")
   (check-equal?
      (infix-render 0 1 "-")
      (text "(0 - 1)" 11 "black")
      "make infix exp zero num")
   (check-equal?
      (prefix-render 3 4 "-")
      (text "(- 3 4)" 11 "black")
      "make prefix exp")
   (check-equal?
      (prefix-render -3 4 "-")
      (text "(- -3 4)" 11 "black")
      "make prefix exp negtive num")
   (check-equal?
      (prefix-render 0 1 "-")
      (text "(- 0 1)" 11 "black")
      "make prefix exp zero num"))

;;Test case design
;;two kinds of exp * two kinds of infix?
;;=four basic tests
(define-test-suite expr-to-image-test
   (check-equal?
      (expr-to-image (make-diff-exp 33 22) false)
      (text "(- 33 22)" 11 "black")
      "prefix diff")
   (check-equal?
      (expr-to-image (make-diff-exp 33 22) true)
      (text "(33 - 22)" 11 "black")
      "infix diff")
   (check-equal?
      (expr-to-image (make-mult-exp 33 22) false)
      (text "(* 33 22)" 11 "black")
      "prefix mult")
   (check-equal?
      (expr-to-image (make-mult-exp 33 22) true)
      (text "(33 * 22)" 11 "black")
      "infix mult")
   (check-equal?
      (expr-to-image (make-mult-exp "33" 22) false)
      "please enter number"
      "enter string")
   (check-equal?
      (expr-to-image (make-mult-exp true 22) true)
      "please enter number"
      "enter boolean"))

(run-tests render-test)
(run-tests expr-to-image-test)
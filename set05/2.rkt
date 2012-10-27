;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; The first accumulator is used to record the position
;; of the expression that will be rendered to image.
;; This accumulator is a list of numbers. 
;; Its length means the level of current expression in the whole expression
;; And the value of first element of accumulator means the position of  
;; current expression in its level.

;; For example, when the accumulator is (list 0 1 3),
;; the length of acc is 3 which means the current expression
;; is in the third level
;; the first element, which is 0, represents there are 0 expr left to process

;; 1)When program enters a new level of expr, add the number of exprs in this 
;; level to the front of accumulator. It represents how many exprs 
;; left to process

;; 2)after program draw one of the expr in the current level, minus the 
;; first number in accumulator by one.

;; The second accumulator, which is a Boolean, is used in function 
;; "number-of-right-parenthesis-needed" 
;; It records whether there are consecutive zeros at the front of the 
;; given list. And the number of consecutive zeros is the number of 
;; right parenthesis needed

;; 1)If the accumulator is true, means all numbers before this sub-list 
;; in the whole list is zero.
;; 2)If encounters a non-zero value, the accumulator will be false.

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)
(require "extras.rkt")

(provide expr-to-image)
(provide make-sum-exp)
(provide sum-exp-exprs)
(provide make-mult-exp)
(provide mult-exp-exprs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

;; The size of number and symbols
(define SIZE 12)
 
;; image of character '+'
(define PLUS (text "+" SIZE "black"))

;; image of character '*'
(define MULT (text "*" SIZE "black"))

;; image of character ' '
(define SPACE (text " " SIZE "black"))

;; image of character '('
(define LEFT-P (text "(" SIZE "black"))

;; image of character ')'
(define RIGHT-P (text ")" SIZE "black"))

;; The width of image "(+ "
(define SYMBOL-WIDTH-1 (image-width (beside LEFT-P PLUS SPACE)))
 
;; The width of image "(* "
(define SYMBOL-WIDTH-2 (image-width (beside LEFT-P MULT SPACE)))

;; The width of image "(+ )"
(define SYMBOL-WIDTH-3 (image-width (beside LEFT-P PLUS SPACE RIGHT-P)))

;; The width of image "(* )"
(define SYMBOL-WIDTH-4 (image-width (beside LEFT-P MULT SPACE RIGHT-P)))

;; The width of image ")"
(define SYMBOL-WIDTH-5 (image-width RIGHT-P))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITION:

(define-struct sum-exp (exprs))
;; A SumExp is a (make-sum-exp LOExpr)
;; interpretation:
;; exprs is a list of operands of an add operation

;; sum-exp-fn: sum-exp -> ??
; (define (sum-exp-fn s)
;   (... (sum-exp-exprs s)))


(define-struct mult-exp (exprs))
;; A MultExp is a (make-mult-exp LOExpr)
;; interpretation:
;; exprs is a list of operands of a multiply operation

;; mult-exp-fn: mult-exp -> ??
; (define (mult-exp-fn m)
;   (... (mult-exp-exprs m)))


;; An Expr is one of
;; -- Number
;; -- (make-sum-exp LOExpr)
;; -- (make-mult-exp LOExpr)
;; Interpretation: 
;; a Number is just a number
;; a SumExp represents a sum expression, 
;; and a MultExp refers to a multiplication expression

;; Expr-fn: Expr -> ??
;(define (Expr-fn e)
;  (cond
;    [(number? e) ...]
;    [(sum-exp? e) (...(LOExpr-fn (sum-exp-exprs e)))]
;    [(mult-exp? e) (...(LOExpr-fn (mult-exp-exprs e)))]))

;; A LOExpr is one of 
;; -- empty
;; -- (cons Expr LOExpr)
;; Intepretation:
;; empty means the list does not contain element
;; (cons Expr LOExpr) means the list contains one or
;; more Expr objects.

;; LOExpr-fn: LOExpr -> ??
;; (define (LOExpr-fn exprs)
;;   (cond
;;     [(empty? exprs) ...]
;;     [else (... (Expr-fn (first exprs)) (LOExpr-fn (rest exprs)))]))


;; A ListOf<Number>(LON) is one of 
;; -- empty
;; -- (cons Number LON)
;; Intepretation:
;; empty means the list does not contain element
;; (cons Number LON) means the list contains one or
;; more Number objects.

;; LON-fn: LON -> ??
;; (define (LON-fn lon)
;;   (cond
;;     [(empty? lon) ...]
;;     [else (... (first lon) (LON-fn (rest lon)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expr-to-image: Expr Number -> Image
;; Renders the given expr to image, according to the given width limit
;; Examples:
;; (expr-to-image 4 10) = (text (number->string 4) SIZE "black")
;; Strategy: Function Composition
(define (expr-to-image e w) 
  (expr-to-image-helper e w empty))

;; expr-to-image-helper : Expr Number LON -> Image
;; Converts the given expr (Expr) to image, according to the given 
;; width limit(Number) and LON which records the position of Expr
;; Examples:
;; (expr-to-image-helper 4 10 empty) = 
;; (text (number->string 4) SIZE "black")
;; Strategy: Structural Decomposition[e:Expr]
(define (expr-to-image-helper e w lon)
  (cond
    [(number? e) (number-to-image e w lon)]
    [(sum-exp? e) (sum-exp-to-image (sum-exp-exprs e) w lon)]             
    [(mult-exp? e) (mult-exp-to-image (mult-exp-exprs e) w lon)]))

;; number-to-image: Number Number LON -> Image
;; Converts the first number to image, according to the given 
;; width limit (2nd Number).
;; If there is no enough space to hold the image, gives an error message.
;; Examples:
;; (number-to-image 4 20 (list 5 3 3)) = 
;; (text (number->string 4) SIZE "black")
;; Strategy: Function Composition
(define (number-to-image num w lon) 
  (if (number-enough-space? num w lon)
      (number-to-image-helper num lon)
      (error "not enough room"))) 

;; number-to-image-helper: Number LON -> Image
;; Converts the given number to image based on the position (LON)
;; Examples:
;; (number-to-image-helper 4 (list 5 3 3)) = 
;; (text (number->string 4) SIZE "black")
;; Strategy: Function Composition
(define (number-to-image-helper num lon)
  (if (last-number? lon)
      (beside 
       (text (number->string num) SIZE "black") 
       (add-parenthesis (number-of-right-parenthesis-needed lon true)))
      (text (number->string num) SIZE "black")))

;; sum-exp-to-image: LOExpr Number LON -> Image
;; Converts the given loexpr (LOExpr) to image, according to the
;; given width limit (Number) and the position (LON) of current loexpr
;; The operation is sum.
;; Examples:
;; (sum-exp-to-image empty 30 (list 3 4 4))
;; = (beside LEFT-P PLUS SPACE RIGHT-P)
;; Strategy: Function Composition
(define (sum-exp-to-image loexpr w lon)
  (if (> SYMBOL-WIDTH-1 w)
      (error "not enough room")
      (beside/align 
       "top" 
       LEFT-P 
       PLUS 
       SPACE 
       (loexpr-to-image
        loexpr (- w SYMBOL-WIDTH-1) (add-level lon loexpr)))))

;; mult-exp-to-image: LOExpr Number LON -> Image
;; Converts the given loexpr to image, according to the
;; given width limit (Number) and positionm (LON) of LOExpr
;; The operation is multiplication
;; Examples:
;; (mult-exp-to-image empty 30 (list 3 4 4))
;; = (beside LEFT-P MULT SPACE RIGHT-P)
;; Strategy: Function Composition
(define (mult-exp-to-image loexpr w lon)
  (if (> SYMBOL-WIDTH-2 w)
      (error "not enough room")
      (beside/align 
       "top" 
       LEFT-P
       MULT 
       SPACE 
       (loexpr-to-image
        loexpr (- w SYMBOL-WIDTH-2) (add-level lon loexpr)))))
 
;; loexpr-to-image : LOExpr Number LON -> Image
;; Converts the given list of expr to image, according to the
;; given width limit and position (LON)
;; Examples:
;; (loexpr-to-image (list 4 5) 50 (list 3 2))
;; =(beside 
;;   (text (number->string 4) SIZE "black") 
;;   SPACE 
;;   (text (number->string 5) SIZE "black"))
;; Strategy: Structural Decomposition[loexpr:LOExpr]
(define (loexpr-to-image loexpr w lon)
  (cond
    [(empty? loexpr) (empty-loexpr-to-image w lon)]
    [else (if (loexpr-enough-space? loexpr w lon)
              (loexpr-to-image-one-line loexpr w lon)
              (loexpr-to-image-stacked loexpr w lon))]))

;; empty-loexpr-to-image : Number LON -> Image
;; Given an empty, converts it to image. If there is no enough
;; space to hold this image, gives an error message.
;; Examples:
;; (empty-loexpr-to-image 30 (list 0 1)) = RIGHT-P
;; Strategy: Function Composition
(define (empty-loexpr-to-image w lon)
  (if (> (* (number-of-right-parenthesis-needed lon true) SYMBOL-WIDTH-5) w)
      (error "not enough room")
      (add-parenthesis (number-of-right-parenthesis-needed lon true))))

;; loexpr-to-image-one-line : LOExpr Number LON -> Image
;; GIVEN a sublist loexpr (LOExpr), a width limit (Number)
;;       and position (LON) of LOExpr
;; WHERE loexpr is the sublist of some list lst0 from the 
;;       n-th (n is the first element of given LON) last element to end 
;; PRODUCE a image that portrays the loexpr 
;; Examples:
;; (loexpr-to-image-one-line (list 4 5) 50 (list 3 2))
;; =(beside 
;;   (text (number->string 4) SIZE "black") 
;;   SPACE 
;;   (text (number->string 5) SIZE "black"))
;; Strategy: Structural Decomposition[loexpr :LOExpr] + accumulator[lon]
(define (loexpr-to-image-one-line loexpr w lon)
  (cond
    [(empty? loexpr) RIGHT-P]
    [else (add-image-horizontal
           (expr-to-image-helper (first loexpr) w (update-position lon))
           (loexpr-to-image-one-line (rest loexpr) w  (update-position lon)))]))

;; loexpr-to-image-stacked : LOExpr Number LON -> Image
;; GIVEN a sublist loexpr (LOExpr), a width limit (Number)
;;       and position (LON) of LOExpr
;; WHERE loexpr is the sublist of some list lst0 from the 
;;       n-th (n is the first element of given LON) last element to end 
;; PRODUCE a image that portrays the loexpr vertically 
;; Examples:
;; (loexpr-to-image-stacked (list 4 5) 50 (list 3 2))
;; =(above/align "left" 
;;   (text (number->string 4) SIZE "black") 
;;   (text (number->string 5) SIZE "black"))
;; Strategy: Structural Decomposition[loexpr :LOExpr] + accumulator[lon]
(define (loexpr-to-image-stacked loexpr w lon)
  (cond
    [(empty? loexpr) RIGHT-P] 
    [else (add-image-vertical
           (expr-to-image-helper (first loexpr) w (update-position lon))
           (loexpr-to-image-stacked (rest loexpr) w (update-position lon)))]))
 
;; add-image-horizontal: Image Image -> Image
;; Combines given two image horizontally
;; Examples:
;; (add-image-horizontal LEFT-P RIGHT-P) = LEFT-P
;; Strategy: Function Composition
(define (add-image-horizontal i1 i2)
  (if (image=? i2 RIGHT-P) 
      i1
      (beside i1 SPACE i2)))
      
;; add-image-vertical: Image Image -> Image
;; Combines given two image vertically
;; Examples:
;; (add-image-vertical LEFT-P RIGHT-P) = LEFT-P
;; Strategy: Function Composition
(define (add-image-vertical i1 i2)
  (if (image=? i2 RIGHT-P) 
      i1 
      (above/align "left" i1 i2)))

;; expr-length : Expr -> Number
;; Calculates the image length in pixels if render the 
;; given expr as an image
;; Examples:
;; (expr-length (make-sum-exp (list 4 5 6))) = 45
;; Strategy: Structural Decomposition[e:Expr]
(define (expr-length e)
  (cond
    [(number? e) (number-length e)]
    [(sum-exp? e) (+ (loexp-length (sum-exp-exprs e)) SYMBOL-WIDTH-3)]
    [(mult-exp? e) (+ (loexp-length (mult-exp-exprs e)) SYMBOL-WIDTH-4)])) 
 
;; number-length: Number -> Number
;; Calculates the image length in pixels if render the 
;; given number as an image
;; Examples: 
;; (number-length 44) = 14
;; Strategy: Function Composition
(define (number-length n)
  (image-width (text (number->string n) SIZE "black")))
 
;; loexp-length : LOExpr -> Number 
;; Calculates the image length in pixels if renders the 
;; given list of loexpr to an image horizontally. 
;; if loexpr is empty, return 0
;; else return the content length plus space length
;; Examples:
;; (loexp-length (list 3 4 5)) = 27
;; Strategy: Function Composition
(define (loexp-length loexpr)
  (if (= (loexp-length-helper loexpr) 0)  
      0
      (+ (loexp-length-helper loexpr) 
         (* (sub1 (length loexpr)) (image-width SPACE)))))

;; loexp-length-helper: LOExpr -> Number
;; Calculates the image length in pixels if renders the
;; given list of loexpr to an image horizontally. 
;; Examples: 
;; (loexp-length-helper (list 3 4 5)) = 21
;; Strategy: Structural Decomposition[loexpr:LOExpr]
(define (loexp-length-helper loexpr)
  (cond
    [(empty? loexpr) 0]
    [else (+ (expr-length (first loexpr)) 
             (loexp-length-helper (rest loexpr)))]))

;; add-level: LON LOExpr -> LON
;; Extends the given LON by adding an element to its beginning, 
;; the value of this element is the length of the given LOExpr
;; Examples:
;; (add-level (list 3 4 5) (list 3 1)) = (list 2 3 4 5)
;; Strategy: Function Composition
(define (add-level lon loe)
  (cons (length loe) lon))

;; update-position : LON -> LON
;; If the given LON is empty, returns empty. 
;; Otherwise, returns the given list with its first element substracted by 1
;; Examples:
;; (update-position (list 3 4 5)) = (list 2 4 5)
;; (update-position empty) = empty
;; Strategy: Structural Decomposition[lon:LON]
(define (update-position lon)
  (cond
    [(empty? lon) empty]
    [else (cons (sub1 (first lon)) (rest lon))]))
 
;; add-parenthesis: Number -> Image
;; Returns a image that contains a number of right parenthesis, the exact
;; number of parenthesis is decided by the given number
;; Examples:
;; (add-parenthesis 3) = (beside RIGHT-P RIGHT-P RIGHT-P)
;; Termination Argument: The given number will be substracted by 1 in
;; each recursion. When it equals to one, the recursion will be terminated.
;; Strategy: General Recursion
(define (add-parenthesis n)
  (if (= n 1) 
      RIGHT-P 
      (beside RIGHT-P (add-parenthesis (sub1 n)))))

;; number-of-right-parenthesis-needed : LON Boolean -> Number
;; Given a sublist lon 
;; WHERE lon is the sublist of some list lon0. The value of b is true means 
;; there are consecutive zeros from the beginning of LON.
;; PRODUCES the number of consecutive zeros from the first element of LON
;; Examples: 
;; (number-of-right-parenthesis-needed (list 0 0 1 0) true) = 2
;; Strategy: Structural Decomposition[lon:LON] + accumulator[b]
(define (number-of-right-parenthesis-needed lon b)
  (cond
    [(empty? lon) 0]
    [else
     (if (and b (= (first lon) 0))
         (+ 1 (number-of-right-parenthesis-needed (rest lon) true))
         (number-of-right-parenthesis-needed (rest lon) false))]))

;; number-enough-space? : Number Number LON -> Boolean
;; Checks whether or not the width of image that converted by the given number
;; is smaller than or equal to the given width limit
;; Examples:
;; (number-enough-space? 45 10 (list 4 5)) = false
;; Strategy: Function Composition
(define (number-enough-space? num w lon)
  (<= (+ (number-length num)
         (* (number-of-right-parenthesis-needed lon true) SYMBOL-WIDTH-5)) w))

;; loexpr-enough-space? : LOExpr Number LON -> Boolean
;; Checks whether or not the width of image that converted
;; by the given loexpr (LOExpr)
;; is smaller than or equal to the given width limit (Number)
;; Examples:
;; (loexpr-enough-space? (list 4 5) 10 (list 4 5)) = false
;; Strategy: Structural Decomposition[lon:LON]
(define (loexpr-enough-space? loexpr w lon)
  (cond
    [(empty? lon) false]
    [else 
     (<= (+ (loexp-length loexpr) 
            (* (add1 (number-of-right-parenthesis-needed (rest lon) true)) 
               SYMBOL-WIDTH-5)) w)]))

;; last-number? : LON -> Boolean
;; Checks whether or not the first element of given LON is zero.
;; Examples:
;; (last-number? (list 5 6 2)) = false
;; (last-number? (list 0 6 2)) = true
;; Strategy: Structural Decomposition[lon:LON]
(define (last-number? lon)
  (cond 
    [(empty? lon) false] 
    [else (= (first lon) 0)]))  
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;tests:
;; normal loexpr without any empty expr
(define EXPR-WITHOUT-EMPTY (make-mult-exp
               (list
                (make-sum-exp (list 22 3333 44))
                (make-mult-exp
                 (list
                  (make-sum-exp (list 66 67 68))
                  (make-mult-exp (list 42 43))))
                (make-mult-exp (list 77 88))))) 
;; normal loexpr with some empty expr
(define EXPR-WITH-EMPTY (make-mult-exp
               (list
                (make-sum-exp (list 22 3333 44))
                (make-mult-exp
                 (list
                  (make-sum-exp (list 66 67 68))
                  (make-mult-exp (list 42 43))
                  (make-sum-exp empty)))
                (make-mult-exp (list 77 88))
                (make-mult-exp empty)))) 
;; empty expr
(define EMPTY-SUM-EXPR (make-sum-exp empty))
(define EMPTY-MULT-EXPR (make-mult-exp empty))
;; all kinds of mark image (e.g (* ), (+ ))
(define SUM-MARK-IMAGE (text "(+ " SIZE "black" ))
(define MULT-MARK-IMAGE (text "(* " SIZE "black" ))
(define CONSTANT 5)
(define CONSTANT-IMAGE (text (number->string CONSTANT) SIZE "black" ))
(define EMPTY-SUM (make-sum-exp empty));;0
(define EMPTY-SUM-IMAGE (text "(+ )" SIZE "black" ));;18
(define EMPTY-MULT (make-mult-exp empty));;
(define EMPTY-MULT-IMAGE (text "(* )" SIZE "black" ));;16()
;; put number list 1 horizontally
(define NUM-LIST-1-ABOVE
    (beside/align "top" 
     (text "(+ " SIZE "black")
        (above/align "left"
                  (text (number->string 22) SIZE "black" )
                  (text (number->string 3333) SIZE "black" )
                  (text (string-append (number->string 44) ")") SIZE "black"))))
;; put number list 1 vertically
(define NUM-LIST-1-BESIDE
  (text (string-append "(+ " (number->string 22) " " (number->string 3333) 
                       " " (number->string 44) ")") SIZE "black" ))
;; put number list 2 horizontally
(define NUM-LIST-2-ABOVE
    (beside/align "top" 
     (text "(+ " SIZE "black")
     (above/align "left"
                  (text (number->string 66) SIZE "black" )
                  (text (number->string 67) SIZE "black" )
                  (text (string-append (number->string 68) ")") SIZE "black"))))
;; put number list 2 vertically
(define NUM-LIST-2-BESIDE
  (text (string-append "(+ " (number->string 66) " " (number->string 67) " " 
                       (number->string 68) ")") SIZE "black" ))
;; put number list 3 horizontally
(define NUM-LIST-3-ABOVE
  (beside/align "top" 
        (text "(* " SIZE "black")
        (above/align "left"
                     (text (number->string 42) SIZE "black" )
                     (text (string-append (number->string 43) ")") 
                           SIZE "black"))))
;; put number list 2 vertically
(define NUM-LIST-3-BESIDE
  (text (string-append "(* " (number->string 42) " " (number->string 43) ")")
        SIZE "black" ))
;; put number list 4 horizontally
(define NUM-LIST-4-ABOVE
  (beside/align "top" 
        (text "(* " SIZE "black")
        (above/align "left"
                     (text (number->string 77) SIZE "black" )
                     (text (string-append (number->string 88) ")") 
                           SIZE "black"))))
;; put number list 4 vertically
(define NUM-LIST-4-BESIDE
  (text (string-append "(* " (number->string 77) " " 
                       (number->string 88) ")") SIZE "black" ))

;; define image constant for the same expr but different width limit

;; when expr can fit in the width limit, put it horizontally
(define ONE-LINE-IMAGE 
    (beside MULT-MARK-IMAGE NUM-LIST-1-BESIDE SPACE MULT-MARK-IMAGE 
            NUM-LIST-2-BESIDE SPACE NUM-LIST-3-BESIDE RIGHT-P 
            SPACE NUM-LIST-4-BESIDE RIGHT-P))


;; when expr cannot fit in the width limit, stack the expr with longest length
;; result is a three line image
(define THREE-LINE-IMAGE ;;144
    (beside/align "top" 
                  MULT-MARK-IMAGE
                  (above/align "left"
                               NUM-LIST-1-BESIDE
                               (beside MULT-MARK-IMAGE NUM-LIST-2-BESIDE 
                                       SPACE NUM-LIST-3-BESIDE RIGHT-P)
                               (beside NUM-LIST-4-BESIDE RIGHT-P))))

;; when expr cannot fit in the width limit, stack the longest expr
;; result is a four line image
(define FOUR-LINE-IMAGE ;;92
    (beside/align "top" 
          MULT-MARK-IMAGE
          (above/align "left"
                       NUM-LIST-1-BESIDE
                       (beside/align "top" 
                              MULT-MARK-IMAGE 
                              (above/align "left"
                                    NUM-LIST-2-BESIDE
                                    (beside NUM-LIST-3-BESIDE RIGHT-P)))
                       (beside NUM-LIST-4-BESIDE RIGHT-P))))
;; when expr cannot fit in the width limit, stack the longest expr
;; result is a six line image
(define SIX-LINE-IMAGE ;;90
  (beside/align "top" 
         MULT-MARK-IMAGE
         (above/align "left"
                NUM-LIST-1-ABOVE
                (beside/align "top"
                       MULT-MARK-IMAGE 
                       (above/align "left"
                                    NUM-LIST-2-BESIDE
                                    (beside NUM-LIST-3-BESIDE RIGHT-P)))
                (beside NUM-LIST-4-BESIDE RIGHT-P))))
;; when expr cannot fit in the width limit, stack the longest expr
;; result is a eight line image
(define EIGHT-LINE-IMAGE ;;75
    (beside/align "top" 
           MULT-MARK-IMAGE
           (above/align "left"
                     NUM-LIST-1-ABOVE
                     (beside/align "top"
                              MULT-MARK-IMAGE 
                              (above/align "left"
                                     NUM-LIST-2-ABOVE
                                     (beside NUM-LIST-3-BESIDE RIGHT-P)))
                               (beside NUM-LIST-4-BESIDE RIGHT-P))))
;; when expr cannot fit in the width limit, stack the longest expr
;; result is a nine line image
(define NINE-LINE-IMAGE ;;63
    (beside/align "top" 
           MULT-MARK-IMAGE
           (above/align "left"
                  NUM-LIST-1-ABOVE
                  (beside/align "top"
                        MULT-MARK-IMAGE 
                        (above/align "left"
                            NUM-LIST-2-ABOVE
                            (beside/align "bottom" NUM-LIST-3-ABOVE RIGHT-P)))
                               (beside NUM-LIST-4-BESIDE RIGHT-P))))
;; when expr cannot fit in the width limit, stack all expr
;; result is a ten line image
(define TEN-LINE-IMAGE ;;58
    (beside/align "top" 
           MULT-MARK-IMAGE
           (above/align "left"
                  NUM-LIST-1-ABOVE
                  (beside/align "top"
                      MULT-MARK-IMAGE 
                      (above/align "left"
                          NUM-LIST-2-ABOVE
                          (beside/align "bottom" NUM-LIST-3-ABOVE RIGHT-P)))
                  (beside/align "bottom" NUM-LIST-4-ABOVE RIGHT-P))))

;; with empty
;; define some image constant with empty exprs involved
(define ONE-LINE-WITH-EMPTY-IMAGE 
    (beside MULT-MARK-IMAGE NUM-LIST-1-BESIDE SPACE MULT-MARK-IMAGE 
            NUM-LIST-2-BESIDE SPACE NUM-LIST-3-BESIDE SPACE EMPTY-SUM-IMAGE
            RIGHT-P SPACE NUM-LIST-4-BESIDE SPACE EMPTY-MULT-IMAGE RIGHT-P))

(define THREE-LINE-WITH-EMPTY-IMAGE ;;165
    (beside/align "top" 
           MULT-MARK-IMAGE
           (above/align "left"
                    NUM-LIST-1-BESIDE
                    (beside MULT-MARK-IMAGE NUM-LIST-2-BESIDE 
                            SPACE NUM-LIST-3-BESIDE SPACE 
                            EMPTY-SUM-IMAGE RIGHT-P)
                            NUM-LIST-4-BESIDE
                            (beside EMPTY-MULT-IMAGE RIGHT-P))))

(define SIX-LINE-WITH-EMPTY-IMAGE ;;92
    (beside/align "top" 
          MULT-MARK-IMAGE
          (above/align "left"
                 NUM-LIST-1-BESIDE
                 (beside/align "top" 
                        MULT-MARK-IMAGE
                        (above/align "left"
                                   NUM-LIST-2-BESIDE
                                   NUM-LIST-3-BESIDE
                                   (beside EMPTY-SUM-IMAGE RIGHT-P)))
                 
                 NUM-LIST-4-BESIDE
                               (beside EMPTY-MULT-IMAGE RIGHT-P))))

(define EIGHT-LINE-WITH-EMPTY-IMAGE ;;90
  (beside/align "top" 
                MULT-MARK-IMAGE
                (above/align 
                 "left"
                 NUM-LIST-1-ABOVE
                 (beside/align 
                  "top" 
                  MULT-MARK-IMAGE
                  (above/align 
                   "left"
                   NUM-LIST-2-BESIDE
                   NUM-LIST-3-BESIDE
                   (beside EMPTY-SUM-IMAGE RIGHT-P)))
                 
                 NUM-LIST-4-BESIDE
                 (beside EMPTY-MULT-IMAGE RIGHT-P))))

(define TEN-LINE-WITH-EMPTY-IMAGE ;;71
  (beside/align 
   "top" 
   MULT-MARK-IMAGE
   (above/align 
    "left"
    NUM-LIST-1-ABOVE
    (beside/align 
     "top" 
     MULT-MARK-IMAGE
     (above/align 
      "left"
      NUM-LIST-2-ABOVE
      NUM-LIST-3-BESIDE
      (beside EMPTY-SUM-IMAGE RIGHT-P)))
    
    NUM-LIST-4-BESIDE
    (beside EMPTY-MULT-IMAGE RIGHT-P))))

(define ELEVEN-LINE-WITH-EMPTY-IMAGE ;;59
  (beside/align 
   "top" 
   MULT-MARK-IMAGE
   (above/align 
    "left"
    NUM-LIST-1-ABOVE
    (beside/align 
     "top" 
     MULT-MARK-IMAGE
     (above/align 
      "left"
      NUM-LIST-2-ABOVE
      NUM-LIST-3-ABOVE
      (beside EMPTY-SUM-IMAGE RIGHT-P)))
    
    NUM-LIST-4-BESIDE
    (beside EMPTY-MULT-IMAGE RIGHT-P))))
(define TWELVE-LINE-WITH-EMPTY-IMAGE ;;56
  (beside/align 
   "top" 
   MULT-MARK-IMAGE
   (above/align 
    "left"
    NUM-LIST-1-ABOVE
    (beside/align 
     "top" 
     MULT-MARK-IMAGE
     (above/align
      "left"
      NUM-LIST-2-ABOVE
      NUM-LIST-3-ABOVE
      (beside EMPTY-SUM-IMAGE RIGHT-P)))
    
    NUM-LIST-4-ABOVE
    (beside EMPTY-MULT-IMAGE RIGHT-P))))

;; define constant containing only empty exprs
(define ALL-EMPTY 
  (make-sum-exp
   (list
    (make-mult-exp empty)
    (make-sum-exp (list (make-mult-exp empty) (make-sum-exp empty))))))
;; define image constant that puts all empty exprs in one line
(define ONE-LINE-EMPTY 
  (beside SUM-MARK-IMAGE EMPTY-MULT-IMAGE SPACE SUM-MARK-IMAGE 
          EMPTY-MULT-IMAGE SPACE EMPTY-SUM-IMAGE RIGHT-P RIGHT-P));;92
;; define image constant that puts all empty exprs in two line
(define TWO-LINE-EMPTY ;;73
  (beside/align 
   "top"
   SUM-MARK-IMAGE
   (above/align 
    "left"
    EMPTY-MULT-IMAGE
    (beside SUM-MARK-IMAGE EMPTY-MULT-IMAGE SPACE 
            EMPTY-SUM-IMAGE RIGHT-P RIGHT-P))))
;; define image constant that puts all empty exprs in three lines
(define THREE-LINE-EMPTY ;;54
  (beside/align 
   "top"
   SUM-MARK-IMAGE
   (above/align 
    "left"
    EMPTY-MULT-IMAGE
    (beside/align 
     "top"
     SUM-MARK-IMAGE
     (above/align 
      "left"
      EMPTY-MULT-IMAGE
      (beside EMPTY-SUM-IMAGE RIGHT-P RIGHT-P))))))

(define-test-suite expr-to-image-tests
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 300)
     ONE-LINE-IMAGE
     "test when normally expr (without empty expr) 
                       can be shown in one line")
  
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 281)
     ONE-LINE-IMAGE
     "test when normally expr (without empty expr) can be 
                       shown in one line with smallest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 280)
     THREE-LINE-IMAGE
      "test when normally expr (without empty expr) can
                        be shown in three line with largest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 144)
     THREE-LINE-IMAGE
      "test when normally expr (without empty expr)
                        can be shown in three line with smallest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 92)
     FOUR-LINE-IMAGE
      "test when normally expr (without empty expr) 
                       can be shown in four line with smallest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 91)
     SIX-LINE-IMAGE
     "test when normally expr (without empty expr) 
                       can be shown in six line with largest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 90)
     SIX-LINE-IMAGE
     "test when normally expr (without empty expr) 
                       can be shown in six line with smallest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 89)
     EIGHT-LINE-IMAGE
     "test when normally expr (without empty expr) 
                       can be shown in eight line with largest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 75)
     EIGHT-LINE-IMAGE
     "test when normally expr (without empty expr) 
                       can be shown in eight line with smallest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 74)
     NINE-LINE-IMAGE
     "test when normally expr (without empty expr)
                        can be shown in nine line with largest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 63)
     NINE-LINE-IMAGE
     "test when normally expr (without empty expr) 
                       can be shown in nine line with smallest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 62)
     TEN-LINE-IMAGE
     "test when normally expr (without empty expr)
                        can be shown in ten line with largest width limit")
  (check-equal?
     (expr-to-image EXPR-WITHOUT-EMPTY 58)
     TEN-LINE-IMAGE
     "test when normally expr (without empty expr) 
          can be shown in ten line with smallest width limit
          it is also the minimal possible width of the image")
  
  (check-equal?
     (expr-to-image CONSTANT 144)
     CONSTANT-IMAGE
     "test only one number with enough space")
  (check-equal?
     (expr-to-image EXPR-WITH-EMPTY 350)
     ONE-LINE-WITH-EMPTY-IMAGE
     "test when normally expr (with some empty expr) 
                       can be shown in one line")
  (check-equal?
     (expr-to-image EXPR-WITH-EMPTY 165)
     THREE-LINE-WITH-EMPTY-IMAGE
     "test when normally expr (with some empty expr)
                        can be shown in three line")

  (check-equal?
     (expr-to-image EXPR-WITH-EMPTY 92)
     SIX-LINE-WITH-EMPTY-IMAGE
     "test when normally expr (with some empty expr) 
                       can be shown in six line")
  (check-equal?
     (expr-to-image EXPR-WITH-EMPTY 90)
     EIGHT-LINE-WITH-EMPTY-IMAGE
     "test when normally expr (with some empty expr) 
                       can be shown in eight line")
  (check-equal?
     (expr-to-image EXPR-WITH-EMPTY 71)
     TEN-LINE-WITH-EMPTY-IMAGE
     "test when normally expr (with some empty expr) 
                       can be shown in ten line")
  (check-equal?
     (expr-to-image EXPR-WITH-EMPTY 59)
     ELEVEN-LINE-WITH-EMPTY-IMAGE
      "test when normally expr (with some empty expr) 
                       can be shown in eleven line")
  (check-equal?
     (expr-to-image EXPR-WITH-EMPTY 56)
     TWELVE-LINE-WITH-EMPTY-IMAGE
     "test when normally expr (with some empty expr)
                        can be shown in 12 line")
  
  (check-equal?
     (expr-to-image ALL-EMPTY 92)
     ONE-LINE-EMPTY
     "test when expr with all empty expr can be shown in one line")
  (check-equal?
     (expr-to-image ALL-EMPTY 91)
     TWO-LINE-EMPTY
     "test when expr with all empty expr can be shown in two line")
  (check-equal?
     (expr-to-image ALL-EMPTY 90)
     TWO-LINE-EMPTY
     "test when expr with all empty expr can be shown in 
                       two line with largest possible width limit")
  (check-equal?
     (expr-to-image ALL-EMPTY 73)
     TWO-LINE-EMPTY
     "test when expr with all empty expr can be shown in 
                       two line with smallest possible width limit")
  (check-equal?
     (expr-to-image ALL-EMPTY 72)
     THREE-LINE-EMPTY
     "test when expr with all empty expr can be shown 
                       in three line with largest possible width limit")
  (check-equal?
     (expr-to-image ALL-EMPTY 54)
     THREE-LINE-EMPTY
     "test when expr with all empty expr can be shown 
                       in three line with smallest possible width limit")
;; tests for expr with only one layer of empty expr  
  (check-equal?
     (expr-to-image EMPTY-SUM-EXPR 165)
     EMPTY-SUM-IMAGE
     "test only one empty sum expr with enough space")
  
  (check-equal?
     (expr-to-image EMPTY-MULT-EXPR 350)
     EMPTY-MULT-IMAGE
     "test only one empty mult expr with enough space")
)
(run-tests expr-to-image-tests)

(define-test-suite loexpr-enough-space?-tests 
  (check-equal?
     (loexpr-enough-space? EXPR-WITH-EMPTY 100 empty)
     false
     "test loexpr-enough-space? that is not covered by main function"))
(run-tests loexpr-enough-space?-tests)

(define-test-suite update-position-tests 
  (check-equal?
     (update-position empty)
     empty
     "test update-position that is not covered by main function"))
(run-tests update-position-tests)

;; "test FOR normal expr without empty expr without enough space"
(check-error (expr-to-image EXPR-WITHOUT-EMPTY 10))
;; "test FOR normal expr with multiple empty expr without enough space"
(check-error (expr-to-image EXPR-WITH-EMPTY 10))
;; "test FOR multiple empty expr without enough space"
(check-error (expr-to-image EMPTY-MULT-EXPR 10))
;; "test FOR only one empty expr with negtive amount of enough space"
(check-error (expr-to-image EMPTY-SUM-EXPR -10))
;; "test FOR only one empty expr with 0 space available"
(check-error (expr-to-image EMPTY-SUM-EXPR 0))
;; "test FOR only one empty expr without enough space"
(check-error (expr-to-image EMPTY-SUM-EXPR 15))
;; "test only one number without enough space"
(check-error (expr-to-image CONSTANT 1))
;; "test FOR expr with all empty expr without enough space"
(check-error (expr-to-image ALL-EMPTY 10))

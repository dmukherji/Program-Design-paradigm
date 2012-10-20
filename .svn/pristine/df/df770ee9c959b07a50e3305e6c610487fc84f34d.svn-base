;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname finger-exercise) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp")))))
(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)
;;(require "extras.rkt")
;; DATA DEFINITION
;; A List-of-numbers (LON) is one of:
;; – empty
;; – (cons Number List-of-numbers)
;;Interp:
;;LON a sequence with no elements
;;LON can be 

;;TEMPLATE
;;lon-fn : List-of-numbers -> ??
;(define (lon-fn lon)
;  (cond 
;     [(empty? lon) ...]
;     [else (... (first lon)
;                (lon-fn (rest lon)))]))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pos? : List-of-numbers -> Boolean
;; pos? determines whether all the elemnts in input are positive number
;;EXAMPLES: 
;;(pos? (list 1 2 3 4)) =true
;;(pos? (list -1 2 3 4)) =false
;;STRATEGY : function composition
(define (pos? lon)
  (cond 
     [(empty? lon) true]
     [else (and (positive? (first lon))
                (pos? (rest lon)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ALL_POSITIVE (list 1 2 3 4))
(define NOT_ALL_POSITIVE (list -1 2 3 4))
(define HAS_ZERO (list 1 2 0 4))
(define-test-suite pos?-test
  (check-equal?
     (pos? ALL_POSITIVE)
     true))

(run-tests pos?-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITION
;; A List-of-booleans (LOB) is one of:
;; – empty
;; – (cons Boolean List-of-booleans)
;;Interp:
;;LOB a sequence with no elements
;;LOB can be a list with a boolean as the first element and a list as the seocnd

;;TEMPLATE
;;lob-fn : List-of-booleans -> ??
;(define (lob-fn lob)
;  (cond 
;     [(empty? lob) ...]
;     [else (... (first lob)
;                (lob-fn (rest lob)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all-true : List-of-Boolean -> Boolean
;; all-true determines whether all the elemnts in input are true
;;EXAMPLES: 
;;(all-true (list true false true))=false
;;(all-true (list true true true))=true
;;STRATEGY : function composition
(define (all-true lob)
  (cond 
     [(empty? lob) true]
     [else (and (first lob)
                (all-true (rest lob)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ALL_TRUE (list  true true true))
(define NOT_ALL_TRUE (list  true true false))
(define EMPTY empty)
(define-test-suite all-true-test
  (check-equal?
     (all-true ALL_TRUE)
     true)
  (check-equal?
     (all-true NOT_ALL_TRUE)
     false)
  (check-equal?
     (all-true EMPTY)
     true))

(run-tests all-true-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; one-true : List-of-Boolean -> Boolean
;; one-true determines whether there is element in input are true
;;EXAMPLES: 
;;(all-true (list true false true))=true
;;(all-true (list true true true))=true
;;STRATEGY : function composition
(define (one-true lob)
  (cond 
     [(empty? lob) true]
     [else (or (first lob)
                (one-true (rest lob)))]))
(define-test-suite one-true-test
  (check-equal?
     (one-true ALL_TRUE)
     true)
  (check-equal?
     (one-true NOT_ALL_TRUE)
     true)
  (check-equal?
     (one-true EMPTY)
     true))

(run-tests one-true-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITION
;; A List-of-string (LOS) is one of:
;; – empty
;; –(cons Boolean List-of-string)
;;Interp:
;;LOS a sequence with no elements
;;LOS can be a list with a string as the first element and a list as the seocnd

;;TEMPLATE
;;los-fn : List-of-string -> ??
;(define (los-fn los)
;  (cond 
;     [(empty? los) ...]
;     [else (... (first los)
;                (los-fn (rest los)))]))

;;juxtapose : List-of-string -> String
;;consumes a list of strings and appends them all into one long string.
;;EXAMPLE: 
;;(juxtapose (list "qwe" "asd"))="qweasd"
;;STRATEGY : function composition

(define (juxtapose los)
  (cond 
     [(empty? los) ""]
     [else (string-append (first los)
                (juxtapose (rest los)))]))

(define-test-suite juxtapose-test
  (check-equal?
     (juxtapose (list "qwe" "asd"))
     "qweasd")
  (check-equal?
     (juxtapose empty)
     ""))

(run-tests juxtapose-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Exercise 118 (ill-sized?);;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITION
;; A List-of-image (LOI) is one of:
;; – empty
;; –(cons Boolean List-of-image)
;;Interp:
;;LOI a sequence with no elements
;;LOI can be a list with a image as the first element and a list as the seocnd

;;TEMPLATE
;;loi-fn : List-of-image -> ??
;(define (loi-fn loi)
;  (cond 
;     [(empty? loi) ...]
;     [else (... (first loi)
;                (loi-fn (rest loi)))]))

;;ill-sized? : List-of-image Number -> String
;;consumes a list of strings and appends them all into one long string.
;;EXAMPLE: 
;;(ill-sized? (list (rectangle 10 10 "solid" "red") (rectangle 10 20 "solid" "red")) 10)=(rectangle 10 20 "solid" "red")
;;STRATEGY : function composition

(define (ill-sized? loi n)
  (cond 
     [(empty? loi) (rectangle 1 1 "solid" "red")]
     [else (beside (check-width (first loi) n)
                   (ill-sized? (rest loi) n))]))

(define (check-width image n)
    (if (= n (image-width image) (image-height image))
        (rectangle 1 1 "solid" "red")
        image))

(define-test-suite ill-sized?-test
  (check-equal?
     (ill-sized? (list (rectangle 10 10 "solid" "red") (rectangle 10 20 "solid" "red")) 10)
     (beside (rectangle 1 1 "solid" "red")(rectangle 10 20 "solid" "red") (rectangle 1 1 "solid" "red"))))

(run-tests ill-sized?-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;Exercise 125 (add-to-pi and add);;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITION
;; N is one of
;;--0
;; (add1 N)
;;Interp:
;;N is a natural number 

;;TEMPLATE
;;n-fn : N -> ??
;(define (n-fn n)
;  (cond 
;    [(zero? n) empty]
;    [(positive? n) (...(n-fn (sub1 n)) )]))


(define (add-to-pi n)
  (cond 
    [(zero? n) pi]
    [(positive? n) (add1 (add-to-pi (sub1 n)))]))

(add-to-pi 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;Exercise 127 (row-col);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rect (rectangle 10 10 "solid" "red"))

(define (col n i)
  (cond
     [(zero? n) i]
     [(positive? n) (beside i (col (sub1 n) i))]))

(define (row n i)
  (cond
     [(zero? n) i]
     [(positive? n) (beside i (row (sub1 n) i))]))

(row 5 rect)
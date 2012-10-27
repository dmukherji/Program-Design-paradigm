;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;; The accumulator in this problem is a list of number, which is the index
;; of the current section. 

;; For example, the accumulator is (list 1 2 3)
;; it means the current section is 1.2.3

;; when program enters into a new sub-section, add 1 to end to 
;; current accumulator
;; e.g. new sub-section of (list 1 2 3) is (list 1 2 3 1)

;; when program enters a section in the same level, add the last
;; element of accumulator by 1.
;; e.g. new section of (list 1 2 3) is (list 1 2 4)

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)
(require "extras.rkt")

(provide nested-to-flat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA DEFINITION
;; flat-list

;; A FlatSection is a (list ListOf<Number> String).
;; Interp:
;; --index is a ListOf<Number> which represents the index of title
;; --title is a String represents the title's content

;; template:
;; fc-fn : FlatSection -> ??
;(define (fc-fn fc)
;    (...
;     (first fc)
;     (second fc)))


;; A ListOf<Number> (LONU) is one of
;; -- empty                      (interp: nothing in the list)
;; --(cons Number ListOf<Number>)(interp: a Number followed by a list of Number)
;; template:
;; LONU-fn: LONU -> ??
;(define (LONU-fn lonu)
;  (cond
;    [(empty? lonu) ...]
;    [else (... 
;           (first lonu) 
;           (LONU-fn (rest lonu)))]))

;; A FlatList is a ListOf<FlatSection> (LOF) which is either 
;; -- empty                               
;; --(cons FlatSection ListOf<FlatSection>) 
;; Interp:
;; empty : nothing in the list
;; else it is a FlatSection followed by a list of FlatSection
;; template:
;; LOF-fn: LOF -> ??
;(define (LOF-fn lof)
;  (cond
;    [(empty? lof) ...]
;    [else (... 
;           (fc-fn (first lof)) 
;           (LOF-fn (rest lof)))]))

;; nested-list

;; A NestedSection is a (cons String NestedList)
;; interp:
;; --title is a String represents title's content
;; --NestedList is a ListOf<NestedSection> represents sub-sections
;; template
;; ns-fn : NestedSection -> ?
;(define (ns-fn ns)
;   (...
;    (first ns)
;    (LONS-fn (rest ns))))

;; A NestedList is ListOf<NestedSection> (LONS) is one of
;; -- empty
;; --(cons NestedSection NestedList)
;; interp: 
;; empty : nothing in the list
;; else it is a NestedSection followed by a list of NestedSection
;; LONS-fn : LONS -> ??
;(define (LONS-fn lons)
;  (cond
;    [(empty? lons) ...]
;    [else (... 
;           (ns-fn (first lons)) 
;           (LONS-fn (rest lons)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nested-to-flat: NestedList -> FlatList
;; this function translates from the nested representation to 
;; the flat representation. In the function, call a helper function which takes 
;; a initial index for the flat representation
;; example: (nested-to-flat '(("sec1")))= (((1) "sec1"))
;; Strategy: function composition
(define (nested-to-flat lst)
   (nested-to-flat-helper lst (update-section-index empty)))

;; nested-to-flat-helper NestedList ListOf<Number> -> FlatList
;; GIVEN index (ListOf<Number>) as the initial index and the nested 
;;       representation (NestedList)
;; WHERE lst is a sub-section of another nested list and index refers 
;;       to index of lst
;; PRODUCE a flat representation (FlatList) translated from the given 
;;         lst (NestedList).
;; example: (nested-to-flat-helper '(("sec1")) (list 1))= (((1) "sec1"))
;; Strategy: Structural Decomposition[lst:NestedList] + accumulator[index]
(define (nested-to-flat-helper lst index)
  (cond
    [(empty? lst) empty]
    [else 
     (append
      (build-flat-section (first lst) index)
      (nested-to-flat-helper (rest lst) (update-section-index index)))]))

;; build-flat-section : NestedSection ListOf<Number> -> FlatList
;; with the given NestedSection and the index (ListOf<Number>)
;; build the related FlatSection. After building the outmost layer,
;; use a mutual recursion to build inner layers.]
;; example: (build-flat-section '("sec1") (list 1)) = (((1) "sec1"))
;; Strategy: Structural Decomposition[sec:NestedSection]
(define (build-flat-section sec index)
  (cons (list index (first sec))
        (nested-to-flat-helper (rest sec) (add-new-subsection-index index))))

;; update-section-index : ListOf<Number> -> ListOf<Number>
;; for a given section index (ListOf<Number>), 
;; increase the right-most sub-section index by 1
;; example: (update-section-index '(1 2 2 3)) = '(1 2 2 4)
;; Strategy: Structural Decomposition[index:ListOf<Number>] 
(define (update-section-index index)
  (cond
    [(empty? index) (list 1)]
    [else (if (empty? (rest index))
              (cons (add1 (first index)) empty)
              (cons (first index) (update-section-index (rest index))))]))

;; add-new-subsection-index : ListOf<Number> -> ListOf<Number>
;; for a given section index (ListOf<Number>), add a new sub-section
;; at the end with initial value 1
;; example: (add-new-subsection-index '(1 2 2 3)) = '(1 2 2 3 1)
;; Strategy: function composition
(define (add-new-subsection-index index)
  (append index (list 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tests:

(define FOUR-LAYER-NL 
  '(("The first section 1"
       ("This is a subsection of 1 -> 1.1"
          ("1.1.1 ddddd")
          ("1.1.2 fffff"
             ("1.1.2.1 last layer")))
       ("This is another subsection of 1 -> 1.2"))
    ("Another section 2"
       ("Sub-section 2.1"))
    ("The third section 3"
       ("This is a subsection of 3 -> 3.1"
          ("3.1.1 ddddd")
          ("3.1.2 fffff"))
       ("This is another subsection of 3 -> 3.2"))))
 
(define FOUR-LAYER-FL 
  '(((1) "The first section 1")
    ((1 1) "This is a subsection of 1 -> 1.1")
    ((1 1 1) "1.1.1 ddddd") 
    ((1 1 2) "1.1.2 fffff")
    ((1 1 2 1) "1.1.2.1 last layer")
    ((1 2) "This is another subsection of 1 -> 1.2")
    ((2) "Another section 2")
    ((2 1) "Sub-section 2.1")
    ((3) "The third section 3")
    ((3 1) "This is a subsection of 3 -> 3.1")
    ((3 1 1) "3.1.1 ddddd") 
    ((3 1 2) "3.1.2 fffff")
    ((3 2) "This is another subsection of 3 -> 3.2")))

(define-test-suite nested-to-flat-tests
 (check-equal? 
   (nested-to-flat FOUR-LAYER-NL)
   FOUR-LAYER-FL
   "test for translate a four layer nested list (1 to 1.1.2.1) with
           three sections (1 to 3) to flat list")
 (check-equal? 
   (nested-to-flat empty)
   empty
   "test for translate an empty nested list to flat list"))
(run-tests nested-to-flat-tests)








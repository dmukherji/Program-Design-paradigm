;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
(require "1.rkt")
(require "extras.rkt")

(provide decode)
(provide succeeded?)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Data definition

;; An SexpOfAtom is one of 
;; -- a Symbol             
;; (interp: the built-in data type Symbol)
;; -- a Number             
;; (interp: the built-in data type Number. 
;;          For robot command forward, it represents the steps the robot walk.
;;          For other command, it represents the times that Cmd will be executed
;; )
;; -- a ListOf<SexpOfAtom> 
;; (interp: A list of SexpOfAtom)

;; Template :
;; sexp-fn : SexpOfAtom -> ?
;(define (sexp-fn sexp)
;    (cond 
;       [(symbol? sexp) ...]
;       [(number? sexp) ...]
;       [else (... 
;;             (los-fn sexp))]))

;; ListOf<SexpOfAtom> is either
;; -- empty                 
;; (interp: nothing in the list)
;; -- (cons SexpOfAtom ListOf<SexpOfAtom>) 
;; (interp: one or more SexpOfAtom in the list)

;; Template :
;; los-fn : ListOf<SexpOfAtom> -> ?
;(define (los-fn los)
;   (cond
;      [(empty? los) ...]
;      [else (... (sexp-fn (first los))
;                 (los-fn (rest los)))]))

;; A Maybe<Cmd> is either
;; -- false  (interp: indicates the data is not a Cmd)
;; -- Cmd    (interp: the data is Cmd)  

;; Template:
;; maybe-cmd-fn : Maybe<Cmd> -> ?
;(define (maybe-cmd-fn c)
;  (cond
;    [(false? c) ...]
;    [else ...]))

;; A NaturalNumber (Nat) is one of
;; -- 0  (interp: the minimal value is zero)
;; -- (add1 Nat) (interp: a integer number larger than 0)

;; Template :
;; nat-fn : Nat -> ?
;(define (nat-fn n)
;   (cond
;      [(zero? n) ...]
;      [else (...
;             (nat-fn (sub1 n)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; decode : SexpOfAtom -> Maybe<Cmd>
;; Given a SexpOfAtom, translates it to correspond Cmd. If the given SexpOfAtom
;; doesn't correspond to any Cmd, return false.
;; Algorithm: 
;; --if the sexp looks like a do-times, sequence-cmd, if-facing-edge
;;   or while-not-facing-edge-do, recur. 
;; --Otherwise, if the sexp is Forward, Left, Right or empty list, 
;;   return correspond Cmd. 
;; --If the sexp is Symbol,Number or others, return false.
;; --In the recursive call, if the recursion succeed return correspond Cmd. 
;;   Else return false. 
;; Example: 
;; (decode '(forward 1)) = (make-forward 1)
;; (decode '(2 3)) = false
;; Termination Argument:
;; The size of sexp is the halting measure.
;; The size of an sexp is always non-negative. The size of (second sexp), 
;; (third sexp) and each sexp in the ListOf<SexpOfAtom> are smaller than 
;; the original sexp.
;; Strategy : General Recursion 
(define (decode sexp)
  (cond
    [(symbol? sexp) false]
    [(number? sexp) false]
    [(is-forward? sexp) (make-forward (second sexp))]
    [(is-left? sexp) (make-left (second sexp))]
    [(is-right? sexp) (make-right (second sexp))]
    [(look-like-do-times? sexp) 
     (do-times-helper (second sexp) (decode (third sexp)))]
    [(look-like-sequence-cmd? sexp) (sequence-cmd-helper sexp)] 
    [(look-like-if-facing-edge? sexp) 
     (if-facing-edge-helper (decode (second sexp)) (decode (third sexp)))] 
    [(look-like-while-not-facing-edge-do? sexp)
     (while-not-facing-edge-do-helper (decode (second sexp)))]
    [else false]))
   
;; do-times-helper : NaturalNumber Maybe<Cmd> -> Maybe<Cmd>
;; Checks whether or not the given mc (Maybe<Cmd>) is a cmd. If so, returns 
;; a do-times cmd. Else, return false.
;; Examples: 
;; (do-times-helper 2 false) = false
;; Strategy: Function Composition
(define (do-times-helper n mc)
  (if (succeeded? mc)
      (make-do-times n mc)
      false))

;; sequence-cmd-helper: ListOf<SexpOfAtom> -> Maybe<Cmd>
;; WHERE los(ListOf<SexpOfAtom>) is a empty list or a list beginning with 
;; a non Symbol element
;; Checks whether or not all elements in los are cmd. If so, returns a 
;; sequence cmd. Else, return false.
;; Algorithm: For each SexpOfAtom in los, call decode. If any of the result
;; is false, reurn false. Else, return correspond sequence cmd.
;; Examples:
;; (sequence-cmd-helper '(3 (forward 3))) = false
;; Termination Argument:
;; The size of SexpOfAtom is the halting measure.
;; The size of an SexpOfAtom is always non-negative. The size of each 
;; SexpOfAtom in los are smaller than los.
;; Strategy: Higher-Order Function Composition(part of General Recursion)
(define (sequence-cmd-helper los)
  (local
    (
     ;; Decode all the SexpOfAtom in given list, returns the list
     (define lst (foldr cons empty (map decode los))))
    (if (andmap succeeded? lst)
      (make-sequence-cmd lst)
      false)))

;; if-facing-edge-helper: Maybe<Cmd> Maybe<Cmd> -> Maybe<Cmd>
;; Checks whether or not both mc1(Maybe<Cmd>) and mc2(Maybe<Cmd>) are cmd. 
;; If so, returns if-facing-edge cmd. Else returns false.
;; Examples:
;; (if-facing-edge-helper false '(forward 2)) = false
;; Strategy: Function Composition
(define (if-facing-edge-helper mc1 mc2)
  (if (and (succeeded? mc1) (succeeded? mc2))
      (make-if-facing-edge mc1 mc2)
      false))

;; while-not-facing-edge-do-helper: Maybe<Cmd> -> Maybe<Cmd>
;; Checks whether or not the given mc(Maybe<Cmd>) is a cmd. If so, returns 
;; a while-not-facing-edge-do cmd. Else, return false. 
;; Examples:
;; (while-not-facing-edge-do-helper false) = false
;; Strategy: Function Composition
(define (while-not-facing-edge-do-helper mc)
  (if (succeeded? mc)
      (make-while-not-facing-edge-do mc)
      false))

;; succeeded? : Maybe<Cmd> -> Boolean
;; Checks whether the input is a Cmd or not
;; Example: (succeeded? false) = false
;; Strategy: Structural Decomposition[mc:Maybe<Cmd>]
(define (succeeded? mc)
   (cond
       [(false? mc) false]
       [else true]))

;; is-forward? : SexpOfAtom -> Boolean
;; WHERE sexp is neither a Symbol nor a Number.
;; Checks whether or not the input sexp(SexpOfAtom) is a Forward.
;; Algorithms: 
;; A valid Forward cmd should have a length of 2
;; the first element should be symbol ('forward) and 
;; the second element should be a non-negative integer
;; Example: (look-like-forward? '(forward 1)) = true
;; Strategy: Function Composition
(define (is-forward? sexp)
    (and
     (= (length sexp) 2)
     (equal? (first sexp) 'forward)
     (natural-number? (second sexp))))

;; is-left? : SexpOfAtom -> Boolean
;; WHERE sexp is neither a Symbol nor a Number.
;; Checks whether or not the input sexp(SexpOfAtom) is a Left
;; Algorithms: 
;; A valid Left cmd should have a length of 2 
;; the first element should be symbol ('left)
;; the second element should be a non-negative integer 
;; Example: (look-like-left? '(left 1)) = true
;; Strategy: Function Composition
(define (is-left? sexp)
  (and
   (= (length sexp) 2)
   (equal? (first sexp) 'left)
   (natural-number? (second sexp))))

;; is-right? : SexpOfAtom -> Boolean
;; WHERE sexp is neither a Symbol nor a Number.
;; Checks whether or not the input sexp(SexpOfAtom) is a Right 
;; Algorithms: 
;; A valid Right cmd should have a length of 2
;; the first element should be symbol ('right) and 
;; the second element should be a non-negative integer 
;; Example: (look-like-right? '(right 1)) = true
;; Strategy: Function Composition
(define (is-right? sexp)
  (and
   (= (length sexp) 2)
   (equal? (first sexp) 'right)
   (natural-number? (second sexp))))

;; look-like-do-times? : SexpOfAtom -> Boolean
;; WHERE sexp is neither a Symbol nor a Number.
;; Checks whether or not the input sexp(SexpOfAtom) looks like a 
;; DoTimes at top level
;; Algorithms: 
;; At the top level, a representation of a DoTimes must be a
;; list of 3 elements, beginning with the symbol do-times. The
;; second element should be a non-negative integer.
;; Example: 
;; (look-like-do-times? '(do-times 2 1)) = true
;; Strategy: Function Composition
(define (look-like-do-times? sexp)
  (and
   (= (length sexp) 3)
   (equal? (first sexp) 'do-times)
   (natural-number? (second sexp))))

;; look-like-sequence-cmd? : SexpOfAtom -> Boolean
;; WHERE sexp is neither a Symbol nor a Number.
;; Checks whether or not the input sexp(SexpOfAtom) looks like a 
;; SequenceCmd at top level
;; Algorithms: 
;; At the top level, a representation of a SequenceCmd must be a
;; empty list or a non-empty list, whose first element is not a symbol.
;; Example: 
;; (look-like-sequence-cmd? '('(do-times 2 '(left 1)))) = true
;; Strategy: Function Composition
(define (look-like-sequence-cmd? sexp)
  (or
   (empty? sexp) 
   (not (symbol? (first sexp)))))
 
;; look-like-if-facing-edge? : SexpOfAtom -> Boolean
;; WHERE sexp is neither a Symbol nor a Number.
;; Checks whether or not the input sexp(SexpOfAtom) looks like a 
;; IfFacingEdge at top level
;; Algorithms: 
;; At the top level, a representation of a IfFacingEdge must be a
;; list of 3 elemets, beginning with the symbol if-facing-edge
;; Example: 
;; (look-like-if-facing-edge? '(right 1)) = false
;; Strategy: Function Composition
(define (look-like-if-facing-edge? sexp)
  (and 
   (= (length sexp) 3)
   (equal? (first sexp) 'if-facing-edge)))
 
;; look-like-while-not-facing-edge-do? : SexpOfAtom -> Boolean
;; WHERE sexp is neither a Symbol nor a Number.
;; Checks whether or not the input sexp(SexpOfAtom) looks like a 
;; WhileNotFacingEdgeDo at top level
;; Algorithms: 
;; At the top level, a representation of a WhileNotFacingEdgeDo must be 
;; a list of 2 elements, beginning with the symbol while-not-facing-edge-do
;; Example: 
;; (look-like-while-not-facing-edge-do? '(right 1)) = false
;; Strategy: Function Composition
(define (look-like-while-not-facing-edge-do? sexp)
  (and   
   (= (length sexp) 2)
   (equal? (first sexp) 'while-not-facing-edge-do)))


;; natural-number? : Any -> Boolean
;; Checks whether or not the input is a NaturalNumber
;; Examples:
;; (natural-number? -1) = false
;; Strategy: Function Composition
(define (natural-number? n)
  (and (integer? n) 
       (not (negative? n))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests:
;; Cmd examples for testing
(define cmd1 (make-sequence-cmd empty))
(define cmd2 (make-forward 0))
(define cmd3 (make-right 1))
(define cmd4 (make-left 2))
(define cmd5 (make-do-times 4 (make-right 1)))
(define cmd6 (make-sequence-cmd 
              (list (make-forward 0) (make-right 1) (make-left 2))))
(define cmd7 (make-if-facing-edge (make-forward 0) (make-right 1)))
(define cmd8 (make-while-not-facing-edge-do (make-right 1)))
(define cmd9 (make-sequence-cmd 
              (list (make-do-times 4 (make-right 1))
                    (make-sequence-cmd 
                     (list (make-forward 0) (make-right 1) (make-left 2)))
                    (make-if-facing-edge (make-forward 0) (make-right 1))
                    (make-while-not-facing-edge-do (make-right 1)))))

;; S-expressions for testing

;; legal S-expression
;; one level
(define legal-sexp-1 '())
(define legal-sexp-2 '(forward 0))
(define legal-sexp-3 '(right 1))
(define legal-sexp-4 '(left 2))
;; two level
(define legal-sexp-5 '(do-times 4 (right 1)))
(define legal-sexp-6 '((forward 0) (right 1) (left 2)))
(define legal-sexp-7 '(if-facing-edge (forward 0) (right 1)))
(define legal-sexp-8 '(while-not-facing-edge-do (right 1)))
;; more than two level
(define legal-sexp-9 (list legal-sexp-5 legal-sexp-6 legal-sexp-7 legal-sexp-8))


;; illegal S-expression
(define illegal-sexp-1 'forward)
(define illegal-sexp-2 34)

;; one level
(define illegal-sexp-3 '(forward -1))
(define illegal-sexp-4 '(left right))
(define illegal-sexp-5 '(right 12 34))
(define illegal-sexp-6 '(do-times 10))
(define illegal-sexp-7 '(sequence-cmd 9))
(define illegal-sexp-8 '(if-facing-edge 8 0))
(define illegal-sexp-9 '(while-not-facing-edge-do 12))
(define illegal-sexp-10 '(illegal-cmd 12))

;; two level
(define illegal-sexp-11 '(do-times -2 (forward 10)))
(define illegal-sexp-12 '(do-times 2 (right -10)))
(define illegal-sexp-13 '((right 10) (forward -1)))
(define illegal-sexp-14 '((right 10) (forward 1) 13))
(define illegal-sexp-15 '(if-facing-edge (right 10) (forward -1)))
(define illegal-sexp-16 '(while-not-facing-edge-do (right -10)))

;; more than two level
(define illegal-sexp-17 '(do-times 3 ((right 10) (forward -1))))
(define illegal-sexp-18 '(if-facing-edge 
                          (forward 10) 
                          (while-not-facing-edge-do (right -10))))


(define-test-suite decode-tests
  
  ;; for legal S-expression
  
  ;; An empty list
  (check-equal? (decode legal-sexp-1) cmd1)
  
  ;; A forward cmd, with argument 0
  (check-equal? (decode legal-sexp-2) cmd2)
  
  ;; A right cmd, with argument 1
  (check-equal? (decode legal-sexp-3) cmd3)
  
  ;; A left cmd, with argument 2
  (check-equal? (decode legal-sexp-4) cmd4)
  
  ;; A do times cmd, do (right 1) 4 times
  (check-equal? (decode legal-sexp-5) cmd5)
  
  ;; A list of cmd
  (check-equal? (decode legal-sexp-6) cmd6)
  
  ;; A if facing edge cmd, with two legal cmd
  (check-equal? (decode legal-sexp-7) cmd7)
  
  ;; A whild not facing edge do cmd, with a legal cmd
  (check-equal? (decode legal-sexp-8) cmd8)
  
  ;; A list of cmd, the level of this S-expression is 3
  (check-equal? (decode legal-sexp-9) cmd9)
  
  
  ;; for illegal S-expression
  
  ;; A Symbol
  (check-equal? (decode illegal-sexp-1) false)
  
  ;; A Number
  (check-equal? (decode illegal-sexp-2) false)
  
  ;; A forward cmd, the second element is a negative number
  (check-equal? (decode illegal-sexp-3) false)
  
  ;; A left cmd, the second element is also a symbol
  (check-equal? (decode illegal-sexp-4) false)
  
  ;; A right cmd, but has three elements
  (check-equal? (decode illegal-sexp-5) false)
  
  ;; A do times cmd, but has only two elements
  (check-equal? (decode illegal-sexp-6) false)
  
  ;; Two elements, first is a symbol sequence-cmd
  (check-equal? (decode illegal-sexp-7) false)
  
  ;; A if facing edge cmd, but the other two elements are number
  (check-equal? (decode illegal-sexp-8) false)
  
  ;; A while not facing edge do cmd, but the second element is a Number
  (check-equal? (decode illegal-sexp-9) false)
  
  ;; The first element is a symbol, but does not represent any cmd
  (check-equal? (decode illegal-sexp-10) false)
  
  ;; A do times cmd, but the second element is a negative number
  (check-equal? (decode illegal-sexp-11) false)
  
  ;; A do times cmd, but the third element is an illegal S-expression
  (check-equal? (decode illegal-sexp-12) false)
  
  ;; A list of cmd, but the second element is an illegal S-expression
  (check-equal? (decode illegal-sexp-13) false)
  
  ;; A list of cmd, but the third element is a Number
  (check-equal? (decode illegal-sexp-14) false)
  
  ;; A if facing edge cmd, but the third element is an illegal S-expression
  (check-equal? (decode illegal-sexp-15) false)
  
  ;; A while not facing edge do cmd, but the second element is an 
  ;; illegal S-expression
  (check-equal? (decode illegal-sexp-16) false)
  
  ;; A do times cmd, the third element is a sequence of cmd, but the second
  ;; element in this sequence is an illegal S-expression
  (check-equal? (decode illegal-sexp-17) false)
  
  ;; A if facing edge cmd, the third element is a while not facing edge do
  ;; cmd, but the argument in this cmd is an illegal S-expression
  (check-equal? (decode illegal-sexp-18) false))

(run-tests decode-tests)
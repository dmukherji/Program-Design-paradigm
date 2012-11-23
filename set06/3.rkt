;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")
(require "1.rkt")
(require "2.rkt")

(provide run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANT

;; Initial robot
(define INITIAL-ROBOT (make-robot 100 100 north 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run: SexoOfAtom -> Maybe<Robot>
;; Given a SexpOfAtom, translates it to robot command and returns the state of
;; robot after running that command from robot's initial position. If the given
;; SexpOfAtom is not the representation of any command, return false.
;; Examples:
;; (run '((left 1) (forward 200) (left 1))) = (make-robot 20 100 "south" 80)
;; (run '(forward -1)) = false
;; Strategy: Function Composition
(define (run sexp)
  (local 
    (;; cmd is a constant variable stores the result after decoding sexp
     (define cmd (decode sexp)))
    (if (succeeded? cmd)
        (robot-after-cmd cmd INITIAL-ROBOT)
        false)))
 
;; tests:
(define-test-suite run-test-suite
  
  ;; The given SexpOfAtom is a representation of a robot command
  (check-equal? (run '((left 1) (forward 200) (left 1))) 
                (make-robot 20 100 south 80))
  
  ;; The given SexpOfAtom is a representation of a robot command
  (check-equal? (run '(do-times 
                       4 
                       ((forward 15) 
                        (if-facing-edge (left 1) (right 1)))))
                (make-robot 100 100 north 60))
  
  ;; The given SexpOfAtom is not a representation of a robot command
  (check-equal? (run '(forward -1)) false))


(run-tests run-test-suite)


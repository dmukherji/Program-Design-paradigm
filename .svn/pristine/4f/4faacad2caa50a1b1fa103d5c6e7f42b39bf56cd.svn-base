;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Write your solution for the third problem of the first problem set in this file
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(provide initial-state)
(provide final-state?)
(provide error-state?)
(provide next-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA DEFINITION

;;A State is one of
;;--"initialState"
;;--"activeState"
;;--"finalState"
;;--"errorState"
;;Interp:
;;the STATE follows the regular expression: a (b|c)* d
;;--"initialState" means ready to start accept strings
;;--"activeState" represents the machine is ready to accept more char
;;--"finalState" means the state is ended with a 'd' and cannot accept more.
;;--"errorState" means there are unexpected input.

;;TEMPLATE
;;state-fn : State -> ??
;(define (state-fn s)
;  (cond 
;    [(string=? "initialState") (...)]
;    [(string=? "activeState") (...)]
;    [(string=? "finalState") (...)]
;    [(string=? "errorState") (...)]
;    [else ...]))

;;A KeyEvent is one of:
;;keys longer than 1
;;"a"
;;"b" or "c"
;;"d"
;;other keys has a length of 1

;;Interp:
;;keys longer than 1 will be ingored
;;"a" let state from initial- to active-
;;"b" or "c" means continuing the active state
;;"d" leads to final state
;;other keys lead to error state

;;TEMPLATE
;;key-fn : KeyEvent -> ?
;(define (key-fn key)
;   (cond 
;       [(> (string-length key) 1) ...]
;       [(string=? key "a") ...]
;       [(or (string=? key "b") (string=? key "c")) ...]
;       [(string=? key "d") ...]
;       [else ...]))



;;initial-state : Number -> State
;;Ignores its argument and returns a representation
;;of the initial state of your machine
;;(initial-state 1)="initialState"
;;STRATEGY: Domain Knowledge
(define (initial-state n)
  "initialState")

;;next-state : State KeyEvent -> State
;;next state is generated based on input key: 
;;--all keys longer than 1 are ingored
;;--"initialState" accepts "a" to activeState
;;--"activeState" accepts "b" or "c" and state doesn't change
;;----"d" leads to "finalState"
;;----others lead to "errorState"
;;--"finalState" automatically changes to "initialState"
;;--"errorState" doesn't accept more inputs and stays the same
;;EXAMPLES:
;;(next-state "initialState" "a")="activeState"
;;(next-state "activeState" "d")="finalState"
;;(next-state "activeState" "b")="activeState"
;;(next-state "finalState" "b")="errorState"
;;(next-state "activeState" "s")="errorState"
;;(next-state "errorState" "b")="errorState"
;;STRATEGY : Function Composition
(define (next-state s key)
  (cond 
    [(string=? "initialState" s) (initial-state-helper s key)]
    [(string=? "activeState" s) (active-state-helper s key)]
    [(final-state? s) (final-state-helper s key)]
    [(error-state? s) s]
    [else s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Helper functions

;;initial-state-helper : State KeyEvent -> State
;;produce another state based on current one and input keyEvent
;;EXAMPLES:
;;(initial-state-helper "initialState" "a")="activeState"
;;(initial-state-helper "initialState" "d")="errorState"
;;(initial-state-helper "initialState" "left")="initialState"
;;(initial-state-helper "initialState" "r")="errorState"
;;STRATEGY : Domain Knowledge
(define (initial-state-helper s key)
   (cond 
     [(> (string-length key) 1) s]
     [(string=? key "a") "activeState"]
     [else "errorState"]))

;;final-state-helper : State KeyEvent -> State
;;produce another state based on current one and input KeyEvent
;;EXAMPLES:
;;(final-state-helper "finalState" "a")="activeState"
;;(final-state-helper "finalState" "d")="errorState"
;;(final-state-helper "finalState" "left")="finalState"
;;(final-state-helper "finalState" "r")="errorState"
;;STRATEGY : Domain Knowledge
(define (final-state-helper s key)
   (cond 
     [(> (string-length key) 1) s]
     [(string=? key "a") "activeState"]
     [else "errorState"]))
       
;;active-state-helper : State KeyEvent -> State
;;produce another state based on current one and input KeyEvent
;;EXAMPLES:
;;(active-state-helper "activeState" "b")="activeState"
;;(active-state-helper "activeState" "d")="finalState"
;;(active-state-helper "activeState" "left")="activeState"
;;(active-state-helper "activeState" "r")="errorState"
;;STRATEGY : Domain Knowledge
(define (active-state-helper s key)
   (cond 
     [(> (string-length key) 1) s]
     [(or (string=? key "b") (string=? key "c")) s]
     [(string=? key "d") "finalState"]
     [else "errorState"]))

;;final-state? : State -> Boolean
;;decide whether the input state is finalState
;;EXAMPLE
;;(final-state? "finalState")=true
;;(final-state? "errorState")=false
;;STRATEGY : Domain Knowledge
(define (final-state? state)
  (if (string=? state "finalState")
      true
      false))

;;error-state? : State -> Boolean
;;decide whether the input state is errorState
;;EXAMPLE
;;(error-state? "errorState")=true
;;(error-state? "initialState")=false
;;STRATEGY : Domain Knowledge
(define (error-state? state)
  (if (string=? state "errorState")
      true
      false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;test cases design:
;;four states * five KeyEvents
;;==20 combinations
(define-test-suite next-state-test
  (check-equal?
     (next-state "initialState" "a")
     "activeState"
     "initial+a")
  (check-equal?
     (next-state "initialState" "b")
     "errorState"
     "initial+b")
  (check-equal?
     (next-state "initialState" "left")
     "initialState"
     "initial+long")
  (check-equal?
     (next-state "initialState" "d")
     "errorState"
     "initial+d")  
  (check-equal?
     (next-state "initialState" "1")
     "errorState"
     "initial+other") 
  
  (check-equal?
     (next-state "activeState" "a")
     "errorState"
     "active+ a")  
  (check-equal?
     (next-state "activeState" "b")
     "activeState"
     "active+ b")
  (check-equal?
     (next-state "activeState" "d")
     "finalState"
     "active+ d")
  (check-equal?
     (next-state "activeState" "left")
     "activeState"
     "active+longInput")
  (check-equal?
     (next-state "activeState" "w")
     "errorState"
     "active+wrongINput")
  
  (check-equal?
     (next-state "finalState" "a")
     "activeState"
     "final+a")
  (check-equal?
     (next-state "finalState" "b")
     "errorState"
     "final+b")
  (check-equal?
     (next-state "finalState" "d")
     "errorState"
     "final+d")
  (check-equal?
     (next-state "finalState" "right")
     "finalState"
     "final+long")
  (check-equal?
     (next-state "finalState" "s")
     "errorState"
     "final+other")
  
  (check-equal?
     (next-state "errorState" "a")
     "errorState"
     "error+a")
  (check-equal?
     (next-state "errorState" "b")
     "errorState"
     "error+b")
  (check-equal?
     (next-state "errorState" "d")
     "errorState"
     "error+d")
  (check-equal?
     (next-state "errorState" "right")
     "errorState"
     "error+long")
  (check-equal?
     (next-state "errorState" "m")
     "errorState"
     "error+other"))
(run-tests next-state-test)

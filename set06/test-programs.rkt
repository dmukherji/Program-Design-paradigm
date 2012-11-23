;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test-programs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
;; test-programs.rkt
;; In this file, you find some definition stubs of programs that should be
;; provided to our test suite. For each program you'll find some comments
;; above the definition stub that contain the program in the robot language.
;; Your goal is to replace 'insert-your-representation-here with your internal
;; representation of the given program in every definition stub.

;; THE ONLY THING YOU MAY CHANGE IN THIS FILE IS TO REPLACE ANY OCCURRENCE OF
;; 'insert-your-representation-here WITH YOUR REPRESENTATION OF THE PROGRAM YOU
;; FIND SOME LINES ABOVE THE RESPECTIVE OCCURRENCE. 
;; DO NOT CHANGE ANYTHING ELSE!

;; submit this file by depositing it in your svn folder, as usual, by 6am on
;; Thursday morning.

(require "1.rkt")
(require "extras.rkt")

(provide CMD1 CMD2 CMD3 CMD4 CMD5 CMD6 CMD7 CMD8 CMD9 CMD10 
         CMD11 CMD12 CMD13 CMD14 CMD15 CMD16 CMD17 CMD18)

;; CMD1
;; do-times(5){forward(2)}
(define CMD1
  (make-do-times 5 (make-forward 2))
  )

;; CMD2
;; right(1)
(define CMD2
  (make-right 1)
  )

;; CMD3
;; forward(20)
(define CMD3
  (make-forward 20)
  )

;; CMD4
;; left(1)
(define CMD4
  (make-left 1)
  )

;; CMD5
;; do-times (4) {forward(15); if-facing-edge then left(1) else right(1)}
(define CMD5
  (make-do-times 4 (make-sequence-cmd (list (make-forward 15) (make-if-facing-edge (make-left 1) (make-right 1)))))
  )

;; CMD6
;; {left(1); forward(200); left(1)}
(define CMD6
  (make-sequence-cmd (list (make-left 1) (make-forward 200) (make-left 1)))
  )

;; CMD7
;; do-times (0) {left(3)}
(define CMD7
  (make-do-times 0 (make-left 3))
  )

;; CMD8
;; do-times (2) right(3)
(define CMD8
  (make-do-times 2 (make-right 3))
  )

;; CMD9
;; {}
(define CMD9
  (make-sequence-cmd empty)
  )

;; CMD10
;; do-times (100) {}
(define CMD10
  (make-do-times 100 (make-sequence-cmd empty))
  )

;; CMD11
;; forward(0)
(define CMD11
  (make-forward 0)
  )

;; CMD12
;; { right(1); forward(200); {forward(5)}; 
;;   {{left(1); forward(50)};
;;    left(3)}; 
;;   forward(5)} 
(define CMD12
  (make-sequence-cmd (list (make-right 1)
                           (make-forward 200)
                           (make-sequence-cmd (list (make-forward 5)))
                           (make-sequence-cmd (list (make-sequence-cmd (list (make-left 1) (make-forward 50)))
                                                    (make-left 3)))
                           (make-forward 5)))
  )

;; CMD13
;; { forward(80); 
;;   if-facing-edge then {right(1); forward(30)} 
;;              else {forward(30); right(1)};
;;   do-times(11) { if-facing-edge then {left(2)} 
;;                   else {right(1); 
;;                         if-facing-edge then {left(2)} else {}};
;;                  forward(80)}} 
(define CMD13
  (make-sequence-cmd (list (make-forward 80) 
                           (make-if-facing-edge (make-sequence-cmd (list (make-right 1) (make-forward 30)))
                                                (make-sequence-cmd (list (make-forward 30) (make-right 1))))
                           (make-do-times 11 (make-sequence-cmd (list (make-if-facing-edge (make-sequence-cmd (list (make-left 2)))
                                                                                           (make-sequence-cmd (list (make-right 1)
                                                                                                                    (make-if-facing-edge (make-sequence-cmd (list (make-left 2)))
                                                                                                                                         (make-sequence-cmd empty)))))
                                                                      (make-forward 80))))))
  )

;; CMD14
;; if-facing-edge then while-not-facing-edge-do left(2) else forward(1)
(define CMD14
  (make-if-facing-edge (make-while-not-facing-edge-do (make-left 2))
                       (make-forward 1))
  )

;; CMD15
;; while-not-facing-edge-do forward(7)
(define CMD15
  (make-while-not-facing-edge-do (make-forward 7))
  )

;; CMD16
;; { forward(5); while-not-facing-edge-do { left(2); forward(80) } }
(define CMD16
  (make-sequence-cmd (list (make-forward 5)
                           (make-while-not-facing-edge-do (make-sequence-cmd (list (make-left 2)
                                                                                   (make-forward 80))))))
  )

;; CMD17
;; while-not-facing-edge-do right(3)
(define CMD17
  (make-while-not-facing-edge-do (make-right 3))
  )

;; CMD18
;; while-not-facing-edge-do { forward(10); left(2); forward(50); right(2); forward(50) }
(define CMD18
  (make-while-not-facing-edge-do (make-sequence-cmd (list (make-forward 10)
                                                          (make-left 2)
                                                          (make-forward 50)
                                                          (make-right 2)
                                                          (make-forward 50))))
  )
;(define INITIAL-ROBOT (make-robot 100 100 north 0))
;(robot-after-cmd CMD1 INITIAL-ROBOT)
;(robot-after-cmd CMD2 INITIAL-ROBOT)
;(robot-after-cmd CMD3 INITIAL-ROBOT)
;(robot-after-cmd CMD4 INITIAL-ROBOT)
;(robot-after-cmd CMD5 INITIAL-ROBOT)
;(robot-after-cmd CMD6 INITIAL-ROBOT)
;(robot-after-cmd CMD7 INITIAL-ROBOT)
;(robot-after-cmd CMD8 INITIAL-ROBOT)
;(robot-after-cmd CMD9 INITIAL-ROBOT)
;(robot-after-cmd CMD10 INITIAL-ROBOT)
;(robot-after-cmd CMD11 INITIAL-ROBOT)
;(robot-after-cmd CMD12 INITIAL-ROBOT)
;(robot-after-cmd CMD13 INITIAL-ROBOT)
;(robot-after-cmd CMD14 INITIAL-ROBOT)
;(robot-after-cmd CMD15 INITIAL-ROBOT)
;(robot-after-cmd CMD16 INITIAL-ROBOT)
;(robot-after-cmd CMD17 INITIAL-ROBOT)
;(robot-after-cmd CMD18 INITIAL-ROBOT)


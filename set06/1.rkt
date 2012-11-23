;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(provide robot-after-cmd)
(provide direction=?)

(provide (struct-out robot))
(provide (struct-out forward))
(provide (struct-out left))
(provide (struct-out right))
(provide (struct-out sequence-cmd))
(provide (struct-out do-times))
(provide (struct-out if-facing-edge))
(provide (struct-out while-not-facing-edge-do))
(provide north)
(provide east)
(provide south)
(provide west)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CONSTANT:

;; dimensions of the canvas
(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 200)

;; dimensions of robot
(define RADIUS 20)

;; value of direction
(define north 0)
(define east 1)
(define south 2)
(define west 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define-struct forward (n))
;; A Forward is a (make-forward Nat)
;; interp: 
;; n is an Nat which refers to the steps the robot 
;; will forward.

;; Template:
;; forward-fn : Forward -> ?
;(define (forward-fn cmd)
;  (... (forward-n cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct left (n))
;; A Left is a (make-left Nat)
;; interp:
;; n is an Nat, which refers to the times the robot will 
;; turn left. Each time the robot will turn 90 degrees (counterclockwise).

;; Template:
;; left-fn: Left -> ??
;(define (left-fn r)
;  (... (left-n r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct right (n))
;; A Right is a (make-right Nat)
;; interp:
;; n is an Nat, which refers to the times the robot will 
;; turn right. Each time the robot will turn 90 degrees (clockwise). 

;; Template:
;; right-fn : Right -> ??
;(define (right-fn r)
;  (... (right-n r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct do-times (n cmd))
;; A DoTimes is a (make-do-times Nat Cmd)
;; interp: 
;; n is an Nat. It refers to the times the robot will
;; do the cmd. If n equals to zero, the robot will do nothing. 

;; Template:
;; DoTimes-fn : DoTimes -> ??
;(define (DoTimes-fn dt)
;  (... (do-times-n dt)
;       (do-times-cmd dt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct sequence-cmd (loc))
;; A SequenceCmd is a (make-sequence-cmd LOC)
;; interp:
;; loc is a LiseOf<Cmd>. The robot will do these Cmd in sequence.

;; Template:
;; SequenceCmd-fn : SequenceCmd -> ?
;(define (SequenceCmd-fn sc)
;  (... (sequence-cmd-loc sc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A ListOf<Cmd> (LOC) is either
;; -- empty            (interp: no Cmd in the list)
;; -- (cons Cmd LOC)   (interp: one or more Cmd in the list)

;; Template:
;; LOC-fn : LOC -> ??
;(define (LOC-fn loc)
;  (cond
;    [(empty? loc) ...]
;    [else (... (Cmd-fn (first loc)) 
;               (LOC-fn (rest loc)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct if-facing-edge (cmd1 cmd2))
;; A IfFacingEdge(IFE) is a (make-if-facing-edge Cmd Cmd)
;; interp:
;; If the robot is facing the edge, it will execute cmd1. Else, execute cmd2

;; Template:
;; IFE-fn : IFE -> ??
;(define (IFE-fn ife)
;  (... (if-facing-edge-cmd1 ife)
;       (if-facing-edge-cmd2 ife)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct while-not-facing-edge-do (cmd))
;; A WhileNotFacingEdgeDo(WNFE) is a (make-while-not-facing-edge-do Cmd)
;; interp:
;; The robot will execute the cmd until it is facing the edge

;; Template:
;; WNFE-fn : WNFE -> ??
;(define (while-not-facing-edge-do-fn wnfe)
;  (... (while-not-facing-edge-do-cmd wnfe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Cmd is one of
;; -- Forward    (interp: robot will move forward n steps) 
;; -- Left       (interp: robot will turn left (counterclockwise) n*90 degrees)
;; -- Right      (interp: robot will turn right (clockwise) n*90 degrees)
;; -- DoTimes    (interp: robot will do cmd n times)
;; -- SequenceCmd(interp: robot will do the cmd in sequence)         
;; -- IFE (interp: if robot is facing the edge, do cmd1, else, do cmd2)
;; -- WNFE  (interp: robot will do the cmd until facing the edge)

;; Template:
;; Cmd-fn : Cmd -> ??
;(define (Cmd-fn cmd)
;  (cond 
;    [(forward? cmd) (... (forward-n cmd))]
;    [(left? cmd)   (... (left-n cmd))]
;    [(right? cmd)  (... (right-n cmd))]
;    [(do-times? cmd) 
;     (... (Cmd-fn (do-times-cmd cmd)) 
;          (do-times-n cmd))]
;    [(sequence-cmd? cmd) 
;     (... (LOC-fn (sequence-cmd-loc cmd)))]
;    [(if-facing-edge? cmd) 
;     (... (Cmd-fn (if-facing-edge-cmd1 cmd)) 
;          (Cmd-fn (if-facing-edge-cmd2 cmd)))]
;    [(while-not-facing-edge-do? cmd) 
;     (... (Cmd-fn (while-not-facing-edge-do-cmd cmd)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Direction is one of
;; -- north  
;; (interp: the direction is north, robot facing the top side of the canvas)
;; -- east
;; (interp: the direction is south, robot facing the bottom side of the canvas)
;; -- south
;; (interp: the direction is west, robot facing the left side of the canvas)
;; -- west
;; (interp: the direction is east, robot facing the right side of the canvas)

;; Template:
;; dir-fn: Direction -> ??
;(define (dir-fn d)
;  (cond
;    [(direction=? d north) ...]
;    [(direction=? d south) ...]
;    [(direction=? d west) ...]
;    [(direction=? d east) ...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct robot (x y heading odometer))
;; A Robot is a (make-robot Nat Nat Direction Nat)
;; Interp:
;; --x, y are Nats which are the coordinates of the center of the robot
;; --heading is the Direction the robot is facing.
;;   When robot turn left, the 
;;   direction order is north->west->south->east->north. When robot turn right, 
;;   the direction order is north->east->south->west->north. 
;; --odometer is a Nat indicating the total number of pixels the robot 
;;   has travelled

;; Template:
;; robot-fn: Robot -> ??
; (define (robot-fn r)
;   (...(robot-x r) 
;       (robot-y r) 
;       (robot-heading r) 
;       (robot-odometer r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; robot-after-cmd : Cmd Robot -> Robot
;; Given: A Cmd and a Robot
;; WHERE: cmd is a Cmd that will be executed. r is the Robot that has finished
;; executing previous Cmd.
;; PRODUCES: The new state of Robot
;; Example: (robot-after-cmd FORWARD-30 INITIAL-ROBOT) 
;;          = (make-robot 100 70 north 30)
;; Strategy: Structural Decomposition[cmd:Cmd] + accumulator[r:Robot]
(define (robot-after-cmd cmd r)
  (cond
    [(forward? cmd) 
     (forward-robot (forward-n cmd) r)]
    [(left? cmd) 
     (robot-turn-left (left-n cmd) r)]
    [(right? cmd) 
     (robot-turn-right (right-n cmd) r)]
    [(do-times? cmd) 
     (robot-do-times (do-times-cmd cmd) (do-times-n cmd) r)]
    [(sequence-cmd? cmd)
     (foldl robot-after-cmd r (sequence-cmd-loc cmd))]
    [(if-facing-edge? cmd) 
     (if-robot-facing-edge (if-facing-edge-cmd1 cmd) 
                           (if-facing-edge-cmd2 cmd) r)]
    [(while-not-facing-edge-do? cmd) 
     (while-robot-not-facing-edge (while-not-facing-edge-do-cmd cmd) r)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; forward-robot: Nat Robot -> Robot
;; GIVEN: a Nat and a Robot
;; WHERE: r is a Robot after move previous Robot one step forward and 
;; n (Nat)is steps still need to move
;; PRODUCES : a new Robot that moves the given r (Robot) forward with 
;; n (Nat) steps
;; Algorithm:
;; If the given n is not equal to zero and the state of robot is not facing 
;; edge, recur. In each recursive call, the state of robot will be updated and
;; the n will minus 1. All the recursion will return the new state of robot
;; and the steps the robot still need to forward.
;; Example:
;; (forward-robot 2 (make-robot 100 100 north 0)) = (make-robot 100 98 north 2)
;; Strategy: Structural decomposition [n: Nat] + accumulator [r:Robot]
(define (forward-robot n r)
   (cond
      [(zero? n) r]
      [else (if (facing-edge? r)
                r
                (forward-robot (sub1 n) (robot-forward-one-step r)))]))

;; facing-edge? : Robot -> Boolean
;; Decides whether a forward motion would take the r (Robot) past the edge
;; Example: (facing-edge? (make-robot 100 100 north 0)) = false
;; Strategy: Structural Decomposition[r:Robot] 
(define (facing-edge? r)
  (facing-edge?-helper 
   (robot-x r)
   (robot-y r)
   (robot-heading r)))
  
;; facing-edge?-helper : Nat Nat Direction -> Boolean
;; With the given dir (Direction) and robot position (x and y), decides 
;; whether or notthe robot is facing the edge
;; Example: (facing-edge?-helper 100 100 north) = false
;; Strategy: Structural Decomposition[dir:Direction]
(define (facing-edge?-helper x y dir)
  (cond
    [(direction=? dir north) (= y RADIUS)]
    [(direction=? dir south) (= y (- CANVAS-HEIGHT RADIUS))]
    [(direction=? dir west) (= x RADIUS)]
    [(direction=? dir east) (= x (- CANVAS-WIDTH RADIUS))]))

;; robot-forward-one-step : Robot -> Robot
;; Move the given r (Robot) one step forward
;; Example: 
;; (robot-forward-one-step INITIAL-ROBOT) = (make-robot 100 99 north 1)
;; Strategy: Structural Decomposition[r:Robot]
(define (robot-forward-one-step r)
    (robot-forward-one-step-helper 
     (robot-x r)
     (robot-y r)
     (robot-heading r)
     (robot-odometer r)))
  
;; robot-forward-one-step-helper : Nat Nat Direction Nat -> Nat
;; Move the given r (Robot) one step forward based on the given dir(Direction)
;; Example: 
;; (robot-forward-one-step-helper 10 10 north 0) = (make-robot 10 9 north 1)
;; Strategy: Structural Decomposition[dir:Direction]
(define (robot-forward-one-step-helper x y dir odometer)
  (cond
    [(direction=? dir north) (make-robot x (sub1 y) dir (add1 odometer))]
    [(direction=? dir south) (make-robot x (add1 y) dir (add1 odometer))]
    [(direction=? dir west) (make-robot (sub1 x) y dir (add1 odometer))]
    [(direction=? dir east) (make-robot (add1 x) y dir (add1 odometer))]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; robot-turn-left : Nat Robot -> Robot
;; Turns the given r (Robot) left (counterclockwise) by n*90 degrees
;; Example: 
;; (robot-turn-left 1 (make-robot 100 99 north 1))=(make-robot 100 99 west 1)
;; Strategy: Structural decomposition [r:Robot]
(define (robot-turn-left n r)
   (make-robot 
      (robot-x r)
      (robot-y r)
      (modulo (- (robot-heading r) n) 4)
      (robot-odometer r))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; robot-turn-right: Nat Robot -> Robot
;; Turns the given r (Robot) right (clockwise) by n*90 degrees
;; Example: 
;; (robot-turn-right: 1 (make-robot 10 99 north 1))=(make-robot 10 99 east 1)
;; Strategy: Structural decomposition [r:Robot]
(define (robot-turn-right n r)
   (make-robot 
      (robot-x r)
      (robot-y r)
      (modulo (+ (robot-heading r) n) 4)
      (robot-odometer r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; robot-do-times: Cmd Nat Robot-> Robot
;; GIVEN : a Cmd, Nat and a Robot
;; WHERE : r is a Robot after executing previous cmd and the Cmd waits to 
;; be executed for n times
;; PRODUCES : a new Robot that executes Cmd for n(Nat) times
;; Algorithm:
;; If the given n is not equal to zero, recur. Each recursion will return the
;; new state of robot and the times the robot need to do the given Cmd. When
;; the given cmd can not be finished, the recursion will not halt. 
;; Example: 
;; (robot-do-times FORWARD-30 1 INITIAL-ROBOT) = (make-robot 100 70 north 30)
;; Strategy: Structural decomposition [n: Nat] + accumulator [r:Robot]
(define (robot-do-times cmd n r)
   (cond
      [(zero? n) r]
      [else (robot-do-times cmd
                      (sub1 n)
                      (robot-after-cmd cmd r))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if-robot-facing-edge Cmd Cmd Robot -> Robot
;; GIVEN: Two Cmds and a Robot
;; WHERE: r is a Robot after executing cmd which must be IFE. cmd1 and cmd2
;; are two Cmds that might will be executed.
;; PRODUCES: a new Robot after executing related Cmd
;; Example: 
;; (if-robot-facing-edge LEFT-1 FORWARD-30 INITIAL-ROBOT)
;;                       = (make-robot 100 70 north 30)
;; Strategy: Strategy Decomposition[r:Robot] + accumulator[r:Robot]
(define (if-robot-facing-edge cmd1 cmd2 r)
  (if (facing-edge? r)
      (robot-after-cmd cmd1 r)
      (robot-after-cmd cmd2 r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; while-robot-not-facing-edge Cmd Robot -> Robot
;; GIVEN : a Cmd and a Robot
;; WHERE : r is a Robot after executing previous cmd and the cmd is a Cmd that 
;; waits to be executed
;; PRODUCES : a new Robot after it facing the wall 
;; Algorithm:
;; If the robot is not facing the edge, recur. Each recursion will return the
;; new state of robot. The recursion will not halt if the robot can not face
;; the edge or given Cmd can not be finished. 
;; Example: 
;; (while-robot-not-facing-edge FORWARD-30 INITIAL-ROBOT)
;;                       = (make-robot 100 20 north 80)
;; Termination argument: 
;; The halting measure is the distance between the robot and the edge it facing 
;; INCOMPLETE HALTING MEASURE:
;; The recursion will not be halt when  
;; 1) the robot will never face the edge or 
;; 2) given Cmd can not be finished forever. 
;; In any one of the two cases, the distance between the robot and edge it 
;; facing can not decrease. 
;; Strategy: General Recursion + accumulator[r:Robot]
(define (while-robot-not-facing-edge cmd r)
  (if (facing-edge? r)
      r
      (while-robot-not-facing-edge cmd (robot-after-cmd cmd r))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; direction=? : Direction Direction -> Boolean
;; Checks whether or not the two given Directions are equal.
;; Example:
;; (direction=? north west) = false
;; Strategy: Domain Knowledge
(define (direction=? d1 d2)
  (= d1 d2))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define INITIAL-ROBOT (make-robot 100 100 north 0))
(define ROBOT-FORWARD-10 (make-robot 100 90 north 10))
(define ROBOT-FORWARD-100 (make-robot 100 20 north 80))
(define FORWARD-0 (make-forward 0))
(define FORWARD-10 (make-forward 10))
(define FORWARD-30 (make-forward 30))
(define FORWARD-100 (make-forward 100))

(define TURN-RIGHT-0 (make-right 0))
(define TURN-RIGHT-1 (make-right 1))
(define TURN-RIGHT-5 (make-right 5))

(define TURN-LEFT-0 (make-left 0))
(define TURN-LEFT-1 (make-left 1))
(define TURN-LEFT-5 (make-left 5))

(define DO-TIMES-1 (make-do-times 1 FORWARD-100))
(define DO-TIMES-3 (make-do-times 3 TURN-RIGHT-1))
(define DO-TIMES-5 (make-do-times 5 FORWARD-10))

(define IF-FACING-EDGE-1 (make-if-facing-edge TURN-RIGHT-1 DO-TIMES-5))
(define IF-FACING-EDGE-2 (make-if-facing-edge DO-TIMES-3 FORWARD-100))

(define CMD-SEQ-1 
  (make-sequence-cmd 
   (list TURN-RIGHT-1 FORWARD-30 FORWARD-30 TURN-LEFT-1 
         DO-TIMES-3 IF-FACING-EDGE-1)))

(define CMD-SEQ-2 
  (make-sequence-cmd 
   (list FORWARD-30 TURN-RIGHT-1 FORWARD-30 TURN-RIGHT-1 FORWARD-30 
         TURN-RIGHT-1 FORWARD-30 TURN-LEFT-1 FORWARD-100
         DO-TIMES-1 IF-FACING-EDGE-2 CMD-SEQ-1)))
(define CMD-SEQ-3 
  (make-sequence-cmd 
   (list FORWARD-30 TURN-RIGHT-1 FORWARD-100 TURN-LEFT-1)))
(define WHILE-NOT-FACING-1 (make-while-not-facing-edge-do FORWARD-10))
(define WHILE-NOT-FACING-2 (make-while-not-facing-edge-do DO-TIMES-5))
(define WHILE-NOT-FACING-3 (make-while-not-facing-edge-do CMD-SEQ-3))

(define-test-suite robot-after-cmd-tests
  (check-equal?
       (robot-after-cmd FORWARD-0 INITIAL-ROBOT)
       INITIAL-ROBOT  
       "forward 0 still inside")
  (check-equal?
       (robot-after-cmd FORWARD-10 INITIAL-ROBOT)
       ROBOT-FORWARD-10  
       "forward 10 still inside")
  (check-equal?
       (robot-after-cmd FORWARD-100 INITIAL-ROBOT)
       ROBOT-FORWARD-100  
       "forward 100 then outside")

  (check-equal?
       (robot-after-cmd TURN-LEFT-0 INITIAL-ROBOT)
       INITIAL-ROBOT
       "turn left 0 times")
  (check-equal?
       (robot-after-cmd TURN-LEFT-1 INITIAL-ROBOT)
       (make-robot 100 100 west 0)  
       "turn left once")
  (check-equal?
       (robot-after-cmd TURN-LEFT-5 INITIAL-ROBOT)
       (make-robot 100 100 west 0)  
       "turn left five times")
  
  (check-equal?
       (robot-after-cmd TURN-RIGHT-0 INITIAL-ROBOT)
       INITIAL-ROBOT 
       "turn right 0 times")
  (check-equal?
       (robot-after-cmd TURN-RIGHT-1 INITIAL-ROBOT)
       (make-robot 100 100 east 0)  
       "turn right once")
  (check-equal?
       (robot-after-cmd TURN-RIGHT-5 INITIAL-ROBOT)
       (make-robot 100 100 east 0)  
       "turen right five times")
;;;;;;;;;;do-times
  (check-equal?
       (robot-after-cmd DO-TIMES-1 INITIAL-ROBOT)
       ROBOT-FORWARD-100  "forward 100 once")
  (check-equal?
       (robot-after-cmd DO-TIMES-3 INITIAL-ROBOT)
       (make-robot 100 100 west 0)  
       "turn right for three times")
  (check-equal?
       (robot-after-cmd DO-TIMES-5 INITIAL-ROBOT)
       (make-robot 100 50 north 50)  
       "forward 10 five times")
;;IF-FACING-EDGE-1
  (check-equal?
       (robot-after-cmd IF-FACING-EDGE-1 ROBOT-FORWARD-100)
       (make-robot 100 20 east 80)  
       "forward 100 until when face edge")
  (check-equal?
       (robot-after-cmd IF-FACING-EDGE-1 INITIAL-ROBOT)
       (make-robot 100 50 north 50)  
       "mot face edge then forward 50")
  (check-equal?
       (robot-after-cmd IF-FACING-EDGE-2 (make-robot 20 20 west 100))
       (make-robot 20 20 south 100)  
       "face edge then turn left")
  (check-equal?
       (robot-after-cmd IF-FACING-EDGE-2 INITIAL-ROBOT)
       ROBOT-FORWARD-100  
       "not face egde then forward 100")
;;sequence CMD-SEQ-1
  (check-equal?
       (robot-after-cmd CMD-SEQ-1 INITIAL-ROBOT)
       (make-robot 110 100 west 110) 
       "move robot according sequence 1")
  (check-equal?
       (robot-after-cmd CMD-SEQ-2 INITIAL-ROBOT)
       (make-robot 100 130 north 250) 
       "move robot according sequence 2")
  
;;WHILE-NOT-FACING-1
  (check-equal?
       (robot-after-cmd WHILE-NOT-FACING-1 INITIAL-ROBOT)
       (make-robot 100 20 north 80)  
       "forward 10 while not facing edge")
  (check-equal?
       (robot-after-cmd WHILE-NOT-FACING-2 INITIAL-ROBOT)
       (make-robot 100 20 north 80)  
       "forward 10 for five times while not facing edge")
  (check-equal?
       (robot-after-cmd WHILE-NOT-FACING-3 INITIAL-ROBOT)
       (make-robot 180 20 north 160)  
       "execute sequence cmd 3 while not facing edge")

  )

(run-tests robot-after-cmd-tests)

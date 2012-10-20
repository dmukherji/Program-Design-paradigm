;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp")))))
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide initial-world)
(provide run)
(provide world-to-center)
(provide world-selected?)
(provide world-after-mouse-event)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS
(define RECT-WIDTH 100)
(define RECT-HEIGHT 60)
(define HALF-RECT-WIDTH (/ RECT-WIDTH 2))
(define HALF-RECT-HEIGHT (/ RECT-HEIGHT 2))
(define RECT (rectangle RECT-WIDTH RECT-HEIGHT "solid" "green"))
(define RECT-OUTLINE (rectangle RECT-WIDTH RECT-HEIGHT "outline" "green"))

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define BACKGROUND (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define RADIUS 5)
(define RED-CIRCLE (circle RADIUS "solid" "red"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

(define-struct rect (posn))
;; A Rect is a (make-rect (make-posn Number Number))
;; where the two Number represent the coordination of the rect
;; template :
;; rect-fn : Rect -> ??
;(define (rect-fn r)
;   ...
;  (posn-x r)
;  (posn-y r))

(define-struct red-circle (posn))
;; A red-circle is a (make-red-circle (make-posn Number Number))
;; where the two Number represent the coordination of the red-circle
;; template :
;; red-circle-fn : red-circle -> ??
;(define (red-circle-fn r)
;   ...
;  (posn-x r)
;  (posn-y r))

(define-struct world (re rc selected?))
;; A World is a (make-world Number Number Boolean)
;; Interpretation: 
;; x-pos, y-pos give the position of the RECT. 
;; selected? describes whether or not the RECT is selected.
;; template:
;; world-fn : World -> ??
;(define (world-fn w)
; (... (world-x w) 
;       (world-y w) 
;       (world-selected? w)))

;; examples with rect selected
;;(define selected-rect (make-world 150 150 true))

;; A DragEvent is a MouseEvent that is one of:
;; -- "button-down"   (interp: maybe select the RECT)
;; -- "drag"          (interp: maybe drag the RECT)
;; -- "button-up"     (interp: unselect the RECT)
;; -- any other mouse event (interp: ignored)
;; template:
;(define (mev-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [(mouse=? mev "drag") ...]
;    [(mouse=? mev "button-up") ...]
;    [else ...]))

;;; END DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-defined constant for testing
(define RECT-STOP-AT-150-150 (make-world (make-rect (make-posn 150 150)) 
                                         (make-red-circle (make-posn 150 150)) 
                                         false))
(define MOUSE-AT-155-145 (make-world (make-rect (make-posn 150 150)) 
                                     (make-red-circle (make-posn 155 145)) 
                                     true))
(define DRAG-TO-180-180 (make-world (make-rect (make-posn 175 185)) 
                                    (make-red-circle (make-posn 180 180)) 
                                    true))
(define MOUSE-UP-180-180 (make-world (make-rect (make-posn 175 185)) 
                                     (make-red-circle (make-posn 175 185)) 
                                     false))

(define DRAG-IMAGE-AT-180-180 (underlay/offset RECT-OUTLINE 5 -5 RED-CIRCLE))
(define MOUSE-ON-IMAGE-AT-155-145 (underlay/offset RECT-OUTLINE 5 -5 
                                                   RED-CIRCLE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main function

;; run : Any -> World
;; Ignores its argument and runs the world. Returns the final state of the world
;; exmaple (run 1) -> (main (make-world empty 0))
;; strategy : function compostion
(define (run n)
    (main (initial-world n)))

;; initial-world : Any -> World
;; Ignores its argument and returns the initial world.
;; example : 
;; (initial-world 1) = RECT-STOP-AT-150-150
;; strategy : function compostion
(define (initial-world n)
   RECT-STOP-AT-150-150)

;; world-to-center : World -> Posn
;; return the position of the center of the rectangele
;; example : (world-to-center DRAG-TO-180-180) = (make-posn 175 185)
;; strategy: function composition
(define (world-to-center w) 
     (rect-posn (world-re w)))

;; world-to-red-circle : WOrld -> POsn
;; return the position of the center of the red-circle
;; example : (world-to-red-circle DRAG-TO-180-180) = (make-posn 180 180)
;; strategy: function composition
(define (world-to-red-circle w) 
     (red-circle-posn (world-rc w)))

;; circle-x : World -> NUmber
;; return the x-coordinate of red-circle
;; example : (circle-x DRAG-TO-180-180) = 180
;; strategy: structure decomposition on World [w]
(define (circle-x w)
   (posn-x (world-to-red-circle w)))

;; circle-y : World -> NUmber
;; return the y-coordinate of red-circle
;; example : (circle-x DRAG-TO-180-180) = 180
;; strategy: structure decomposition on World [w]
(define (circle-y w)
   (posn-y (world-to-red-circle w)))

;; circle-x : World -> NUmber
;; return the x-coordinate of red-circle
;; example : (circle-x DRAG-TO-180-180) = 180
;; strategy: structure decomposition on World [w]
(define (rect-x w)
   (posn-x (world-to-center w)))

;; circle-x : World -> NUmber
;; return the x-coordinate of red-circle
;; example : (circle-x DRAG-TO-180-180) = 180
;; strategy: structure decomposition on World [w]
(define (rect-y w)
   (posn-y (world-to-center w)))

;; world-after-mouse-event : World Number Number DragEvent -> World
;; produce the world that should follow the given mouse event
;; examples: (world-after-mouse-event DRAG-TO-180-180 155 145 "button-up")
;;           =(world-after-mouse-up DRAG-TO-180-180 155 145)
;; strategy: structure decomposition on mouse events [mev]
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down") (world-after-mouse-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up") (world-after-mouse-up w mx my)]
    [else w]))

;; select-inside-rect? World Number Number -> Boolean
;; decide whether the mouse is clicked inside the world
;; example : (select-inside-rect? RECT-STOP-AT-150-150 100 120)
;; strategy: structure decomposition on world [w]
(define (select-inside-rect? w x y)
    (and
       (<= 
          (- (rect-x w) HALF-RECT-WIDTH)
          x
          (+ (rect-x w) HALF-RECT-WIDTH))
       (<= 
          (- (rect-y w) HALF-RECT-HEIGHT)
          y
          (+ (rect-y w) HALF-RECT-HEIGHT))))

;; world-after-mouse-down : World Number Number -> World
;; return a new world based on current position after button-down
;; if select on the RECT, then stay there with select? become true
;; else nothing changes
;; example : (world-after-mouse-down RECT-STOP-AT-150-150 155 145)
;;           =MOUSE-AT-155-145
;; strategy: function composition
(define (world-after-mouse-down w x y) 
 (if (select-inside-rect? w x y)
     (make-world
        (world-re w)
        (make-red-circle (make-posn x y))
        true)
     w))

;; world-after-drag : World Number Number -> World
;; return a new world based on current position after dragging
;; if RECT has been select, return a new world locate at the position of mouse
;; example : (world-after-drag MOUSE-AT-155-145 180 180)
;;           =DRAG-TO-180-180
;; strategy: function composition
(define (world-after-drag w x y)
 (if (world-selected? w)
     (make-world 
        (make-rect (make-posn 
                        (+ (rect-x w) (- x (circle-x w)))
                        (+ (rect-y w) (- y (circle-y w)))))
        (make-red-circle (make-posn x y))
        true)
     w))

;; world-after-mouse-up : World Number Number -> World
;; return a new world based on current position after releaseing the mouse
;; if RECT has been select, return a new world at current location
;; and the RECT is no longer selected
;; example: (world-after-mouse-up DRAG-TO-180-180 180 180)
;;          = MOUSE-UP-180-180
;; strategy: function composition
(define (world-after-mouse-up w x y)
 (if (world-selected? w)
     (make-world 
        (world-re w)
        (make-red-circle (make-posn (rect-x w) (rect-y w)))
        false)
     w))


;(define (mouse-inside-canvas? x y)
;   (and 
;     (<= 0 
;         x
;         CANVAS-WIDTH)
;     (<= 0
;         y 
;         CANVAS-HEIGHT)))
;; world->scene : World -> Image

(define (world->scene w)
   (if (world-selected? w)
;       (place-image 
;           (text (string-append (number->string (rect-x w)) 
;                                "   "
;                                (number->string (rect-y w))
;                                "   "
;                                (number->string (circle-x w)) 
;                                "   "
;                                (number->string (circle-y w)) 
;                                ) 30 "green")
;           140
;           50
           (place-image
             (combine-rect-circle w)
             (rect-x w)
             (rect-y w)
             BACKGROUND)
           ;)
;(place-image 
;           (text (string-append (number->string (rect-x w)) 
;                                "   "
;                                (number->string (rect-y w))
;                                "   "
;                                (number->string (circle-x w)) 
;                                "   "
;                                (number->string (circle-y w)) 
;                                ) 30 "green")
;           140
;           50
        (place-image RECT
             (circle-x w)
             (circle-y w)
             BACKGROUND)))
;)
;; combine-rect-circle : World -> Image
;; combine the rectangle and red-circle in the world
;; example :(combine-rect-circle DRAG-TO-180-180)
;;          =DRAG-IMAGE-AT-180-180
;; strategy: structure decomposition on world [w]
(define (combine-rect-circle w)
    (underlay/offset RECT-OUTLINE
         (-  (circle-x w) (rect-x w))
         (-  (circle-y w) (rect-y w))
         RED-CIRCLE))

(define (main w)
  (big-bang w
            (on-mouse world-after-mouse-event)
            (on-draw world->scene)
            ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests:
;; Catelog:
;; select-inside-rect?-tests
;; world-after-mouse-down-tests
;; world-after-drag-tests
;; world-after-mouse-up-tests
;; world-after-mouse-event-tests
;; combine-rect-circle-tests

(define-test-suite select-inside-rect?-tests
  (check-equal?
       (select-inside-rect? RECT-STOP-AT-150-150 100 120)
       true
       " north west corner")
  (check-equal?
       (select-inside-rect? RECT-STOP-AT-150-150 100 180)
       true
       "south west corner")
  (check-equal?
       (select-inside-rect? RECT-STOP-AT-150-150 200 120)
       true
       "north east corner")
  (check-equal?
       (select-inside-rect? RECT-STOP-AT-150-150 200 180)
       true
       "south east corner")
  (check-equal?
       (select-inside-rect? RECT-STOP-AT-150-150 180 160)
       true
       "middle corner")
  (check-equal?
       (select-inside-rect? RECT-STOP-AT-150-150 201 180)
       false
       "outside")
  (check-equal?
       (select-inside-rect? RECT-STOP-AT-150-150 160 119)
       false
       "outside")
  )
(run-tests select-inside-rect?-tests)

(define-test-suite world-after-mouse-down-tests
  (check-equal?
       (world-after-mouse-down RECT-STOP-AT-150-150 155 145)
       MOUSE-AT-155-145
       "mouse at north east")
  (check-equal?
       (world-after-mouse-down RECT-STOP-AT-150-150 90 90)
       RECT-STOP-AT-150-150
       "mouse outside rect"))
(run-tests world-after-mouse-down-tests)

(define-test-suite world-after-drag-tests
(check-equal?
      (world-after-drag MOUSE-AT-155-145 180 180)
      DRAG-TO-180-180
      "DRAG TO 180 180")
  (check-equal?
       (world-after-mouse-down RECT-STOP-AT-150-150 500 500)
       RECT-STOP-AT-150-150
       "drag outside canvas"))
(run-tests world-after-drag-tests)

(define-test-suite world-after-mouse-up-tests
(check-equal?
      (world-after-mouse-up DRAG-TO-180-180 180 180)
      MOUSE-UP-180-180
      "mouse up at 180 180"))
(run-tests world-after-mouse-up-tests)

(define-test-suite combine-rect-circle-tests
(check-equal?
      (combine-rect-circle DRAG-TO-180-180)
      DRAG-IMAGE-AT-180-180
      "DRAG-IMAGE-AT-180-180")
  (check-equal?
       (combine-rect-circle MOUSE-AT-155-145)
       DRAG-IMAGE-AT-180-180
       "MOPUSE ON IMAGE"))
(run-tests combine-rect-circle-tests)

(define-test-suite world-after-mouse-event-tests
  (check-equal?
       (world-after-mouse-event RECT-STOP-AT-150-150 155 145 "button-down")
       (world-after-mouse-down RECT-STOP-AT-150-150 155 145)
       "mouse down")
  (check-equal?
       (world-after-mouse-event MOUSE-AT-155-145 155 145 "drag")
       (world-after-drag MOUSE-AT-155-145 155 145)
       "mouse down")
  (check-equal?
       (world-after-mouse-event DRAG-TO-180-180 155 145 "button-up")
       (world-after-mouse-up DRAG-TO-180-180 155 145)
       "mouse down")
  (check-equal?
       (world-after-mouse-event DRAG-TO-180-180 155 145 "enter")
       DRAG-TO-180-180
       "else"))
(run-tests world-after-mouse-event-tests)
(run 1)

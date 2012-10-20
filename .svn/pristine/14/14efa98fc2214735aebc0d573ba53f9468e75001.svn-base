;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide initial-world)
(provide run)
(provide world-after-mouse-event)
(provide world-after-key-event)
(provide world-to-trees)
(provide tree-to-root-node)
(provide node-to-sons)
(provide node-to-center)
(provide node-to-selected?)
(provide node-room-for-son?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANT VARIABLES
(define RADIUS 10)
(define 3RADII (* 3 RADIUS))
(define 4RADII (* 4 RADIUS))
(define DIAMETER (* 2 RADIUS))
(define NODE (circle RADIUS "outline" "green"))
(define NODE-SELECTED (circle RADIUS "solid" "green"))
(define NODE-ERROR (circle RADIUS "solid" "red"))
(define CANVAS-WIDTH 300)
(define CANVAS-HEIGHT 400)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
(define BACKGROUND (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define INITIAL-ROOT-POSN (make-posn HALF-CANVAS-WIDTH RADIUS))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data definition

;; A Posn is (make-posn Number Number)
;; Interp:
;; x is the x-cood number for the position
;; y is the y-cood number for this position
;; template
;; posn-fn : Posn -> ?
;(define (posn-fn p)
;    ...
;    (posn-x p)
;    (posn-y p))

(define-struct node (lon selected? center))
;; A Node is (make-node ListOf<Node> Boolean Posn)
;; lon       : is a ListOf<Node> represents the sub-nodes of the given node
;; selected? : is a Boolean to flag whether the node is selected
;; center    : is a Posn represents the central position 

;; template:
;; node-fn : node->?
;(define (node-fn node)
;    (...
;     (node-lon node)
;     (node-selected? node)
;     (node-center node)))

;; example: 
(define INITIAL-ROOT-NODE (make-node empty false INITIAL-ROOT-POSN))

;; A ListOf<Node> (LON) which is either
;; -- empty                     (interp: no node in the list)
;; -- (cons Node ListOf<Node>)  (interp: a node followed by a sequence of node)

;; template:
;; lon-fn : ListOf<Node> -> ?
;(define (lon-fn lon)
;   (cond 
;       [(empty? lon) ...]
;       [else (...
;                (node-fn (first lon))
;                (lon-fn (rest lon)))]))

(define-struct world (lon))
;; A World is a (make-world ListOf<Node>)
;; interp:
;; lon is a ListOf<Node> containing all root nodes in the canvas
;; templte
;; w-fn : World -> ?
;(define (w-fn w)
;    (... 
;     (world-lon w)))

;; A NodeKeyEvent is a KeyEvent that is one of:
;; "t" (interp: at any time, a new root node is added in the center 
;;              of the top of the canvas)
;; "n" (interp: add a new sub-node to the selected root node. 
;;              if nore more space, then ignore "n")
;; "d" (interp: delete the selected node and its sub-nodes)
;; other key inputs (interp: all other keys are ignored)

;; template:
;; bke-fn : KeyEvent -> ?
;(define (bke-fn key)
;   (cond
;       [(string=? key "t") ...]
;       [(string=? key "n") ...]
;       [(string=? key "d") ...]
;       [else ...]))

;; A DragEvent is a MouseEvent that is one of:
;; -- "button-down"   (interp: select the node)
;; -- "drag"          (interp: drag the node)
;; -- "button-up"     (interp: unselect the node)
;; -- any other mouse event (interp: ignored)

;; template:
;; mev-fn : MouseEvent -> ?
;(define (mev-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [(mouse=? mev "drag") ...]
;    [(mouse=? mev "button-up") ...]
;    [else ...]))

; [X] (X -> Boolean) [List-of X] -> [List-of X]
; produce a list from all those items on list for which p holds
; (define (filter p list) ...)

; [X Y] (X Y -> Y) Y [List-of X] -> Y
; compute the result of applying f from right to left to all of
; alox and base, that is, apply f to
;    the last item in alox and base,
;    the penultimate item and the result of the first step,
;    and so on up to the first item
;    (foldr f base (list x-1 ... x-n)) = (f x-1 ... (f x-n base))
; (define (foldr f base alox) ...)

; [X Y] (X -> Y) [List-of X] -> [List-of Y]
; construct a list by applying f to each item on alox
;    (map f (list x-1 ... x-n)) = (list (f x-1) ... (f x-n))
;(define (map f alox) ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define TEST-IMAGE-WORLD1
  (make-world
     (list 
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 50 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 20 240)))
                true
                (make-posn 120 200)))
       false
       (make-posn 100 160)))))

(define TEST-IMAGE-WORLD2
  (make-world
     (list 
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 50 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 20 240)))
                false
                (make-posn 120 200)))
       false
       (make-posn 100 160)))))
(define TEST-IMAGE-WORLD3
  (make-world
     (list 
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 50 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 20 240)))
                false
                (make-posn 120 200)))
       true
       (make-posn 100 160)))))
(define TEST-IMAGE-LIST
     (list 
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 50 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 20 240)))
                true
                (make-posn 120 200)))
       false
       (make-posn 100 160))))
(define RESULT-IMAGE1
  (scene+line
   (scene+line
    (scene+line
     (place-image
      NODE
      20 240
      (place-image
       NODE-ERROR
       120 200
       (place-image
        NODE
        50 80
        (place-image
         NODE
         100 160
         BACKGROUND)))) 100 160 50 80 "blue")
    100 160 120 200 "blue")
   120 200 20 240 "blue")
    )
(define RESULT-IMAGE2
  (scene+line
   (scene+line
    (scene+line
     (place-image
      NODE
      20 240
      (place-image
       NODE
       120 200
       (place-image
        NODE
        50 80
        (place-image
         NODE
         100 160
         BACKGROUND)))) 100 160 50 80 "blue")
    100 160 120 200 "blue")
   120 200 20 240 "blue")
    )
(define RESULT-IMAGE3
  (scene+line
   (scene+line
    (scene+line
     (place-image
      NODE
      20 240
      (place-image
       NODE
       120  200
       (place-image
        NODE
        50 80
        (place-image
         NODE-SELECTED
         100 160
         BACKGROUND)))) 100 160 50 80 "blue")
    100 160 120 200 "blue")
   120 200 20 240 "blue"))
(define SECOND-TREE     
  (make-node ;; root node of all node (level-0)
     (list ;; not root node (level-1)
         (make-node empty false (make-posn 100 100))
          ;; root node     (level-1)
         (make-node (list ;; not root node (level-2)
                (make-node empty false (make-posn 180 350))
                ;; not root node (level-2)
                (make-node empty false (make-posn 240 260)))
                false
                (make-posn 200 300)))
   false
   (make-posn 200 150)))
(define SECOND-TREE-SELECTED     
  (make-node ;; root node of all node (level-0)
     (list ;; not root node (level-1)
         (make-node empty false (make-posn 100 100))
          ;; root node     (level-1)
         (make-node (list ;; not root node (level-2)
                (make-node empty false (make-posn 180 350))
                ;; not root node (level-2)
                (make-node empty false (make-posn 240 260)))
                false
                (make-posn 200 300)))
   true
   (make-posn 200 150)))

(define NEW-TREE 
  (make-node empty false (make-posn 150 10)))
(define EMPTY-WORLD 
   (make-world empty))
(define ONE-TREE-WORLD 
   (make-world (list NEW-TREE)))
(define TWO-TREE-WORLD 
   (make-world (list NEW-TREE NEW-TREE)))

(define INITIAL-TEST
  (make-world
     (list 
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define MOUSE-DOWN-ON-LEVEL-0-ROOT
  (make-world
     (list 
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       true
       (make-posn 100 160)))))
(define MOUSE-DOWN-ON-LEVEL-1-NOT-ROOT
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty true (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))
(define MOUSE-DOWN-ON-LEVEL-1-NOT-ROOT-BOUNDARY
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty true (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define MOUSE-DOWN-ON-LEVEL-1-ROOT
    (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                true
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty true (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define DRAG-LEVEL-0-ROOT-INSIDE
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 180))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 340))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 350)))
                false
                (make-posn 120 300))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 210)))
       true
       (make-posn 100 260)))))
(define DRAG-LEVEL-0-ROOT-OUTSIDE
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 280))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 440))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 450)))
                false
                (make-posn 120 400))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 310)))
       true
       (make-posn 100 360)))))

(define DRAG-LEVEL-1-NOT-ROOT-INSIDE
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty true (make-posn 45 100))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define DRAG-LEVEL-1-NOT-ROOT-OUTSIDE
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty true (make-posn 8 100))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))
(define DRAG-LEVEL-1-ROOT-INSIDE
    (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 220 140))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 100 150)))
                true
                (make-posn 60 100))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define DRAG-LEVEL-1-ROOT-OUTSIDE
    (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 445 140))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 325 150)))
                true
                (make-posn 285 100))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define DRAG-LEVEL-2-NOT-ROOT-INSIDE
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty true (make-posn 200 200))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))
(define LEVEL-2-NOT-ROOT-INSIDE-FOR-OVERLAP-NOT-SELECTED
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 210 300))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define MOUSE-DOWN-LEVEL-2-NOT-ROOT-INSIDE-FOR-OVERLAP
  (make-world
     (list  
      (make-node ;; root node of all node (level-0)
       (list ;; not root node (level-1)
        (make-node empty false (make-posn 100 100))
        ;; root node     (level-1)
        (make-node (list ;; not root node (level-2)
                    (make-node empty false (make-posn 180 350))
                    ;; not root node (level-2)
                    (make-node empty false (make-posn 240 260)))
                   true
                   (make-posn 200 300)))
       false
       (make-posn 200 150))
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty true (make-posn 210 300))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))
(define DRAG-LEVEL-2-NOT-ROOT-INSIDE-FOR-OVERLAP
  (make-world
     (list  
      (make-node ;; root node of all node (level-0)
       (list ;; not root node (level-1)
        (make-node empty false (make-posn 100 100))
        ;; root node     (level-1)
        (make-node (list ;; not root node (level-2)
                    (make-node empty false (make-posn 160 330))
                    ;; not root node (level-2)
                    (make-node empty false (make-posn 220 240)))
                   true
                   (make-posn 180 280)))
       false
       (make-posn 200 150))
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty true (make-posn 180 280))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))
;;separate two balls
(define MOUSE-DOWN-LEVEL-1-ROOT-INSIDE-FOR-OVERLAP
  (make-world
     (list  
      (make-node ;; root node of all node (level-0)
       (list ;; not root node (level-1)
        (make-node empty false (make-posn 100 100))
        ;; root node     (level-1)
        (make-node (list ;; not root node (level-2)
                    (make-node empty false (make-posn 160 330))
                    ;; not root node (level-2)
                    (make-node empty false (make-posn 220 240)))
                   false
                   (make-posn 180 280)))
       false
       (make-posn 200 150))
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 180 280))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                true
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define SEPARATE-LEVEL-2-NOT-ROOT-INSIDE
  (make-world
     (list  
      (make-node ;; root node of all node (level-0)
       (list ;; not root node (level-1)
        (make-node empty false (make-posn 100 100))
        ;; root node     (level-1)
        (make-node (list ;; not root node (level-2)
                    (make-node empty false (make-posn 160 330))
                    ;; not root node (level-2)
                    (make-node empty false (make-posn 220 240)))
                   false
                   (make-posn 180 280)))
       false
       (make-posn 200 150))
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 200 300))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 180 270)))
                true
                (make-posn 140 220))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))
(define DRAG-LEVEL-2-NOT-ROOT-OUTSIDE
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty true (make-posn 20 380))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

;;;;;;;;;;;;;for key event
(define MOUSE-DOWN-ON-LEVEL-0-ROOT-ADD-1
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 45 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       true
       (make-posn 100 160)))))

(define MOUSE-DOWN-ON-LEVEL-0-ROOT-ADD-2
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  
                ;; not root node (level-1)
                (make-node empty false (make-posn 15 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 45 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       true
       (make-posn 100 160)))))

(define DRAG-LEVEL-1-NOT-ROOT-INSIDE-ADD-1
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node (list
                                (make-node empty false (make-posn 45 140))) 
                           true (make-posn 45 100))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define DRAG-LEVEL-1-NOT-ROOT-INSIDE-ADD-2
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node (list
                                (make-node empty false (make-posn 15 140))
                                (make-node empty false (make-posn 45 140))) 
                           true (make-posn 45 100))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define DRAG-LEVEL-1-ROOT-INSIDE-ADD-1
    (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 70 140))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 220 140))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 100 150)))
                true
                (make-posn 60 100))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define DRAG-LEVEL-2-NOT-ROOT-INSIDE-ADD-1
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node 
                        ;;new node here 
                         (list (make-node empty false (make-posn 200 240)))
                         true (make-posn 200 200))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

;; delete node
(define DELETE-LEVEL-2-NOT-ROOT
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define DELETE-LEVEL-1-ROOT
    (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

(define DELETE-LEVEL-1-NOT-ROOT
  (make-world
     (list  
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))

;; leave only second tree and it's selected
(define DELETE-LEVEL-0-ROOT-SELECTED
  (make-world (list SECOND-TREE-SELECTED)))

;; leave only second tree and it's not selected
(define DELETE-LEVEL-0-ROOT
  (make-world (list SECOND-TREE)))

(define INITIAL-TEST-ADD-NEW-TREE
  (make-world
     (list  
      NEW-TREE 
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))
(define INITIAL-TEST-ADD-NEW-TREE-TWICE
  (make-world
     (list 
      NEW-TREE
      NEW-TREE 
      SECOND-TREE
      (make-node ;; root node of all node (level-0)
         (list  ;; not root node (level-1)
                (make-node empty false (make-posn 75 80))
                ;; root node     (level-1)
                (make-node 
                 (list ;; not root node (level-2)
                       (make-node empty false (make-posn 280 240))
                       ;; not root node (level-2)
                       (make-node empty false (make-posn 160 250)))
                false
                (make-posn 120 200))
                ;; not root node (level-1)
                (make-node empty false (make-posn 290 110)))
       false
       (make-posn 100 160)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; main: World -> ?
;; Given a initial world, run the program
;; example: (main (make-world empty))
;; strategy : function composition
(define (main w)
  (big-bang w
            (on-draw render)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)
            ))

;; initial-world : Any -> World
;; interp: Ignore the input, return an initial world
;; example: (initial-world a)=(make-world empty)
;; strategy : Domain Knowledge
(define (initial-world a)
    (make-world empty))

(define-test-suite initial-world-tests
  (check-equal?
     (initial-world 12)
     (make-world empty)
     "ignore input and generate a new empty world"))

(run-tests initial-world-tests)
;; run : Any -> World
;; runs the world. Returns the final state of the world
;; exmaple (run 1) -> (main (make-world empty))
;; strategy : function composition
(define (run n)
    (main (initial-world n)))

;; node-to-sons : World Tree Node -> ListOf<Node>
;; return the sons of the given node
;; example:(node-to-sons 1 2 (make-node empty false (make-posn 100 200)))=empty
;; strategy : structural decomposition on node [Node]
(define (node-to-sons world tree node)
    (node-lon node))

(define-test-suite node-to-sons-tests
  (check-equal?
     (node-to-sons 1 2 (make-node 
                       (list (make-node empty false (make-posn 100 200)))
                       false
                       (make-posn 200 200)))
     (list (make-node empty false (make-posn 100 200)))
     "have sub-nodes")
  (check-equal?
     (node-to-sons 1 2 (make-node empty false (make-posn 100 200)))
     empty
     "no sub-nodes"))
(run-tests node-to-sons-tests)
;; render: World -> Image
;; draw all nodes in the world
;; example: (render TEST-IMAGE-WORLD1)=RESULT-IMAGE1
;; strategy : function composition
(define (render world)
  (draw-lines (world-to-trees world)
    (draw-lon (world-to-trees world) BACKGROUND)))

;; draw-lon : ListOf<Node> Image -> Image
;; draw all node in the given image
;; example:(render TEST-IMAGE-LIST)=RESULT-IMAGE-LIST
;; strategy : structural decomposition on lon [ListOf<Node>]
#(define (draw-lon lon image)
   (cond 
       [(empty? lon) image]
       [else (draw-node
                (first lon)
                (draw-lon (rest lon) image))]))
;; strategy : higher order function composition
(define (draw-lon lon image)
   (foldr draw-node image lon))

;; draw-node : Node Image-> Image
;; draw the node and all blue line between root and sub nodes
;; example: (draw-node (make-node empty false (make-posn 50 80)) BACKGROUND)
;;  (place-image NODE 50 80 BACKGROUND)
;; strategy : structural decomposition on node [Node]
(define (draw-node node image)
     (place-image
       (choose-node-type node)
       (get-posn-x (node-to-center node))
       (get-posn-y (node-to-center node))
       (overlay
        (draw-lon (node-lon node) image)
        image)))

;; draw-lines : ListOf<Node> Image -> Image
;; draw all lines in the given lsit to the given image
;; example: (draw-lines TEST-IMAGE-LIST BACKGROUND)=RESULT-IMAGE-LIST
;; strategy : structural decomposition on lon [ListOf<Node>]
#(define (draw-lines lon image)
   (cond 
       [(empty? lon) image]
       [else (draw-lines-helper
                (first lon)
                (draw-lines (rest lon) image))]))
;; strategy : higher order function composition
(define (draw-lines lon image)
   (foldr draw-lines-helper image lon))

;; draw-lines-helper Node Image -> Image
;; draw all lines involved in the given Node to the Image
;; and return a new image
;; example: 
;; (draw-lines-helper (make-node empty false (make-posn 50 80)) BACKGROUND)
;;  =(place-image NODE 50 80 BACKGROUND)
;; strategy : structural decomposition on node [Node]
(define (draw-lines-helper node image)
        (draw-line2node (node-lon node) (node-center node) image))

;; draw-line2node : ListOf<Node> Posn Image -> Image
;; override the draw-lines and draw line from each node in the given list
;; to the given Posn on the image
;; example:(draw-lines TEST-IMAGE-LIST (make-posn 50 80) BACKGROUND)
;;          =RESULT-IMAGE-LIST
;; strategy : structural decomposition on lon [ListOf<Node>]
#(define (draw-line2node lon center image)
   (cond 
       [(empty? lon) image]
       [else (overlay-helper
                (first lon) center
                (draw-lines2 (rest lon) center image))]))
;; strategy : higher order function composition
(define (draw-line2node lon center image)
  (local (;; draw-line2node-helper : ListOf<Node> Image -> Image
          ;; overlay each one on the existing image
          (define (draw-line2node-helper lon image) 
               (overlay-helper lon center image)))
    (foldr draw-line2node-helper image lon)))

;; overlay-helper: Node Posn Image -> Image
;; draw the line from the Node to the Posn on the Image
;; example:
;; (overlay-helper (make-node empty false false 
;                    (make-posn 120 220) 0) (make-posn 120 220) image)=image
;; strategy : structural decomposition on node [Node]
(define (overlay-helper node center image)
  (scene+line
      (draw-line2node (node-lon node) (node-to-center node) image)
      (get-posn-x (node-to-center node))
      (get-posn-y (node-to-center node))
      (get-posn-x center)
      (get-posn-y center)
      "blue"))

;; choose-node-type : Node -> Image
;; choose the correct image type for display
;; example:(choose-node-type (make-node empty false (make-posn 50 80)))
;;         = NODE
;; strategy : function composition
(define (choose-node-type node)
    (if (node-to-selected? 1 2 node)
        (if (node-room-for-son? 1 2 node)
            NODE-SELECTED
            NODE-ERROR)
        NODE))

;; world-to-trees : World -> ListOf<Node>
;; returns a list of nodes in the given world
;; examples:(world-to-trees TEST-IMAGE-WORLD1)=TEST-IMAGE-LIST
;; (world-to-trees initial-world) = empty
;; strategy: structural decomposition on world [w]
(define (world-to-trees w)
    (world-lon w))

(define-test-suite world-to-trees-tests
  (check-equal?
     (world-to-trees (make-world empty))
     empty
     "empty world")
  (check-equal?
     (world-to-trees (make-world (list SECOND-TREE)))
     (list SECOND-TREE)
     "non-empty world"))
(run-tests world-to-trees-tests)
;; world-after-mouse-event : World Number Number MouseEvent -> World
;; for position, select or drag or release a suitable node. 
;; And return a new world.
;; examples:
;; (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-0-ROOT 60 100 "button-up")
;; =INITIAL-TEST
;; strategy : structural decomposition on mev [MouseEvent]
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down") (world-after-mouse-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up") (world-after-mouse-up w mx my)]
    [else w]))

;; world-after-mouse-down : World Number Number -> World
;; select the nodes whcih the given Posn locates inside the node
;; example: 
;; (world-after-mouse-down INITIAL-TEST 130 200)=MOUSE-DOWN-ON-LEVEL-1-ROOT
;; strategy : structural decomposition on world [World]
(define (world-after-mouse-down world x y)
     (make-world (world-after-mouse-down-helper (world-lon world) x y)))
 
;; world-after-mouse-down-helper : ListOf<Node> Number Number -> ListOf<Node>
;; for each node in the given list, update its information 
;; based on the given position
;; example : 
;; (world-after-mouse-down-helper INITIAL-TEST-LIST 130 200)
;;           =MOUSE-DOWN-ON-LEVEL-1-ROOT-LIST
;; strategy : structural decomposition on lon [ListOf<Node>]  
#(define (world-after-mouse-down-helper lon x y)
   (cond 
       [(empty? lon) empty]
       [else (cons
                (update-node-to-selected (first lon) x y)
                (world-after-mouse-down-helper (rest lon ) x y))]))

;; strategy : higher order function composition
(define (world-after-mouse-down-helper lon x y)
  (local (;; world-after-mouse-down-map: Node -> Node
          ;; updated node which is selected after mouse is down
          (define (world-after-mouse-down-map node)
              (update-node-to-selected node x y)))
   (map world-after-mouse-down-map lon)))
;; update-node-to-selected : Node Number Number -> Node
;; update the node infor after mouse down
;; if selected inside, change selected? to true
;; else false
;; example: 
;; (update-node-to-selected (make-node empty false (make-posn 10 30)) 10 30)
;; =(make-node empty true (make-posn 10 30))
;; strategy : structural decomposition on node [Node]
(define (update-node-to-selected node x y)
   (make-node
       (world-after-mouse-down-helper (node-lon node) x y)
       (selected-inside-node? (node-center node) x y)
       (node-center node)))

;; selected-inside-node? : Posn Number Number -> Boolean
;; Decide whether the given Posn is inside the given node
;; example: (selected-inside-node? (make-posn 10 30) 10 30)=true
;; strategy : function composition
(define (selected-inside-node? center x y)
    (<= (real-part (sqrt (+ (expt (- (get-posn-x center) x) 2)
                (expt (- (get-posn-y center) y) 2)))) RADIUS))
  
;; world-after-drag : World Number Number -> World
;; drag the selected node and its sub-node in the given world
;; return a new world after drag
;; example:  (world-after-drag MOUSE-DOWN-ON-LEVEL-0-ROOT 100 260)
;;           =DRAG-LEVEL-0-ROOT-INSIDE
;; strategy : structural decomposition on world [World]
(define (world-after-drag world x y)
    (make-world 
        (world-after-drag-helper (world-lon world) x y x y)))

;; world-after-drag-helper : ListOf<Node> Number Number Number Number
;;                                         -> ListOf<Node>
;; drag the selected node and its sub-node in the given LOB
;; return a new world after drag
;; example:  
;; (world-after-drag-helper MOUSE-DOWN-ON-LEVEL-0-ROOT-LIST 100 260 100 260)
;;           =DRAG-LEVEL-0-ROOT-INSIDE-LIST
;; strategy : structural decomposition on lon [ListOf<Node>]
#(define (world-after-drag-helper lon a s x y)
   (cond 
       [(empty? lon) empty]
       [else (cons
                (drag-node (first lon) a s x y)
                (world-after-drag-helper (rest lon) a s x y))]))

;; strategy : higher order function composition
(define (world-after-drag-helper lon a s x y)
  (local (;; world-after-drag-helper :  Node -> Node
          ;; updated node which is selected after dragging
          (define (drag-node-helper node)
              (drag-node node a s x y)))
   (map drag-node-helper lon)))
;; drag-node : Node Number Number Number Number-> Node
;; drag the given node to a new position based on the given Posn
;; and return the new node
;; example: 
;; (drag-node (make-node empty false (make-posn 10 30)) 10 30) 20 40 10 25)
;;           =(make-node empty false (make-posn 20 55))
;; strategy : function composition
(define (drag-node node a s x y)
  (if (node-selected? node)
       (make-node
        (world-after-drag-helper (node-lon node) 
                                 (get-posn-x (node-center node))
                                 (get-posn-y (node-center node)) x y)
        (node-selected? node)
        (make-posn x y))
       (make-node
        (world-after-drag-helper (node-lon node) a s x y)
        (node-selected? node)
        (make-posn (+ x (- (get-posn-x  (node-center node)) a))
                   (+ y (- (get-posn-y  (node-center node)) s))))))

;; world-after-mouse-up : World Number Number -> World
;; release the button, updated related node in the world 
; ;and return a new world
;; example:
;; (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-0-ROOT 60 100) =INITIAL-TEST
;; strategy : structural decomposition on world [World]
(define (world-after-mouse-up world x y)
    (make-world 
        (world-after-mouse-up-helper (world-lon world))))

;; world-after-mouse-up-helper : ListOf<Node> -> ListOf<Node>
;; change the selected node's selected? field to false
;; return a new world after mouse up
;; example: (world-after-mouse-up-helper MOUSE-DOWN-ON-LEVEL-0-ROOT-LIST)
;;          =INITIAL-TEST-LIST
;; strategy : structural decomposition on lon [ListOf<Node>]
#(define (world-after-mouse-up-helper lon)
   (cond 
       [(empty? lon) empty]
       [else (cons
                (node-after-mouse-up (first lon))
                (world-after-mouse-up-helper (rest lon)))]))


;; strategy : higher order function composition
(define (world-after-mouse-up-helper lon)
   (map node-after-mouse-up lon))

;; node-after-mouse-up : Node-> Node
;; change the selected? feld to false, represent the node is not selected
;; and return the new node
;; example: 
;; (world-after-mouse-up-helper (make-node empty true (make-posn 10 30)))
;;          =(make-node empty false (make-posn 10 30))
;; strategy : structural decomposition on node [Node]
(define (node-after-mouse-up node)
   (if (node-selected? node)
       (make-node
          (node-lon node)
          false
          (node-center node))
       (make-node
          (world-after-mouse-up-helper (node-lon node))
          (node-selected? node)
          (node-center node))))

;; world-after-key-event : World KeyEvent -> World
;; Returns the world that follows the given world after the given key event.
;; example : (world-after-key-event MOUSE-DOWN-ON-LEVEL-0-ROOT "n")
;;           =MOUSE-DOWN-ON-LEVEL-0-ROOT-ADD-1
;; strategy: structural decomposition on key [NodeKeyEvent]
(define (world-after-key-event w key)
   (cond
       [(string=? key "t") (creat-new-root-node w)]
       [(string=? key "n") (add-new-node w)]
       [(string=? key "d") (delete-node w)]
       [else w]))

;; creat-new-root-node : World > World
;; add a new initial root node to the world, and return the new world
;; example: (creat-new-root-node EMPTY-WORLD) = ONE-TREE-WORLD
;; strategy : function composition
(define (creat-new-root-node world)
    (make-world (cons 
                 INITIAL-ROOT-NODE
                 (world-to-trees world))))

;; add-new-node : World -> World
;; add a new sub-node to the slected node in the world, and return the new world
;; example: (add-new-node EMPTY-WORLD) = ONE-TREE-WORLD
;; strategy : function composition
(define (add-new-node world)
     (make-world (add-new-node-helper (world-to-trees world))))

;; add-new-node-helper : ListOf<Node> -> ListOf<Node>
;; for the given list of nodes, update each node if it can add a new node
;; then, return the new list
;; example: (add-new-node empty) = (list NEW-TREE)
;; strategy : structural decomposition on lon [ListOf<Node>]
#(define (add-new-node-helper lon)
   (cond 
       [(empty? lon) empty]
       [else (cons
                (add-new-sub-node (first lon))
                (add-new-node-helper (rest lon)))]))
;; strategy : higher order function composition
(define (add-new-node-helper lon)
   (map add-new-sub-node lon))

;; add-new-sub-node : Node -> Node
;; for the given node, return the updated one
;; if it has room for new one and is selected, return the new one,
;; else return the old one
;; example: (add-new-sub-node (make-node empty false (make-posn 10 30))
;;          =(make-node empty false (make-posn 10 30))
;; strategy : function composition
(define (add-new-sub-node node)
    (if (can-add-new-ball? node)
        (add-new-sub-node-helper node)
        (search-addable-sub-node node)))

;; can-add-new-ball? : Node -> Node
;; decide whethe rthe given ball is available for adding a new ball
;; example: (can-add-new-ball? (make-node empty false (make-posn 10 30)) =false
;; strategy : function composition
(define (can-add-new-ball? node)
   (and (node-to-selected? 1 2 node) (node-room-for-son? 1 2 node)))

;; add-new-sub-node-helper : Node -> Node
;; return the updated node if the node is selected 
;; and has enough room for a new node
;; example: (add-new-sub-node-helper (make-node empty false (make-posn 10 30))
;;          =(make-node empty false (make-posn 10 30))
;; strategy : structural decomposition on node [Node]
(define (add-new-sub-node-helper node)
    (make-node
       (cons (new-sub-node node) (node-lon node))
       (node-selected? node)
       (node-center node)))

;; search-addable-sub-node : Node -> Node
;; seach for sub-node that can add new ball
;; example: (search-addable-sub-node (make-node empty false (make-posn 10 30))
;;          =(make-node empty false (make-posn 10 30))
;; strategy : structural decomposition on node [Node]
(define (search-addable-sub-node node)
    (make-node
       (add-new-node-helper (node-lon node))
       (node-selected? node)
       (node-center node)))

;; node-to-selected? : World Tree Node -> Boolean
;; decide whether the is selected
;; example: 
;; (node-to-selected? 1 2 (make-node empty false (make-posn 10 30))=false
;; strategy : structural decomposition on node [Node]
(define  (node-to-selected? w t node)
   (node-selected? node))

(define-test-suite node-to-selected?-tests
  (check-equal?
     (node-to-selected? 1 2 (make-node empty false (make-posn 15 200)))
     false
     "not selected")
  (check-equal?
     (node-to-selected? 1 2 (make-node empty true (make-posn 15 200)))
     true
     "selected"))
(run-tests node-to-selected?-tests)
;; node-room-for-son? : World Tree Node -> Boolean
;; decide whether the given node has enough room for a new son
;; it requires to check whether the new ball is totally inside the canvas
;; including left, right, bottom, top
;; PS. first two parameters are ignored
;; example: 
;; (node-room-for-son? 1 2 (make-node empty false (make-posn 10 30))=false
;; strategy : function composition
(define (node-room-for-son? world tree node)
  (and
     ;; left edge of new ball >= 0?
     (>= (- (new-sub-node-x node) RADIUS) 0)
     ;; right edge of new ball <= canvas width
     (<= (+ RADIUS (new-sub-node-x node)) CANVAS-WIDTH)
     ;; bottom edge of new ball <= canvas height
     (<= (+ (new-sub-node-y node) RADIUS) CANVAS-HEIGHT)
     ;; top edge of new ball >= 0
     (>= (- (new-sub-node-y node) RADIUS) 0)))

;; new-sub-node-x : Node -> Number
;; generate the x of new sub-node 
;; example: 
;; (new-sub-node-x (make-node empty false (make-posn 10 30)) = -20
;; strategy : structural decomposition on node [Node]
(define (new-sub-node-x node)
  (if (empty? (node-lon node))
      (get-posn-x (node-to-center node))
      (- (get-minX node) 3RADII)))

;; get-minX Node: Node -> Number
;; get the minimal X for all sub-nodes
;; example: 
;; (get-minX (make-node empty false (make-posn 10 30)) = -20
;; strategy : structural decomposition on node [Node]
(define (get-minX node)
    (get-minX-helper (node-lon node)))

;; get-minX Node: ListOf<Node> -> Number
;; get the minimal X for sub-nodes
;; example: 
;; (get-minX-helper (list (make-node empty false (make-posn 10 30)))) = -20
;; strategy : structural decomposition on lon [ListOf<Node>]
#(define (get-minX-helper lon)
   (cond 
       [(empty? lon) (* 3 CANVAS-WIDTH)]
       [else (min
                (get-posn-x (node-to-center (first lon)))
                (get-minX-helper (rest lon)))]))


;; strategy : higher order function composition
(define (get-minX-helper lon)
  (local (;; draw-lon-map Node -> Number
          ;; get the x axis position of the given node
          (define (draw-lon-map node)
              (get-posn-x (node-to-center node))))
    (foldr min (* 3 CANVAS-WIDTH) (map draw-lon-map lon))))

;; new-sub-node-y : Node -> Number
;; generate the y of new sub-node 
;; example:
;; (new-sub-node-y (make-node empty false (make-posn 10 30)) = 70
;; strategy : function composition
(define (new-sub-node-y node)
    (+ (get-posn-y (node-to-center node)) 4RADII))

;; new-sub-node : Node-> Node
;; generate the new sub-node based on the givenn node
;; example:
;; (new-sub-node (make-node empty false (make-posn 50 30)) 
;   = (make-node empty false (make-posn 50 70)
;; strategy : function composition
(define (new-sub-node node)
    (make-node empty false
     (make-posn (new-sub-node-x node) (new-sub-node-y node))))

;; get-posn-x : Posn -> Number
;; given a position of root, return the x axis position of it
;; example: (get-posn-x (make-posn 2 3))=2
;; strategy : structural decomposition on posn [Posn]
(define (get-posn-x posn)
   (posn-x posn))

;; get-posn-y : Posn -> Number
;; given a position of root, return the y axis position of it
;; example: (get-posn-y (make-posn 2 3))=3
;; strategy : structural decomposition on posn [Posn]
(define (get-posn-y posn)
   (posn-y posn))

;; node-to-center : Node-> Posn
;; return the central position of the given Node
;; example: (node-to-center (make-node empty false (make-posn 10 30)))
;;          = (make-posn 10 30)
;; strategy : structural decomposition on node [Node]
(define (node-to-center node)
   (node-center node))

(define-test-suite node-to-center-tests
  (check-equal?
     (node-to-center (make-node empty false (make-posn 75 280)))
     (make-posn 75 280)
     "regular node"))
(run-tests node-to-center-tests)
;; delete-node : World -> World
;; delete the selected node and its sub-tree
;; example: (world-after-key-event MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT)
;;         =DELETE-LEVEL-2-NOT-ROOT
;; strategy : structural decomposition on world [World]
(define (delete-node world)
    (make-world 
       (delete-node-helper (world-lon world))))

;; delete-node-helper : ListOf<Node> -> ListOf<Node>
;; for the given list of nodes, delete the node if it is selected 
;; besides, also delete the selected node's sub-nodes
;; then, return the new list
;; example: (delete-node-helper MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT-LIST)
;;          =DELETE-LEVEL-2-NOT-ROOT-LIST
;; strategy : function composition
(define (delete-node-helper lon)
   (update-unsected-node-for-deletion (filter-selected-node lon)))
;;;;;;;;;;;;;;;;;;;;;;old;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;#(define (delete-node-helper lon)
;   (cond 
;       [(empty? lon) empty]
;       [else (if (not (node-to-selected? 1 2 (first lon)))
;                 (cons 
;                    (update-node-for-deletion (first lon))
;                    (delete-node-helper (rest lon)))
;                 (delete-node-helper (rest lon))
;                 )]))
;;;;;;;;;;;;;;;;;;;;;;old;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; filter-selected-node : ListOf<Node> -> ListOf<Node>
;; it is used to filter selected node
;; example:
;; (filter-unselected-node (list (make-node empty true (make-posn 10 30))))
;;         =empty
;; strategy : structural decomposition on lon [ListOf<Node>]
#(define (filter-selected-node lon)
   (cond 
       [(empty? lon) empty]
       [else (if (not (node-to-selected? 1 2 (first lon)))
                 (cons 
                    (first lon)
                    (delete-node-helper (rest lon)))
                 (filter-selected-node (rest lon))
                 )]))
;; strategy : higher order function composition
(define (filter-selected-node lon)
  (local (;;selected-helper: Node -> Boolean
          ;; decide whether the given node is selected or not
          (define (selected-helper node)
              (not (node-to-selected? 1 2 node))))
  (filter selected-helper lon)))

;; delete-node-helper-helper: ListOf<Node> -> ListOf<Node>
;; for all unselected nodes, use a recursion to update their sub-nodes
;; example:
;; (update-unsected-node-for-deletion (list (make-node empty 
;;                                          false (make-posn 10 30))))
;;         =(list (make-node empty false (make-posn 10 30)))
;; strategy : structural decomposition on lon [ListOf<Node>]
#(define (update-unsected-node-for-deletion lon)
   (cond 
       [(empty? lon) empty]
       [else (cons 
                (update-node-for-deletion (first lon))
                (update-unsected-node-for-deletion (rest lon)))]))

;; strategy : higher order function composition
(define (update-unsected-node-for-deletion lon)
  (map update-node-for-deletion lon))

;; update-node-for-deletion : Node -> Node
;; for the given node, return the updated one
;; if the given node is selected, delete its all sub nodes, return new one
;; else return the old one
;; example: 
;; (update-node-for-deletion (make-node empty true (make-posn 10 30)))
;;          = (make-node empty true (make-posn 10 30))
;; strategy : structural decomposition on node [Node]
(define (update-node-for-deletion node)
      (make-node
       (delete-node-helper (node-lon node))
       (node-selected? node)
       (node-center node)))
        
;; tree-to-root-node : World Tree -> Node
;; returns the node
;; Examples:(tree-to-root-node 1 SECOND-TREE)=SECOND-TREE
;; strategy : domain knowledge
(define (tree-to-root-node world tree)
    tree)

(define-test-suite tree-to-root-node-tests
  (check-equal?
     (tree-to-root-node 1 SECOND-TREE)
     SECOND-TREE
     "return tree"))
(run-tests tree-to-root-node-tests)
(define-test-suite world-after-mouse-event-tests
  ;; test mouse down event
  (check-equal?
     (world-after-mouse-event INITIAL-TEST 105 155 "button-down")
     MOUSE-DOWN-ON-LEVEL-0-ROOT
     "mouse down inside root node of all node")
  (check-equal?
     (world-after-mouse-event INITIAL-TEST 110 160 "button-down")
     MOUSE-DOWN-ON-LEVEL-0-ROOT
     "mouse down on boundry root node of all node")
  (check-equal?
     (world-after-mouse-event INITIAL-TEST 70 75 "button-down")
     MOUSE-DOWN-ON-LEVEL-1-NOT-ROOT
     "mouse down inside non-root node of level 1")
  (check-equal?
     (world-after-mouse-event INITIAL-TEST 300 110 "button-down")
     MOUSE-DOWN-ON-LEVEL-1-NOT-ROOT-BOUNDARY
     "mouse down on boundry of ball and canvas")
  (check-equal?
     (world-after-mouse-event INITIAL-TEST 125 205 "button-down")
     MOUSE-DOWN-ON-LEVEL-1-ROOT
     "mouse down inside root node of level 1")
  (check-equal?
     (world-after-mouse-event INITIAL-TEST 130 200 "button-down")
     MOUSE-DOWN-ON-LEVEL-1-ROOT
     "mouse down on boundary of root node of level 1")
  (check-equal?
     (world-after-mouse-event INITIAL-TEST 275 245 "button-down")
     MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT
     "mouse down inside non-root node of level 2")
  (check-equal?
     (world-after-mouse-event INITIAL-TEST 280 250 "button-down")
     MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT
     "mouse down on boundary of non-root node of level 2")
  (check-equal?
     (world-after-mouse-event INITIAL-TEST 290 290 "button-down")
     INITIAL-TEST
     "mouse down outside of all balls")
  
  ;;MOUSE DOWN ON overlap nodes
   (check-equal?
     (world-after-mouse-event LEVEL-2-NOT-ROOT-INSIDE-FOR-OVERLAP-NOT-SELECTED
                              205 300 "button-down")
     MOUSE-DOWN-LEVEL-2-NOT-ROOT-INSIDE-FOR-OVERLAP
     "mouse down in side of two balls") 
;; drag-overlap nodes
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-LEVEL-2-NOT-ROOT-INSIDE-FOR-OVERLAP 
                              180 280 "drag")
     DRAG-LEVEL-2-NOT-ROOT-INSIDE-FOR-OVERLAP
     "drag two nodes together(one is leaf one is root node)")
  ;; separate two overlap nodes
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-LEVEL-1-ROOT-INSIDE-FOR-OVERLAP
                              140 220 "drag")
     SEPARATE-LEVEL-2-NOT-ROOT-INSIDE
     "drag level 0 root inside canvas (can add ball)")
  
  ;; test drag event
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-0-ROOT 100 260 "drag")
     DRAG-LEVEL-0-ROOT-INSIDE
     "drag level 0 root inside canvas (can add ball)")
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-0-ROOT 100 360 "drag")
     DRAG-LEVEL-0-ROOT-OUTSIDE
     "drag level 0 root inside canvas, (cannot add ball); 
                        because new ball below canvas bottom")
  
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-1-NOT-ROOT 45 100 "drag")
     DRAG-LEVEL-1-NOT-ROOT-INSIDE
     "drag level 1 NON-root inside canvas, (can add ball)")
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-1-NOT-ROOT 8 100 "drag")
     DRAG-LEVEL-1-NOT-ROOT-OUTSIDE
     "drag level 1 NON-root inside canvas. canNOT add ball
              because ball out of left canvas boundary")
  
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-1-ROOT 60 100 "drag")
     DRAG-LEVEL-1-ROOT-INSIDE
     "drag level 1 root inside canvas, (can add ball)")
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-1-ROOT 285 100 "drag")
     DRAG-LEVEL-1-ROOT-OUTSIDE
     "drag level 1 root inside canvas, canNOT add ball
           because right edge outside canvas(left edge inside)")
  
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT 200 200 "drag")
     DRAG-LEVEL-2-NOT-ROOT-INSIDE
     "drag level 2 non root inside canvas, (can add ball)")
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT 20 380 "drag")
     DRAG-LEVEL-2-NOT-ROOT-OUTSIDE
     "drag level 1 root inside canvas, (canNOT add ball
             because the ball total outside canvas")  
  
  ;; Mouse up on the given world
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-0-ROOT 60 100 "button-up")
     INITIAL-TEST
     "mouse up for level 0 root")
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-1-ROOT 290 100 "button-up")
     INITIAL-TEST
     "mouse up for level 1 root")
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-1-NOT-ROOT 200 200 
                              "button-up")
     INITIAL-TEST
     "mouse up for level 1 non root")
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT 20 20 "button-up")
     INITIAL-TEST
     "mouse up for level 2 non root")
  
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-1-NOT-ROOT 200 200 "enter")
     MOUSE-DOWN-ON-LEVEL-1-NOT-ROOT
     "test for other mouse events")
  (check-equal?
     (world-after-mouse-event MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT 20 20 "leave")
     MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT
    "test for other mouse events")
  )
(run-tests world-after-mouse-event-tests)
(define-test-suite world-after-key-event-tests
  ;; add node test for root
  (check-equal?
     (world-after-key-event MOUSE-DOWN-ON-LEVEL-0-ROOT "n")
     MOUSE-DOWN-ON-LEVEL-0-ROOT-ADD-1
     "add ball for a node that can add ball")
  (check-equal?
     (world-after-key-event MOUSE-DOWN-ON-LEVEL-0-ROOT-ADD-1 "n")
     MOUSE-DOWN-ON-LEVEL-0-ROOT-ADD-2
     "add two balls in a row")
  (check-equal?
     (world-after-key-event MOUSE-DOWN-ON-LEVEL-0-ROOT-ADD-2 "n")
     MOUSE-DOWN-ON-LEVEL-0-ROOT-ADD-2
     "cannot add more balls")
  
  ;; add node for level 1 non-root node
  ;; add two balls in a row, then not able to another ball
  (check-equal?
     (world-after-key-event DRAG-LEVEL-1-NOT-ROOT-INSIDE "n")
     DRAG-LEVEL-1-NOT-ROOT-INSIDE-ADD-1
     "add ball for level 1 non-root")
  (check-equal?
     (world-after-key-event DRAG-LEVEL-1-NOT-ROOT-INSIDE-ADD-1 "n")
     DRAG-LEVEL-1-NOT-ROOT-INSIDE-ADD-2
     "add ball for level 1 non-root twice")
  (check-equal?
     (world-after-key-event DRAG-LEVEL-1-NOT-ROOT-INSIDE-ADD-2 "n")
     DRAG-LEVEL-1-NOT-ROOT-INSIDE-ADD-2
     "cannot add ball for level 1 non-root 
                  (no more space when having more than one sub-node)")
  (check-equal?
     (world-after-key-event DRAG-LEVEL-1-NOT-ROOT-OUTSIDE "n")
     DRAG-LEVEL-1-NOT-ROOT-OUTSIDE
     "cannot add ball for level 1 non-root (no space for first sub-node)")

  ;; add node for level 1 root node
  (check-equal?
     (world-after-key-event DRAG-LEVEL-1-ROOT-INSIDE "n")
     DRAG-LEVEL-1-ROOT-INSIDE-ADD-1
     "add ball for level 1 root [normal situation]")
  (check-equal?
     (world-after-key-event DRAG-LEVEL-1-ROOT-OUTSIDE "n")
     DRAG-LEVEL-1-ROOT-OUTSIDE
     "CANNOT add ball because new right edge of new ball
              outside of canvas")

  ;; add node for level 2 non root node
  (check-equal?
     (world-after-key-event DRAG-LEVEL-2-NOT-ROOT-INSIDE "n")
     DRAG-LEVEL-2-NOT-ROOT-INSIDE-ADD-1
     "add ball for level 2 non root [normal situation]")
  (check-equal?
     (world-after-key-event DRAG-LEVEL-2-NOT-ROOT-OUTSIDE "n")
     DRAG-LEVEL-2-NOT-ROOT-OUTSIDE
     "CANNOT add ball because new bottom edge of new ball
              outside of canvas")

  (check-equal?
     (world-after-key-event INITIAL-TEST "n")
     INITIAL-TEST
     "add ball when nothing selected")  
  
;; tests for node deletion
  ;; test delete level-2 non-root node
  (check-equal?
     (world-after-key-event MOUSE-DOWN-ON-LEVEL-2-NOT-ROOT "d")
     DELETE-LEVEL-2-NOT-ROOT
     "delete level-2 non root node")
  ;; test delete level-1 root node
  (check-equal?
     (world-after-key-event MOUSE-DOWN-ON-LEVEL-1-ROOT "d")
     DELETE-LEVEL-1-ROOT
     "delete level-1 root node")
  ;; test delete level-1 non-root node
  (check-equal?
     (world-after-key-event MOUSE-DOWN-ON-LEVEL-1-NOT-ROOT "d")
     DELETE-LEVEL-1-NOT-ROOT
     "delete level-1 non root node")
  ;; test delete level-0 root node
  (check-equal?
     (world-after-key-event MOUSE-DOWN-ON-LEVEL-0-ROOT "d")
     DELETE-LEVEL-0-ROOT
     "delete level-0 root node")
  ;; test delete the second tree
  (check-equal?
     (world-after-key-event DELETE-LEVEL-0-ROOT-SELECTED "d")
     EMPTY-WORLD
     "delete everything")
  ;; test delete when nothing selected
  (check-equal?
     (world-after-key-event INITIAL-TEST "d")
     INITIAL-TEST
     "delete when nothing selected")  
  
  ;; tests for adding new tree
  (check-equal?
     (world-after-key-event EMPTY-WORLD "t")
     ONE-TREE-WORLD
     "from empty world to world with one tree initialized")
  (check-equal?
     (world-after-key-event ONE-TREE-WORLD "t")
     TWO-TREE-WORLD
     "from one tree to two trees with newly added tree initialized")
  (check-equal?
     (world-after-key-event INITIAL-TEST "t")
     INITIAL-TEST-ADD-NEW-TREE
     "from TWO tree to three trees with newly added tree initialized")
  (check-equal?
     (world-after-key-event INITIAL-TEST-ADD-NEW-TREE "t")
     INITIAL-TEST-ADD-NEW-TREE-TWICE
     "click 't' twice in a row -> generate two idential trees")
  
  (check-equal?
     (world-after-key-event INITIAL-TEST-ADD-NEW-TREE "left")
     INITIAL-TEST-ADD-NEW-TREE
     "test other keys")
  (check-equal?
     (world-after-key-event INITIAL-TEST-ADD-NEW-TREE "w")
     INITIAL-TEST-ADD-NEW-TREE
     "test other keys")
  )
(run-tests world-after-key-event-tests)
(define-test-suite render-image-tests
  (check-equal?
     (render TEST-IMAGE-WORLD1)
     RESULT-IMAGE1
     "one-node selected and no more space, thus show red node")
  
  (check-equal?
     (render TEST-IMAGE-WORLD2)
     RESULT-IMAGE2
     "nothing selected")
  
  (check-equal?
     (render TEST-IMAGE-WORLD3)
     RESULT-IMAGE3
     "one-node selected and has more space, thus show green node")
  )
(run-tests render-image-tests)
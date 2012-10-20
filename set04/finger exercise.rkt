;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |finger exercise|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

(define-struct quadtree (data nw ne sw se))
; A Quadtree is either
;--empty
;--(make-quadtree Number Quadtree Quadtree Quadtree Quadtree)

;(define (fn qt)
;    (cond
;        [(empty? qt) ...]
;        [else (... 
;              (quadtree-data qt)
;              (fn (quadtree-nw qt))
;              (fn (quadtree-ne qt))
;              (fn (quadtree-sw qt))
;              (fn (quadtree-se qt)))]))
;; data: number that represents the value 
;; nw ne sw se represent q-trees in the four quadrants


(define (sum-of-quadtree qt)
    (cond
        [(empty? qt) 0]
        [else (+ 
              (quadtree-data qt)
              (sum-of-quadtree (quadtree-nw qt))
              (sum-of-quadtree (quadtree-ne qt))
              (sum-of-quadtree (quadtree-sw qt))
              (sum-of-quadtree (quadtree-se qt)))]))

(define q1 (make-quadtree 1 empty empty empty empty))
(define q2 (make-quadtree 2 empty empty empty empty))
(define q3 (make-quadtree 3 empty empty empty empty))
(define q4 (make-quadtree 4 empty empty empty empty))
(define q5 (make-quadtree 5 q1 q2 q3 q4))

(sum-of-quadtree q5)

(define (max-of-quadtree qt)
    (cond
        [(empty? qt) 0]
        [else (max 
              (quadtree-data qt)
              (max-of-quadtree (quadtree-nw qt))
              (max-of-quadtree (quadtree-ne qt))
              (max-of-quadtree (quadtree-sw qt))
              (max-of-quadtree (quadtree-se qt)))]))

(max-of-quadtree q5)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |week1-finger exercise|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;exercise 59 time-structure
;compound data type
;;DATA DEFINITION
(define-struct point (hours minutes seconds))
;;Interp:
;;A Point in time is a (make-time number number number)
;;it consists of hour, minute and second;
;;it represents time since mid-night
;;Interp, each number (hour, minute and second) represent 
;;hours, minutes and seconds of a given point in time.
;;(make-time 2 3 4) represent 2:03:04 am in the morning
;;(make-time 13 33 45) represent 1:33:45 pm in the afternoon
;;TEMPLATE
;;point-fn : Point ->Number
;(define (point-fn p)
;   ...
;  (point-hours t)
;  (point-minutes t)
;  (point-seconds t)
;  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;exercise 62
;;DATA DEFINITION
(define-struct movie (title producer year))
;;Interp:
;;A movie contains many information
;;title refers to the movie's name
;;producer refers to the movie maker
;;year means the year movie was made
;;movie-fn: Movie -> ?
;(define (movie-fn m)
;  (...
;   (movie-title m)
;   (movie-producer m)
;   (movie-year m)))

(define-struct boyfriend (name hair eyes phone))
;;Interp:
;;there are several aspects can decribe a bf
;;including name, hair style, eye and phone
;;bf-fn: Boyfriend -> ??
;(define (bf-fn b)
;  (...
;   (boyfriend-name b)
;   (boyfriend-hair b)
;   (boyfriend-eyes b)
;   (boyfriend-phone b)))

(define-struct cheerleader (name number))
;;Interp:
;;there are several aspects can decribe 
;;a cheerleader including name and number
;;cl-fn: cheerleader -> ??
;(define (cl-fn c)
;  (...
;   (cheerleader-name c)
;   (cheerleader-number c)))

(define-struct CD (artist title price))
;;Interp:
;;A CD has several info, including:
;;artist who made it, title and price
;;cl-fn: cheerleader -> ??
;cd-fn CD -> ?
;(define (cd-fn cd)
;  (...
;   (cd-artist cd)
;   (cd-title cd)
;   (cd-price cd)))

(define-struct sweater (material size producer))
;;Interp:
;;There are several aspects to decribe a sweater 
;;includes material size and producer
;sw: Sweater -> ?
;(define (sw s)
;  (...
;   (sweater-material s)
;   (sweater-sizw s)
;   (sweater-producer s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;exercise 63
;time->seconds: Point ->Number
;produce the number seconds between
;mid-night and the given point in time
;examples: 
;for time (make-time 1 2 3), which is 1:02:03 am;
;result is 1*3600+2*60+3=3723 seconds.
;for time (make-time 13 33 45), which is 1:33:45 pm;
;result is 13*3600+33*60+45=48825 seconds.
(define (time->seconds t)
  (+ 
   (* (point-hours t) 3600)
   (* (point-minutes t) 60)
   (point-seconds t))) 
(check-expect (time->seconds (make-point 1 2 3)) 3723)
(check-expect (time->seconds (make-point 13 33 45)) 48825)
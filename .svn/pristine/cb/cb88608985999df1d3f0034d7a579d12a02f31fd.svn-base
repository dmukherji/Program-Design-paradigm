;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
;; Write your solution for the first problem of the first problem set in this file
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")
(require 2htdp/image) 
(provide make-editor)
(provide editor-pre)
(provide editor-post)
(provide editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DATA DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA DEFINITION
(define-struct editor (pre post))
;;A Editor is a (make-editor String String)
;;Interp:
;;pre is part of the string that before the cursor
;;post is part of the string that after the cursor

;;TEMPLATE
;;editor-fn : Editor -> ??
;(define (editor-fn e)
;  (...
;   (editor-pre e)
;   (editor-post e)))

;;A KeyEvent is one of :
;;"\b"
;;"\t" or "\u007f"
;;"left"
;;"right"
;;keys whose length is one except the first three cases
;;all other keys

;;Interp:
;;"\b" means backspace
;;"\t" or "\u007f" should be ignored
;;"left" let the cursor goes left
;;"right" let the cursor goes right
;;keys whose length is one will be append to the pre
;;all other keys will be ignored
;;TEMPLATE
;;key-fn : KeyEvent -> ?
;(define (key-fn key)
;   (cond 
;      [(string=? k "\b") ...]
;      [(or (string=? k "\t") (string=? k "\u007f")) ...]
;      [(string=? k "left") ...]
;      [(string=? k "right") ...]
;      [(= (string-length k) 1) ...]
;      [else ...]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;main function;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;edit: Editor KeyEvent-> Editor
;;produce a new editor based on the input key
;;(edit (make-editor "abc" "def") "left")= (make-editor "ab" "cdef")
;;(edit (make-editor "abc" "def") "right")= (make-editor "abcd" "ef")
;;(edit (make-editor "abc" "def") "g")= (make-editor "abcg" "def")
;;STRATEGY : function composition
(define (edit e k)
  (cond
    [(string=? k "\b") (deleteOneChar (editor-pre e) (editor-post e))]
    [(or (string=? k "\t") (string=? k "\u007f")) e]
    [(string=? k "left") (moveCursorLeft (editor-pre e) (editor-post e))]
    [(string=? k "right") (moveCursorRight (editor-pre e) (editor-post e))]
    [(= (string-length k) 1) (addOneChar (editor-pre e) (editor-post e) k)]
    [else e]))
;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Help functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;deleteOneChar : String String->Editor
;;delete the last char of first string
;;then, make a new editor
;;(deleteOneChar "asd" "fgh")=(make-editor "as" "fgh")
;;STRATEGY : Domain Knowledge
(define (deleteOneChar f s)
  (make-editor (string-remove-last f) s))

;;addOneChar : String String String->Editor
;;add the third string to the end of first string
;;then, make a new editor based on first two strings
;;(addOneChar "asd" "fgh" "q")=(make-editor "asdq" "fgh")
;;STRATEGY : Domain Knowledge
(define (addOneChar f s k)
  (make-editor (string-append f k) s))

;;moveCursorLeft : String String->Editor
;;move the last char of the first string 
;;to the beginning of the second string
;;and form a new editor
;;(moveCursorLeft "asd" "fgh")=(make-editor "as" "dfgh")
;;STRATEGY : Domain Knowledge
(define (moveCursorLeft f s)
      (make-editor (string-remove-last f) 
                   (string-append (string-last f) s)))

;;moveCursorRight : String String->Editor
;;move the beginning char of the second string 
;;to the end of the first string
;;and form a new editor
;;(moveCursorLeft "asd" "fgh")=(make-editor "asdf" "gh")
;;STRATEGY : Domain Knowledge
(define (moveCursorRight f s)
  (make-editor (string-append f (string-first s))
               (string-rest s)))

;;string-remove-last : String->String
;;delete the last char of the given string
;;(string-remove-last "asd")="as"
;;(string-remove-last "")=""
;;STRATEGY : Domain Knowledge
(define (string-remove-last str)
 (if (= (string-length str) 0) 
     str 
     (substring str 0 (- (string-length str) 1))))

;;string-last: String-> String
;;return the last char of the given string
;;(string-last "asd")="d"
;;(string-last "")=""
;;STRATEGY : Domain Knowledge
(define (string-last str)
 (if (= (string-length str) 0) 
    str 
    (substring str (- (string-length str) 1) (string-length str))))

;;string-rest string->string
;;return the rest of the given string except the first char
;;(string-rest "asd")="sd"
;;(string-rest "")=""
;;STRATEGY : Domain Knowledge
(define (string-rest str)
 (if (= (string-length str) 0) 
    str 
    (substring str 1 (string-length str))))  

;;string-first string->string
;;return the first char of the give string
;;(string-first "asd")="a"
;;(string-first "")=""
;;STRATEGY :Domain Knowledge
(define (string-first str)
 (if (= (string-length str) 0) 
     str 
     (substring str 0 1)))

;;;render: Editor->Image
;;;generate text image from e. The text is generated
;;;from pre and post part of e. The cursor will be 
;;;added between pre and post
;;;Example:
;;;STRATEGY :Domain Knowledge
;(define (render e)
;  (overlay/align "left" "middle"
;   (beside (build-text (editor-pre e))
;          cursor
;          (build-text (editor-post e)))
;   background)
;)

;;;buile-text: String->Image
;;;this is a help function which generate 
;;;an image based on given text
;;;STRATEGY: domain knowledge
;(define (build-text str)
;  (text str 11 "black") )
;;;main: Editor -> Image
;;;use the given editor to present a
;;;image which acts like a text editor
;(define (main e)
;  (big-bang e
;   (on-key edit)
;   (to-draw render)))

;;(main (make-editor "asd" "fgh"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Test Units;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Test cases design
;;four kinds of editor (pre/post is null or not null)
;;six kinds of keyEvent
;;=24 combinations of tests
(define-test-suite edit-test
  (check-equal?
   (edit (make-editor "asd" "fgh") "left")
   (make-editor "as" "dfgh")
   "test move cursor to left")
  (check-equal?
   (edit (make-editor "asd" "fgh") "right")
   (make-editor "asdf" "gh")
   "test move cursor to right")
  (check-equal?
   (edit (make-editor "asd" "fgh") "\b")
   (make-editor "as" "fgh")
   "delete one char")  
  (check-equal?
   (edit (make-editor "asd" "fgh") "e")
   (make-editor "asde" "fgh")
   "add 'e' ")
  (check-equal?
   (edit (make-editor "asd" "fgh") "up")
   (make-editor "asd" "fgh")
   "test tab")
  (check-equal?
   (edit (make-editor "asd" "fgh") "\u007F")
   (make-editor "asd" "fgh")
   "test robust")  
  
  
  (check-equal?
   (edit (make-editor "" "fgh") "left")
   (make-editor "" "fgh")
   "test move cursor to left (with null string)")
  (check-equal?
   (edit (make-editor "" "fgh") "\b")
   (make-editor "" "fgh")
   "delete one char (with nothing left)")  
  (check-equal?
   (edit (make-editor "" "fgh") "e")
   (make-editor "e" "fgh")
   "add 'e' (empty pre)")  
  (check-equal?
   (edit (make-editor "" "fgh") "right")
   (make-editor "f" "gh")
   "test move cursor to right")
  (check-equal?
   (edit (make-editor "" "fgh") "\t")
   (make-editor "" "fgh")
   "ingore")  
  (check-equal?
   (edit (make-editor "" "fgh") "up")
   (make-editor "" "fgh")
   "add 'e' (empty pre)")   
  
  (check-equal?
   (edit (make-editor "" "") "left")
   (make-editor "" "")
   "test move cursor to left (without input)")
  (check-equal?
   (edit (make-editor "" "") "right")
   (make-editor "" "")
   "test move cursor to right (without input)")  
  (check-equal?
   (edit (make-editor "" "") "e")
   (make-editor "e" "")
   "add 'e' (empty input)")  
  (check-equal?
   (edit (make-editor "" "") "down")
   (make-editor "" "")
   "long nput")
  (check-equal?
   (edit (make-editor "" "") "\b")
   (make-editor "" "")
   "delete")  
  (check-equal?
   (edit (make-editor "" "") "\t")
   (make-editor "" "")
   "ignore")   

  (check-equal?
   (edit (make-editor "asd" "") "right")
   (make-editor "asd" "")
   "test move cursor to right (with null string)")
  (check-equal?
   (edit (make-editor "asd" "") "left")
   (make-editor "as" "d")
   "add 'e'(empty post)")
  (check-equal?
   (edit (make-editor "asd" "") "e")
   (make-editor "asde" "")
   "add 'e'(empty post)")
  (check-equal?
   (edit (make-editor "asd" "") "\b")
   (make-editor "as" "")
   "delete")
  (check-equal?
   (edit (make-editor "asd" "") "\t")
   (make-editor "asd" "")
   "ignore")
  (check-equal?
   (edit (make-editor "asd" "") "up")
   (make-editor "asd" "")
   "long input"))

(run-tests edit-test)
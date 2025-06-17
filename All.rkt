;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname All) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Datendefinitionen

(define-struct vector (phi len))

; Konstantendefinitionen

; Helper funktionen Definitionen

; List-of-X -> X
; returns the last element of a list
(check-expect (last (list 1 2 3 4))4)
(check-expect (last (list "Das" "ist" "ein" "Test"))"Test")
(check-error (last empty))
(define (last lst)
  (cond
    [(empty? lst)(error 'last "List empty!")]
    [(empty? (rest lst))(first lst)]
    [else (last (rest lst))]))

; List-of-X -> List-of-X
; returns List without the last element
(check-expect (start (list 1 2 3 4))(list 1 2 3))
(check-expect (start (list "Test" "Case"))(list "Test"))
(check-error (start empty))

(define (start lst)
  (cond
    [(empty? lst) (error start "List empty!")]
    [(empty? (rest lst)) empty]
    [else (cons (first lst) (start (rest lst)))]))

; Number Number -> posn
; returns the coordinates in cartesian form for a Radiant phi and a length l 
(check-within (posn-x (polar->cartesian (/ pi 2) 2)) 0.0 1e-6)
(check-within (posn-y (polar->cartesian (/ pi 2) 2)) 2.0 1e-6)
(check-within (posn-x (polar->cartesian 0 0)) 0.0 1e-6)
(check-within (posn-y (polar->cartesian 0 0)) 0.0 1e-6)

(define (polar->cartesian phi l)
  (make-posn (* l (cos phi)) (* l (sin phi))))

; posn vector color image -> iamge
; Draws a line with length and direction given in vector in the color of color
; at the coordintes of posn into scene and returns scene

(define (put-branch pos vec color scene)
  (


; Helperfunktionen für big-bang-handler

; Aufruf big-bang-funktion

; weitere Ausdrücke
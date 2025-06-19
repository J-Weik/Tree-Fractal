;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname All) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Datendefinitionen

(define-struct vector (phi len))

; Konstantendefinitionen

(define BLOSSOM-SIZE 10)
(define BLOSSOM-TYPE "outline")
(define TREE-CANVAS-SIZE-X 500)
(define TREE-CANVAS-SIZE-Y 500)
(define TRANSPARENT (make-color 0 0 0 0))
(define TEST-COLLIST '("red" "orange" "yellow" "green" "blue" "purple" "black"))
(define TEST-LONGLIST '("black" "black" "black" "black" "black" "black" "black" "black" "black" "black" "black" "black" "black" "black"))

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
   (add-line scene
    (posn-x pos)(posn-y pos)
             (+ (posn-x pos) (posn-x (polar->cartesian (vector-phi vec) (vector-len vec))))
             (+ (posn-y pos) (posn-y (polar->cartesian (vector-phi vec) (vector-len vec))))
             color))



 
; posn color image -> image
; draws a circle in the color of color at position posn in the image and returns the image

(define (put-blossom pos color scene)
  (put-image
   (circle BLOSSOM-SIZE BLOSSOM-TYPE color)
   (posn-x pos)
   (posn-y pos)
   scene))

; posn vector Number Number Number List-of-colors -> image
; draws 2 branches from the given start position with lenght and direction given in the vector,
; 

(define (tree startpos vec verzweigungInRad growthRatio colorList)
  (cond
      [(empty? (rest colorList)) (empty-scene TREE-CANVAS-SIZE-X TREE-CANVAS-SIZE-Y TRANSPARENT)]
    [else 
           (put-branch startpos vec (first colorList)
           (local [
                   (define NEW-STARTPOS (make-posn (+ (posn-x startpos) (posn-x (polar->cartesian (vector-phi vec)(vector-len vec))))
                                                    (+ (posn-y startpos) (posn-y (polar->cartesian (vector-phi vec)(vector-len vec))))))
                   (define NEW-VEC-L (make-vector (+ (vector-phi vec) verzweigungInRad )(* growthRatio (vector-len vec))))
                   (define NEW-VEC-R (make-vector (- (vector-phi vec) verzweigungInRad )(* growthRatio (vector-len vec))))]

                   (put-image (tree
                         NEW-STARTPOS
                         NEW-VEC-L
                         verzweigungInRad
                         growthRatio
                         (rest colorList)) (/ TREE-CANVAS-SIZE-X 2) (/ TREE-CANVAS-SIZE-Y 2)
                   (tree
                          NEW-STARTPOS
                          NEW-VEC-R
                          verzweigungInRad
                          growthRatio
                          (rest colorList)
                         ))))]))

(tree (make-posn 250 500) (make-vector (/ pi 2) -150) (/ pi 3) 0.66 TEST-COLLIST)
  (tree (make-posn 250 500) (make-vector (/ pi 2) -130) (/ pi 3) 0.66 TEST-LONGLIST)


;(put-blossom (make-posn 43 340) "green"
; (put-blossom (make-posn 100 439) "green"
;  (put-blossom (make-posn 250 400) "green"
;   (put-branch (make-posn 0 0) (make-vector (/ pi 2) 100) "green"
;    (put-branch (make-posn 250 250) (make-vector (/ pi 4) 150) "green"
;     (put-branch (make-posn 250 250) (make-vector (* pi (/ 3 2)) 200) "green"
;      (put-image (circle BLOSSOM-SIZE BLOSSOM-TYPE "red") 43 340
;       (put-image (circle BLOSSOM-SIZE BLOSSOM-TYPE "red") 100 439
;        (put-image (circle BLOSSOM-SIZE BLOSSOM-TYPE "red") 250 400
;         (add-line (add-line (add-line (empty-scene 500 500)1 1 1 1 "red")250 250 325 325 "red") 250 250 250 50 "red"))))))))))



; Helperfunktionen für big-bang-handler

; Aufruf big-bang-funktion

; Weitere Ausdrücke
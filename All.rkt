;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname All) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Datendefinitionen

(define-struct vector (phi len))
(define-struct WorldState (verzweigung growth colorList))

; Konstantendefinitionen

(define BLOSSOM-SIZE 5)
(define BLOSSOM-TYPE "outline")
(define TREE-CANVAS-SIZE-X 1000)
(define TREE-CANVAS-SIZE-Y 1000)
(define TRANSPARENT (make-color 0 0 0 0))
(define DEFAULT_WORLD_STATE (make-WorldState
                             (/ pi 3)
                             0.66
                             `(
                               ,(make-color 255 0 0)
                               ,(make-color 255 128 0)
                               ,(make-color 128 255 0)
                               ,(make-color 0 255 128)
                               ,(make-color 0 255 255)
                               ,(make-color 0 128 255)
                               ,(make-color 64 0 255)
                               ,(make-color 255 0 255)
                               ,(make-color 255 0 64)
                               )))

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
  (place-image
   (circle BLOSSOM-SIZE BLOSSOM-TYPE color)
   (posn-x pos)
   (posn-y pos)
   scene))

; posn vector Number Number Number List-of-colors -> image
; draws 2 branches from the given start position with lenght and direction given in the vector,
; adapts the direction of the next branch depending on verZweigungInRad. Next branch is always
; growthRatio * length of vector long. does this for as many colors as there are in the given colorList.
; Returns the drawn Fractal Tree

(define (tree startpos vec verzweigungInRad growthRatio colorList)
  (cond
      [(empty? (rest colorList)) (put-blossom startpos (first colorList) (empty-scene TREE-CANVAS-SIZE-X TREE-CANVAS-SIZE-Y TRANSPARENT))]
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

; world-state -> image
; renders the fractal tree from the current world state
(define (render world)
  (tree
   (make-posn (/ TREE-CANVAS-SIZE-X 2) (/ TREE-CANVAS-SIZE-Y 1.5))
   (make-vector
    (/ pi 2)
    -150)
   (WorldState-verzweigung world)
   (WorldState-growth world)
   (WorldState-colorList world)
   ))

; input -> WorldState
; takes input from keyboard and alters the worldState given from the input

(define (change world key)
  (cond
    [(key=? key "up") (make-WorldState
                       (+ (WorldState-verzweigung world) 0.1)
                       (WorldState-growth world)
                       (WorldState-colorList world))]
    [(key=? key "down") (make-WorldState
                         (- (WorldState-verzweigung world) 0.1)
                         (WorldState-growth world)
                         (WorldState-colorList world))]                      
    [(key=? key "left") (make-WorldState
                         (WorldState-verzweigung world)
                         (+ (WorldState-growth world) 0.1)
                         (WorldState-colorList world))]
    [(key=? key "right") (make-WorldState
                         (WorldState-verzweigung world)
                         (- (WorldState-growth world) 0.1)
                         (WorldState-colorList world))]
    [(key=? key "+") (make-WorldState
                        (WorldState-verzweigung world)
                        (WorldState-growth world)
                        (cons (last (WorldState-colorList world)) (start (WorldState-colorList world))))]
    [(key=? key "-") (make-WorldState
                        (WorldState-verzweigung world)
                        (WorldState-growth world)
                        (cons (last (WorldState-colorList world)) (start (WorldState-colorList world))))]
    [(key=? key " ") ()

  

; Aufruf big-bang-funktion

; WorldState input -> image
; takes a world-state and gives an image of a fractal Tree.
; worldState gets altered by user input
(big-bang DEFAULT_WORLD_STATE
  (on-key change)
  (to-draw render)
  )

; Weitere Ausdrücke
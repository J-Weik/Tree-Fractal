;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname All) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; AUFGABE i) / ERWEITERUNG :
;; mit w lässt sich die Menge an branches die pro branch gemalt werden +1 setzen
;; mit s lässt sich die Menge an branches die pro branch gemalt werden -1 setzen
;; mit q lässt sich die vorderste Farbe der FarbListe entfernen um das rendern zu beschleunigen
;; mit t lässt sich das rendern der Blossoms toggeln (um Bäume mit mehr als 2 branches besser zu sehen)
;; HINWEIS: Zugewiesener RAM für DrRacket muss möglicherweise hochgestellt werden


; Datendefinitionen

(define-struct vector (phi len))
(define-struct WorldState (verzweigung growth colorList branches drawBlossoms))

; Konstantendefinitionen

(define BLOSSOM-SIZE 5)
(define BLOSSOM-TYPE "solid")
(define TREE-CANVAS-SIZE-X 1000)
(define TREE-CANVAS-SIZE-Y 1000)
(define FIRST-BRANCH-LENGTH -150)
(define FIRST-BRANCH-RAD (/ pi 2))
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
                               )
                             2
                             true))

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

; Tests sind unter funktion put-blossom da sich racket sonst beschwert
(define (put-branch pos vec color scene)
   (add-line scene
    (posn-x pos)(posn-y pos)
             (+ (posn-x pos) (posn-x (polar->cartesian (vector-phi vec) (vector-len vec))))
             (+ (posn-y pos) (posn-y (polar->cartesian (vector-phi vec) (vector-len vec))))
             color))

; posn color image -> image
; draws a circle in the color of color at position posn in the image and returns the image

; Tests sind unter funktion da sich racket sonst beschwert
(define (put-blossom pos color scene)
  (place-image
   (circle BLOSSOM-SIZE BLOSSOM-TYPE color)
   (posn-x pos)
   (posn-y pos)
   scene))

; Test für put-blossom und put-branch

(put-blossom (make-posn 43 340) "green"
 (put-blossom (make-posn 100 439) "green"
  (put-blossom (make-posn 250 400) "green"
   (put-branch (make-posn 0 0) (make-vector (/ pi 3) 100) "green"
    (put-branch (make-posn 250 250) (make-vector (/ pi 4) 106) "green"
     (put-branch (make-posn 250 250) (make-vector (* 1.5 pi) 200) "green"
      (place-image (circle BLOSSOM-SIZE BLOSSOM-TYPE "red") 43 340
       (place-image (circle BLOSSOM-SIZE BLOSSOM-TYPE "red") 100 439
        (place-image (circle BLOSSOM-SIZE BLOSSOM-TYPE "red") 250 400
         (add-line
          (add-line
           (add-line (empty-scene 500 500)
                     0 0
                     (posn-x (polar->cartesian (/ pi 3) 100))
                     (posn-y (polar->cartesian (/ pi 3) 100))
                     "red")
           250 250
           (+ 250 (posn-x (polar->cartesian (/ pi 4) 106)))
           (+ 250 (posn-y (polar->cartesian (/ pi 4) 106)))
           "red")
          250 250
          (+ 250 (posn-x (polar->cartesian (* 1.5 pi) 200)))
          (+ 250 (posn-y (polar->cartesian (* 1.5 pi) 200)))
          "red"))))))))))

; posn vector Number Number Number List-of-colors Boolean -> image
; draws 2 branches from the given start position with lenght and direction given in the vector,
; adapts the direction of the next branch depending on verZweigungInRad. Next branch is always
; growthRatio * length of vector long. does this for as many colors as there are in the given colorList.
; Returns the drawn Fractal Tree draws given amount of branches per branch and if drawBlossoms? is true draw Blossoms at the last branch

(define (tree startpos vec verzweigungInRad growthRatio colorList amountBranches drawBlossoms?)
  (cond
    [(empty? (rest colorList))
     (cond
       [drawBlossoms? (put-blossom startpos (first colorList)
                  (empty-scene TREE-CANVAS-SIZE-X TREE-CANVAS-SIZE-Y TRANSPARENT))]
       [else (empty-scene TREE-CANVAS-SIZE-X TREE-CANVAS-SIZE-Y TRANSPARENT)])]
    [else
     (put-branch
      startpos vec (first colorList)
      (local
        [(define new-startpos
           (make-posn
            (+ (posn-x startpos) (posn-x (polar->cartesian (vector-phi vec) (vector-len vec))))
            (+ (posn-y startpos) (posn-y (polar->cartesian (vector-phi vec) (vector-len vec))))))
         (define (create-angle-list amount verzweigungInRad)
           (local [
                   (define (helper i)
                     (cond
                       [(= i amount) empty]
                       [else
                        (cons
                         (+ (- verzweigungInRad)
                            (* i (/ (* 2 verzweigungInRad) (max 1 (- amount 1)))))
                         (helper (+ i 1)))]))]
             (helper 0)))

         (define angle-list (create-angle-list amountBranches verzweigungInRad))
         (define (draw-branches angles)
           (cond
             [(empty? angles)
              (empty-scene TREE-CANVAS-SIZE-X TREE-CANVAS-SIZE-Y TRANSPARENT)]
             [else
              (overlay
               (tree new-startpos
                     (make-vector (+ (vector-phi vec) (first angles)) (* growthRatio (vector-len vec)))
                     verzweigungInRad
                     growthRatio
                     (rest colorList)
                     amountBranches
                     drawBlossoms?)
               (draw-branches (rest angles)))]))]
        (draw-branches angle-list)))]))


; Helperfunktionen für big-bang-handler

; world-state -> image
; renders the fractal tree from the current world state

(define (render world)
  (tree
   (make-posn (/ TREE-CANVAS-SIZE-X 2) (/ TREE-CANVAS-SIZE-Y 1.5))
   (make-vector
    FIRST-BRANCH-RAD
    FIRST-BRANCH-LENGTH)
   (WorldState-verzweigung world)
   (WorldState-growth world)
   (WorldState-colorList world)
   (WorldState-branches world)
   (WorldState-drawBlossoms world)
   ))

; input -> WorldState
; takes input from keyboard and alters the worldState given from the input

; Tests stehen unter funktion, da sich racket sonst beschwert

(define (change world key)
  (cond
    [(key=? key "up") (make-WorldState
                       (+ (WorldState-verzweigung world) 0.1)
                       (WorldState-growth world)
                       (WorldState-colorList world)
                       (WorldState-branches world)
                       (WorldState-drawBlossoms world))]
    [(key=? key "down") (make-WorldState
                         (- (WorldState-verzweigung world) 0.1)
                         (WorldState-growth world)
                         (WorldState-colorList world)
                         (WorldState-branches world)
                         (WorldState-drawBlossoms world))]                      
    [(key=? key "left") (make-WorldState
                         (WorldState-verzweigung world)
                         (+ (WorldState-growth world) 0.1)
                         (WorldState-colorList world)
                         (WorldState-branches world)
                         (WorldState-drawBlossoms world))]
    [(key=? key "right") (make-WorldState
                          (WorldState-verzweigung world)
                          (- (WorldState-growth world) 0.1)
                          (WorldState-colorList world)
                          (WorldState-branches world)
                          (WorldState-drawBlossoms world))]
    [(key=? key "+") (make-WorldState
                        (WorldState-verzweigung world)
                        (WorldState-growth world)
                        (cons (last (WorldState-colorList world)) (start (WorldState-colorList world)))
                        (WorldState-branches world)
                        (WorldState-drawBlossoms world))]
    [(key=? key "-") (make-WorldState
                        (WorldState-verzweigung world)
                        (WorldState-growth world)
                        (append (rest (WorldState-colorList world)) (list (first (WorldState-colorList world))))
                        (WorldState-branches world)
                        (WorldState-drawBlossoms world))]
    [(key=? key " ") (make-WorldState
                      (WorldState-verzweigung world)
                      (WorldState-growth world)
                      (map (lambda (c)
                             (make-color (color-green c) (color-blue c) (color-red c)))
                           (WorldState-colorList world))
                      (WorldState-branches world)
                      (WorldState-drawBlossoms world))]
    [(key=? key "w") (make-WorldState
                      (WorldState-verzweigung world)
                      (WorldState-growth world)
                      (WorldState-colorList world)
                      (+ (WorldState-branches world) 1)
                      (WorldState-drawBlossoms world))]
    [(key=? key "s") (make-WorldState
                      (WorldState-verzweigung world)
                      (WorldState-growth world)
                      (WorldState-colorList world)
                      (- (WorldState-branches world) 1)
                      (WorldState-drawBlossoms world))]
    [(key=? key "q") (make-WorldState
                      (WorldState-verzweigung world)
                      (WorldState-growth world)
                      (rest (WorldState-colorList world))
                      (WorldState-branches world)
                      (WorldState-drawBlossoms world))]
        [(key=? key "t") (make-WorldState
                      (WorldState-verzweigung world)
                      (WorldState-growth world)
                      (WorldState-colorList world)
                      (WorldState-branches world)
                      (cond
                        [(WorldState-drawBlossoms world) false ]
                        [else true]))]
    [else world]))

; Tests für change

(define CHANGE_TEST1 (change (change (change DEFAULT_WORLD_STATE "up") "left") "+"))
(define CHANGE_TEST2 (change (change (change DEFAULT_WORLD_STATE "down") "right") "-"))
(define CHANGE_TEST3 (change DEFAULT_WORLD_STATE "w"))
(define CHANGE_TEST4 (change DEFAULT_WORLD_STATE "s"))
(define CHANGE_TEST5 (change DEFAULT_WORLD_STATE "q"))
(define CHANGE_TEST6 (change DEFAULT_WORLD_STATE "t"))

; Tests for CHANGE_TEST1
(check-within (WorldState-verzweigung CHANGE_TEST1) (+ (/ pi 3) 0.1) 1e-6)
(check-within (WorldState-growth CHANGE_TEST1) 0.76 1e-6)
(check-expect (WorldState-colorList CHANGE_TEST1)
              `(
                ,(make-color 255 0 64)
                ,(make-color 255 0 0)
                ,(make-color 255 128 0)
                ,(make-color 128 255 0)
                ,(make-color 0 255 128)
                ,(make-color 0 255 255)
                ,(make-color 0 128 255)
                ,(make-color 64 0 255)
                ,(make-color 255 0 255)
                ))
(check-expect (WorldState-branches CHANGE_TEST1) 2)
(check-expect (WorldState-drawBlossoms CHANGE_TEST1) true)

; Tests for CHANGE_TEST2
(check-within (WorldState-verzweigung CHANGE_TEST2) (- (/ pi 3) 0.1) 1e-6)
(check-within (WorldState-growth CHANGE_TEST2) 0.56 1e-6)
(check-expect (WorldState-colorList CHANGE_TEST2)
              `(
                ,(make-color 255 128 0)
                ,(make-color 128 255 0)
                ,(make-color 0 255 128)
                ,(make-color 0 255 255)
                ,(make-color 0 128 255)
                ,(make-color 64 0 255)
                ,(make-color 255 0 255)
                ,(make-color 255 0 64)
                ,(make-color 255 0 0)
                ))
(check-expect (WorldState-branches CHANGE_TEST2) 2)
(check-expect (WorldState-drawBlossoms CHANGE_TEST2) true)

; Tests for CHANGE_TEST3
(check-within (WorldState-verzweigung CHANGE_TEST3) (/ pi 3) 1e-6)
(check-within (WorldState-growth CHANGE_TEST3) 0.66 1e-6)
(check-expect (WorldState-colorList CHANGE_TEST3)
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
                ))
(check-expect (WorldState-branches CHANGE_TEST3) 3)
(check-expect (WorldState-drawBlossoms CHANGE_TEST3) true)

; Tests for CHANGE_TEST4
(check-within (WorldState-verzweigung CHANGE_TEST4) (/ pi 3) 1e-6)
(check-within (WorldState-growth CHANGE_TEST4) 0.66 1e-6)
(check-expect (WorldState-colorList CHANGE_TEST4)
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
                ))
(check-expect (WorldState-branches CHANGE_TEST4) 1)
(check-expect (WorldState-drawBlossoms CHANGE_TEST4) true)

; Tests for CHANGE_TEST5
(check-within (WorldState-verzweigung CHANGE_TEST5) (/ pi 3) 1e-6)
(check-within (WorldState-growth CHANGE_TEST5) 0.66 1e-6)
(check-expect (WorldState-colorList CHANGE_TEST5)
              `(
                ,(make-color 255 128 0)
                ,(make-color 128 255 0)
                ,(make-color 0 255 128)
                ,(make-color 0 255 255)
                ,(make-color 0 128 255)
                ,(make-color 64 0 255)
                ,(make-color 255 0 255)
                ,(make-color 255 0 64)
                ))
(check-expect (WorldState-branches CHANGE_TEST5) 2)
(check-expect (WorldState-drawBlossoms CHANGE_TEST5) true)

; Tests for CHANGE_TEST6
(check-within (WorldState-verzweigung CHANGE_TEST6) (/ pi 3) 1e-6)
(check-within (WorldState-growth CHANGE_TEST6) 0.66 1e-6)
(check-expect (WorldState-colorList CHANGE_TEST6)
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
                ))
(check-expect (WorldState-branches CHANGE_TEST6) 2)
(check-expect (WorldState-drawBlossoms CHANGE_TEST6) false)

  

; Aufruf big-bang-funktion

; WorldState input -> image
; takes a world-state and gives an image of a fractal Tree.
; worldState gets altered by user input

(big-bang DEFAULT_WORLD_STATE
  (on-key change)
  (to-draw render)
  )

; Weitere Ausdrücke

; Aufgabe j)

(render (make-WorldState
 1.0471975511965976
 0.66
 (list
  (make-color 0 255 128 255)
  (make-color 0 255 255 255)
  (make-color 0 128 255 255)
  (make-color 64 0 255 255)
  (make-color 255 0 255 255)
  (make-color 255 0 64 255)
  (make-color 255 0 0 255)
  (make-color 255 128 0 255)
  (make-color 128 255 0 255))
 4
 false))
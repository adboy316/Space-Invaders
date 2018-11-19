;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define GAME-OVER (text "GAME-OVER" 24 'black))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T2))

;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main game)
  (big-bang game                   ; game
            (on-tick   tock)       ; game -> game
            (to-draw   render)     ; game -> Image
            (stop-when game-over)  ; game -> Boolean
            (on-key    fire)))     ; game KeyEvent -> game

;; Game -> Game
;; produce the next instance of the game 
(check-expect (tock G0) (make-game (list (make-invader (random WIDTH) 50 12)) empty ; Start-game
                                   (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))) 

(check-expect (tock G2) (make-game (list (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12))          ;In-game
                                   (list (make-missile 150 (+ 300 MISSILE-SPEED))) (make-tank (+ 50 TANK-SPEED) 1)))

(check-expect (tock G3) G0 ) ;; Game-over

;(define (tock g) (list G0)) ;stub

(define (tock g)
  (make-game
       (next-loi  (game-invaders g))
       (next-lom (game-missiles g))
       (next-tank     (game-tank g))))

;;-----------------
;; Tock Helpers



;; ListOfInvader -> ListOfInvader
;; Produce filtered and ticked list of invaders
(check-expect (next-loi empty)(list (make-invader (random WIDTH) 50 12)))
(check-expect (next-loi empty) (list (make-invader (+ 150 INVADER-X-SPEED ) (+ 100 INVADER-Y-SPEED ) 12)))
(check-expect (next-loi (list I1 I2)) (list (make-invader (+ 150 INVADER-X-SPEED ) (+ 100 INVADER-Y-SPEED ) 12)))
(check-expect (next-loi (list I1 I3)) (list (make-invader (+ 150 INVADER-X-SPEED ) (+ 100 INVADER-Y-SPEED ) 12)))

;(define (next-loi loi) (list I1)) ; stub

(define (next-loi loi)
  (onscreen-only (tick-invaders loi)))


;; ListiOInvaders -> ListOfInvaders
;; Produce list of ticked invaders
(check-expect (tick-invaders empty) empty)
(check-expect (tick-invaders (list I1 I2)) (list (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED ) 12)
                                                 (make-invader (+ 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) -10)))

;(define (tick-invaders loi) empty) ;stub

(define (tick-invaders loi)
  (cond [(empty? loi) empty]                   
        [else (list (advance-invader (first loi))                
                    (tick-invaders   (rest loi)))]))

;; Invader -> Invader
;; Produce a new invader that is INVADER-X-SPEED and INVADER-Y-SPEED pixels down the screen
(check-expect (advance-invader I1) (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED ) 12))


;(define (advance-invader inv) I1) ;stub
(define (advance-invader inv)
  (make-invader (+ (invader-x inv) INVADER-X-SPEED)
                (+ (invader-y inv) INVADER-Y-SPEED)
                (invader-dx inv)))


;; ListiOInvaders -> ListOfInvaders
;; Produce a list of invaders containing only the invaders that are onscreen?
;; !!!
(define (onscreen-only loi) empty) ;stub


;; ListOfMissiles -> ListOfMissiles
;; Produce list of ticked invaders
;; !!!
(define (next-lom lom) (list M1)) ; stub

;; Tank -> Tank
;; Advance tank screen coordinates by TANK-SPEED if dir = 1, by -TANK-SPEED if dir = -1
;; !!!
(define (next-tank t) (make-tank (/ WIDTH 2) 1)) ; stub


;;----------------
;; Main Helpers

;; Game -> Image
;; render tank, bullets, invaders 
;; !!!
(define (render game) BACKGROUND ) ;stub

;; Game -> Boolean
;; End game (produce true) when invader reaches bottom of the screen 
;; !!!
(define (game-over game) true) ;stub

;; Game KeyEvent -> Game
;; Handle key-events
;; !!!
(define (fire game) G0) ;stub

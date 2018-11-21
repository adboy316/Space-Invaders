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
  (big-bang game            ; game
    (on-tick   tock )       ; game -> game
    (to-draw   render)      ; game -> Image
    (stop-when game-over?)   ; game -> Boolean
    (on-key    fire)))      ; game KeyEvent -> game

;; Game -> Game
;; produce the next instance of the game 
(check-random (tock G0) (make-game (generate-inv empty) empty ; Start-game
                                   (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))) 

(check-random (tock (make-game (list (make-invader 150 200 12) (make-invader 155 220 -10) (make-invader 225 233 -10))          ;In-game
                                   (list (make-missile (+ 150 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED)) (make-missile 155 210) (make-missile 200 134))
                                   (make-tank 50 1)))
                    (make-game (list (make-invader 150 200 12) (make-invader 222 230 -10) (make-invader 225 233 -10))          ;In-game
                                   (list (make-missile 150 200) (make-missile 155 220) (make-missile 200 134))
                                   (make-tank (+ 50 TANK-SPEED) 1)))

;(define (tock g) (list G0)) ;stub

(define (tock g)
  (make-game
   (next-loi  (game-invaders g) (game-missiles g))
   (next-lom  (game-invaders g) (game-missiles g) )
   (next-tank (game-tank     g))))

;;-----------------
;; Tock Helpers

;; ListOfInvader ListOfMissiles -> ListOfInvader
;; Produce filtered and ticked lists of invaders 
(check-random (next-loi empty empty)
              (generate-inv empty)) ;no invaders/missiles

(check-random (next-loi (list I1) empty)
              (generate-inv (list (make-invader (+ 150 INVADER-X-SPEED ) (+ 100 INVADER-Y-SPEED ) 12)))) ;invaders/no missiles
(check-random (next-loi (list I1 I2) empty)
              (generate-inv (list (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED ) 12)
                                   (make-invader (+ 150 INVADER-X-SPEED)  (+ HEIGHT INVADER-Y-SPEED) -10))))

(check-random (next-loi (list I1) (list M1))
              (generate-inv (list (make-invader (+ 150 INVADER-X-SPEED ) (+ 100 INVADER-Y-SPEED ) 12)))) ;invaders/missiles no-hit single
(check-random (next-loi (list I1)
                        (list(make-missile (+ 150 INVADER-X-SPEED ) (+ 100 INVADER-Y-SPEED ))))
              (generate-inv empty)) ;invaders/missiles hit single
(check-random (next-loi (list I1 I2)
                        (list (make-missile (+ 150 INVADER-X-SPEED ) (+ 100 INVADER-Y-SPEED ))))
              (generate-inv (list (make-invader (+ 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) -10)))) ;invaders/missiles hit 1 miss 1


(define (next-loi loi lom)
  (notHit-only (generate-inv (tick-invaders loi)) lom))


;; ListiOInvaders  -> ListOfInvaders 
;; Produce list of ticked invaders
(check-expect (tick-invaders empty) empty)
(check-expect (tick-invaders (list I1 I2))
              (list (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED ) 12)
                    (make-invader (+ 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) -10)))

(check-expect (tick-invaders (list I1 I2))
              (list (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED ) 12)
                    (make-invader (+ 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) -10)))

;(define (tick-invaders-missiles loi lom) empty) ;stub

(define (tick-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader  (first loi))                
               (tick-invaders    (rest loi)))]))

;; ListOfInvader -> ListOfInvader
;; Consume ListOfInvader and produce ListOfInvaders with invaders randomly on top of screen
(check-random (generate-inv empty) (cond [(< (random 99) INVADE-RATE)
                                          (list (make-invader (random WIDTH) 0 (random 12)))]))


(check-random (generate-inv (list I1)) (cond[(< (random 99) INVADE-RATE)
                                             (list (make-invader (random WIDTH) 0 (random 12)) I1)]))

;(define (generate-inv loi) (list I1)) ;stub


(define (generate-inv loi)
  (cond[(< (random 99) INVADE-RATE)
        (cons (make-invader (random WIDTH) 0 (random 12)) loi)]
       [else loi]))


;; Invader -> Invader Boolean
;; Produce a new invader that is INVADER-X-SPEED and INVADER-Y-SPEED pixels down the screen
(check-expect (advance-invader I1) (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED ) 12))


;(define (advance-invader inv) I1) ;stub
(define (advance-invader inv)
       (make-invader (+ (invader-x inv) INVADER-X-SPEED)
                     (+ (invader-y inv) INVADER-Y-SPEED)
                     (invader-dx inv)))


;; ListiOfInvaders ListOfMissiles -> ListOfInvaders 
;; Produce lists of invaders containing only the invaders that are notHit?
;; Assume Invaders have already been advanced
(check-expect (notHit-only empty empty) empty)
(check-expect (notHit-only (list I1) empty) (list I1)) ; no missiles just invaders
(check-expect (notHit-only empty (list M1)) empty) ; no invaders just missiles

(check-expect (notHit-only (list (make-invader 100 120 10)) (list M2)) ;Missile miss
              (list (make-invader 100 120 10)))

(check-expect (notHit-only (list (make-invader (missile-x M2) (missile-y M2) 10)) (list M2)) empty) ;Missile hit


(check-expect (notHit-only (list (make-invader (missile-x M2) (missile-y M2) 10) ; Missile hit and miss
                                 (make-invader 155 125 -10)) (list M2))
              (list (make-invader 155 125 -10)))

;(define (notHit-only loi lom) empty) ;stub

(define (notHit-only loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (noHit? (first loi) lom)
             (notHit-only (rest loi) lom)
             (cons (first loi) (notHit-only (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; Produce true if invader x and y coordinates = missile x and y coordinates, else produce false

(check-expect (noHit? (make-invader 100 110 -10) empty) false) ;no hit
(check-expect (noHit? (make-invader 100 110 -10) (list M1 M2)) false) ;no hit
(check-expect (noHit? (make-invader 100 110 -10) (list M1 M2 (make-missile 100 110))) true) ;hit


;(define (noHit? inv lom) false) ; stub

(define (noHit? inv lom)
  (cond [(empty? lom) false]
        [else
         (if (equal? (make-missile (invader-x inv) (invader-y inv)) (first lom))
             true
             (noHit?  inv (rest lom)))]))


;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; Produce list of ticked and filtered missiles
(check-expect (next-lom empty empty) empty) ;no missiles and no invaders

(check-expect (next-lom (list I1) empty) empty) ;invaders no missiles

(check-expect (next-lom (list I1 I2) (list (make-missile 150 300) (make-missile 145 245)))
              (list (make-missile 150 (+ 300 MISSILE-SPEED)) (make-missile 145 (+ 245 MISSILE-SPEED)))) ;invaders and missile no hit

(check-expect (next-lom (list (make-invader 150 100 12) (make-invader 130 100 12))
                        (list (make-missile  150 90)   (make-missile 128 90)))
              (list (make-missile 128 (+ 90 MISSILE-SPEED)))) ;1 hit 1 miss

(check-expect (next-lom (list (make-invader 150 100 12) (make-invader 130 100 12)) 
                        (list (make-missile  150 90)   (make-missile 130 90)))
               empty)  ;both hit           

;(define (next-lom loi lom) empty) ; stub

(define (next-lom loi lom)
  (onscreen-only loi (tick-missiles lom)))


;; ListOfMissiles -> ListOfMissiles
;; Produce a list of ticked missiles
(check-expect (tick-missiles empty) empty)

(check-expect (tick-missiles (list (make-missile 150 200)))
              (list (make-missile 150 (+ 200 MISSILE-SPEED))))

(check-expect (tick-missiles (list (make-missile 150 200) (make-missile 150 220)))
              (list (make-missile 150 (+ 200 MISSILE-SPEED)) (make-missile 150 (+ 220 MISSILE-SPEED))))

;(define (tick-missiles lom) empty); stub
(define (tick-missiles lom)
  (cond [(empty? lom) empty]                   
        [else (cons  (advance-missile (first lom))                 
                     (tick-missiles (rest lom)))]))


;; Missile -> Missile
;; Advance missile y screen coordinate by MISSILE-SPEED
(check-expect (advance-missile (make-missile 100 100))
              (make-missile 100 (+ 100 MISSILE-SPEED)))

;(define (advance-missile m) M1) ; stub

(define (advance-missile m)
  (make-missile (missile-x m) (+ (missile-y m) MISSILE-SPEED)))



;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; Produce a list of missiles containing only missiles that are onscreen
   ; Filter missies that reach HEIGHT 0 and missiles that have hit invaders
;; Assume missiles are already ticked
(check-expect (onscreen-only empty empty) empty)

(check-expect (onscreen-only (list I1 I2) empty) empty)

(check-expect (onscreen-only (list I1 I2) (list (make-missile 100 110) (make-missile 100 120))) ;missiles miss
              (list (make-missile 100 110)  (make-missile 100 120)))

(check-expect (onscreen-only (list (make-invader 135 235 -10) (make-invader 165 205 -10)) ;1 hit and 1 miss
                             (list (make-missile 135 235) (make-missile 160 200))) 
               (list (make-missile 160 200)))

(check-expect (onscreen-only (list (make-invader 135 235 -10) (make-invader 160 200 -10)) ;both missiles hit
                             (list (make-missile 135 235) (make-missile 160 200))) 
               empty)

;(define (onscreen-only loi lom) empty) ;stub

(define (onscreen-only loi lom)
  (flew-away (missile-nohit loi lom)))

;; LisOfInvaders ListOfMissiles -> ListOfMissiles
;; Produce a ListOfMissiles that have not collided with invaders

(check-expect (missile-nohit (list (make-invader 135 235 -10) (make-invader 165 205 -10)) ;1 hit and 1 miss
                             (list (make-missile 135 235) (make-missile 160 200))) 
               (list (make-missile 160 200)))

(check-expect (missile-nohit (list (make-invader 135 235 -10) (make-invader 160 200 -10)) ;both missiles hit
                             (list (make-missile 135 235) (make-missile 160 200))) 
               empty)

;(define (missile-nohit loi lom) empty) ;stub

(define (missile-nohit loi lom)
  (cond [(empty? lom) empty]                   
        [else
         (if (noHit? (first loi) lom)
             (missile-nohit (rest loi) (rest lom))
             (cons (first lom) (missile-nohit (rest loi )(rest lom))))]))






;;-----


;; ListOfMissiles -> ListOfMissiles
;; Produce a list of missiles that have not flown away from screen (y coordinate is > 0)
(check-expect (flew-away empty) empty)
(check-expect (flew-away (list M1 M2)) (list M1 M2))
(check-expect (flew-away (list M1 M2 (make-missile 200 1))) (list M1 M2 (make-missile 200 1)))
(check-expect (flew-away (list M1 M2 (make-missile 200 0))) (list M1 M2))
(check-expect (flew-away (list M1 M2 (make-missile 200 -1))) (list M1 M2))

;(define (flew-away lom) empty) ;stub

(define (flew-away lom)
  (cond [(empty? lom) empty]                   
        [else
         (if (flewaway? (first lom))
             (flew-away (rest lom))
             (cons (first lom) (flew-away (rest lom))))]))

;; Missile -> Boolean
;; Produce true if missile y coordiante is < 0, else false
(check-expect (flewaway? M1) false)
(check-expect (flewaway? (make-missile 200 0)) true)
(check-expect (flewaway? (make-missile 200 -1)) true)

;(define (flewaway? m) false) ;stub
(define (flewaway? m)
  (false? (> (missile-y m) 0)))


;; Tank -> Tank
;; Advance tank screen coordinates by TANK-SPEED if dir = 1, by -TANK-SPEED if dir = -1
(check-expect (next-tank (make-tank 30 1)) (make-tank (+ 30 TANK-SPEED) 1))
(check-expect (next-tank (make-tank 30 -1)) (make-tank (- 30 TANK-SPEED) -1))

;(define (next-tank t) (make-tank (/ WIDTH 2) 1)) ; stub
(define (next-tank t)
  (if (> (tank-dir t) 0)
         (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))
         (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))))



;;-----------------------------------------------------






;;----------------
;; Main Helpers

;; Game -> Image
;; render tank, bullets, invaders 
;; !!!
(define (render game) BACKGROUND ) ;stub

;; Game KeyEvent -> Game
;; Handle key-events
;; !!!
(define (fire game) G0) ;stub

;;--------------
;; Stop-when helper

;; Game -> Boolean
;; Consumes game and produces true if it contains an Invader with y coordinate => HEIGHT

(check-expect (game-over? (make-game (list I1 (make-invader 100 200 10)) (list M1) T1)) false)
(check-expect (game-over? (make-game (list (make-invader 100 HEIGHT -10)) (list M1) T1)) true)
(check-expect (game-over? (make-game (list (make-invader 100 (+ 10 HEIGHT) 10)) (list M1) T1)) true)

;(define (game-over? loi) false) ;stub

(define (game-over? g)
  (if (invader-touch? (game-invaders g))
      true
      false))


;; ListOfInvader -> Boolean
;; Consume ListOfInvader and produce true if invader y coordinate is => HEIGHT
(check-expect (invader-touch? (list I1 (make-invader 100 200 10))) false)
(check-expect (invader-touch? (list (make-invader 100 HEIGHT -10))) true)
(check-expect (invader-touch? (list (make-invader 100 (+ 10 HEIGHT) 10))) true)

;(define (invader-touch? loi) false) ;stub

(define (invader-touch? loi)
  (cond [(empty? loi) false]                   
        [else
         (if (touch? (first loi))
              true
              (invader-touch? (rest loi)))]))

;; Invader -> Boolean
;; Consume invader and produce true if invader y coordinate is => HEIGHT, else false

(define (touch? inv)
  (if  ( >= (invader-y inv) HEIGHT)
       true
       false))


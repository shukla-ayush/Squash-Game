;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(check-location "06" "q2.rkt")

(provide
 simulation
 initial-world
 world-ready-to-serve?
 world-after-tick
 world-after-key-event
 world-balls
 world-racket
 ball-x
 ball-y
 racket-x
 racket-y
 ball-vx
 ball-vy
 racket-vx
 racket-vy
 world-after-mouse-event
 racket-after-mouse-event
 racket-selected?)



;; a squash game simulation

;; starts with: (simulation s)
;; where s is speed of simulation that can be specified by user.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SIMULATION-FUNCTION (Main Function):

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; simulation : PosReal -> World
;; GIVEN: the speed of the simulation, in seconds per tick
;;        (so larger numbers run slower)
;; EFFECT : runs the simulation, starting with the initial world
;; RETURNS: the final state of the world

;; EXAMPLES:
;;     (simulation 1) runs in super slow motion
;;     (simulation 1/24) runs at a more realistic speed

;; DESIGN STRATEGY: Combine simpler functions

(define (simulation s)
  (big-bang (initial-world s)
            (on-tick world-after-tick s)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ball:
(define BALL (circle 3 "solid" "black"))

;; racket:
(define RACKET (rectangle 47 7 "solid" "green"))

;; court
(define COURT (empty-scene 425 649))

;; walls:
(define WALL (rectangle 425 649 "outline" "black"))

;; pause-court:
(define PAUSE-COURT (rectangle 425 649 "solid" "yellow"))

;; mouse-pointer:
(define MOUSE-POINTER (circle 4 "solid" "blue"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;

;; WORLD:

;;;;;;;;;

;; REPRESENTATION:
;; A World is represented as (make-world ball racket) with the following
;; fields:

;; INTERPRETATION:
;; balls       : BallList , represents a list of all balls present in world.
;; racket      : Racket , represents the characterstics of racket.
;; speed       : PosReal , represents the speed of simulation as a PosReal,
;;               in Pixels/tick 
;; counter     : Real , represents the state of the game.

;; IMPLEMENTATION:
(define-struct world (balls racket speed counter))

;; CONSTRUCTOR TEMPLATE:
;; (make-world Ball Racket PosReal Integer)

;; OBSERVER TEMPLATE:
;; world-fn : World -> ??
#|(define (world-fn w)
  (...
   (world-balls w)
   (world-racket w)
   (world-speed w)
   (world-counter w))
|#

;;;;;;;;;;;

;; COUNTER:

;;;;;;;;;;;

;; A counter will have 3 states:

;; -- "0" which is the initial state
;; -- "-1" which is the rally state
;; -- ">0" which is the pause state

;;;;;;;;;

;; BALL:

;;;;;;;;;

;; REPRESENTATION:
;; A ball is represented as (make-ball x y vx vy) with the following fields:

;; INTERPRETATION:
;; vx : Integer , represents the velocity component of ball in x-direction
;;               (in pixels per tick)
;; vy : Integer , represents the velocity component of ball in y-direction
;;               (in pixels per tick)
;; x  : Integer , represents the x coordinate of the ball (in pixels)
;; y  : Integer , represents the y coordinate of the ball (in pixels)

;; IMPLEMENTATION:
(define-struct ball (vx vy x y))

;; CONSTRUCTOR TEMPLATE:
;; (make-ball Integer Integer Integer Integer)

;; OBSERVER TEMPLATE:
;; ball-fn : Ball -> ??
#|(define (ball-fn b)
  (...
   (ball-vx b)
   (ball-vy b)
   (ball-x b)
   (ball-y b))
|#


;;;;;;;;;;;;

;; BallList:

;;;;;;;;;;;;

;; An BallList is represented as a list of Integers.

;; Constructor Template and Interpretation:
;; empty                    --- the empty list
;; (cons b bl)
;;   WHERE:
;;    b is a ball           --- the first ball 
;;    bl is the list        --- rest of the balls except the first

;; Observer Template:
;; blist-fn : BallList -> ??
#|(define (balls-fn lst)
  |(cond
    [(empty? lst)...]
    [ else (... (first lst)
                (blist-fn (rest lst)))]))
|#

;;;;;;;;;;;;

;; RACKET:

;;;;;;;;;;;;

;; REPRESENTATION:
;; A racket is represented as
;; (make-racket vx vy x y selected?) with the following
;; fields:

;; INTERPRETATION:
;; vx : Integer , represents the velocity component of racket in x-direction
;;      (in pixels/tick)
;; vy : Integer , represents the velocity component of racket in y-direction
;;      (in pixels/tick)
;; x  : Integer , represents the x coordinate of the racket (in pixels)
;; y  : Integer , represents the y coordinate of the racket (in pixels)
;; selected? : Boolean , represents if racket is selected by mouse or not
;; mouse : Mouse, represents the various fields of mouse.

;; IMPLEMENTATION:
(define-struct racket (vx vy x y selected? mouse))

;; CONSTRUCTOR TEMPLATE:
;; (make-racket Integer Integer Integer Integer Boolean)

;; OBSERVER TEMPLATE:
;; racket-fn : Racket -> ??
#|(define (racket-fn b)
  (...
   (racket-vx b)
   (racket-vy b)
   (racket-x b)
   (racket-y b)))
|#



;;;;;;;;;;

;; MOUSE:

;;;;;;;;;;

;; REPRESENTATION:
;; A mouse is represented as
;; (make-mouse mox moy pressed?) with the following
;; fields:

;; INTERPRETATION:
;; mox : PosReal , represents the x component of mouse in pixels.
;; moy : PosReal , represents the y component of mouse in pixels.
;; pressed? : Boolean , represents if moues button is pressed

;; IMPLEMENTATION:
(define-struct mouse (mox moy pressed?))

;; CONSTRUCTOR TEMPLATE:
;; (make-racket Integer Integer Integer Integer Boolean)

;; OBSERVER TEMPLATE:
;; mouse-fn : Mouse -> ??
#|(define (mouse-fn m)
  (...
   (mouse-mox m)
   (mouse-moy m)
   (mouse-pressed? m)))
|#

;; initial-ball
(define INITIAL-BALL (make-ball 0 0 330 384))

;; initial-racket
(define INITIAL-RACKET (make-racket 0 0 330 384 false (make-mouse 0 0 false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; INITIAL-WORLD: 

;; initial-world : PosReal -> World
;; GIVEN  : the speed of the simulation, in seconds per tick
;;          (so larger numbers run slower)
;; RETURNS: the ready-to-serve state of the world

;; EXAMPLE: (initial-world 1)

;; DESIGN STRATEGY: Use Constructor template on World

(define (initial-world s)
  (make-world
   (cons INITIAL-BALL empty)
   INITIAL-RACKET s 0))


;; TESTS:
(begin-for-test
  (check-equal?(initial-world 1)
               (make-world (list INITIAL-BALL)
                           INITIAL-RACKET
                           1
                           0)
                "initial world check"))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD-READY-TO-SERVE? : 

;; world-ready-to-serve? : World -> Boolean
;; GIVEN  : a world
;; RETURNS: true iff the world is in its ready-to-serve state

;; EXAMPLE:
;; (world-ready-to-serve
;; ((make-world (list(make-ball 0 0 330 384))
;;              (make-racket 0 0 330 384)
;;              (world-speed w)))) => true

;; DESIGN STRATEGY: Use Constructor Template on World

(define (world-ready-to-serve? w)
  (equal? (make-world (cons INITIAL-BALL empty)
                      INITIAL-RACKET
                      (world-speed w) 0) w))


;; TESTS:
(begin-for-test
  (check-equal?(world-ready-to-serve?
                (make-world
                 (list INITIAL-BALL)
                 INITIAL-RACKET
                 1 0))
               true
                "ready to serve check"))

;;;;;;;;;;;;;;;;;;;;

;; WORLD-AFTER-TICK :

;; world-after-tick : World -> World
;; GIVEN  : any world that's possible for the simulation
;; RETURNS: the world that should follow the given world
;;          after a tick

;; EXAMPLES:
;; (world-after-tick (make-world (list (make-ball 0 0 330 384))
;;                               (make-racket 0 0 330 384 false)
;;                               1 0))
;;                                   => (make-world
;;                                      (list (make-ball 0 0 330 384))
;;                                      (make-racket 0 0 330 384 false)
;;                                      1 0)

;; DESIGN STRATEGY: Divide into cases of World Counter.

(define (world-after-tick w)
  (cond
    [(= (world-counter w) 0)
     (initial-world (world-speed w))]
    [(> (world-counter w) 0) (counter-decrease w)]
    [else
     (rally-state w)]
    ))


;; TESTS:
(begin-for-test
  (check-equal?(world-after-tick
                (make-world
                 (list (make-ball 0 0 330 384))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 1 0))
               (make-world
                 (list (make-ball 0 0 330 384))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 1 0)
                "world after tick check")
  
  (check-equal?(world-after-tick
                (make-world
                 (list (make-ball 0 0 300 284))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 0.25 12))
               (make-world
                 (list (make-ball 0 0 300 284))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 0.25 11)
                "world after tick check")
  
  (check-equal?(world-after-tick
                (make-world
                (list (make-ball 3 -9 333 375))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                5 -1))
                (make-world
                (list (make-ball 3 -9 336 366))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                5 -1)
                "world after tick check")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RALLY-STATE:

;; rally-state : World -> World
;; GIVEN: a World
;; RETURNS:  a World bases on changes made in the rally state

;; EXAMPLE:
;;(rally-state
;; (make-world)
;; (list) (make-racket 0 0 200 200 false (make-mouse 0 0 false))
;; 0.5 -1))
;; =>
;; (make-world)
;; (list) (make-racket 0 0 200 200 false (make-mouse 0 0 false))
;; 0.5 3))

;; DESIGN STRATEGY: Divide into cases based on rally state

(define (rally-state w)
  (cond
    
       [(empty? (world-balls w)) (game-pause w)]
       
       [(< (change-pos-y-racket (world-racket w)) 0) (game-pause w)]
       
       [(> (change-pos-y-racket (world-racket w)) 649) (game-pause w)]
       
       [else (make-world
               (balls-rally-state (world-balls w) (world-racket w))
               (racket-rally-state (world-balls w) (world-racket w))
               (world-speed w)
               -1)]))

;; TESTS:
(begin-for-test
 (check-equal?(rally-state
                (make-world
                 (list)
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 1 0))
               (make-world
                 (list)
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 1 3)
                "rally-state check")
  
  (check-equal?(rally-state
                (make-world
                 (list (make-ball 0 0 300 300))
                 (make-racket 0 -5 100 3 false (make-mouse 0 0 false))
                 1 0))
               (make-world
                 (list (make-ball 0 0 300 300))
                 (make-racket 0 -5 100 3 false (make-mouse 0 0 false))
                 1 3)
                "rally-state check")
  
  (check-equal?(rally-state
                (make-world
                 (list (make-ball 0 0 300 300))
                 (make-racket 0 5 330 645 false (make-mouse 0 0 false))
                 1 0))
               (make-world
                 (list (make-ball 0 0 300 300))
                 (make-racket 0 5 330 645 false (make-mouse 0 0 false))
                 1 3)
                "rally-state check"))



;;;;;;;;;;;;;;;;;;;

;; READY-TO-RALLY:

;; ready-to-rally: World -> World
;; GIVEN   : a World
;; RETURNS : a World state with started rally.

;; EXAMPLE:
;; (ready-to-rally (make-world
;;                 (list(make-ball 0 0 330 384))
;;                 (make-racket 0 0 330 384 false)
;;                 1 0)
;;                 =>
;;                 (make-world
;;                 (list(make-ball 3 (- 0 9) 330 384))
;;                 (make-racket 0 0 330 384 false)
;;                 1 0)

;; DESIGN STRATEGY: Use Constructor Template on World

(define (ready-to-rally w)
  (make-world
   (cons (make-ball 3 -9 330 384) empty)
   (make-racket 0 0 330 384 (racket-selected? (world-racket w))
                (racket-mouse (world-racket w)))
   (world-speed w) -1))


;; TESTS:
(begin-for-test
  (check-equal? (ready-to-rally
                 (make-world
                  (list (make-ball 0 0 330 384))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  1 0))
                  (make-world
                  (list (make-ball 3 (- 0 9) 330 384))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  1 -1)
                  "ready to rally state")
  )


;;;;;;;;;;;;;;;;

;; GAME-PAUSE:

;; game-pause: World ->  World
;; GIVEN  : a World
;; RETURNS: a world in pause state

;; EXAMPLE:
;; (game-pause (make-world
;;                  (list (make-ball 3 -9 330 384))
;;                  (make-racket 0 0 330 384 false)
;;                  0.25 -1))
;; =>
;;            (make-world
;;                  (list (make-ball 3 -9 330 384))
;;                  (make-racket 0 0 330 384 false)
;;                  0.25 12))

;; DESIGN STRATEGY: Use Constructor template on World

(define (game-pause w)
  (make-world
   (world-balls w)
   (make-racket
    (racket-vx (world-racket w)) (racket-vy (world-racket w))
    (racket-x (world-racket w)) (racket-y (world-racket w))
    false (make-mouse 0 0 false))
   (world-speed w)
   (/ 3 (world-speed w))))


;; TESTS:
(begin-for-test
  (check-equal? (game-pause
                 (make-world
                  (list (make-ball 3 -9 330 384))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  0.25 -1))
                  (make-world
                  (list (make-ball 3 -9 330 384))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  0.25 12)
                  "game pause check")
  )

;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD-AFTER-KEY:

;; world-after-key-event : World KeyEvent -> World
;; GIVEN  : a world and a key event
;; RETURNS: the world that should follow the given world
;;          after the given key event

;; EXAMPLES:
;; (world-after-key-event
;;  (make-world
;;   (list(make-ball 0 0 300 284)) (make-racket 0 0 330 384 false) 0.25 0) " ")
;;     =>
;;   (make-world
;;    (list (make-ball 3 -9 300 284)) (make-racket 0 0 330 384 false) 0.25 -1) 

;; DESIGN STRATEGY: Divide into cases of key pressed

(define (world-after-key-event w k)
  (cond
    
    [(string=? k " ")
    (if (world-ready-to-serve? w) (ready-to-rally w) (game-pause w))]

    [(string=? k "left")
    ( if(= (world-counter w) -1)
        (left-key-pressed w) (no-rally-left-key-pressed w))]

    [(string=? k "right")
    ( if(= (world-counter w) -1)
        (right-key-pressed w) (no-rally-right-key-pressed w))]

    [(string=? k "up")
    ( if(= (world-counter w) -1)
        (up-key-pressed w) (no-rally-up-key-pressed w))]
    
    [(string=? k "down")
    ( if(= (world-counter w) -1)
        (down-key-pressed w) (no-rally-down-key-pressed w))]

    [(string=? k "b")
    (if (= (world-counter w) -1)
        (make-world
        (add-ball (world-balls w)) (world-racket w) (world-speed w) -1)
         w)]

    [else w]
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no-rally-down-key-pressed: World ->  World
;; GIVEN: a World
;; RETURNS: World after down key is pressed in no rally state.
;; EXAMPLE: (world-after-key-event
;;             (make-world
;;              (list (make-ball 3 -9 300 360))
;;             (make-racket 0 0 330 384 false (make-mouse 0 0 false))
;;              0.25 0) "down")
;;         =>  (make-world
;;              (list (make-ball 3 -9 300 360))
;;              (make-racket 0 0 330 649 false (make-mouse 0 0 false))
;;              0.25 0)

;; DESIGN STRATEGY: Use constructor template of world

(define (no-rally-down-key-pressed w)
  (make-world
   (world-balls w)
   (make-racket
    0
    0
    (racket-x (world-racket w))
    649
    (racket-selected? (world-racket w))
    (racket-mouse (world-racket w)))
   (world-speed w)
   (world-counter w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no-rally-up-key-pressed: World ->  World
;; GIVEN: a World
;; RETURNS: World after up key is pressed in no rally state.
;; EXAMPLE: (world-after-key-event
;;             (make-world
;;              (list (make-ball 3 -9 300 360))
;;              (make-racket 0 0 330 384 false (make-mouse 0 0 false))
;;              0.25 0) "up")
;;         => (make-world
;;              (list (make-ball 3 -9 300 360))
;;              (make-racket 0 0 330 0 false (make-mouse 0 0 false))
;;              0.25 0)

;; DESIGN STRATEGY: Use constructor template of world

(define (no-rally-up-key-pressed w)
  (make-world
   (world-balls w)
   (make-racket
    0
    0
    (racket-x (world-racket w))
    0
    (racket-selected? (world-racket w))
    (racket-mouse (world-racket w)))
   (world-speed w)
   (world-counter w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no-rally-left-key-pressed: World ->  World
;; GIVEN: a World
;; RETURNS: World after left key is pressed in no rally state.
;; EXAMPLE: (world-after-key-event
;;             (make-world
;;              (list(make-ball 0 -9 300 360))
;;              (make-racket 0 0 330 384 false (make-mouse 0 0 false))
;;              0.25 0) "left")
;;         =>  (make-world
;;              (list (make-ball 0 -9 300 360))
;;              (make-racket 0 0 330 384 false (make-mouse 0 0 false))
;;              0.25 0)

;; DESIGN STRATEGY: Use constructor template of world

(define (no-rally-left-key-pressed w)
  (make-world
   (world-balls w)
   (make-racket
    0
    0
    (racket-x (world-racket w))
    (racket-y (world-racket w))
    (racket-selected? (world-racket w))
    (racket-mouse (world-racket w)))
   (world-speed w)
   (world-counter w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no-rally-right-key-pressed: World ->  World
;; GIVEN: a World
;; RETURNS: World after right key is pressed in no rally state.
;; EXAMPLE: (world-after-key-event
;;             (make-world
;;              (list(make-ball 3 -9 300 360))
;;              (make-racket 0 0 330 384 false (make-mouse 0 0 false))
;;              0.25 0) "right")
;;          => (make-world
;;              (list(make-ball 3 -9 300 360))
;;              (make-racket 0 0 330 384 false (make-mouse 0 0 false))
;;              0.25 0)

;; DESIGN STRATEGY: Use constructor template of world

(define (no-rally-right-key-pressed w)
  (make-world
   (world-balls w)
   (make-racket
    0
    0
    (racket-x (world-racket w))
    (racket-y (world-racket w))
    (racket-selected? (world-racket w))
    (racket-mouse (world-racket w)))
   (world-speed w)
   (world-counter w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; left-key-pressed : World -> World
;; GIVEN: a World
;; RETURNS: a World after left key pressed
;; EXAMPLE: (world-after-key-event
;;             (make-world
;;              (list (make-ball 3 -9 300 360))
;;              (make-racket 0 0 330 384 false (make-mouse 0 0 false))
;;              0.25 -1) "left")
;;             (make-world
;;              (list (make-ball 3 -9 300 360))
;;              (make-racket -1 0 330 384 false (make-mouse 0 0 false))
;;              0.25 -1)

;; DESIGN STRATEGY: Use Constructor template on World

(define (left-key-pressed w)
  (make-world
       (world-balls w)
       (make-racket
        (- (racket-vx (world-racket w)) 1)
        (racket-vy (world-racket w))
        (change-pos-x-racket (world-racket w))
        (racket-y (world-racket w))
        (racket-selected? (world-racket w))
        (racket-mouse (world-racket w)))
      (world-speed w)
      -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; right-key-pressed : World -> World
;; GIVEN: a World
;; RETURNS: a World after right key pressed
;; EXAMPLE: (world-after-key-event
;;             (make-world
;;              (list (make-ball 3 -9 300 360))
;;              (make-racket 0 0 330 384 false (make-mouse 0 0 false))
;;              0.25 -1) "right")
;;             (make-world
;;              (list (make-ball 3 -9 300 360))
;;              (make-racket 1 0 330 384 false (make-mouse 0 0 false))
;;              0.25 -1) 

;; DESIGN STRATEGY: Use Constructor template on World

(define (right-key-pressed w)
  (make-world
       (world-balls w)
       (make-racket
        (+ (racket-vx (world-racket w)) 1)
        (racket-vy (world-racket w))
        (change-pos-x-racket (world-racket w))
        (racket-y (world-racket w))
        (racket-selected? (world-racket w))
        (racket-mouse (world-racket w)))
       (world-speed w) -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; up-key-pressed : World -> World
;; GIVEN: a World
;; RETURNS: a World after up key pressed
;; EXAMPLE: (world-after-key-event
;;             (make-world
;;              (list(make-ball 3 -9 300 360))
;;              (make-racket 0 0 330 384 false (make-mouse 0 0 false))
;;              0.25 -1) "up")
;;             (make-world
;;              (list(make-ball 3 -9 300 360))
;;              (make-racket 0 -1 330 384 false (make-mouse 0 0 false))
;;              0.25 -1)

;; DESIGN STRATEGY: Use Constructor template on World

(define (up-key-pressed w)
  (make-world
       (world-balls w)
       (make-racket
       (racket-vx (world-racket w)) (- (racket-vy (world-racket w)) 1)
       (racket-x (world-racket w)) (change-pos-y-racket (world-racket w))
       (racket-selected? (world-racket w))
       (racket-mouse (world-racket w)))
       (world-speed w) -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; down-key-pressed : World -> World
;; GIVEN: a World
;; RETURNS: a World after down key pressed
;; EXAMPLE: (world-after-key-event
;;             (make-world
;;              (list (make-ball 3 -9 300 360))
;;              (make-racket 0 0 330 384 false (make-mouse 0 0 false))
;;              0.25 -1) "down")
;;             (make-world
;;              (list (make-ball 3 -9 300 360))
;;              (make-racket 0 1 330 384 false (make-mouse 0 0 false))
;;              0.25 -1)

;; DESIGN STRATEGY: Use Constructor template on World

(define (down-key-pressed w)
  (make-world
      (world-balls w)
      (make-racket
       (racket-vx (world-racket w)) (+ (racket-vy (world-racket w)) 1)
       (racket-x (world-racket w)) (change-pos-y-racket (world-racket w))
       (racket-selected? (world-racket w))
       (racket-mouse (world-racket w)))
      (world-speed w) -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-ball: BallList -> Ball
;; GIVEN: a BallList
;; RETURNS: a Ball added at rally state to the BallList
;; EXAMPLE: (add-ball (list (make-ball 3 -9 330 384))
;;          => (list (make-ball 3 -9 330 384))

;; DESIGN STRATEGY: Divide into cases of BallList

(define (add-ball l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (make-ball 3 -9 330 384)
       l)]))

;; TESTS:
(begin-for-test
  (check-equal? (world-after-key-event
                 (make-world
                  (list (make-ball 2 2 380 184))
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  1 -1) "b")
                 (make-world
                 (list (make-ball 3 -9 330 384) (make-ball 2 2 380 184))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 1 -1)
                "world after b key")
  
  (check-equal? (world-after-key-event
                 (make-world
                  (list (make-ball 0 0 380 184))
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  1 0) "b")
                 (make-world
                 (list  (make-ball 0 0 380 184))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 1 0)
                "world after b key")
  
  (check-equal? (world-after-key-event
                 (make-world
                  (list '())
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  1 0) "b")
                 (make-world
                 (list '())
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 1 0)
                "world after b key")
  
  (check-equal? (world-after-key-event
                 (make-world
                  (list (make-ball 0 0 330 384))
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  1 0) " ")
                 (make-world
                 (list (make-ball 3 -9 330 384))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 1 -1)
                "world after space key")
  
  (check-equal?(world-after-key-event
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 -1) " ")
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 12)
                "world after space key")
  
  (check-equal?(world-after-key-event
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 -1) "left")
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket -1 0 330 384 false (make-mouse 0 0 false))
                0.25 -1)
                "world after left key")
  
  (check-equal?(world-after-key-event
               (make-world
                (list(make-ball 0 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 0) "left")
               (make-world
                (list (make-ball 0 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 0)
                "world after left key")
  
  (check-equal?(world-after-key-event
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 -1) "right")
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 1 0 330 384 false (make-mouse 0 0 false))
                0.25 -1)
                "world after right key")
  
  (check-equal?(world-after-key-event
               (make-world
                (list(make-ball 3 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 0) "right")
               (make-world
                (list(make-ball 3 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 0)
                "world after right key")
  
  (check-equal?(world-after-key-event
               (make-world
                (list(make-ball 3 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 -1) "up")
               (make-world
                (list(make-ball 3 -9 300 360))
                (make-racket 0 -1 330 384 false (make-mouse 0 0 false))
                0.25 -1)
                "world after up key")
  
  (check-equal?(world-after-key-event
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 0) "up")
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 0 0 330 0 false (make-mouse 0 0 false))
                0.25 0)
                "world after up key")
  
  (check-equal?(world-after-key-event
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 -1) "down")
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 0 1 330 384 false (make-mouse 0 0 false))
                0.25 -1)
                "world after down key")
  
  (check-equal?(world-after-key-event
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                0.25 0) "down")
               (make-world
                (list (make-ball 3 -9 300 360))
                (make-racket 0 0 330 649 false (make-mouse 0 0 false))
                0.25 0)
                "world after down key")

  (check-equal? (world-after-key-event
                 (make-world
                  (list '())
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  1 0) "r")
                 (make-world
                 (list '())
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 1 0)
                "world after any key other than mentioned ones")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD-AFTER-MOUSE-EVENT:

;; world-after-mouse-event : World Int Int MouseEvent -> World
;; GIVEN: a world, the x and y coordinates of a mouse event,
;;        and the mouse event
;; RETURNS: the world that should follow the given world after
;;          the given mouse event

;; REPRESENTATION:
;; --"mx" represensts x coordinate of mouse pointer
;; --"my" represensts y coordinate of mouse pointer
;; --"mev" represents mouse event 

;; EXAMPLE:
;; (world-after-mouse-event
;;        (make-world
;;                (list(make-ball 1 1 320 384))
;;                (make-racket 0 0 330 384 false) 1 0) 320 384 "button-down")
;;      =>(make-world
;;                (list(make-ball 1 1 320 384))
;;                (make-racket 0 0 330 384 true) 1 0)

;; DESIGN STRATEGY: Divide into cases of world counter

(define (world-after-mouse-event w mx my mev)
 (if(= (world-counter w) -1)
   (make-world
    (world-balls w)
    (racket-after-mouse-event (world-racket w) mx my mev)
    (world-speed w)
    (world-counter w))
  w))


;; TESTS:
(begin-for-test
  (check-equal? (world-after-mouse-event
                 (make-world
                  (list (make-ball 1 1 320 384))
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  1 0) 325 384 "button-down")
                 (make-world
                  (list (make-ball 1 1 320 384))
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  1 0)
                  "world after mouse event")
  
  (check-equal? (world-after-mouse-event
                 (make-world
                  (list (make-ball 1 1 320 384))
                  (make-racket 0 0 330 384 false (make-mouse 325 384 true))
                  1 -1) 325 384 "button-down")
                 (make-world
                  (list (make-ball 1 1 320 384))
                  (make-racket 0 0 330 384 true (make-mouse 325 384 true))
                  1 -1)
                  "world after mouse event"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RACKET-AFTER-MOUSE-EVENT:

;; racket-after-mouse-event : Racket Int Int MouseEvent -> Racket
;; GIVEN: a racket, the x and y coordinates of a mouse event,
;;        and the mouse event
;; RETURNS: the racket as it should be after the given mouse event

;; EXAMPLE:
;; (racket-after-mouse-event
;;        (make-racket 0 0 330 384 false) 320 384 "button-down")
;;  =>(make-racket 0 0 330 384 true)


;; DESIGN STRATEGY: Divide into cases of mouse event

(define (racket-after-mouse-event r mx my mev)
  (cond
    
    [(mouse=? mev "button-down")
     (racket-after-button-down r mx my)]
    
    [(mouse=? mev "drag")
     (racket-after-drag r mx my)]
    
    [(mouse=? mev "button-up")
     (racket-after-button-up r mx my)]
    
    [else r]))


;; TESTS:
(begin-for-test
  (check-equal? (racket-after-mouse-event
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  325 384 "button-down")
                 (make-racket 0 0 330 384 true (make-mouse 325 384 true))
                  "racket after mouse event")
  
  (check-equal? (racket-after-mouse-event
                  (make-racket 0 0 330 384 true (make-mouse 325 384 false))
                  280 384 "drag")
                 (make-racket 0 0 285 384 true (make-mouse 280 384 true))
                  "racket after mouse event")
  
  (check-equal? (racket-after-mouse-event
                  (make-racket 0 0 330 384 true (make-mouse 280 384 true))
                  320 384 "button-up")
                 (make-racket 0 0 370 384 false (make-mouse 320 384 false))
                  "racket after mouse event")
  
  (check-equal? (racket-after-mouse-event
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  320 384 "leave")
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  "racket after mouse event")
  
  (check-equal? (racket-after-mouse-event
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  320 384 "move")
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  "racket after mouse event")
  
  (check-equal? (racket-after-mouse-event
                  (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  320 384 "enter")
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                  "racket after mouse event"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RACKET-SELECTED?:

;; racket-selected? : Racket-> Boolean
;; GIVEN: a racket
;; RETURNS: true iff the racket is selected

;; EXAMPLE:
;; (racket-selected? (make-racket 1 1 310 370 false) => false

;; DESIGN STRATEGY: Use constructor template of Racket.


;; TESTS:
(begin-for-test
  (check-equal? (racket-selected? (make-racket 1 1 310 370 false
                                               (make-mouse 0 0 false)))
                false
                "not selected")
  
  (check-equal? (racket-selected? (make-racket 1 1 310 370 true
                                               (make-mouse 0 0 false)))
                true
                "selected")
  )


;;;;;;;;;;;;;;;;;;;;;;

;; WORLD-BALLS:

;; world-balls : World -> BallList
;; GIVEN: a world
;; RETURNS: a list of the balls that are present in the world
;;     (but does not include any balls that have disappeared
;;     by colliding with the back wall)

;; EXAMPLES:
;; (world-balls (make-world
;;              (make-ball 3 -9 300 360)
;;              (make-racket 0 0 330 384) 0.25 -1))
;; => (list (make-ball 3 -9 300 360))

;; DESIGN STRATEGY: Use Constructor Template of World

;; TESTS:
(begin-for-test
  (check-equal?(world-balls
                (make-world
                 (list (make-ball 3 -9 300 360))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 0.25 -1))
                (list (make-ball 3 -9 300 360))
                "world-balls check"))

;;;;;;;;;;;;;;;;;;;;;;

;; WORLD-RACKET:

;; world-racket : World -> Racket
;; GIVEN  : a world
;; RETURNS: the racker that's present in the world

;; EXAMPLES:
;; (world-ball (make-world
;;              (list(make-ball 3 -9 300 360))
;;              (make-racket 0 0 330 384) 0.25 -1))
;; => (make-racket 0 0 330 384)

;; DESIGN STRATEGY: Use Constructor Template of World


;; TESTS:
(begin-for-test
 (check-equal?(world-racket
                (make-world
                 (list (make-ball 3 -9 300 360))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 0.25 -1))
                (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                "world-racket check"))

;;;;;;;;;;;;;;;;;;;;;;;

;; BALL-X

;; ball-x : Ball -> Integer
;; GIVEN: a ball
;; RETURNS: the x coordinate of ball's position,
;;          in graphics coordinates

;; EXAMPLES:
;; (ball-x (first (world-balls
;; (make-world (make-ball 3 -9 300 360)) (make-racket 0 0 330 384) 0.25 -1))
;; => 300

;; DESIGN STRATEGY: Use Constructor Template on World


;; TESTS:
(begin-for-test
 (check-equal? (ball-x (first (world-balls (make-world
                                            (list (make-ball 0 0 300 384))
                                            (make-racket 0 0 300 384
                                                         false
                                                         (make-mouse
                                                          0 0 false))
                                            1 -1))))
               300
               "ball-x check"))

;;;;;;;;;;;

;; BALL-Y

;; ball-y : Ball -> Integer
;; GIVEN: a ball
;; RETURNS: the y coordinate of ball's position,
;;          in graphics coordinates

;; EXAMPLES:
;; (ball-y (first (world-ball
;; (make-world (make-ball 3 -9 300 360)) (make-racket 0 0 330 384) 0.25 -1))
;; => 360

;; DESIGN STRATEGY: Use Constructor Template on World


;; TESTS:
(begin-for-test
 (check-equal? (ball-y (first (world-balls (make-world
                                            (list (make-ball 0 0 300 384))
                                            (make-racket 0 0 300 384
                                                         false
                                                         (make-mouse
                                                          0 0 false))
                                            1 -1))))
               384
               "ball-y check"))

;;;;;;;;;;;;;

;; RACKET-X

;; racket-x : Racket -> Integer
;; GIVEN  : a racket
;; RETURNS: the x coordinate of racket's position,
;;          in graphics coordinates

;; EXAMPLES:
;; (racket-x (world-racket
;; (list (make-ball 3 -9 300 360)) (make-racket 0 0 330 384) 0.25 -1))
;; => 330

;; DESIGN STRATEGY: Use Constructor Template on World

;; TESTS:
(begin-for-test
 (check-equal?(racket-x
               (world-racket
                (make-world
                 (list (make-ball 3 -9 300 360))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 0.25 -1)))
                330
                "world-racket-x check"))

;;;;;;;;;;;

;; RACKET-Y

;; racket-y : Racket -> Integer
;; GIVEN  : a racket
;; RETURNS: the y coordinate of racket's position,
;;          in graphics coordinates

;; EXAMPLES:
;; (racket-y (world-racket
;; (make-ball 3 -9 300 360) (make-racket 0 0 330 384 false) 0.25 -1))
;; => 384

;; DESIGN STRATEGY: Use Constructor Template on World

;; TESTS:
(begin-for-test
 (check-equal?(racket-y
               (world-racket
                (make-world
                 (list (make-ball 3 -9 300 360))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 0.25 -1)))
                384
                "world-racket-y check"))

;;;;;;;;;;;;;;;;;;;;;;;

;; BALL-VX

;; ball-vx : Ball -> Integer
;; GIVEN: a ball
;; RETURNS: the x coordinate of ball's velocity,
;;          in pixels per tick

;; EXAMPLES:
;; (ball-vx (first (world-ball
;; (make-world (make-ball 3 -9 300 360)) (make-racket 0 0 330 384 false)
;; 0.25 -1))
;; => 3

;; DESIGN STRATEGY: Use Constructor Template on World

;; TESTS:
(begin-for-test
 (check-equal? (ball-vx (first (world-balls (make-world
                                            (list (make-ball 0 0 300 384))
                                            (make-racket 0 0 300 384
                                                         false
                                                         (make-mouse
                                                          0 0 false))
                                            1 -1))))
               0
               "ball-vx check"))

;;;;;;;;;;;

;; BALL-VY

;; ball-vy : Ball -> Integer
;; GIVEN: a ball
;; RETURNS: the y coordinate of ball's velocity,
;;          in pixels per tick

;; EXAMPLES:
;; (ball-vy first (world-ball
;; (make-world (make-ball 3 -9 300 360))
;; (make-racket 0 0 330 384 false) 0.25 -1))
;; => -9

;; DESIGN STRATEGY: Use Constructor Template on World

;; TESTS:
(begin-for-test
 (check-equal? (ball-vy (first (world-balls (make-world
                                            (list (make-ball 0 0 300 384))
                                            (make-racket 0 0 300 384
                                                         false
                                                         (make-mouse
                                                          0 0 false))
                                            1 -1))))
               0
               "ball-vy check"))

;;;;;;;;;;;

;; RACKET-VX

;; racket-vx : Racket -> Integer
;; GIVEN  : a racket
;; RETURNS: the x coordinate of racket's velocity,
;;          in pixels per tick

;; EXAMPLES:
;; (racket-vx (world-racket
;; (list (make-ball 3 -9 300 360)) (make-racket 0 0 330 384 false) 0.25 -1))
;; => 0

;; DESIGN STRATEGY: Use Constructor Template on World

;; TESTS:
(begin-for-test
 (check-equal?(racket-vx
               (world-racket
                (make-world
                 (list (make-ball 3 -9 300 360))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 0.25 -1)))
                0
                "world-racket-vx check"))

;;;;;;;;;;;

;; RACKET-VY

;; racket-vy : Racket -> Integer
;; GIVEN  : a racket
;; RETURNS: the y coordinate of racket's velocity,
;;          in pixels per tick

;; EXAMPLES:
;; (racket-vy (world-racket
;; (list (make-ball 3 -9 300 360)) (make-racket 0 0 330 384 false) 0.25 -1))
;; => 0

;; DESIGN STRATEGY: Use Constructor Template on World


;; TESTS:
(begin-for-test
 (check-equal?(racket-vy
               (world-racket
                (make-world
                 (list (make-ball 3 -9 300 360))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 0.25 -1)))
                0
                "world-racket-vy check"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; RACKET-AFTER-BUTTON-DOWN:

;; racket-after-button-down : Racket mx my -> Racket
;; GIVEN : A Racket and x y coordinates of mouse
;; RETURNS: A Racket after button is pressed

;; EXAMPLE:
;; (racket-after-button-down
;;        (make-racket 3 4 300 300 false) 320 380)
;; =>(make-racket 3 4 300 300 true)

;; DESIGN STRATEGY: Divide into cases of pointer inside racket

(define (racket-after-button-down r mx my)
  (if (in-racket? r mx my)
      (make-racket
       (racket-vx r)
       (racket-vy r)
       (+ (mouse-mox (racket-mouse r)) (constant-calc-x r mx))
       (+ (mouse-moy (racket-mouse r)) (constant-calc-y r my))
       true (make-mouse mx my true))
      r))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; constant-calc-x: Racket mx -> Real
;; GIVEN: a Racket and x coordinate of mouse pointer
;; RETURNS: Constant difference of center of mouse pointer and racket

;; DESIGN STRATEGY: Transcribe Formula

(define (constant-calc-x r mx)
  (- (racket-x r) (mouse-mox (racket-mouse r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; constant-calc-x: Racket mx -> Real
;; GIVEN: a Racket and x coordinate of mouse pointer
;; RETURNS: Constant difference of center of mouse pointer and racket

;; DESIGN STRATEGY: Transcribe Formula

(define (constant-calc-y r my)
  (- (racket-y r) (mouse-moy (racket-mouse r))))


;; TESTS:
(begin-for-test
  (check-equal? (racket-after-button-down
                 (make-racket 3 4 300 300 false (make-mouse 0 0 false))
                 285 384)
                (make-racket 3 4 300 300 false (make-mouse 0 0 false))
                "racket after button down"
                )
  
  (check-equal? (racket-after-button-down
                 (make-racket 3 4 330 380 false (make-mouse 280 384 false))
                 280 384)
                (make-racket 3 4 330 380 false (make-mouse 280 384 false))
                "racket after button down"
                )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RACKET-AFTER-DRAG

;; racket-after-drag: Racket mx my -> Racket
;; GIVEN : a Racket and x y coordinates of mouse
;; RETURNS : a Racket after racket dragged through mouse

;; EXAMPLE:
;; (racket-after-drag
;;      (make-racket 2 2 300 380 false) 280 200)
;; =>(make-racket 2 2 300 280)

;; DESIGN STRATEGY: Divide into cases of mouse inside racket

(define (racket-after-drag r mx my)
  (if  (racket-selected? r)
      (make-racket
        0
        0
        (+ mx (constant-calc-x r mx))
        (+ my (constant-calc-y r my))
        true
        (make-mouse mx my true))
      (make-racket
        (racket-vx r)
        (racket-vy r)
        (racket-x r)
        (racket-y r)
        false
        (make-mouse mx my false))))


;; TESTS:
(begin-for-test
  (check-equal? (racket-after-drag
                 (make-racket 2 2 300 380 true
                              (make-mouse 0 0 true))
                 280 200)
                 (make-racket 0 0 580 580 true
                              (make-mouse 280 200 true))
                 "racket after drag")
  
  (check-equal? (racket-after-drag
                 (make-racket 0 0 300 380 false
                              (make-mouse 290 380 false))
                 290 380)
                 (make-racket 0 0 300 380 false
                              (make-mouse 290 380 false))
                 "racket after drag"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RACKET-AFTER-BUTTON-UP

;; racket-after-button-up : Racket mx my -> World
;; GIVEN : a Racket and x y coordinates of mouse
;; RETURNS : a Racket after button is released

;; EXAMPLE:
;; (racket-after-button-up
;;     (make-racket 1 1 300 380 true) 200 380 )
;;  =>(make-racket 1 1 300 380 true)))

;; DESIGN STRATEGY: Divide into cases of mouse inside racket

(define (racket-after-button-up r mx my)
  (if (racket-selected? r)
      (make-racket
       (racket-vx r)
       (racket-vy r)
       (+ mx (constant-calc-x r mx))
       (+ my (constant-calc-y r my))
       false
       (make-mouse mx my false))
      r))


;; TESTS:
(begin-for-test
  (check-equal? (racket-after-button-up
                 (make-racket 1 1 300 380 true
                              (make-mouse 0 0 false))
                 200 380 )
                 (make-racket 1 1 500 760 false
                              (make-mouse 200 380 false)))
  (check-equal? (racket-after-button-up
                 (make-racket 1 1 300 380 false
                              (make-mouse 200 0 false))
                 200 380 )
                 (make-racket 1 1 300 380 false
                              (make-mouse 200 0 false))))

;;;;;;;;;;;;;;;;;;;;;;

;; IN-RACKET?

;; in-racket? : World mx my-> Boolean
;; GIVEN  : a racket
;; RETURNS: true iff the pointer is in racket

;; EXAMPLE:
;; (in-racket?
;;     (make-racket 2 2 300 380) 350 380)
;; => false

;; DESIGN STRATEGY: Use Observer template of Racket on r

(define (in-racket? r mx my)
  (and
   (<=
    (- (racket-x r) 25)
    mx
    (+ (racket-x r) 25))
   (<=
    (- (racket-y r) 25)
    my
    (+ (racket-y r) 25))))

;; TESTS;
(begin-for-test
  (check-equal? (in-racket?
                 (make-racket 2 2 300 380 false (make-mouse 0 0 false))
                 350 380)
                 false
                 "inside racket"))

;;;;;;;;;;;;;;;;;;;;;

;; COUNTER-DECREASE:

;; counter-decrease: World -> World
;; GIVEN  :  a World
;; RETURNS:  a World with counter value decreased by 1.

;; EXAMPLE:
;; (counter-decrease
;;     (make-world
;;           (list (make-ball 0 0 300 354))
;;              (make-racket 0 0 330 384 false) 0.25 12))
;;  => (make-world
;;           (list (make-ball 0 0 300 354))
;;              (make-racket 0 0 330 384 false) 0.25 11)

;; DESIGN STRATEGY: Transcribe Formula

(define (counter-decrease w)
  (make-world
   (world-balls w) (world-racket w) (world-speed w) (- (world-counter w) 1)))

;; TESTS:

(begin-for-test
  (check-equal?(counter-decrease
                (make-world
                (list (make-ball 0 0 300 354))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false))
                 0.25 12))
               (make-world
                 (list (make-ball 0 0 300 354))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false)) 0.25 11)
                "counter after decrement"))

;;;;;;;;;;;;;;;;;;;;;;;

;; CHANGE-POS-X-BALL:

;; change-pos-x-ball: Ball -> ball-x
;; GIVEN  : a Ball
;; RETURNS: x coordinates of the ball after one tick

;; EXAMPLE:
;; (change-pos-x-ball (make-ball 2 2 200 200))
;; => 202

;; DESIGN STRATEGY: Transcribe Formula

(define (change-pos-x-ball b)
  (+ (ball-x b) (ball-vx b)))

;; TESTS:
(begin-for-test
  (check-equal? (change-pos-x-ball (make-ball 2 2 200 200))
                  202
                  "ball x changed")
  )


;;;;;;;;;;;;;;;;;;;;;

;; CHANGE-POS-y-BALL:

;; change-pos-y-ball: Ball -> ball-y
;; GIVEN  : a Ball
;; RETURNS: y coordinates of the ball after one tick

;; EXAMPLE:
;; (change-pos-y-ball (make-ball 2 2 200 200))
;; => 202

;; DESIGN STRATEGY: Transcribe Formula

(define (change-pos-y-ball b) ;)
  (+ (ball-y b) (ball-vy b)))

;; TESTS:
(begin-for-test
  (check-equal? (change-pos-y-ball (make-ball 2 2 200 200))
                  202
                  "ball y changed")
  )

;;;;;;;;;;;;;;;;;;;;;;;

;; CHANGE-POS-X-RACKET:

;; change-pos-x-racket: Racket -> racket-x
;; GIVEN  : a Racket
;; RETURNS: x coordinates of the racket after one tick

;; EXAMPLE:
;; (change-pos-x-racket (make-racket 5 5 200 200 false (make-mouse 0 0 false))
;; => 205

;; DESIGN STRATEGY: Transcribe Formula

(define (change-pos-x-racket r)
  (+ (racket-x r) (racket-vx r)))


;; TESTS:
(begin-for-test
  (check-equal? (change-pos-x-racket (make-racket 5 5 200 200
                                                  false
                                                  (make-mouse 0 0 false)))
                  205
                  "racket x changed")
  )

;;;;;;;;;;;;;;;;;;;;;;;

;; CHANGE-POS-Y-RACKET:

;; change-pos-y-racket: Racket -> racket-y
;; GIVEN  : a Racket
;; RETURNS: y coordinates of the racket after one tick

;; EXAMPLE:
;; (change-pos-y-racket (make-racket 5 5 200 200 false (make-mouse 0 0 false))
;; => 205

;; DESIGN STRATEGY: Transcribe Formula
(define (change-pos-y-racket r)
  (+ (racket-y r) (racket-vy r)))

(begin-for-test
  (check-equal? (change-pos-y-racket (make-racket 5 5 200 200
                                                  false
                                                  (make-mouse 0 0 false)))
                  205
                  "racket y changed")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD-TO-SCENE:

;; world-to-scene : World -> Scene
;; GIVEN  : a World
;; RETURNS: a Scene with racket and ball at the current tick.

;; EXAMPLES: (world-to-scene
;;                 (make-world
;;                  (list (make-world 0 0 330 384))
;;                  (make-racket 0 0 330 384 false
;;                               (make-mouse 0 0 false))
;;                  1
;;                  0))
;;            =>  (place-image BALL 330 384
;;                 (place-image RACKET
;;                 330
;;                 384
;;                 (place-image WALL 212.5 324.5 COURT))))


;; DESIGN STRATEGY: Divide into cases of counter .

(define (world-to-scene w)
  (cond
    [(= (world-counter w) 0)
     (world-to-scene-initial w)]   

    [(> (world-counter w) 0)
     (world-to-scene-pause w (racket-to-pause-scene w))]
    
    [(= (world-counter w) -1)
     (if (mouse-pressed? (racket-mouse (world-racket w)))
         (world-to-scene-rally-pressed w (racket-to-scene w))
         (world-to-scene-rally-not-pressed w (racket-to-scene w))
         )]))

;; TESTS:
(begin-for-test
  (check-equal? (world-to-scene
                 (make-world
                  (list (make-world 0 0 330 384))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  1
                  0))
                (place-image BALL 330 384
                 (place-image RACKET
                 330
                 384
                 (place-image WALL 212.5 324.5 COURT))))
  
  (check-equal? (world-to-scene
                 (make-world
                  (list (make-ball 1 1 333 392))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  1
                  3) )
                (place-image BALL 333 392
                 (place-image RACKET
                  330
                  384
                  (place-image WALL 212.5 324.5 PAUSE-COURT))))

  (check-equal? (world-to-scene
                 (make-world
                  (list (make-ball 1 1 333 392))
                  (make-racket 0 0 330 384 true
                               (make-mouse 10 10 true))
                  1
                  -1) )
                (place-image BALL 333 392
                 (place-image MOUSE-POINTER 10 10
                 (place-image RACKET
                  330
                  384
                  (place-image WALL 212.5 324.5 COURT)))))

  (check-equal? (world-to-scene
                 (make-world
                  (list (make-ball 1 1 333 392))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  1
                  -1))
                (place-image BALL 333 392
                 (place-image RACKET
                  330
                  384
                  (place-image WALL 212.5 324.5 COURT)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene-initial: World -> Scene
;; GIVEN: a World
;; RETURNS: a scene with world in initial state

;; EXAMPLE:
;;  (world-to-scene-initial
;;               (make-world
;;                (list (make-world 0 0 330 384))
;;                (make-racket 0 0 330 384 false
;;                             (make-mouse 0 0 false))
;;                1
;;                0))
;; => (place-image BALL 330 384
;;               (place-image RACKET
;;               330
;;               384
;               (place-image WALL 212.5 324.5 COURT)))

;; DESIGN STRATEGY: Use Constructor template of place-image (provided function)

(define (world-to-scene-initial w)
  (place-image BALL 330 384
               (place-image RACKET
               (racket-x (world-racket w))
               (racket-y (world-racket w))
               (place-image WALL 212.5 324.5 COURT))))

;; TESTS:
(begin-for-test
  (check-equal? (world-to-scene-initial
                 (make-world
                  (list (make-world 0 0 330 384))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  1
                  0))
                (place-image BALL 330 384
                 (place-image RACKET
                 330
                 384
                 (place-image WALL 212.5 324.5 COURT)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene-pause: World -> Scene
;; GIVEN: a World
;; RETURNS: a scene with world in pause state

;; EXAMPLE:
;;(world-to-scene-pause
;;               (make-world
;;                (list (make-ball 1 1 333 392))
;;                (make-racket 0 0 330 384 false
;;                             (make-mouse 0 0 false))
;;                1
;;                0)
;;               (place-image RACKET
;;                 330 384
;;                 (place-image WALL 212.5 324.5 PAUSE-COURT)) )
;; => (place-image BALL 333 392
;;               (place-image RACKET
;;                330
;;                384
;;                (place-image WALL 212.5 324.5 PAUSE-COURT)))


;; DESIGN STRATEGY: Divide into cases of BallList.

#|
(define (world-to-scene-pause w s)
 (cond
   [(empty? (world-balls w)) s]
   [else
   (world-to-scene-pause
    (make-world
     (rest (world-balls w)) (world-racket w) (world-speed w) (world-counter w)) 
   (place-image BALL
               (ball-x (first (world-balls w)))
               (ball-y (first (world-balls w)))
               s))]))
|#

  ;; foldr: (X Y -> Y) Y XList -> Y
  ;; GIVEN: a function, base value and a list
  ;; RETURNS: the result of applying function (X Y -> Y) on the elements of
  ;;          the list from right to left, starting with the base.

;; Use HOF foldr on BallList.

(define (world-to-scene-pause w s)
  (foldr ball-to-scene s (world-balls w)))

;; TESTS:
(begin-for-test
  (check-equal? (world-to-scene-pause
                 (make-world
                  (list (make-ball 1 1 333 392))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  1
                  3)
                 (place-image RACKET
                   330 384
                   (place-image WALL 212.5 324.5 PAUSE-COURT)) )
                (place-image BALL 333 392
                 (place-image RACKET
                  330
                  384
                  (place-image WALL 212.5 324.5 PAUSE-COURT)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene-rally-pressed: World -> Scene
;; GIVEN: a World
;; RETURNS: a scene with world in rally state with mouse pressed and selected

;; EXAMPLE:
;;(world-to-scene-rally-pressed
;;               (make-world
;;                (list (make-ball 1 1 333 392))
;;                (make-racket 0 0 330 384 true
;;                             (make-mouse 10 10 true))
;;                1
;;                0)
;;               (place-image RACKET
;;                 330 384
;;                 (place-image WALL 212.5 324.5 COURT)) )
;; =>
;; (place-image BALL 333 392
;;               (place-image MOUSE-POINTER 10 10
;;               (place-image RACKET
;;                330
;;                384
;;                (place-image WALL 212.5 324.5 COURT))))

;; DESIGN STRATEGY: Divide into cases of BallList
#|
(define (world-to-scene-rally-pressed w s)
 (cond
   [(empty? (world-balls w)) s]
   [else
   (world-to-scene-rally-pressed
    (make-world (rest (world-balls w)) (world-racket w) (world-speed w) -1)
      (place-image BALL
               (ball-x (first (world-balls w)))
               (ball-y (first (world-balls w)))
               (place-image MOUSE-POINTER
                (mouse-mox (racket-mouse (world-racket w)))
                (mouse-moy (racket-mouse (world-racket w))) s)))]))
|#

  ;; foldr: (X Y -> Y) Y XList -> Y
  ;; GIVEN: a function, base value and a list
  ;; RETURNS: the result of applying function (X Y -> Y) on the elements of
  ;;          the list from right to left, starting with the base.

;; DESIGN STRATEGY: Use HOF foldr on BallList.

(define (world-to-scene-rally-pressed w s)
(foldr ball-to-scene
               (place-image MOUSE-POINTER
                (mouse-mox (racket-mouse (world-racket w)))
                (mouse-moy (racket-mouse (world-racket w))) s)
               (world-balls w))) 

;; TESTS:
(begin-for-test
(check-equal? (world-to-scene-rally-pressed
                 (make-world
                  (list (make-ball 1 1 333 392))
                  (make-racket 0 0 330 384 true
                               (make-mouse 10 10 true))
                  1
                  -1)
                 (place-image RACKET
                   330 384
                   (place-image WALL 212.5 324.5 COURT)) )
                (place-image BALL 333 392
                 (place-image MOUSE-POINTER 10 10
                 (place-image RACKET
                  330
                  384
                  (place-image WALL 212.5 324.5 COURT))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene-rally-not-pressed: World -> Scene
;; GIVEN: a World
;; RETURNS: a scene with world in rally state and mouse not pressed

;; EXAMPLE:
;;(world-to-scene-rally-not-pressed
;;               (make-world
;;                (list (make-ball 1 1 333 392))
;;                (make-racket 0 0 330 384 false
;;                             (make-mouse 0 0 false))
;;                1
;;                -1)
;;               (place-image RACKET
;;                 330 384
;;                (place-image WALL 212.5 324.5 COURT)) )
;; => (place-image BALL 333 392
;;               (place-image RACKET
;;                330
;;                384
;;                (place-image WALL 212.5 324.5 COURT)))

;; DESIGN STRATEGY: Divide into cases of BallList

;(define (world-to-scene-rally-not-pressed w s)
;  (cond
;    [(empty? (world-balls w)) s]
;  [else (world-to-scene-rally-not-pressed
;         (make-world
;          (rest (world-balls w)) (world-racket w) (world-speed w) -1)
;  (place-image BALL
;               (ball-x (first (world-balls w)))
;               (ball-y (first (world-balls w)))
;               s))]))

  ;; foldr: (X Y -> Y) Y XList -> Y
  ;; GIVEN: a function, base value and a list
  ;; RETURNS: the result of applying function (X Y -> Y) on the elements of
  ;;          the list from right to left, starting with the base.

;; DESIGN STRATEGY: Use HOF foldr on BallList.

(define (world-to-scene-rally-not-pressed w s)
(foldr ball-to-scene s (world-balls w)))

;; TESTS:
(begin-for-test
(check-equal? (world-to-scene-rally-not-pressed
                 (make-world
                  (list (make-ball 1 1 333 392))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  1
                  -1)
                 (place-image RACKET
                   330 384
                   (place-image WALL 212.5 324.5 COURT)) )
                (place-image BALL 333 392
                 (place-image RACKET
                  330
                  384
                  (place-image WALL 212.5 324.5 COURT)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-to-scene: Ball Scene -> Scene
;; GIVEN: a World
;; RETURNS: a Scene with Ball on court
;; EXAMPLE: (ball-to-scene
;;               (make-ball 0 0 330 380)
;;               (place-image WALL 212.5 324.5 COURT))
;;          =>    (place-image BALL
;;                 330
;;                 380
;;                 (place-image WALL 212.5 324.5 COURT))

;; DESIGN STRATEGY: Use constructor template on place-image

(define (ball-to-scene b s)
  (place-image BALL
               (ball-x b)
               (ball-y b) s))

;; TESTS:
(begin-for-test
  (check-equal? (ball-to-scene
                 (make-ball 0 0 330 380)
                 (place-image WALL 212.5 324.5 COURT))
                (place-image BALL
                  330
                  380
                  (place-image WALL 212.5 324.5 COURT))
                "Correct scene with ball"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-to-scene: World ->  Scene
;; GIVEN: a World
;; RETURNS: a Scene with Racket on court

;; EXAMPLE:
;;(racket-to-scene
;;               (make-world
;;                (list (make-ball 1 1 333 392))
;;                (make-racket 0 0 330 384 false
;;                             (make-mouse 0 0 false))
;;                1
;;                -1))
;; =>
;;(place-image RACKET
;;                330
;;                384
;;                (place-image WALL 212.5 324.5 COURT))

;; DESIGN STRATEGY: Use Constructor Template on place-image

(define (racket-to-scene w)
 (place-image RACKET
  (racket-x (world-racket w)) (racket-y (world-racket w))
   (place-image WALL 212.5 324.5 COURT)))

;; TESTS:
(begin-for-test
(check-equal? (racket-to-scene
                 (make-world
                  (list (make-ball 1 1 333 392))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  1
                  -1))
                 (place-image RACKET
                  330
                  384
                  (place-image WALL 212.5 324.5 COURT))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-to-pause-scene: World -> Scene
;; GIVEN: a World
;; RETURNS: a Scene with Racket and ball(s) on paused court

;; EXAMPLE:
;;(racket-to-pause-scene
;;               (make-world
;;                (list (make-ball 1 1 333 392))
;;                (make-racket 0 0 330 384 false
;;                             (make-mouse 0 0 false))
;;                1
;;                -1))
;; =>
;;(place-image RACKET
;;                330
;;                384
;;                (place-image WALL 212.5 324.5 PAUSE-COURT))

;; DESIGN STRATEGY: Use Constructor Template on place-image

(define (racket-to-pause-scene w)
 (place-image RACKET
  (racket-x (world-racket w)) (racket-y (world-racket w))
   (place-image WALL 212.5 324.5 PAUSE-COURT)))

;; TESTS:
(begin-for-test
(check-equal? (racket-to-pause-scene
                 (make-world
                  (list (make-ball 1 1 333 392))
                  (make-racket 0 0 330 384 false
                               (make-mouse 0 0 false))
                  1
                  -1))
                 (place-image RACKET
                  330
                  384
                  (place-image WALL 212.5 324.5 PAUSE-COURT))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS FOR BALLS-RALLY-STATE:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ball Collision with right wall

;; b-right-clsn: Ball  -> Ball 
;; GIVEN:  a Ball
;; RETURNS: a Ball after collision of ball with right wall

;; EXAMPLE:
;;(b-right-clsn (make-ball 5 5 430 30))
;; => (make-ball -5 5 420 30)

;; DESIGN STRATEGY: Use constructor template of ball.

#|
(define (b-right-clsn l r)
  (cond
    [(empty? l) '()]
    [else
     (cons
     (make-ball (- 0 (ball-vx (first l)))
                (ball-vy (first l))
                (- 425 (- (change-pos-x-ball (first l)) 425))
                (ball-y (first l)))
     (balls-rally-state (rest l) r))]))
|#

#|
;; TESTS:
(begin-for-test
  (check-equal? (b-right-clsn (list (make-ball 5 3 426 300))
                                    (make-racket 0 0 330 384 false
                                                 (make-mouse 0 0 false)))
                (list (make-ball -5 3 419 300))))
|#

(define (b-right-clsn b)
  (make-ball
   (- 0 (ball-vx b))
   (ball-vy b)
   (- 425 (- (change-pos-x-ball b) 425))
   (ball-y b)))

;; TESTS:
(begin-for-test
  (check-equal? (b-right-clsn (make-ball 5 5 430 30))
                (make-ball -5 5 415 30)
                "right wall collision correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ball Collision with left wall

;; b-left-clsn: Ball -> Ball
;; GIVEN:  a Ball
;; RETURNS: a Ball after collision of ball with left wall

;; EXAMPLE:
;;(b-left-clsn (make-ball 5 5 -5 30))
;; =>           (make-ball 5 5 5 30)

;; DESIGN STRATEGY: Use Constructor template of ball.

#|
(define (b-left-clsn l r)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (make-ball (- 0 (ball-vx (first l)))
                 (ball-vy (first l))
                 (- 0 (change-pos-x-ball (first l)))
                 (ball-y (first l)) )
      (balls-rally-state (rest l) r))]))

;; TESTS:
(begin-for-test
  (check-equal? (b-left-clsn (list (make-ball 5 3 -1 300))
                                    (make-racket 0 0 330 384 false
                                                 (make-mouse 0 0 false)))
                (list (make-ball -5 3 -4 300))))
|#

(define (b-left-clsn b)
  (make-ball
   (- 0 (ball-vx b))
   (ball-vy b)
   (- 0 (change-pos-x-ball b))
   (ball-y b) ))

;; TESTS:
(begin-for-test
  (check-equal? (b-left-clsn (make-ball 5 5 -5 30))
                (make-ball -5 5 0 30)
                "left wall collision correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ball Collision with front wall

;; b-left-clsn: Ball -> Ball
;; GIVEN:  a Ball
;; RETURNS: a Ball after collision of ball with front wall

;; EXAMPLE: (b-front-clsn (make-ball 5 -5 5 30))
;; =>       (make-ball 5 5 5 -30)

;; DESIGN STRATEGY: Use constructor template of ball.

#|
(define (b-front-clsn l r)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (make-ball (ball-vx (first l))
                 (- 0 (ball-vy (first l)))
                 (ball-x (first l))
                 (- 0 (change-pos-y-ball (first l))) )
      (balls-rally-state (rest l) r))]))

;; TESTS:
(begin-for-test
  (check-equal? (b-front-clsn (list (make-ball 5 -3 300 -1))
                                    (make-racket 0 0 330 384 false
                                                 (make-mouse 0 0 false)))
                (list (make-ball 5 3 300 4))))
|#

(define (b-front-clsn b)
   (make-ball
    (ball-vx b)
    (- 0 (ball-vy b))
    (ball-x b)
    (- 0 (change-pos-y-ball b))))

;; TESTS:
(begin-for-test
  (check-equal? (b-front-clsn (make-ball 5 -5 5 30))
                (make-ball 5 5 5 -25)
                "front wall collision correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ball Collision with back wall

;; b-left-clsn: BallList Racket -> BallList
;; GIVEN:  a BallList
;; RETURNS: a BallList after collision of ball with back wall

;; DESIGN STRATEGY: Divide into cases of number of BallList elements

;; EXAMPLE:
;;(b-back-clsn (list (make-ball 5 3 35 640)(make-ball 5 3 300 640))
;;                                    (make-racket 0 0 330 384 false
;;                                                 (make-mouse 0 0 false)))
;;  => (list (make-ball 5 3 305 643))))


(define (b-back-clsn l r)
  (cond
    [(empty? l) '()]
    [else
     (balls-rally-state (rest l) r)]))

;; TESTS:
(begin-for-test
  (check-equal? (b-back-clsn (list (make-ball 5 3 35 640)
                                    (make-ball 5 3 300 640))
                                    (make-racket 0 0 330 384 false
                                                 (make-mouse 0 0 false)))
                (list (make-ball 5 3 305 643))))



;(define (b-back-clsn b)
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;; BALL RACKET COLLISION CONDITIONS:

;; ball-racket check

;; ball-racket-check: Ball Racket -> Ball
;; GIVEN: a Ball and a Racket
;; RETURNS: a Ball after ball's y coordinate is greater than
;;          racket's y coordinate

;; EXAMPLE:
;; (ball-racket-check
;;             (make-ball 3 4 5 395)
;;             (make-racket 2 0 425 384 false
;;                         (make-mouse 0 0 false)))
;; =>  (make-ball 3 4 8 399))
  
;; DESIGN STRATEGY: Divide into cases of y coordinate of ball and racket

#|
(define (ball-racket-check l r)
  (cond
    [(> (- (change-pos-y-ball (first l)) (racket-y r)) 10)
     (rally-movement-ball (first l) r)]
    [else (ball-racket (first l) r)]))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-check
                 (list (make-ball 3 4 5 395))
                 (make-racket 2 0 425 384 false
                              (make-mouse 0 0 false)))
                 (list (make-ball 3 4 8 399)))
  
  (check-equal? (ball-racket-check
                 (list (make-ball 3 4 5 390))
                 (make-racket 2 0 425 384 false
                              (make-mouse 0 0 false)))
                 (list (make-ball 3 -4 5 394))))
|#

(define (ball-racket-check b r)
  (cond
    [(> (- (change-pos-y-ball b) (racket-y r)) 10)
     (rally-movement-ball b)]
    [else (ball-racket b r)]))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-check
                 (make-ball 3 4 5 395)
                 (make-racket 2 0 425 384 false
                              (make-mouse 0 0 false)))
                 (make-ball 3 4 8 399))
  
  (check-equal? (ball-racket-check
                 (make-ball 3 4 5 390)
                 (make-racket 2 0 425 384 false
                              (make-mouse 0 0 false)))
                 (make-ball 3 -4 5 394)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-racket collision

;; ball-racket: Ball -> Ball
;; GIVEN:  a Ball
;; RETURNS: a Ball after racket collides with ball.

;; EXAMPLE:
;;(ball-racket
;;                 (list (make-ball 3 4 5 300))
;;                 (make-racket 2 0 425 384 false
;;                              (make-mouse 0 0 false)))
;; => (list (make-ball 3 -4 5 304))

;; DESIGN STRATEGY: Use constructor template of ball.
#|
(define (ball-racket l r)
  (cons (make-ball
              (ball-vx (first l))
              (- (racket-vy r) (ball-vy (first l)))
              (ball-x (first l))
              (change-pos-y-ball (first l)))
        (balls-rally-state (rest l) r)))

;; TESTS:
(begin-for-test
(check-equal? (ball-racket
                 (list (make-ball 3 4 5 300))
                 (make-racket 2 0 425 384 false
                              (make-mouse 0 0 false)))
                 (list (make-ball 3 -4 5 304))))
|#
(define (ball-racket b r)
  (make-ball
              (ball-vx b)
              (- (racket-vy r) (ball-vy b))
              (ball-x b)
              (change-pos-y-ball b)))

;; TESTS:
(begin-for-test
(check-equal? (ball-racket
                 (make-ball 3 4 5 300)
                 (make-racket 2 0 425 384 false
                              (make-mouse 0 0 false)))
                 (make-ball 3 -4 5 304)))

;;;;;;;;; RACKET RIGHT WALL CONDITIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-racket-right wall check

;; ball-racket-right-wall-check: Ball Racket -> Bal
;; GIVEN: a Ball and a Racket
;; RETURNS: a Ball after racket colliding with right wall and then ball.

;; EXAMPLE:
;; (ball-racket-right-wall-check
;;               (make-ball -3 4 5 390)
;;               (make-racket 0 0 330 384 false
;;                            (make-mouse 0 0 false)))
;; =>   (make-ball -3 -4 5 394)))


;; DESIGN STRATEGY: Divide into cases of y coordinate of ball and racket.

#|
(define (ball-racket-right-wall-check l r)
  (cond
    [(> (- (change-pos-y-ball (first l)) (racket-y r)) 10)
     (rally-movement-ball l r)]
    [else (ball-racket-right-wall l r)]))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-right-wall-check
                 (list (make-ball -3 4 5 395))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (list (make-ball -3 4 2 399)))
  
  (check-equal? (ball-racket-right-wall-check
                 (list (make-ball -3 4 5 390))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (list (make-ball -3 -4 5 394))))
|#

(define (ball-racket-right-wall-check b r)
  (cond
    [(> (- (change-pos-y-ball b) (racket-y r)) 10)
     (rally-movement-ball b)]
    [else (ball-racket-right-wall b r)]))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-right-wall-check
                 (make-ball -3 4 5 390)
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-ball -3 -4 5 394)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-racket-right-wall

;; ball-racket-right-wall: Ball Racket -> Ball
;; GIVEN : a Ball and a Racket
;; RETURNS: a Ball after ball collides with racket which has collided
;;          with right wall

;; EXAMPLES:
;;(ball-racket-right-wall
;;                 (make-ball -3 4 5 300)
;;                 (make-racket 0 0 330 384 false
;;                             (make-mouse 0 0 false)))
;; => (make-ball -3 -4 5 304)

;; DESIGN STRATEGY: Use Constructot template of ball.

#|
(define (ball-racket-right-wall l r)
  (cons (make-ball (ball-vx (first l))
              (- (racket-vy r) (ball-vy (first l)))
              (ball-x (first l))
              (change-pos-y-ball (first l)))
        (balls-rally-state (rest l) r)))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-right-wall
                 (list (make-ball -3 4 5 300))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (list (make-ball -3 -4 5 304))))
|#

(define (ball-racket-right-wall b r)
  (make-ball
   (ball-vx b)
   (- (racket-vy r) (ball-vy b))
   (ball-x b)
   (change-pos-y-ball b)))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-right-wall
                 (make-ball -3 4 5 300)
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-ball -3 -4 5 304)))

;;;;;;;;;;; RACKET LEFT WALL CONDITIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-racket-left wall check

;; ball-racket-left-wall-check: Ball Racket -> Ball
;; GIVEN: a Ball and a Racket
;; RETURNS: a Ball after racket colliding with left wall and then ball

;; EXAMPLE:
;;(ball-racket-left-wall-check
;;               (make-ball -3 4 5 395)
;;               (make-racket 0 0 330 384 false
;;                            (make-mouse 0 0 false)))
;; =>            (make-ball -3 4 2 399))

;; DESIGN STRATEGY: Divide into cases of y coordinate of ball and racket

#|
(define (ball-racket-left-wall-check l r)
  (cond
    [(> (- (change-pos-y-ball (first l)) (racket-y r)) 10)
     (rally-movement-ball l r)]
    [else (ball-racket-left-wall l r)]))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-left-wall-check
                 (list (make-ball -3 4 5 395))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (list (make-ball -3 4 2 399)))
  (check-equal? (ball-racket-left-wall-check
                 (list (make-ball -3 4 5 390))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (list (make-ball -3 -4 5 394))))
|#

(define (ball-racket-left-wall-check b r)
  (cond
    [(> (- (change-pos-y-ball b) (racket-y r)) 10)
     (rally-movement-ball b)]
    [else (ball-racket-left-wall b r)]))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-left-wall-check
                 (make-ball -3 4 5 395)
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-ball -3 4 2 399))
  
  (check-equal? (ball-racket-left-wall-check
                 (make-ball -3 4 5 390)
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-ball -3 -4 5 394)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-racket left wall

;; ball-racket-left-wall: Ball -> Ball
;; GIVEN : a Ball 
;; RETURNS: a Ball after ball collides with racket which has collided
;;          with left wall

;; EXAMPLES:
;;(ball-racket-left-wall
;;               (make-ball -3 4 5 300)
;;               (make-racket 0 0 330 384 false
;;                            (make-mouse 0 0 false)))
;; =>            (make-ball -3 -4 5 304)))


;; DESIGN STRATEGY: Use Constructot template of ball.

#|
(define (ball-racket-left-wall l r)
  (cons (make-ball
               (ball-vx (first l))
               (- (racket-vy r) (ball-vy (first l)))
               (ball-x (first l))
               (change-pos-y-ball (first l)))
        (balls-rally-state (rest l) r)))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-left-wall
                 (list (make-ball -3 4 5 300))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (list (make-ball -3 -4 5 304))))
|#

(define (ball-racket-left-wall b r)
  (make-ball
     (ball-vx b)
     (- (racket-vy r) (ball-vy b))
     (ball-x b)
     (change-pos-y-ball b)))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-left-wall
                 (make-ball -3 4 5 300)
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-ball -3 -4 5 304)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BALLS MOTION:

;; rally-movement-ball: Ball -> Ball
;; GIVEN: a Ball
;; RETURNS: a Ball after tick in rally state

;; EXAMPLE:
;;(rally-movement-ball
;;               (make-ball 2 2 200 200)
;;               (make-racket 0 0 330 384 false
;;                            (make-mouse 0 0 false)))
;; =>            (make-ball 2 2 202 202)))

;; DESIGN STRATEGY: Use constructor template of ball.

#|
(define (rally-movement-ball l r)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (make-ball
       (ball-vx (first l)) (ball-vy (first l))
       (change-pos-x-ball (first l)) (change-pos-y-ball (first l)))
      (balls-rally-state (rest l) r))]))

;; TESTS:
(begin-for-test
  (check-equal? (rally-movement-ball
                 (list (make-ball 2 2 200 200))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                (list (make-ball 2 2 202 202))))
|#

(define (rally-movement-ball b)
  (make-ball
   (ball-vx b) (ball-vy b)
   (change-pos-x-ball b) (change-pos-y-ball b)))

;; TESTS:
(begin-for-test
  (check-equal? (rally-movement-ball
                 (make-ball 2 2 200 200))
                (make-ball 2 2 202 202)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; b-racket-clsn: Ball Racket -> Ball
;; GIVEN: a Ball and a Racket
;; RETURNS: Ball depending on the coordinates of racket and ball after
;;          ball and racket collide

;; EXAMPLE:
;;(b-racket-clsn
;;              (make-ball 2 2 370 340)
;;              (make-racket 0 0 330 300 false
;;                           (make-mouse 0 0 false)))
;; =>              (make-ball 2 2 372 342))

;; DESIGN STRATEGY: Divide into cases on coordinates of racket and ball

(define (b-racket-clsn b r)
  (if (>= (ball-vy b) 0)
      (if (<= (- (racket-x r) 23.5) (change-pos-x-ball b) (+ (racket-x r) 23.5))
          (ball-racket-check b r)
          (rally-movement-ball b))
      (rally-movement-ball b)))

;; TESTS:
(begin-for-test
  (check-equal? (b-racket-clsn
                (make-ball 2 2 320 340)
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 322 342))

  (check-equal? (b-racket-clsn
                (make-ball 2 2 370 340)
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 372 342))

  (check-equal? (b-racket-clsn
                (make-ball 2 -2 370 340)
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 -2 372 338)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; b-r-clsn-right-wall: Ball Racket -> Ball
;; GIVEN: a Ball and a Racket
;; RETURNS: a Ball after racket collision (when racket can collide with
;;           right wall)

;; EXAMPLE: (b-r-clsn-right-wall
;;              (make-ball 2 2 390 290)
;;              (make-racket 0 0 430 300 false
;;                           (make-mouse 0 0 false)))
;; =>           (make-ball 2 2 392 292)))


;; DESIGN STRATEGY: Divide into cases of coordinates of ball and racket.

(define (b-r-clsn-right-wall b r)
  (if (>= (- (change-pos-y-ball b) (racket-y r)) 0)
      (if (<=
           (- (racket-x r) 23.5) (change-pos-x-ball b) (+ (racket-x r) 23.5))
          (ball-racket-right-wall-check b r)
          (rally-movement-ball b))
      (rally-movement-ball b)))

;; TESTS:
(begin-for-test
  (check-equal? (b-r-clsn-right-wall
                (make-ball 2 2 420 340)
                (make-racket 0 0 430 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 422 342))
  
  (check-equal? (b-r-clsn-right-wall
                (make-ball 2 2 390 340)
                (make-racket 0 0 430 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 392 342))

  (check-equal? (b-r-clsn-right-wall
                (make-ball 2 2 390 290)
                (make-racket 0 0 430 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 392 292)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; b-r-clsn-left-wall: Ball Racket -> Ball
;; GIVEN: a Ball and a Racket
;; RETURNS: a Ball after racket collision (when racket can collide with
;;           left wall)

;; EXAMPLE:


;; DESIGN STRATEGY: Divide into cases of coordinates of ball and racket.

(define (b-r-clsn-left-wall b r)
  (if (>= (- (change-pos-y-ball b) (racket-y r)) 0)
      (if (<=
           (- (racket-x r) 23.5) (change-pos-x-ball b) (+ (racket-x r) 23.5))
          (ball-racket-left-wall-check b r)
          (rally-movement-ball b))
      (rally-movement-ball b)))

;; TESTS:
(begin-for-test
  (check-equal? (b-r-clsn-left-wall
                (make-ball 2 2 20 340)
                (make-racket 0 0 30 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 22 342))
  
  (check-equal? (b-r-clsn-left-wall
                (make-ball 2 2 70 340)
                (make-racket 0 0 30 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 72 342))

  (check-equal? (b-r-clsn-left-wall
                (make-ball 2 2 390 290)
                (make-racket 0 0 430 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 392 292)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BALLS-RALLY-STATE:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; balls-rally-state: BallList Racket -> BallList
;; GIVEN: a BallList and a Racket
;; RETURNS: a BallList after checking various conditions of collision in rally
;;          state.

;; EXAMPLE:
;;(balls-rally-state
;;              (list (make-ball 2 2 428 340))
;;              (make-racket 0 0 330 300 false
;;                           (make-mouse 0 0 false)))
;; => (list (make-ball -2 2 420 340))


;; DESIGN STRATEGY: Divide into cases of Ball and Racket positions.

#|
(define (balls-rally-state l r)
  (cond

    ;; No Balls
    [(empty? l) '()]
    
    ;; Ball Collision with right wall
    [(> (change-pos-x-ball (first l)) 425)
     (b-right-clsn l r)]

    ;; Ball Collision with left wall
    [(< (change-pos-x-ball (first l)) 0)
     (b-left-clsn l r)]

    ;; Ball Collision with front wall
    [(< (change-pos-y-ball (first l)) 0)
     (b-front-clsn l r)]

    ;; Ball collision with back wall
    [(> (change-pos-y-ball (first l)) 649)
     (b-back-clsn l r)]

    ;; Ball collision with racket
    [(>= (- (change-pos-y-ball (first l)) (racket-y r)) 0)
     (b-racket-clsn l r)]

    ;; Racket collision with right wall
    [(>= (+ (change-pos-x-racket r) (/ 47 2)) 425)
     (b-r-clsn-right-wall l r)]

    ;; Racket collision with left wall  
    [(<= (- (change-pos-x-racket r) (/ 47 2)) 0)
      (b-r-clsn-left-wall l r)]

    ;; No Collisions
    [else (rally-movement-ball l r)]))

;; TESTS:
(begin-for-test
  (check-equal? (balls-rally-state
                (make-ball 2 2 428 340))
                (make-ball -2 2 420 340))

  (check-equal? (balls-rally-state
                (make-ball -2 2 -1 340)
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 3 340))

  (check-equal? (balls-rally-state
                (make-ball -2 -2 5 -1)
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))
               (make-ball -2 2 5 3))

  (check-equal? (balls-rally-state
                (make-ball 2 2 30 200)
                (make-racket 2 0 420 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 32 202))

  (check-equal? (balls-rally-state
                (make-ball 2 2 30 200)
                (make-racket -2 0 1 300 false
                             (make-mouse 0 0 false)))
                (make-ball 2 2 32 202)))
|#

 ;; filter: (X -> Boolean) XList -> XList
 ;; GIVEN: a condition and a List
 ;; RETURNS: a list of elements in the List that satisfy the condition.

;; DESIGN STRATEGY: Use HOF filter on BallList.

(define (balls-rally-state balllist r)
   (filter (lambda (n) (< (change-pos-y-ball n) 649))
           (balls-rally-state-helper balllist r)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; balls-rally-state-helper : BallList Racket ->  BallList
;; GIVEN: a BallList and a Racket
;; RETURNS: a BallList after checking various collision conditions.
;; EXAMPLE: (balls-rally-state-helper
;;               (list (make-ball 5 0 422 80))
;;               (make-racket 0 0 330 384 false (make-mouse 0 0 false)))
;;              (list (make-ball -5 0 423 80))
;;              "ball's correct collision with right wall")
   
;; DESIGN STRATEGY: Use HOF map on BallList.

   ;; map: (X -> Y) XList -> YList
   ;; GIVEN: a function and a list
   ;; RETURNS: the list with function applied on all the values of the list.

(define (balls-rally-state-helper balllist r)
  (map
   ;; lambda: BallList -> BallList
   ;; GIVEN: a BallList
   ;; RETURNS: the BallList after different operations have been performed
   ;;          on the balls during the tick
   (lambda (n)
     (cond

      ;; Ball Collision with right wall
      [(> (change-pos-x-ball n) 425) (b-right-clsn n)]

      ;; Ball Collision with left wall
      [(< (change-pos-x-ball n) 0) (b-left-clsn n)]

      ;; Ball Collision with front wall
      [(< (change-pos-y-ball n) 0) (b-front-clsn n)]

      ;; Ball collision with racket
      [(>= (- (change-pos-y-ball n) (racket-y r)) 0)
       (b-racket-clsn n r)]

      ;; Racket collision with right wall
      [(>= (+ (change-pos-x-racket r) (/ 47 2)) 425)
       (b-r-clsn-right-wall n r)]

      ;; Racket collision with left wall  
      [(<= (- (change-pos-x-racket r) (/ 47 2)) 0)
       (b-r-clsn-left-wall n r)]

      ;; No Collisions
      [else (rally-movement-ball n)])) balllist ))

;; TESTS:
(begin-for-test
  (check-equal? (balls-rally-state-helper
                 (list (make-ball 5 0 422 80))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false)))
                (list (make-ball -5 0 423 80))
                "ball's correct collision with right wall")
  
  (check-equal? (balls-rally-state-helper
                 (list (make-ball -5 0 3 80))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false)))
                (list (make-ball 5 0 2 80))
                "ball's correct collision with left wall")
  
  (check-equal? (balls-rally-state-helper
                 (list (make-ball 0 -5 200 1))
                 (make-racket 0 0 330 384 false (make-mouse 0 0 false)))
                (list (make-ball 0 5 200 4))
                "ball's correct collision with front wall")

  (check-equal? (balls-rally-state-helper
                 (list (make-ball 5 5 150 150))
                 (make-racket 5 0 422 384 false (make-mouse 0 0 false)))
                (list (make-ball 5 5 155 155))
                "racket's correct collision with right wall")

  (check-equal? (balls-rally-state-helper
                 (list (make-ball 5 5 150 150))
                 (make-racket -5 0 3 384 false (make-mouse 0 0 false)))
                (list (make-ball 5 5 155 155))
                "racket's correct collision with left wall"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS FOR RACKET-RALLY-STATE:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; RACKET BALL COLLISION CONDITIONS:

;; racket-ball check

;; racket-ball-check: BallList Racket -> Racket
;; GIVEN: a BallList and a Racket
;; RETURNS: a Racket after ball's y coordinate is greater than
;;          racket's y coordinate

;; EXAMPLE:
;;(racket-ball-check
;;                 (list (make-ball 3 4 5 395))
;;                 (make-racket 2 0 420 384 false
;;                              (make-mouse 0 0 false)))
;; => (make-racket 2 0 422 384 false
;;                              (make-mouse 0 0 false))

;; DESIGN STRATEGY: Divide into cases of y coordinate of ball and racket

(define (racket-ball-check l r)
  (cond
    [(> (- (change-pos-y-ball (first l)) (racket-y r)) 10)
     (rally-movement-racket r)]
    [else (racket-ball r)]))

;; TESTS:
(begin-for-test
  (check-equal? (racket-ball-check
                 (list (make-ball 3 4 5 395))
                 (make-racket 2 0 420 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 2 0 422 384 false
                              (make-mouse 0 0 false)))
  
  (check-equal? (racket-ball-check
                 (list (make-ball 3 4 5 390))
                 (make-racket 2 0 425 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 2 0 425 384 false
                              (make-mouse 0 0 false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-ball collision

;; racket-ball: BallList Racket -> Racket
;; GIVEN:  a BallList and a Racket
;; RETURNS: a Racket after racket collides with ball.

;; EXAMPLE:
;;(racket-ball
;;           (make-racket 2 -4 425 384 false
;;                          (make-mouse 0 0 false)))
;; => (make-racket 2 0 425 384 false
;;                             (make-mouse 0 0 false))

;; DESIGN STRATEGY: Divide into cases of racket-vy 

(define (racket-ball r)
  (if(< (racket-vy r) 0)
     (make-racket
      (racket-vx r)
      0
      (racket-x r)
      (racket-y r)
      (racket-selected? r)
      (racket-mouse r))
     r))

;; TESTS:
(begin-for-test
(check-equal? (racket-ball
                 (make-racket 2 -4 425 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 2 0 425 384 false
                              (make-mouse 0 0 false))))

;;;;;; RACKET RIGHT WALL CONDITIONS:


;; racket-right-wall

;; r-right-wall: BallList Racket -> Racket
;; GIVEN: a BallList and a Racket
;; RETURNS: a Racket after racket collides with right wall
;;         (for changes in Racket)

;; EXAMPLE:
;;(r-right-wall
;;             (list (make-ball 3 4 5 300))
;;             (make-racket 2 0 425 384 false
;;                              (make-mouse 0 0 false)))
;; => (make-racket 0 0 401.5 384 false
;;                             (make-mouse 0 0 false))

;; DESIGN STRATEGY: Use constructor template on Racket

(define (r-right-wall l r)
  (make-racket
       0
       (racket-vy r)
       (- 425 (/ 47 2))
       (racket-y r)
       (racket-selected? r)
       (racket-mouse r)))

;; TESTS:
(begin-for-test
  (check-equal? (r-right-wall
                 (list (make-ball 3 4 5 300))
                 (make-racket 2 0 425 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 0 0 401.5 384 false
                              (make-mouse 0 0 false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-ball-right wall check

;; racket-ball-right-wall-check: BallList Racket -> Racket
;; GIVEN: a BallList and a Racket
;; RETURNS: a Racket after racket colliding with right wall and then ball

;; EXAMPLE:
;;(racket-ball-right-wall-check
;;         (list (make-ball -3 4 5 395))
;;               (make-racket 0 0 330 384 false
;;                            (make-mouse 0 0 false)))
;; => (make-racket 0 0 330 384 false
;;                             (make-mouse 0 0 false))

;; DESIGN STRATEGY: Divide into cases of y coordinate of ball and racket

(define (racket-ball-right-wall-check l r)
  (cond
    [(> (- (change-pos-y-ball (first l)) (racket-y r)) 10)
     (rally-movement-racket r)]
    [else (racket-ball-right-wall l r)]))

;; TESTS:
(begin-for-test
  (check-equal? (racket-ball-right-wall-check
                 (list (make-ball -3 4 5 395))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
  
  (check-equal? (racket-ball-right-wall-check
                 (list (make-ball -3 4 5 390))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 0 0 401.5 384 false
                              (make-mouse 0 0 false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-ball-right-wall

;; racket-ball-right-wall: BallList Racket -> Racket
;; GIVEN : a BallList and a Racket
;; RETURNS: a Racket after ball collides with racket which has collided
;;          with right wall

;; EXAMPLES:
;(racket-ball-right-wall
;                 (list (make-ball -3 4 5 300))
;                 (make-racket 0 0 330 384 false
;                              (make-mouse 0 0 false)))
; => (list (make-ball -3 -4 5 304))

;; DESIGN STRATEGY: Use Constructot template on Racket
(define (racket-ball-right-wall l r)
  (make-racket 0 0 (- 425 (/ 47 2)) (racket-y r)
                (racket-selected? r)
                (racket-mouse r)))

;; TESTS:
(begin-for-test
  (check-equal? (racket-ball-right-wall
                 (list (make-ball -3 4 5 300))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 0 0 401.5 384 false
                              (make-mouse 0 0 false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; RACKET LEFT WALL CONDITIONS:

;; racket-left-wall

;; r--left-wall: BallList Racket -> Racket
;; GIVEN: a BallList and a Racket
;; RETURNS: a Racket after racket collides with left wall
;;         (for changes in ball)

;; EXAMPLE:
;;(r-left-wall
;;         (list (make-ball -3 4 5 300))
;;             (make-racket 0 0 330 384 false
;;                        (make-mouse 0 0 false)))
;; => (make-racket 0 0 23.5 384 false
;;                              (make-mouse 0 0 false))

;; DESIGN STRATEGY: Use constructor template on Racket

(define (r-left-wall l r)
  (make-racket
       0 (racket-vy r)
       (/ 47 2) (racket-y r)
       (racket-selected? r)
       (racket-mouse r)))

;; TESTS:
(begin-for-test
  (check-equal? (r-left-wall
                 (list (make-ball -3 4 5 300))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 0 0 23.5 384 false
                              (make-mouse 0 0 false))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-ball-left wall check

;; racket-ball-left-wall-check: BallList Racket -> Racket
;; GIVEN: a BallList and a Racket
;; RETURNS: a Racket after racket colliding with left wall and then ball

;; EXAMPLE:
;;(racket-ball-left-wall-check
;;                 (list (make-ball -3 4 5 390))
;;                 (make-racket 0 0 330 384 false
;;                              (make-mouse 0 0 false)))
;; (make-racket 0 0 23.5 384 false
;;                             (make-mouse 0 0 false))

;; DESIGN STRATEGY: Divide into cases of y coordinate of ball and racket

(define (racket-ball-left-wall-check l r)
  (cond
    [(> (- (change-pos-y-ball (first l)) (racket-y r)) 10)
     (rally-movement-racket r)]
    [else (racket-ball-left-wall l r)]))

;; TESTS:
(begin-for-test
  (check-equal? (racket-ball-left-wall-check
                 (list (make-ball -3 4 5 395))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
  (check-equal? (racket-ball-left-wall-check
                 (list (make-ball -3 4 5 390))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 0 0 23.5 384 false
                              (make-mouse 0 0 false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-ball left wall

;; racket-ball-left-wall: BallList Racket -> Racket
;; GIVEN : a BallList and a Racket
;; RETURNS: a Racket after ball collides with racket which has collided
;;          with left wall

;; EXAMPLES:
;;(racket-ball-left-wall
;;              (list (make-ball -3 4 5 300))
;;               (make-racket 0 0 330 384 false
;;                           (make-mouse 0 0 false)))
;; => (make-racket 0 0 23.5 384 false
;;                             (make-mouse 0 0 false))

;; DESIGN STRATEGY: Use Constructot template on Racket

(define (racket-ball-left-wall l r)
  (make-racket
               0 0 (/ 47 2)
               (racket-y r)
               (racket-selected? r)
               (racket-mouse r)))

;; TESTS:
(begin-for-test
  (check-equal? (racket-ball-left-wall
                 (list (make-ball -3 4 5 300))
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                 (make-racket 0 0 23.5 384 false
                              (make-mouse 0 0 false))))

;;;;;;;;;;;;;;;;;;;;;

;;;;;; RALLY MOTION:

;; racket rally movement

;; rally-movement-racket: BallList Racket -> Racket
;; GIVEN: a BallList and a Racket
;; RETURNS: a Racket after tick in rally state

;; EXAMPLE:
;; (rally-movement-racket
;;                 (make-racket 0 0 330 384 false
;;                              (make-mouse 0 0 false)))
;; => (make-racket 0 0 330 384 false
;;                              (make-mouse 0 0 false))

;; DESIGN STRATEGY: Constructor template on Racket

(define (rally-movement-racket r)
  (make-racket
   (racket-vx r) (racket-vy r)
   (change-pos-x-racket r) (change-pos-y-racket r)
   (racket-selected? r)
   (racket-mouse r)))

;; TESTS:
(begin-for-test
  (check-equal? (rally-movement-racket
                 (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false)))
                (make-racket 0 0 330 384 false
                              (make-mouse 0 0 false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; r-clsn-right-wall: BallList Racket -> Racket
;; GIVEN: a BallList and a Racket
;; RETURNS: a Racket depending upon the change in coordinates
;;          that occur due to collision of ball and racket (racket collides
;;          with right wall)

;; EXAMPLE:
;;(r-clsn-right-wall
;;              (list (make-ball 2 2 420 340))
;;              (make-racket 0 0 430 300 false
;;                           (make-mouse 0 0 false)))
;; (make-racket 0 0 430 300 false
;;                           (make-mouse 0 0 false))

;; DESIGN STRATEGY: Divide into cases of relative coordinates of ball and
;;                  racket

#|
(define (r-clsn-right-wall l r)
  (cond
        [(>= (- (change-pos-y-ball (first l)) (racket-y r)) 0)
         (cond
           [(< (change-pos-x-ball (first l)) (+ (racket-x r) 23.5))
            (if (> (change-pos-x-ball (first l)) (- (racket-x r) 23.5))
                (racket-ball-right-wall-check l r)
                 (rally-movement-racket r))])]
        [(< (- (change-pos-y-ball (first l)) (racket-y r)) 0)
         (r-right-wall l r)]))
|#

(define (r-clsn-right-wall l r)
  (if (>= (- (change-pos-y-ball (first l)) (racket-y r)) 0)
      (if (<=
           (- (racket-x r) 23.5)
           (change-pos-x-ball (first l))
           (+ (racket-x r) 23.5))
          (racket-ball-right-wall-check l r)
          (rally-movement-racket r))
      (r-right-wall l r)))

;;TESTS:
(begin-for-test
  (check-equal? (r-clsn-right-wall
                (list (make-ball 2 2 420 340))
                (make-racket 0 0 430 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 430 300 false
                             (make-mouse 0 0 false)))
  
  (check-equal? (r-clsn-right-wall
                (list (make-ball 2 2 390 340))
                (make-racket 0 0 430 300 false
                             (make-mouse 0 0 false)))
               (make-racket 0 0 430 300 false
                             (make-mouse 0 0 false)))

  (check-equal? (r-clsn-right-wall
                (list (make-ball 2 2 390 290))
                (make-racket 0 0 430 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 401.5 300 false
                             (make-mouse 0 0 false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; r-clsn-left-wall: BallList Racket -> Racket
;; GIVEN: a BallList and a Racket
;; RETURNS: a Racket depending upon the change in coordinates
;;          that occur due to collision of ball and racket (racket collides
;;          with left wall)

;; EXAMPLE:
;;(r-clsn-left-wall
;;              (list (make-ball 2 2 20 340))
;;              (make-racket 0 0 30 300 false
;;                           (make-mouse 0 0 false)))
;; (make-racket 0 0 30 300 false
;;                           (make-mouse 0 0 false))

;; DESIGN STRATEGY: Divide into cases of relative coordinates of ball and
;;                  racket

#|
(define (r-clsn-left-wall l r)
  (cond
        [(>= (- (change-pos-y-ball (first l)) (racket-y r)) 0)
         (if(< (change-pos-x-ball (first l)) (+ (racket-x r) 23.5))
             (cond
               [(> (change-pos-x-ball (first l)) (- (racket-x r) 23.5))
                (racket-ball-left-wall-check l r)])
            (rally-movement-racket r))]
        [(< (- (change-pos-y-ball (first l)) (racket-y r)) 0)
         (r-left-wall l r)]))
|#

(define (r-clsn-left-wall l r)
  (if (>= (- (change-pos-y-ball (first l)) (racket-y r)) 0)
      (if (<=
           (- (racket-x r) 23.5)
           (change-pos-x-ball (first l))
           (+ (racket-x r) 23.5))
          (racket-ball-left-wall-check l r)
          (rally-movement-racket r))
      (r-left-wall l r)))

;; TESTS:
(begin-for-test
  (check-equal? (r-clsn-left-wall
                (list (make-ball 2 2 20 340))
                (make-racket 0 0 30 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 30 300 false
                             (make-mouse 0 0 false)))
  
  (check-equal? (r-clsn-left-wall
                (list (make-ball 2 2 70 340))
                (make-racket 0 0 30 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 30 300 false
                             (make-mouse 0 0 false)))

  (check-equal? (r-clsn-left-wall
                (list (make-ball 2 2 390 290))
                (make-racket 0 0 430 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 23.5 300 false
                             (make-mouse 0 0 false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; r-ball-clsn: BallList Racket -> Racket
;; GIVEN: a BallList and a Racket
;; RETURNS: Racket depending on the coordinates of racket and ball after
;;          ball and racket collide

;; EXAMPLE:
;;(r-ball-clsn
;;          (list (make-ball 2 2 320 340))
;;            (make-racket 0 0 330 300 false
;;                           (make-mouse 0 0 false)))
;; => (make-racket 0 0 330 300 false
;;                           (make-mouse 0 0 false))

;; DESIGN STRATEGY: Divide into cases on coordinates of racket and ball


(define (r-ball-clsn l r)
  (if (>= (ball-vy (first l)) 0)
     (if(< (change-pos-x-ball (first l)) (+ (racket-x r) 23.5))
        (if (> (change-pos-x-ball (first l)) (- (racket-x r) 23.5))
            (racket-ball-check l r)
             (rally-movement-racket r))
        (rally-movement-racket r))
     (rally-movement-racket r)))



;; TESTS:
(begin-for-test
  (check-equal? (r-ball-clsn
                (list (make-ball 2 2 320 340))
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))

  (check-equal? (r-ball-clsn
                (list (make-ball 2 2 370 340))
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))

  (check-equal? (r-ball-clsn
                (list (make-ball 2 2 300 340))
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))

  (check-equal? (r-ball-clsn
                (list (make-ball 2 -2 370 340))
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 330 300 false
                             (make-mouse 0 0 false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RACKET-RALLY-STATE:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rackets-rally-state: BallList Racket -> Racket
;; GIVEN: a BallList and a Racket
;; RETURNS: a Rcket after checking various conditions of collision in rally
;;          state.

;; EXAMPLE:
;;(racket-rally-state
;;              (list (make-ball 2 -2 370 340))
;;              (make-racket 2 0 424 300 false
;;                           (make-mouse 0 0 false)))
;; => (make-racket 2 0 426 300 false
;;                           (make-mouse 0 0 false))


;; DESIGN STRATEGY: Divide into cases of relative
;;                  coordinates of racket and ball.


(define (racket-rally-state l r)
  (cond

    ;; racket-collision with ball
    [(>= (- (change-pos-y-ball (first l)) (racket-y r)) 0)
     (r-ball-clsn l r)]
    
     ;;racket collision with right wall
    [(>= (+ (change-pos-x-racket r) (/ 47 2)) 425)
     (r-clsn-right-wall l r)]

    ;; Racket collision with left wall  
    [(<= (- (change-pos-x-racket r) (/ 47 2)) 0)
      (r-clsn-left-wall l r)]
    
    ;; Racket No Collision
    [else (rally-movement-racket r)]))

;; TESTS:
(begin-for-test
  (check-equal? (racket-rally-state
                (list (make-ball 2 -2 370 100))
                (make-racket 2 0 424 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 401.5 300 false
                             (make-mouse 0 0 false)))

  (check-equal? (racket-rally-state
                (list (make-ball 2 -2 370 250))
                (make-racket -2 0 1 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 23.5 300 false
                             (make-mouse 0 0 false)))

  (check-equal? (racket-rally-state
                (list (make-ball 0 0 310 350))
                (make-racket 0 0 20 300 false
                             (make-mouse 0 0 false)))
                (make-racket 0 0 20 300 false
                             (make-mouse 0 0 false))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  

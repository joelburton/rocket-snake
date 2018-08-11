#lang racket
(require 2htdp/universe 2htdp/image)

;;; Snake

; Constants

(define SCALE 20)  ; translation of xy of food/snake -> pixels
(define WIDTH 30)  ; width of board in xy
(define HEIGHT 30) ; height of board in xy
(define GROW 2)    ; # of lengths to grow when eating pellet

(define SNAKE (circle (/ SCALE 2) "solid" "red"))
(define FOOD (circle (/ SCALE 2) "solid" "green"))
(define SCENE (empty-scene (* SCALE WIDTH) (* SCALE HEIGHT)))


; Game state is
;   dir = "up|down|left|right"
;   snake = (list (posn x y) (posn x y) ...)   [head is start]
;   food = (list (posn x y) (posn x y) ...)
;   grow = # of ticks where snake should grow (goes down each tick, up each pellet eaten)

(struct game [dir snake food grow] #:transparent)
(struct posn [x y] #:transparent)

;; Rendering Functions

; render-snake: Snake -> Image
;   Recursively renders snake onto passed-in-scene
(define (render-snake snake scene)
  (foldl
   (λ (pos next) (place-image SNAKE (* SCALE (posn-x pos)) (* SCALE (posn-y pos)) next))
   scene
   snake)) 

; render-food: Food Chain -> Image
;    Recursively renders food dots onto passed-in-scene
(define (render-food food scene)
  (foldl
   (λ (pos next) (place-image FOOD (* SCALE (posn-x pos)) (* SCALE (posn-y pos)) next))
   scene
   food))

; render: World -> Image
;    Render all: food and snake on top of SCENE
(define (render w)
  (render-food (game-food w) (render-snake (game-snake w) SCENE)))

;; Key handling & direction changing
;;   this stuff doesn't actually move snake -- snake moves on each tick; the key
;;   handling is to change the *direction it will move*, not move it

; new-direction: Key Dir -> Dir
;    Decide new direction for snake: can't change 180 degrees
(define (new-direction key dir)
  (cond
    [(and (key=? key "up") (not (string=? dir "down"))) "up"]
    [(and (key=? key "down") (not (string=? dir "up"))) "down"]
    [(and (key=? key "left") (not (string=? dir "right"))) "left"]
    [(and (key=? key "right") (not (string=? dir "left"))) "right"]
    [else dir]))
    
; keyhander: World Key -> Posn
;    Handle keys:
;      up/down/left/right: change direction
(define (keyhandler w key)
  (if (member key '("up" "down" "left" "right"))
      (struct-copy game w [dir (new-direction key (game-dir w))])
      w))

;; Moving Snake
;;   the snake is a list of points, "moving it" is adding a new head in the right 
;;   direction and, if snake isn't supposed to be growing, to truncate the tail

; truncate: Seq -> Seq
;    Truncate last item from list
(define (truncate seq)
  (reverse (cdr (reverse seq))))

; move-snake: Snake Dir Grow -> Snake
;   Add move snake in given direction; if not growing, truncate end
(define (move-snake snake dir grow)
  (define (_move head dx dy) (posn (+ (posn-x head) dx) (+ (posn-y head) dy)))
  (cons
   (cond
     [(string=? dir "up") (_move (first snake) 0 -1)]
     [(string=? dir "down") (_move (first snake) 0 +1)]
     [(string=? dir "left") (_move (first snake) -1 0)]
     [(string=? dir "right") (_move (first snake) +1 0)])
   (if (positive? grow) snake (truncate snake))))

;; Food-eating functions
;;   snake eats a pellet if head collides with it
;;   on eating a pellet, add a new pllet to board

; is-eating? snake food -> Boolean
;   Is the snake eating a food pellet?
(define (is-eating? snake food)
  (if (member (first snake) food) #t #f))

;; Game Update functions -- called on every tick and handle parts of state

; update-grow: ate grow -> grow
;    Always decreate grow, and, if ate, increase by GROW. Don't go below 0.
(define (update-grow ate grow)
  (max 0 (+ (if ate GROW 0) (sub1 grow))))

; new-food: food snake -> posn
;   Generate a random food pellet, avoiding overlapping with existing food or snake
(define (new-food food snake)
  (define (new-food-check candidate)
    (if (or (member candidate food) (member candidate snake))
        (new-food food snake)
        candidate))
  (new-food-check (posn (add1 (random (sub1 WIDTH))) (add1 (random (sub1 HEIGHT))))))

; update-food: ate snake food -> food
;   Eat pellet where snake is and add a new pellet
(define (update-food ate snake food)
  (if ate (cons (new-food food snake) (remove (first snake) food)) food))

; tock: World -> World
;   move snake (growing if grow), eat food & replace, update grow
(define (tock w)
  (define dir (game-dir w))
  (define snake (game-snake w))
  (define food (game-food w))
  (define grow (game-grow w))
  (define ate (is-eating? snake food))
  (game
   dir
   (move-snake snake dir grow)
   (update-food ate snake food)
   (update-grow ate grow)))

;; Game end functions

; snake-crash? Snake -> Boolean
;   Did snake head crash into body of snake?
(define (snake-crash? snake) 
  (if (member (first snake) (rest snake)) #t #f))

; wall-crash?: Pos -> Boolean
;   Did snake head crash into wall?
(define (wall-crash? pos)
  (or (<= (posn-x pos) 0)
      (>= (posn-x pos) WIDTH)
      (<= (posn-y pos) 0)
      (>= (posn-y pos) HEIGHT)))

; end?: World -> Boolean
;   End game on crash into wall or snake head crashes into body of snake
(define (end? w)
  (define snake (game-snake w))
  (or (wall-crash? (first snake)) (snake-crash? snake)))

; show-crash: Game -> Image
;   Show game-end crash message
(define (show-crash game)
  (define score (number->string (length (game-snake game))))
  (place-image/align
   (text (string-append "Score: " score) (* 1.5 SCALE) "black")
   SCALE (- (* HEIGHT SCALE) SCALE) "left" "bottom" (render game)))

;; Generate new game state

; new-snake: startx starty num -> Snake
;    Make snake going right from this point
(define (new-snake x y n)
  (build-list n (λ (dx) (posn (- x dx) y)))) 

; new-foods: food# -> Foods
;   Make # of random new foods
(define (new-foods n food snake)
  (build-list n (λ (x) (new-food food snake))))

; new-game: #food #snake -> game
(define (new-game food# snake#)
  (define my-new-snake (new-snake (quotient WIDTH 2) (quotient HEIGHT 2) snake#))
  (game
   "right"
   my-new-snake
   (new-foods food# my-new-snake '())
   GROW))

;; Main functions

; snake: Game -> Game
;   Play snake game
(define (snake speed)
  (big-bang (new-game 5 3)
    [to-draw render]
    [on-tick tock speed]
    [on-key keyhandler]
    [name "Snake"]
    [stop-when end? show-crash]))

(snake .1)

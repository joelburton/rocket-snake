#lang racket
(require 2htdp/universe 2htdp/image rackunit)

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
  (foldl (λ (pos next) (place-image SNAKE (* SCALE (posn-x pos)) (* SCALE (posn-y pos)) next))
         scene
         snake)) 

(check-equal? (render-snake (list (posn 5 5) (posn 5 6)) SCENE)
              (place-image SNAKE (* SCALE 5) (* SCALE 5) (place-image SNAKE (* SCALE 5) (* SCALE 6) SCENE)))


; render-food: Food Chain -> Image
;    Recursively renders food dots onto passed-in-scene
(define (render-food food scene)
  (foldl (λ (pos next) (place-image FOOD (* SCALE (posn-x pos)) (* SCALE (posn-y pos)) next))
         scene
         food))

(check-equal? (render-food (list (posn 5 5) (posn 5 6)) SCENE)
              (place-image FOOD (* SCALE 5) (* SCALE 5) (place-image FOOD (* SCALE 5) (* SCALE 6) SCENE)))


; render: World -> Image
;    Render all: food and snake on top of SCENE
(define (render w)
  (render-food (game-food w) (render-snake (game-snake w) SCENE)))

(check-equal? (render (game "up" (list (posn 5 5)) (list (posn 4 4)) 0))
              (render-food (list (posn 4 4)) (render-snake (list (posn 5 5)) SCENE)))

;; Key handling & direction changing
;;   this stuff doesn't actually move snake -- snake moves on each tick; the key handling
;;   is to change the *direction it will move*, not move it


; new-direction: Key Dir -> Dir
;    Decide new direction for snake: can't change 180 degrees
(define (new-direction key dir)
  (cond
    [(and (key=? key "up") (not (string=? dir "down"))) "up"]
    [(and (key=? key "down") (not (string=? dir "up"))) "down"]
    [(and (key=? key "left") (not (string=? dir "right"))) "left"]
    [(and (key=? key "right") (not (string=? dir "left"))) "right"]
    [else dir]))

(check-equal? (new-direction "up" "left") "up")
(check-equal? (new-direction "up" "down") "down")
(check-equal? (new-direction "down" "left") "down")
(check-equal? (new-direction "down" "up") "up")
(check-equal? (new-direction "left" "up") "left")
(check-equal? (new-direction "left" "right") "right")
(check-equal? (new-direction "right" "up") "right")
(check-equal? (new-direction "right" "left") "left")

    
; keyhander: World Key -> Posn
;    Handle keys:
;      up/down/left/right: change direction
(define (keyhandler w key)
  (if (member key '("up" "down" "left" "right"))
      (struct-copy game w [dir (new-direction key (game-dir w))])
      w))

(test-begin (define sn '()) (define food '())
            (check-equal? (keyhandler (game "up" sn food 0) "left") (game "left" sn food 0))
            (check-equal? (keyhandler (game "up" sn food 0) "right") (game "right" sn food 0))
            (check-equal? (keyhandler (game "left" sn food 0) "up") (game "up" sn food 0))
            (check-equal? (keyhandler (game "left" sn food 0) "down") (game "down" sn food 0))
            (check-equal? (keyhandler (game "up" sn food 0) " ") (game "up" sn food 0)))


;; Moving Snake
;;   the snake is a list of points, "moving it" is adding a new head in the right direction and,
;;   if the snake isn't supposed to be growing, to truncate the tail


; truncate: Seq -> Seq
;    Truncate last item from list
(define (truncate seq)
  (reverse (cdr (reverse seq))))

(check-equal? (truncate '(1 2 3)) '(1 2))


; move-snake: Snake Dir Grow -> Snake
;   Add move snake in given direction; if not growing, truncate end
(define (move-snake snake dir grow)
  (define (_move dx dy) (posn (+ (posn-x (first snake)) dx) (+ (posn-y (first snake)) dy)))
  (cons
   (cond
     [(string=? dir "up") (_move 0 -1)]
     [(string=? dir "down") (_move 0 +1)]
     [(string=? dir "left") (_move -1 0)]
     [(string=? dir "right") (_move +1 0)])
   (if (positive? grow) snake (truncate snake))))

(test-begin (define sn (list (posn 5 5)))
            (check-equal? (move-snake sn "up" 0) (list (posn 5 4)))
            (check-equal? (move-snake sn "down" 0) (list (posn 5 6)))
            (check-equal? (move-snake sn "left" 0) (list (posn 4 5)))
            (check-equal? (move-snake sn "right" 0) (list (posn 6 5)))
            (check-equal? (move-snake sn "right" 1) (list (posn 6 5) (posn 5 5))))

;; Food-eating functions
;;   snake eats a pellet if head collides with it
;;   on eating a pellet, add a new pllet to board


; is-eating? snake food -> Boolean
;   Is the snake eating a food pellet?
(define (is-eating? snake food)
  (if (member (first snake) food) #t #f))

(test-begin (define sn (list (posn 5 5) (posn 5 4)))
            (check-true (is-eating? sn (list (posn 5 5) (posn 10 10))))
            (check-false (is-eating? sn (list (posn 11 11) (posn 10 10))))
            (check-true (is-eating? sn (list (posn 10 10) (posn 5 5)))))


;; Game Update functions -- called on every tick and handle parts of state


; update-grow: ate grow -> grow
;    Always decreate grow, and, if ate, increase by GROW. Don't go below 0.
(define (update-grow ate grow)
  (max 0 (+ (if ate GROW 0) (sub1 grow))))

(check-equal? (update-grow #f 2) 1)
(check-equal? (update-grow #f 0) 0)
(check-equal? (update-grow #t 0) (- GROW 1))


; new-food: food snake -> posn
;   Generate a random food pellet, avoiding overlapping with existing food or snake
(define (new-food food snake)
  (define (new-food-check candidate)
    (if (or (member candidate food) (member candidate snake))
        (new-food food snake)
        candidate))
  (new-food-check (posn (add1 (random (sub1 WIDTH))) (add1 (random (sub1 HEIGHT))))))

(check-pred posn? (new-food '() '()))


; update-food: ate snake food -> food
;   Eat pellet where snake is and add a new pellet
(define (update-food ate snake food)
  (if ate (cons (new-food food snake) (remove (first snake) food)) food))

(test-begin (define sn (list (posn 5 5))) (define food (list (posn 5 5) (posn 10 10)))
            (check-match (update-food #t sn food) (list _ (posn 10 10)))
            (check-equal? (update-food #f sn food) food))


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

(check-true (snake-crash? (list (posn 5 5) (posn 5 5))))
(check-false (snake-crash? (list (posn 5 5) (posn 5 4))))


; wall-crash?: Pos -> Boolean
;   Did snake head crash into wall?
(define (wall-crash? pos)
  (or (<= (posn-x pos) 0)
      (>= (posn-x pos) WIDTH)
      (<= (posn-y pos) 0)
      (>= (posn-y pos) HEIGHT)))

(check-false (wall-crash? (posn 1 1)))
(check-true (wall-crash? (posn 0 1)))
(check-true (wall-crash? (posn 1 0)))
(check-true (wall-crash? (posn WIDTH 1)))
(check-true (wall-crash? (posn 1 HEIGHT)))


; end?: World -> Boolean
;   End game on crash into wall or snake head crashes into body of snake
(define (end? w)
  (define snake (game-snake w))
  (or (wall-crash? (first snake)) (snake-crash? snake)))

(check-false (end? (game "up" (list (posn 5 5)) '() 0)))
(check-true (end? (game "up" (list (posn 0 0)) '() 0)))
(check-true (end? (game "up" (list (posn 5 5) (posn 5 5)) '() 0)))


; show-crash: Game -> Image
;   Show game-end crash message
(define (show-crash game)
  (define score (number->string (length (game-snake game))))
  (place-image/align
   (text (string-append "Score: " score) (* 1.5 SCALE) "black")
   SCALE (- (* HEIGHT SCALE) SCALE) "left" "bottom" (render game)))

(test-begin (define sample-game (game "up" '() '() 0))
            (check-equal? (show-crash sample-game)
                          (place-image/align (text "Score: 0" (* 1.5 SCALE) "black")
                                             SCALE (- (* HEIGHT SCALE) SCALE) "left" "bottom"
                                             (render sample-game))))



;; Generate new game state


; new-snake: startx starty num -> Snake
;    Make snake going right from this point
(define (new-snake x y n)
  (build-list n (λ (dx) (posn (- x dx) y)))) 

(check-equal? (new-snake 5 5 3) (list (posn 5 5) (posn 4 5) (posn 3 5)))



; new-foods: food# -> Foods
;   Make # of random new foods
(define (new-foods n food snake)
  (build-list n (λ (x) (new-food food snake))))

(check-match (new-foods 2 '() '()) (list _ _))


; new-game: #food #snake -> game
(define (new-game food# snake#)
  (define my-new-snake (new-snake (quotient WIDTH 2) (quotient HEIGHT 2) snake#))
  (game
   "right"
   my-new-snake
   (new-foods food# my-new-snake '())
   GROW))

(check-equal? (new-game 0 1) (game "right" (list (posn 15 15)) '() GROW))


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
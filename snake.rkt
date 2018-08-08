;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
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
;   snake = (list (make-posn x y) (make-posn x y) ...)   [head is start]
;   food = (list (make-posn x y) (make-posn x y) ...)
;   grow = # of ticks where snake should grow (goes down each tick, up each pellet eaten)

(define-struct game [dir snake food grow])


;; Rendering Functions


; render-snake-dot: posn Image -> Image
;   Renders a single xy posn of snake on top of passed-in-scene
(check-expect (render-snake-dot (make-posn 5 5) SCENE) (place-image SNAKE 100 100 SCENE))
(define (render-snake-dot pos scene)
  (place-image SNAKE (* SCALE (posn-x pos)) (* SCALE (posn-y pos)) scene))


; render-snake: Snake -> Image
;   Recursively renders snake onto passed-in-scene
(check-expect (render-snake (list (make-posn 5 5) (make-posn 5 6)) SCENE)
              (render-snake-dot (make-posn 5 5) (render-snake-dot (make-posn 5 6) SCENE)))
(define (render-snake snake scene)
  (cond
    [(empty? snake) scene]
    [else (render-snake-dot (first snake) (render-snake (rest snake) scene))]))


; render-food-dot: Food Dot -> Image
;   Renders a single xy posn of food onto passed-in-scene
(check-expect (render-food-dot (make-posn 5 5) SCENE) (place-image FOOD 100 100 SCENE))
(define (render-food-dot pos scene)
  (place-image FOOD (* SCALE (posn-x pos)) (* SCALE (posn-y pos)) scene))


; render-food: Food Chain -> Image
;    Recursively renders food dots onto passed-in-scene
(check-expect (render-food (list (make-posn 5 5) (make-posn 5 6)) SCENE)
              (render-food-dot (make-posn 5 5) (render-food-dot (make-posn 5 6) SCENE)))
(define (render-food food scene)
  (cond
    [(empty? food) scene]
    [else (render-food-dot (first food) (render-food (rest food) scene))]))


; render: Game -> Image
;    Render all: food and snake on top of SCENE
(check-expect (render (make-game "up" (list (make-posn 5 5)) (list (make-posn 4 4)) 0))
              (render-food (list (make-posn 4 4)) (render-snake (list (make-posn 5 5)) SCENE)))
(define (render game)
  (render-food (game-food game) (render-snake (game-snake game) SCENE)))


;; Key handling & direction changing
;;   this stuff doesn't actually move snake -- snake moves on each tick; the key handling
;;   is to change the *direction it will move*, not move it


; new-direction: Key Dir -> Dir
;    Decide new direction for snake: can't change 180 degrees
(check-expect (new-direction "up" "left") "up")
(check-expect (new-direction "up" "down") "down")
(check-expect (new-direction "down" "left") "down")
(check-expect (new-direction "down" "up") "up")
(check-expect (new-direction "left" "up") "left")
(check-expect (new-direction "left" "right") "right")
(check-expect (new-direction "right" "up") "right")
(check-expect (new-direction "right" "left") "left")
(define (new-direction key dir)
  (cond
    [(and (key=? key "up") (not (string=? dir "down"))) "up"]
    [(and (key=? key "down") (not (string=? dir "up"))) "down"]
    [(and (key=? key "left") (not (string=? dir "right"))) "left"]
    [(and (key=? key "right") (not (string=? dir "left"))) "right"]
    [else dir]))

    
; keyhander: Game Key -> Posn
;    Handle keys:
;      up/down/left/right: change direction
(check-expect (keyhandler (make-game "up" (list (make-posn 5 5)) (list (make-posn 4 4)) 0) "left")
              (make-game "left" (list (make-posn 5 5)) (list (make-posn 4 4)) 0))
(check-expect (keyhandler (make-game "up" (list (make-posn 5 5)) (list (make-posn 4 4)) 0) "right")
              (make-game "right" (list (make-posn 5 5)) (list (make-posn 4 4)) 0))
(check-expect (keyhandler (make-game "left" (list (make-posn 5 5)) (list (make-posn 4 4)) 0) "up")
              (make-game "up" (list (make-posn 5 5)) (list (make-posn 4 4)) 0))
(check-expect (keyhandler (make-game "left" (list (make-posn 5 5)) (list (make-posn 4 4)) 0) "down")
              (make-game "down" (list (make-posn 5 5)) (list (make-posn 4 4)) 0))
(check-expect (keyhandler (make-game "up" (list (make-posn 5 5)) (list (make-posn 4 4)) 0) " ")
              (make-game "up" (list (make-posn 5 5)) (list (make-posn 4 4)) 0))
(define (keyhandler game key)
  (cond
    [(or (key=? key "up") (key=? key "down") (key=? key "left") (key=? key "right"))
     (make-game (new-direction key (game-dir game))
                (game-snake game)
                (game-food game)
                (game-grow game))]
    [else game]))


;; Moving Snake
;;   the snake is a list of points, "moving it" is adding a new head in the right direction and,
;;   if the snake isn't supposed to be growing, to truncate the tail


; truncate: Seq -> Seq
;    Truncate last item from list
(check-expect (truncate (list 1 2 3)) (list 1 2))
(define (truncate seq)
  (cond
    [(empty? (rest seq)) '()]
    [else (cons (first seq) (truncate (rest seq)))]))


; move-up: Snake -> posn
;   Get new head about current head
(check-expect (move-up (list (make-posn 10 10) (make-posn 10 11))) (make-posn 10 9))
(define (move-up snake)
  (make-posn (posn-x (first snake)) (sub1 (posn-y (first snake))))) 
  

; move-down: Snake -> posn
;   Get new head below current head
(check-expect (move-down (list (make-posn 10 10) (make-posn 10 9))) (make-posn 10 11))
(define (move-down snake)
  (make-posn (posn-x (first snake)) (add1 (posn-y (first snake)))))


; move-left: Snake -> posn
;   Get new head left of current head
(check-expect (move-left (list (make-posn 10 10) (make-posn 10 9))) (make-posn 9 10))
(define (move-left snake)
  (make-posn (sub1 (posn-x (first snake))) (posn-y (first snake))))


; move-right: Snake -> posn
;   Get new head right of current head
(check-expect (move-right (list (make-posn 10 10) (make-posn 10 9))) (make-posn 11 10))
(define (move-right snake)
  (make-posn (add1 (posn-x (first snake))) (posn-y (first snake))))


; move-snake: Snake Dir Grow -> Snake
;   Add move snake in given direction; if not growing, truncate end
(check-expect (move-snake (list (make-posn 5 5) (make-posn 5 6)) "up" 0)
              (list (make-posn 5 4) (make-posn 5 5)))
(check-expect (move-snake (list (make-posn 5 5) (make-posn 5 4)) "down" 0)
              (list (make-posn 5 6) (make-posn 5 5)))
(check-expect (move-snake (list (make-posn 5 5) (make-posn 5 6)) "left" 0)
              (list (make-posn 4 5) (make-posn 5 5)))
(check-expect (move-snake (list (make-posn 5 5) (make-posn 5 6)) "right" 0)
              (list (make-posn 6 5) (make-posn 5 5)))
(check-expect (move-snake (list (make-posn 5 5) (make-posn 5 6)) "right" 1)
              (list (make-posn 6 5) (make-posn 5 5) (make-posn 5 6)))
(define (move-snake snake dir grow)
  (cons
   (cond
     [(string=? dir "up") (move-up snake)]
     [(string=? dir "down") (move-down snake)]
     [(string=? dir "left") (move-left snake)]
     [(string=? dir "right") (move-right snake)])
   (if (positive? grow) snake (truncate snake))))


;; Food-eating functions
;;   snake eats a pellet if head collides with it
;;   on eating a pellet, add a new pllet to board


; is-eating? snake food -> Boolean
;   Is the snake eating a food pellet?
(check-expect (is-eating? (list (make-posn 5 5) (make-posn 5 4))
                          (list (make-posn 5 5) (make-posn 10 10))) #true)
(check-expect (is-eating? (list (make-posn 5 4) (make-posn 5 4))
                          (list (make-posn 5 5) (make-posn 10 10))) #false)
(check-expect (is-eating? (list (make-posn 5 5) (make-posn 5 4))
                          (list (make-posn 10 10) (make-posn 5 5))) #true)
(define (is-eating? snake food)
  (member? (first snake) food))




;; Game Update functions -- called on every tick and handle parts of state


; update-grow: ate grow -> grow
;    Always decreate grow, and, if ate, increase by GROW. Don't go below 0.
(check-expect (update-grow #false 2) 1)
(check-expect (update-grow #false 0) 0)
(check-expect (update-grow #true 0) (- GROW 1))
(define (update-grow ate grow)
  (max 0 (+ (if ate GROW 0) (sub1 grow))))


; update-food: ate snake food -> food
;   Eat pellet where snake is and add a new pellet
(check-random (update-food #true (list (make-posn 5 5)) (list (make-posn 5 5) (make-posn 10 10)))
              (list (new-food (list (make-posn 5 5)) (list (make-posn 5 5) (make-posn 10 10))) (make-posn 10 10)))
(check-expect (update-food #false (list (make-posn 5 5)) (list (make-posn 4 4) (make-posn 10 10)))
              (list (make-posn 4 4) (make-posn 10 10)))
(define (update-food ate snake food)
  (if ate (cons (new-food food snake) (remove-all (first snake) food)) food))


; update-game: ate dir snake food score grow -> game
;   move snake (growing if grow), eat food & replace, update grow
(check-expect (update-game #false "up" (list (make-posn 5 5)) '() 0)
              (make-game "up" (list (make-posn 5 4)) '() 0)) 
(define (update-game ate dir snake food grow)
  (make-game
   dir
   (move-snake snake dir grow)
   (update-food ate snake food)
   (update-grow ate grow)))


; tock: Game -> Game
(check-expect (tock (make-game "up" (list (make-posn 5 5)) '() 0))
                    (update-game #false "up" (list (make-posn 5 5)) '() 0))
(define (tock game)
  (update-game (is-eating? (game-snake game) (game-food game))
               (game-dir game)
               (game-snake game)
               (game-food game)
               (game-grow game)))


;; Game end functions


; snake-crash? Snake -> Boolean
;   Did snake head crash into body of snake?
(check-expect (snake-crash? (list (make-posn 5 5) (make-posn 5 5))) #true)
(check-expect (snake-crash? (list (make-posn 5 5) (make-posn 5 4))) #false)
(define (snake-crash? snake) 
  (member? (first snake) (rest snake)))


; wall-crash?: Pos -> Boolean
;   Did snake head crash into wall?
(check-expect (wall-crash? (make-posn 1 1)) #false)
(check-expect (wall-crash? (make-posn 0 1)) #true)
(check-expect (wall-crash? (make-posn 1 0)) #true)
(check-expect (wall-crash? (make-posn WIDTH 1)) #true)
(check-expect (wall-crash? (make-posn 1 HEIGHT)) #true)
(define (wall-crash? pos)
  (or (<= (posn-x pos) 0)
      (>= (posn-x pos) WIDTH)
      (<= (posn-y pos) 0)
      (>= (posn-y pos) HEIGHT)))


; end?: Game -> Boolean
;   End game on crash into wall or snake head crashes into body of snake
(check-expect (end? (make-game "up" (list (make-posn 5 5)) '() 0)) #false)
(check-expect (end? (make-game "up" (list (make-posn 0 0)) '() 0)) #true)
(check-expect (end? (make-game "up" (list (make-posn 5 5) (make-posn 5 5)) '() 0)) #true)
(define (end? game)
  (or (wall-crash? (first (game-snake game)))
      (snake-crash? (game-snake game))))


; show-crash: Game -> Image
;   Show game-end crash message
(define sample-game (make-game "up" '() '() 0))
(check-expect (show-crash sample-game)
              (place-image/align (text "Score: 0" (* 1.5 SCALE) "black")
                                 SCALE (- (* HEIGHT SCALE) SCALE) "left" "bottom"
                                 (render sample-game)))
(define (show-crash game)
  (place-image/align
   (text (string-append "Score: " (number->string (length (game-snake game)))) (* 1.5 SCALE) "black")
   SCALE (- (* HEIGHT SCALE) SCALE) "left" "bottom" (render game)))



;; Generate new game state


; new-snake: startx starty num -> Snake
;    Make snake going right from this point
(check-expect (new-snake 5 5 3) (list (make-posn 5 5) (make-posn 4 5) (make-posn 3 5)))
(define (new-snake x y n)
  (cond
    [(zero? n) '()]
    [else (cons (make-posn x y) (new-snake (sub1 x) y (sub1 n)))]))


; new-food: food snake -> posn
;   Generate a random food pellet, avoiding overlapping with existing food or snake
(check-random (new-food '() '()) (new-food '() '()))
(define (new-food food snake)
  (new-food-check food
                  snake
                  (make-posn (add1 (random (sub1 WIDTH))) (add1 (random (sub1 HEIGHT))))))


; new-food-check: food snake proposed-new-posn
;   Generative recursion - checks if candidate is in existing food or snake,
;   generates another candidate to attempt
(define (new-food-check food snake candidate)
  (if (or (member? candidate food) (member? candidate snake))
      (new-food food snake)
      candidate))


; new-foods: food# -> Foods
;   Make # of random new foods
(check-random (new-foods 2 '() '()) (list (new-food '() '()) (new-food '() '())))
(define (new-foods n food snake)
  (cond
    [(zero? n) '()]
    [else (cons (new-food food snake) (new-foods (sub1 n) food snake))]))


; new-game-from-snake: snake food# -> game
;    make a new game from a snake
;    this has to be split from new-game since that needs to make the snake
;    which is used here twice; once for the snake and once to exclude the snake
;    from the random food.
(check-expect (new-game-from-snake (list (make-posn 5 5)) 0)
              (make-game "right" (list (make-posn 5 5)) '() GROW))
(define (new-game-from-snake snake food#)
  (make-game
   "right"
   snake
   (new-foods food# snake '())
   GROW))


; new-game: #food #snake -> game
(check-expect (new-game 0 1)
              (make-game "right" (list (make-posn 15 15)) '() GROW))
(define (new-game food# snake#)
  (new-game-from-snake (new-snake (quotient WIDTH 2) (quotient HEIGHT 2) snake#) food#))


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
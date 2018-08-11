#lang racket
(require rackunit)
(load "snake")

(check-equal? (render-snake (list (posn 5 5) (posn 5 6)) SCENE)
              (place-image SNAKE (* SCALE 5) (* SCALE 5) (place-image SNAKE (* SCALE 5) (* SCALE 6) SCENE)))

(check-equal? (render-food (list (posn 5 5) (posn 5 6)) SCENE)
              (place-image FOOD (* SCALE 5) (* SCALE 5) (place-image FOOD (* SCALE 5) (* SCALE 6) SCENE)))

(check-equal? (render (game "up" (list (posn 5 5)) (list (posn 4 4)) 0))
              (render-food (list (posn 4 4)) (render-snake (list (posn 5 5)) SCENE)))

(check-equal? (new-direction "up" "left") "up")
(check-equal? (new-direction "up" "down") "down")
(check-equal? (new-direction "down" "left") "down")
(check-equal? (new-direction "down" "up") "up")
(check-equal? (new-direction "left" "up") "left")
(check-equal? (new-direction "left" "right") "right")
(check-equal? (new-direction "right" "up") "right")
(check-equal? (new-direction "right" "left") "left")

(test-begin (define sn '()) (define food '())
            (check-equal? (keyhandler (game "up" sn food 0) "left") (game "left" sn food 0))
            (check-equal? (keyhandler (game "up" sn food 0) "right") (game "right" sn food 0))
            (check-equal? (keyhandler (game "left" sn food 0) "up") (game "up" sn food 0))
            (check-equal? (keyhandler (game "left" sn food 0) "down") (game "down" sn food 0))
            (check-equal? (keyhandler (game "up" sn food 0) " ") (game "up" sn food 0)))

(check-equal? (truncate '(1 2 3)) '(1 2))

(test-begin (define sn (list (posn 5 5)))
            (check-equal? (move-snake sn "up" 0) (list (posn 5 4)))
            (check-equal? (move-snake sn "down" 0) (list (posn 5 6)))
            (check-equal? (move-snake sn "left" 0) (list (posn 4 5)))
            (check-equal? (move-snake sn "right" 0) (list (posn 6 5)))
            (check-equal? (move-snake sn "right" 1) (list (posn 6 5) (posn 5 5))))

(test-begin (define sn (list (posn 5 5) (posn 5 4)))
            (check-true (is-eating? sn (list (posn 5 5) (posn 10 10))))
            (check-false (is-eating? sn (list (posn 11 11) (posn 10 10))))
            (check-true (is-eating? sn (list (posn 10 10) (posn 5 5)))))

(check-equal? (update-grow #f 2) 1)
(check-equal? (update-grow #f 0) 0)
(check-equal? (update-grow #t 0) (- GROW 1))

(check-pred posn? (new-food '() '()))

(test-begin (define sn (list (posn 5 5))) (define food (list (posn 5 5) (posn 10 10)))
            (check-match (update-food #t sn food) (list _ (posn 10 10)))
            (check-equal? (update-food #f sn food) food))

(check-true (snake-crash? (list (posn 5 5) (posn 5 5))))
(check-false (snake-crash? (list (posn 5 5) (posn 5 4))))

(check-false (wall-crash? (posn 1 1)))
(check-true (wall-crash? (posn 0 1)))
(check-true (wall-crash? (posn 1 0)))
(check-true (wall-crash? (posn WIDTH 1)))
(check-true (wall-crash? (posn 1 HEIGHT)))

(check-false (end? (game "up" (list (posn 5 5)) '() 0)))
(check-true (end? (game "up" (list (posn 0 0)) '() 0)))
(check-true (end? (game "up" (list (posn 5 5) (posn 5 5)) '() 0)))

(test-begin (define sample-game (game "up" '() '() 0))
            (check-equal? (show-crash sample-game)
                          (place-image/align (text "Score: 0" (* 1.5 SCALE) "black")
                                             SCALE (- (* HEIGHT SCALE) SCALE) "left" "bottom"
                                             (render sample-game))))

(check-equal? (new-snake 5 5 3) (list (posn 5 5) (posn 4 5) (posn 3 5)))

(check-match (new-foods 2 '() '()) (list _ _))

(check-equal? (new-game 0 1) (game "right" (list (posn 15 15)) '() GROW))

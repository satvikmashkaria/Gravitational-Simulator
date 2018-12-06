#lang racket

(require 2htdp/universe)
(require 2htdp/image)

;structures

(struct particle (mass posn velocity) #:transparent)
(struct tree (mass posn velocity subtrees) #:transparent)
(struct area (lx ly rx ry) #:transparent)
(struct vec (x y) #:transparent)

;Global definations

(define e 1)
(define limit 2)     
(define g 170.64)   
(define timescale 0.01)

;;;; Test Cases Helper functions

(define (testcase-generator-x  y)
  (define (helper x n l)
    (cond [(= n 0) l]
          [else (helper (+ x 30) (- n 1) (cons (particle 50 (vec x y) (vec 0 0)) l))]))
  (helper 10 50 '()))

(define (tcm n r)
  (define a (/ (* 2 pi) n))
  (define x0 500)
  (define y0 500)
  (define (loop count ans)
    (cond [(= count n) ans]
          [else (loop (+ 1 count) (cons (particle 500 (vec (+ x0 (* r (cos (* count a)))) (+ y0 (* r (sin (* count a))))) (vec 0 0)) ans))]))
  (loop 0 null))
;;;;;;Testcases

(define periodic-ellipse (list (particle 10000 (vec 500 500) (vec 0 0)) (particle 10 (vec 700 500) (vec 0 100))))
(define 3particle-collision (tcm 3 100))
(define 3particle-movement (list (particle 1000 (vec 500 500) (vec 0 0)) (particle 1000 (vec 580 500) (vec 0 0)) (particle 20000 (vec 1000 500) (vec 0 0))))
(define testcase12 (list (particle 10000 (vec 500 500) (vec 0 0)) (particle 10 (vec 700 500) (vec 0 75))))
(define 2particle-collision (list (particle 500 (vec 500 500) (vec 0 0)) (particle 500 (vec 700 500) (vec 0 0))))
(define 4particle-periodic (list (particle 1000 (vec 700 300) (vec 40.414 0))
                                 (particle 1000 (vec 800 400) (vec 0 40.414))
                                 (particle 1000 (vec 600 400) (vec 0 -40.414))
                                 (particle 1000 (vec 700 500) (vec -40.414 0))))
(define eight (list (particle 50 (vec 500 500) (vec -93.24 -86.47)) (particle 50 (vec 402.96 524.30) (vec 46.62 43.235)) 
                    (particle 50 (vec 597 475.692) (vec 46.62 43.235))))
(define 50p (list (particle 50 (vec 18 20) (vec 11 7)) (particle 50 (vec 97 20) (vec 14 -3)) (particle 50 (vec 200 35) (vec 11 7))
                  (particle 50 (vec 280 42) (vec 6  15)) (particle 50 (vec 370 60 ) (vec 11 -6)) (particle 50 (vec 450 60) (vec 11 7))
                  (particle 50 (vec 540 45) (vec 0 0)) (particle 50 (vec 620 85) (vec 0 0)) (particle 34 (vec 800 90) (vec -20 11))
                  (particle 80 (vec 950 85) (vec -32 0)) (particle 40 (vec 1200 85) (vec 0 0)) (particle 50 (vec 25 200) (vec 0 0))
                  (particle 50 (vec 100  175) (vec 11 17)) (particle 30  (vec 230  156) (vec 3 4))
                  (particle 30  (vec 330  156) (vec 3 4)) (particle 30  (vec 550  196) (vec 13 14))
                  (particle 65  (vec 750  180) (vec 7 19)) (particle 30  (vec 900  196) (vec -26 0))
                  (particle 30  (vec 230  156) (vec 3 4)) (particle 87  (vec 1200  156) (vec 0 0))
                  (particle 30  (vec 120  400) (vec 3 4)) (particle 30  (vec 240  426) (vec 0 0))
                  (particle 30  (vec 360  390) (vec 0 0)) (particle 30  (vec 480  426) (vec 7 11))
                  (particle 100 (vec 700 460) (vec -11 -1 )) (particle 75  (vec 850 424) (vec 0 0))
                  (particle 30  (vec 1200  426) (vec 0 0)) (particle 88  (vec 1350  396) (vec -5 -6))
                  (particle 34 (vec 200 600) (vec 0 0)) (particle 900  (vec 350 540) (vec 1 1))
                  (particle 80  (vec 500  560) (vec 5 5)) (particle 80  (vec 600  560) (vec -5 5))
                  (particle 80  (vec 690  560) (vec -8 5)) (particle 40  (vec 800  560) (vec -1 5))
                  (particle 80  (vec 900  560) (vec 0 -5)) (particle 45  (vec 1200  560) (vec -5 -5))
                  (particle 80  (vec 1300  560) (vec 5 5)) (particle 20  (vec 1380  560) (vec 5 5))
                  (particle 40 (vec 123  700) (vec 7 7)) (particle 60 (vec 323  700) (vec 7 7))
                  (particle 40 (vec 453  700) (vec -7 -7)) (particle 60  (vec 600  700) (vec -7 -7))
                  (particle 40 (vec 700  700) (vec -3 -8)) (particle 75 (vec 780  700) (vec -7 -7))
                  (particle 100 (vec 900  700) (vec -11 -40)) (particle 42 (vec 1200  700) (vec -3 -5))))
(define (func str)
  (cond [(equal? str "1") 3particle-collision]
        [(equal? str "2") 2particle-collision]
        [(equal? str "3") 3particle-movement]
        [(equal? str "4") 4particle-periodic]
        [(equal? str "5") 3particle-movement]
        [(equal? str "6") periodic-ellipse]
        [(equal? str "7") 50p]))
(displayln "Test cases Available")
(displayln "1. 3particle-collision")
(displayln "2. 2particle-collision")
(displayln "3. 3particle-movement ")
(displayln "4. 4particle-periodic ")
(displayln "5. 3particle-movement ")
(displayln "6. periodic-ellipse ")
(displayln "7. 50particles")

(newline)
(define state (func (read)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))
(define (zipwith f l1 l2)
  (cond [(or (null? l1) (null? l2)) null]
        [else (cons (f (car l1) (car l2)) (zipwith f (cdr l1) (cdr l2)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;helper functions

(define (zip f l1 l2)
  (cond [(or (null? l1) (null? l2)) null]
        [else (cons (f (car l1) (car l2)) (zip f (cdr l1) (cdr l2)))]))

(define (vecsum l)
  (foldl add (vec 0 0) l))

(define (sum l)
  (foldl + 0 l))

(define (add v1 v2)
  (vec (+ (vec-x v1) (vec-x v2)) (+ (vec-y v1) (vec-y v2))))

(define (prod l1 l2)
  (cond [(null? l1) 0]
        [else (+ (* (car l1) (car l2)) (prod (cdr l1) (cdr l2)))]))

(define (same p1 p2)
  (equal? (particle-posn p1) (particle-posn p2)))

(define (takesum lst vel)
  (cond [(not (member (vec 0 0) lst)) (vecsum lst)]
        [else (let* ([lst1 (lc x : x <- lst @ (not (equal? (vec 0 0) x)))])
                (add vel (vecsum lst1)))]))

(define (checker l)
  (define k (map (lambda(x)(vec-x x)) l))
  (sum k))

(define (** a v)
  (vec (* a (vec-x v)) (* a (vec-y v))))

(define k 0)
(define (change vel)  ;;;;;remove this function at end
  (vec (-  (vec-x vel) (* k (vec-x vel))) (- (vec-y vel) (* k (vec-y vel)))))

;;Below function is used in debugging the code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (mydefine-h p)
  (display (particle-mass p))
  (display " ")
  (display (vec-x (particle-posn p)))
  (display " ")
  (display (vec-y (particle-posn p)))
  (display " ")
  (display (vec-x (particle-velocity p)))
  (display " ")
  (display (vec-y (particle-velocity p)))
  (newline))
(define (mydisplay ps)
  (define (helper count ps1)
    (cond [(< count (+ 1 (length ps))) (mydefine-h (car ps1)) (helper (+ 1 count) (cdr ps1))]))
  (helper 1 ps))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Collision Code

(define (collision particle tree)
  (let* ([x1 (vec-x (particle-posn particle))]
         [y1 (vec-y (particle-posn particle))]
         [x2 (vec-x (tree-posn tree))]
         [y2 (vec-y (tree-posn tree))]
         [sin (/ (- y2 y1) (distance particle tree))]
         [cos (/ (- x2 x1) (distance particle tree))]     
         [vh1 (vec-x (particle-velocity particle))]
         [vh2 (vec-y (particle-velocity particle))]
         [vh3 (* -1 (vec-x (tree-velocity tree)))]
         [vh4 (* -1 (vec-y (tree-velocity tree)))]
         [m1 (particle-mass particle)]
         [m2 (tree-mass tree)]
         [v1 (+ (* vh1 cos) (* vh2 sin))]
         [v2 (- (* vh2 cos) (* vh1 sin))]
         [v3 (+ (* vh3 cos) (* vh4 sin))]
         [v4 (- (* vh4 cos) (* vh3 sin))]
         [a (* (* -1 e) (+ v1 v3))]
         [b (- (* m1 v1) (* m2 v3))]
         [vf1 (/ (+ a (/ b m2)) (+ (/ m1 m2)1))]
         [vf2 v2]
         [vf3 (/ (- (* a m1) b) (+ m1 m2))]
         [vf4 v4])
   
    (if (> (+ v1 v3) 0) (vec (* 1 (- (* cos vf1) (* sin vf2))) (* 1 (+ (* sin vf1) (* cos vf2)))) (vec 0 0))))
      

;;;;;Implementation of Barnes Hut algorithm 

(define (cover-area particles)
  (define x1 (myfunc particles (lambda(x) (vec-x (particle-posn x))) min))
  (define y1 (myfunc particles (lambda(x) (vec-y (particle-posn x))) min))
  (define x2 (myfunc particles (lambda(x) (vec-x (particle-posn x))) max))
  (define y2 (myfunc particles (lambda(x) (vec-y (particle-posn x))) max))
  (area (- x1 20) (- y1 20) (+ x2 20)  (+ 20 y2)))

(define (myfunc particles access min-max)
  (define lsst (map access particles))
  (foldr min-max (car lsst) lsst))




(define (com particles)                                                   ;Finds centre of mass of a list of particles
  (define mass (map (lambda(x)(particle-mass x)) particles))
  (define ini-x (map (lambda(x)(vec-x (particle-posn x))) particles))
  (define ini-y (map (lambda(x)(vec-y (particle-posn x))) particles))
  (define totalmass (sum mass))
  
  (define final-x (/ (prod mass ini-x) totalmass))
  (define final-y (/ (prod mass ini-y) totalmass))
  (particle totalmass (vec final-x final-y) (vec 0 0)))

(define (in? particle area)                       ;Checks if a particle is in given area
  (define lx (area-lx area))
  (define ly (area-ly area))
  (define rx (area-rx area))
  (define ry (area-ry area))
  (define x (vec-x (particle-posn particle)))
  (define y (vec-y (particle-posn particle)))
  (and (>= x lx) (< x rx) (>= y ly) (< y ry)))
(define (satisfying particlelist area)
  (lc x : x <- particlelist @ (in? x area)))



(define (buildTree initialArea particlelist)             ;Makes Barnes Hut tree
  (define finallist (satisfying particlelist initialArea))
  (define lx (area-lx initialArea))
  (define ly (area-ly initialArea))
  (define rx (area-rx initialArea))
  (define ry (area-ry initialArea))
  (cond
    [(null? finallist) null]
    [(or (null? (cdr finallist)) (and (= 2 (length finallist)) (same (car finallist) (last finallist)))) (tree (particle-mass (car finallist)) (particle-posn (car finallist)) (particle-velocity (car finallist)) null)]
    [else (let* ([l (com finallist)])
            (tree (particle-mass l) (particle-posn l) (particle-velocity l) (list (buildTree (area lx ly (/ (+ lx rx) 2) (/ (+ ly ry) 2)) finallist)
                                                                                  (buildTree (area (/ (+ lx rx) 2) ly rx (/ (+ ly ry) 2)) finallist)
                                                                                  (buildTree (area (/ (+ lx rx) 2) (/ (+ ly ry) 2) rx ry) finallist)
                                                                                  (buildTree (area lx (/ (+ ly ry) 2) (/ (+ lx rx) 2) ry) finallist))))]))


(define (distance p1 p2)  ;p2 is tree
  (define x1 (vec-x (particle-posn p1)))
  (define y1 (vec-y (particle-posn p1)))
  (define x2 (vec-x (tree-posn p2)))
  (define y2 (vec-y (tree-posn p2)))
  (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

(define (force p1 p2) ;p2 is tree
  (define x1 (vec-x (particle-posn p1)))
  (define y1 (vec-y (particle-posn p1)))
  (define x2 (vec-x (tree-posn p2)))
  (define y2 (vec-y (tree-posn p2)))
  (define m1 (particle-mass p1))
  (define m2 (tree-mass p2))
  (define c (* g m1 m2))
  (define d (distance p1 p2))
  (define gx (* (/ c (expt d 3)) (- x2 x1)))
  (define gy (* (/ c (expt d 3)) (- y2 y1)))
  (vec gx gy))


(define (netforce area tree particle)
  (define s (- (area-rx area) (area-lx area)))
  (define total (vec 0 0))
  (define bool #f)
  (define bool2 #f)
  (define velocity null)
  (define (helper tree)
    (cond [(or (null? tree)) (void)]
          [(null? (tree-subtrees tree)) (let*([d (distance particle tree)]
                                              [mass1 (particle-mass particle)]
                                              [mass2 (tree-mass tree)]
                                              [rad1 (* 3 (expt mass1 (/ 1 3)))]
                                              [rad2 (* 3 (expt mass2 (/ 1 3)))])
                                          (if (= d 0) (void)
                                              (if (> d (+  rad1 rad2)) (set! total (add total (force particle tree)))
                                                  (begin
                                                    (set! bool #t)
                                                    (set! velocity (append velocity (list (collision particle tree ))))))))]
          [(> (/ (distance particle tree) s) limit) (set! total (add total (force particle tree)))] 
          [else (let* ([list (tree-subtrees tree)])
                  (set! s (/ s 2))
                  (helper (car list))
                  (helper (second list))
                  (helper (third list))
                  (helper (last list)))]))
  (helper tree)
  (if bool (** (/ (particle-mass particle) timescale) (add (takesum velocity (particle-velocity particle)) (** -1 (particle-velocity particle)))) total))



(define (calcForces area tree particles)
  (define (helper x)
    (netforce area tree x))
  (map helper particles))


(define (moveparticles particlelist forces)
  (define (single particle1 force)
    (let* ([newposn (add (particle-posn particle1) (add (** timescale (particle-velocity particle1)) (** (/ (* 0.5 (* timescale timescale)) (particle-mass particle1)) force   )))]
           [newvelocity (change (add (particle-velocity particle1) (** (/ timescale (particle-mass particle1)) force)))]
           [newparticle (particle (particle-mass particle1) newposn newvelocity)])
      newparticle))
      
  (zip single particlelist forces))

(define (singlestep particles)
  (let* ([initialArea (cover-area particles)]
         [tree (buildTree initialArea particles)] 
         [forces (calcForces initialArea tree particles)])
    (set! state (moveparticles particles forces)) (moveparticles particles forces)))

;;Graphics
(define count 0)
(define time 0)
(define (sub v1 v2)
  (list (- (vec-x v1) (vec-x v2)) (- (vec-y v1) (vec-y v2))))
(define (check-pos x)
  (<= (abs x) 8))
(define (check-vel v)
  (<= (abs v) 8))
(define (percen v1 v2)
  (list (/ (- (vec-x v1) (vec-x v2)) (vec-x v2)) (/ (- (vec-y v1) (vec-y v2)) (vec-y v2))))


(define initial-state state)
(define pos-in (map particle-posn initial-state))
(define vel-in (map particle-velocity initial-state))

(define (common-checker particles access check initial) ; state = '(particle-posn sub check-pos pos-in)
  (define argument (map access particles))
  (define check-this (append* (zipwith sub argument initial)))
  (define y (map check check-this))
  (foldl (lambda (x y) (and x y)) #t y))

(define (is-same particles)
  (and (common-checker particles particle-velocity check-vel vel-in)
       (common-checker particles particle-posn check-pos pos-in)))


(define list-points '())
(define counter 1)
(define (lolipop lsst)
  (cond [(null? lsst) image]
        [else (set! image (add-line image (caar lsst) (cadar lsst) (caddar lsst) (car (cdddr(car lsst))) "white")) (lolipop (cdr lsst))]))
(define (drawing-func state)
 
 (if (= count 2) (void) (set! time (+ time 0.01)))
  (set! image (rectangle 1400 800 "solid" "black"))
  (define (init)
    (cond [(= count 0) (if (is-same state) (void) (set! count (+ count 1)))]
          [(= count 1) (if (is-same state) (set! count (+ count 1)) (void))]
          [(= count 2) (set! image (overlay (place-image (text (string-append "Motion is periodic with time period "
                                                                 (string-append (number->string (/ (floor (* 1000 time)) 1000)) "s")) 24 "yellow")
                                                         700 75
                                                         (rectangle 1400 800 "outline" "black"))
                                            image))]))
  (init)
    
             
  (define (helper ll)
    (if (>= counter 2000) (void) (set! counter (+ counter 1)))
    (cond [(null? ll) image]
          [else (let*([position (particle-posn (car ll))]
                      ;[zz (particle-posn (car kk))]                   ; We tried to implement trace, but couldn't. 
                      ;[prev-x (vec-x zz)]
                      ;[prev-y (vec-y zz)]
                      [x (vec-x position)]
                      [y (vec-y position)]
                      [mass (particle-mass (car ll))]
                      [emass (* 3 (expt mass (/ 1 3)))])
               
                  ;(if (>= counter 2000) (begin (set! list-points (cons (list prev-x (- 800 prev-y) x (- 800 y)) (take list-points 1200)))) 
                  ;   (set! list-points (cons (list prev-x (- 800 prev-y) x (- 800 y)) list-points)))                
                  (overlay (place-image (overlay (circle emass "outline" (pen "white" (round (/ (sqrt emass) 5)) "solid" "round" "round"))
                                                 (ellipse (* 2 emass) (* 2 emass) "solid" "blue")) x (- 800 y) (rectangle 1400 800 "outline" "black"))
                           (helper (cdr ll))))]))
  (helper state))

(define image (rectangle 1400 800 "solid" "black"))

(define (iter)
  (big-bang state
    (on-tick singlestep timescale)
    (to-draw drawing-func))
  (displayln "Test cases Available")
  (displayln "1. 3particle-collision")
  (displayln "2. 2particle-collision")
  (displayln "3. 3particle-movement ")
  (displayln "4. 4particle-periodic ")
  (displayln "5. 3particle-movement ")
  (displayln "6. periodic-ellipse ")
  (displayln "7. 50particles")
  (newline)
  (set! state (func (read)))
  (iter))

(iter)

























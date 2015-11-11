#lang racket

(require #;"minecraft-mcr-reader.rkt"
         #;"minecraft-editor.rkt"
         rackunit)

(define dx 16)
(define dy 128)
(define dz 16)

;; an ArrayChunk is a big 3d vector plus offsets
(struct arraychunk (minx miny minz vec))

(define (arraychunk-ref ac x y z)
  (match ac
    [(arraychunk minx miny minz vec)
     (vector-ref vec (xyz->offset (- x minx) (- y miny) (- z minz)))]))

(define (arraychunk-set! ac x y z n)
  (match ac
    [(arraychunk minx miny minz vec)
     (vector-set! vec (xyz->offset (- x minx) (- y miny) (- z minz)) n)]))

(define (xyz->offset rx ry rz)
  (unless (and (< -1 rx dx)
               (< -1 ry dy)
               (< -1 rz dz))
    (error 'xyz-offset "illegal x or y or z: ~s ~s ~s" rx ry rz))
  (+ (* rz dx dy) (* ry dx) rz))

(define (new-arraychunk minx miny minz)
  (arraychunk minx miny minz (make-vector (* dx dy dz) #f)))

(define (in-range-of? ac x y z)
  (match ac
    [(arraychunk minx miny minz vec)
     (and (< (sub1 minx) x (+ dx minx))
          (< (sub1 miny) y (+ dy miny))
          (< (sub1 minz) z (+ dz minz)))]))

(define test-ac (new-arraychunk 32 0 64))
(check-equal? (arraychunk-ref test-ac 35 40 69) #f)
(check-exn exn:fail? (lambda () (arraychunk-ref test-ac 31 40 69)))

;; an AClist is a list of arraychunks

(define (add ac-list x y z v)
  (cond [(empty? ac-list) (list (new-arraychunk (trimdown x dx)
                                                (trimdown y dy)
                                                (trimdown z dz)))]
        [else (cond [(in-range-of? (first ac-list) x y z)
                     (arraychunk-set! (first ac-list) x y z v)]
                    [else (cons (first ac-list) (add (rest ac-list x y z v)))])]))

(define (trimdown a da)
  (- a (modulo a da)))

(define test-aclist (add empty 234 222 -2134 'booger))

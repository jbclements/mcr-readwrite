#lang typed/racket

(require "minecraft-mcr-reader.rkt")
;; a renderzone is a function mapping x,y, and z to either a number from 0 to 256 or false.

(define-type Bounds (Vector Integer Integer))

(struct: Bounds3 ([x : Bounds]
                  [y : Bounds]
                  [z : Bounds]))

(define-type Blockfun (Integer Integer Integer -> (U Natural False)))
(struct: Renderzone ([bounds : Bounds3] 
                     [contents : Blockfun]))

(: make-blockfun (Bounds3 Natural -> Blockfun))
(define (make-blockfun bounds material)
  (lambda (x y z) 
    (cond [(and ((between (Bounds3-x bounds)) x)
                ((between (Bounds3-y bounds)) y)
                ((between (Bounds3-z bounds)) z)) material]
          [else #f])))

(: bounds3-union (Bounds3 Bounds3 -> Bounds3))
(define (bounds3-union b1 b2)
  (Bounds3 (bounds-union (Bounds3-x b1) (Bounds3-x b2))
           (bounds-union (Bounds3-y b1) (Bounds3-y b2))
           (bounds-union (Bounds3-z b1) (Bounds3-z b2))))

(: bounds-union (Bounds Bounds -> Bounds))
(define (bounds-union b1 b2)
  (vector (min (vector-ref b1 0) (vector-ref b2 0))
          (max (vector-ref b1 1) (vector-ref b2 1))))

(: between (Bounds -> (Integer -> Boolean)))
(define (between bounds)
  (lambda (x)
    (<= (vector-ref bounds 0) x (sub1 (vector-ref bounds 1)))))





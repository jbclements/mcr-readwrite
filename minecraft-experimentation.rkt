#lang racket

(require "minecraft-mcr-reader.rkt"
         "minecraft-editor.rkt"
         rackunit)

;; blocks for a huge pit
(define (pit-blocks pit-bottom chunk-x chunk-z)
  (define pit-rim 67)
  (when (< pit-rim pit-bottom)
    (error 'pit-blocks "only works for pit-bottom <= ~s" pit-rim))
  (define (block-maker x y z)
    (cond [(<= pit-rim y) 0]
          [(<= y pit-bottom) 7]
          [(or (= x 0) (= x 15)) 16]
          [(or (= z 0) (= z 15)) 16]
          [else 0]))
  (define (skylight-maker x y z)
    (match (block-maker x y z)
      [0 15]
      [other 0]))
  (define (height-map-maker x z)
    (cond [(or (= x 0) (= x 15) (= z 0) (= z 15)) (+ pit-rim 1)]
          [else (+ pit-bottom 1)]))
  (define (extra-data-maker x y z) 0)
  (generate-chunk block-maker
                  skylight-maker
                  extra-data-maker
                  chunk-x
                  chunk-z))

(define (tower-block-maker x y z)
    (cond [(= y 127) 0]
          [(and (= x 7) (= z 7)) (cond [(= (modulo y 4) 1) 5]
                                       [else 0])]
          [(and (= x 7) (= z 8)) (cond [(= (modulo y 4) 2) 5]
                                       [else 0])]
          [(and (= x 8) (= z 8)) (cond [(= (modulo y 4) 3) 5]
                                       [else 0])]
          [(and (= x 8) (= z 7)) (cond [(= (modulo y 4) 0) 5]
                                       [else 0])]
          [(= (modulo y 4) 0) 1]
          #;[(or (= x 0) (= x 15)) 1]
          #;[(or (= z 0) (= z 15)) 1]
          [else 0]))

(define (zero x y z) 0)

(define (random-block-maker x y z)
    (if (< (random) 0.1) 1 0))

(define ((level-dirt level) x y z)
  (cond [(<= y level) 3]
        [else 0]))

(define (simple-chunk block-maker chunk-x chunk-z tick)
  (generate-chunk block-maker zero zero
                  chunk-x chunk-z tick))

(define (stair-block-maker x y z)
  (cond [(and (< 4 x 8) (< z 8) (= y 63)) 24]
        [(< x 3) 0]
        [(>= x 13) 0]
        [(< z 3) 0]
        [(>= z 13) 0]
        [(and (< x 7) (< z 9)) (cond [(= (modulo y 4) 1) 24]
                                     [else 0])]
        [(and (< x 9) (>= z 9)) (cond [(= (modulo y 4) 2) 24]
                                      [else 0])]
        [(and (>= x 9) (>= z 7)) (cond [(= (modulo y 4) 3) 24]
                                       [else 0])]
        [(and (>= x 7) (< z 7)) (cond [(= (modulo y 4) 0) 24]
                                      [else 0])]
        [else 0]))


(define (vslice chunk x z ymin ymax)
  (define cblocks (second (get-field/chain chunk '("" "Level" "Blocks"))))
  (for/list ([y (in-range ymin ymax)])
    (regular-byte-ref cblocks x y z)))



#;(call-with-output-file "/tmp/data"
  #:exists 'truncate
  (lambda (port)
    (block-bytes-display (maker->block-bytes tower-block-maker) port)))


(define save-dir "/Users/clements/Library/Application Support/minecraft/saves/z6/")

(define player-data `(compound ,(parse-player-file (build-path save-dir "level.dat"))))

(define player-pos (get-field/chain player-data '("" "Data" "Player" "Pos")))
(define abs-x (posn->chunk (second (third player-pos))))
(define abs-z (posn->chunk (second (fifth player-pos))))


(define (hollow-out cx cz)
  (printf "hollowing out ~s, ~s\n" cx  cz)
  (define northern-chunk (chunk-read/dir save-dir cx cz))
  
  (define killed-block-types (list 1 3))
  ;; either air, or things that will fall if unsupported:
  (define preserve-next-to-types (list 0 8 9 10 11 12 13))
  (define pre-blocks (second (get-field/chain northern-chunk '("" "Level" "Blocks"))))
  (define post-blocks (make-bytes (* 16 16 128) 0))
  
  
  (define (adjoins-preserved? x y z pre-blocks)
    (for*/or ([dx (in-range -1 2)]
              [dy (in-range -1 2)]
              [dz (in-range -1 2)]
              #:when (and (<= 0 (+ dx x) 15)
                          (<= 0 (+ dy y) 128)
                          (<= 0 (+ dz z) 15)
                          (not (= 1 dx dy dz))))
      (member (regular-byte-ref pre-blocks (+ x dx) (+ y dy) (+ z dz))
              preserve-next-to-types)))
  
  (for* ([x (in-range 16)]
         [y (in-range 128)]
         [z (in-range 16)])
    (define existing (regular-byte-ref pre-blocks x y z))
    (cond [(or (not (member existing killed-block-types))
               (adjoins-preserved? x y z pre-blocks))
           (regular-byte-set! post-blocks x y z 
                              (regular-byte-ref pre-blocks x y z))]
          [else ;; leave it as air...
           (cond [(= 0 (modulo y 5)) (regular-byte-set! post-blocks x y z 1)]
                 [else (void)])]))
  
  (define new-chunk (set-field/chain northern-chunk '("" "Level" "Blocks")
                                     (lambda (dc) `(bytearray ,post-blocks))))
  
  (with-handlers ([exn:fail?
                   (lambda (exn) (fprintf (current-error-port)
                                          "~s\n" (exn-message exn)))])
    (chunk-overwrite/dir save-dir cx cz new-chunk)))

(define (highway x z)
  (printf "highway for ~s, ~s\n" x  z)
  (with-handlers ([exn:fail?
                   (lambda (exn) (fprintf (current-error-port)
                                          "~s\n" (exn-message exn)))])
    (define this-chunk (chunk-read/dir save-dir x z))
    
    (define pre-blocks (second (get-field/chain this-chunk '("" "Level" "Blocks"))))
    (define post-blocks (make-bytes (* 16 16 128) 0))
    
    (for* ([x (in-range 16)]
           [z (in-range 7 10)])
      (regular-byte-set! pre-blocks x 82 z 45))
    
    (define new-chunk (set-field/chain this-chunk '("" "Level" "Blocks")
                                       (lambda (dc) `(bytearray ,pre-blocks))))
    
    (chunk-overwrite/dir save-dir x z new-chunk)))

(define (ew-highway x z)
  (printf "east-west highway for ~s, ~s\n" x  z)
  (with-handlers ([exn:fail?
                   (lambda (exn) (fprintf (current-error-port)
                                          "~s\n" (exn-message exn)))])
    (define this-chunk (chunk-read/dir save-dir x z))
    
    (define pre-blocks (second (get-field/chain this-chunk '("" "Level" "Blocks"))))
    (define post-blocks (make-bytes (* 16 16 128) 0))
    
    (for* ([z (in-range 16)]
           [x (in-range 7 10)])
      (regular-byte-set! pre-blocks x 82 z 45))
    
    (define new-chunk (set-field/chain this-chunk '("" "Level" "Blocks")
                                       (lambda (dc) `(bytearray ,pre-blocks))))
    
    (chunk-overwrite/dir save-dir x z new-chunk)))

(for* ([x (in-range (- abs-x 3) (+ abs-x 4))]
       [z (in-range (- abs-z 3) (+ abs-z 4))])
  (hollow-out x z))

#;(for ([z (in-range -30 30)])
  (ew-highway abs-x z))




;; simple round-trip:
#|
(define cx -1)
(define cz -3)
(define f "/tmp/r.-1.-1.mcr")

(define relative-cx (modulo cx 32))
(define relative-cz (modulo cz 32))

(define level (chunk-read f relative-cx relative-cz))


(rip-out-bytearrays level)

#;(overwrite-chunk f cx cz level)

(for*/list ([x (in-range 4)]
            [z (in-range 12 16)])
           (vslice level x z 40 69))

(define (skylight chunk x y z)
  (define clight (second (get-field/chain chunk '("" "Level" "SkyLight"))))
  (half-byte-ref clight x y z))

(for*/list ([x (in-range 4)]
            [z (in-range 4)])
  (for/list ([y (in-range 40 69)])
    (skylight level x y z)))

(define l (get-field/chain level '("" "Level")))

(define new-chunk ()(stair-blocks cx cz 0))
#;(check-equal? (legal-chunk? new-chunk) #t)
(overwrite-chunk f relative-cx relative-cz new-chunk)


#;(let ([pos (get-fieldzz player '("" "Data" "Player" "Pos"))])
  (xz->chunk (second (third pos))
             (second (fifth pos))))



(define sample-blocks (stair-blocks 4 5 1234))
(check-equal? (get-field/chain sample-blocks '("" "Level" "xPos"))
              '(i32 4))
(check-equal? (get-field/chain sample-blocks '("" "Level" "zPos"))
              '(i32 5))


|#
#lang racket


(require "minecraft-mcr-reader.rkt"
         "minecraft-editor.rkt"
         "data-skeleton.rkt"
         mred
         (except-in slideshow get-field)
         plot)


#;(compound-thing->names c1)

;(mc-thing->skeleton c1)
;(define data (second (get-field/chain c1 '("" "Level" "Blocks"))))

#;(for*/list ([z (in-range 15)] [x (in-range 15)])
  (vector-ref block-names (regular-byte-ref data x 63 z)))







(define green-bytes (list->bytes '(255 0 255 0)))
(define blue-bytes (list->bytes '(255 0 0 255)))
(define white-bytes (list->bytes '(255 255 255 255)))
(define black-bytes (list->bytes '(255 0 0 0)))
(define red-bytes (list->bytes '(255 255 0 0)))

(define water-num (block-name->num "Stationary water"))
(define air-num (block-name->num "Air"))

(define diamond-ore-num (block-name->num "Diamond Ore"))
(define moss-stone-num (block-name->num "Moss Stone"))
(define special-block-nums 
  (map block-name->num
       '("Moss Stone"
         "Monster Spawner"
         "Locked Chest")))


(define all-black-block (apply bytes-append 
                               (for/list ([i (in-range (* 16 16))])
                                 black-bytes)))


(define (cell->bytes val)
  (cond [(= val air-num) white-bytes]
        [(= val water-num) blue-bytes]
        [else green-bytes]))




(define (column-has-special? data x z)
  (for/or ([y (in-range CHUNKDY)])
    (member (regular-byte-ref data x y z)
            special-block-nums)))

(define (column-has-diamond-ore? data x z)
  (for/or ([y (in-range CHUNKDY)])
    (= (regular-byte-ref data x y z)
       diamond-ore-num)))

(define (column-has-moss-stone? data x z)
  (for/or ([y (in-range CHUNKDY)])
    (= (regular-byte-ref data x y z)
       moss-stone-num)))


(define (make-map global-x-offset
                  global-y-offset
                  map-x-chunks
                  map-y-chunks
                  y-level)
  (define x-pixels (* map-x-chunks CHUNKDX))
  (define z-pixels (* map-z-chunks CHUNKDZ))
  (define d (make-object bitmap% x-pixels z-pixels))
  
(for* ([x-chunk (in-range map-x-chunks)]
       [z-chunk (in-range map-z-chunks)])
  (when (and (= (modulo x-chunk 5) 0)
             (= z-chunk 0))
    (printf "x = ~s\n" x-chunk))
  (define x-offset (* x-chunk CHUNKDX))
  (define z-offset (* z-chunk CHUNKDZ))
  (with-handlers 
      ([(lambda (exn)
          (and (exn:fail? exn)
               (regexp-match #px"does not exist in this file"
                             (exn-message exn))))
        (lambda (exn)
          (send d set-argb-pixels
                (- x-pixels (+ x-offset CHUNKDX))
                (- z-pixels (+ z-offset CHUNKDZ))
                CHUNKDX CHUNKDZ
                all-black-block))])
    (define chunk-data
      (second
       (get-field/chain
        (chunk-read/dir "/tmp/world" 
                        (+ global-x-offset
                           (- x-chunk 
                              (/ map-x-chunks 2)))
                        (+ global-z-offset
                           (- z-chunk 
                              (/ map-z-chunks 2))))
        '("" "Level" "Blocks"))))
    (for* ([z (in-range CHUNKDZ)]
           [x (in-range CHUNKDX)])
      (send d set-argb-pixels
            (- x-pixels 1 (+ x-offset x)) 
            (- z-pixels 1 (+ z-offset z))
            1 
            1
            #;(cell->bytes (regular-byte-ref chunk-data x y-level z))
            (cond [(column-has-moss-stone? chunk-data x z) red-bytes]
                  [else (cell->bytes (regular-byte-ref chunk-data x 62 z))])))))
  (bitmap d))





(define map-x-chunks 2)
(define map-z-chunks 2)
(define global-x-offset -8)
(define global-z-offset -8)

#;(make-map global-x-offset global-z-offset
            map-x-chunks map-z-chunks 62)
#;(for/list ([y (in-range 62 0 -2)])
  (make-map global-x-offset global-z-offset
            map-x-chunks map-z-chunks y))


(define (get-chunk-data x-chunk z-chunk)
  (second
   (get-field/chain
    (chunk-read/dir "/tmp/world" 
                    x-chunk
                    z-chunk)
    '("" "Level" "Blocks"))))

(define (one-column x z)
  (define chunk-x (floor (/ x CHUNKDX)))
  (define chunk-z (floor (/ z CHUNKDZ)))
  (define data (get-chunk-data chunk-x chunk-z))
  (define rel-x (modulo x CHUNKDX))
  (define rel-z (modulo z CHUNKDZ))
  (for/list ([y (in-range CHUNKDY)])
    (vector-ref block-names (regular-byte-ref data rel-x y rel-z))))


(define (find-diamonds chunk-x chunk-z)
  (define data (get-chunk-data chunk-x chunk-z))
  (for* ([x (in-range CHUNKDX)]
         [z (in-range CHUNKDZ)])
    (for ([y (in-range CHUNKDY)])
      (when (= (regular-byte-ref data x y z) 
               diamond-ore-num)
        (printf "diamond ore: ~s ~s ~s\n" x y z)))))

#;(find-diamonds -5 -81)

#;(column-has-special? -72 -1268)
#;(one-column -72 -1290)

#;(one-column (+ -1 (* 16 -6))
            (+ 4 (* 16 -10)))

#;(list (+ 1 (* 16 -6))
            (+ 3 (* 16 -10)))


#|

(define (square->image bytes x z)
  (define content (vector-ref
                   block-names
                   (regular-byte-ref bytes x 63 z)))
  (cond [(equal? content "Water") "blue"]
        [else "green"]))

(define (row->image bytes z)
  (apply 
   append
   (reverse
    (for/list ([x (in-range 15)])
      (square->image bytes x z)))))

(define (chunk->image bytes)
  (color-list->bitmap
   (apply 
    append
    (reverse
     (for/list ([z (in-range 15)])
       (row->image bytes z))))
   16 16))

(define (region-row->image z)
  (apply 
   beside
   (reverse
    (for/list ([x (in-range 0 31)])
      (with-handlers ((exn:fail? 
                       (lambda (exn)
                         (rectangle 16 16 "solid" "black"))))
        (chunk->image ))))))

(time
 (apply
  above
  (reverse
   (for/list ([z (in-range 0 31)])
     (region-row->image z)))))

#;(for ([z (in-range 15)]
      [x (in-range 15)]))
#;(block-bytes-display data (current-output-port))


|#

(define (find-something sought
                        cx-min cx-max
                        cz-min cz-max)
  (define found empty)
  
  (for* ([x-chunk (in-range cx-min cx-max)]
         [z-chunk (in-range cz-min cz-max)])
    (when (and (= (modulo x-chunk 5) 0))
      (printf "x = ~s\n" x-chunk))
    #;(define x-offset (* x-chunk CHUNKDX))
    #;(define z-offset (* z-chunk CHUNKDZ))
    (with-handlers 
        ([(lambda (exn)
            (and (exn:fail? exn)
                 (regexp-match #px"does not exist in this file"
                               (exn-message exn))))
          (lambda (exn) (void))])
      (define chunk-data
        (second
         (get-field/chain
          (chunk-read/dir "/tmp/world" x-chunk z-chunk)
          '("" "Level" "Blocks"))))
      (for* ([z (in-range CHUNKDZ)]
             [x (in-range CHUNKDX)]
             [y (in-range CHUNKDY)])
        (cond [(= (regular-byte-ref chunk-data 
                                    x y z) sought)
               (set! found (cons
                            (list
                             (+ (* CHUNKDX x-chunk)x)
                             y 
                             (+ (* CHUNKDZ z-chunk)z))
                            found))]))))
  found)

(define diamond-locations (find-something diamond-ore-num
                                          -15 5
                                          -91 -71
                                          ))

(define diamond-ys (map second diamond-locations))

(define min-y (apply min diamond-ys))
(define max-y (apply max diamond-ys))
(define the-hash (make-hash))
(for ([y (in-list diamond-ys)])
  (hash-set! the-hash y (add1 (hash-ref the-hash y 0))))
(define bins (sort (hash-map the-hash (lambda (k v) (vector k v)))
                   <
                   #:key (lambda (v) (vector-ref v 0))))
(plot (discrete-histogram bins))
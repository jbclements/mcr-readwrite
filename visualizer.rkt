#lang racket

(require "minecraft-mcr-reader.rkt"
         "minecraft-editor.rkt"
         2htdp/image)

(define chunk-image-pixels 10)
(define region-image-pixels (* chunk-image-pixels REGIONDHORIZ))

(define toc (toc-read "/tmp/world/region/r.0.0.mcr"))


(define (bools->row-image bools)
  (apply above (for/list ([b (reverse bools)])
                  (rectangle chunk-image-pixels
                             chunk-image-pixels
                             "solid"
                             (cond [b "green"] [else "black"])))))

(define (lolob->image lolob)
  (apply beside (map bools->row-image (reverse lolob))))


(define (file->toc-image path)
  (lolob->image (toc-read path)))

;; return a list of the region files in the dir
(define (region-files dir)
  (parameterize ([current-directory dir])
    (for/list ([f (in-list (directory-list))]
               #:when (file-exists? f)
               #:when (regexp-match #px"r\\..*\\.mcr" f))
      (build-path dir f))))

;; given a region path, return the x and z coordinates
;; contained in the filename.
(define (region-xz region-path)
  (define-values (base filename must-be-dir?)
    (split-path region-path))
  (match (list filename must-be-dir?)
    [(list filename #f)
     (match (regexp-match #px"r\\.([0-9-]+)\\.([0-9-]+)\\.mcr" 
                          filename)
       [(list dc x-str z-str)
        (list (string->number x-str)
              (string->number z-str))])]
    [else (error 'region-xz "unexpected")]))


(define the-files (region-files "/tmp/world/region/"))
(define path-xzs
  (for/list ([p the-files])
    (list p (region-xz p))))
(define xs (map first (map second path-xzs)))
(define min-x (apply min xs))
(define max-x (apply max xs))
(define zs (map second (map second path-xzs)))
(define min-z (apply min zs))
(define max-z (apply max zs))
(define bg (rectangle (* (- (add1 max-x) min-x) REGIONDHORIZ
                         chunk-image-pixels)
                      (* (- (add1 max-z) min-z) REGIONDHORIZ
                         chunk-image-pixels)
                      "solid"
                      "gray"))
(first path-xzs)
(define (overlay-toc path-xz bg)
  (match-define (list path (list x z)) path-xz)
  (overlay/align/offset "right" "bottom"
                        (file->toc-image path)
                        (* region-image-pixels (- x min-x))
                        (* region-image-pixels (- z min-z))
                        bg))
(foldl overlay-toc bg path-xzs)

#;(define image2 (file->i))
#;(overlay/align/offset "right" "bottom"
                      test-image (- x-offset)
                      (- z-offset) bg)


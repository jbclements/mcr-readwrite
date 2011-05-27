#lang racket

(require "minecraft-mcr-reader.rkt"
         "minecraft-editor.rkt"
         rackunit
         racket/runtime-path)

(define-runtime-path testdata "./testdata")

;; TAKEN FROM MINECRAFT-MCR-READER:

(define test-region-file (build-path testdata "r.-1.-1.mcr"))
(require "testdata/chunk.rkt")
;; extra "equal?" wrapper to prevent grotesque amounts of output on fail:
(check-equal?
 (equal? (chunk-read test-region-file 28 26)
         testchunk)
 #t)

;; overwrite the file, make sure that nothing changes (won't work with timestamps...)
(define tempregionfile (make-temporary-file))
(delete-file tempregionfile)
;; in principle, someone could grab this filename in here...
(copy-file test-region-file tempregionfile)
(chunk-overwrite tempregionfile 28 26 testchunk)
(check-equal? 
 (system 
  (string-append "diff "(path->string tempregionfile)" "(path->string test-region-file)))
 #true)
(delete-file tempregionfile)


(check-equal?
 (map (lambda (row)
        (apply string-append
               (map (lambda (elt)
                      (if elt "X" " "))
                    row)))
      (toc-read (build-path testdata "r.-1.-1.mcr")))
 '("                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                                "
   "                              XX"
   "                         XXXXXXX"
   "                         XXXXXXX"
   "                         XXXXXXX"
   "                        XXXXXXXX"
   "                         XXXXXXX"
   "                        XXXXXXXX"
   "                         XXXXXXX"
   "                         XXXXXXX"))

;; round-trip tests

(define (round-trip parsed)
  (define-values (in-port out-port) (make-pipe))
  (write-tag parsed out-port)
  (parse-tag in-port))

(check-equal? (round-trip '(named "foo" (i8 13))) '(named "foo" (i8 13)))
(check-exn exn:fail? 
           (lambda () (round-trip '(named "z" (i16 12341230897)))))
(check-equal? (round-trip '(named "q" (i16 234))) '(named "q" (i16 234)))
(check-equal? (round-trip '(named "i" (bytearray #"trogdor")))
              '(named "i" (bytearray #"trogdor")))

(define big-fat-example
  `(named "ohth" (compound (named "floozy" (i8 23))
                           (named "bo" (i16 2231))
                           (named "oo" (string "zagga-wagga"))
                           (named "ff" (f32 3.125))
                           (named "gg" (f64 2.7872901))
                           (named "hh" (i32 -22798))
                           (named "ii" (i64 2982739710982))
                           (named "oth" (bytearray #"ontho nteh"))
                           (named "com" (compound (named "bongo" (string "bill"))
                                                  (named "boingo" (i8 23))))
                           (named "blug" (list i8
                                               (i8 13)
                                               (i8 14)
                                               (i8 187))))))

(check-equal? (round-trip big-fat-example) big-fat-example)

(check-equal? (named-mc-thing? big-fat-example)
              true)
(check-equal?
 (named-mc-thing?
  `(named "ohth" (compound (named "floozy" (i8 23))
                           (named "bo" (i16 2231))
                           (named "oo" (string "zagga-wagga"))
                           (named "ff" (f32 3.125))
                           (named "gg" (f64 2.7872901))
                           (named "oth" (bytearray #"ontho nteh"))
                           (named "com" (compound (named "bongo" (string "bill"))
                                                  (named "boingo" (i83 23))))
                           (named "blug" (list i8
                                               (i8 13)
                                               (i8 14)
                                               (i8 187))))))
 false)

;; TAKEN FROM MINECRAFT-EDITOR:

(check-equal? (set-field `(compound (named "a" (i8 14)) 
                                    (named "b" (string "goo"))
                                    (named "c" (i64 2134)))
                         "b"
                         (lambda (dc) '(string "babba")))
              `(compound (named "a" (i8 14)) 
                         (named "b" (string "babba"))
                         (named "c" (i64 2134))))


(check-equal? (set-field/chain `(compound (named "a" (i8 4))
                                          (named "b" 
                                                 (compound (named "b1" (i8 5))
                                                           (named "b2" (i8 6))
                                                           (named "b3" (i8 7))))
                                          (named "c" (i8 8)))
                               `("b" "b2")
                               (lambda (elt) `(i8 1234)))
              `(compound (named "a" (i8 4))
                         (named "b" 
                                (compound (named "b1" (i8 5))
                                          (named "b2" (i8 1234))
                                          (named "b3" (i8 7))))
                         (named "c" (i8 8))))

(check-equal? (get-field `(compound (named "a" (i8 14)) 
                                    (named "b" (string "goo"))
                                    (named "c" (i64 2134)))
                         "b")
              `(string "goo"))

(check-equal? (half-byte-ref (list->bytes (list #xa8 #xb9))
                             0 1 0)
              #xa)
(check-equal? (half-byte-ref (list->bytes (list #xa8 #xb9))
                             0 2 0)
              #x9)


(check-exn (lambda (exn)
             (regexp-match #px"expecting inputs for x=0, y=0" (exn-message exn)))
           (lambda ()
             (raw-nums-read (open-input-bytes #""))))
(check-exn (lambda (exn)
             (regexp-match #px"line contained 1 element" (exn-message exn)))
           (lambda ()
             (raw-nums-read (open-input-bytes #"aobesc"))))
(check-exn (lambda (exn)
             (regexp-match #px"illegal element" (exn-message exn)))
           (lambda ()
             (raw-nums-read (open-input-bytes #"a b c d e f g h a b c d e f g h"))))
(let ([test-bytes (apply bytes-append
                         (for/list ([i (in-range (* CHUNKDX CHUNKDY))])
                           #"1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 \n"))])
  (define parsed (raw-nums-read (open-input-bytes test-bytes)))
  (check-equal? (regular-byte-ref parsed 14 24 3)
                4))

;; round-trip on raw-nums format
(let ([t (make-temporary-file)]
      [b (make-bytes (* CHUNKDX CHUNKDY CHUNKDZ) 0)])
  ;; fill with random data:
  (for ([i (in-range (bytes-length b))])
    (bytes-set! b i (random 256)))
  (call-with-output-file t
    (lambda (port)
      (block-bytes-display b port))
    #:exists 'truncate)
  (call-with-input-file t
    (lambda (port)
      (check-equal? (raw-nums-read port)
                    b)))
  (delete-file t))

;; REGRESSION TEST

(define ((make-hashy-xyz-fun seed modulus) x y z)
  (modulo (+ (* seed x)
             (* (* seed 3) y)
             (* (* seed 11) z))
          modulus))

(require "regression-test.rkt")

(check-equal?
 ;; I'm using equal? to hide the output. When it fails,
 ;; rackunit otherwise tries to print everything out,
 ;; and DrRacket hangs...
 (equal? (generate-chunk (make-hashy-xyz-fun 13 256)
                         (make-hashy-xyz-fun 17 16)
                         (make-hashy-xyz-fun 19 16)
                         -45
                         13
                         1238298)
         test-out)
 true)

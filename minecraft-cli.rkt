#lang racket

(require "minecraft-editor.rkt"
         "minecraft-mcr-reader.rkt"
         rackunit)


(define FILECHUNKDX 32)
(define FILECHUNKDZ 32)

(define nat? exact-nonnegative-integer?)

(define (mcr-check file)
  (unless (file-exists? file)
    (fprintf (current-error-port)
             "file ~s does not exist\n" file) 
    (exit 1))
  (unless (regexp-match #px".*r\\..*\\..*\\.mcr" file)
    (fprintf (current-error-port)
             "WARNING: file's name is not of the form r.?.?.mcr. Probably wrong...\n")))

(define (check-range str name lo hi)
  (define converted (string->number str))
  (unless (and (integer? converted)
               (<= lo converted hi))
    (fprintf (current-error-port)
             "value for ~a must be an integer in range [~a,~a], given: ~s\n"
             name lo hi str)
    (exit 1)))

;; check that x and z are in-range for the given filename, and compute the relative
;; rel-cx and rel-cz.
(define (scrub-and-check file x-str z-str)
  (define-values (rx rz) (match (regexp-match #px"r.([-0-9]+)\\.([-0-9]+)\\.mcr$"
                                              file)
                           [(list dc rx-str rz-str)
                            (values (string->number rx-str)
                                    (string->number rz-str))]))
  (define minx (* FILECHUNKDX rx))
  (define maxx (sub1 (* FILECHUNKDX (add1 rx))))
  (define minz (* FILECHUNKDX rz))
  (define maxz (sub1 (* FILECHUNKDX (add1 rz))))
  (check-range x-str "x" minx maxx)
  (check-range z-str "z" minz maxz)
  (define cx (string->number x-str))
  (define cz (string->number z-str))
  (values cx cz
          (modulo cx FILECHUNKDX)
          (modulo cz FILECHUNKDZ)))



;; check that a file exists and has the extension ".mcr"
(command-line 
 #:once-any 
 [("-e" "--extract") file x z
                     "extract the blocks from a chunk, sending output to stdout"
                     (mcr-check file)
                     (define-values (cx cz rel-cx rel-cz)
                       (scrub-and-check file x z))
                     (with-handlers ([exn:fail?
                                      (lambda (exn)
                                        (fprintf (current-error-port)
                                                 "ERROR: ~a\n" (exn-message exn)))])
                       (blocks-display (chunk-read file rel-cx rel-cz)
                                       (current-output-port)))]
 [("-i" "--inject") file x z
                    "inject blocks into a chunk, taking input from stdin"
                    (mcr-check file)
                    (define-values (cx cz rel-cx rel-cz)
                      (scrub-and-check file x z))
                    (define new-chunk (raw-nums->blocks (current-input-port)
                                                        cx cz))
                    (chunk-overwrite file rel-cx rel-cz new-chunk)]
 #;[("-a" "--about") file
                   "extract the meta-info from an .mcr file"
                   (mcr-check file)]
 [("-p" "--playerinfo") file
                        "read player info from a level.dat file"
                        (unless (file-exists? file)
                          (fprintf (current-error-port)
                                   "file ~s does not exist\n" file) 
                          (exit 1))
                        (unless (regexp-match #px".*level\\.dat$" file)
                          (fprintf (current-error-port)
                                   "WARNING: file not named level.dat. Probably wrong....\n"))
                        (with-handlers ([exn:fail?
                                         (lambda (exn)
                                           (fprintf (current-error-port)
                                                    "ERROR: ~a\n" (exn-message exn)))])
                          (define parsed (parse-player-file file))
                          (pretty-print 
                           parsed)
                          (define posn (get-field/chain `(compound ,parsed) 
                                                        '("" "Pos")))
                          (define player-x (inexact->exact (floor (second (third posn)))))
                          (define player-z (inexact->exact (floor (second (fifth posn)))))
                          (define player-xchunk (floor (/ player-x CHUNKDX)))
                          (define player-zchunk (floor (/ player-z CHUNKDZ)))
                          (printf "player currently in chunk: ~s,~s.\n"
                                  player-xchunk player-zchunk)
                          (printf "this is in file ~s\n"
                                  (format "r.~s.~s.mcr"
                                          (floor (/ player-xchunk FILECHUNKDX))
                                          (floor (/ player-zchunk FILECHUNKDZ)))))])



;; a test case, to whet the appetite...

;; actually, this test case caught a significant bug.
(check-equal? (call-with-values (lambda () (scrub-and-check "r.-1.-1.mcr" "-5" "-19"))
                                list)
              (list -5 -19 27 13))
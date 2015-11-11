#lang racket

(require "minecraft-mcr-reader.rkt"
         "minecraft-editor.rkt"
         "data-skeleton.rkt")

(define player
  (named-mc-thing->mc-thing
   (parse-player-file "/tmp/bronzeeagle577.dat")))

(define new-player
  (mc-thing->named-mc-thing
   (set-field/chain
    player
    '("" "Pos")
    (lambda (oldpos)
      '(list
        f64
        (f64 20002.9585546164089473)
        (f64 100.0)
        (f64 -20003.8615251231797743))))))

(write-player-file! new-player "/tmp/foo.dat")

(define player2 
  (parse-player-file "/tmp/foo.dat"))

(equal? player player2)


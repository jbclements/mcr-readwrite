#lang htdp/bsl

(require "minecraft-bsl.rkt")

(with-world 
 "z7"
 (list )
 (get-block (player-pos-x)
            (- (player-pos-y) 2)
            (player-pos-z)))




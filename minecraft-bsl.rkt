#lang racket

(require "minecraft-editor.rkt"
         "minecraft-mcr-reader.rkt")

(provide player-pos-x
         player-pos-y
         player-pos-z
         get-block)
#;(provide/contract [in-world (->  void?)])

;; first, locate the minecraft directory
(define minecraft-dir
  (match (system-type 'os)
    ['macosx 
     (match (getenv "HOME")
       [(? string? homepath) 
        (build-path homepath "Library" "Application Support" "minecraft")]
       [other
        (error 'minecraft-dir "$HOME env variable not defined, can't find minecraft dir")])]
    [other
     (error 'minecraft-dir "don't know where minecraft data lives on this kind of system: ~a"
            other)]))

(unless (directory-exists? minecraft-dir)
  (error 'minecraft-dir "expected to find minecraft directory at ~s, but it doesn't exist"
         minecraft-dir))

(define saves-dir (build-path minecraft-dir "saves"))

(unless (directory-exists? minecraft-dir)
  (error 'minecraft-dir "expected to find minecraft saves directory at ~s, but it doesn't exist"
         saves-dir))

(define (find-world-directory world-name)
  (let ([worlds (directory-list saves-dir)])
    (unless (member (string->path world-name) worlds)
      (error 'with-world
             "can't find world ~s. Available worlds: ~s" world-name worlds))
    (build-path saves-dir (string->path world-name))))

(provide with-world)

(define-syntax (with-world stx)
  (syntax-case stx ()
    [(_ world-name body ...)
     (string? (syntax-e #'world-name))
     #'(parameterize ([current-directory (find-world-directory world-name)])
         body ...)]))

;; player-pos : -> pos
(define (player-pos)
  (unless (file-exists? "level.dat")
    (error 'player-pos-x "no level.dat file in current directory"))
  (get-field/chain `(compound ,(parse-player-file "level.dat"))
                   '("" "Data" "Player" "Pos")))

;; player-pos-x : -> integer
(define (player-pos-x)
  (inexact->exact (floor (second (third (player-pos))))))

;; player-pos-y : -> integer
(define (player-pos-y)
  (inexact->exact (floor (second (fourth (player-pos))))))

;; player-pos-z : -> integer
(define (player-pos-z)
  (inexact->exact (floor (second (fifth (player-pos))))))

;; get-block : x y z -> int
(define (get-block x y z)
  (define chunk (chunk-read/dir (current-directory) (posn->chunk x) (posn->chunk z)))
  (define blocks (second (get-field/chain chunk '("" "Level" "Blocks"))))
  (regular-byte-ref blocks
                    (- x (* CHUNKDX (posn->chunk x)))
                    y
                    (- z (* CHUNKDZ (posn->chunk z)))))

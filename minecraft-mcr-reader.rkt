#lang racket

;; I'm assuming that lengths are to be represented using 
;; unsigned bits...

;; I'm guessing we're using utf-8 encodings, or just sticking to ASCII

(require "data-skeleton.rkt"
         file/gunzip
         (planet soegaard/gzip))




;; FILE I/O

(define CHUNKSPERREGION 32)

(provide/contract [chunk-read (-> path-string? nat? nat? blank-compound-mc-thing?)]
                  [toc-read (-> path-string? (listof (listof boolean?)))]
                  [chunk-timestamp/file (-> path-string? nat? nat? nat?)]
                  [chunk-overwrite (-> path-string? 
                                       nat?
                                       nat?
                                       blank-compound-mc-thing?
                                       void?)]
                  [parse-player-file (-> path-string? named-mc-thing?)]
                  [write-player-file! (-> named-mc-thing? path-string? void?)]
                  [parse-tag (-> input-port? named-mc-thing?)]
                  [write-tag (-> named-mc-thing? output-port? void?)])

;; given the file name, return a 2d list of which chunks are allocated
(define (toc-read path)
  (define header (file-header path))
  (for*/list ([x (in-range CHUNKSPERREGION)])
    (for/list ([z (in-range CHUNKSPERREGION)])
      (not (= 0 (chunk-length header x z))))))

;; path-string? nat? nat? -> named-thing
;; given a filename and two *relative* integers, read the
;; corresponding chunk from the file
(define (chunk-read file chunk-x chunk-z)
  (define-values (pipe-in pipe-out) (make-pipe))
  (chunk-read/bytes file chunk-x chunk-z pipe-out)
  `(compound ,(parse-tag pipe-in)))


;; return the timestamp of a chunk
(define (chunk-timestamp/file file chunk-x chunk-y)
  (chunk-timestamp (file-header file) chunk-x chunk-y))


;; given a file and the *relative* x and z of a chunk and a new chunk, 
;; replace the old one with the new one (if it fits in the space
;; allocated for the old one).
(define (chunk-overwrite file x z new-chunk)
  (match-let* ([(list posn space)(file-chunk-info file x z)])
    (when (= posn 0)
      (error 'chunk-read/bytes
             "chunk ~s,~s does not exist in this file (not allocated yet)."
             x z))
    (define op (open-output-bytes))
    (write-tag (second new-chunk) op)
    (define new-chunk-bytes (get-output-bytes op))
    (define compressed (compress-bytes new-chunk-bytes))
    (define compressed-length (bytes-length compressed))
    (unless (< compressed-length space)
      (error 'overwrite-chunk 
             "compressed chunk has length ~s, too long for allocated space ~s"
             compressed-length
             space))
    (define file-port (open-output-file file #:exists 'update))
    (file-position file-port posn)
    (write-bytes (integer->integer-bytes (+ compressed-length 1) 4 #t #t)
                 file-port)
    ;; zlib format:
    (write-byte 2 file-port)
    (write-bytes compressed file-port)
    (close-output-port file-port)
    #;(chunk-timestamp-write/file file x z (current-seconds))))

(provide/contract [generate-mcr-file (-> path-string?
                                         nat?
                                         nat?
                                         (-> nat? nat? named-mc-thing?)
                                         void?)])
;; write a fresh new mcr file
(define (generate-mcr-file file big-x big-z block-maker)
  (error 'generate-mcr-file "unimplemented!"))



;; given the file, return the first 8K (the header)
(define (file-header file)
  (call-with-input-file* file
    (lambda (port)
      (read-bytes 8192 port))))

;; given the header bytes x and z in the range [0,31], compute
;; the offset of the chunk data.
(define (chunk-offset header x z)
  (define info-offset (chunkxz->offset x z)) 
  (* 4096
     (integer-bytes->integer
      (bytes-append (bytes 0) (subbytes header info-offset (+ 3 info-offset)))
      #f
      #t)))

;; given the header bytes and x and z in the range [0,31], compute
;; the timestamp of the chunk
(define (chunk-timestamp header x z)
  (define info-offset (+ 4096 (chunkxz->offset x z))) 
  (integer-bytes->integer
   (subbytes header info-offset (+ 4 info-offset))
   #f
   #t))

;; overwrite the timestamp of a chunk
(define (chunk-timestamp-write/file file chunk-x chunk-z timestamp)
  (let ([file-port (open-output-file file #:exists 'update)])
    (file-position file-port (+ 4096 (chunkxz->offset chunk-x chunk-z)))
    (write-bytes (integer->integer-bytes timestamp 4 #f #t) file-port)
    (close-output-port file-port)))

;; given x and z, find the location within the header (TOC) of the reference
;; to the chunk's data
(define (chunkxz->offset x z)
  (* 4 (+ x (* z 32))))

;; given the header bytes and x and z in the range [0,31], compute
;; the length of the chunk data
(define (chunk-length header x z)
  (define info-offset (* 4 (+ x (* z 32))))
  (* 4096
     (first
      (bytes->list
       (subbytes header (+ info-offset 3) (+ info-offset 4))))))


;; returns the location & space allocated to the given chunk
;; within the given file
(define (file-chunk-info file x z)
  (define header (file-header file))
  (define offset (chunk-offset header x z))
  (define space (chunk-length header x z))
  (list offset space))


;; read the given chunk, pipe it (decompressed) to out-port
(define (chunk-read/bytes file x z out-port)
  (define header (file-header file))
  (define offset (chunk-offset header x z))
  (when (= offset 0)
    (error 'chunk-read/bytes
           "chunk ~s,~s does not exist in this file (not allocated yet)."
           x z))
  (call-with-input-file* file
    (lambda (port)
      (file-position port (+ 7 offset))
      (inflate port out-port))))



;; read a player file from disk
(define (parse-player-file filename)
  (define path (cond [(string? filename) (string->path filename)]
                     [else filename]))
  (parse-tag (open-input-gz-file path)))


;; write a player file to disk
(define (write-player-file! player filename)
  (define path (cond [(string? filename) (string->path filename)]
                     [else filename]))
  (define op (open-output-gz-file path))
  (write-tag player op)
  (close-output-port op))

;;
;;
;;   READ
;;
;;


;; the tags, ordered by their numbering. E.G. i16 is represented as 2.
(define ordered-tags
  '#(end i8 i16 i32 i64 f32 f64 bytearray string list compound))

;; given a tag number, return the corresponding symbol
(define (tag-num->symbol i) (vector-ref ordered-tags i))

(define tag->num-table
  (for/hash ([t ordered-tags] [i (in-naturals)]) (values t i)))

;; given a tag symbol, return the corresponding number.
(define (tag-symbol->num s) (hash-ref tag->num-table s))

;; given a port, read a named-thing from it.
(define (parse-tag in-port #:omit-arrays [omit-arrays? #f])
  
  (define (parse-tree)
    (match (read-byte in-port)
      ;; this kind of "end" occurs at the end of a compound object
      [ 0 'end]
      [tag (list 'named 
                 (parse-string)
                 (parse-payload (tag-num->symbol tag)))]))
  
  (define (parse-payload tag)
    (match tag
      ['end 'end]
      ['i8  (list tag (read-byte in-port))]
      ['i16 (list tag (parse-numeric 2))]
      ['i32 (list tag (parse-numeric 4))]
      ['i64 (list tag (parse-numeric 8))]
      ['f32 (list tag
                  (floating-point-bytes->real 
                   (read-bytes 4 in-port)
                   #t))]
      ['f64 (list tag
                  (floating-point-bytes->real
                   (read-bytes 8 in-port)
                   #t))]
      ['bytearray
       (define len (parse-numeric 4))
       (define content (read-bytes len in-port))
       (list tag (cond [omit-arrays? `(omitted ,len)]
                       [else content]))]
      ['string (list tag (parse-string))]
      ['list (define common-tag (tag-num->symbol (read-byte in-port)))
             (define len (parse-numeric 4))
             (cons 'list
                   (cons 
                    common-tag
                    (build-list len
                                (lambda (dc)
                                  (parse-payload common-tag)))))]
      ['compound (cons tag 
                       (parse-until-stop))]))
  
  ;; keep reading named-things until encountering "end", put them in a list
  (define (parse-until-stop)
    (define parsed (parse-tree))
    (cond [(eq? parsed 'end) '()]
          [else (cons parsed (parse-until-stop))]))
  
  ;; read a "bytes"-bytes integer
  (define (parse-numeric bytes)
    (integer-bytes->integer 
     (read-bytes bytes in-port)
     #t #t))
  
  ;; read a string
  (define (parse-string)
    (define strlen (parse-numeric 2))
    (bytes->string/utf-8 (read-bytes strlen in-port)))
  
  (parse-tree))

;;
;;
;;   WRITE
;;
;;

;; serialize a named-thing to an output port
(define (write-tag parsed out-port)
  
  ;; write a named thing to out-port
  (define (write-named parsed)
    (match parsed 
      [`(named ,(? string? name) ,thing)
       (write-tagged-thing thing name)]
      [other (error 'write-named "not a named thing: ~s" parsed)]))
  
  (define (write-tagged-thing thing [name #f])
    (define tag (first thing))
    (write-byte (tag-symbol->num tag) out-port)
    (when name
      (write-string name))
    (write-untagged-thing thing))
  
  (define (write-untagged-thing thing)   
    (match thing
      [`(i8 ,num) (write-bytes 
                   (list->bytes (list num))
                   out-port)]
      [`(i16 ,num) (write-numeric num 2)]
      [`(i32 ,num) (write-numeric num 4)]
      [`(i64 ,num) (write-numeric num 8)]
      [`(f32 ,num) (write-bytes (real->floating-point-bytes num 4 #t)
                                out-port)]
      [`(f64 ,num) (write-bytes (real->floating-point-bytes num 8 #t)
                                out-port)]
      [`(bytearray ,bytes)
       (write-numeric (bytes-length bytes) 4)
       (write-bytes bytes out-port)]
      [`(string ,str)
       (write-string str)]
      [`(compound . ,nameds)
       (for-each write-named nameds)
       (write-byte 0 out-port)]
      [`(list ,tag-sym . ,things)
         (unless (andmap (lambda (x) (eq? (first x) tag-sym))
                         things)
           (error 'write-thing 
                  "not all things in uniform list had tag ~s" tag-sym))
         (write-byte (tag-symbol->num tag-sym) out-port)
         (write-numeric (length things) 4)
         (for-each write-untagged-thing things)]))
  
  ;; write the given number in the given number of bytes 
  ;; to the output port
  (define (write-numeric number num-bytes)
    (write-bytes (integer->integer-bytes number num-bytes #t #t)
                 out-port))
  
  ;; write the (2-byte) length and the string's bytes to
  ;; the output port
  (define (write-string str)
    (write-numeric (string-length str) 2)
    (write-bytes (string->bytes/utf-8 str)
                 out-port))
  
  
  (write-named parsed)
  (void))





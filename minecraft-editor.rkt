#lang racket


(require "data-skeleton.rkt"
         "minecraft-mcr-reader.rkt"
         rackunit)

(define CHUNKDHORIZ 16)
(define CHUNKDX CHUNKDHORIZ)
(define CHUNKDY 128)
(define CHUNKDZ CHUNKDHORIZ)


(define REGIONDHORIZ 32)
(define MAX-CHUNK-IDX (- REGIONDHORIZ 1))

(define (posn->chunk a)
  (inexact->exact (floor (/ a CHUNKDHORIZ))))

(define (chunk->region a)
  (floor (/ a REGIONDHORIZ)))

(define (chunk->rel-chunk a)
  (modulo a REGIONDHORIZ))

(define maker-fun? (-> nat? nat? nat? nat?))
                  


(provide CHUNKDX CHUNKDY CHUNKDZ REGIONDHORIZ block-names
         block-name->num
         get-field/chain
         set-field/chain)

(provide/contract [posn->chunk (-> real? integer?)]
                  [chunk->region (-> integer? integer?)]
                  [chunk->rel-chunk (-> integer? integer?)]
                  [chunk-read/dir (-> path-string?
                                      integer?
                                      integer?
                                      blank-compound-mc-thing?)]
                  [chunk-overwrite/dir (-> path-string?
                                           integer?
                                           integer?
                                           blank-compound-mc-thing? 
                                           void?)]
                  [raw-nums-read (-> input-port? 
                                     bytes?)]
                  [raw-nums->blocks (-> port?
                                        integer?
                                        integer?
                                        mc-thing?)]
                  [regular-byte-ref (-> bytes? nat? nat? nat? nat?)]
                  [regular-byte-set! (-> bytes? nat? nat? nat? nat? void?)]
                  [half-byte-ref (-> bytes? nat? nat? nat? nat?)]
                  [half-byte-set! (-> bytes? nat? nat? nat? nat? void?)]
                  [wrap-chunk (-> bytes? bytes? bytes? bytes? 
                                  integer? integer? nat?
                                  mc-thing?)]
                  [generate-chunk
                   (-> maker-fun?
                       maker-fun?
                       maker-fun?
                       integer?
                       integer?
                       nat?
                       mc-thing?)]
                  
                  [blocks-display (-> mc-thing? port? void?)]
                  [block-bytes-display (-> bytes? port? void?)]
                  
                  

                  [maker->block-bytes (-> (-> nat? nat? nat? nat?)
                                          bytes?)])


;; given the save directory and the desired chunk indices, return
;; the chunk.  
(define (chunk-read/dir directory-name chunk-x chunk-z)
  (define region-file (check-and-find-region-file directory-name chunk-x chunk-z))
  (define rel-cx (chunk->rel-chunk chunk-x))
  (define rel-cz (chunk->rel-chunk chunk-z))
  (chunk-read region-file rel-cx rel-cz))

;; given the save directory and the desired chunk indices and the new chunk, overwrite
;; the existing chunk
(define (chunk-overwrite/dir directory-name chunk-x chunk-z new-chunk)
  (define region-file (check-and-find-region-file directory-name chunk-x chunk-z))
  (define rel-cx (chunk->rel-chunk chunk-x))
  (define rel-cz (chunk->rel-chunk chunk-z))
  (define xpos (get-field/chain new-chunk '("" "Level" "xPos")))
  (define zpos (get-field/chain new-chunk '("" "Level" "zPos")))
  (unless (and (equal? xpos `(i32 ,chunk-x))
               (equal? zpos `(i32 ,chunk-z)))
    (error 'chunk-overwrite/dir
           "trying to overwrite region ~s,~s with a chunk labeled ~s, ~s"
           chunk-x chunk-z xpos zpos))
  (chunk-overwrite region-file rel-cx rel-cz new-chunk))

;; given a directory and a pair of absolute chunk numbers, return
;; the path to the file containing that chunk.
(define (check-and-find-region-file directory-name chunk-x chunk-z)
  (define region-directory (build-path directory-name "region"))
  (unless (directory-exists? region-directory)
    (raise-type-error 'read-chunk/dir "name of directory containing region subdirectory" 0
                      directory-name chunk-x chunk-z))
  (define rx (chunk->region chunk-x))
  (define rz (chunk->region chunk-z))
  (define region-file (build-path region-directory (format "r.~s.~s.mcr" rx rz)))
  (unless (file-exists? region-file)
    (error 'read-chunk/dir "desired region file ~s doesn't exist in the region directory"
           region-file))
  region-file)


;; convert x, y, and z into an index
(define (xyz->idx x y z)
  (+ y (* z CHUNKDY) (* x CHUNKDY CHUNKDZ)))



;; return the byte corresponding to x, y, and z in the bytes
(define (regular-byte-ref bytes x y z)
  (bytes-ref bytes (xyz->idx x y z)))

;; mutate the byte corresponding to x, y, and z in the bytes
(define (regular-byte-set! bytes x y z new-val)
  (bytes-set! bytes (xyz->idx x y z) new-val))

;; return the half-byte corresponding to x, y, and z in the
;; bytes
(define (half-byte-ref bytes x y z)
  (define idx (xyz->idx x y z))
  (define idx/b (arithmetic-shift idx -1))
  (define idx/o (bitwise-and idx 1))
  (define the-byte (bytes-ref bytes idx/b))
  (match idx/o
    [0 (bitwise-and the-byte #xf)]
    [1 (arithmetic-shift the-byte -4)]))

;; mutate the half-byte corresponding to x, y, and z in the
;; bytes
(define (half-byte-set! bytes x y z new-val)
  (unless (and (<= 0 new-val) (< new-val 16))
    (error 'half-byte-set! "can't handle new-val ~s (out of range)" new-val))
  (define idx (xyz->idx x y z))
  (define idx/b (arithmetic-shift idx -1))
  (define idx/o (bitwise-and idx 1))
  (define the-byte (bytes-ref bytes idx/b))
  (define new-byte
    (match idx/o
      [0 (bitwise-ior (bitwise-and the-byte #xf0) new-val)]
      [1 (bitwise-ior (bitwise-and the-byte #x0f) (arithmetic-shift new-val 4))]))
  (bytes-set! bytes idx/b new-byte))




;; given functions generating blocks, skylight, and extra data, and chunk-x
;; and chunk-y coordinates, generate a new compound object representing a block.
(define (generate-chunk block-maker
                        skylight-maker
                        extra-data-maker 
                        absolute-chunk-x 
                        absolute-chunk-z
                        update-tick)
  (define max-idx (* CHUNKDX CHUNKDY CHUNKDZ))
  (define block-bytes (maker->block-bytes block-maker))
  (define skylight-bytes (make-bytes (/ max-idx 2) 0))
  (define blocklight-bytes (make-bytes (/ max-idx 2) 0))
  (define extra-data-bytes (make-bytes (/ max-idx 2) 0))
  (for* ([y (in-range CHUNKDY)]
         [z (in-range CHUNKDZ)]
         [x (in-range CHUNKDX)])
    (define idx (xyz->idx x y z))
    (half-byte-set! blocklight-bytes x y z 0)
    (half-byte-set! extra-data-bytes x y z
                    (extra-data-maker x y z))
    (half-byte-set! skylight-bytes x y z 
                    (skylight-maker x y z)))  
  (wrap-chunk block-bytes skylight-bytes blocklight-bytes extra-data-bytes
              absolute-chunk-x 
              absolute-chunk-z
              update-tick))

(define (maker->block-bytes maker)
  (define block-bytes (make-bytes (* CHUNKDX CHUNKDY CHUNKDZ) 0))  
  ;; top layer is all air (already)
  ;; bottom layer all bedrock:
  (for* ([x (in-range CHUNKDX)]
         [z (in-range CHUNKDZ)])
    (regular-byte-set! block-bytes x 0 z 7))
  ;; other blocks are determined by user:
  (for* ([y (in-range 1 (- CHUNKDY 1))]
         [z (in-range CHUNKDZ)]
         [x (in-range CHUNKDX)])
    (define idx (xyz->idx x y z))
    (regular-byte-set! block-bytes x y z (maker x y z)))
  block-bytes)


;; given byte-arrays and other pieces, assemble a chunk:
;; NB: x and z must be *absolute*, not relative to the region.
(define (wrap-chunk block-bytes 
                    skylight-bytes
                    blocklight-bytes
                    extra-data-bytes
                    absolute-chunk-x
                    absolute-chunk-z
                    update-tick)
  (define num-cells (* CHUNKDX CHUNKDY CHUNKDZ))
  (unless (and (= (bytes-length block-bytes) num-cells)
               (= (bytes-length skylight-bytes) (/ num-cells 2))
               (= (bytes-length blocklight-bytes) (/ num-cells 2))
               (= (bytes-length extra-data-bytes) (/ num-cells 2)))
    (error 'wrap-chunk "one of the byte arrays had the wrong size!"))
  ;; compute the heightmap from the blocks:
  (define heightmap (make-bytes (* CHUNKDX CHUNKDZ) 0))
  (for* ([x (in-range CHUNKDX)]
        [z (in-range CHUNKDZ)])
    (let loop ([y (- CHUNKDY 2)])
      (when (< y 0)
        (error 'generate-block "got all the way to the bottom without finding any blocks!"))
      (match (regular-byte-ref block-bytes x y z) 
        [0 (loop (- y 1))]
        [other (bytes-set! heightmap (+ z (* 16 x)) 
                           (+ y 1))])))
  `(compound
    (named
     ""
     (compound
      (named 
       "Level"
       (compound
        (named "Data" (bytearray ,extra-data-bytes))
        (named "Entities" (list compound))
        (named "LastUpdate" (i64 ,update-tick))
        (named "xPos" (i32 ,absolute-chunk-x))
        (named "zPos" (i32 ,absolute-chunk-z))
        (named "TileEntities" (list i8))
        (named "TerrainPopulated" (i8 0))
        (named "SkyLight" (bytearray ,skylight-bytes))
        (named "HeightMap" (bytearray ,heightmap))
        (named "BlockLight" (bytearray ,blocklight-bytes))
        (named "Blocks" (bytearray ,block-bytes))))))))


(define ((bytes-of-len len) b)
  (and (bytes? b) (= (bytes-length b) len)))



(define (blocks chunk)
  (get-field/chain chunk '("" "Level" "Blocks")))

;; READING/WRITING BLOCKS TO LINES OF STRINGS

;; given a chunk and a port, display the blocks in the 
;; chunk to the port (as lines of numbers)
(define (blocks-display chunk port)
  (define cblocks (second (blocks chunk)))
  (block-bytes-display cblocks port))


;; given the bytes representing a block array and an output port,
;; write lines of numbers to the port
(define (block-bytes-display block-bytes out-port)
  (for* ([y (in-range CHUNKDY)]
         [x (in-range CHUNKDX)])
    (for ([z (in-range CHUNKDZ)])
      (fprintf out-port "~s " (regular-byte-ref block-bytes x y z)))
    (fprintf out-port "\n")))



(define (raw-nums->blocks port absolute-chunk-x absolute-chunk-y)
  (define block-bytes (raw-nums-read port))
  (define max-idx (* CHUNKDX CHUNKDY CHUNKDZ))
  ;; looks like it's okay to just use zeros here...
  (define skylight-bytes (make-bytes (/ max-idx 2) 0))
  (define blocklight-bytes (make-bytes (/ max-idx 2) 0))
  (define extra-data-bytes (make-bytes (/ max-idx 2) 0))
  (wrap-chunk block-bytes skylight-bytes blocklight-bytes extra-data-bytes
              absolute-chunk-x absolute-chunk-y (current-seconds)))

;; read an array of blocks from stdin
(define (raw-nums-read port)
  (define blocks (make-bytes (* CHUNKDX CHUNKDY CHUNKDZ)))
  (for* ([y (in-range CHUNKDY)]
         [x (in-range CHUNKDX)])
    (define line (read-line port))
    (when (eof-object? line)
      (error 'raw-nums-read
             "ran out of lines, expecting inputs for x=~s, y=~s\n" x y))
    
    (match-let ([l (discard-empties (regexp-split #px"\\s+" line))])
      (unless (= (length l) CHUNKDZ)
        (error 'raw-nums-read
               "line contained ~s element(s), rather than expected ~s: ~s"
               (length l) CHUNKDZ line))
      (for ([n (in-list l)] [z (in-naturals)])
        (define num (string->number n))
        (unless (and (exact-nonnegative-integer? num)
                     (<= 0 num 256))
          (error 'raw-nums-read
                 "line contained illegal element, expecting number 0<=n<128, got: ~s"
                 n))
        (regular-byte-set! blocks x y z num))))
  blocks)

(define (discard-empties l)
  (filter (lambda (s) (not (string=? "" s))) l))



(define block-names
  '#("Air"
     "Stone"
     "Grass Block"
     "Dirt"
     "Cobblestone"
     "Wooden Planks"
     "Saplings"
     "Bedrock"
     "Water"
     "Stationary water"
     "Lava"
     "Stationary lava"
     "Sand"
     "Gravel"
     "Gold Ore"
     "Iron Ore"
     "Coal Ore"
     "Wood"
     "Leaves"
     "Sponge"
     "Glass"
     "Lapis Lazuli Ore"
     "Lapis Lazuli Block"
     "Dispenser"
     "Sandstone"
     "Note Block"
     "Bed"
     "Powered Rail"
     "Detector Rail"
     "Sticky Piston"
     "Cobweb"
     "Tall Grass"
     "Dead Bush"
     "Piston"
     "Piston Extension"
     "Wool"
     "Block moved by Piston"
     "Dandelion"
     "Rose"
     "Brown Mushroom"
     "Red Mushroom"
     "Block of Gold"
     "Block of Iron"
     "Double Slabs"
     "Slabs"
     "Bricks"
     "TNT"
     "Bookshelf"
     "Moss Stone"
     "Obsidian"
     "Torch"
     "Fire"
     "Monster Spawner"
     "Wooden Stairs"
     "Chest"
     "Redstone Wire"
     "Diamond Ore"
     "Block of Diamond"
     "Crafting Table"
     "Wheat Seeds"
     "Farmland"
     "Furnace"
     "Burning Furnace"
     "Sign Post"
     "Wooden Door"
     "Ladders"
     "Rails"
     "Cobblestone Stairs"
     "Wall Sign"
     "Lever"
     "Stone Pressure Plate"
     "Iron Door"
     "Wooden Pressure Plate"
     "Redstone Ore"
     "Glowing Redstone Ore"
     "Redstone Torch (\"off\" state)"
     "Redstone Torch (\"on\" state)"
     "Stone Button"
     "Snow"
     "Ice"
     "Snow Block"
     "Cactus"
     "Clay Block"
     "Sugar Cane"
     "Jukebox"
     "Fence"
     "Pumpkin"
     "Netherrack"
     "Soul Sand"
     "Glowstone Block"
     "Portal"
     "Jack-O-Lantern"
     "Cake Block"
     "Redstone Repeater (\"off\" state)"
     "Redstone Repeater (\"on\" state)"
     "Locked Chest"
     "Trapdoor"
     "Hidden Silverfish"
     "Stone Bricks"
     "Huge Brown Mushroom"
     "Huge Red Mushroom"
     "Iron Bars"
     "Glass Pane"
     "Melon"
     "Pumpkin Stem"
     "Melon Stem"
     "Vines"
     "Fence Gate"
     "Brick Stairs"
     "Stone Brick Stairs"
     "Mycelium"
     "Lily Pad"
     "Nether Brick"
     "Nether Brick Fence"
     "Nether Brick Stairs"
     "Nether Wart"
     "Enchantment Table"
     "Brewing Stand"
     "Cauldron"
     "End Portal"
     "End Portal Frame"
     "End Stone"
     "Dragon Egg")
)

(define block-name-hash
  (for/fold ([ht (hash)])
    ([i (in-naturals)]
     [name (in-vector block-names)])
    (hash-set ht name i)))

(define (block-name->num name)
  (hash-ref block-name-hash name))

(check-equal? (block-name->num "Tall Grass") 31)
(check-equal? (block-name->num "Furnace") 61)
(check-equal? (block-name->num "Dragon Egg") 122)
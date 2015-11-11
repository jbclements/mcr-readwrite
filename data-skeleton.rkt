#lang racket



(provide  mc-thing?
          blank-compound-mc-thing?
          nat?
          named-mc-thing?
          mc-thing->skeleton)

(provide (contract-out [named-mc-thing->mc-thing (-> named-mc-thing? mc-thing?)]
                       [mc-thing->named-mc-thing (-> mc-thing? named-mc-thing?)]))

;; a named-mc-thing is (list 'named string thing)
;; an mc-thing is either
;; `(i8 ,number),
;; `(i16 ,number),
;; `(i32 ,number),
;; `(i64 ,number),
;; `(f32 ,number),
;; `(f64 ,number),
;; `(bytearray ,bytes),
;; `(string ,string),
;; `(list ,symbol ,mc-thing ...)
;; `(compound ,named-mc-thing ...)




(define nat? exact-nonnegative-integer?)

(define-values (named-mc-thing? mc-thing?)
  (flat-murec-contract 
   ([named-mc-thing? (list/c 'named string? mc-thing?)]
    [mc-thing? (or/c (cons/c 'compound (listof named-mc-thing?))
                     (cons/c 'list 
                             (cons/c symbol? (listof mc-thing?)))
                     (list/c 'i8 integer?)
                     (list/c 'i16 integer?)
                     (list/c 'i32 integer?)
                     (list/c 'i64 integer?)
                     (list/c (symbols 'f32 'f64 'bytearray 'string)
                             any/c))])
   (values named-mc-thing? mc-thing?)))

(define (mc-thing->skeleton obj)
  (match obj
    [`(compound . ,nameds)
     `(compound . ,(map named-mc-thing->skeleton nameds))]
    [`(list ,tag . ,objs)
     `(list ,tag . ,(map mc-thing->skeleton objs))]
    [`(,othertag ,data)
     `(,othertag omitted)]
    [other other]))

(define (named-mc-thing->skeleton named)
  (match named
    [`(named ,name ,val)
     `(named ,name ,(mc-thing->skeleton val))]))



(define blank-compound-mc-thing?
  (list/c 'compound
          (list/c 'named "" mc-thing?)))


(define (compound-thing->names thing)
  (match thing
    [(cons 'compound named-things)
     (map second named-things)]))

(provide/contract                  
 [get-field (-> mc-thing? 
                string?
                mc-thing?)]
 [set-field (-> mc-thing? 
                string?
                (-> mc-thing? mc-thing?)
                mc-thing?)]
 [set-field/chain (-> mc-thing? 
                      (listof string?)
                      (-> mc-thing? mc-thing?)
                      mc-thing?)]
 [get-field/chain (-> mc-thing?
                      (listof string?)
                      mc-thing?)]
 [compound-thing->names (-> mc-thing? (listof string?))])

;; dig through a sequence of named fields 
(define (get-field compound name)
  (match compound
    [`(compound . ,fields)
     (let loop ([fields fields])
       (cond [(empty? fields) (error 'get-foo "field not found: ~s" name)]
             [else (match (first fields)
                     [`(named ,this-name ,val)
                      (cond [(string=? name this-name) val]
                            [else (loop (cdr fields))])]
                     [other (error 'get-field "not a named field: ~e" other)])]))]
    [other (error 'get-foo "not a compound object")]))

;; functionally update a named field within a compound object
(define (set-field compound name thunk)
  (match compound
    [`(compound . ,fields)
     (cons 
      'compound
      (let loop ([fields fields])
        (cond [(empty? fields) (error 'get-foo "field not found: ~s" name)]
              [else (match (first fields)
                      [`(named ,this-name ,val)
                       (cond [(string=? name this-name) (cons `(named ,this-name 
                                                                      ,(thunk val))
                                                              (rest fields))]
                             [else (cons (first fields)
                                         (loop (rest fields)))])]
                      [other (error 'get-foo "not a named field: ~s" other)])])))]
    [other (error 'get-foo "not a compound object")]))








;; find a named field given a list of field names to navigate down through
(define (get-field/chain val lon)
  (cond [(empty? lon) val]
        [(get-field/chain (get-field val (first lon)) (rest lon))]))



(define (set-field/chain compound name-list thunk)
  (cond [(empty? (rest name-list))
         (set-field compound (first name-list) thunk)]
        [else (set-field compound (first name-list)
                         (lambda (elt) (set-field/chain elt (rest name-list) thunk)))]))


(define (named-mc-thing->mc-thing m)
  `(compound ,m))

(define (mc-thing->named-mc-thing m)
  (match m
    [(list 'compound datum) datum]
    [else (error mc-thing->named-mc-thing "expected mc-thing of compound form, got: ~s" m)]))
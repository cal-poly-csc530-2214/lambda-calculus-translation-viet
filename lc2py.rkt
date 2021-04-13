#lang racket
 
(require rackunit)
 
#|LC	 	=	 	num
                |	 	id
                |	 (	(/ id => LC)
                |	 	(LC LC)
                |	 	(+ LC LC)
                |	 	(* LC LC)
                |	 	(ifleq0 LC LC LC)
                |	 	(println LC)|#
 
(define (my-read str)
  (call-with-input-string str read)) 

(define (translate stx)
  (cond [(number? stx) stx]
        [(symbol? stx) stx]
        [(and (list? stx)
              (= 4 (length stx))
              (eq? (first stx) '/)
              (symbol? (second stx))
              (eq? (third stx) '=>))
         (list (second stx)
               '=
               (translate (fourth stx)))
         ]
        [(and (list? stx)
              (= 2 (length stx))
              (eq? (first stx) 'println)
              )
         (list 'print "(" (translate (second stx)) ")") 
         ]
        [(and (list? stx)
              (= 2 (length stx))
              )
         (list (translate (first stx)) (translate (second stx)))
         ]
        [(and (list? stx)
              (= 3 (length stx))
              (or (eq? (first stx) '+) (equal? (first stx) '*))
              )
         (list (translate (second stx)) (first stx) (translate (third stx)))
         ]
        [(and (list? stx)
              (= 4 (length stx))
              (eq? (first stx) 'ifleq0)
              )
         (list (translate (third stx)) 'if (translate (second stx)) 'else (translate (fourth stx)))
         ]
        [else (error 'ta "bad input: ~v\n" stx)]
        )
  )

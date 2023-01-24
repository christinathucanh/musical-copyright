;Project "Musical Copyright" follows in lawyer Damien Rihel and technologist Noah Rubins' footsteps and write a small recursive program capable of enumerating write all the songs of a given length drawn from a set of notes. The project briefly then reflect on the nature of computers, copyright, and creativity.
; Author: Anh Thuc (Christina) Vu 
; Date: 2022-10-12
; PART 1 
(import music)
; all-pairs: list of pairs?
; v: any 
; lst: list? 
; all-pairs v tail: recurse on the tail of the list 
; Return a list of pairs of values where the first element is v and the second element is a value from lst
(define all-pairs
    (lambda (v lst)
        (match lst
            [null null]
            [(cons head tail) (cons (pair v head) (all-pairs v tail))])))
(all-pairs "q" (range 6)) ; all-parts function 
(test-case "all-pairs" equal? (all-pairs "q" (range 6)) (list (cons "q" 0) (cons "q" 1) (cons "q" 2) (cons "q" 3) (cons "q" 4) (cons "q" 5)) )

;cartesian-product: list of pairs? 
; l1, l2: list? 
; cartesian-product tail l2: recurse over the tail and l2 
; Returns a list of pairs (pair x y) where x is drawn from l1 and y is drawn from l2.
(define cartesian-product
    (lambda (l1 l2)
        (match l1  
            [null null]
            [(cons head tail) (append (all-pairs head l2) (cartesian-product tail l2))])))
(cartesian-product (range 3) (list "a" "b"))  
(test-case "cartesian-product" equal? (cartesian-product (range 3) (list "a" "b")) (list (cons 0 "a") (cons 0 "b") (cons 1 "a") (cons 1 "b") (cons 2 "a") (cons 2 "b")) )

; all-two-note-songs: list of pairs? 
; notes: numbers? 
; Produces all the possible two note songs drawn from the list of provided notes
(define all-two-note-songs
    (lambda (notes) 
        (let ([all-notes (cartesian-product notes notes)]
              [notes-sequence (lambda (n) (seq (note (car n) qn) (note (cdr n) qn)))])
              (map notes-sequence all-notes))))

; two-note-example: sequence of notes 
; Demonstrate that all-two-note-songs works with note C and A. 
(define two-note-example (all-two-note-songs (list 60 69)))
two-note-example
; --------------------------------------
"PART 2"
; cons-all: list of pairs ? 
; x: any?
; lsts: list of lists? 
; Return lsts but with x added to the front of every list.
; cons-all x tail: recurse over tail 
(define cons-all 
    (lambda (x lst)
        (match lst
            [null null]
            [(cons head tail) (cons (cons x head) (cons-all x tail))])))

(cons-all 0 (list (list 1 2)
                   (list 3 4 5)
                   (list 6 7))) 

; combinations: list of pairs? 
; lsts : list of lists? 
; Returns lsts but with x added to the front of every list.
(define combinations    
    (lambda (lsts)
        (match lsts
            [null (list null)]
            [(cons head tail) (if (null? head)
                                   null   
                                   (append (cons-all (car head) (combinations tail)) 
                                           (combinations (cons (cdr head) tail))))])))

 (combinations (list (list 1 2) (list 3 4 5) (list 6 7) (list 4 5 6 7)))

; all-songs: list of pairs
; n: any 
; notes: numbers? 
; combinations (make-list n notes): recurse over the pairs in the list 
"all-songs"
(define all-songs
    (lambda (n notes)
        (let ([more-notes (lambda (x) (note x qn))]
              [sequence (lambda (y) (apply seq y))])
             (map sequence (combinations (make-list n (map more-notes notes)))))))


;Demonstrate that all-songs works with  the notes C (MIDI note 60), B♭ (MIDI note 58), and F (MIDI note 65).
(define five-note-example 
(all-songs 5 (list 60 58 65)))
five-note-example 
; ------------------------------------





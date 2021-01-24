;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lesson_15_HW) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A Bit is one of
; - "0"
; - "1"
; repr. a bit.
(define B-0 "0")
(define B-1 "1")
"#;"
(define (bit-templ b)
  (cond [(string=? b "0") ...]
        [(string=? b "1") ...]))
 
; A BitString is a String over Bits
; repr. a bitvector (which is a technical term).
(define BS-0 "")
(define BS-1 "001")
(define BS-2 "101")
"#;"
(define (bitstring-templ bs)
  (... bs ...))

;; 1a
;; Boolean -> Bit
;; Output 1 for #t and 0 for #f
(check-expect (boolean->bit #t) "1")
(check-expect (boolean->bit #f) "0")
(define (boolean->bit bool)
  (if (boolean=? #t bool)
      "1"
      "0"))

;; LOB -> BitString
;; Output a bitString depending on the LOB
(check-expect (bl2bs (list #f #f #t)) BS-1)
(check-expect (bl2bs (list #t #f #t)) BS-2)
(check-expect (bl2bs '()) BS-0)
(define (bl2bs lob)
  (implode (map boolean->bit lob)))

;; 1b
;; Bit -> Boolean
;; Converts a bit to a boolean
(check-expect (bit->boolean "1") #t)
(check-expect (bit->boolean "0") #f)
(define (bit->boolean bit)
  (or (string=? "1" bit)
      #f))

;; BitString -> LOB
;; Output a LOB based on the bitString
(check-expect (bs2bl BS-1) (list #f #f #t))
(check-expect (bs2bl BS-2) (list #t #f #t))
(check-expect (bs2bl BS-0) '())
(define (bs2bl bit-str)
  (map bit->boolean (explode bit-str)))

;; 1c
; LOX LOX [X X -> Boolean] -> Boolean
; return #t if the two lists are equal
(check-expect (list=? '()            '()           string=?)  #t)
(check-expect (list=? (list 1 2 3)    (list 1 2 3) =)         #t)
(check-expect (list=? (list #t #f #t) (list #f #t) boolean=?) #f)
(define (list=? l1 l2 is=?)
  (cond [(empty? l1) (empty? l2)]
        [(cons?  l1) (and (is=? (first l1) (first l2))
                          (list=? (rest l1) (rest l2) is=?))]))

;; 1d
;; LOB -> Boolean
;; Takes in a LOB and converts it to a BitString; then it converts the bitString back to a LOB and compares the lists
(check-expect (test-double-conv '()) #t)
(check-expect (test-double-conv (list #t #f #f)) #t)
(check-expect (test-double-conv (list #f #f #f)) #t)
(define (test-double-conv lob)
  (list=? lob (bs2bl (bl2bs lob)) boolean=?))

; Exercise 2a
; LOS (one contains the number of words)
; LOS (other contains the literall text)
; [List-of-String] -> [List-of-Numbers]
; Output the frequency of the LOS in the LON
;(check-expect (word-counts (list "I" "really" "really" "don't" "know")) (list 1 2 2 1 1))
;(define (word-counts los)
;  (cond
;    [(empty? los) '()]
;    [(cons? los) (cons (if (string=? (first los) (rest los))
;                           2
;                           1)
;                       (word-counts (rest los)))]))

;; Ex. 3a
;; Foldr returns a value from the lox deoending on the func
;; [X, Y] {X -> Y] List-of-x -> List-of-y
;; Make a list based on func
(check-expect (my-map add1 (list 4 8)) (list 5 9))
(define (my-map func lox)
  (foldr (lambda (x y) (cons (func x) y)) '() lox))

;; Ex. 3b
;; [x] Lox x -> Boolean
;; Return true if x is in lox
(check-expect (my-ormap number? (list 3 4 "hi")) #true)
(check-expect (my-ormap number? (list "hi" "ola")) #false)
(define (my-ormap func lox)
  (foldr (lambda (x y) (or (func x) y)) #false lox))

;; Exercise 2
;; One list is for recurring and one is for checking
;; 2a
;; Helper-helper
;; Contains and the # of times
;; Empty case would be 0
;; Use list template
;; Think abt how to combine it

;; String LOS -> Number
;; Check how many times the given string is in the LOS
(check-expect (helper-counts "really" (list "I" "really" "really" "don't" "know")) 2)
(check-expect (helper-counts "I" (list "I" "really" "really" "don't" "know")) 1)
(check-expect (helper-counts "I" '()) 0)
(define (helper-counts str los)
  (cond
    [(empty? los) 0]
    [(cons? los) (if (string=? str (first los))
                     (+ (helper-counts str (rest los)) 1)
                     (helper-counts str (rest los)))]))

;; LOS LOS -> LON
;; Outputs how many times the ith string appears in the second LOS
(check-expect (helper-word-counts (list "I" "really" "really" "don't" "know") (list "I" "really" "really" "don't" "know")) (list 1 2 2 1 1))
(check-expect (helper-word-counts '() '()) '())
(define (helper-word-counts los los2)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (helper-counts (first los) los2)
                        (helper-word-counts (rest los) los2))]))

;; LOS -> List-of-Number
;; Output how many times the ith string is in list2
(check-expect (word-counts (list "I" "really" "really" "don't" "know")) (list 1 2 2 1 1))
(define (word-counts los)
  (cond
    [(empty? los) '()]
    [(cons? los) (helper-word-counts los los)]))

;; With the abstraction
;; String LOS -> Number
;; Check how many times the given string is in the LOS
(check-expect (helper-counts-2 "really" (list "I" "really" "really" "don't" "know")) 2)
(check-expect (helper-counts-2 "I" (list "I" "really" "really" "don't" "know")) 1)
(check-expect (helper-counts-2 "I" '()) 0)
(define (helper-counts-2 str los)
  (foldr (lambda (x y) (if (string=? str x)
                           (+ y 1)
                            y)) 0 los))

;; LOS LOS -> LON
;; Outputs how many times the ith string appears in the second LOS
(check-expect (helper-word-counts-2 (list "I" "really" "really" "don't" "know") (list "I" "really" "really" "don't" "know")) (list 1 2 2 1 1))
(check-expect (helper-word-counts-2 '() '()) '())
(define (helper-word-counts-2 los los2)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (helper-counts-2 (first los) los2)
                        (helper-word-counts-2 (rest los) los2))]))

;; LOS -> List-of-Number
;; Output how many times the ith string is in list2
(check-expect (word-counts-2 (list "I" "really" "really" "don't" "know")) (list 1 2 2 1 1))
(define (word-counts-2 los)
  (cond
    [(empty? los) '()]
    [(cons? los) (helper-word-counts-2 los los)]))

;; Exercise 2b
;; String -> LON
;; Return a list of numbers with the frequency of each character in the string
(check-expect (char-counts "really") (list 1 1 1 2 2 1))
(define (char-counts str)
  (word-counts (explode str)))

;; Exercise 2c
;; LOX -> Number
;; Output the number of times the given value is in the LOX
(check-expect (contains-1 "really" (list "i" "really" "really") string=?) 2)
(define (contains-1 str lox is=?)
  (foldr (lambda (x y) (if (is=? str x)
                           (+ y 1)
                           y)) 0 lox))

;; LOX LOX -> LON
;; Output the number of times the ith string is in the second lox
(check-expect (helper-counts-1 (list "1" "1" "2") (list "1" "1" "2") string=?) (list 2 2 1))
(define (helper-counts-1 lox lox2 is=?)
  (cond
    [(empty? lox) '()]
    [(cons? lox) (cons (contains-1 (first lox) lox2 is=?)
                       (helper-counts-1 (rest lox) lox2 is=?))]))

;; LOX -> LON
;; Output how many times each element repeats in the given list
(check-expect (counts (list 1 2 3 2 1) =) (list 2 2 1 2 2))
(define (counts lox is=?)
  (cond
    [(empty? lox) '()]
    [(cons? lox) (helper-counts-1 lox lox is=?)]))

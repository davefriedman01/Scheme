; This file contains an implementation in Chez Scheme of the programs presented in the following work.
;   Friedman, Daniel P. & Matthias Felleisen. (1996). The Little Schemer. 4th Ed. MIT Press.
;
; Load this file into Chez Scheme via
;   (load "exercises.ss") 

(car '(a b c))
(cdr '(a b c))
(cons 'a '(b c))
(null? '())
(eq? '1 '1)
(or (atom? 'a) (atom? '(a)))

(define square
  (lambda (n)
    (* n n)))

(define reciprocal
  (lambda (n)
    (if (= n 0) "oops!" (/ 1 n))))

;;;;;;;;;;;;;;;;;;;;
;
; LITTLE SCHEMER
;
;;;;;;;;;;;;;;;;;;;;

; ATOM a
;   a string of characters or a number
; LIST l
;   either the empty list '()
;   or a non-empty list of S-expressions
; LIST OF ATOMS lat
;   either the empty list '()
;   or
;     an atom (car lat)
;     and a lat (cdr lat)
; TUPLE tup
;   either the empty tup '()
;   or
;     a number (car tup)
;     and a tup (cdr tup)
; S-EXPRESSION
;   an atom or a list

; building numbers
;   constructor:              add1
;   terminal condition:       (null? tup)
;   terminal condition value: 0
;                             ((null? tup) 0)
; building lists
;   constructor:              cons
;   terminal condition:       (null? l)
;   terminal condition value: '()
;                             ((null? l) '())



;;;;;;;;;;;;;;;;;;;;

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

; firsts
;   input: null list or list of non-empty lists
;   output: a list composed of the first S-expression of each internal list
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons old (cons new (cdr lat))))
              (else (cons (car lat) (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new lat))
              (else (cons (car lat) (insertL new old (cdr lat)))))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new (cdr lat)))
              (else (cons (car lat) (subst new old (cdr lat)))))))))

;(define subst2
;  (lambda (new o1 o2 lat)
;    (cond
;      ((null? lat) '())
;      (else (cond
;              ((eq? (car lat) o1) (cons new (cdr lat)))
;              ((eq? (car lat) o2) (cons new (cdr lat)))
;              (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((or
                (eq? (car lat) o1)
                (eq? (car lat) o2))
                  (cons new (cdr lat)))
              (else
                (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

; multirember
;   removes all occurrences of an S-expression from a list of S-expressions
;
; example
;   (multirember 'cup '(coffee cup tea cup and hick cup))
; produces
;   (coffee tea and hick)
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
        ((eq? (car lat) a) (multirember a (cdr lat)))
        (else
          (cons (car lat) (multirember a (cdr lat)))))))))

; multiinsertR
;   inserts an S-expression to the right of
;   all occurrences of some S-expression in a list of S-expressions
;
; example
;   (multiinsertR 'fried 'fish '(chips and fish or fish and fried))
; produces
;   (chips and fish fried or fish fried and fried)
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
        ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
        (else
          (cons (car lat) (multiinsertR new old (cdr lat)))))))))

; multiinsertL
;   inserts an S-expression to the left of
;   all occurrences of some S-expression in a list of S-expressions
;
; example
;   (multiinsertL 'fried 'fish '(chips and fish or fish and fried))
; produces
;   (chips and fried fish or fried fish and fried)
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
        ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (else
          (cons (car lat) (multiinsertL new old (cdr lat)))))))))

; multisubst
;   replaces all occurrences of some S-expression
;   by some other S-expression
;   in a list of S-expressions
;
; example
;   (multisubst 'taco 'fish '(chips and fish or fish and fried))
; produces
;   (chips and taco or taco and fried)
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
              (else
                (cons (car lat) (multisubst new old (cdr lat)))))))))

(atom? 14)      ; #t
(atom? -3)      ; #t
(atom? 3.14159) ; #t

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(add1 67)       ; 68
(sub1 5)        ; 4
(sub1 0)        ; -1
(zero? 1492)    ; #f

; addition, nonnegative integer
;   m cannot be negative
;
;   adds unity to n as many times as
;   unity may be substracted from m
;   until m reachs null
;
; example
;   (o+ 46 12)
; produces
;   58
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
        (add1 (o+ n (sub1 m)))))))

; substraction, nonnegative integer
;   m cannot be negative
;
;   subtracts unity from n as many times as
;   unity may be substracted from m
;   until m reachs null
;
; example
;   (o- 14 3)
; produces
;   11
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
        (sub1 (o- n (sub1 m)))))))



;;;;;;;;;;;;;;;;;;;;
;
; REASONED SCHEMER
;
;;;;;;;;;;;;;;;;;;;;


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
          (cons (car lat) (multirember a (cdr lat)))
        )
      ))
    )
  )
)

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
        ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
        (else
          (cons (car lat) (multiinsertR new old (cdr lat)))
        )
      ))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;
;
; REASONED SCHEMER
;
;;;;;;;;;;;;;;;;;;;;


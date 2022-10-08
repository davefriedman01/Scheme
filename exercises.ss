; This file contains an implementation in Chez Scheme of the programs presented in the following work.
;   [SICP] Abelson, Harold & Gerald Jay Sussman. (1996). Structure and Interpretation of Computer Programs. 2nd Ed. MIT Press.
;   [SPL4] Dybvig, R. Kent. The Scheme Programming Language. 4th Ed. https://www.scheme.com/tspl4/.
;   [TLS4] Friedman, Daniel P. & Matthias Felleisen. (1996). The Little Schemer. 4th Ed. MIT Press.
;
; Load this file into Chez Scheme REPL via
;   (load "exercises.ss") 

;;;;;;;;;;;;;;;;;;;;
;
; TLS4
;
;;;;;;;;;;;;;;;;;;;;
;
; ATOM a
;   a string of characters or a number
; LIST l
;   either the empty list '()
;   or
;     an S-expression (car l)
;     and a list l (cdr l)
;
; S-EXPRESSION
;   an atom or a list
;
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
; NUMBER n
;   either the empty number 0
;   or
;     unity 1
;     and a number (sub1 n)
;
; building lists
;   constructor:              cons
;   terminal condition:       (null? l)
;   terminal condition value: '()
;                             ((null? l) '())
;   natural recursion:        (cdr lat)
;     the rest of a non-empty list is a list
;   questions to ask of a list
;     1. (null? lat)
;     2. else
;
; building tuples
;   constructor:              o+
;   terminal condition:       (null? tup)
;   terminal condition value: 0
;                             ((null? tup) 0)
;   natural recursion:        (cdr tup)
;     the rest of a non-empty tup is a tup
;   questions to ask of a tup
;     1. (null? tup)
;     2. else
;
; building numbers
;   constructor:              add1
;   terminal condition:       (zero? n)
;   terminal condition value: 0
;                             ((zero? n) 0)
;   natural recursion:        (sub1 n)
;   questions to ask of a number
;     (zero? n)
;     else
;
; C1
;   Recursion on a list of atoms lat: (null? lat) and else
;   Recursion on a number n:          (zero? n)   and else
; C4
;   Change at least one argument in recursion.
;   Change it closer to termination.
;   Test the changing argument in the termination condition.
;   Test null? with cdr
;   Test zero? with sub1

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
;   adds unity to n as many times as
;   unity may be substracted from m
;   until m reachs null
;   (rule: m cannot be negative)
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
;   subtracts unity from n as many times as
;   unity may be substracted from m
;   until m reachs null
;   (rule: m cannot be negative)
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

; addtup, nonegative integer
;   builds a number
;   by totaling all the numbers in a tup
;   (rule: no negative integers)
;
; terminal condition: ((null? tup) 0)
; natural recursion:  (addtup (cdr tup))
;
; example
;   (addtup '(1 2 3 4))
; produces
;   10
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
        (o+ (car tup) (addtup (cdr tup)))))))

; x, nonnegative integer
;   builds a number
;   by totaling n with itself m times
;   (rule: neither n nor m can be negative)
;   (technical exception: n may be negative when m is either 0 or 1)
;
; terminal condition: ((zero? m) 0)
; natural recursion:  (x n (sub1 m))
;
; example
;   (x 5 3)
; produces
;   15
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

;;;;;;;;;;;;;;;;;;;;
;
; REASONED SCHEMER
;
;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;
;
; SICP
;
;;;;;;;;;;;;;;;;;;;;
;
; DATA
;   stuff to be manipulated
; PROCEDURE
;   a description of a process
;   that which gives the rules for manipulating data
;   procedures themselves can be represented and manipulated as data
;
; EXPRESSION
;
; COMBINATION
;   an expression formed by delimiting a list of expressions within parentheses
;   in order to denote procedure application
;   the leftmost element in the list is called the operator
;   and the rest of the elements are called operands
;
; to evaluate a combination
;   1. evaluate the subexpressions of the combination
;   2. apply the procedure that is the value of the leftmost subexpression (the operator)
;      to the arguments that are the values of the other subexpressions (the operands)

(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(* 2.7 10)
(+ 21 35 12 7)
(* 25 4 12)
(+ (* 3 5) (- 10 6))
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

; NAMING
;   things that can be named are done so with `define`

(define size 2)
size
(* 5 size)
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference

; (GLOBAL) ENVIRONMENT

; SPECIAL FORMS
;   e.g., (define x 3)

; COMPOUND PROCEDURE DEFINITION
;   a technique of abstraction
;   by which a compound operation can be given a name
;
; general form
;   (define (<name> <parameters>) <body>)
;
;   <name>       a symbol to be associated with with the procedure definition in the environment
;   <parameters> the names used within the body of the procedure
;                to refer to the corresponding arguments of the procedure
;   <body>       a sequence of expressions that will yield the value of the procedure application
;                when the parameters are replaced by the actual arguments to which the procedure is applied
;
; two operations
;   to create a procedure without naming it
;   to name a procedure which has already been created

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(square 21)          ; 441
(square (+ 2 5))     ; 49
(square (square 3))  ; 81
(sum-of-squares 3 4) ; 25
(f 5)                ; 136

; SUBSTITUTION MODEL FOR PROCEDURE APPLICATION

; Applicative-Order Evaluation
; (f 5)
; (sum-of-squares (+ 5 1) (* 5 2))
; (+ (square 6) (square 10))
; (+ (* 6 6) (* 10 10))
; (+ 36 100)
; 136

; Normal-Order Evaluation
; expansions
; (f 5)
; (sum-of-squares (+ 5 1) (* 5 2))
; (+ (square (+ 5 1)) (square (* 5 2)))
; (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
; reductions
; (+ (* 6 6) (* 10 10))
; (+ 36 100)
; 136

; it can be show that
; for procedure applications that can be modeled using substitution
; and that yield legitimate values
; normal-order and applicative-order evaluation produce the same result

; CONDITIONAL EXPRESSION
;
; general form
;   (cond (<p1> <e1>)
;         (<p2> <e2>)
;         ...
;         (<pn> <en>))
;
; each clause (<p> <e>) is composed of
;   a predicate <p>
;   a (sequence of) consequent expressions <e>

; PREDICATE
;   a procedure that returns true or false
;
; primitive predicates
;   <
;   >
;   =

(define (abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))))

(define (abs x)
  (cond
    ((< x 0) (- x))
    (else x)))

; IF
;   SPECIAL FORM
;   a restricted conditional expression
;   to be used when there are two and only two cases in the case analysis
;
; Evaluation Rule
;   evaluate <predicate>
;   if <predicate> is true
;     then evaluate <consequent> and return its value
;   else
;     evaluate <alternative> and return its value
;
; general form
;   (if <predicate> <consequent> <alternative>)
;
;   <consequent> and <alternative> must be single expressions

(define (abs x)
  (if (< x 0) (- x) x))

; AND
;   COMPOUND PREDICATE
;   SPECIAL FORM
;     the subexpressions are not necessarily all evaluated
;
; Evaluation Rule
;   ...
;
; general form
;   (and <e1> ... <en>)

; OR
;   COMPOUND PREDICATE
;   SPECIAL FORM
;     the subexpressions are not necessarily all evaluated
;
; Evaluation Rule
;   ...
;
; general form
;   (or <e1> ... <en>)

; NOT
;   COMPOUND PREDICATE
;
; general form
;   (not <e>)

(define (>= x y)
  (or (> x y)
      (= x y)))

(define (>= x y)
  (not (< x y)))



;;;;;;;;;;;;;;;;;;;;
;
; SPL4
;
;;;;;;;;;;;;;;;;;;;;

(define square
  (lambda (n)
    (* n n)))

(define reciprocal
  (lambda (n)
    (if (= n 0) "oops!" (/ 1 n))))
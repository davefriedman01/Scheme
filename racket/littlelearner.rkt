(require malt)

;
; CHAPTER 0
;

(define pie 3.14)

pie ; 3.14

(define a-radius 8.4)

a-radius ; 8.4

(define an-area
  (* pie
    (* a-radius a-radius)))

an-area ; 221.5584 = 3.14 * 8.4 * 8.4

(define area-of-circle
  (lambda (r)
    (* pie
      (* r r))))

(define area-of-rectangle     ; a function with one formal `width` (a number) that
  (lambda (width)
    (lambda (height)
      (* width height))))

(area-of-rectangle 3.0)       ; produces a new function with one formal `height` (a number)
((area-of-rectangle 3.0) 4.0) ; 12

(define double-result-of-f    ; a function with one formal `f` (a function) that
  (lambda (f)
    (lambda (z)
      (* 2 (f z)))))

(define add3
  (lambda (x)
    (+ 3 x)))

(double-result-of-f add3)     ; produces a new function with one formal `z` (a number)
((double-result-of-f add3) 4) ; 14

(cond
  ((= pie 4) 28)
  ((< pie 4) 33)
  (else 17))                  ; 33

(define abs
  (lambda (x)
    (cond
      ((< x 0) (- 0 x))
      (else x))))

(define silly-abs
  (lambda (x)
    (let ((x-is-negative (< x 0)))
      (cond
        (x-is-negative (- 0 x))
        (else x)))))

; simple remainder function
;   the first argument `x` is a nonnegative number
;   the second argument `y` is a positive number
;
;   check whether x - y >= 0 (this is only possible when x >= y)
;     when x < y, x must be the remainder
;     when it is not the case that x < y, start again with x=x-y (remove y from x once) and y

(define remainder
  (lambda (x y)
    (cond
      ((< x y) x)                     ; base      case `(< x y)` and base value `x`
      (else (remainder (- x y) y))))) ; recursive case

; add two natural numbers (nonnegative integers that are constructed from the value 0 and the function add1)
(define add
  (lambda (n m)
    (cond
      ((zero? m) n)                     ; base      case `(zero? m)` and base value `n`
      (else (add1 (add n (sub1 m))))))) ; recursive case

;
; CHAPTER 1
;

; LINE_1
;            a function of one argument
;   result - a function of two arguments
;
;   x is known   - argument
;   w is unknown - parameter
;   b is unknown - parameter
;   w, b must be determined from a bunch of given values of x and y

(define line
  (lambda (x)
    (lambda (w b)
      (let ((y (+ (* w x) b)))
        y))))

; LINE_2
;            a function of one argument
;   result - a function of two arguments
;
;   x is known   - argument
;   w is unknown - parameter
;   b is unknown - parameter
;   w, b must be determined from a bunch of given values of x and y

(define line
  (lambda (x)
    (lambda (w b)
      (+ (* w x) b))))

; (line 8)
;
; (lambda (w b)
;   (+ (* w 8) b))

((line 8) 4 6) ; 38 = 8 * 4 + 6

; LINE-XS

(define line-xs
  (tensor 2.0 1.0 4.0 3.0))

; LINE-YS

(define line-ys
  (tensor 1.8 1.2 4.2 3.3))

; LINE

(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (ref theta 0) x) (ref theta 1)))))

((line 7.3) (list 1.0 0.0))

; same-as chart
;
; ((line 7.3) (list 1.0 0.0))
;
; ((lambda (theta)
;    (+ (* (ref theta 0) 7.3) (ref theta 1)))
;  (list 1.0 0.0))
;
; (+ (* (ref (list 1.0 0.0) 0) 7.3) (ref (list 1.0 0.0) 1))
;
; (+ (* 1.0 7.3) 0.0)
;
; 7.3

(scalar? 7.18) ; #t
(scalar? pi)   ; #t

(tensor 5.0 7.18 pi)
(tensor 2.0 1.0 4.0 3.0)
(tensor 8)

(tensor (tensor 7 6 2 5) (tensor 3 8 6 9) (tensor 9 8 4 5))

(tlen (tensor 17 12 91 67))                   ; 4
(tlen (tensor (tensor 3 2 8) (tensor 7 1 9))) ; 2

(tensor (tensor (tensor (tensor 8))))

(tensor (tensor 5 6 4) (tensor 9 1 1) (tensor 0 6 2))

(tensor (tensor (tensor 5) (tensor 6) (tensor 7)) (tensor (tensor 8) (tensor 9) (tensor 0)))

(tensor (tensor (tensor 8) (tensor 9)) (tensor (tensor 4) (tensor 7)))

; RANK_1

(define rank
  (lambda (t)
    (cond
      ((scalar? t) 0)                    ; base      case
      (else (add1 (rank (tref t 0))))))) ; recursive case

; SHAPE

(define shape
  (lambda (t)
    (cond
      ((scalar? t) (list))
      (else (cons (tlen t) (shape (tref t 0)))))))

(shape 9)                    ; '()
(shape (tensor 9 4 7 8 0 1)) ; '(6)

; RANK & RANKED
;
; a accumulator

(define rank
  (lambda (t)
    (ranked t 0)))

(define ranked
  (lambda (t a)
    (cond
      ((scalar? t) a)
      (else (ranked (tref t 0) (add1 a))))))
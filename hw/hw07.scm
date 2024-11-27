(define (square n) (* n n))

(define (pow base exp)
  (cond 
    ((zero? exp)  1)
    ((= exp 1)    base)
    ((even? exp)  (square (pow base (/ exp 2))))
    ((odd? exp)   (* base 
                     (square (pow base (/ (- exp 1) 2)))))
  ))

(define (repeatedly-cube01 n x)
  (if (zero? n)
      x
      (let ( (y (repeatedly-cube (- n 1) x)) )
        (* y y y))))

(define (repeatedly-cube n x)
  (define (helper count acc)
    (if (zero? count)
      acc
      (helper (- count 1) (* acc acc acc))
    )
  )
  (helper n x)
)

(define (cddr s) (cdr (cdr s)))

(define (cadr s) (car (cdr s) ))

(define (caddr s) (car (cddr s)))


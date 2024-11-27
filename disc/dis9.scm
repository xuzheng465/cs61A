(define with-list
    (list
        (list 'a 'b)
        'c
        'd
        (list 'e)    
    )
)

(define with-quote
    '((a b) c d (e))
 )


; 预定义各个部分
(define first             ; 定义 (a b)
  (cons 'a (cons 'b nil)))

(define second           ; 定义 c
  'c)

(define third            ; 定义 d
  'd)

(define last             ; 定义 (e)
  (cons 'e nil))

; 使用预定义构造最终列表
(define with-cons
  (cons first
        (cons second
              (cons third
                    (cons last
                          nil)))))


(define (pair-up s)
    (if (<= (length s) 3)
        (cons s nil)
        (cons (list (car s) (car (cdr s)))
            (pair-up (cdr (cdr s) )))
    )
)

(define (pair-up s)
    (define (build-pairs remaining)
        (let ((len (length remaining)))
            (cond 
                ((null? remaining) '())
                ((<= len 3) (list remaining))
                (else 
                    (let ((first (car remaining))
                          (second (car (cdr remaining)))
                          (rest (cdr (cdr remaining))))
                        (cons (list first second)
                              (build-pairs rest)))))))
    (build-pairs s))


(define (pair-up s)
    (define (append-pairs curr-pair rest-pairs)
        (if (null? curr-pair)
            rest-pairs
            (cons curr-pair rest-pairs)))
            
    (let ((len (length s)))
        (cond 
            ((null? s) '())
            ((<= len 3) (list s))
            (else 
                (cons (list (car s) (car (cdr s)))
                      (pair-up (cdr (cdr s))))))))

(define (pair-up s)
    (define (build-pairs remaining result)
        (let ((len (length remaining)))
            (cond 
                ((null? remaining) 
                    (reverse result))           ; 最后需要reverse
                ((<= len 3) 
                    (reverse (cons remaining result)))
                (else 
                    (build-pairs 
                        (cdr (cdr remaining))
                        (cons (list (car remaining) 
                                  (car (cdr remaining)))
                              result))))))
                              
    ; 如果不能用reverse，可以在构建时就保持正确顺序
    (define (build-pairs-forward remaining result)
        (let ((len (length remaining)))
            (cond 
                ((null? remaining) result)
                ((<= len 3) 
                    (append result (list remaining)))
                (else 
                    (build-pairs-forward
                        (cdr (cdr remaining))
                        (append result
                            (list (list (car remaining) 
                                      (car (cdr remaining))))))))))
                                      
    (build-pairs-forward s '()))
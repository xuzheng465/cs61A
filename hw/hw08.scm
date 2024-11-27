(define (ascending1? s)
  (cond 
    ((< (length s) 2)
     #t)
    (else
     (and (<= (car s) (car (cdr s))) (ascending1? (cdr s))))))

(define (ascending2? s)
    (cond
        ((null? s) #t)
        ((null? (cdr s)) #t)
        (else
            (and 
                (<= (car s) (car (cdr s)))
                (ascending? (cdr s))
            )
        )
    )
)

(define (ascending? s)
    (define (iter lst)
        (cond
            ((null? lst) #t)
            ((null? (cdr lst)) #t)
            ((> (car lst) (car (cdr lst))) #f)
            (else (iter (cdr lst)))
        )
    )
    (iter s)
)


(define (my-filter1 pred s)
    (cond
        ((null? s) '())
        ((pred (car s))
            (cons (car s) (my-filter pred (cdr s)))
        )
        (else
            (my-filter pred (cdr s))
        )
    )
)

(define (my-reverse s)
    (define (helper lst acc)
        (if (null? lst)
            acc
            (helper (cdr lst) (cons (car lst) acc))
        )
    )
    (helper s '())
)


(define (my-filter pred s)
    (define (helper lst acc)
        (cond
            ( (null? lst) acc )
            ( (pred (car lst))
                (helper (cdr lst) (cons (car lst) acc))
            )
            (else 
                (helper (cdr lst) acc)
            )
        )
    )
    (my-reverse (helper s '()))
)



(define (interleave1 lst1 lst2) 
    (define (helper l1 l2 acc)
        (cond
           (
            (and (null? l1) (null? l2))
            (my-reverse acc)
            )
           ( (null? l1) (helper l1 (cdr l2) (cons (car l2) acc)))
           ( (null? l2) (helper (cdr l1) l2 (cons (car l1) acc)))
           (else
            (helper (cdr l1) (cdr l2) (cons (car l2) (cons(car l1) acc)))
           )
        )
    )
    (helper lst1 lst2 '())
)

(define (interleave11 lst1 lst2)
    (cond
        ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else
            (cons (car lst1) (cons (car lst2) (interleave (cdr lst1) (cdr lst2))) )
        )
    )
)

(define (in-lst? el lst)
  (cond
    ((null? lst) #f)
    ((= el (car lst)) #t)
    (else (in-lst? el (cdr lst)))))


(define (no-repeats1 s)
    (define (helper lst acc)
        (cond
            ( (null? lst) (my-reverse acc) )
            (else
                (if (in-lst? (car lst) acc)
                    (helper (cdr lst) acc)
                    (helper (cdr lst) (cons (car lst) acc))
                  
                )
            )
        )    
    )

    (helper s '())
)

(define (no-repeats s)
  (cond ((null? s) '())  ; 处理空列表情况
        ((null? (cdr s)) s)  ; 处理单元素列表情况
        (else
         (cons (car s)  ; 保留当前元素
               (no-repeats  ; 递归处理剩余元素
                (filter (lambda (x) (not (= x (car s))))  ; 过滤掉与当前元素相等的项
                        (cdr s)))))))
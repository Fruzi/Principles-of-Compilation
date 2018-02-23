(define -
  (lambda x
    (if (null? (cdr x))
        (* (car x) -1)
        (+ (car x) (* (apply + (cdr x)) -1)))))

(define zero? (lambda (n) (= n 0)))
(define list (lambda x x))
(define null? (lambda (x) (eq? x '())))
(define number? rational?)

(define fold-right
  (lambda (f init l)
    (if (null? l)
        init
        (f (car l)
           (fold-right f init (cdr l))))))

(define append
  (lambda x
    (if (null? (cdr x))
        (car x)
        (letrec ((append2 (lambda (l1 l2)
                            (if (null? l1)
                                l2
                                (if (not (pair? l1))
                                    l1
                                    (cons (car l1) (append2 (cdr l1) l2)))))))
          (fold-right append2 '() x)))))

(define map
  (lambda (f . s)
    (letrec ((map1 (lambda (f s)
                     (if (null? s)
                         '()
                         (cons (f (car s))
                               (map1 f (cdr s))))))
             (maplist (lambda (f s)
                        (if (null? (car s))
                            '()
                            (cons (apply f (map1 car s))
                                  (maplist f (map1 cdr s)))))))
      (maplist f s))))

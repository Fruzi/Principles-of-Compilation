(load "qq.scm")

(define parse
  (lambda (e)
    (cond ((quote? e) `(const ,@(cdr e)))
          ((constant? e) `(const ,e))
          ((if3? e) (make-if3 e))
          ((or? e) (make-or e))
          ((lambda? e) (make-lambda e))
          ((seq? e) (make-seq e))
          ((applic? e) (make-applic e))
          ((set!? e) (make-set e))
          ((define? e) (make-define e))

          ((variable? e) `(var ,e))
          (else (errorf 'parser "Unknown form: ~a" e)))))

(define *reserved-words*
  '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote
    unquote-splicing quote set!))

(define constant?
  (let ((predicates (list null? vector? const?)))
    (lambda (e)
      (or (eq? e (void))
          (ormap (lambda (p?) (p? e))
                 predicates)))))

(define variable?
  (lambda (e)
    (not (or (pair? e)
             (member e *reserved-words*)))))

(define if3?
  (lambda (e)
    (and (keyword? e 'if)
         (or (= (length e) 3)
             (= (length e) 4)))))

(define or?
  (lambda (e)
    (keyword? e 'or)))

(define lambda?
  (lambda (e)
    (and (keyword? e 'lambda)
         (> (length e) 2))))

(define seq?
  (lambda (e)
    (keyword? e 'begin)))

(define applic?
  (lambda (e)
    (and (pair? e)
         (not (member (car e) *reserved-words*)))))

(define set!?
  (lambda (e)
    (and (keyword? e 'set!)
         (= (length e) 3))))

(define define?
  (lambda (e)
    (and (keyword? e 'define)
         (or (and (= (length e) 3)
                  (not (pair? (cadr e))))
             (and (> (length e) 2)
                  (pair? (cadr e)))))))

(define make-if3
  (lambda (e)
    (let ((test (cadr e))
          (dit (caddr e))
          (dif (if (= (length e) 4)
                   (cadddr e)
                   (void))))
      `(if3 ,(parse test) ,(parse dit) ,(parse dif)))))

(define make-or
  (lambda (e)
    (let ((exprs (cdr e)))
      (if (null? exprs)
          (parse '#f)
          `(or ,(map parse exprs))))))

(define make-lambda
  (lambda (e)
    (let ((params (cadr e))
          (body (cddr e)))
      (cond ((list? params)
             `(lambda-simple ,params ,(parse `(begin ,@body))))
            ((pair? params)
             `(lambda-opt ,@(split-improper-list params) ,(parse `(begin ,@body))))
            (else
             `(lambda-opt () ,params ,(parse `(begin ,@body))))))))

(define make-seq
  (lambda (e)
    (let ((exprs (cdr e))
          (ignore-nested-seqs (lambda (acc curr)
                               (if (seq? curr)
                                   `(,@acc ,@(map parse (cdr curr)))
                                   `(,@acc ,(parse curr))))))
      (if (null? exprs)
          (parse (void))
          (if (null? (cdr exprs))
              (parse (car exprs))
              `(seq ,(fold-left ignore-nested-seqs '() exprs)))))))

(define make-applic
  (lambda (e)
    (let ((f (car e))
          (args (cdr e)))
      `(applic ,(parse f)
               ,(if (null? args)
                    '()
                    (map parse args))))))

(define make-set
  (lambda (e)
    `(set (var ,(cadr e)) ,(parse (caddr e)))))

(define make-define
  (lambda (e)
    (if (pair? (cadr e))
        (make-mit-define e)
        (make-regular-define e))))

(define make-regular-define
  (lambda (e)
    `(define (var ,(cadr e)) ,(parse (caddr e)))))

(define make-mit-define
  (lambda (e)
    (let ((f (caadr e))
          (params (cdadr e))
          (body (cddr e)))
      `(define (var ,f) ,(parse `(lambda ,params ,@body))))))

(define keyword?
  (lambda (e k)
    (and (pair? e)
         (eq? (car e) k))))

(define split-improper-list
  (lambda (l)
    (letrec ((loop (lambda (l acc)
                     (if (not (pair? l))
                         (list acc l)
                         (loop (cdr l) `(,@acc ,(car l)))))))
      (loop l '()))))

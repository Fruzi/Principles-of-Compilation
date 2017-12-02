(load "qq.scm")

(define parse
  (lambda (e)
    (cond ((quote? e) `(const ,@(cdr e)))
          ((quasiquote? e) (expand-qq (cadr e)))
          ((constant? e) `(const ,e))
          ((if3? e) (make-if3 e))
          ((or? e) (make-or e))
          ((and? e) (make-and e))
          ((lambda? e) (make-lambda e))
          ((seq? e) (make-seq e))
          ((applic? e) (make-applic e))
          ((set!? e) (make-set e))
          ((define? e) (make-define e))
          ((cond? e) (make-cond e))
          ((let? e) (make-let e))
          ((let*? e) (make-let* e))
          ((letrec? e) (make-letrec e))
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

(define quasiquote?
  (lambda (e)
    ((^quote? 'quasiquote) e)))

(define variable?
  (lambda (e)
    (not (or (pair? e)
             (member e *reserved-words*)))))

(define if3?
  (lambda (e)
    (and (first-eq? e 'if)
         (or (= (length e) 3)
             (= (length e) 4)))))

(define or?
  (lambda (e)
    (first-eq? e 'or)))

(define and?
  (lambda (e)
    (first-eq? e 'and)))

(define lambda?
  (lambda (e)
    (and (first-eq? e 'lambda)
         (> (length e) 2))))

(define seq?
  (lambda (e)
    (first-eq? e 'begin)))

(define applic?
  (lambda (e)
    (and (pair? e)
         (not (member (car e) *reserved-words*)))))

(define set!?
  (lambda (e)
    (and (first-eq? e 'set!)
         (= (length e) 3))))

(define define?
  (lambda (e)
    (and (first-eq? e 'define)
         (or (and (= (length e) 3)
                  (not (pair? (cadr e))))
             (and (> (length e) 2)
                  (pair? (cadr e)))))))

(define cond?
  (lambda (e)
    (and (first-eq? e 'cond)
         (> (length e) 1))))

(define let?
  (lambda (e)
    (and (first-eq? e 'let)
         (let-base? e))))

(define let*?
  (lambda (e)
    (and (first-eq? e 'let*)
         (let-base? e))))

(define letrec?
  (lambda (e)
    (and (first-eq? e 'letrec)
         (let-base? e))))

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
          (if (null? (cdr exprs))
              (parse (car exprs))
              `(or ,(map parse exprs)))))))

(define make-and
  (lambda (e)
    (letrec ((exprs (cdr e))
             (make-ifs (lambda (l)
                         (if (null? (cdr l))
                             (car l)
                             `(if ,(car l) ,(make-ifs (cdr l)) '#f)))))
      (if (null? exprs)
          (parse '#t)
          (parse (make-ifs exprs))))))

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
    (letrec ((exprs (cdr e))
             (ignore (lambda (curr)
                       (if (not (pair? curr))
                           (if (not (eq? curr 'begin))
                               (list curr)
                               '())
                           (if (not (seq? curr))
                               (list (fold-left append '() (map ignore curr)))
                               (fold-left append '() (map ignore curr)))))))
      (if (null? exprs)
          (parse (void))
          (if (null? (cdr exprs))
              (parse (car exprs))
              `(seq ,@(map parse (car (ignore exprs)))))))))

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

(define make-cond
  (lambda (e)
    (letrec ((exprs (cdr e))
             (make-ifs (lambda (l)
                         (let ((test (caar l))
                               (exp `(begin ,@(cdar l))))
                           (cond ((eq? test 'else) exp)
                                 ((null? (cdr l)) `(if ,test ,exp))
                                 (else `(if ,test ,exp ,(make-ifs (cdr l)))))))))
      (parse (make-ifs exprs)))))

(define make-let
  (lambda (e)
    (let* ((bindings (cadr e))
           (vars (map car bindings))
           (values (map cadr bindings))
           (body (cddr e)))
      (parse `((lambda ,vars ,@body) ,@values)))))

(define make-let*
  (lambda (e)
    (let ((bindings (cadr e))
          (body (cddr e)))
      (letrec ((make-lets (lambda (bindings body)
                            (if (or (null? bindings)
                                    (null? (cdr bindings)))
                                `(let ,bindings ,@body)
                                (let ((vars (map car bindings))
                                      (values (map cadr bindings)))
                                  `(let ((,(car vars) ,(car values))) ,(make-lets (cdr bindings) body)))))))
            (parse (make-lets bindings body))))))

(define make-letrec
  (lambda (e)
    (let* ((bindings (cadr e))
           (vars (map car bindings))
           (values (map cadr bindings))
           (let-bindings (map (lambda (v) `(,v #f)) vars))
           (body (cddr e))
           (new-body `(begin ,@(map (lambda (var val)
                                     `(set! ,var ,val))
                                   vars values)
                             (let () ,@body))))
      (parse `(let ,let-bindings ,new-body)))))

(define first-eq?
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

(define let-base?
  (lambda (e)
    (letrec ((check-unique (lambda (rest)
                             (or (null? rest)
                                 (and (not (member (car rest) (cdr rest)))
                                      (check-unique (cdr rest)))))))
      (and (> (length e) 2)
           (let ((bindings (cadr e)))
             (or (null? bindings)
                 (if (first-eq? e 'let)
                     (let ((vars (map car bindings)))
                       (check-unique vars))
                     #t)))))))

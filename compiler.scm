(define pipeline
  (lambda (s)
    ((star <sexpr>) s
                    (lambda (m r)
                      (map (lambda (e)
                             (annotate-tc
                              (pe->lex-pe
                               (box-set
                                (remove-applic-lambda-nil
                                 (parse e))))))
                           m))
                    (lambda (f) 'fail))))
 
(define file->list
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
                (lambda ()
                  (let ((ch (read-char in-port)))
                    (if (eof-object? ch)
                        (begin
                          (close-input-port in-port)
                          '())
                        (cons ch (run)))))))
        (run)))))

(define list->set
  (lambda (l)
    (fold-left
     (lambda (acc curr)
       (if (member curr acc)
           acc
           (append acc (list curr))))
     '()
     l)))

(define get-consts
  (lambda (l pe)
    (let ((tag (car pe)))
      (cond ((eq? tag 'const)
             (append l (cdr pe)))
            ((eq? tag 'if3)
             (let ((test (cadr pe))
                   (dit (caddr pe))
                   (dif (cadddr pe)))
               (append (get-consts l test)
                       (get-consts l dit)
                       (get-consts l dif))))
            ((or (eq? tag 'or) (eq? tag 'seq))
             (apply append (map (lambda (el) (get-consts l el)) (cadr pe))))
            ((or (eq? tag 'define) (eq? tag 'set) (eq? tag 'box-set))
             (let ((var (cadr pe))
                   (val (caddr pe)))
               (get-consts l val)))
            ((eq? tag 'lambda-simple)
             (let ((params (cadr pe))
                   (body (caddr pe)))
               (get-consts l body)))
            ((eq? tag 'lambda-opt)
             (let ((params (cadr pe))
                   (opt-param (caddr pe))
                   (body (cadddr pe)))
               (get-consts l body)))
            ((or (eq? tag 'tc-applic) (eq? tag 'applic))
             (let ((f (cadr pe))
                   (args (caddr pe)))
               (append (get-consts l f)
                       (apply append (map (lambda (el) (get-consts l el)) args)))))
            ((or (eq? tag 'box) (eq? tag 'box-get))
             (get-consts l (cadr pe)))
            (else l)))))

(define get-fvars
  (lambda (l pe)
    (let ((tag (car pe)))
      (cond ((eq? tag 'fvar)
             (append l (cdr pe)))
            ((eq? tag 'if3)
             (let ((test (cadr pe))
                   (dit (caddr pe))
                   (dif (cadddr pe)))
               (append (get-fvars l test)
                       (get-fvars l dit)
                       (get-fvars l dif))))
            ((or (eq? tag 'or) (eq? tag 'seq))
             (apply append (map (lambda (el) (get-fvars l el)) (cadr pe))))
            ((or (eq? tag 'define) (eq? tag 'set) (eq? tag 'box-set))
             (let ((var (cadr pe))
                   (val (caddr pe)))
               (get-fvars l val)))
            ((eq? tag 'lambda-simple)
             (let ((params (cadr pe))
                   (body (caddr pe)))
               (get-fvars l body)))
            ((eq? tag 'lambda-opt)
             (let ((params (cadr pe))
                   (opt-param (caddr pe))
                   (body (cadddr pe)))
               (get-fvars l body)))
            ((or (eq? tag 'applic) (eq? tag 'tc-applic))
             (let ((f (cadr pe))
                   (args (caddr pe)))
               (append (get-fvars l f)
                       (apply append (map (lambda (el) (get-fvars l el)) args)))))
            ((or (eq? tag 'box) (eq? tag 'box-get))
             (get-fvars l (cadr pe)))
            (else l)))))

(define constants
  (lambda (pes)
    (list->set (apply append (map (lambda (pe) (get-consts '() pe)) pes)))))

(define fvars
  (lambda (pes)
    (list->set (apply append (map (lambda (pe) (get-fvars '() pe)) pes)))))

(define data-gen-const
  (lambda (entry)
    (let ((value (const-entry->value entry))
          (label (const-entry->label entry)))
      (cond ((null? value)
             (format "~a:\n\tdq SOB_NIL\n" label))
            ((eq? value (void))
             (format "~a:\n\tdq SOB_VOID\n" label))
            ((eq? value #t)
             (format "~a:\n\tdq SOB_TRUE\n" label))
            ((eq? value #f)
             (format "~a:\n\tdq SOB_FALSE\n" label))
            ((integer? value)
             (format "~a:\n\tdq MAKE_LITERAL(T_INTEGER, ~a)\n" label value))
            ((and (number? value) (not (integer? value)))
             "TODO")
            ((char? value)
             (format "~a:\n\tdq MAKE_LITERAL(T_CHAR, ~a)\n" label (char->integer value)))
            ((string? value)
             (format "~a:\n\tMAKE_LITERAL_STRING ~s\n" label value))
            ((symbol? value)
             (format "~a:\n\tdq MAKE_LITERAL(T_SYMBOL, ~s)\n" label (symbol->string value)))
            ((pair? value)
             (let ((car-label (get-const-label (car value)))
                   (cdr-label (get-const-label (cdr value))))
               (format "~a:\n\tdq MAKE_LITERAL_PAIR(~a, ~a)\n" label car-label cdr-label)))
            (else (format "Data-gen unsopported for ~a" value))))))

(define get-const-label
  (lambda (value)
    (let ((entry
           (find (lambda (entry) (equal? value (const-entry->value entry))) consts-table)))
      (if entry
          (const-entry->label entry)
          ""))))

(define const-entry->value car)

(define const-entry->label cdr)

(define make-const-entry cons)

(define add-const-if-not-exists!
  (lambda (value label)
    (if (equal? (get-const-label value) "")
        (append! consts-table (list (make-const-entry value label))))))

(define consts-table
  (list (make-const-entry '() "sobNil")
        (make-const-entry (void) "sobVoid")
        (make-const-entry #t "sobTrue")
        (make-const-entry #f "sobFalse")))

(define add-const-entry
  (lambda (value label)
    (if (not (pair? value))
        (add-const-if-not-exists! value label)
        (let ((idx 0))
          (letrec ((inc (lambda () (set! idx (+ idx 1))))
                   (loop (lambda (current)
                           (let ((new-label (string-append label (number->string idx))))
                             (if (not (pair? current))
                                 (begin
                                   (add-const-if-not-exists! current new-label)
                                   (inc))
                                 (begin
                                   (add-const-if-not-exists! (car current) new-label)
                                   (inc)
                                   (loop (cdr current))
                                   (set! new-label (string-append label (number->string idx)))
                                   (add-const-if-not-exists! current new-label)
                                   (inc)))))))
            (loop value))))))

(define build-consts-table
  (lambda (consts)
    (for-each (lambda (const label-num)
                (add-const-entry const
                                 (format "sob~a" label-num)))
              consts
              (enumerate consts))))

(define asm-prologue
  (lambda (code)
    (append
      (list
        "%include 'scheme.s'"
        "section .data"
        "start_of_data:"
        (apply string-append (map data-gen-const consts-table))
        "extern exit, printf, scanf"
        "global main"
        "section .text"
        "main:")
      code)))

(define asm-epilogue
  (lambda (code)
    (append
      code
      (list
        "xor rdi, rdi"
        "call exit"))))

(define if3-label-index 0)

(define or-label-index 0)

(define code-gen-const
  (lambda (pe)
    (format "mov rax, [~a]\n" (get-const-label (cadr pe)))))

(define code-gen-fvar
  (lambda (pe)
    "TODO"))

(define code-gen-pvar
  (lambda (pe)
    "TODO"))

(define code-gen-bvar
  (lambda (pe)
    "TODO"))

(define code-gen-if3
  (lambda (pe)
    (set! if3-label-index (+ if3-label-index 1))
    (let ((test (cadr pe))
          (dit (caddr pe))
          (dif (cadddr pe)))
      (string-append
      (code-gen test)
      "cmp rax, [sobFalse]\n"
      (format "je .if3_false_~a\n" if3-label-index)
      (code-gen dit)
      (format "jmp .if3_end_~a\n" if3-label-index)
      (format ".if3_false_~a:\n" if3-label-index)
      (code-gen dif)
      (format ".if3_end_~a:\nnop\n" if3-label-index)))))

(define code-gen-or
  (lambda (pe)
    (set! or-label-index (+ or-label-index 1))
    (let ((or-pes (cadr pe)))
      (string-append
        (fold-left (lambda (acc curr)
                      (string-append acc
                        "cmp rax, [sobFalse]\n"
                        (format "jne .or_end_~a\n" or-label-index)
                        (code-gen curr)))
                    (code-gen (car or-pes))
                    (cdr or-pes))
        (format ".or_end_~a:\nnop\n" or-label-index)))))

(define code-gen-define
  (lambda (pe)
    "TODO"))

(define code-gen-set
  (lambda (pe)
    "TODO"))

(define code-gen-seq
  (lambda (pe)
    (apply string-append (map code-gen (cadr pe)))))

(define code-gen-lambda-simple
  (lambda (pe)
    "TODO"))

(define code-gen-lambda-opt
  (lambda (pe)
    "TODO"))

(define code-gen-applic
  (lambda (pe)
    "TODO"))

(define code-gen-tc-applic
  (lambda (pe)
    "TODO"))

(define code-gen-box
  (lambda (pe)
    "TODO"))

(define code-gen-box-get
  (lambda (pe)
    "TODO"))

(define code-gen-box-set
  (lambda (pe)
    "TODO"))

(define code-gen
  (lambda (pe)
    (let* ((tag (car pe))
           (code-gen-fun (cond
            ((eq? tag 'const) code-gen-const)
            ((eq? tag 'fvar) code-gen-fvar)
            ((eq? tag 'pvar) code-gen-pvar)
            ((eq? tag 'bvar) code-gen-bvar)
            ((eq? tag 'if3) code-gen-if3)
            ((eq? tag 'or) code-gen-or)
            ((eq? tag 'define) code-gen-define)
            ((eq? tag 'set) code-gen-set)
            ((eq? tag 'seq) code-gen-seq)
            ((eq? tag 'lambda-simple) code-gen-lambda-simple)
            ((eq? tag 'lambda-opt) code-gen-lambda-opt)
            ((eq? tag 'applic) code-gen-applic)
            ((eq? tag 'tc-applic) code-gen-tc-applic)
            ((eq? tag 'box) code-gen-box)
            ((eq? tag 'box-get) code-gen-box-get)
            ((eq? tag 'box-set) code-gen-box-set)
            (else (lambda (_) "")))))
      (code-gen-fun pe))))

(define compile
  (lambda (pes)
    (let ((constants (constants pes))
          (fvars (fvars pes)))
      (build-consts-table constants)
      (let ((code (map (lambda (pe)
                         (string-append
                           (code-gen pe)
                           "push rax\n"
                           "call write_sob_if_not_void\n"
                           "add rsp, 1*8\n"))
                       pes)))
        (asm-epilogue (asm-prologue code))))))

(define compile-scheme-file
  (lambda (in-file out-file)
    (let ((parsed (pipeline (file->list in-file)))
          (out-port (open-output-file out-file)))
      (for-each (lambda (line) (fprintf out-port "~a\n" line))
                (compile parsed))
      (close-output-port out-port))))

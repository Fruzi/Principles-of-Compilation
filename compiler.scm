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
               (append
                (get-consts l test)
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
               (append
                (get-consts l f)
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
               (append
                (get-fvars l test)
                (get-fvars l dit)
                (get-fvars l dif))))
            ((or (eq? tag 'or) (eq? tag 'seq))
             (apply append (map (lambda (el) (get-fvars l el)) (cadr pe))))
            ((or (eq? tag 'define) (eq? tag 'set) (eq? tag 'box-set))
             (let ((var (cadr pe))
                   (val (caddr pe)))
               (append
                (get-fvars l var)
                (get-fvars l val))))
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
               (append
                (get-fvars l f)
                (apply append (map (lambda (el) (get-fvars l el)) args)))))
            ((or (eq? tag 'box) (eq? tag 'box-get))
             (get-fvars l (cadr pe)))
            (else l)))))

(define constants-set
  (lambda (pes)
    (list->set (apply append (map (lambda (pe) (get-consts '() pe)) pes)))))

(define fvars-set
  (lambda (pes)
    (list->set (apply append (map (lambda (pe) (get-fvars '() pe)) pes)))))

(define data-gen-const-1
  (lambda (entry)
    (let ((value (table-entry->value entry))
          (label (table-entry->label entry)))
      (format "~a: resq 1\n" label))))

(define data-gen-const-2
  (lambda (entry)
    (let ((value (table-entry->value entry))
          (label (table-entry->label entry)))
      (cond ((null? value)
             (format "mov qword [~a], SOB_NIL\n" label))
            ((eq? value (void))
             (format "mov qword [~a], SOB_VOID\n" label))
            ((eq? value #t)
             (format "mov qword [~a], SOB_TRUE\n" label))
            ((eq? value #f)
             (format "mov qword [~a], SOB_FALSE\n" label))
            ((integer? value)
             (format "MAKE_LITERAL_2 ~a, T_INTEGER, ~a\n" label value))
            ((rational? value)
             (format "MAKE_LITERAL_FRACTION ~a, ~a, ~a\n" label (numerator value) (denominator value)))
            ((char? value)
             (format "MAKE_LITERAL_2 ~a, T_CHAR, ~a\n" label (char->integer value)))
            ((string? value)
             (string-append
              (format "MY_MALLOC ~a\n" (string-length value))
              "xor rcx, rcx\n"
              (apply string-append
                     (map (lambda (ch)
                            (string-append
                             (format "mov byte [rax + rcx], ~a\n" (char->integer ch))
                             "inc rcx\n"))
                          (string->list value)))
              (format "MAKE_LITERAL_STRING ~a, rax, ~a\n" label (string-length value))))
            ((symbol? value)
             (string-append
              (data-gen-const-2 (make-table-entry (symbol->string value) label))
              (format "xor qword [~a], T_SYMBOL ^ T_STRING\n" label)))
            ((pair? value)
             (let ((car-label (table-find-label consts-table (car value)))
                   (cdr-label (table-find-label consts-table (cdr value))))
               (format "MAKE_LITERAL_PAIR ~a, ~a, ~a\n" label car-label cdr-label)))
            ((vector? value)
             (string-append
              (format "MY_MALLOC ~a * 8\n" (vector-length value))
              "xor rcx, rcx\n"
              (apply string-append
                     (map (lambda (el)
                            (string-append
                             (format "mov qword [rax + 8 * rcx], ~a\n" (table-find-label consts-table el))
                             "inc rcx\n"))
                          (vector->list value)))
              (format "MAKE_LITERAL_VECTOR ~a, rax, ~a\n" label (vector-length value))))
            (else "")))))

(define data-gen-global-1
  (lambda (entry)
    (let ((var (table-entry->value entry))
          (label (table-entry->label entry)))
      (format "~a: resq 2\t; ~s\n" label var))))

(define data-gen-global-2
  (lambda (entry)
    (let ((var (table-entry->value entry))
          (label (table-entry->label entry)))
      (format "mov qword [~a], SOB_UNDEFINED\n" label))))

(define table-entry->value car)

(define table-entry->label cdr)

(define make-table-entry cons)

(define table-find-label
  (lambda (table value)
    (let ((entry (find (lambda (entry)
                         (equal? value (table-entry->value entry)))
                       table)))
      (if entry
          (table-entry->label entry)
          ""))))

(define add-entry-if-not-exists!
  (lambda (table value label)
    (if (equal? (table-find-label table value) "")
        (append! table (list (make-table-entry value label)))
        table)))

(define add-const-if-not-exists!
  (lambda (value)
    (add-entry-if-not-exists! consts-table value (new-const-label))))

(define add-global-if-not-exists!
  (lambda (var)
    (add-entry-if-not-exists! globals-table var (new-global-label))))

(define consts-table
  (list
   (make-table-entry '() "SobNil")
   (make-table-entry (void) "SobVoid")
   (make-table-entry #t "SobTrue")
   (make-table-entry #f "SobFalse")
   (make-table-entry 0 "SobZero")))

(define const-label-num 0)

(define new-const-label
  (lambda ()
    (set! const-label-num (+ const-label-num 1))
    (format "Lconst~a" const-label-num)))

(define globals-table
  (list
   (make-table-entry 'car "ProcCar")
   (make-table-entry 'cdr "ProcCdr")
   (make-table-entry 'cons "ProcCons")
   (make-table-entry 'eq? "ProcEq")
   (make-table-entry '= "ProcEquals")
   (make-table-entry '< "ProcLessThan")
   (make-table-entry '> "ProcGreaterThan")
   (make-table-entry '+ "ProcAdd")
   (make-table-entry '* "ProcMul")
   (make-table-entry '/ "ProcDiv")
   (make-table-entry 'boolean? "ProcBoolean")
   (make-table-entry 'char? "ProcChar")
   (make-table-entry 'procedure? "ProcProcedure")
   (make-table-entry 'integer? "ProcInteger")
   (make-table-entry 'rational? "ProcRational")
   (make-table-entry 'pair? "ProcPair")
   (make-table-entry 'string? "ProcString")
   (make-table-entry 'symbol? "ProcSymbol")
   (make-table-entry 'vector? "ProcVectorQ")
   (make-table-entry 'integer->char "ProcIntegerToChar")
   (make-table-entry 'char->integer "ProcCharToInteger")
   (make-table-entry 'remainder "ProcRemainder")
   (make-table-entry 'numerator "ProcNumerator")
   (make-table-entry 'denominator "ProcDenominator")
   (make-table-entry 'not "ProcNot")
   (make-table-entry 'string-length "ProcStringLength")
   (make-table-entry 'string-ref "ProcStringRef")
   (make-table-entry 'string-set! "ProcStringSet")
   (make-table-entry 'make-string "ProcMakeString")
   (make-table-entry 'vector "ProcVector")
   (make-table-entry 'vector-length "ProcVectorLength")
   (make-table-entry 'vector-ref "ProcVectorRef")
   (make-table-entry 'vector-set! "ProcVectorSet")
   (make-table-entry 'make-vector "ProcMakeVector")
   (make-table-entry 'apply "ProcApply")
   (make-table-entry 'symbol->string "ProcSymbolToString")
   (make-table-entry 'string->symbol "ProcStringToSymbol")))

(define globals-label-num 0)

(define new-global-label
  (lambda ()
    (set! globals-label-num (+ globals-label-num 1))
    (format "Lglobal~a" globals-label-num)))

(define add-const-entry
  (lambda (value)
    (cond ((pair? value)
           (add-const-entry (car value))
           (add-const-entry (cdr value)))
          ((vector? value)
           (for-each add-const-entry (vector->list value))))
    (add-const-if-not-exists! value)))

(define build-consts-table
  (lambda (consts)
    (for-each add-const-entry consts)))

(define build-globals-table
  (lambda (globals)
    (for-each add-global-if-not-exists! globals)))

(define if3-label-index 0)

(define or-label-index 0)

(define lambda-label-index 0)

(define applic-label-index 0)

(define code-gen-const
  (lambda (pe)
    (format "lea rax, [~a]\n" (table-find-label consts-table (cadr pe)))))

(define code-gen-fvar
  (lambda (pe)
    (format "lea rax, [~a]\n" (table-find-label globals-table (cadr pe)))))

(define code-gen-pvar
  (lambda (pe)
    (let ((minor (caddr pe)))
      (format "mov rax, An(~a)\n" minor))))

(define code-gen-bvar
  (lambda (pe)
    (let ((major (caddr pe))
          (minor (cadddr pe)))
      (string-append
       "mov rax, env\n"
       (format "mov rax, [rax + 8 * ~a]\n" major)
       (format "mov rax, [rax + 8 * ~a]\n" minor)))))

(define code-gen-if3
  (lambda (pe)
    (set! if3-label-index (+ if3-label-index 1))
    (let ((test (cadr pe))
          (dit (caddr pe))
          (dif (cadddr pe))
          (if3-label-index if3-label-index))
      (string-append
       (code-gen test)
       "cmp rax, qword SobFalse\n"
       (format "je if3_false_~a\n" if3-label-index)
       (code-gen dit)
       (format "jmp if3_end_~a\n" if3-label-index)
       (format "if3_false_~a:\n" if3-label-index)
       (code-gen dif)
       (format "if3_end_~a:\nnop\n" if3-label-index)))))

(define code-gen-or
  (lambda (pe)
    (set! or-label-index (+ or-label-index 1))
    (let ((or-pes (cadr pe))
          (or-label-index or-label-index))
      (string-append
       (fold-left (lambda (acc curr)
                    (string-append
                     acc
                     "cmp rax, qword SobFalse\n"
                     (format "jne or_end_~a\n" or-label-index)
                     (code-gen curr)))
                  (code-gen (car or-pes))
                  (cdr or-pes))
       (format "or_end_~a:\nnop\n" or-label-index)))))

(define code-gen-define
  (lambda (pe)
    (let ((var (cadr (cadr pe)))
          (val (caddr pe)))
      (string-append
       (code-gen val)
       "mov rax, [rax]\n"
       (format "mov [~a], rax\n" (table-find-label globals-table var))
       "lea rax, [SobVoid]\n"))))

(define code-gen-set
  (lambda (pe)
    (let* ((var (cadr pe))
           (var-tag (car var))
           (var-sym (cadr var))
           (val (caddr pe)))
      (string-append
       (code-gen val)
       (cond ((eq? var-tag 'fvar)
              (string-append
               "mov rax, [rax]\n"
               (format "mov [~a], rax\n" (table-find-label globals-table var-sym))))
             ((eq? var-tag 'pvar)
              (let ((minor (caddr var)))
                (format "mov An(~a), rax\n" minor)))
             ((eq? var-tag 'bvar)
              (let ((major (caddr var))
                    (minor (cadddr var)))
                (string-append
                 "mov rbx, env\n"
                 (format "mov rbx, [rbx + 8 * ~a]\n" major)
                 (format "mov [rbx + 8 * ~a], rax\n" minor)))))
       "lea rax, [SobVoid]\n"))))

(define code-gen-seq
  (lambda (pe)
    (if (null? (cadr pe))
        "lea rax, [SobVoid]\n"
        (apply string-append (map code-gen (cadr pe))))))

(define lambda-depth 0)

(define code-gen-lambda-prologue
  (lambda (lambda-label-index lambda-depth)
    (string-append
     "MY_MALLOC 8 * 2\n"
     "push rax\n"
     (format "MY_MALLOC 8 * ~a\n" lambda-depth)
     "push rax\n"
     "mov rcx, arg_count\n"
     "shl rcx, 3\n"
     "MY_MALLOC rcx\n"
     "mov rdx, rax\n"
     "pop rbx\n"
     "pop rax\n"
     "mov rcx, arg_count\n"
     "cmp rcx, 0\n"
     (format "je empty_env_~a\n" lambda-label-index)
     (format "new_env_loop_~a:\n" lambda-label-index)
     "mov rdi, An(rcx - 1)\n"
     "mov [rdx + 8 * (rcx - 1)], rdi\n"
     (format "loop new_env_loop_~a\n" lambda-label-index)
     (format "jmp new_env_loop_end_~a\n" lambda-label-index)
     (format "empty_env_~a:\n" lambda-label-index)
     "xor rdx, rdx\n"
     (format "new_env_loop_end_~a:\n" lambda-label-index)
     "mov [rbx], rdx\n"
     (format "mov rcx, (~a - 1)\n" lambda-depth)
     "cmp rcx, 0\n"
     (format "je extend_env_loop_end_~a\n" lambda-label-index)
     "mov rdx, env\n"
     (format "extend_env_loop_~a:\n" lambda-label-index)
     "mov rdi, [rdx + 8 * (rcx - 1)]\n"
     "mov [rbx + 8 * rcx], rdi\n"
     (format "loop extend_env_loop_~a\n" lambda-label-index)
     (format "extend_env_loop_end_~a:\n" lambda-label-index)
     (format "MAKE_LITERAL_CLOSURE rax, rbx, lambda_code_~a\n" lambda-label-index)
     (format "jmp lambda_end_~a\n" lambda-label-index)
     (format "lambda_code_~a:\n" lambda-label-index)
     "push rbp\n"
     "mov rbp, rsp\n")))

(define code-gen-lambda-epilogue
  (lambda (lambda-label-index lambda-depth)
    (string-append
     "leave\n"
     "ret\n"
     (format "lambda_end_~a:\n" lambda-label-index))))

(define code-gen-lambda-simple
  (lambda (pe)
    (set! lambda-label-index (+ lambda-label-index 1))
    (set! lambda-depth (+ lambda-depth 1))
    (let ((params (cadr pe))
          (body (caddr pe))
          (lambda-label-index lambda-label-index))
      (let ((gen (string-append
                  (code-gen-lambda-prologue lambda-label-index lambda-depth)
                  (format "cmp arg_count, ~a\n" (length params))
                  (format "jne lambda_end_~a\n" lambda-label-index)
                  (code-gen body)
                  (code-gen-lambda-epilogue lambda-label-index lambda-depth))))
        (set! lambda-depth (- lambda-depth 1))
        gen))))
       
(define code-gen-lambda-opt
  (lambda (pe)
    (set! lambda-label-index (+ lambda-label-index 1))
    (set! lambda-depth (+ lambda-depth 1))
    (let ((params (cadr pe))
          (opt-param (caddr pe))
          (body (cadddr pe))
          (lambda-label-index lambda-label-index))
      (let ((gen (string-append
                  (code-gen-lambda-prologue lambda-label-index lambda-depth)
                  (format "cmp arg_count, ~a\n" (length params))
                  (format "jl lambda_end_~a\n" lambda-label-index)
                  (format "cmp arg_count, ~a\n" (length params))
                  (format "jne has_opt_~a\n" lambda-label-index)
                  "sub rbp, 8\n"
                  "mov rdx, rbp\n"
                  (format "mov rcx, (~a + 4)\n" (length params))
                  (format "shift_up_loop_~a:\n" lambda-label-index)
                  "mov rax, [rdx + 8]\n"
                  "mov [rdx], rax\n"
                  "add rdx, 8\n"
                  (format "loop shift_up_loop_~a\n" lambda-label-index)
                  "mov qword [rdx], SobNil\n"
                  (format "jmp opt_end_~a\n" lambda-label-index)
                  (format "has_opt_~a:\n" lambda-label-index)
                  "mov rcx, arg_count\n"
                  (format "sub rcx, ~a\n" (length params))
                  "mov rsi, arg_count\n"
                  "add rsi, 3\n"
                  "shl rsi, 3\n"
                  "push qword SobNil\n"
                  (format "opt_list_loop_~a:\n" lambda-label-index)
                  "push rcx\n"
                  "push rsi\n"
                  "MY_MALLOC 8\n"
                  "pop rsi\n"
                  "pop rcx\n"
                  "pop rdx\n"
                  "push rsi\n"
                  "mov rsi, [rbp + rsi]\n"
                  "MAKE_DYNAMIC_PAIR rax, rsi, rdx\n"
                  "pop rsi\n"
                  "sub rsi, 8\n"
                  "push rax\n"
                  (format "loop opt_list_loop_~a\n" lambda-label-index)
                  "pop rax\n"
                  "mov rcx, arg_count\n"
                  "mov An(rcx - 1), rax\n"
                  "mov rdi, rcx\n"
                  "add rdi, 2\n"
                  "shl rdi, 3\n"
                  (format "mov rsi, (~a + 3)\n" (length params))
                  "shl rsi, 3\n"
                  (format "mov rcx, (~a + 4)\n" (length params))
                  "mov rbx, arg_count\n"
                  (format "shift_down_loop_~a:\n" lambda-label-index)
                  "mov rax, [rbp + rsi]\n"
                  "mov [rbp + rdi], rax\n"
                  "sub rsi, 8\n"
                  "sub rdi, 8\n"
                  (format "loop shift_down_loop_~a\n" lambda-label-index)  
                  (format "sub rbx, (~a + 1)\n" (length params))
                  "shl rbx, 3\n"
                  "add rbp, rbx\n"
                  (format "opt_end_~a:\n" lambda-label-index)
                  (format "mov arg_count, (~a + 1)\n" (length params))
                  "mov rsp, rbp\n"
                  (code-gen body)
                  (code-gen-lambda-epilogue lambda-label-index lambda-depth))))
        (set! lambda-depth (- lambda-depth 1))
        gen))))

(define code-gen-applic-prologue
  (lambda (proc args applic-label-index)
    (string-append
     (apply string-append
            (map (lambda (arg)
                   (string-append
                    (code-gen arg)
                    "push rax\n"))
                 (reverse args)))
     (format "push ~a\n" (length args))
     (code-gen proc)
     "mov rax, [rax]\n"
     "mov rbx, rax\n"
     "TYPE rbx\n"
     "cmp rbx, T_CLOSURE\n"
     (format "je applic_cont_~a\n" applic-label-index)
     "xor rax, rax\n"
     "mov rdi, 1\n"
     "call exit\n"
     (format "applic_cont_~a:\n" applic-label-index)
     "mov rbx, rax\n"
     "CLOSURE_ENV rbx\n"
     "push rbx\n")))

(define code-gen-applic
  (lambda (pe)
    (set! applic-label-index (+ applic-label-index 1))
    (let ((proc (cadr pe))
          (args (caddr pe))
          (applic-label-index applic-label-index))
      (string-append
       (code-gen-applic-prologue proc args applic-label-index)
       "CLOSURE_CODE rax\n"
       "call rax\n"
       "mov rbx, post_applic_arg_count\n"
       "add rbx, 2\n"
       "sal rbx, 3\n"
       "add rsp, rbx\n"))))

(define code-gen-tc-applic
  (lambda (pe)
    (set! applic-label-index (+ applic-label-index 1))
    (let ((proc (cadr pe))
          (args (caddr pe))
          (applic-label-index applic-label-index))
      (string-append
       (code-gen-applic-prologue proc args applic-label-index)
       "mov rdx, arg_count\n"
       "push ret_addr\n"
       "push rax\n"
       "mov rbx, rbp\n"
       "mov rbp, old_rbp\n"
       (format "mov rcx, ~a + 3\n" (length args))
       "mov rsi, rcx\n"
       "sal rsi, 3\n"
       "mov rdi, rdx\n"
       "add rdi, 3\n"
       "sal rdi, 3\n"
       (format "loop_tc_frame_~a:\n" applic-label-index)
       "mov rax, [rsp + rsi]\n"
       "mov [rbx + rdi], rax\n"
       "sub rsi, 8\n"
       "sub rdi, 8\n"
       (format "loop loop_tc_frame_~a\n" applic-label-index)
       "pop rax\n"
       "CLOSURE_CODE rax\n"
       "lea rsp, [rbx + rdi + 8]\n"
       "jmp rax\n"))))

(define code-gen-box
  (lambda (pe)
    (string-append
     (code-gen (cadr pe))
     "MAKE_POINTER rax\n")))

(define code-gen-box-get
  (lambda (pe)
    (string-append
     (code-gen (cadr pe))
     "mov rax, [rax]\n")))

(define code-gen-box-set
  (lambda (pe)
    (let ((var (cadr pe))
          (val (caddr pe)))
      (string-append
       (code-gen val)
       "mov rbx, rax\n"
       (code-gen var)
       "mov [rax], rbx\n"
       "lea rax, [SobVoid]\n"))))

(define code-gen
  (lambda (pe)
    (let* ((tag (car pe))
           (code-gen-fun
            (cond
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

(define asm-prologue
  (lambda (code)
    (append
     (list
      "%include './project/scheme.s'"
      ""
      "section .bss"
      "malloc_pointer:"
      "resq 1"
      "start_of_data:"
      (apply string-append (map data-gen-const-1 consts-table))
      (apply string-append (map data-gen-global-1 globals-table))
      "start_of_free_data:"
      "resb gigabyte(1)"
      ""
      "section .text"
      "main:"
      "push 0"
      "push 0"
      "push 0"
      "push rbp"
      "mov rbp, rsp"
      ""
      "mov rax, malloc_pointer"
      "mov qword [rax], start_of_free_data"
      ""
      (apply string-append (map data-gen-const-2 consts-table))
      (apply string-append (map data-gen-global-2 globals-table))
      ""
      "MAKE_INITIAL_CLOSURE ProcCar, prim_car"
      "MAKE_INITIAL_CLOSURE ProcCdr, prim_cdr"
      "MAKE_INITIAL_CLOSURE ProcCons, prim_cons"
      "MAKE_INITIAL_CLOSURE ProcEq, prim_eq"
      "MAKE_INITIAL_CLOSURE ProcEquals, prim_equals"
      "MAKE_INITIAL_CLOSURE ProcAdd, prim_add"
      "MAKE_INITIAL_CLOSURE ProcMul, prim_mul"
      "MAKE_INITIAL_CLOSURE ProcDiv, prim_div"
      "MAKE_INITIAL_CLOSURE ProcBoolean, prim_boolean"
      "MAKE_INITIAL_CLOSURE ProcChar, prim_char"
      "MAKE_INITIAL_CLOSURE ProcProcedure, prim_procedure"
      "MAKE_INITIAL_CLOSURE ProcInteger, prim_integer"
      "MAKE_INITIAL_CLOSURE ProcRational, prim_rational"
      "MAKE_INITIAL_CLOSURE ProcPair, prim_pair"
      "MAKE_INITIAL_CLOSURE ProcString, prim_string"
      "MAKE_INITIAL_CLOSURE ProcSymbol, prim_symbol"
      "MAKE_INITIAL_CLOSURE ProcVectorQ, prim_vector_q"
      "MAKE_INITIAL_CLOSURE ProcIntegerToChar, prim_integer_to_char"
      "MAKE_INITIAL_CLOSURE ProcCharToInteger, prim_char_to_integer"
      "MAKE_INITIAL_CLOSURE ProcRemainder, prim_remainder"
      "MAKE_INITIAL_CLOSURE ProcNumerator, prim_numerator"
      "MAKE_INITIAL_CLOSURE ProcDenominator, prim_denominator"
      "MAKE_INITIAL_CLOSURE ProcNot, prim_not"
      "MAKE_INITIAL_CLOSURE ProcStringLength, prim_string_length"
      "MAKE_INITIAL_CLOSURE ProcStringRef, prim_string_ref"
      "MAKE_INITIAL_CLOSURE ProcStringSet, prim_string_set"
      "MAKE_INITIAL_CLOSURE ProcMakeString, prim_make_string"
      "MAKE_INITIAL_CLOSURE ProcVector, prim_vector"
      "MAKE_INITIAL_CLOSURE ProcVectorLength, prim_vector_length"
      "MAKE_INITIAL_CLOSURE ProcVectorRef, prim_vector_ref"
      "MAKE_INITIAL_CLOSURE ProcVectorSet, prim_vector_set"
      "MAKE_INITIAL_CLOSURE ProcMakeVector, prim_make_vector"
      "MAKE_INITIAL_CLOSURE ProcApply, prim_apply"
      "MAKE_INITIAL_CLOSURE ProcSymbolToString, prim_symbol_to_string"
      "MAKE_INITIAL_CLOSURE ProcStringToSymbol, prim_string_to_symbol"
      "MAKE_INITIAL_CLOSURE ProcLessThan, prim_less_than"
      "MAKE_INITIAL_CLOSURE ProcGreaterThan, prim_greater_than"
      "")
     code)))

(define asm-epilogue
  (lambda (code)
    (append
     code
     (list
      "xor rdi, rdi"
      "call exit"))))

(define compile
  (lambda (pes)
    (let ((constants (constants-set pes))
          (fvars (fvars-set pes)))
      (build-consts-table constants)
      (build-globals-table fvars))
    (let ((code
           (map (lambda (pe)
                  (string-append
                   (code-gen pe)
                   "push rax\n"
                   "call write_sob_if_not_void\n"
                   "add rsp, 1*8\n"))
                pes)))
      (asm-epilogue (asm-prologue code)))))

(define compile-scheme-file
  (lambda (in-file out-file)
    (let* ((input (append (file->list "./project/builtins.scm") (file->list in-file)))
           (code (compile (pipeline input)))
           (out-port (open-output-file out-file 'replace)))
      (for-each (lambda (line) (fprintf out-port "~a\n" line))
                code)
      (close-output-port out-port))))

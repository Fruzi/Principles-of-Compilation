;;; scheme.s
;;; Support for the Scheme compiler
;;; 
;;; Programmer: Mayer Goldberg, 2018

%define T_UNDEFINED 0
%define T_VOID 1
%define T_NIL 2
%define T_INTEGER 3
%define T_FRACTION 4
%define T_BOOL 5
%define T_CHAR 6
%define T_STRING 7
%define T_SYMBOL 8
%define T_CLOSURE 9
%define T_PAIR 10
%define T_VECTOR 11

%define CHAR_NUL 0
%define CHAR_TAB 9
%define CHAR_NEWLINE 10
%define CHAR_PAGE 12
%define CHAR_RETURN 13
%define CHAR_SPACE 32

%define TYPE_BITS 4
%define WORD_SIZE 64

%define MAKE_LITERAL(type, lit) ((lit << TYPE_BITS) | type)

;;; MAKE_LITERAL_FROM_REG type, [register]
%macro MAKE_LITERAL_FROM_REG 2
  sal %2, TYPE_BITS
  or rax, %1
%endmacro

%macro TYPE 1
	and %1, ((1 << TYPE_BITS) - 1) 
%endmacro

%macro DATA 1
	sar %1, TYPE_BITS
%endmacro

%macro DATA_UPPER 1
	sar %1, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
%endmacro

%macro DATA_LOWER 1
	sal %1, ((WORD_SIZE - TYPE_BITS) >> 1)
	DATA_UPPER %1
%endmacro

%define LITERAL_PAIR_BASE(car, cdr) (((car << ((WORD_SIZE - TYPE_BITS) >> 1)) | cdr) << TYPE_BITS)

%define MAKE_LITERAL_PAIR(car, cdr) ((LITERAL_PAIR_BASE((car - start_of_data), (cdr - start_of_data))) | T_PAIR)

%define MAKE_LITERAL_FRACTION(num, den) ((LITERAL_PAIR_BASE(num, den)) | T_FRACTION)

%macro MAKE_LITERAL_FRACTION_2 2
  mov rax, %1
  shl rax, (WORD_SIZE - TYPE_BITS) >> 1
  or rax, %2
  shl rax, TYPE_BITS
  or rax, T_FRACTION
%endmacro

%define MAKE_LITERAL_SYMBOL(str) (MAKE_LITERAL(T_SYMBOL, (str - start_of_data)))

%macro MAKE_LITERAL_SYMBOL_2 1
  mov rax, %1
  sub rax, start_of_data
  MAKE_LITERAL_2 T_SYMBOL, rax
%endmacro
  
%macro MAKE_LITERAL_2 2
  mov rax, %2
  sal rax, TYPE_BITS
  or rax, %1
%endmacro

%macro CAR 1
	DATA_UPPER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

%macro CDR 1
	DATA_LOWER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

;;; MAKE_LITERAL_CLOSURE target, env, code
%macro MAKE_LITERAL_CLOSURE 3
	push rax
	push rbx
	mov rax, %1
  mov rbx, %2
  sub rbx, start_of_data
	mov qword [rax], rbx
	shl qword [rax], ((WORD_SIZE - TYPE_BITS) >> 1)
	lea rbx, [rax + 8]
  sub rbx, start_of_data
	or qword [rax], rbx
	shl qword [rax], TYPE_BITS
	or qword [rax], T_CLOSURE
	mov qword [rax + 8], %3
	pop rbx
	pop rax
%endmacro

%macro CLOSURE_ENV 1
	DATA_UPPER %1
	add %1, start_of_data
%endmacro

%macro CLOSURE_CODE 1
	DATA_LOWER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

%macro MAKE_LITERAL_STRING 1+
	dq (((((%%LstrEnd - %%Lstr) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Lstr - start_of_data)) << TYPE_BITS) | T_STRING)
	%%Lstr:
	db %1
	%%LstrEnd:
%endmacro

%macro STRING_LENGTH 1
	DATA_UPPER %1
%endmacro

%macro STRING_ELEMENTS 1
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

;;; STRING_REF dest, src, index
;;; dest cannot be RAX! (fix this!)
%macro STRING_REF 3
	push rax
	mov rax, %2
	STRING_ELEMENTS rax
	add rax, %3
	mov %1, byte [rax]
	pop rax
%endmacro

%macro MAKE_LITERAL_VECTOR 1+
	dq ((((((%%VecEnd - %%Vec) >> 3) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Vec - start_of_data)) << TYPE_BITS) | T_VECTOR)
	%%Vec:
	dq %1
	%%VecEnd:
%endmacro

%macro VECTOR_LENGTH 1
	DATA_UPPER %1
%endmacro

%macro VECTOR_ELEMENTS 1
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

;;; VECTOR_REF dest, src, index
;;; dest cannot be RAX! (fix this!)
%macro VECTOR_REF 3
	mov %1, %2
	VECTOR_ELEMENTS %1
	lea %1, [%1 + %3*8]
	mov %1, qword [%1]
	mov %1, qword [%1]
%endmacro

%define SOB_UNDEFINED MAKE_LITERAL(T_UNDEFINED, 0)
%define SOB_VOID MAKE_LITERAL(T_VOID, 0)
%define SOB_FALSE MAKE_LITERAL(T_BOOL, 0)
%define SOB_TRUE MAKE_LITERAL(T_BOOL, 1)
%define SOB_NIL MAKE_LITERAL(T_NIL, 0)

;;; MAKE_DYNAMIC_PAIR target-addrees, car-addrees, cdr-addrees
%macro MAKE_DYNAMIC_PAIR 3
    push rax 
    push rbx 
    mov rax, %1 
    mov qword [rax], %2
    sub qword [rax], start_of_data
    shl qword [rax], ((WORD_SIZE - TYPE_BITS) >> 1) 
    mov rbx, %3 
    sub rbx, start_of_data
    or qword [rax], rbx 
    shl qword [rax], TYPE_BITS 
    or qword [rax], T_PAIR 
    pop rbx 
    pop rax 
%endmacro

;;; Simple frame access

%define param(offset) qword [rbp + offset]

struc scmframe
.old_rbp: resq 1
.ret_addr: resq 1
.env: resq 1
.arg_count: resq 1
.A0: resq 1
.A1: resq 1
.A2: resq 1
.A3: resq 1
.A4: resq 1
.A5: resq 1
endstruc

%define old_rbp param(scmframe.old_rbp)
%define ret_addr param(scmframe.ret_addr)
%define env param(scmframe.env)
%define arg_count param(scmframe.arg_count)
%define A0 param(scmframe.A0)
%define A1 param(scmframe.A1)
%define A2 param(scmframe.A2)
%define A3 param(scmframe.A3)
%define A4 param(scmframe.A4)
%define A5 param(scmframe.A5)
%define An(n) qword [rbp + 8 * (n + 4)]

;; Right after an applic, the stack is: [ env | arg_count | arg0 | ... | argN | ... ]
%define post_applic_arg_count qword [rsp + 8 * 1]

;;; MALLOC n -> malloc(8 * n)
%macro MALLOC 1
  mov rdi, %1
  sal rdi, 3
  call malloc
%endmacro

%macro PRIM_TYPE_PRED 1
	cmp arg_count, 1
  jne %%false
  mov rax, A0
  TYPE rax
  cmp rax, %1
  jne %%false
  mov rax, SOB_TRUE
  jmp %%end

%%false:
  mov rax, SOB_FALSE
%%end:
  nop
%endmacro

%macro PAIR_LENGTH 1
  mov rax, %1
  xor rcx, rcx
%%loop:
  mov rbx, rax
  CAR rbx
  cmp rbx, SOB_NIL
  je %%end
  inc rcx
  CDR rax
  jmp %%loop

%%end:
  mov rax, rcx
%endmacro

%macro NUMBER_TO_NUM_DEN 3
  mov %2, %1
  TYPE %2
  cmp %2, T_FRACTION
  je %%fraction
  mov %2, %1
  DATA %2
  mov %3, 1
  jmp %%end
%%fraction:
  mov %2, %1
  DATA_UPPER %2
  mov %3, %1
  DATA_LOWER %3
%%end:
  nop
%endmacro

extern exit, printf, scanf, malloc, c_gcd, c_divide, c_remainder, c_add_numerator, c_multiply
global main, write_sob, write_sob_if_not_void

prim_car:
  push rbp
  mov rbp, rsp

  cmp arg_count, 1
  jne .error
  mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_PAIR
  jne .error

  CAR rax
  jmp .end

.error:
  mov rax, SOB_NIL
.end:
  leave
  ret

prim_cdr:
  push rbp
  mov rbp, rsp

  cmp arg_count, 1
  jne .error
  mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_PAIR
  jne .error

  CDR rax
  jmp .end

.error:
  mov rax, SOB_NIL
.end:
  leave
  ret

prim_cons:
  push rbp
  mov rbp, rsp

  cmp arg_count, 2
  jne .error

  MALLOC 1
  push rax
  MALLOC 1
  push rax
  MALLOC 1
  mov rcx, rax
  mov rax, A1
  mov [rcx], rax
  pop rbx
  mov rax, A0
  mov [rbx], rax
  pop rax
  MAKE_DYNAMIC_PAIR rax, rbx, rcx
  mov rax, [rax]
  jmp .end

.error:
  mov rax, MAKE_LITERAL(T_PAIR, 0)
.end:
  leave
  ret

prim_equals:
  push rbp
  mov rbp, rsp

  mov rax, SOB_TRUE
  cmp arg_count, 1
  jl .false
  mov rsi, A0
  mov rbx, rsi
  TYPE rbx
  cmp rbx, T_INTEGER
  je .first_cont
  cmp rbx, T_FRACTION
  jne .false

.first_cont:
  DATA rsi
  mov rcx, arg_count
  sub rcx, 1
  cmp rcx, 0
  je .end

.loop:
  mov rdi, An(rcx)
  mov rbx, rdi
  TYPE rbx
  cmp rbx, T_INTEGER
  je .k_cont
  cmp rbx, T_FRACTION
  jne .false
.k_cont:
  DATA rdi
  cmp rsi, rdi
  jne .false
  loop .loop

  mov rax, SOB_TRUE
  jmp .end

.false:
  mov rax, SOB_FALSE
.end:
  leave
  ret

prim_less_than:
  push rbp
  mov rbp, rsp

  mov rcx, arg_count
  cmp rcx, 0
  je .false
  cmp rcx, 1
  je .true
  mov rbx, A0
  NUMBER_TO_NUM_DEN rbx, rax, rbx
  dec rcx
.loop:
  push rcx
  mov rdx, arg_count
  sub rdx, rcx
  mov rdi, An(rdx)
  mov rsi, rdi
  TYPE rsi
  cmp rsi, T_INTEGER
  je .loop_cont
  cmp rsi, T_FRACTION
  jne .end
.loop_cont:
  NUMBER_TO_NUM_DEN rdi, rsi, rdi
  push rdi
  push rsi
  push rbx
  push rax
  mov rdi, [rsp + 8]
  mov rsi, [rsp + 8 * 2]
  call c_multiply

  mov rbx, rax
  mov rdi, [rsp]
  mov rsi, [rsp + 8 * 3]
  call c_multiply

  add rsp, 8 * 4
  cmp rax, rbx
  jge .false

  mov rax, [rsp - 8 * 2]
  mov rbx, [rsp - 8]
  pop rcx
  dec rcx
  jnz .loop

.true:
  mov rax, SOB_TRUE
  jmp .end
.false:
  mov rax, SOB_FALSE
.end:
  leave
  ret

prim_greater_than:
  push rbp
  mov rbp, rsp

  mov rcx, arg_count
  cmp rcx, 0
  je .false
  cmp rcx, 1
  je .true
  mov rbx, A0
  NUMBER_TO_NUM_DEN rbx, rax, rbx
  dec rcx
.loop:
  push rcx
  mov rdx, arg_count
  sub rdx, rcx
  mov rdi, An(rdx)
  mov rsi, rdi
  TYPE rsi
  cmp rsi, T_INTEGER
  je .loop_cont
  cmp rsi, T_FRACTION
  jne .end
.loop_cont:
  NUMBER_TO_NUM_DEN rdi, rsi, rdi
  push rdi
  push rsi
  push rbx
  push rax
  mov rdi, [rsp + 8]
  mov rsi, [rsp + 8 * 2]
  call c_multiply

  mov rbx, rax
  mov rdi, [rsp]
  mov rsi, [rsp + 8 * 3]
  call c_multiply

  add rsp, 8 * 4
  cmp rax, rbx
  jle .false

  mov rax, [rsp - 8 * 2]
  mov rbx, [rsp - 8]
  pop rcx
  dec rcx
  jnz .loop

.true:
  mov rax, SOB_TRUE
  jmp .end
.false:
  mov rax, SOB_FALSE
.end:
  leave
  ret

prim_add:
  push rbp
  mov rbp, rsp

  xor rax, rax
  xor rbx, rbx
  mov rcx, arg_count
  cmp rcx, 0
  je .int
  NUMBER_TO_NUM_DEN rbx, rax, rbx
.loop:
  push rcx
  mov rdx, arg_count
  sub rdx, rcx
  mov rdi, An(rdx)
  mov rsi, rdi
  TYPE rsi
  cmp rsi, T_INTEGER
  je .loop_cont
  cmp rsi, T_FRACTION
  jne .end
.loop_cont:
  NUMBER_TO_NUM_DEN rdi, rsi, rdi
  push rdi
  push rsi
  push rbx
  push rax
  mov rdi, [rsp + 8 * 1]
  mov rsi, [rsp + 8 * 3]
  call c_multiply

  mov rbx, rax
  mov rdi, [rsp]
  mov rsi, [rsp + 8 * 1]
  mov rdx, [rsp + 8 * 2]
  mov rcx, [rsp + 8 * 3]
  call c_add_numerator
  
  add rsp, 8 * 4
  pop rcx
  dec rcx
  jnz .loop

.loop_end:
  push rbx
  push rax
  mov rdi, rax
  mov rsi, rbx
  call c_gcd

  push rax
  mov rdi, [rsp + 8]
  mov rsi, [rsp]
  call c_divide

  push rax
  mov rdi, [rsp + 8 * 3]
  mov rsi, [rsp + 8]
  call c_divide

  mov rbx, rax
  pop rax
  add rsp, 8 * 3
  cmp rbx, 1
  je .int
  cmp rbx, -1
  je .int
  cmp rbx, 0
  jl .negative_fraction
  MAKE_LITERAL_FRACTION_2 rax, rbx
  jmp .end
.negative_fraction:
  neg rax
  neg rbx
  MAKE_LITERAL_FRACTION_2 rax, rbx
  jmp .end
.int:
  MAKE_LITERAL_FROM_REG T_INTEGER, rax
.end:
  leave
  ret

prim_mul:
  push rbp
  mov rbp, rsp

  mov rax, 1
  mov rbx, rax
  mov rcx, arg_count
  cmp rcx, 0
  je .int
  
.loop:
  push rcx
  mov rdx, arg_count
  sub rdx, rcx
  mov rdi, An(rdx)
  mov rsi, rdi
  TYPE rsi
  cmp rsi, T_INTEGER
  je .loop_cont
  cmp rsi, T_FRACTION
  jne .end
.loop_cont:
  NUMBER_TO_NUM_DEN rdi, rsi, rdi
  push rdi
  push rsi
  push rbx
  push rax
  mov rdi, [rsp + 8 * 1]
  mov rsi, [rsp + 8 * 3]
  call c_multiply

  mov rbx, rax
  mov rdi, [rsp]
  mov rsi, [rsp + 8 * 2]
  call c_multiply

  add rsp, 8 * 4
  pop rcx
  dec rcx
  jnz .loop

.loop_end:
  push rbx
  push rax
  mov rdi, rax
  mov rsi, rbx
  call c_gcd

  push rax
  mov rdi, [rsp + 8]
  mov rsi, [rsp]
  call c_divide

  push rax
  mov rdi, [rsp + 8 * 3]
  mov rsi, [rsp + 8]
  call c_divide

  mov rbx, rax
  pop rax
  add rsp, 8 * 3
  cmp rbx, 1
  je .int
  cmp rbx, -1
  je .negative_int
  cmp rbx, 0
  jge .fraction
  neg rax
  neg rbx
.fraction:
  MAKE_LITERAL_FRACTION_2 rax, rbx
  jmp .end
.negative_int:
  neg rax
.int:
  MAKE_LITERAL_FROM_REG T_INTEGER, rax
.end:
  leave
  ret


; TODO: division
prim_div:
  push rbp
  mov rbp, rsp

  mov rax, 1
  mov rbx, rax
  mov rcx, arg_count
  cmp rcx, 1
  jl .int
  cmp rcx, 1
  je .loop
  mov rbx, A0
  NUMBER_TO_NUM_DEN rbx, rax, rbx
  dec rcx
  
.loop:
  push rcx
  mov rdx, arg_count
  sub rdx, rcx
  mov rdi, An(rdx)
  mov rsi, rdi
  TYPE rsi
  cmp rsi, T_INTEGER
  je .loop_cont
  cmp rsi, T_FRACTION
  jne .end
.loop_cont:
  NUMBER_TO_NUM_DEN rdi, rsi, rdi
  push rdi
  push rsi
  push rbx
  push rax
  mov rdi, [rsp + 8]
  mov rsi, [rsp + 8 * 2]
  call c_multiply

  mov rbx, rax
  mov rdi, [rsp]
  mov rsi, [rsp + 8 * 3]
  call c_multiply

  add rsp, 8 * 4
  pop rcx
  dec rcx
  jnz .loop

.loop_end:
  push rbx
  push rax
  mov rdi, rax
  mov rsi, rbx
  call c_gcd

  push rax
  mov rdi, [rsp + 8]
  mov rsi, [rsp]
  call c_divide

  push rax
  mov rdi, [rsp + 8 * 3]
  mov rsi, [rsp + 8]
  call c_divide

  mov rbx, rax
  pop rax
  add rsp, 8 * 3
  cmp rbx, 1
  je .int
  cmp rbx, -1
  je .negative_int
  cmp rbx, 0
  jge .fraction
  neg rax
  neg rbx
.fraction:
  MAKE_LITERAL_FRACTION_2 rax, rbx
  jmp .end
.negative_int:
  neg rax
.int:
  MAKE_LITERAL_FROM_REG T_INTEGER, rax
.end:
  leave
  ret

prim_eq:
	push rbp
	mov rbp, rsp

	cmp arg_count, 2
  jne .false
  mov rax, A0
  mov rbx, A1
  cmp rax, rbx
  jne .false
  mov rax, SOB_TRUE
  jmp .end

.false:
  mov rax, SOB_FALSE
.end:
	leave
	ret

prim_boolean:
  push rbp
	mov rbp, rsp

	PRIM_TYPE_PRED T_BOOL

	leave
	ret

prim_char:
  push rbp
	mov rbp, rsp

	PRIM_TYPE_PRED T_CHAR

	leave
	ret

prim_procedure:
  push rbp
	mov rbp, rsp

	PRIM_TYPE_PRED T_CLOSURE

	leave
	ret

prim_integer:
  push rbp
	mov rbp, rsp

	PRIM_TYPE_PRED T_INTEGER

	leave
	ret

prim_rational:
  push rbp
	mov rbp, rsp

	PRIM_TYPE_PRED T_INTEGER
  cmp rax, SOB_TRUE
  je .end
	PRIM_TYPE_PRED T_FRACTION
.end:
	leave
	ret

prim_pair:
  push rbp
	mov rbp, rsp

	PRIM_TYPE_PRED T_PAIR

	leave
	ret

prim_string:
  push rbp
	mov rbp, rsp

	PRIM_TYPE_PRED T_STRING

	leave
	ret

prim_symbol:
  push rbp
	mov rbp, rsp

	PRIM_TYPE_PRED T_SYMBOL

	leave
	ret

prim_vector:
  push rbp
	mov rbp, rsp

	PRIM_TYPE_PRED T_VECTOR

	leave
	ret

prim_integer_to_char:
  push rbp
	mov rbp, rsp

	cmp arg_count, 1
  jne .error
  mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_INTEGER
  jne .error
  mov rbx, rax
  DATA rbx
  cmp rbx, 0
  jl .error
  cmp rbx, 256
  jge .error

  xor rax, T_CHAR ^ T_INTEGER
  jmp .end

.error:
  mov rax, MAKE_LITERAL(T_CHAR, -1)
.end:
	leave
	ret

prim_char_to_integer:
  push rbp
	mov rbp, rsp

	cmp arg_count, 1
  jne .error
  mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_CHAR
  jne .error
  mov rbx, rax
  DATA rbx
  cmp rbx, 0
  jl .error
  cmp rbx, 256
  jge .error

  xor rax, T_INTEGER ^ T_CHAR
  jmp .end

.error:
  mov rax, MAKE_LITERAL(T_INTEGER, -1)
.end:
	leave
	ret

prim_remainder:
  push rbp
	mov rbp, rsp

  cmp arg_count, 2
  jne .error
	mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_INTEGER
  jne .error
  mov rax, A1
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_INTEGER
  jne .error

  xor rax, rax
  mov rsi, A1
  DATA rsi
  mov rdi, A0
  DATA rdi
  call c_remainder
  MAKE_LITERAL_FROM_REG T_INTEGER, rax
  jmp .end

.error:
  mov rax, MAKE_LITERAL(T_INTEGER, -1)
.end:
	leave
	ret

prim_numerator:
	push rbp
	mov rbp, rsp

	cmp arg_count, 1
  jne .error
  mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_FRACTION
  jne .integer

  DATA_UPPER rax
  MAKE_LITERAL_FROM_REG T_INTEGER, rax
  jmp .end

.integer:
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_INTEGER
  jne .error
  DATA rax
  MAKE_LITERAL_FROM_REG T_INTEGER, rax
  jmp .end

.error:
  mov rax, MAKE_LITERAL(T_INTEGER, -1)
.end:
	leave
	ret

prim_denominator:
	push rbp
	mov rbp, rsp

	cmp arg_count, 1
  jne .error
  mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_FRACTION
  jne .integer

  DATA_LOWER rax
  MAKE_LITERAL_FROM_REG T_INTEGER, rax
  jmp .end

.integer:
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_INTEGER
  jne .error
  mov rax, MAKE_LITERAL(T_INTEGER, 1)
  jmp .end

.error:
  mov rax, MAKE_LITERAL(T_INTEGER, -1)
.end:
	leave
	ret

prim_not:
	push rbp
	mov rbp, rsp

  cmp arg_count, 1
  jne .false
	mov rax, A0
  cmp rax, SOB_FALSE
  jne .false
  mov rax, SOB_TRUE
  jmp .end

.false:
  mov rax, SOB_FALSE
.end:
	leave
	ret

prim_string_length:
	push rbp
	mov rbp, rsp

  cmp arg_count, 1
  jne .error
	mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_STRING
  jne .error

  STRING_LENGTH rax
  MAKE_LITERAL_FROM_REG T_INTEGER, rax
  jmp .end

.error:
  mov rax, MAKE_LITERAL(T_INTEGER, -1)
.end:
	leave
	ret

prim_string_ref:
  push rbp
	mov rbp, rsp

  cmp arg_count, 2
  jne .error
	mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_STRING
  jne .error
  mov rax, A1
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_INTEGER
  jne .error

  mov rbx, rax
  DATA rbx
  mov rax, A0
  STRING_ELEMENTS rax
  mov rax, [rax + rbx]
  MAKE_LITERAL_FROM_REG T_CHAR, rax
  jmp .end

.error:
  mov rax, MAKE_LITERAL(T_INTEGER, -1)
.end:
	leave
	ret

prim_vector_length:
	push rbp
	mov rbp, rsp

  cmp arg_count, 1
  jne .error
	mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_VECTOR
  jne .error

  VECTOR_LENGTH rax
  MAKE_LITERAL_FROM_REG T_INTEGER, rax
  jmp .end

.error:
  mov rax, MAKE_LITERAL(T_INTEGER, -1)
.end:
	leave
	ret

prim_vector_ref:
  push rbp
	mov rbp, rsp

  cmp arg_count, 2
  jne .error
	mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_VECTOR
  jne .error
  mov rax, A1
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_INTEGER
  jne .error

  mov rbx, rax
  DATA rbx
  mov rax, A0
  VECTOR_ELEMENTS rax
  mov rax, [rax + 8 * rbx]
  mov rax, [rax]
  jmp .end

.error:
  mov rax, MAKE_LITERAL(T_INTEGER, -1)
.end:
	leave
	ret

prim_apply:
	push rbp
	mov rbp, rsp

	cmp arg_count, 2
  jne .error
  mov rax, A0
  TYPE rax
  cmp rax, T_CLOSURE
  jne .error
  mov rax, A1
  TYPE rax
  cmp rax, T_PAIR
  jne .error

  mov rsi, A0
  mov rdi, A1

  PAIR_LENGTH A1
  sal rax, 3
  sub rsp, rax
  add rsp, 8 * 3

  PAIR_LENGTH A1
  mov rcx, rax
  mov rax, ret_addr
  mov rbx, env
  mov rbp, old_rbp
  mov [rsp], rax
  mov [rsp + 8 * 1], rbx
  mov [rsp + 8 * 2], rcx

  xor rcx, rcx
.loop:
  cmp rdi, SOB_NIL
  je .end
  mov rax, rdi
  CAR rax
  mov [rsp + 8 * (3 + rcx)], rax
  inc rcx
  CDR rdi
  jmp .loop
.end:
  CLOSURE_CODE rsi
  jmp rsi
.error:
  mov rax, SOB_NIL
	leave
	ret

prim_symbol_to_string:
	push rbp
	mov rbp, rsp

  cmp arg_count, 1
  jne .error
  mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_SYMBOL
  jne .error

	DATA rax
  add rax, start_of_data
  mov rax, [rax]
  jmp .end

.error:
  mov rax, .error_sob_str
.end:
	leave
	ret

section .data
.error_sob_str:
  MAKE_LITERAL_STRING ""

prim_string_to_symbol:
	push rbp
	mov rbp, rsp

  cmp arg_count, 1
  jne .error
  mov rax, A0
  mov rbx, rax
  TYPE rbx
  cmp rbx, T_STRING
  jne .error

  MALLOC 1
  mov rbx, rax
	mov rax, A0
  mov [rbx], rax
  MAKE_LITERAL_SYMBOL_2 rbx
  jmp .end

.error:
  mov rax, MAKE_LITERAL_SYMBOL(start_of_data)
.end:
	leave
	ret

write_sob_undefined:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .undefined
	call printf

	leave
	ret

section .data
.undefined:
	db "#<undefined>", 0

write_sob_integer:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	sar rsi, TYPE_BITS
	mov rdi, .int_format_string
	mov rax, 0
	call printf

	leave
	ret

section .data
.int_format_string:
	db "%ld", 0

write_sob_char:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	DATA rsi

	cmp rsi, CHAR_NUL
	je .Lnul

	cmp rsi, CHAR_TAB
	je .Ltab

	cmp rsi, CHAR_NEWLINE
	je .Lnewline

	cmp rsi, CHAR_PAGE
	je .Lpage

	cmp rsi, CHAR_RETURN
	je .Lreturn

	cmp rsi, CHAR_SPACE
	je .Lspace
	jg .Lregular

	mov rdi, .special
	jmp .done	

.Lnul:
	mov rdi, .nul
	jmp .done

.Ltab:
	mov rdi, .tab
	jmp .done

.Lnewline:
	mov rdi, .newline
	jmp .done

.Lpage:
	mov rdi, .page
	jmp .done

.Lreturn:
	mov rdi, .return
	jmp .done

.Lspace:
	mov rdi, .space
	jmp .done

.Lregular:
	mov rdi, .regular
	jmp .done

.done:
	mov rax, 0
	call printf

	leave
	ret

section .data
.space:
	db "#\space", 0
.newline:
	db "#\newline", 0
.return:
	db "#\return", 0
.tab:
	db "#\tab", 0
.page:
	db "#\page", 0
.nul:
	db "#\nul", 0
.special:
	db "#\x%02x", 0
.regular:
	db "#\\%c", 0

write_sob_void:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .void
	call printf

	leave
	ret

section .data
.void:
	db "#<void>", 0
	
write_sob_bool:
	push rbp
	mov rbp, rsp

	mov rax, qword [rbp + 8 + 1*8]
	cmp rax, SOB_FALSE
	je .sobFalse
	
	mov rdi, .true
	jmp .continue

.sobFalse:
	mov rdi, .false

.continue:
	mov rax, 0
	call printf	

	leave
	ret

section .data			
.false:
	db "#f", 0
.true:
	db "#t", 0

write_sob_nil:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .nil
	call printf

	leave
	ret

section .data
.nil:
	db "()", 0

write_sob_string:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .double_quote
	call printf

	mov rax, qword [rbp + 8 + 1*8]
	mov rcx, rax
	STRING_LENGTH rcx
	STRING_ELEMENTS rax

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rbx, CHAR_TAB
	je .ch_tab
	cmp rbx, CHAR_NEWLINE
	je .ch_newline
	cmp rbx, CHAR_PAGE
	je .ch_page
	cmp rbx, CHAR_RETURN
	je .ch_return
	cmp rbx, CHAR_SPACE
	jl .ch_hex
	
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx
	jmp .printf
	
.ch_tab:
	mov rdi, .fs_tab
	mov rsi, rbx
	jmp .printf
	
.ch_page:
	mov rdi, .fs_page
	mov rsi, rbx
	jmp .printf
	
.ch_return:
	mov rdi, .fs_return
	mov rsi, rbx
	jmp .printf

.ch_newline:
	mov rdi, .fs_newline
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .double_quote
	call printf

	leave
	ret
section .data
.double_quote:
	db '"', 0
.fs_simple_char:
	db "%c", 0
.fs_hex_char:
	db "\x%02x;", 0	
.fs_tab:
	db "\t", 0
.fs_page:
	db "\f", 0
.fs_return:
	db "\r", 0
.fs_newline:
	db "\n", 0

write_sob_pair:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .open_paren
	call printf
	mov rax, qword [rbp + 8 + 1*8]
	CAR rax
	push rax
	call write_sob
	add rsp, 1*8
	mov rax, qword [rbp + 8 + 1*8]
	CDR rax
	push rax
	call write_sob_pair_on_cdr
	add rsp, 1*8
	mov rdi, .close_paren
	mov rax, 0
	call printf

	leave
	ret

section .data
.open_paren:
	db "(", 0
.close_paren:
	db ")", 0

write_sob_pair_on_cdr:
	push rbp
	mov rbp, rsp

	mov rbx, qword [rbp + 8 + 1*8]
	mov rax, rbx
	TYPE rbx
	cmp rbx, T_NIL
	je .done
	cmp rbx, T_PAIR
	je .cdrIsPair
	push rax
	mov rax, 0
	mov rdi, .dot
	call printf
	call write_sob
	add rsp, 1*8
	jmp .done

.cdrIsPair:
	mov rbx, rax
	CDR rbx
	push rbx
	CAR rax
	push rax
	mov rax, 0
	mov rdi, .space
	call printf
	call write_sob
	add rsp, 1*8
	call write_sob_pair_on_cdr
	add rsp, 1*8

.done:
	leave
	ret

section .data
.space:
	db " ", 0
.dot:
	db " . ", 0

write_sob_vector:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .fs_open_vector
	call printf

	mov rax, qword [rbp + 8 + 1*8]
	mov rcx, rax
	VECTOR_LENGTH rcx
	cmp rcx, 0
	je .done
	VECTOR_ELEMENTS rax

	push rcx
	push rax
	mov rax, qword [rax]
	push qword [rax]
	call write_sob
	add rsp, 1*8
	pop rax
	pop rcx
	dec rcx
	add rax, 8

.loop:
	cmp rcx, 0
	je .done

	push rcx
	push rax
	mov rax, 0
	mov rdi, .fs_space
	call printf
	
	pop rax
	push rax
	mov rax, qword [rax]
	push qword [rax]
	call write_sob
	add rsp, 1*8
	pop rax
	pop rcx
	dec rcx
	add rax, 8
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .fs_close_vector
	call printf

	leave
	ret

section	.data
.fs_open_vector:
	db "#(", 0
.fs_close_vector:
	db ")", 0
.fs_space:
	db " ", 0

write_sob_symbol:
	push rbp
	mov rbp, rsp

  mov rax, qword [rbp + 8 + 1*8]
  DATA rax
  add rax, start_of_data
  mov rax, [rax]
  mov rcx, rax
	STRING_LENGTH rcx
	STRING_ELEMENTS rax

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xfff
  mov rdi, .fs_simple_char
	mov rsi, rbx
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	leave
	ret

section .data
.fs_simple_char:
  db "%c", 0

write_sob_fraction:
	push rbp
	mov rbp, rsp

  mov rax, qword [rbp + 8 + 1*8]
  mov rbx, rax
  DATA_UPPER rax
  DATA_LOWER rbx

  mov rdx, rbx
  mov rsi, rax
  mov rdi, .frac_format_string
  xor rax, rax
  call printf

	leave
	ret

section .data
.frac_format_string:
	db "%ld/%ld", 0

write_sob_closure:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	mov rdx, rsi
	CLOSURE_ENV rsi
	CLOSURE_CODE rdx
	mov rdi, .closure
	mov rax, 0
	call printf

	leave
	ret
section .data
.closure:
	db "#<closure [env:%p, code:%p]>", 0

write_sob:
	mov rax, qword [rsp + 1*8]
	TYPE rax
	jmp qword [.jmp_table + rax * 8]

section .data
.jmp_table:
	dq write_sob_undefined, write_sob_void, write_sob_nil
	dq write_sob_integer, write_sob_fraction, write_sob_bool
	dq write_sob_char, write_sob_string, write_sob_symbol
	dq write_sob_closure, write_sob_pair, write_sob_vector

section .text
write_sob_if_not_void:
	mov rax, qword [rsp + 1*8]
	cmp rax, SOB_VOID
	je .continue

	push rax
	call write_sob
	add rsp, 1*8
	mov rax, 0
	mov rdi, .newline
	call printf
	
.continue:
	ret
section .data
.newline:
	db CHAR_NEWLINE, 0
	
	
	

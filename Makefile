CC		:=	gcc -m64
CC_FLAGS	:=	-Wall -g
ASM		:=	nasm
ASM_FLAGS	:=	-f elf64

%:
	echo \
	'(load "ass1/pc.scm")' \
	'(load "ass1/sexpr-parser.scm")' \
	'(load "ass2/pattern-matcher.scm")' \
	'(load "ass2/qq.scm")' \
	'(load "ass2/tag-parser.scm")' \
	'(load "ass3/semantic-analyzer.scm")' \
	'(load "compiler.scm")' \
	'(compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	$(ASM) $(ASM_FLAGS) $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	$(CC) $(CC_FLAGS) $(MAKECMDGOALS).o -o $(MAKECMDGOALS)

clean:
	rm test test.s test.o

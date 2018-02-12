CC		:=	gcc
CC_FLAGS	:=	-m64 -g -Wall
ASM		:=	nasm
ASM_FLAGS	:=	-g -f elf64

%:
	echo \
	'(load "ass1/pc.scm")' \
	'(load "ass1/sexpr-parser.scm")' \
	'(load "ass2/pattern-matcher.scm")' \
	'(load "ass2/qq.scm")' \
	'(load "ass2/tag-parser.scm")' \
	'(load "ass3/semantic-analyzer.scm")' \
	'(load "compiler.scm")' \
	'(load "builtins.scm")' \
	'(compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	$(ASM) $(ASM_FLAGS) $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	$(CC) -c $(CC_FLAGS) util.c -o util.o
	$(CC) $(CC_FLAGS) $(MAKECMDGOALS).o util.o -o $(MAKECMDGOALS)

.PHONY: clean
clean:
	rm test test.s test.o util.o

CC		:=	gcc
CC_FLAGS	:=	-m64 -g
ASM		:=	nasm
ASM_FLAGS	:=	-g -f elf64

%:
	echo \
	'(load "./project/ass1/pc.scm")' \
	'(load "./project/ass1/sexpr-parser.scm")' \
	'(load "./project/ass2/pattern-matcher.scm")' \
	'(load "./project/ass2/qq.scm")' \
	'(load "./project/ass2/tag-parser.scm")' \
	'(load "./project/ass3/semantic-analyzer.scm")' \
	'(load "./project/compiler.scm")' \
	'(load "./project/builtins.scm")' \
	'(compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	$(ASM) $(ASM_FLAGS) $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	$(CC) -c $(CC_FLAGS) ./project/util.c -o util.o
	$(CC) $(CC_FLAGS) $(MAKECMDGOALS).o util.o -o $(MAKECMDGOALS)

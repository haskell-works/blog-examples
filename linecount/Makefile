SRC_FILES := $(wildcard *.c)

%.s : %.c
	gcc -O2 -mavx2 -mbmi2 -fno-tree-vectorize -S $< -o $@

%.auto.s : %.c
	gcc -O2 -mavx2 -mbmi2 -S $< -o $@

%.out : %.c
	gcc -O2 -mavx2 -mbmi2 -fno-tree-vectorize $< -o $@

%.auto.out : %.c
	gcc -O2 -mavx2 -mbmi2 $< -o $@

all: \
		$(patsubst %.c,%.s,$(SRC_FILES)) \
		$(patsubst %.c,%.out,$(SRC_FILES)) \
		$(patsubst %.c,%.auto.s,$(SRC_FILES)) \
		$(patsubst %.c,%.auto.out,$(SRC_FILES))

clean:
	rm *.s
	rm *.out

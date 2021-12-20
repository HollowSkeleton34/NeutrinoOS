# Set the C compiler and debugger
CC = clang
GDB = gdb 

LD = ld
ASM = nasm

INC = -Ikernel/inc/
SRC = $(shell find . -type f -name "*.c")
ASM_SRC = $(shell find . -type f -name "*.asm")

# CRITICAL: assembly must be linked first
OBJ = ${ASM_SRC:.asm=.o} ${SRC:.c=.o}

CCFLAGS = -nostdinc -ggdb -c -ffreestanding -fno-omit-frame-pointer -target x64-none-elf
LDFLAGS = -T link.ld -m elf_x86_64
LDFLAGS_BIN = ${LDFLAGS} --oformat binary
ASFLAGS = -f elf64

all: os os.elf

os: ${OBJ}
	${LD} -o $@ ${LDFLAGS_BIN} $^

os.elf: ${OBJ}
	${LD} -o $@ ${LDFLAGS} $^
	find . -type f -name '*.o' -exec rm {} +

%.o: %.c
	${CC} ${CCFLAGS} ${INC} -o $@ $^

%.o: %.asm
	${ASM} $< ${ASFLAGS} -o $@

clean:
	rm -f os os.elf *.o **/*.o
	find . -type f -name '*.o' -exec rm {} +
TEST_DIR  = test

CC = gcc
CFLAGS = -g -nostdlib -static

EXE = sforth

all: $(EXE)

run: $(EXE)
	@./$(EXE); echo "**Exited with status $$?**"

db: $(EXE)
	gdb $(EXE)

table: $(EXE)
	objdump -t $(EXE)

$(EXE) : % : %.S
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(EXE)

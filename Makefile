CC = gcc
CFLAGS = -g -nostdlib -static

sforth: sforth.S
	$(CC) $(CFLAGS) -o $@ $<

test: sforth
	@./test.sh

run: sforth
	@./run.sh

clean:
	rm -f sforth

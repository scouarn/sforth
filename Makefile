CC = gcc
CFLAGS = -g -nostdlib -static

sforth: sforth.S
	$(CC) $(CFLAGS) -o $@ $<

run: sforth
	@./run.sh

clean:
	rm -f sforth

sforth: sforth.S
	as -o sforth.o $< && ld a.out -o $@
	rm sforth.o

test: sforth
	@./test.sh

run: sforth
	@./run.sh

clean:
	rm -f sforth

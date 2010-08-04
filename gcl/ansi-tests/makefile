-include ../makedefs

test-unixport:
	echo "(load \"gclload.lsp\")" | ../unixport/saved_ansi_gcl$(EXE) | tee test.out

test:
	echo "(load \"gclload.lsp\")" | gcl | tee test.out

clean:
	rm -f test.out *.fasl *.o *.so *~ *.fn *.x86f *.fasl *.ufsl

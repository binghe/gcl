-include ../makedefs

test-unixport:
	echo "(load \"gclload.lsp\")" | ../unixport/saved_ansi_gcl$(EXE) | tee test.out

test:
	echo "(load \"gclload.lsp\")" | gcl | tee test.out

clean:
	rm -f test.out *.fasl *.o *.so *~ *.fn *.x86f *.fasl *.ufsl
	rm -f foo.txt temp.dat file-that-was-renamed.txt tmp.dat tmp.dat.BAK tmp2.dat
	rm -rf scratch tmp.txt foo.lsp 'CLTEST:foo.txt'

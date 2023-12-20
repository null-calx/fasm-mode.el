.POSIX:
.SUFFIXES: .el .elc
EMACS = emacs

compile: fasm-mode.elc

clean:
	rm -f fasm-mode.elc

.el.elc:
	$(EMACS) -Q -batch -f batch-byte-compile $<

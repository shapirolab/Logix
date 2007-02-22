# fcp make
SHELL = /bin/csh
GCC = gcc -arch ppc -m32
CFLAGS = -c -DDEBUG -DMACINTOSH

INCLUDES = -I.

OFILES = concatenate.o ctl.o dist.o emulate.o externs.o fcp.o file.o \
	 freeze_term.o freezer.o global.o heap.o interface.o kernels.o \
	 link_static.o logix.o math.o notify.o spicomm.o spiweight.o \
	 streams.o timer.o tty.o unify.o utility.o


LIBS   = -lm -lc

fcp: \
	$(OFILES)
	$(GCC) $(OFILES) $(LIBS) -o fcp

spiweight.o: \
	spiweight.c
	$(GCC) $(CFLAGS) $(OPT) $(INCLUDES) spiweight.c

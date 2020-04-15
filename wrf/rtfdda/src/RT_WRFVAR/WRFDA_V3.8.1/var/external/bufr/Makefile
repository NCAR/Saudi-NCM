.SUFFIXES: .c .f .for .o

include make.filelist

AR = ar
ARFLAGS = cr
LIB_TARGET = libbufr.a

library:$(OBJ)
	$(AR) $(ARFLAGS) $(LIB_TARGET) $(OBJ)
	$(RANLIB) $(LIB_TARGET)

.c.o:
	$(RM) $@
	$(CC) -o $@ -c $(CFLAGS) -DUNDERSCORE $*.c

.f.o:
	$(FC) -c $(FFLAGS) $< 

.for.o:
	./preproc.sh $*.for
	$(FC) -c $(FFLAGS) $*.f

clean:
	rm -f $(OBJ) $(LIB_TARGET) irev.f wrdlen.f

superclean: clean

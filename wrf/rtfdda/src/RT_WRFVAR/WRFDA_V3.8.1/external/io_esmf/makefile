# these settings for compiling standalone on Compaq. Type "make -r"
#CPP = /lib/cpp 
#FC  = f90 -free

.SUFFIXES: .F90 .o

AR = ar
#RANLIB	= ranlib
RANLIB	= echo

OBJS = module_symbols_util.o \
       module_esmf_extensions.o \
       module_utility.o \
       io_esmf.o \
       ext_esmf_open_for_read.o \
       ext_esmf_open_for_write.o \
       ext_esmf_read_field.o \
       ext_esmf_write_field.o

LIB_TARGET = libwrfio_esmf.a

$(LIB_TARGET) : $(OBJS)
	$(AR) ru $(LIB_TARGET) $(OBJS)
	$(RANLIB) $(LIB_TARGET)

.F90.o :
	$(CPP) -I../ioapi_share -P -traditional -DESMF_COUPLING $*.F90 > $*.f
	$(FC) -c -g -I../ioapi_share $*.f

superclean: 
	/bin/rm -f *.f *.o $(LIB_TARGET) *.mod

# DEPENDENCIES : only dependencies after this line 

module_utility.o : module_symbols_util.o module_esmf_extensions.o
io_esmf.o : module_esmf_extensions.o
ext_esmf_open_for_read.o : io_esmf.o
ext_esmf_open_for_write.o : io_esmf.o
ext_esmf_read_field.o : io_esmf.o
ext_esmf_write_field.o : io_esmf.o


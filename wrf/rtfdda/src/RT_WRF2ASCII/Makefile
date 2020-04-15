###########################################################################
#
# Makefile for Modeling System (MS)
# RTFDDA/src/RT_WRF2ASCII
#
# Dependencies:
#      (1) You must have the MS_MAKE_MACROS_DIR environment variable defined
#          and it must point to the directory of Modeling System Makefile
#          include files.
#      (2) You must have the MS_OS_TYPE environment variable defined and it
#          must be one of the types in:
#          $MS_MAKE_MACROS_DIR/compiler_macros.<ms_os_type>.
#
# Mar 2007
#
###########################################################################

# ---------- Definitions -----------
#
# Include the standard shared make info (make_defns) and the
# operating system specific info (compiler_macros.$(MS_OS_TYPE))

include $(MS_MAKE_MACROS_DIR)/make_defns
include $(MS_MAKE_MACROS_DIR)/compiler_macros.$(MS_OS_TYPE)

# --------- Object files ----------

EXECS	=	wrf2ascii4medoc.exe \
                wrf2ascii4medoc_uve.exe \
                transpose_array.exe \
		wrf2prf.exe

OBJ = 	wrf_arrays.o \
	wrf2ascii4medoc.o \
	wrf2ascii4medoc_uve.o \
        transpose_array.o \
	wrf2prf.o

MOD =	wrf_arrays.mod subs.mod

# --------- Local compile/link definitions -----

NET		=	$(NETCDF)

LOC_INCLUDES	=	$(NET_INCS)
LOC_LDLIBS	=	$(LDLIBS) $(NET_LDLIBS)
LOC_LDFLAGS	=	$(NET_LDFLAGS) -Wl,-rpath,$(LD_LIBRARY_PATH):$(NET)/lib -limf -lm
LOC_F90_LDFLAGS	=	
LOC_F90_FLAGS	=	

# --------- Targets --------------

all: $(MOD) $(EXECS)

install: all
	$(CP) $(EXECS) $(DEST)

wrf2ascii4medoc.exe: wrf2ascii4medoc.o wrf_arrays.o
	$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)

wrf2ascii4medoc_uve.exe: wrf2ascii4medoc_uve.o wrf_arrays.o
	$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)

transpose_array.exe: transpose_array.o
	$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)

wrf2prf.exe: wrf2prf.o wrf_arrays.o subs.o
	$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)

clean:
	$(RM) $(CLEAN_LIST) $(EXECS) $(OBJ) *.mod

add2readme:
	for exe in ${EXECS} ;\
	do \
		echo $${exe} -- source: `$(PWD)`, `$(DATE)`>> $(DEST_SRC_README);\
	done

# --------- Dependencies ----------

# DO NOT DELETE THIS LINE -- make depend depends on it.


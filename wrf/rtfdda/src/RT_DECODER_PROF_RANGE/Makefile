###########################################################################
#
# Makefile for Modeling System (MS)
# RTFDDA/src/RT_DECODER_CLARUS
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

EXEC	=	decode_profiler.exe	

OBJ = 	wrf_arrays.o \
        datetime_module.o \
        decode_profiler.o

NETCDF_MOD =	netcdf.mod \
		typesizes.mod

MOD =	wrf_arrays.mod \
        datetime_module.mod

# --------- Local compile/link definitions -----

NET		=	$(NETCDF)
LOC_INCLUDES	=	
LOC_LDLIBS	=	$(NET_LDLIBS)
LOC_LDFLAGS	=	-Wl,-rpath,$(NET)/lib $(NET_LDFLAGS)
LOC_F90_LDFLAGS	=	
LOC_F90_FLAGS	=	

# --------- Targets --------------

all: $(NETCDF_MOD) $(MOD) $(EXEC)

install: all
	$(CP) $(EXEC) $(DEST)

$(EXEC): $(OBJ)
	$(F90) -o $@ $? $(F90_POST_LINK)

$(NETCDF_MOD):
	$(LN) $(NET)/include/$@ $@

$(MOD): $(NETCDF_MOD)

clean:
	$(RM) $(CLEAN_LIST) $(EXEC) $(OBJ) *.mod

add2readme:
	for exe in ${EXEC} ;\
	do \
		echo $${exe} -- source: `$(PWD)`, `$(DATE)`>> $(DEST_SRC_README);\
	done

# --------- Dependencies ----------

# DO NOT DELETE THIS LINE -- make depend depends on it.


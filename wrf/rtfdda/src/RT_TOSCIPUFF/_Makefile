###########################################################################
#
# Makefile for Modeling System (MS)
# RTFDDA/RT_TOSCIPUFF
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

OBJS	= cnvmm5e.o cnvmm5e_bin.o for_scipuff_RT.o medoc_driver.o medoc_driver_bin.o
EXECS	= medoc_driver.exe medoc_driver_uve.exe medoc_driver_bin.exe mm52scipuf

# --------- Local compile/link definitions -----

LOC_LDFLAGS	=
LOC_FC_FLAGS	=	$(FFLAGS90)

# --------- Targets --------------

all: $(EXECS)

install: $(EXECS)
	$(CP) $(EXECS) $(DEST)

clean:
	$(RM) $(CLEAN_LIST) $(EXECS)

add2readme:
	for exec in ${EXECS} ;\
	do \
		echo $${exec} -- source: `$(PWD)`, `$(DATE)`>> $(DEST_SRC_README);\
	done

medoc_driver.exe: cnvmm5e.o medoc_driver.o
	$(F90_PRE_LINK) -o $@ cnvmm5e.o medoc_driver.o $(F90_POST_LINK)

medoc_driver_uve.exe: cnvmm5e.o medoc_driver_uve.o
	$(F90_PRE_LINK) -o $@ cnvmm5e.o medoc_driver_uve.o $(F90_POST_LINK)

medoc_driver_bin.exe: cnvmm5e_bin.o medoc_driver_bin.o
	$(F90_PRE_LINK) -o $@ cnvmm5e_bin.o medoc_driver_bin.o $(F90_POST_LINK)

mm52scipuf: for_scipuff_RT.o
	$(F90_PRE_LINK) -o $@ for_scipuff_RT.o $(F90_POST_LINK)

# Special case compiles. Do not use the LOC_FC_FLAGS for these files

cnvmm5e.o: cnvmm5e.f
	$(FC) $(SYS_FCFLAGS) -c $(LOC_INCLUDES) -o $@ cnvmm5e.f

cnvmm5e_bin.o: cnvmm5e_bin.f
	$(FC) $(SYS_FCFLAGS) -c $(LOC_INCLUDES) -o $@ cnvmm5e_bin.f

# --------- Dependencies ----------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.


###########################################################################
#
# Makefile for Modeling System (MS)
# RTFDDA/RT_DECODE_NSSL_MOSAIC
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

OBJS1	=	dbz_2_mm5grid.o mm5_utils.o utils.o
OBJS2	=	radar_for_nudge.o mm5_utils.o utils.o
EXEC1	=	dbz_2_mm5grid.exe
EXEC2	=	radar_for_nudge.exe
EXECS	=	$(EXEC1) $(EXEC2)
RM_LIST	=	$(EXECS)

# --------- Local compile/link definitions --------

LOC_INCLUDES	=	$(NET_INCS)
LOC_LDLIBS	=	$(LDLIBS) $(NET_LDLIBS)
LOC_LDFLAGS	=	$(NET_LDFLAGS) 
LOC_F90_LDFLAGS	=	$(LDFLAGS90)
LOC_F90_FLAGS	=	$(FFLAGS90)

# --------- Targets --------------

all:	$(EXECS)

install:	$(EXECS)
	$(CP) $(EXECS) $(DEST)

$(EXEC1):	$(OBJS1)
	$(F90_PRE_LINK) -o $@ $(OBJS1) $(F90_POST_LINK)

$(EXEC2):	$(OBJS2)
	$(F90_PRE_LINK) -o $@ $(OBJS1) $(F90_POST_LINK)

clean:
	$(RM) $(CLEAN_LIST) $(EXECS)

add2readme:
	for file in ${EXECS} ;\
	do \
		echo "$${file} -- source: `$(PWD)`, `$(DATE)`" >> $(DEST_SRC_README);\
	done


# --------- Dependencies ----------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.

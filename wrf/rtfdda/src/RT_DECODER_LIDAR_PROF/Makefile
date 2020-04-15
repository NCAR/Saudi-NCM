###########################################################################
#
# Makefile for Modeling System (MS)
# RTFDDA/RT_DECODER_LIDAR_PROF
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

OBJS	=	rd_lidar_prof.o
EXEC	=	rd_lidar_prof.exe 

# --------- Local compile/link definitions --------

LOC_LDFLAGS	= 
LOC_FC_FLAGS	= $(FFLAGS90F)
LOC_F90_LDFLAGS	= $(LDFLAGS90)

# --------- Targets --------------

all: $(EXEC)

install: $(EXEC)
	$(CP) $(EXEC) $(DEST)

$(EXEC): $(OBJS)
	$(F90_PRE_LINK) -o $@ $(OBJS) $(F90_POST_LINK)

clean:
	$(RM) $(CLEAN_LIST) $(EXEC)

add2readme:
	echo "$(EXEC) -- source: `$(PWD)`, `$(DATE)`" >> $(DEST_SRC_README)

# --------- Dependencies ----------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.


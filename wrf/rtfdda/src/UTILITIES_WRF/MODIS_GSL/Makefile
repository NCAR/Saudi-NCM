###########################################################################
#
# Makefile for Modeling System (MS)
# RTFDDA/src/WRF_UTILITIES
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

OBJS	=	add_GSL_WST_WPS.o GSL_WST_funcs_subrs.o
EXEC	=	add_GSL_WST_WPS.exe
MOD	=	funcs_subrs.mod

# --------- Local compile/link definitions -----

LOC_F90_FLAGS	=	$(FFLAGS_BYTESWAP)
LOC_F90_LDFLAGS	=	

# --------- Targets --------------

all: $(MOD) $(EXEC)

install: $(EXEC)
	$(CP) $(EXEC) $(DEST)

$(EXEC): $(OBJS)
	$(F90_PRE_LINK) -o $@ $(OBJS)

funcs_subrs.mod: GSL_WST_funcs_subrs.o

clean:
	$(RM) $(CLEAN_LIST) $(EXEC)

add2readme:
	echo "$(EXEC) -- source: `$(PWD)`, `$(DATE)`" >> $(DEST_SRC_README)

# --------- Dependencies ----------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.


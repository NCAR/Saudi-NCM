###########################################################################
#
# Makefile for Modeling System (MS)
# RTFDDA/src/RT_DECODER_MODIS
#
# Dependencies:
#      (1) You must have the MS_MAKE_MACROS_DIR environment variable defined
#          and it must point to the directory of Modeling System Makefile
#          include files.
#      (2) You must have the MS_OS_TYPE environment variable defined and it
#          must be one of the types in:
#          $MS_MAKE_MACROS_DIR/compiler_macros.<ms_os_type>.
#
# May 2012
#
###########################################################################

# ---------- Definitions -----------
#
# Include the standard shared make info (make_defns) and the
# operating system specific info (compiler_macros.$(MS_OS_TYPE))

include $(MS_MAKE_MACROS_DIR)/make_defns
include $(MS_MAKE_MACROS_DIR)/compiler_macros.$(MS_OS_TYPE)

# --------- Object files ----------

OBJS	=	sst_conv.o
EXEC	=	modis_sst.exe

# --------- Local compile/link definitions -----

NET		=	$(NETCDF)
#LOC_INCLUDES	=	$(NETCDF)/include
#LOC_F90_FLAGS	=	$(FFLAGS_BYTESWAP) $(FFLAGS_FREE)
#LOC_F90_LDFLAGS	=	$(NET_LDFLAGS)
#LOC_F90_FLAGS   =       $(FFLAGS90) $(NET_LDFLAGS) $(NET_LDLIBS) $(NET_INCS)
LOC_F90_FLAGS   =       -o $@ $(FFLAGS90) $(NET_LDFLAGS) $(NET_LDLIBS) $(NET_INCS)
F90_FLAGS   =       $(FFLAGS90) $(NET_LDFLAGS) $(NET_LDLIBS) $(NET_INCS)
#LDFLAGS90   =       $(FFLAGS90) $(NET_LDFLAGS) $(NET_LDLIBS) $(NET_INCS)
LDFLAGS90   =       -Wl,-rpath,$(NET)/lib $(NET_LDFLAGS) $(NET_LDLIBS)


# --------- Targets --------------

all: $(EXEC)

install: $(EXEC)
	$(CP) $(EXEC) $(DEST)

$(EXEC): $(OBJS)
	$(F90_PRE_LINK) -o $@ $(LDFLAGS90) $(F90_POST_LINK) $(OBJS)

clean:
	$(RM) $(CLEAN_LIST) $(EXEC)

add2readme:
	echo "$(EXEC) -- source: `$(PWD)`, `$(DATE)`" >> $(DEST_SRC_README)

# --------- Dependencies ----------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.


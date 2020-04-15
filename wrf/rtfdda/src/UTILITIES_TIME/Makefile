###########################################################################
#
# Makefile for Modeling System (MS)
# RTFDDA/UTILITIES
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

EXEC=	advance_cymdh.exe \
	advance_cymdh \
	geth_newdate.exe \
	geth_idts.exe

MOD=

# --------- Local compile/link definitions --------

LOC_INCLUDES	=	
LOC_LDFLAGS     =       -Wl,-rpath,$(LD_LIBRARY_PATH) -limf -lm
LOC_FC_FLAGS	=	$(FFLAGS90)
LOC_F90_FLAGS	=	$(FFLAGS90)
LOC_F90_LDFLAGS	=	$(LDFLAGS90)

# --------- Targets --------------

all: $(MOD) $(EXEC)

install: $(MOD) $(EXEC)
	$(CP) $(EXEC) $(DEST)

clean:
	$(RM) $(CLEAN_LIST) $(EXEC) 

$(MOD):
	$(LN) $(INSTALL_LIB_DIR)/$@ $@

add2readme:
	for exe in ${EXEC} ;\
	do \
		echo $${exe} -- source: `$(PWD)`, `$(DATE)`>> $(DEST_SRC_README);\
	done

advance_cymdh.exe: advance_cymdh.o
	$(F90_PRE_LINK) -o $@ $?

advance_cymdh: advance_cymdh.o
	$(F90_PRE_LINK) -o $@ $?

geth_newdate.exe: geth_newdate_ccyymmdd.o geth_newdate.o nfeb.o
	$(F90_PRE_LINK) -o $@ $?

geth_idts.exe: geth_idts_main.o geth_idts.o nfeb.o
	$(F90_PRE_LINK) -o $@ $?

# Additional targets

diff_cymdh.exe: diff_cymdh.o
	$(FC_PRE_LINK) -o $@ $?

# --------- Dependencies ----------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.

###########################################################################
#
# Makefile for Modeling System (MS)
# Top level Makefile for RTFDDA/RT_DECODE_NSSL_MOSAIC
#
# Dependencies:
#      (1) You must have the MS_MAKE_MACROS_DIR environment variable defined
#          and it must point to the directory of Modeling System Makefile
#          include files.
#
# Mar 2007
#
###########################################################################

# --------- Definitions ----------
#
# Include the standard shared make info (make_defns)

include $(MS_MAKE_MACROS_DIR)/make_defns

# Local definitions

TARGETS=all install diff clean add2readme

SUB_DIRS = \
	RT_DBZ2QR \
	RT_MERGE_TILE \
	interp_ncepMRMS

# ---------------------------------------
# Recursive targets for subdirs

$(TARGETS): FORCE
	$(MAKE) $(SUB_DIRS) TARGET=$@

$(SUB_DIRS): FORCE
	if $(TEST) -d $@; then cd $@; $(MAKE) $(TARGET); fi ;

FORCE:

# -----------------------------------------------------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.


###########################################################################
#
# Makefile for Modeling System (MS)
# Top level Makefile for RTFDDA/RT_DECODER_PWIDS
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
include $(MS_MAKE_MACROS_DIR)/compiler_macros.$(MS_OS_TYPE)

# Local definitions

TARGETS=all install diff clean add2readme

SUB_DIRS = \
	constant_files \
	src

# ---------------------------------------
# Recursive targets for subdirs

$(TARGETS): FORCE
	$(MAKE) $(SUB_DIRS) TARGET=$@

$(SUB_DIRS): FORCE
	if $(TEST) -d $@; then cd $@; $(MAKE) $(TARGET); fi ;

FORCE:

# special for this Makefile

tar:
		( cd .. ; tar cvf RT_DECODER_PWIDS.tar RT_DECODER_PWIDS; );
		( mv ../RT_DECODER_PWIDS.tar .);

# -----------------------------------------------------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.


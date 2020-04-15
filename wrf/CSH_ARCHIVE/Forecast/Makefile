###########################################################################
#
# Makefile for Modeling System (MS)
# Makefile for RTFDDA/scripts/Forecast
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
# Currently all the scripts in this directory are copied to the
# TARGET_DIR.

TARGET_DIR=$(CSH_DEST)/Forecast

# ------------ Targets --------------

all:		
	echo "Scripts dir. No compile needed."

clean:
	$(RM) $(CLEAN_LIST)

install:	dir
	files=`ls` ;\
	for file in $${files} ; do \
		do_copy=TRUE ;\
		if [ -d $${file} ] ; then \
			do_copy=FALSE ;\
		fi ;\
		if [ $${file} = Makefile ] ; then \
			do_copy=FALSE ;\
		fi ;\
		if [ $${do_copy} = TRUE ] ; then \
			$(CP) $${file} $(TARGET_DIR) ;\
		fi ;\
	done

add2readme:	dir
	for file in $(SCRIPTS) ; do \
		echo "$${file} -- source: `$(PWD)`, `$(DATE)`" >> $(TARGET_DIR)/$(SRC_README_FNAME) ;\
	done

dir:
	if test -d $(TARGET_DIR) ;\
	then \
		: ;\
	else \
		mkdir -p $(TARGET_DIR) ;\
	fi

# -----------------------------------------------------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.

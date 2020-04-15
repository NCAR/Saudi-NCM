###########################################################################
#
# Makefile for Modeling System (MS)
# Makefile for apps/4dwx/RTFDDA/scripts/python
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

TARGET_DIR=$(MM5HOME)/cycle_code/PYTHON

# ------------ Targets --------------

all:		
	echo "Python scripts dir. No compile needed."

clean:
	$(RM) $(CLEAN_LIST)

install:	dir
	files=`ls` ;\
	for file in $${files} ; do \
		if [ -d $${file} ] ; then \
			$(CP) -r $${file} $(TARGET_DIR) ;\
		else \
			if ! ( echo $${file} | grep -i Makefile ) ; then \
				$(CP) $${file} $(TARGET_DIR) ;\
			fi ;\
		fi ;\
	done

add2readme:	dir
	files=`ls` ;\
	for file in $${files} ; do \
		if [ -d $${file} ] ; then \
			echo "$${file} (dir) -- source: `$(PWD)`, `$(DATE)`" >> $(TARGET_DIR)/$(SRC_README_FNAME) ;\
		else \
			echo "$${file} -- source: `$(PWD)`, `$(DATE)`" >> $(TARGET_DIR)/$(SRC_README_FNAME) ;\
		fi ;\
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

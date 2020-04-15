###########################################################################
#
# Makefile for Modeling System (MS)
# Makefile for RTFDDA/scripts/postprocs
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

# All the scripts in the current directory are copied to TARGET_DIR,
# Plus one specific subdirectory

TARGETS=all clean install add2readme

TARGET_DIR=$(MM5HOME)/cycle_code/POSTPROCS

WANTED_SUBDIRS=NCL gbc AnEn

# ------------ Targets --------------

all:		
	echo "Scripts dir. No compile needed."

clean: FORCE
	$(RM) $(CLEAN_LIST)

install: dir FORCE
	files=`ls` ;\
	for file in $${files} ; do \
		if [ -d $${file} ] ; then \
			for wdir in $(WANTED_SUBDIRS) ; do \
				if [ $${file} = $${wdir} ] ; then \
					$(CP) -r $${file} $(TARGET_DIR) ;\
				fi ;\
			done ;\
		else \
			$(CP) $${file} $(TARGET_DIR) ;\
		fi ;\
	done

add2readme:	dir
	files=`ls` ;\
	for file in $${files} ; do \
		if [ -d $${file} ] ; then \
			for wdir in $(WANTED_SUBDIRS) ; do \
				if [ $${file} = $${wdir} ] ; then \
					echo "$${file} (dir) -- source: `$(PWD)`, `$(DATE)`" >> $(TARGET_DIR)/$(SRC_README_FNAME) ;\
				fi ;\
			done ;\
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

FORCE:

# -----------------------------------------------------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.

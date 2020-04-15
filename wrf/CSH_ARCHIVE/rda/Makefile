###########################################################################
#
# Makefile for Modeling System (MS)
# Makefile for RTFDDA/scripts/util
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

# All scripts in this directory are copied to the TARGET_DIR, except
# those listed in BIN_DIR_ONLY_FILES which are copied to the TARGET_BIN_DIR
#
# Important: If you want to install all files in this directory to
#            a target directory other than TARGET_DIR (e.g., for nnt-gw), do the 
#            following: pass in a new TARGET_DIR on the command line and
#            set BIN_DIR_ONLY_FILES on the command line to blank. This is
#            done in the ../_Makefile.nnt_gw.

TARGET_DIR=$(CSH_DEST)/rda
TARGET_BIN_DIR=$(HOME_BIN)

BIN_DIR_ONLY_FILES=nps

# ------------ Targets --------------

all:		
	echo "Scripts dir. No compile needed."

clean:
	$(RM) $(CLEAN_LIST)

install: dir
	files=`ls -1 | grep -v akefile | grep -v CVS`;\
	for file in $${files} ; do \
		dest_is_target_dir=TRUE ;\
		if [ -d $${file} ] ; then \
			dest_is_target_dir=FALSE ;\
		fi ;\
		for binfile in $(BIN_DIR_ONLY_FILES) ; do \
			if [ $${file} = $${binfile} ] ; then \
				dest_is_target_dir=FALSE ;\
				$(CP) $${file} $(TARGET_BIN_DIR) ;\
			fi ;\
		done ;\
		if [ $${dest_is_target_dir} = TRUE ] ; then \
			$(CP) $${file} $(TARGET_DIR) ;\
		fi ;\
	done

add2readme: dir
	files=`ls -1 | grep -v akefile | grep -v CVS` ;\
	for file in $${files} ; do \
		dest_is_target_dir=TRUE ;\
		if [ -d $${file} ] ; then \
			dest_is_target_dir=FALSE ;\
		fi ;\
		for binfile in $(BIN_DIR_ONLY_FILES) ; do \
			if [ $${file} = $${binfile} ] ; then \
				dest_is_target_dir=FALSE ;\
				echo "$${file} -- source: `$(PWD)`, `$(DATE)`" >> $(TARGET_BIN_DIR)/$(SRC_README_FNAME) ;\
			fi ;\
		done ;\
		if [ $${dest_is_target_dir} = TRUE ] ; then \
			echo "$${file} -- source: `$(PWD)`, `$(DATE)`" >> $(TARGET_DIR)/$(SRC_README_FNAME) ;\
		fi ;\
	done

dir:
	if test -d $(TARGET_DIR) ;\
	then \
		: ;\
	else \
		mkdir -p $(TARGET_DIR) ;\
	fi ;\
	if test -d $(TARGET_BIN_DIR) ;\
	then \
		: ;\
	else \
		mkdir -p $(TARGET_BIN_DIR) ;\
	fi

# -----------------------------------------------------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.

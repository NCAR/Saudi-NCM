###########################################################################
#
# Makefile for Modeling System (MS)
# Makefile for RT_DECODER_PWIDS/constant_files
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
# Include the standard shared make info (make_defns) and the
# operating system specific info (compiler_macros.$(MS_OS_TYPE))

include $(MS_MAKE_MACROS_DIR)/make_defns

# Local definitions

# --------- Object files ----------

CONSTANT_FILES = Instruments.txt

TARGET_DIR=$(DEST)/../CONSTANT_FILES

# ------------ Targets --------------

all:		
		echo "Constant_files dir. No compile needed."

install:	dir
		$(CP) $(CONSTANT_FILES) $(TARGET_DIR)

diff:
		for file in ${CONSTANT_FILES} ;\
		do \
			diff $${file} $(TARGET_DIR)/ ;\
		done

clean:
		$(RM) $(CLEAN_LIST)

add2readme: dir
	for file in ${CONSTANT_FILES} ;\
	do \
		echo $${file} -- source: `$(PWD)`, `$(DATE)`>> $(TARGET_DIR)/$(SRC_README_FNAME);\
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


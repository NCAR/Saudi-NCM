###########################################################################
#
# Makefile for Modeling System (MS)
# Makefile for RT_DECODER_PWIDS/src
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

# --------- Definitions ----------
#
# Include the standard shared make info (make_defns) and the
# operating system specific info (compiler_macros.$(MS_OS_TYPE))

include $(MS_MAKE_MACROS_DIR)/make_defns
include $(MS_MAKE_MACROS_DIR)/compiler_macros.$(MS_OS_TYPE)


# --------- Object files ----------
#
# Object files that make up the TARGET_FILE

OBJS =			openfiles.o fill_decoded.o read_instruments.o rd_pwids.o
OBJCS =			clean_string.o

MODS =			module_date.o module_decoded.o \
			module_pwids.o

TARGET_FILE	= 	rd_pwids.exe
RM_LIST 	=	$(TARGET_FILE)

# --------- Local compile/link definitions --------
# Redefine CC to use the PGI compiler

LOC_LDFLAGS	=	
LOC_LDLIBS	=	
LOC_FC_FLAGS	=	$(FCFLAGS)
LOC_F90_FLAGS	=	$(FCFLAGS)
LOC_CC_FLAGS	=
LOC_FC_LDFLAGS	=

# ------------ Targets --------------

all:		$(MODS) $(TARGET_FILE)

install:	$(TARGET_FILE)
		$(CP) $(TARGET_FILE) $(DEST)/

diff:
		$(CMP) $(TARGET_FILE) $(DEST)/

clean:
		$(RM) $(RM_LIST) $(CLEAN_LIST)

$(TARGET_FILE):	$(MODS) $(OBJS) $(OBJCS)
		$(RM) $@
		$(FC_PRE_LINK) -o $@ $(OBJS) $(MODS) $(OBJCS) $(FC_POST_LINK)

add2readme:
	echo "$(TARGET_FILE) -- source: `$(PWD)`, `$(DATE)`" >> $(DEST_SRC_README)


# --------- Dependencies ----------

rd_pwids.o:		rd_pwids.f90 \
			module_date.o module_decoded.o \
			module_pwids.o \
			openfiles.o fill_decoded.o

openfiles.o:		openfiles.f90

fill_decoded.o:		fill_decoded.f90 \
			module_date.o module_pwids.o \
			module_decoded.o

read_instruments.o:	read_instruments.f90 \
			module_pwids.o \
			module_date.o \
			clean_string.o

module_date.o:		module_date.f90

module_decoded.o:	module_decoded.f90

module_pwids.o:         module_pwids.f90 \
			module_date.o

clean_string.o:         clean_string.c

# -----------------------------------------------------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.

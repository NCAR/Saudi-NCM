# $Date: 2007/10/19 21:14:46 $ $Revision: 1.1.1.2 $
#------------------------------------------------------------------------------
#  Make rules for building an application program.
#
#  This file is intended for use in Makefile via the include directive, e.g.
#
#      include $(BUILD_DIR)/application_rules.mk
#
#  It is assumed that the environment has been set by sourcing the build
#  resource file (buildrc).
# 
#  Copyright (C) 2001, WSI Corporation
#------------------------------------------------------------------------------
#
#  For portability, use the Bourne shell within Makefiles. 
#  There have been problems using the C-shell under Linux.
#
SHELL=/bin/sh

#
#  RULES for building a single utility program.
#        The program will be named $(EXE_NAME).
#        The objects files used to create the utility should be $(OBJS).  
#        The libraries that should be linked should be $(DEP_LIBS).
# 
all: exe config

exe: $(OBJS) 
	@echo "    Building utility program $(EXE_NAME)" ; \
	$(LDD) $(DEBUG) $(OPTIMIZE) -o $(EXE_NAME) $(OBJS) $(DEP_LIBS) ;\
	mv -f $(EXE_NAME) $(BIN_DEST) ;

#
#  Include the RULES for compilation and installation of config files
#
include $(BUILD_DIR)/compile_rules.mk
include $(BUILD_DIR)/config_rules.mk

#
#  RULE for building a library
#
#  For exe modules, these do nothing, but we define one so that make lib 
#  can be passed down to all source directories.
#
lib:
	@echo "make lib does nothing for utility modules"

#
#  RULES for cleaning up derived files.
#
#  'clean' removes all objects produced by this file, as well as other 
#      extraneous artifacts of compiling and building applications.
# 
#      A subsequent make will both recompile the source code and recreate
#      the executable.  clean also removes files core files and other
#      auxilliary files created during compilation.
#
#  'clean_exe' removes the utility program.
#
clean: 
	@/bin/rm -f *.o core* so_locations Makefile.bak *~ #*#
	@/bin/rm -fr ii_files

clean_exe:
	@/bin/rm -f $(BIN_DEST)/$(EXE_NAME)

clean_lib:
	@echo "       make clean_lib does nothing for utility modules"

#
#  RULES for creating the include dependencies.
#
include $(BUILD_DIR)/depend_rules.mk

clean_depend: generic_clean_depend

depend: generic_depend

#==============================================================================
#
# Makefile for JCSDA CRTM Build
#
# $Id: makefile,v 1.1 2017/06/30 19:53:22 sheu Exp $
#
#==============================================================================

include ../make.macros
include make.filelist


# Library build
all library: $(LIBRARY)

$(LIBRARY): $(OBJ_FILES)
	$(ARCHIVER) $(ARCHIVER_FLAGS) $@ $(OBJ_FILES)


# Library cleanup
clean:
	$(REMOVE) $(OBJ_FILES) *.$(EXT_MOD) $(LIBRARY)


include make.dependencies
include ../make.rules

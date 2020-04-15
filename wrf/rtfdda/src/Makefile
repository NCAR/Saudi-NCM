#
# Top-level Makefile for RTFDDA/src subdirectories for builds
# of the Modeling System. This is specific to WRF version 3.8.1
#
# Nov 2018
#===============================================================================
#
# --------- Definitions ----------
#
# Include the standard shared make info (make_defns)  and the
# operating system specific info (compiler_macros.$(MS_OS_TYPE))

include $(MS_MAKE_MACROS_DIR)/make_defns
include $(MS_MAKE_MACROS_DIR)/compiler_macros.$(MS_OS_TYPE)

# Local definitions and info:
# - The TARGETS macro is not actually used, it is here for reference.
# - The all, clean, install, add2readme targets do not build WRF or WPS.
# - For WRF and WPS builds, use the wrf_only target.
# - MM5 is not built using this Makefile
# - The RIP_USE_MS_OS_TYPE is separate so can handle linux64 target when
#   other subdirs need linux. Override on the make command line to use
#   a different target for RT_PLOT: e.g., 'make all RIP_USE_MS_OS_TYPE=linux64'
# - The ADDL_MAKEFILE_NAME is specific to the WRF and WPS compiles to
#   wrap the WRF and WPS compiles into a Makefile

USE_MS_OS_TYPE=$(MS_OS_TYPE)
RIP_USE_MS_OS_TYPE=$(USE_MS_OS_TYPE)
TARGETS=all clean install add2readme
ADDL_MAKEFILE_NAME=_Makefile.auto_build

SUB_DIRS = \
	GTS_DECODER \
	QCtoNC \
	RT_DECODER_CLASS \
	RT_DECODER_LIDAR_PROF \
	RT_DECODER_MEDOC \
	RT_DECODER_MODIS \
	RT_DECODER_PROF_BLP \
	RT_DECODER_PROF_NPN \
	RT_DECODER_PROF_RANGE \
	RT_DECODER_RAWS \
	RT_DECODER_SAMS_NEW \
	RT_TOSCIPUFF \
	RT_VERIFY \
	RT_VERIFY_WRF \
	RT_WRF2ASCII \
	UTILITIES_TIME \
	UTILITIES_WRF
	#RADAR_TO_WRFFDDA \
	#RT_DECODE_NSSL_MOSAIC \
	#RT_DECODER_PWIDS \

#$(SUB_DIRS):
#	$(MAKE) -C $@
test: 
	@for i in $(SUB_DIRS); do \
	echo "make all in $$i..."; \
	$(MAKE) -C $$i; \
	done

all:
	$(MAKE) $(SUB_DIRS) TARGET=all

clean add2readme: FORCE
	$(MAKE) $(SUB_DIRS) TARGET=clean

install: all FORCE
	$(MAKE) $(SUB_DIRS) TARGET=$@

wrf_only: FORCE
	$(MAKE) $(ADDL_SUB_DIRS_REQUIRE_ADDL_MAKEFILE) TARGET=clean
	$(MAKE) $(ADDL_SUB_DIRS_REQUIRE_ADDL_MAKEFILE) TARGET=install
	$(MAKE) $(ADDL_SUB_DIRS_REQUIRE_ADDL_MAKEFILE) TARGET=add2readme

$(SUB_DIRS): FORCE
	if $(TEST) -d $@; then cd $@; $(MAKE) $(TARGET); fi;
#	if $(TEST) -d $@; then cd $@; $(MAKE) -C $(TARGET); fi;

FORCE:

# -----------------------------------------------------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.


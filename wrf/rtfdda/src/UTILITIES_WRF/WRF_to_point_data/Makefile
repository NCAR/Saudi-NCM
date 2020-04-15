###########################################################################
#
# Makefile for Modeling System (MS)
# apps/netcdf_utils/src/WRF_to_point_data
#
# Dependencies:
#      (1) You must have the MS_MAKE_MACROS_DIR environment variable defined
#          and it must point to the directory of Modeling System Makefile
#          include files.
#      (2) You must have the MS_OS_TYPE environment variable defined and it
#          must be one of the types in:
#          $MS_MAKE_MACROS_DIR/compiler_macros.<ms_os_type>.
#
# Jun 2008
#
###########################################################################

# ---------- Definitions -----------
#
# Include the standard shared make info (make_defns) and the
# operating system specific info (compiler_macros.$(MS_OS_TYPE))

include $(MS_MAKE_MACROS_DIR)/make_defns
include $(MS_MAKE_MACROS_DIR)/compiler_macros.$(MS_OS_TYPE)

# -------- Namelist files --------
# The namelist file needs to be installed for RTFDDA into
# cycle_code/POSTPROCS

NAMELIST_FILES=site_fields.nml

# --------- Object files ----------
#
# Warning: the MODS list is in explicit dependency order.

OBJS =  WRF_kinds.o \
	WRF_ncread.o \
	WRF_tools.o \
	WRF_site.o \
	WRF_cmdLine.o \
	WRF_sounding.o \
	WRF_constants.o \
	WRF_ptData.o \
	WRF_input.o \
	WRF_main.o \
        derived_fields.o \
        site_arrays.o \
        time_routines.o \
        write_headers.o

OBJS2 = WRF_latlon.o \
	WRF_kinds.o \
	WRF_ncread.o

OBJS3 = WRF_lltoxy.o \
	WRF_kinds.o \
	WRF_ncread.o \
	WRF_cmdLine.o \
	WRF_input.o  \
	WRF_tools.o \
	WRF_constants.o

MODS = wrf_kinds.mod \
	wrf_constants.mod \
	wrf_ncread.mod \
	wrf_tools.mod \
	wrf_input.mod \
	wrf_site.mod \
	wrf_sounding.mod \
	wrf_ptData.mod \
	wrf_cmdLine.mod \
        derived_fields.mod \
        site_arrays.mod \
        time_routines.mod \
        write_headers.mod 

EXECS	= readWRF.exe WRF_lltoxy.exe WRF_latlon.exe

# --------- Local compile/link definitions -----

NET		=	$(NETCDF)

LOC_INCLUDES	=	$(NET_INCS)
LOC_LDLIBS	=	$(LDLIBS) $(NET_LDLIBS)
LOC_LDFLAGS	=	-Wl,-rpath,$(LD_LIBRARY_PATH):$(NET)/lib $(NET_LDFLAGS) -limf
LOC_F90_LDFLAGS	=	$(LDFLAGS90)
LOC_F90_FLAGS	=	$(FFLAGS90)
LOC_FC_FLAGS	=	$(FFLAGS90F)

# --------- Targets --------------

all: clean $(EXECS)

install: all
	$(CP) $(EXECS) $(DEST)
	$(CP) $(NAMELIST_FILES) $(DEST)/../POSTPROCS/

readWRF.exe: $(MODS) $(OBJS)
	$(F90_PRE_LINK) -o $@ $(OBJS) $(F90_POST_LINK)

WRF_latlon.exe: $(MODS) $(OBJS2)
	$(F90_PRE_LINK) -o $@ $(OBJS2) $(F90_POST_LINK)

WRF_lltoxy.exe: $(MODS) $(OBJS3)
	$(F90_PRE_LINK) -o $@ $(OBJS3) $(F90_POST_LINK)

WRF_site.o: WRF_site.f90 derived_fields.mod site_arrays.mod
	$(F90_PRE_LINK) -c $<

time_routines.o: time_routines.f90
	$(F90_PRE_LINK) -c $<

write_headers.o: write_headers.f90 time_routines.mod wrf_tools.mod
	$(F90_PRE_LINK) -c $<

WRF_sounding.o: WRF_sounding.f90 derived_fields.mod write_headers.mod
	$(F90_PRE_LINK) -c $<

clean:
	$(RM) $(CLEAN_LIST) $(EXECS)

add2readme:
	for exec in $(EXECS) ; do \
		echo "$(exec) -- source: `$(PWD)`, `$(DATE)`" >> $(DEST_SRC_README) ;\
	done

# --------- Dependencies ----------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.
wrf_kinds.mod: WRF_kinds.o
wrf_cmdLine.mod: WRF_cmdLine.o
wrf_input.mod: WRF_input.o
wrf_constants.mod: WRF_constants.o
wrf_tools.mod: WRF_tools.o
wrf_site.mod: WRF_site.o
wrf_sounding.mod: WRF_sounding.o
wrf_ptData.mod: WRF_ptData.o
wrf_ncread.mod: WRF_ncread.o
time_routines.mod: time_routines.o
write_headers.mod: write_headers.o

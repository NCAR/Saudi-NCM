###########################################################################
#
# Makefile for Modeling System (MS)
# RTFDDA/src/RT_VERIFY_WRF
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

# ---------- Definitions -----------
#
# Include the standard shared make info (make_defns) and the
# operating system specific info (compiler_macros.$(MS_OS_TYPE))

include $(MS_MAKE_MACROS_DIR)/make_defns
include $(MS_MAKE_MACROS_DIR)/compiler_macros.$(MS_OS_TYPE)

# --------- Object files ----------

EXECS	=	v_wrf_sfc_interp.exe \
		v_wrf_snd_pairs.exe \
		v_wrf_read_snd_pairs.exe \
		v_wrf_sfc_interp_model_only.exe

OBJ = 	wrf_arrays.o \
	wrf_tools.o \
	wrf_kinds.o \
	wrf_constants.o \
	wrf_ncread.o \
        write_snd_bin.o

NETCDF_MOD =	netcdf.mod \
		typesizes.mod

MOD =	wrf_arrays.mod \
	wrf_kinds.mod \
	wrf_constants.mod \
	wrf_ncread.mod \
	wrf_tools.mod \
	write_snd_bin.mod \
        datetime_module.mod

# --------- Local compile/link definitions -----

NET             =       $(NETCDF)

LOC_INCLUDES	=	$(NET_INCS)
LOC_LDLIBS	=	$(NET_LDLIBS)
LOC_LDFLAGS	=	$(NET_LDFLAGS)
LOC_F90_LDFLAGS	=	-Wl,-rpath,$(NETCDF)/lib
LOC_F90_FLAGS	=	-assume byterecl

# --------- Targets --------------

all: $(NETCDF_MOD) $(MOD) $(EXECS)

install: all
	$(CP) $(EXECS) $(DEST)

v_wrf_sfc_interp.exe: $(OBJ) wrf_sfc_interp.o qfromttd.o right_time.o qfromrh.o
	$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)

v_wrf_snd_pairs.exe: $(OBJ) wrf_snd_pairs.o eta2p.o vertical_interp.o \
                     nearest_hour.o right_time.o qfromttd.o wrf_pseudo_snd.o \
                     write_snd_bin.o wrf_pseudo_2d.o qfromrh.o
	$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)

v_wrf_read_snd_pairs.exe: wrf_read_snd_pairs.o vertical_interp.o
	$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)

v_wrf_sfc_interp_model_only.exe: $(OBJ) wrf_sfc_interp_model_only.o \
                                 qfromttd.o right_time.o qfromrh.o \
                                 datetime_module.o
	$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)

$(NETCDF_MOD):
	$(LN) $(NET)/include/$@ $@

$(MOD): $(NETCDF_MOD)

clean:
	$(RM) $(CLEAN_LIST) $(EXEC) $(OBJ) *.mod

add2readme:
	for exe in ${EXECS} ;\
	do \
		echo $${exe} -- source: `$(PWD)`, `$(DATE)`>> $(DEST_SRC_README);\
	done

# --------- Dependencies ----------

# DO NOT DELETE THIS LINE -- make depend depends on it.


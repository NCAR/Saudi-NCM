###########################################################################
#
# Makefile for Modeling System (MS)
# Makefile for RTFDDA/src/RT_VERIFY
#
# Dependencies:
#      (1) You must have the MS_MAKE_MACROS_DIR environment variable defined
#          and it must point to the directory of Modeling System Makefile
#          include files.
#      (2) You must have the MS_OS_TYPE environment variable defined and it
#          must be one of the types in:
#          $MS_MAKE_MACROS_DIR/compiler_macros.<ms_os_type>.
#
# Jun 2007
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
# Object files that make up the EXECs

MODULES = header_types.o meas_types.o level_record.o mm5_header.o \
	level_record1.o

EXEC=v_read_pairs.exe \
	v_mm5_sfc.exe \
	v_sfc_pairs_4rip.exe \
	v_merge_pairs.exe \
	v_rewrite_obs.exe \
	v_snd_pairs.exe \
	v_snd_pairs_4rip.exe \
	v_rewrite_snd.exe \
	v_rewrite_snd1.exe \
	v_merge_snd_pairs.exe \
	v_read_snd_pairs.exe \
	v_qcobs_from_sfcsta.exe \
	v_cress_sfc_interp.exe \
	v_sfc_pair_bias_remove.exe

RM_LIST 	=	$(EXEC)

# --------- Local compile/link definitions --------

LOC_LDFLAGS	= 
LOC_LDLIBS	=	
LOC_FC_FLAGS	= $(FFLAGS90) -assume byterecl
LOC_F90_FLAGS	= $(FFLAGS90) -assume byterecl

# ------------ Targets --------------

all:		$(MODULES) $(EXEC)

install:	all
		cp $(EXEC) $(DEST)/

clean:
		$(RM) $(RM_LIST) $(CLEAN_LIST)

add2readme: 
	for exe in ${EXEC} ;\
	do \
		echo $${exe} -- source: `$(PWD)`, `$(DATE)`>> $(DEST_SRC_README);\
	done

v_read_pairs.exe:: read_pairs.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $? 

v_mm5_sfc.exe:: mm5_sfc_interp_v3.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $? 

v_sfc_pairs_4rip.exe:: sfc_pairs_4rip.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $? 

v_merge_pairs.exe:: merge_pairs.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?

v_rewrite_obs.exe:: rewrite_obs.o header_types.o meas_types.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?

v_snd_pairs.exe:: snd_pairs.o mm5_header.o pseudo_snd.o \
                  qfromttd.o sig2p.o vertical_interp.o \
                  nearest_hour.o write_snd_pairs1.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?

v_snd_pairs_4rip.exe:: snd_pairs_4rip.o mm5_header.o pseudo_snd.o \
                  qfromttd.o sig2p.o vertical_interp.o \
                  nearest_hour.o write_snd_pairs1.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?

v_rewrite_snd.exe:: rewrite_snd.o level_record.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?

v_rewrite_snd1.exe:: rewrite_snd1.o level_record1.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?

v_merge_snd_pairs.exe:: merge_snd_pairs.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?

v_read_snd_pairs.exe:: read_snd_pairs.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?

v_qcobs_from_sfcsta.exe:: qcobs_from_sfcsta.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) -pc 32 -Wl,-Bstatic -o $@  $?

v_cress_sfc_interp.exe:: cress_sfc_interp.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?

v_sfc_pair_bias_remove.exe:: sfc_pair_bias_remove.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?


mm52modis.exe::	mm5_header.o pseudo_snd.o \
                qfromttd.o sig2modis.o vertical_interp.o \
                nearest_hour.o write_decoded.o \
                mm52modis.o
		$(RM) $@
		$(F90_PRE_LINK) -o $@ $? $(F90_POST_LINK)
##	$(F90) $(LDFLAGS90) -o $@  $?

# --------- Dependencies ----------

# -----------------------------------------------------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.

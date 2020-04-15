###########################################################################
#
# Makefile for Modeling System (MS)
# RTFDDA/src/GTS_DECODER
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

OBJ =   code_tables_new.o code_tables.o decode_bulletin_header.o gts_fm_num.o \
        identify_code_form.o interpret_bulletin.o read_a_bulletin.o \
	nan_arithmetic.o \
        decode_fm12.o wr_rawins_fm12.o wr_rap_fm12.o \
        decode_fm15.o wr_rap_fm15.o\
        decode_fm32.o \
        decode_fm35.o wr_rawins_fm35.o wr_rap_fm35.o \
        decode_fm86.o wr_rawins_fm86.o \
        decode_fm88.o wr_rawins_fm88.o wr_rap_fm88.o \
        decode_airep.o wr_rap_airep.o \
	util_tested.o

SRC =	$(OBJ:.o:.f90)

MOD =   bulletin.module \
	nan_arithmetic.module \
	record.module \
	mm5obs.module

INC =   inc.traceflow \
	inc.special_symbols \
        inc.bulletin \
	inc.formats \
        $(MOD)

EXEC = gtsdecoder.exe

RM_LIST=$(EXEC) UpdateGTS UpdateICAO sid_icao.exe sid_wmo.exe \
	write_stnid_into_bn.exe

# --------- Local compile/link definitions -----

LOC_INCLUDES	= 
LOC_LDFLAGS     =       -Wl,-rpath,$(LD_LIBRARY_PATH) -limf -lm
LOC_F90_FLAGS	= $(FFLAGS90) -r8 -assume byterecl
LOC_LDFLAGS	= 
LOC_F90_LDFLAGS	= $(LDFLAGS90) -r8

# --------- Targets --------------

all: $(EXEC) UpdateGTS UpdateICAO sid_icao.exe sid_wmo.exe write_stnid_into_bn.exe

install: $(EXEC)
	$(CP) $(EXEC) $(DEST)/

clean:
	$(RM) $(CLEAN_LIST) $(RM_LIST)

add2readme:
	for exe in ${EXEC} ;\
	do \
		echo "$${exe} -- source: `$(PWD)`, `$(DATE)`" >> $(DEST_SRC_README);\
	done

$(EXEC): $(OBJ) $(INC) main.o
	$(F90_PRE_LINK) main.o -o $@ $(OBJ) $(F90_POST_LINK)

UpdateGTS: UpdateGTS.o
	$(F90_PRE_LINK) UpdateGTS.o -o $@ $(F90_POST_LINK)

UpdateICAO: UpdateICAO.o
	$(F90_PRE_LINK) UpdateICAO.o -o $@ $(F90_POST_LINK)

sid_icao.exe: sid_icao.o
	$(F90_PRE_LINK) sid_icao.o -o $@ $(F90_POST_LINK)

sid_wmo.exe: sid_wmo.o
	$(F90_PRE_LINK) sid_wmo.o -o $@ $(F90_POST_LINK)

write_stnid_into_bn.exe: write_stnid_into_bn.o
	$(F90_PRE_LINK) write_stnid_into_bn.o -o $@ $(F90_POST_LINK)

# ------ Unusual target here, copied from Makefile --------

alpha_decoder: main.f90 $(SRC) $(INC)
	\rm -f alpha_decoder alpha_decoder.f90 gts_decoder.f90 *.o *.mod
	cat $(MOD) main.f90 $(SRC) > alpha_decoder.f90
	grep -v '\.module' alpha_decoder.f90 > gts_decoder.f90
	$(COMPILE) $(FFLAGS) gts_decoder.f90 -o gts_decoder
	ln -s gts_decoder alpha_decoder
	\rm -f alpha_decoder.f90 gts_decoder.f90

# --------- Dependencies ----------

code_tables_new.o: code_tables_new.f90 $(INC)

code_tables.o: code_tables.f90 inc.code_tables $(INC)

decode_fm12.o: decode_fm12.f90 $(INC)

decode_fm15.o: decode_fm15.f90 $(INC)

decode_fm32.o: decode_fm32.f90 $(INC)

decode_fm35.o: decode_fm35.f90 $(INC)

decode_fm86.o: decode_fm86.f90 $(INC)

decode_fm88.o: decode_fm88.f90 $(INC)

decode_airep.o: decode_airep.f90 $(INC)

wr_rap_fm12.o: wr_rap_fm12.f90 $(INC)

wr_rap_fm35.o: wr_rap_fm35.f90 $(INC)

# ----------------------------------

# This needs to come after the other make targets

# DO NOT DELETE THIS LINE -- make depend depends on it.


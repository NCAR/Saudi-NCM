###########################################################################
#
# Makefile for spdb2ascii
#
###########################################################################

# ---------- Definitions -----------
#
# Include the standard shared make info (make_defns) and the
# operating system specific info (compiler_macros.$(MS_OS_TYPE))

include $(MS_MAKE_MACROS_DIR)/make_defns
include $(MS_MAKE_MACROS_DIR)/compiler_macros.$(MS_OS_TYPE)


TARGET_FILE = radar2wrffdda
TARGET_FILE1 = radar_to_wrffdda.exe
TARGET_FILE2 = qr2wrffdda
TARGET_FILE3 = radar2wrfout
netcdf_includ = $(NETCDF)/include
netcdf_lib = $(NETCDF)/lib -lnetcdf -lnetcdff
#netcdf_includ = /home/fddasys/fddahome/cycle_code/SOURCE/wrf/netcdf/include
#netcdf_lib = /home/fddasys/fddahome/cycle_code/SOURCE/wrf/netcdf/lib -lnetcdf

#FC = pgf90  -Mfreeform -r4 -i4 -pc 32 -byteswapio
#FC90 = pgf90 -byteswapio
#FORCMP = pgf90
 
SRCS = radar_to_wrffdda.f90 WRF_kinds.F90 WRF_ncread.F90 WRF_utils.F90 nc_utils.F qr3d_to_wrffdda.f90 radar3d_to_wrfout.f90
OBJ = radar_to_wrffdda.o WRF_kinds.o WRF_ncread.o WRF_utils.o mm5_utils.o nc_utils.o
OBJ2 = qr3d_to_wrffdda.o WRF_kinds.o WRF_ncread.o WRF_utils.o mm5_utils.o nc_utils.o
OBJ3 = radar3d_to_wrfout.o WRF_kinds.o WRF_ncread.o WRF_utils.o mm5_utils.o nc_utils.o

all:$(TARGET_FILE) $(TARGET_FILE1) $(TARGET_FILE2) $(TARGET_FILE3)

$(TARGET_FILE):$(OBJ)
	$(FC90) -o $(TARGET_FILE) $(OBJ) -L $(netcdf_lib) -I $(netcdf_includ)

$(TARGET_FILE1):$(OBJ)
	$(FC90) -o $(TARGET_FILE1) $(OBJ) -L $(netcdf_lib) -I $(netcdf_includ)

$(TARGET_FILE2):$(OBJ2)
	$(FC90) -o $(TARGET_FILE2) $(OBJ2) -L $(netcdf_lib) -I $(netcdf_includ)

$(TARGET_FILE3):$(OBJ3)
	$(FC90) -o $(TARGET_FILE3) $(OBJ3) -L $(netcdf_lib) -I $(netcdf_includ)

.F.o:
	$(FC) -c  -I ./  -I $(netcdf_includ)  $<

radar_to_wrffdda.o: radar_to_wrffdda.f90 WRF_kinds.mod WRF_ncread.mod WRF_utils.mod
	$(FC90) -c  -I ./  -I $(netcdf_includ)  $<
qr3d_to_wrffdda.o: qr3d_to_wrffdda.f90 WRF_kinds.mod WRF_ncread.mod WRF_utils.mod
	$(FC90) -c  -I ./  -I $(netcdf_includ)  $<
radar3d_to_wrfout.o: radar3d_to_wrfout.f90 WRF_kinds.mod WRF_ncread.mod WRF_utils.mod
	$(FC) -c  -I ./  -I $(netcdf_includ)  $<

.f.o:
	$(FC90) -c  -I ./  -I $(netcdf_includ)  $<

WRF_kinds.mod: WRF_kinds.o
WRF_ncread.mod: WRF_ncread.o
WRF_utils.mod: WRF_utils.o

WRF_kinds.o:
	${FORCMP} -c WRF_kinds.F90

WRF_ncread.o: WRF_kinds.mod
	${FORCMP} -c -I $(netcdf_includ) WRF_ncread.F90

WRF_utils.o: WRF_kinds.mod WRF_ncread.mod
	${FORCMP} -c -I $(netcdf_includ) WRF_utils.F90
    
install:
	cp -p $(TARGET_FILE1) $(RAP_BIN_DIR)/

clean:
	rm -f *.o $(TARGET_FILE) *.mod

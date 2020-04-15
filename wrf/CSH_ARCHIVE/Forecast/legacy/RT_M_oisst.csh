#!/bin/csh -f
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ----------------------- RT_M_oisst starts ----------------------------"
echo  " ----------------------------------------------------------------------"
echo  " ---------------- Replace LOWBDY sst with OISST -----------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM OISST
setenv RM "rm -rf"

#  
# ENVIRONMENT
#  
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"
   
$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
    echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
    exit (2)
endif

source ${CFILE}user.mm5sys.${MM5HOST}
source ${CFILE}sizes.mm5sys.${MM5HOST}


if ( $#argv < 4 ) then
    echo "usage: $0 start_date this_cycle datadir resolution"
    echo "where start_date,end_date and this_cycle are CCYYMMDDHH"
    echo "datadir is the path to directory OISST." 
    echo "resolution is the resolution of the SST input data in dg (1/025)"
    exit ( 1 )
endif

set start_date = $1
set this_cycle = $2
set CYC_INT    = $3
set DATADIR    = $4

set resol = 1
if ( ${#argv} >= 5) then
  set resol = $5
endif

set ccyy = `echo $start_date |cut -c1-4`
echo $ccyy, $start_date
# If date is before Jan 1 1982 use 2 degree resolution data 
if ($ccyy < 1982 && $resol == 1) then
    echo "Date is before 1982, default to ERSST, 2 deg"
    set resol = 2
endif

set ccyy = `echo $this_cycle |cut -c1-4`
echo $ccyy, $this_cycle
# If resol = 2 and date after 2002, switch back to 1 degree SST
if ($ccyy > 2002 && $resol == 2) then
    echo "Date is after 2002, default to OISST, 1 deg"
    set resol = 1
endif

if ($resol == 2) then
    # Use 2 degree weekly OI SSTs
    setenv OISST_EXE $EXECUTABLE_ARCHIVE/ersst.exe
else if ($resol == 1) then
    # Use 1 degree weekly OI SSTs
    setenv OISST_EXE $EXECUTABLE_ARCHIVE/oisst.exe
else 
    # Use 0.25 degree daily OI SSTs
    setenv OISST_EXE $EXECUTABLE_ARCHIVE/oisst4.exe
endif
echo "Using executable: $OISST_EXE"

# Calculate number of day periods required

@ ntimes = $CYC_INT / 24
@ ntimes = $ntimes + 1
echo "SST times: $ntimes"

#	Get to the right directory

set ccdir = $RUNDIR/$this_cycle

if (-d $GEAPSTMP) then
   $MustHaveDir $GEAPSTMP/OISST
   ln -s $GEAPSTMP/OISST $RUNDIR/$this_cycle/OISST
else
   $MustHaveDir $RUNDIR/$this_cycle/OISST
endif

cd $RUNDIR/$this_cycle/OISST
echo changing to $RUNDIR/$this_cycle/OISST
echo now in `pwd`

#------------------------------------------------------------------------------#
#   Bring input SST data files
#------------------------------------------------------------------------------#

# First day of time period on which data are needed.
set ccyymmdd_start = `echo $start_date |cut -c1-8`
echo "Start date: $ccyymmdd_start"

# Last day of time period on which data are needed.
# oisst.f90 and oisst4.f90 assumes 7 days of data from ccyy_start
set ccyymmdd_end   = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $ccyymmdd_start $ntimes`

# First and last year of time period on which data are needed.
set ccyy_start     = `echo $ccyymmdd_start |cut -c1-4`
set ccyy_end       = `echo $ccyymmdd_end |cut -c1-4`

echo "Start year: $ccyy_start"
echo "End year  : $ccyy_end"

set n = 0

if ($resol == 2) then

    echo "scp ${DATADIR}/OISST/ERSST_V2.nc ."
    scp ${DATADIR}/OISST/ERSST_V2.nc .
    if (! -e ERSST_V2.nc) then
       echo "WARNING: Could not find 2 degree ERSST data... exiting"
       exit(2)
    endif

    set  oisstPath = "./"

else if ($resol == 1) then

    set ccyy = $ccyy_start

    while ($ccyy <= $ccyy_end)

       scp ${DATADIR}/OISST/oisst.${ccyy}.asc .           

       if (! -e oisst.${ccyy}.asc) then
           echo "WARNING: Cannot find file ${DATADIR}/OISST/oisst.${ccyy}.asc"
           exit(2)
       endif

       @ ccyy ++
       @ n ++

       if ($n >= 100) break  # already 100 years, avoid infinite loop.

    end

#   Need land/sea mask too

    scp ${DATADIR}/OISST/land.sea.mask.v2.asc .

    if (! -e land.sea.mask.v2.asc) then
        echo "WARNING: Cannot find file ${DATADIR}/OISST/land.sea.mask.v2.asc"
        exit(2)
    endif

# set local path to input data files

    set  oisstPath = "./"

else

    set ccyymmdd = `echo $start_date |cut -c 1-8`

    while ($ccyymmdd <= $ccyymmdd_end)

       set ccyy = `echo $ccyymmdd |cut -c1-4`
       scp ${DATADIR}/OISST4/${ccyy}/sst4.${ccyymmdd} .
    
       if (! -e sst4.${ccyymmdd}) then
           echo "WARNING: Cannot find files ${DATADIR}/OISST4/${ccyy}/sst4.${ccyymmdd}"
           exit(2)
       endif

       set ccyymmdd = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $ccyymmdd 1`

       @ n ++
       if ($n >= 100) break  # already 100 years, avoid infinite loop.

    end

# set local path to input data files

    set  oisstPath = "./sst4."

endif

#------------------------------------------------------------------------------#
# Need MM5 files
#------------------------------------------------------------------------------#

set d = 1

while ($d <= $NUM_DOMS)

#	Which lowbdy file

    if ( ( -l LowBdyFile ) || ( -e LowBdyFile ) ) rm LowBdyFile
    if      ( -e  ${ccdir}/${this_cycle}_LOWBDY_DOMAIN${d}.${MM5HOST} ) then
	set lower_boundary_file = ${ccdir}/${this_cycle}_LOWBDY_DOMAIN${d}.${MM5HOST}
	echo "${SUBSYSTEM}  Using lower boundary file $lower_boundary_file"
    else
	echo "${SUBSYSTEM}  Missing file -> NO lower boundary file  ${ccdir}/${this_cycle}_LOWBDY_DOMAIN${d}.${MM5HOST}"
	exit (2)	
    endif

#	Which terrain file

    if ( ( -l TerrainFile ) || ( -e TerrainFile ) ) rm TerrainFile

    if ( -e $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN$d ) then
	set terrain_file = $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN$d
	echo "${SUBSYSTEM}  Using Terrain in $GSJOBDIR $terrain_file"
    else if ( -e  $TERRAIN_DIR/Domain${d}_New_LU.V.${MM5HOST} ) then
	set terrain_file = $TERRAIN_DIR/Domain${d}_New_LU.V.${MM5HOST}
	echo "${SUBSYSTEM}  Using Terrain in $TERRAIN_DIR"
    else
    echo "${SUBSYSTEM}  Missing file -> NO terrain in $GSJOBDIR or $TERRAIN_DIR"
    exit (2)
    endif

    #	Create namelist

cat << EOF > namelist.input
&fileio
 lbdyFile = '$lower_boundary_file'
 terrainFile = '$terrain_file'
 oisstPath = '$oisstPath',
 landseaFile = './land.sea.mask.v2.asc'
 ntimes = $ntimes
/
EOF

    #	Run program

    rm -f NewLowBdy.dat 

    ( time $OISST_EXE namelist.input) >! oisst${d}_print.out

    #	move files and listing to top run dir

    mv oisst${d}_print.out $ccdir/${this_cycle}_oisst${d}_print.out

    if ( ( -e NewLowBdy.dat ) ) then
	echo "All files created by OISST for domain $d"
	cp NewLowBdy.dat $lower_boundary_file
    else
	echo "not all files created by OISST for domain $d"
	exit ( 2 ) 
    endif

    @ d = $d + 1
end

exit 0;

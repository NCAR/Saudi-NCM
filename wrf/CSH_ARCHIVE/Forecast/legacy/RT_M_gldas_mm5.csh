#!/bin/csh -f
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ----------------------- RT_M_gldas starts ----------------------------"
echo  " ----------------------------------------------------------------------"
echo  " ---------------- Replace MMINPUT sfc data with GLDAS -----------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM GLDAS
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
    echo "usage: $0 start_date this_cycle cyc_int datadir"
    echo "where: start_date,end_date and this_cycle are CCYYMMDDHH"
    echo "       cyc_int is the cycle lenght (in hours)"
    echo "       datadir is the path to directory GLDAS." 
    exit ( 1 )
endif

set start_date = $1
set this_cycle = $2
set CYC_INT    = $3
set DATADIR    = $4

set ccyy_start = `echo $start_date |cut -c1-4`
echo $start_date, $ccyy_start 

setenv GLDAS_EXE $EXECUTABLE_ARCHIVE/readgldas.exe
echo "Using executable: $GLDAS_EXE"

# Calculate number of day periods required

@ ntimes = $CYC_INT / 24
@ ntimes = $ntimes + 1
echo "SST times: $ntimes"

#	Get to the right directory

set ccdir = $RUNDIR/$this_cycle

if (-d $GEAPSTMP) then
   $MustHaveDir $GEAPSTMP/GLDAS
   ln -s $GEAPSTMP/GLDAS $RUNDIR/$this_cycle/GLDAS
else
   $MustHaveDir $RUNDIR/$this_cycle/GLDAS
endif

cd $RUNDIR/$this_cycle/GLDAS
echo changing to $RUNDIR/$this_cycle/GLDAS
echo now in `pwd`

#------------------------------------------------------------------------------#
#   Bring input SST data files
#------------------------------------------------------------------------------#

# First day of time period on which data are needed.
set ccyymmdd_start = `echo $start_date |cut -c1-8`
echo "Start date: $ccyymmdd_start"

# Last day of time period on which data are needed.
# gldas.f90 and gldas4.f90 assumes 7 days of data from ccyy_start
set ccyymmdd_end   = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $ccyymmdd_start $ntimes`

# First and last year of time period on which data are needed.
set ccyy_start     = `echo $ccyymmdd_start |cut -c1-4`
set ccyy_end       = `echo $ccyymmdd_end |cut -c1-4`

echo "Start year: $ccyy_start"
echo "End year  : $ccyy_end"

set n = 0

set ccyymmdd = `echo $start_date |cut -c 1-8`
while ($ccyymmdd <= $ccyymmdd_end)

   set ccyy = `echo $ccyymmdd |cut -c1-4`
   echo "scp ${DATADIR}/gldas/cfdda_input/${ccyy}/NOAH10SUBP.${ccyymmdd}00.grib"
   scp ${DATADIR}/gldas/cfdda_input/${ccyy}/NOAH10SUBP.${ccyymmdd}00.grib .
    
   if (! -e NOAH10SUBP.${ccyymmdd}00.grib) then
      echo "WARNING: Cannot find files ${DATADIR}/gldas/cfdda_input/${ccyy}/NOAH10SUBP.${ccyymmdd}00.grib"
      exit(2)
   endif

   set ccyymmdd = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $ccyymmdd 1`

   @ n ++
   if ($n >= 100) break  # already 100 years, avoid infinite loop.

end

# set local path to input data files

set  gldasPath = "./"

#------------------------------------------------------------------------------#
# Need MM5 input files
#------------------------------------------------------------------------------#

set d = 1

while ($d <= $NUM_DOMS)

# Which lowbdy file

  if ( ( -l MMinputFile ) || ( -e MMinputFile ) ) rm MMinputFile
  if      ( -e  ${ccdir}/${this_cycle}_MMINPUT_DOMAIN${d}.${MM5HOST} ) then
     set mminput_file = ${ccdir}/${this_cycle}_MMINPUT_DOMAIN${d}.${MM5HOST}
     echo "${SUBSYSTEM}  Using MM5 input file $mminput_file"
  else
     echo "${SUBSYSTEM}  Missing file -> NO MM5 input file  ${ccdir}/${this_cycle}_MMINPUT_DOMAIN${d}.${MM5HOST}"
     exit (2)	
  endif

# Which terrain file

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
 mminfile = '$mminput_file'
 terrainFile = '$terrain_file'
 gldasPath = '$gldasPath'
 lprint = .T.
/
EOF

# Run program

  rm -f NewMMINPUT.dat

  ( time $GLDAS_EXE namelist.input) >! gldas${d}_print.out

  # move files and listing to top run dir

  mv gldas${d}_print.out $ccdir/${this_cycle}_gldas${d}_print.out

  if ( ( -e NewMMINPUT.dat ) ) then
     echo "All files created by GLDAS for domain $d"
     cp NewMMINPUT.dat $mminput_file
  else
     echo "not all files created by GLDAS for domain $d"
     exit ( 2 ) 
  endif

  @ d = $d + 1
end

exit 0;

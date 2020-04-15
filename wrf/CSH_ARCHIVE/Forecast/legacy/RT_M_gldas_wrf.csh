#!/bin/csh -f
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " --------------------- RT_M_gldas_wrf starts --------------------------"
echo  " ----------------------------------------------------------------------"
echo  " ---------------- Replace WRFINPUT sfc data with GLDAS ----------------"
echo  " ----------------------------------------------------------------------"
echo
echo "$0 $argv[*]"
echo ""
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM GLDAS
setenv RM "rm -rf"

##  
## ENVIRONMENT
##  
#set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"
#   
#$CheckConfigFiles
#set cfstat = $status
#if ( $cfstat != 0 ) then
#    echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
#    exit (2)
#endif

#source ${CFILE}user.mm5sys.${MM5HOST}
#source ${CFILE}sizes.mm5sys.${MM5HOST}

#
# Environment from MM
#
if (-e $GSJOBDIR/tmp/$this_cycle/cshrc) then
   source $GSJOBDIR/tmp/$this_cycle/cshrc
   echo "Found and Sourced $GSJOBDIR/tmp/$this_cycle/cshrc"
endif


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

setenv GLDAS_EXE $EXECUTABLE_ARCHIVE/readgldas_wrf.exe
echo "Using executable: $GLDAS_EXE"

# Calculate number of day periods required

@ ntimes = $CYC_INT / 24
@ ntimes = $ntimes + 1
echo "GLDAS times: $ntimes"

#	Get to the right directory

set ccdir = $RUNDIR/$this_cycle

if (-d $GEAPSTMP) then
   mkdir -p $GEAPSTMP/GLDAS
   ln -s $GEAPSTMP/GLDAS $RUNDIR/$this_cycle/GLDAS
else
   mkdir -p $RUNDIR/$this_cycle/GLDAS
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
# Need WRF input files
#------------------------------------------------------------------------------#

if (-e print.out_gldas) rm -f print.out_gldas

set dom = 1

while ($dom <= $NUM_DOMS)

  echo \
 "-----------------------------------------------------------------------"

  if ($dom < 10) then
      set d = "0$dom"
  else
      set d = "$dom"
  endif

# Which lowbdy file

  if ( -s ${ccdir}/${this_cycle}_wrfinput_d${d} ) then
     set wrfinput_file = ${ccdir}/${this_cycle}_wrfinput_d${d}
  else if ( -s ${ccdir}/${this_cycle}_wrfinput_d${d}_cold ) then
     set wrfinput_file = ${ccdir}/${this_cycle}_wrfinput_d${d}_cold
  else
     echo "${SUBSYSTEM}  Missing file -> NO WRF input file  ${ccdir}/${this_cycle}_wrfinput_d${d}[_cold]"
     exit (2)	
  endif

# Which geogrid file

  if ( -e $GSJOBDIR/wps/geo_em.d${d}.nc ) then
     set geogrid_file = $GSJOBDIR/wps/geo_em.d${d}.nc
     echo "${SUBSYSTEM}  Using geogrid file in $GSJOBDIR $geogrid_file"
  else
    echo "${SUBSYSTEM}  Missing file -> NO geogrid file in $GSJOBDIR/wps"
    exit (2)
  endif

    #	Create namelist

cat << EOF > namelist.input
&fileio
 wrfinfile = '$wrfinput_file'
 geogridFile = '$geogrid_file'
 gldasPath = '$gldasPath'
 lprint = .T.
/
EOF

# Run program

   echo "Domain $dom" >>! print.out_gldas

  ( time $GLDAS_EXE namelist.input) >>! print.out_gldas

# Check

   set s = `grep Successful print.out_gldas`

   if ($#s <= 0) then
       echo "${SUBSYSTEM}  $GLDAS_EXE failed for domain $dom"
       echo "Check listing $ccdir/print.out_gldas"
   else
       echo "${SUBSYSTEM}  Successful completion of $GLDAS_EXE for domain $dom"
   endif

# Go to next domain
  @ dom = $dom + 1

end

# move files and listing to top run dir

# cp -f print.out_gldas $ccdir/print.out_gldas

echo
echo  "End of RT_M_gldas_wrf"

exit 0;

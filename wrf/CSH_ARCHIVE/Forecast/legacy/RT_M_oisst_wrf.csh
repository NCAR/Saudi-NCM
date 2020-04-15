#!/bin/csh -f
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ----------------------- RT_M_oisst_wrf starts ------------------------"
echo  " ----------------------------------------------------------------------"
echo  " ------------- Replace WRF INPUT SST/TSK with OISST -------------------"
echo  " ----------------------------------------------------------------------"
echo
echo "$0 $argv[*]"
echo ""
###############################################################################

# set echo 
set timestamp 
setenv SUBSYSTEM OISST
setenv RM "rm -rf"

## This stuff is not needed anymore
## ENVIRONMENT
##  
#set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"
   
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
    setenv OISST_WRF_EXE $EXECUTABLE_ARCHIVE/ersst_wrf.exe
else if ($resol == 1) then
    # Use 1 degree weekly OI SSTs
    setenv OISST_WRF_EXE $EXECUTABLE_ARCHIVE/oisst_wrf.exe
else 
    # Use 0.25 degree daily OI SSTs
    setenv OISST_WRF_EXE $EXECUTABLE_ARCHIVE/oisst4_wrf.exe
endif
echo "Using executable: $OISST_WRF_EXE"

# Calculate number of day periods required

#20080913 FV and RS set ntimes = 1 in all cases
#@ ntimes = $CYC_INT / 24
#@ ntimes = $ntimes + 1
set ntimes = 1
echo "SST times: $ntimes"

#	Get to the right directory

set ccdir = $RUNDIR/$this_cycle

if (-d $GEAPSTMP) then
   mkdir -p $GEAPSTMP/OISST
   ln -s $GEAPSTMP/OISST $RUNDIR/$this_cycle/OISST
else
   mkdir -p $RUNDIR/$this_cycle/OISST
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

    echo \
   "scp ${DATADIR}/OISST/ERSST_V2.nc ."
    scp ${DATADIR}/OISST/ERSST_V2.nc .

    if (! -e ERSST_V2.nc) then
       echo "WARNING: Could not find 2 degree ERSST data... exiting"
       exit(2)
    endif

    set  oisstPath = "./"

else if ($resol == 1) then

    set ccyy = $ccyy_start

    while ($ccyy <= $ccyy_end)

       echo \
      "scp ${DATADIR}/OISST/oisst.${ccyy}.asc ."
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

    echo \
   "scp ${DATADIR}/OISST/land.sea.mask.v2.asc ."
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

       echo \
      "scp ${DATADIR}/OISST4/${ccyy}/sst4.${ccyymmdd} ."
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

if (-e print.out_oisst) rm -f print.out_oisst

set dom = 1

while ($dom <= $NUM_DOMS)

   echo \
  "-----------------------------------------------------------------------"

# Need two digits for WRF input file

    if ($dom < 10) then
        set d = "0$dom"
    else
        set d = "$dom"
    endif

#	Which lowbdy file

    if      ( -s  ${ccdir}/${this_cycle}_wrfinput_d${d} ) then
        set wrfinput_file = ${ccdir}/${this_cycle}_wrfinput_d${d}
	echo "${SUBSYSTEM}  Using WRF input file $wrfinput_file"
    else if ( -s ${ccdir}/${this_cycle}_wrfinput_d${d}_cold ) then
        set wrfinput_file = ${ccdir}/${this_cycle}_wrfinput_d${d}_cold
	echo "${SUBSYSTEM}  Using WRF input file $wrfinput_file"
    else
	echo "${SUBSYSTEM}  Missing file -> NO WRF input file  ${ccdir}/${this_cycle}_wrfinput_d${d}[_cold]"
	exit (2)	
    endif

    # for the purpose of comapring with original fields:
    cp -f $wrfinput_file ${wrfinput_file}.save

#	Which geogrid file

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
 wrfinFile = '$wrfinput_file'
 geogridFile = '$geogrid_file'
 oisstPath = '$oisstPath',
 landseaFile = './land.sea.mask.v2.asc'
 ntimes = $ntimes
/
EOF

    #	Run program

    echo "Domain $dom" >>!  print.out_oisst 

    ( time $OISST_WRF_EXE namelist.input) >>! print.out_oisst

   set s = `grep Successful print.out_oisst`

   if ($#s <= 0) then
       echo "${SUBSYSTEM}  $OISST failed for domain $dom"
       echo "Check listing $ccdir/print.out_oisst"
       echo
       echo "Reverting changes in files $wrfinput_file"
       mv -f ${wrfinput_file}.save $wrfinput_file 
   else
       echo "${SUBSYSTEM}  Successful completion of $OISST_WRF_EXE for domain $dom"
       rm -f ${wrfinput_file}.save
   endif

    @ dom = $dom + 1

end

# move files and listing to top run dir

# cp -f print.out_oisst $ccdir/print.out_oisst

echo
echo "End of RT_M_oisst_wrf" 

exit 0;

#!/bin/tcsh -f
#------------------------------------------------------------------------------
# This shell decodes MICROSTEPS data files for MM5 RT-FDDA ingestion.
#
# Copyright UCAR (c) 1992 - 2004.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Program (RAP),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#
# Francois Vandenberghe, vandenb@ucar.edu, August 2004.
#------------------------------------------------------------------------------

echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- MICROSTEPS decoder starts  ------------------------------"
echo  " ----------------------------------------------------------------------"

set debug = 0  # 0=real-time, 1=specify work dir+printout on stderrout

if ( ${#argv} <  2 ) then
	echo "usage: $0 cycle_date obs_date [averaging_time]"
	echo "where start_date is CCYYMMDDHH"
	echo "and obs_date is CCYYMMDDHH"
	echo "Averaging time is in minutes (0 by default)"
	exit ( 1 )
else
        echo
        echo "$0 $argv[*]"
        echo
endif

#set echo
set timestamp
setenv SUBSYSTEM MICROSTEPS_FDDA

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

# Testing only

if ($debug > 0) then 
    setenv MM5HOST GRM
endif

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST}; # This sets var. MICROSTEPS_DATA_DIR

set start_date = $1
set obs_date   = $2

if ( ${#argv} >= 3 ) then
    set Tavering_mn = $3
else
    set Tavering_mn = 0
endif

# Testing only

if ($debug > 0) then 
set RUNDIR = /data/cycles/GMUAE/GRM
if (! -d $RUNDIR/${start_date}) mkdir -p $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_MICROSTEPS) mkdir -p $RUNDIR/${start_date}/RD_MICROSTEPS
endif

#	Does the directory exist

if(-d $GEAPSTMP) then
 if (! -d $GEAPSTMP/RD_MICROSTEPS) then
  $MustHaveDir $GEAPSTMP/RD_MICROSTEPS
  ln -s $GEAPSTMP/RD_MICROSTEPS $RUNDIR/${start_date}/RD_MICROSTEPS
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_MICROSTEPS
endif

#	Have we already done this

if ( -e  $RUNDIR/${start_date}/RD_MICROSTEPS/${obs_date}_MICROSTEPS_data.${MM5HOST}   && \
    ! -z $RUNDIR/${start_date}/RD_MICROSTEPS/${obs_date}_MICROSTEPS_data.${MM5HOST} ) then
	echo "MICROSTEPS data have already been decoded at:"
	echo
	echo "$RUNDIR/${start_date}/RD_MICROSTEPS/${obs_date}_MICROSTEPS_data.${MM5HOST}"
	echo
	exit (3)
endif

#	Go to the work directory

echo
echo "Data directory is $MICROSTEPS_DATA_DIR"
echo "Work directory is $RUNDIR/${start_date}/RD_MICROSTEPS"

cd $RUNDIR/${start_date}/RD_MICROSTEPS

#       Bring the locations files

#cp  ${CONSTANT_FILES}/MicroStep_Station_Information.txt .

#	Bring obs valid for the day and the day before

set ccyy  = `echo  $obs_date |cut -c1-4`
set mm    = `echo  $obs_date |cut -c5-6`
set dd    = `echo  $obs_date |cut -c7-8`
set hh    = `echo  $obs_date |cut -c9-10`

set ccyymmddb = `$EXECUTABLE_ARCHIVE/geth_newdate.exe ${ccyy}${mm}${dd} -1` 

set ccyyb  = `echo  $ccyymmddb |cut -c1-4`
set mmb    = `echo  $ccyymmddb |cut -c5-6`
set ddb    = `echo  $ccyymmddb |cut -c7-8`

#if ($debug > 0) then
#echo
#echo "find ${MICROSTEPS_DATA_DIR} -name ${ccyy}_${mm}_${dd}_${hh}\*_microStep.dataStore | grep -v  ${ccyy}${mm}${dd}"
#endif
#set files = `find ${MICROSTEPS_DATA_DIR} -name ${ccyy}_${mm}_${dd}_${hh}\*_microStep.dataStore |grep -v ${ccyy}${mm}${dd}`
#if ($#files <= 0) then
#       echo
#	echo "No MICROSTEPS data files found for ${obs_date}"
#       echo
#	exit (4)
#endif

# Path  MICROSTEPS_DATA_DIR was read in config file at line 49
#if (! -e ${MICROSTEPS_DATA_DIR}/microStep.dataStore) then
#       echo
#       echo "File microStep.dataStore cannot be found at ${MICROSTEPS_DATA_DIR}"
#       echo
#       exit (4)
#endif

#       Grab the data valid for this hour

#grep "${ccyy}-${mm}-${dd} ${hh}" ${MICROSTEPS_DATA_DIR}/microStep.dataStore >! ./${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore

# Path  MICROSTEPS_DATA_DIR was read in config file at line 49

if (`find ${MICROSTEPS_DATA_DIR} -name microStep.dataStore.${ccyy}${mm}${dd}.\* -print | wc -l` <= 0 && \
    `find ${MICROSTEPS_DATA_DIR} -name microStep.dataStore.${ccyyb}${mmb}${ddb}.\* -print | wc -l` <= 0) then
   echo
   echo "Cannot find files microStep.dataStore.${ccyy}${mm}${dd}.* or microStep.dataStore.${ccyyb}${mmb}${ddb}.*"
   echo "in directory ${MICROSTEPS_DATA_DIR}"
   echo
   exit (4)
endif

if (-e ./${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore) \
 rm -f ./${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore

foreach f (`find ${MICROSTEPS_DATA_DIR} -name microStep.dataStore.${ccyyb}${mmb}${ddb}.\*`)
cat $f >>! ./${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore
end

foreach f (`find ${MICROSTEPS_DATA_DIR} -name microStep.dataStore.${ccyy}${mm}${dd}.\*`)
cat $f >>! ./${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore
end

#cat microStep.dataStore.${ccyy}${mm}${dd}.${hh}* >! ./${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore

#set ffiles = ($files:gt)
# echo
#echo "MICROSTEPS input files at ${obs_date}:"
#set n = 1
#while ($n <= $#files)
# set ff = $files[$n]
# set f  = ($ff:t)
# echo "$f"
# cat $ff >>! ${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore
# @ n = $n + 1
#end

#       Check input files

if (! -e ${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore  || \
      -z ${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore) then
       echo
       echo "Cannot find any data valid at ${hh}z on $ccyy/$mm/$dd"
       echo "In file $MICROSTEPS_DATA_DIR/microStep.dataStore"
       echo
   exit (5)
endif

#if (! -e MicroStep_Station_Information.txt) then
#    echo
#    echo "Cannot find file MicroStep_Station_Information.txt in directory ${CONSTANT_FILES}"
#    echo
#    exit (7)
#endif

#if (-z MicroStep_Station_Information.txt) then
#    echo
#    echo "Empty file ${CONSTANT_FILES}/MicroStep_Station_Information.txt"
#    echo
#    exit (8)
#endif

#     Decode data

echo
echo "Decoded MicroSteps data at $obs_date."

if ($debug > 0) then
$EXECUTABLE_ARCHIVE/rd_microstep.exe -i ${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore -t $Tavering_mn -debug
else
$EXECUTABLE_ARCHIVE/rd_microstep.exe -i ${ccyy}_${mm}_${dd}_${hh}_microStep.dataStore -t $Tavering_mn >&! rd_microstep_${hh}_print.out
endif

#     Check output file

if (! -e ${ccyy}_${mm}_${dd}_${hh}_microStep.decoded) then
    echo
    echo "Problem with rd_microstep.exe, not output file was generated."
    echo
    exit (9)
endif

if (-z ${ccyy}_${mm}_${dd}_${hh}_microStep.decoded) then
    echo
    echo "Potential problem with rd_microstep.exe, output file is empty."
    echo
    exit (10)
endif

#	Put output where it is visible

if ( -e ${obs_date}_MICROSTEPS_data.${MM5HOST}) \
  rm -f ${obs_date}_MICROSTEPS_data.${MM5HOST}
  cp -f ${ccyy}_${mm}_${dd}_${hh}_microStep.decoded  \
        $RUNDIR/${start_date}/RD_MICROSTEPS/${obs_date}_MICROSTEPS_data.${MM5HOST}

if ( -e rd_microstep_${hh}_print.out) cat rd_microstep_${hh}_print.out >>! \
        $RUNDIR/${start_date}/${start_date}_microstep_print.out.all

# Print-out exit message

echo
echo "Successful decoding of MicroSteps data for $obs_date."
echo
echo "Output is $RUNDIR/${start_date}/RD_MICROSTEPS/${obs_date}_MICROSTEPS_data.${MM5HOST}"
echo

if ($debug > 0) then 
    exit (0)
endif

#	Clean up house

foreach f (rd_microstep_${hh}_print.out ${ccyy}_${mm}_${dd}_${hh}_microStep.rip)
#  if (-e $f) rm -f $f
end

exit (0) 

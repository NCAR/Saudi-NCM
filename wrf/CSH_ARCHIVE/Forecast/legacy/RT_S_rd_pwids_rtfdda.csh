#!/bin/tcsh -f
#------------------------------------------------------------------------------
# This shell decodes PWIDS data files for MM5 RT-FDDA ingestion.
#
# Copyright UCAR (c) 1992 - 2004.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Program (RAP),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#
# Francois Vandenberghe, vandenb@ucar.edu, April 2004.
#------------------------------------------------------------------------------

echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- PWIDS decoder starts  ------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"

set debug = 0  # 0=real-time, 1=specify work dir+printout on stderrout

if ( ${#argv} <  2 ) then
	echo "usage: $0 cycle_date obs_date [averaging_time]"
	echo "where start_date is CCYYMMDDHH"
	echo  "d obs_date is CCYYMMDDHH"
	echo "Averaging time is in minutes (15 by default)"
	exit ( 1 )
endif

#set echo
set timestamp
setenv SUBSYSTEM PWIDS_FDDA

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
source ${CFILE}user.mm5sys.${MM5HOST};    

set start_date = $1
set obs_date   = $2

if ( ${#argv} >= 3 ) then
    set Tavering_mn = $3
else
    set Tavering_mn = 15
endif

# Testing only

if ($debug > 0) then 
set RUNDIR = /data/cycles/DARPAP/GRM
if (! -d $RUNDIR/${start_date}) mkdir -p $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_PWIDS) mkdir -p $RUNDIR/${start_date}/RD_PWIDS
endif

#	Does the directory exist

if(-d $GEAPSTMP) then
 if (! -d $GEAPSTMP/RD_PWIDS) then
  $MustHaveDir $GEAPSTMP/RD_PWIDS
  ln -s $GEAPSTMP/RD_PWIDS $RUNDIR/${start_date}/RD_PWIDS
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_PWIDS
endif

#	Have we already done this

if ( -e  $RUNDIR/${start_date}/${obs_date}_PWIDS_data.${MM5HOST}   && \
    ! -z $RUNDIR/${start_date}/${obs_date}_PWIDS_data.${MM5HOST} ) then
	echo "PWIDS data have already been decoded at:"
	echo "$RUNDIR/${start_date}/${obs_date}_PWIDS_data.${MM5HOST}"
	exit (3)
endif

#	Go to the work directory

echo  "Data directory is $PWIDS_DATA_DIR"
echo "Work directory is $RUNDIR/${start_date}/RD_PWIDS"

cd $RUNDIR/${start_date}/RD_PWIDS
echo "Now working in  $cwd"

#       Bring the locations files

cp  ${CONSTANT_FILES}/Instruments.txt .

#	Bring obs valid between t+00mn and t+59mn

set ccyy = `echo  $obs_date |cut -c1-4`
set mm   = `echo  $obs_date |cut -c5-6`
set dd   = `echo  $obs_date |cut -c7-8`
set hh   = `echo  $obs_date |cut -c9-10`

if ($debug > 0) then
echo  "find ${PWIDS_DATA_DIR} -name ${ccyy}_${mm}_${dd}_${hh}\*_pwids.dat | grep -v  ${ccyy}${mm}${dd}"
endif

set files = `find ${PWIDS_DATA_DIR} -name ${ccyy}_${mm}_${dd}_${hh}\*_pwids.dat |grep -v ${ccyy}${mm}${dd}`

if ($#files <= 0) then
	echo  "No PWIDS data files found for ${obs_date}"
	exit (4)
endif

#       Catenate all the PWIDS input files in one single file

if (-e ${ccyy}_${mm}_${dd}_${hh}_pwids.dat) \
 rm -f ${ccyy}_${mm}_${dd}_${hh}_pwids.dat

#set ffiles = ($files:gt)
echo  "PWIDS input files at ${obs_date}:"

set n = 1
while ($n <= $#files)
 set ff = $files[$n]
 set f  = ($ff:t)
 echo "$f"
 cat $ff >>! ${ccyy}_${mm}_${dd}_${hh}_pwids.dat
 @ n = $n + 1
end

#       Check input files

if (! -e ${ccyy}_${mm}_${dd}_${hh}_pwids.dat) then
   echo  "Cannot find files $PWIDS_DATA_DIR/${ccyy}_${mm}_${dd}_${hh}*_pwids.dat"
   exit (5)
endif

if (-z ${ccyy}_${mm}_${dd}_${hh}_pwids.dat) then
    echo  "Cannot find any valid data in $PWIDS_DATA_DIR/${ccyy}_${mm}_${dd}_${hh}*_pwids.dat"
    exit (6)
endif

if (! -e Instruments.txt) then
    echo  "Cannot find file Instruments.txt in directory ${CONSTANT_FILES}"
    exit (7)
endif

if (-z Instruments.txt) then
    echo  "Empty file ${CONSTANT_FILES}/Instruments.txt"
    exit (8)
endif

#     Decode data

echo  "Decoded PWDIS data at $obs_date with $Tavering_mn mn averaging."

if ($debug > 0) then
${EXECUTABLE_ARCHIVE}/rd_pwids.exe -i ${ccyy}_${mm}_${dd}_${hh}_pwids.dat \
                                 -n Instruments.txt -t $Tavering_mn \
				 -debug
else
${EXECUTABLE_ARCHIVE}/rd_pwids.exe -i ${ccyy}_${mm}_${dd}_${hh}_pwids.dat \
                                 -n Instruments.txt -t $Tavering_mn \
                                 >&! rd_pwids_${hh}_print.out
endif

#     Check output file

if (! -e ${ccyy}_${mm}_${dd}_${hh}_pwids.decoded) then
    echo  "Problem with rd_pwids.exe, not output file was generated."
    exit (9)
endif

if (-z ${ccyy}_${mm}_${dd}_${hh}_pwids.decoded) then
    echo  "Possible problem with rd_pwids.exe, output file is empty."
    exit (10)
endif

#	Put output where it is visible

if ( -e ${obs_date}_PWIDS_data.${MM5HOST}) \
  rm -f ${obs_date}_PWIDS_data.${MM5HOST}
  mv -f ${ccyy}_${mm}_${dd}_${hh}_pwids.decoded  \
        $RUNDIR/${start_date}/RD_PWIDS/${obs_date}_PWIDS_data.${MM5HOST}

if ( -e rd_pwids_${hh}_print.out) cat rd_pwids_${hh}_print.out >>! \
        $RUNDIR/${start_date}/${start_date}_pwids_print.out.all

# Print-out exit message

echo  "Succesful decoding of PWDIDS data for $obs_date."
echo  "Output is $RUNDIR/${start_date}/RD_PWIDS/${obs_date}_PWIDS_data.${MM5HOST}"

if ($debug > 0) then 
    exit (0)
endif

#	Clean up house

foreach f (rd_pwids_${hh}_print.out ${ccyy}_${mm}_${dd}_${hh}_pwids.rip) 
   if (-e $f) rm -f $f
end

exit (0) 

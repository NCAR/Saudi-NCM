#!/bin/tcsh -f
#------------------------------------------------------------------------------
# This shell decodes LIDPROF data files for MM5 RT-FDDA ingestion.
#
# Copyright UCAR (c) 1992 - 2004.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Program (RAP),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#
# Yubao Liu, April 2004.
#------------------------------------------------------------------------------

echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- LIDPROF decoder starts  ------------------------------"
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
setenv SUBSYSTEM LIDPROF_FDDA

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

# Testing only

if ($debug > 0) then 
set RUNDIR = /data/cycles/DARPAP/GRM
if (! -d $RUNDIR/${start_date}) mkdir -p $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_LIDPROF) mkdir -p $RUNDIR/${start_date}/RD_LIDPROF
endif

#	Does the directory exist

if(-d $GEAPSTMP) then
 if (! -d $GEAPSTMP/RD_LIDPROF) then
  $MustHaveDir $GEAPSTMP/RD_LIDPROF
  ln -s $GEAPSTMP/RD_LIDPROF $RUNDIR/${start_date}/RD_LIDPROF
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_LIDPROF
endif

#	Go to the work directory

echo  "Data directory is $LIDPROF_DATA_DIR"
echo "Work directory is $RUNDIR/${start_date}/RD_LIDPROF"

cd $RUNDIR/${start_date}/RD_LIDPROF
echo "Now working in  $cwd"

#       Bring the locations files

cp  ${CONSTANT_FILES}/Instruments.txt .

#	Bring obs valid between t+00mn and t+59mn

set ccyy = `echo  $obs_date |cut -c1-4`
set mm   = `echo  $obs_date |cut -c5-6`
set dd   = `echo  $obs_date |cut -c7-8`
set hh   = `echo  $obs_date |cut -c9-10`

rm *prf
cp ${LIDPROF_DATA_DIR}/$ccyy$mm${dd}_$hh* .

#     Decode data
foreach aprf (`ls *.prf`)
echo $aprf > input
${EXECUTABLE_ARCHIVE}/rd_lidar_prof.exe <input >> ${start_date}_lidprof_print.out.all
cat fort.50 >> ${obs_date}_LIDPROF_data
end

exit (0) 

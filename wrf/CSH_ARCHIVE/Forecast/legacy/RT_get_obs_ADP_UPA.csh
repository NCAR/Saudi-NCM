#!/bin/csh -f
#------------------------------------------------------------------------------#
#
# Bring ADP UPA data files to the local disk. This script is a compilation
# of MM5 RT-FDDA script RT_get_obs_WMO_csh and script fetch_adp_upa.csh
# from MM5 FETCH package available at ftp://ftp.ucar.edu/mesouser/MM5V3
#
# Read in NCAR Mass Storage system has been replaced by simple link
# on the local disk. It is therefore assumed that ADP UPA data have
# already been downloaded and converted into IEEE binary format and
# saved on the local disk at $datadir/ADP_UPA, where datadir is the second
# argument in the command line calling this script. The first argument
# being the MM5 RT-FFDA cycle time.
#
# It is also assumed that the data catalog file MSS-file-list has been
# from NCAR's server  ncardata.ucar.edu:/datasets/ds464.0/MSS-file-list
# and copied at the path $datadir/ncardata/datasets/ds353.0/MSS-file-list
#
# Usage: RT_get_obs_ADP_UPA_csh cycle_date start_date end_date datadir
# -----
# Where: cycle_date is the MM5 RT-FDDA cycle time (CCYYMMDDHH)
#        start_date is the first time when the data are needed (CCYYMMDDHH).
#        end_date   is the last time when the data are needed (CCYYMMDDHH).
#        datadir is the path to the ADP_UPA directory where ADP UPA input data
#        are stored.
#
# ADP data stored in data "datadir" valid in [start_date, end_date] 
# will be copied into local data directory. 
# The "MustHaveDir" fuctions are used.
#
#------------------------------------------------------------------------------!
# Copyright UCAR (c) 1992 - 2004.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Program (RAP),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#
# Francois Vandenberghe, vandenb@ucar.edu, September 2004.
#------------------------------------------------------------------------------#
echo
echo " ----------------------------------------------------------------------"
echo " ------------------- Bring ADP UPA observations -----------------------"
echo " ----------------------------------------------------------------------"
echo
echo "$0 $argv[*]"
echo

#set echo
set timestamp
setenv SUBSYSTEM ADP_UPA
setenv RM "rm -rf"

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile  ${CFILE}user.mm5sys.${MM5HOST} -> exiting"
 exit (4)
endif
source ${CFILE}user.mm5sys.${MM5HOST}
source ${CFILE}sizes.mm5sys.${MM5HOST}
#
#
# Environment from MM
#
if(-e $GSJOBDIR/tmp/$this_cycle/cshrc) then
  source $GSJOBDIR/tmp/$this_cycle/cshrc
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Found and Sourced $GSJOBDIR/tmp/$this_cycle/cshrc"
endif
#
#------------------------------------------------------------------------------#
# Necessary arguments

if ( ${#argv} < 4 ) then
  echo "usage: ${0}:"
  echo "$0 cycle_date cycle_date start_date end_date datadir"
  echo "Where: cycle_date is the MM5 RT-FDDA cycle time (CCYYMMDDHH)"
  echo "       start_date is the first time when the data are needed (CCYYMMDDHH)"
  echo "       end_date is the last time when the data are needed (CCYYMMDDHH)"
  echo "       datadir is the path to the ADP_SFC directory where ADP SFC data are stored"
  exit (1)
endif

if ($1 < 1900000000 || $1 > 2100000000) then
  echo "ERROR: Bad cycle time $1"
  exit ( 2 )
endif

set cycle_date = $1
set start_date = $2
set end_date   = $3
set DATADIR = $4

# Bulding path to input data and catalog

set DSS = /DSS
set ncardata = ${DATA_DIR}/ncardata/datasets/ds353.4
set GCATDataDir = ${DATA_DIR}/ADP_UPA
#set LocalDataDir = /raid4/hahmann/data/ADP_UPA

#	Where is the data supposed to reside locally?

if (-d $GEAPSTMP) then
  $MustHaveDir $GEAPSTMP/RD_ADP
  if (! -e $RUNDIR/${cycle_date}/RD_ADP) \
     ln -s $GEAPSTMP/RD_ADP $RUNDIR/${cycle_date}/RD_ADP
else
  $MustHaveDir $RUNDIR/$cycle_date/RD_ADP
endif

cd $RUNDIR/$cycle_date/RD_ADP
echo "Now working in  $cwd"


if (-e catalog.upa) rm -f catalog.upa

#	Build the UTC date as yy mm dd hh

set cc = `date -u '+%Y' | cut -c -2`
set yy = `date -u '+%y'`
set mm = `date -u '+%m'`
set dd = `date -u '+%d'`
set hh = `date -u '+%H'`

#	Choose which UTC time to access

#if ( ! -x $EXECUTABLE_ARCHIVE/geth_newdate.exe ) then
#  echo "${SUBSYSTEM}" "Missing functions $EXECUTABLE_ARCHIVE/geth_newdate.exe  -> exiting"
#  exit (6)
#endif

# Grab data valid between [cycle_time-cyc_int, cycle_time+1hr]

#set i = 1
#foreach hour ( -$cyc_int 1 )

#   if ($i == 1) then
#      set start_date = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $cycle_date $hour`
#   else if ($i == 2) then
#      set end_date = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $cycle_date $hour`
#   else
#      continue
#   endif

#   @ i ++
#end

#------------------------------------------------------------------------------#
#	This is the the fetch_adp_sfc.csh script from the MM5 FETCH facility.
#	It has been modified to use data on local disks and not in NCAR MSS.
#       The starting and ending dates YYMMDDHH was coming in as arguments
#------------------------------------------------------------------------------#

set numbers = ( 1 2 3 4 5 6 7 8 9 10 11 12 )
set numbers0 = ( 01 02 03 04 05 06 07 08 09 10 11 12 )

if ( $start_date > $end_date ) then
   echo "Look for ADP observations between $start_date and $end_date"
   echo "but starting_date must be <= ending_date"
   exit ( 8 )
endif

if ( ( $start_date < 1900000000 ) || ( $end_date < 1900000000 ) ) then
   echo "Look for ADP observations between $start_date and $end_date"
   echo "but dates are out of ranges"
   exit ( 9 )
endif

if ( ( $start_date > 2100000000 ) || ( $end_date > 2100000000 ) ) then
   echo "Look for ADP observations between $start_date and $end_date"
   echo "but dates are out of ranges"
   exit ( 10 )
endif

echo ""
echo "Will use ADP UPA observations between $start_date and $end_date"

set start_year = `echo ${start_date} | cut -c1-4`
set start_month = `echo ${start_date} | cut -c5-6`
set start_month = $numbers[$start_month]
set start_day = `echo ${start_date} | cut -c7-8`
set start_hour = `echo ${start_date} | cut -c9-10`

set end_year = `echo ${end_date} | cut -c1-4`
set end_month = `echo ${end_date} | cut -c5-6`
set end_month = $numbers[$end_month]
set end_day = `echo ${end_date} | cut -c7-8`
set end_hour = `echo ${end_date} | cut -c9-10`

# Set up the ftp command-script to get the archive lists.  First, set
# up to login anonymously to ncardata.ucar.edu

if (-e $ncardata/MSS-file-list ) then
   echo "get MSS-file-list from $ncardata"
   cp $ncardata/MSS-file-list catalog.raob
   if (! -e catalog.raob) then
      echo "Failed to get catalog --> exit"
      exit(4)
   endif
else
   echo "get MSS-file-list from $ncardata --> exit"
   exit(4)
endif

set month = ( jan feb mar apr may jun jul aug sep oct nov dec )

echo
echo "-----------------------------------------------------------------------"

foreach list ( A B D F )
#foreach list ( A ) # Raobs only for MM5 C-FDDA.

echo "Searching for ADP UPA list $list" 

set file_names = ( ` grep ${start_year}$month[$start_month] catalog.raob | grep "LIST ${list}" | grep blks | cut -d" " -f2 ` )
set file_start = ( ` grep ${start_year}$month[$start_month] catalog.raob | grep "LIST ${list}" | grep blks | awk -F, '{ print $1 }' | awk -F" " '{ print $2 }' | awk -F- '{ print $1 }' ` )
set file_end   = ( ` grep ${start_year}$month[$start_month] catalog.raob | grep "LIST ${list}" | grep blks | awk -F, '{ print $1 }' | awk -F- '{ print $2 }' ` )

if ($#file_names > 0 && $#file_start > 0 && $#file_end > 0) then
    echo
    echo "Will seek file(s) $file_names in list ${list} "
    echo "Starting at $file_start"
    echo "Ending at   $file_end"

endif

if ( ( $start_year != $end_year ) || ( $start_month != $end_month ) ) then

  if ( $start_month != 12 ) then
    set y = $start_year
  else
    @ y = $start_year + 1
  endif

  while ( $y <= $end_year )
    if ( $y == $start_year ) then
      @ m = $start_month + 1
    else
      set m = 1
    endif
    if ( $y != $end_year ) then
      set em = 12
    else
      set em = $end_month
    endif
    while ( $m <= $em )

# File name
      set new_file_names = `grep ${y}$month[$m] catalog.raob | grep "LIST ${list}" | grep blks | cut -d" " -f2`
      if ($#new_file_names > 0) set file_names = ($file_names $new_file_names) 

# Starting date of validity
      set new_file_start = ` grep ${y}$month[$m] catalog.raob | grep "LIST ${list}" | grep blks | awk -F, '{ print $1 }' | awk -F" " '{ print $2 }' | awk -F- '{ print $1 }' `
      if ($#new_file_start > 0) set file_start = ($file_start $new_file_start) 

# Ending date of validity
      set new_file_end   = `grep ${y}$month[$m] catalog.raob | grep "LIST ${list}" | grep blks | awk -F, '{ print $1 }' | awk -F- '{ print $2 }'`
      if ($#new_file_end > 0) set file_end = ($file_end $new_file_end) 

      if ($#new_file_names > 0 && $#new_file_start > 0 && $#new_file_end > 0) then
          echo "Will seek file(s) $file_names in list ${list} "
          echo "Starting at $file_start"
          echo "Ending at   $file_end"
      endif

#     set file_names = ( $file_names ` grep ${y}$month[$m] catalog.raob | \\
#        grep "LIST ${list}" | grep blks | cut -d" " -f2 ` )
#     set file_start = ( $file_start ` grep ${y}$month[$m] catalog.raob | \\
#        grep "LIST ${list}" | grep blks | awk -F, '{ print $1 }' | awk -F" " '{ print $2 }' | awk -F- '{ print $1 }' ` )
#     set file_end   = ( $file_end ` grep ${y}$month[$m] catalog.raob | \\
#        grep "LIST ${list}" | grep blks | awk -F, '{ print $1 }' | awk -F- '{ print $2 }' ` )
      @ m ++
    end
    @ y ++
  end
endif

set count = ${#file_start}
set file_start_b = ( $file_start )
foreach f ( $file_start )
   set file_start_b[$count] = $f
   @ count --
end

# Find the first of the DATA files that is needed.

set start_count = 0
set start_found_flag = FALSE

foreach n ( $file_end )

   @ start_count ++

   set test_date = ${start_year}$numbers0[${start_month}]${start_day}

   if ( -e data_tmp ) rm data_tmp
   echo "$test_date" > data_tmp
   echo "$n" >> data_tmp

   cat >! ed_input << EOF
	r data_tmp
	g/jan/s//01/
	g/feb/s//02/
	g/mar/s//03/
	g/apr/s//04/
	g/may/s//05/
	g/jun/s//06/
	g/jul/s//07/
	g/aug/s//08/
	g/sep/s//09/
	g/oct/s//10/
	g/nov/s//11/
	g/dec/s//12/
	1,2w
	q
EOF
   ed < ed_input >& /dev/null

   if ( `cat data_tmp | sort | head -1` == $test_date ) then
      set start_found_flag = TRUE
      goto start_found
   endif
end

echo "Never found the starting file of list ${list} successfully."
set start_count = 100

start_found:

if ( $start_found_flag == TRUE ) then
  echo Found starting file of list ${list}: $file_names[$start_count]
endif

# Find the last of the DATA files that is needed.

set end_count = 0
set end_found_flag = FALSE
foreach n ( $file_start_b )

  @ end_count ++

  set test_date = ${end_year}$numbers0[${end_month}]${end_day}

  if ( -e data_tmp ) rm data_tmp
    echo "$test_date" > data_tmp
    echo "$n" >> data_tmp

    cat >! ed_input << EOF
	r data_tmp
	g/jan/s//01/
	g/feb/s//02/
	g/mar/s//03/
	g/apr/s//04/
	g/may/s//05/
	g/jun/s//06/
	g/jul/s//07/
	g/aug/s//08/
	g/sep/s//09/
	g/oct/s//10/
	g/nov/s//11/
	g/dec/s//12/
	1,2w
	q
EOF
    ed < ed_input >& /dev/null

    if ( `cat data_tmp | sort -r | head -1` == $test_date ) then
	@ end_count = ${#file_start_b} + 1 - $end_count
	set end_found_flag = TRUE
	goto end_found
    endif
end

echo "Never found the ending file of list ${list} file successfully."
set end_count = 0

end_found:

if ( $end_found_flag == TRUE ) then
   echo Found ending   file of list ${list}: $file_names[$end_count]
endif

@ nexpected = 1 + $end_count - $start_count
set count = 0
set incr  = 0
foreach f ( $file_names )

  @ count ++
  if ( ( $count >= $start_count ) && ( $count <= $end_count ) ) then
     if ( $count > 1 ) then
	@ prev = $count - 1
	if ( $f != $file_names[$prev] ) then
	   @ incr ++
	   if (-e $GCATDataDir/$f) then
	      cp -f $GCATDataDir/$f UPA_${list}.${incr}
           endif
           if (! -e UPA_${list}.$incr) then
              echo "WARNING: Probles copying file UPA_${list}.${incr}!"
              @ incr --
              continue
           endif
	endif
     else
	@ incr ++
	if (-e $GCATDataDir/$f) then
	   cp -f $GCATDataDir/$f UPA_${list}.${incr}
        endif
        if (! -e UPA_${list}.$incr) then
           echo "WARNING: Problems copying file UPA_${list}.${incr}!"
           @ incr --
           continue
        endif
     endif
  endif
end

#wait
echo
echo "Expected $nexpected file(s), found $incr file(s) for list $list"

echo "-----------------------------------------------------------------------"

end

rm data_tmp ed_input

echo "Content of directory ${cwd}:"
echo

ls -alh

cd $RUNDIR/$cycle_date

exit (0)

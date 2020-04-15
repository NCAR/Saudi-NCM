#!/bin/csh -f
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- ATHENS04 decoder starts  ---------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################
#------------------------------------------------------------------------------#
# This script decodes the WMO data provided by the Athens Observatory.
# The data files present in WMO format and the GTS decoder is used.
# The list of stations ids and locations is stored in the data input directory.
#
# Copyright UCAR (c) 1992 - 2004.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Program (RAP),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#
# Francois Vandenberghe, vandenb@ucar.edu, July 2004.
#------------------------------------------------------------------------------#
#set echo
# Temporary statement while the decoder is built.
exit 0

set debug = 0

#set echo
set timestamp
setenv SUBSYSTEM GTS_DECODER # Use GTS DECODER ENVIRONMENT

# This normally defined in the environment, but because it's temporary
# we define the path to the input data directory and the decoder executable here

setenv ATHENS04_DATA_DIR /raid/input/greece/surface
setenv ATHENS04_STTID /raid/input/greece/athens04_sttid

# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "  ${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST};

if ($debug > 0) then
    setenv MM5HOST GRM
endif

setenv USE_ONE_HOUR FALSE
setenv ATHENS04_EXE $GTS_DECODER_EXE

#	Check usage

if ( ${#argv} == 2 ) then
    set start_date = $1
    set obs_date   = $2
else
    echo "usage: $0 start_date obs_date"
    echo "where start_date is CCYYMMDD"
    echo "obs_date is CCYYMMDDHH"
    exit ( 4 )
endif

#	Does the directory exist

if ($debug == 0) then # Regular statements

if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/RD_ATHENS04
$MustHaveDir $GEAPSTMP/RD_ATHENS04/${obs_date}
ln -s $GEAPSTMP/RD_ATHENS04/${obs_date} $RUNDIR/${start_date}/RD_ATHENS04/${obs_date}
else
$MustHaveDir $RUNDIR/${start_date}
$MustHaveDir $RUNDIR/${start_date}/RD_ATHENS04/${obs_date}
endif

else # Testing only

setenv  RUNDIR  /raid/cycles/GREECE/GRM
if (! -d $RUNDIR/${start_date}) mkdir $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/ATHENS04_UCARGPS) mkdir $RUNDIR/${start_date}/RD_ATHENS04
if (! -d $RUNDIR/${start_date}/ATHENS04_UCARGPS) mkdir $RUNDIR/${start_date}/RD_ATHENS04/${obs_date}

endif

#	Have we already done this
## Skip this check, since it  needs to be re-run at each hour... as new obs arrive
##if ( -e $RUNDIR/${start_date}/${obs_date}_GTS_data.${MM5HOST} ) then
##	echo "INFO: already finished the GTS decode"
##	exit ( 0 )
##endif

#	Go to the directory

echo "Now working in  $cwd"
cd $RUNDIR/${start_date}/RD_ATHENS04/${obs_date}
echo "Now working in  $cwd"

pwd
#	Get the station ID files

if ( ( -l gts_sttnid_final ) || ( -e gts_sttnid_final ) ) rm gts_sttnid_final
echo "INFO: Linking $GTS_STTNID to current directory `pwd`"
ln -s $GTS_STTNID .
set ok = $status
if ( $ok != 0 ) then
    cp $GTS_STTNID .
endif

if ( ( -l gts_sttnid_final.icao ) || ( -e gts_sttnid_final.icao ) ) rm gts_sttnid_final.icao
echo "Linking $GTS_ICAO to current directory `pwd`"
ln -s $GTS_ICAO .
set ok = $status
if ( $ok != 0 ) then
    cp $GTS_ICAO .
endif

#	Get the GTS data over here

if ( -e athens04_data ) rm athens04_data
if ( -e gts_data )      rm gts_data

set date = ` echo $obs_date `
echo "Using DATA files:"
ls -1 $ATHENS04_DATA_DIR/${date}*.dat
cat $ATHENS04_DATA_DIR/${date}*.dat>>! athens04_data
ln -s -f athens04_data gts_data

#	Run the program

echo "$obs_date , 3" >! input
${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
set cutoff = `cat output`
echo "$cutoff" >! athens04.in
#( time $GTS_DECODER_EXE ) < gts.in >& /dev/null
 ( time $GTS_DECODER_EXE ) < gts.in >& athens04_decoder.print.out

#	Put output where it is visible
pwd
#set echo
if ( -e $RUNDIR/RD_ATHENS04/${start_date}/${obs_date}_ATHENS04_data.${MM5HOST} ) rm $RUNDIR/RD_ATHENS04/${start_date}/${obs_date}_ATHENS04_data.${MM5HOST}
cat *.7?? >! ../${obs_date}_ATHENS04_data.${MM5HOST}
cat *.7?? >! $RUNDIR/${start_date}/RD_ATHENS04/${obs_date}_ATHENS04_data.${MM5HOST}

#	Clean up house

if ($debug > 0) then
exit (0)
endif

rm input output gts.in
rm athens04_out*
rm gts_sttnid_final.icao gts_sttnid_final athens04_data
cd $RUNDIR/${start_date}/RD_ATHENS04
echo "Now working in  $cwd"
#rm -r ${obs_date}
exit 0


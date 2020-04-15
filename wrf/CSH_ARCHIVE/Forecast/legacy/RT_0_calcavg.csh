#!/bin/csh -f
#
###############################################################################
echo  " ----------------------------------------------------------------------"
echo  " ---------------- Calculate past 24 hour mean of Salt Lake T ------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM SNOW_INIT
setenv RM "rm -rf"

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 logger -p local4.emerg -t "${SUBSYSTEM}" "Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST};
source ${CFILE}sizes.mm5sys.${MM5HOST}

#       Check usage

if      ( ${#argv} != 1 )  then
        echo "usage: $0 this_cycle"
        echo "where this_cycle is given in CCYYMMDDHH"
        exit ( 4 )
endif

#       Does the directory exist

$MustHaveDir $RUNDIR/$1/LAKE_T

#       go to working directory
cd $RUNDIR/$1/LAKE_T
echo "Now working in  $cwd"


#       Remove old file, first of all
${RM} $RUNDIR/lake_t.dat
echo $MM5HOST
set TIMET = `echo $1 |cut -b 1-10`
echo "$TIMET , -5" >! input
${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
set TIME = `cat output`
echo 'time=' $TIME
rm -f tbar.dat
$CALCAVG_EXE << enddata >lake_t.print.out
$RAWS_DATA_DIR
$TIME
enddata
#
cp tbar.dat $RUNDIR/lake_t.dat

exit

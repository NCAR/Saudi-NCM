#!/bin/csh -f

###############################################################################
echo
echo " ----------------------------------------------------------------------"
echo " ---------------- Superadiabatic removal ------------------------------"
echo "$0 $argv[*]"
echo " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM NO_SUPER
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
source ${CFILE}user.mm5sys.${MM5HOST};    
source ${CFILE}sizes.mm5sys.${MM5HOST}

#       Check usage

if      ( ${#argv} != 2 )  then
        echo "usage: $0 eta_date cycle_date"
        echo "where eta_date is the 0 h of the ETA forecast, and"
        echo "cycle_date is the 0h of the MM5 forecast"
        echo "where both are given in CCYYMMDDHH"
        exit ( 4 )
endif

set eta_date = $1
set cycle_date = $2

#	Does the directory exist

setenv  NO_SUPER  ${RUNDIR}/${this_cycle}/NO_SUPER

if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/NO_SUPER
ln -s $GEAPSTMP/NO_SUPER  $NO_SUPER
else
$MustHaveDir $NO_SUPER
endif

cd $NO_SUPER
echo "Now working in  $cwd"


#	Has this already been run?

if ( ( -e $RUNDIR/${cycle_date}/${eta_date}_REGRIDv3.${MM5HOST} ) && \
     ( -e $RUNDIR/${cycle_date}/${eta_date}_REGRIDv3_supers.${MM5HOST} ) ) then
	echo "INFO: The no_super program has already run"
	exit ( 0 )
endif

#	Go to the directory

cd $NO_SUPER
echo "Now working in  $cwd"


#	Need a REGRIDv3 file, though we should not get to here without one.

if      ( -e $RUNDIR/${cycle_date}/${eta_date}_REGRIDv3.${MM5HOST} ) then
	if ( ( -l fort.10 ) || ( -e fort.10 ) ) rm fort.10
	ln -s $RUNDIR/${cycle_date}/${eta_date}_REGRIDv3.${MM5HOST} fort.10
	set ok = $status
	if ( $ok != 0 ) then
		cp $RUNDIR/${cycle_date}/${eta_date}_REGRIDv3.${MM5HOST} fort.10
	endif
else
	echo "have to use old data, not yet implemented"
	exit ( 1 )
endif

#	Run the program

( time $NO_SUPER_EXE ) >&! no_super_print.out

#	Put output where it is visible

#mv $RUNDIR/${cycle_date}/${eta_date}_REGRIDv3.${MM5HOST} $RUNDIR/${cycle_date}/${eta_date}_REGRIDv3_supers.${MM5HOST}

rm -rf $RUNDIR/${cycle_date}/${eta_date}_REGRIDv3.${MM5HOST} 
mv fort.11 $RUNDIR/${cycle_date}/${eta_date}_REGRIDv3.${MM5HOST}
mv no_super_print.out $RUNDIR/${cycle_date}/${eta_date}_no_super_print.out
#echo `date` > $RUNDIR/${cycle_date}/${eta_date}_REGRIDv3_supers.${MM5HOST}

#	Clean up house

rm fort.10 

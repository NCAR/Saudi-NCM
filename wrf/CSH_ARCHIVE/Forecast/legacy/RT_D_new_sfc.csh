#!/bin/csh -f

###############################################################################
echo  " ----------------------------------------------------------------------"
echo  " ---------------- New surface adjustment   ----------------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM NEW_SFC
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
        echo "usage: $0 eta_date start_date"
        echo "where eta_date is the 0 h of the ETA forecast, and"
        echo "start_date is the 0h of the MM5 forecast"
        echo "where both are given in CCYYMMDDHH"
        exit ( 4 )
endif

set eta_date = $1
set start_date = $2

#	Does the directory exist

$MustHaveDir $RUNDIR/${start_date}/NEW_SFC

#       Has this already been run?

if ( ( -e $RUNDIR/${start_date}/${eta_date}_REGRIDv3.${MM5HOST} ) && \
     ( -e $RUNDIR/${start_date}/${eta_date}_REGRIDv3_old_sfc.${MM5HOST} ) ) then
        echo "INFO: The new_sfc program has already run"
        exit ( 0 )
endif

#	Go to the directory

cd $RUNDIR/${start_date}/NEW_SFC
echo "Now working in  $cwd"


#	Need a REGRIDv3 file, though we should not get to here without one.

if      ( -e $RUNDIR/${start_date}/${eta_date}_REGRIDv3.${MM5HOST} ) then
	if ( ( -l fort.10 ) || ( -e fort.10 ) ) ${RM} fort.10
	ln -s $RUNDIR/${start_date}/${eta_date}_REGRIDv3.${MM5HOST} fort.10
	set ok = $status
	if ( $ok != 0 ) then
		cp $RUNDIR/${start_date}/${eta_date}_REGRIDv3.${MM5HOST} fort.10
	endif
else
	echo "have to use old data, not yet implemented"
	exit ( 1 )
endif

#	Run the program

( time $NEW_SFC_EXE ) >&! new_sfc_print.out

#	Put output where it is visible

#mv $RUNDIR/${start_date}/${eta_date}_REGRIDv3.${MM5HOST} $RUNDIR/${start_date}/${eta_date}_REGRIDv3_old_sfc.${MM5HOST}

rm -rf $RUNDIR/${start_date}/${eta_date}_REGRIDv3.${MM5HOST} 
mv fort.11 $RUNDIR/${start_date}/${eta_date}_REGRIDv3.${MM5HOST}
mv new_sfc_print.out $RUNDIR/${start_date}/${eta_date}_new_sfc_print.out
# echo `date` > $RUNDIR/${start_date}/${eta_date}_REGRIDv3_old_sfc.${MM5HOST}

#	Clean up house

${RM} fort.10 

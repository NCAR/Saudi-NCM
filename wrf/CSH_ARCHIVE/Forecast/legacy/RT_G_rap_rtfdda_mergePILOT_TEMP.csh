#!/bin/csh -f

###############################################################################
echo  " ----------------------------------------------------------------------"
echo  " ------------- Little_r for obs QC starts  ----------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM RAP_FDDA
setenv RM "rm -rf"

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo  "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST};    
source ${CFILE}sizes.mm5sys.${MM5HOST};    
set s = $MM5HOST
set this_cycle = $1
 
#       Get to the right directory
 
if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/RAP_RTFDDA
ln -s $GEAPSTMP/RAP_RTFDDA $RUNDIR/${this_cycle}/RAP_RTFDDA
else
$MustHaveDir $RUNDIR/$this_cycle/RAP_RTFDDA
endif

cd $RUNDIR/$this_cycle/RAP_RTFDDA
echo "Now working in  $cwd"


echo "   merge PILOT and TEMP "
foreach tmpr ( qc_out_* )
    $CSH_ARCHIVE/Forecast/RT_all.obs_trim-merge.USA $tmpr  >/dev/null
    mv $tmpr.trim $tmpr
end

echo "   reformat  the qc-ed output"
foreach tmpr ( qc_out_* )
    ${EXECUTABLE_ARCHIVE}/RT_fdda_reformat.pl $tmpr  >/dev/null
end
#foreach tmpr ( qc_out_* )
#    $RD_QC_RF_EXE $tmpr  >/dev/null
#end
#echo "   Merge and sort the qc-ed output"
#foreach tmps ( *qc_obs_for_assimilation )
#   ${EXECUTABLE_ARCHIVE}/RT_fdda_qc_sort.pl $tmps
#end


#	Move the important files around

mv *_qc_obs_for_assimilation_s $RUNDIR/${this_cycle}/

if(-d $GEAPSTMP) then
rm $RUNDIR/$this_cycle/RAP_RTFDDA
$MustHaveDir $RUNDIR/$this_cycle/RAP_RTFDDA
mv $GEAPSTMP/RAP_RTFDDA/*  $RUNDIR/${this_cycle}/RAP_RTFDDA
endif


#	Clean up


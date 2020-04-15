#!/bin/csh -f

###############################################################################
echo  " ----------------------------------------------------------------------"
echo  " ------------- Little_r for obs QC starts  ----------------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM  RAP_FDDA

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

if ( ${#argv} != 3 ) then
        echo "ERROR (usage): $0 start_date end_date this_cycle"
        echo "where start_date is the initial time of the MM5 fdda"
        echo  $end_date is the final time of the MM5 fdda"
        echo "(both are in CCYYMMDDHH) "
        exit ( 1 )
endif

set start_date = $1
set end_date   = $2
set this_cycle = $3

set s = $MM5HOST

#	Get to the right directory

if(-d $GEAPSTMP) then
 if ( ! -d $GEAPSTMP/RAP_RTFDDA ) then
  $MustHaveDir $GEAPSTMP/RAP_RTFDDA
  ln -s $GEAPSTMP/RAP_RTFDDA $RUNDIR/${this_cycle}/RAP_RTFDDA
 endif
 cd $GEAPSTMP/RAP_RTFDDA
else
 $MustHaveDir $RUNDIR/$this_cycle/RAP_RTFDDA
 cd $RUNDIR/$this_cycle/RAP_RTFDDA
endif


set start_yy = `echo "${start_date}"|cut -c 1-4`
set start_mm = `echo "${start_date}"|cut -c 5-6`
set start_dd = `echo "${start_date}"|cut -c 7-8`
set start_hh = `echo "${start_date}"|cut -c 9-10`
#set start_sams = `echo "${start_date}"|cut -c 3-10`
#echo $start_sams


set end_yy = `echo "${end_date}"|cut -c 1-4`
set end_mm = `echo "${end_date}"|cut -c 5-6`
set end_dd = `echo "${end_date}"|cut -c 7-8`
set end_hh = `echo "${end_date}"|cut -c 9-10`
#set end_sams = `echo "${end_date}"|cut -c 3-10`
#echo  "d_sams

#	Bring stuff over that we need

cp ${RAP_TEMPLATE1_RTFDDA} namelist1.template
cp ${RAP_TEMPLATE2_RTFDDA} namelist2.template

#	Modify namelist

if ( -e namelist.input ) rm namelist.input
ed namelist1.template << EOF > /dev/null
g/starting_year/s//$start_yy/
g/starting_month/s//$start_mm/
g/starting_day/s//$start_dd/
g/starting_hour/s//$start_hh/
g/ending_year/s//$end_yy/
g/ending_month/s//$end_mm/
g/ending_day/s//$end_dd/
g/ending_hour/s//$end_hh/
w namelist1.input
q
EOF

#	Do we have the obs files?

## YLIU: what is this doing??? ----------
#set lss=`ls ../DECODE_SAMS/${start_sams}*_SAMS`

#rm input
#touch input
#foreach i ( $lss )
#  echo "'${i}'," >>! input
#end

#set lss=`ls ../DECODE_SAMS/${end_sams}*_SAMS`
#foreach i ( $lss )
#  echo "'${i}'," >>! input
#end

#cat namelist1.input input namelist2.template >>! namelist.input
#-----------------------------------------

 cat namelist1.input namelist2.template >>! namelist.input
if ( -e all.obs) rm all.obs
cat ../DECODE_BLP_PROF/PROF_* ../DECODE_NPN_PROF/PROF_* >! all.obs
cat ../DECODE_SATWVCD/SATWINDS.* ../DECODE_ACARS/ACARS.*all >> all.obs
cat ../DECODE_CLASS/CLASS*all ../DECODE_SAMS/*SAMS.allrange >> all.obs
cat ../DECODE_PROF/PROF_QC* >> all.obs
cat ../DECODE_GTS/*_GTS_data.${s} ../RD_RAWS/*_RAWS_data.${s} >> all.obs
cat ../RD_OKMESO/*_OKMESO_data.${MM5HOST} ../DECOD*_VAD_PROF/NIDSVAD* >> all.obs
cat ../RD_WVR/WVR.*  >> all.obs
cat ../RD_SODAR/*SODAR* >> all.obs
cat ../RD_PWIDS/*PWIDS* >> all.obs
cat ../RD_LIDARVAD/*LIDARVAD* >> all.obs
cat ../RD_MADIS/MADIS.* >> all.obs
cat ../RD_QWND/QWND.* >> all.obs
cat ../DECODE_HISFCW/HISFCW.* >> all.obs
cat ../RD_QCOUT/QCOUT* >> all.obs
cat ../RD_SPECIAL/SPECIAL* >> all.obs

## yliu -- any special obs in all.obs format 
cat /data/inputspecial/* >> all.obs

$CSH_ARCHIVE/Forecast/RT_all.obs_trim.domain.USA
mv all.obs all.obs.glob
mv all.obs.trim all.obs

if ( ( -l DATAGRID ) || ( -e DATAGRID ) ) rm DATAGRID
if ( -e ../${this_cycle}_1STGUESSMM5_DOMAIN1.${MM5HOST}) then
 ln -s ../${this_cycle}_1STGUESSMM5_DOMAIN1.${MM5HOST} DATAGRID
else if (-e ../${this_cycle}_1STGUESSETA_DOMAIN1.${MM5HOST}) then
 ln -s ../${this_cycle}_1STGUESSETA_DOMAIN1.${MM5HOST} DATAGRID
else
 echo " Error: Need first guess for QC"
 exit(0)
endif

#	Run the program

( time $RAP_RTFDDA_EXE ) >! rap_print.out

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


#       Move the important files around

if(-d $GEAPSTMP) then
cp *_qc_obs_for_assimilation_s ../
gzip all.obs.glob
endif
mv *_qc_obs_for_assimilation_s $RUNDIR/${this_cycle}/
cat rap_print.out >>  $RUNDIR/$this_cycle/${this_cycle}_rap_rtffdda_print.out

if(-d $GEAPSTMP) then
rm $RUNDIR/$this_cycle/RAP_RTFDDA
$MustHaveDir $RUNDIR/$this_cycle/RAP_RTFDDA
cp $GEAPSTMP/RAP_RTFDDA/qc_out*  $RUNDIR/${this_cycle}/RAP_RTFDDA/
cp $GEAPSTMP/RAP_RTFDDA/namelist.input $RUNDIR/${this_cycle}/RAP_RTFDDA/
cp $GEAPSTMP/RAP_RTFDDA/all.obs.glob.gz $RUNDIR/${this_cycle}/RAP_RTFDDA/
endif
 
#mv LITTLE_R_DOMAIN1 $RUNDIR/$this_cycle/${this_cycle}_DATAGRID_+analysis.FDDA.${MM5HOST}
#
#mv plot_rtfdda_print.out /production/mm5sys/$this_cycle/${this_cycle}_plot_rtfdda_print.out
#mv gmeta /production/mm5sys/$this_cycle/${this_cycle}_PLOTS.gmeta

#	Clean up


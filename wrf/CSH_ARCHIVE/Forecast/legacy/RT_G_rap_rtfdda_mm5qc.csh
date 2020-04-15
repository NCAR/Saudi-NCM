#!/bin/csh -f
## *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
## ** Copyright UCAR (c) [RAP] 1996 - 2004. All Rights Reserved.
## *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
###############################################################################
echo  " ----------------------------------------------------------------------"
echo  " ------------------------- MM5 QC starts ------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################
# set echo

set timestamp
setenv SUBSYSTEM  RAP_FDDA

## Environment

set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

#$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo  "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST};
source ${CFILE}sizes.mm5sys.${MM5HOST};

## Check Arguments

if ( ${#argv} < 5 ) then
        echo ""
        echo "ERROR (usage): $0 start_date end_date this_cycle last_cycle stage"
        echo "where start_date is the initial time of the MM5 fdda"
        echo "end_date is the final time of the MM5 fdda"
        echo "(both are in CCYYMMDDHH) "
        echo "stage is either MM5_F or MM5_P"
        echo ""
        exit ( 1 )
endif

set start_date = `echo $1 | cut -c1-10`
set end_date   = `echo $2 | cut -c1-10`
set this_cycle = `echo $3 | cut -c1-10`
set last_cycle = `echo $4 | cut -c1-10`
set stage = $5

if (stage == "MM5_F") then
   set TYPE = "_F"
else
   set TYPE = "_P+FCST"
endif

## Debug stuff
## debug=0: default (no debugging)
## debug=1: like 0 + print listing of QC program on stder
## debug=2: like 1 + run in directory RUNDIR/MM5_QC + no clean-up
## debug=3: like 2 + RUNDIR = /data/cycles/GEAPSO/0

if ( ${#argv} > 5 ) then
  set debug = $6
else
  set debug = 0
endif

# Save QC (and intermediate) files to QC_FILE directory
set save_data = 1
if ($save_data > 0) then
  set QC_FILES_DIR = $RUNDIR/QC_FILES
endif

if ($debug == 3) then
    setenv RUNDIR /data/cycles/PTMUGU/GRM
    setenv MM5HOST GRM  # testing
endif
set s = $MM5HOST

#if ($debug <=  1) then
    setenv RAP_RTFDDA RAP_RTFDDA
#else
#    setenv RAP_RTFDDA MM5_QC
#endif

## Get to the right directory

if(-d $GEAPSTMP) then
 if ( ! -d $GEAPSTMP/${RAP_RTFDDA} ) then
  $MustHaveDir $GEAPSTMP/${RAP_RTFDDA}
  ln -s $GEAPSTMP/${RAP_RTFDDA} $RUNDIR/${this_cycle}/${RAP_RTFDDA}
 endif
 cd $GEAPSTMP/${RAP_RTFDDA}
 echo "Current directory is $RUNDIR/${this_cycle}/${RAP_RTFDDA}"
else
 $MustHaveDir $RUNDIR/$this_cycle/${RAP_RTFDDA}
 cd $RUNDIR/$this_cycle/${RAP_RTFDDA}
 echo "Current directory is $RUNDIR/$this_cycle/${RAP_RTFDDA}"
endif

setenv RAP_MM5QC_EXE ${EXECUTABLE_ARCHIVE}/DAProg_Decoded.exe


## Bring stuff over that we need

cp ${CONSTANT_FILES}/MM5QC.namelist.decoded.template namelist.decoded
cp ${CONSTANT_FILES}/errtable_* .

if (-e ${CONSTANT_FILES}/MM53.6/LANDUSE.TBL) then
    cp ${CONSTANT_FILES}/MM53.6/LANDUSE.TBL .
else
    cp ${SOURCE}/RT_MM5/RT_3.5/Run/LANDUSE.TBL .
endif

## Do we have the obs files?

set obs = all.obs

#if ( -e all.obs) rm all.obs
#cat ../DECODE_BLP_PROF/PROF_* ../DECODE_NPN_PROF/PROF_* ../DECODE_SATWVCD/SATWINDS.* ../DECODE_ACARS/ACARS.*all ../DECODE_CLASS/CLASS*all ../DECODE_SAMS/*SAMS.allrange ../DECODE_PROF/PROF_QC*  ../DECODE_GTS/*_GTS_data.${s} ../RD_RAWS/*_RAWS_data.${s} ../RD_OKMESO/*_OKMESO_data.${MM5HOST} ../DECOD*_VAD_PROF/NIDSVAD* ../RD_WVR/WVR.* >! all.obs
cat ../DECODE_BLP_PROF/PROF_* ../DECODE_NPN_PROF/PROF_* >! all.obs
cat ../DECODE_SATWVCD/SATWINDS.* ../DECODE_ACARS/ACARS.*all >> all.obs
cat ../DECODE_CLASS/CLASS*all ../DECODE_SAMS/*SAMS.allrange >> all.obs
cat ../DECODE_PROF/PROF_QC* >> all.obs
cat ../DECODE_GTS/*_GTS_data.* ../RD_RAWS/*_RAWS_data.* >> all.obs
cat ../RD_OKMESO/*_OKMESO_data.*
cat ../DECOD*_VAD_PROF/NIDSVAD* >> all.obs
cat ../RD_WVR/WVR.*  >> all.obs
cat ../RD_SODAR/*SODAR* >> all.obs
cat ../RD_PWIDS/*PWIDS* >> all.obs
cat ../RD_LIDARVAD/*LIDARVAD* >> all.obs
cat ../RD_MADIS/MADIS.* >> all.obs
cat ../RD_NIDSVAD/NIDSVAD.* >> all.obs
cat ../DECODE_HISFCW/HISFCW* >> all.obs
cat ../RD_QWND/QWND* >> all.obs
cat ../RD_WMO/WMO* >> all.obs
cat ../RD_LIDPROF/*LIDPROF*data >> all.obs
cat ../RD_MICROSTEPS/*MICROSTEPS* >> all.obs
cat ../RD_DTE/*DTE* >> all.obs
cat ../RD_DCNET/DCNET.* >> all.obs
cat ../RD_QCOUT/QCOUT* >> all.obs
cat ../RD_SPECIAL/SPECIAL* >> all.obs
cat ../RD_TAMDAR/TAMDAR* >> all.obs
cat ../RD_ADP/*_ADP_data.* >> all.obs
ls ../RD_AFCCC/afccc_*.decoded
cat ../RD_AFCCC/afccc_*.decoded >> all.obs
cat ../RD_ALLOBS/ALLOBS* >> all.obs

## yliu -- any special obs in obs format
#cat /data/inputspecial/* >> ${obs}

#$EXECUTABLE_ARCHIVE/RT_all.obs_trim.domain.USA
#$CSH_ARCHIVE/Forecast/RT_all.obs_trim.domain.USA
#mv ${obs} ${obs}.glob
#mv ${obs}.trim ${obs}

if (! -e ${obs}) then
    set obs = ${obs}.glob
endif

if (-e ${obs}.gz) gunzip ${obs}.gz


## Stop in absence of observations

if (! -e $obs) then
  echo "Cannot find observations file $RUNDIR/$this_cycle/${RAP_RTFDDA}/$obs"
  exit (1)
endif

if ($save_data > 0) then
   if (! -d $QC_FILES_DIR) then
      mkdir $QC_FILES_DIR
      echo "Making QC_FILES_DIR at $QC_FILES_DIR"
   endif

#   if (! -d /data/cycles/vandenb/ETA_NEWQ/${this_cycle}) then
#      mkdir /data/cycles/vandenb/ETA_NEWQ/${this_cycle}
#   endif
endif

#   if ($save_data > 0) then
#   cp $obs /data/cycles/vandenb/ETA_NEWQ/${this_cycle}/.
#   endif

## Look for the first guess in the same directory directory

ls -l ../${this_cycle}_MMINPUT_DOMAIN1.${s}

if    (-e ../${last_cycle}_MMOUTPUT_DOMAIN1) then
set mm5 = ../${last_cycle}_MMOUTPUT_DOMAIN1

## Otherwise Look for the first guess in the previous cycle directory

else if (-e ../../${last_cycle}/${last_cycle}_MMOUTPUT_DOMAIN1.${s}_P+FCST)then
  set mm5 = ../../${last_cycle}/${last_cycle}_MMOUTPUT_DOMAIN1.${s}_P+FCST
else if (-e ../../${last_cycle}/${last_cycle}_MMOUTPUT_DOMAIN1.${s}_F) then
  set mm5 = ../../${last_cycle}/${last_cycle}_MMOUTPUT_DOMAIN1.${s}_F
else if (-e ../${this_cycle}_MMINPUT_DOMAIN1.${s}) then
  set mm5 = ../${this_cycle}_MMINPUT_DOMAIN1.${s}
else

## Stop in absence of first guess

  echo "Cannot find first guess file $RUNDIR/$this_cycle/${last_cycle}_MMOUTPUT_DOMAIN1"
  exit (1)
endif

#   if ($save_data > 0) then
#   cp $mm5 /data/cycles/vandenb/ETA_NEWQ/${this_cycle}/.
#   endif

## Cleaning before processing

if ((-l rap_mm5qc_print.out) || (-e rap_mm5qc_print.out)) then
     rm rap_mm5qc_print.out
endif

if ((-l all.obs_qc-ed_mm5qc) || (-e all.obs_qc-ed_mm5qc)) then
     rm all.obs_qc-ed_mm5qc
endif

## Get the MM5 domain limits

if (-e latlon.txt) rm latlon.txt

$EXECUTABLE_ARCHIVE/latlon.exe -i $mm5

#   if ($save_data > 0) then
#   cp latlon.txt  /data/cycles/vandenb/ETA_NEWQ/${this_cycle}/.
#   cp LANDUSE.TBL /data/cycles/vandenb/ETA_NEWQ/${this_cycle}/.
#   cp errtable_*.txt   /data/cycles/vandenb/ETA_NEWQ/${this_cycle}/.
#   cp namelist.decoded /data/cycles/vandenb/ETA_NEWQ/${this_cycle}/.
#   endif

##  Split obs by [t-30mn,t+30mn] time intervals, t = start_date,..., end_date

echo "Split file ${obs} into hourly files ([t-30mn,t+30mn] time-intervals)."
$CSH_ARCHIVE/Forecast/RT_all.obs_trim-merge.USA ${obs} 30 latlon.txt >/dev/null

## Select the AVN forecast error file based on the mid latitude of the MM5 dom

if (-e latlon.txt) then
   set latlon = `cat latlon.txt`
else
   echo "Cannot find file latlon.txt, assign mid-latitude AVN forecast error."
   set latlon = "45 45 45 45 "
endif

@ latc = $latlon[1] + $latlon[2]
@ latc = $latc / 2

set  sign = `echo $latc |cut -c1-1`
if ($sign =~ -) then
set  latc = `echo $latc |cut -c2-`
endif

if ($latc !~ [0-9]*) then
    echo "Unknown latitude $latc, assign mid-latitude AVN forecast error."
    set domlat = "midlat"
else

if ($latc < 10) then
    set domlat = "equator"
else if ($latc < 30) then
    set domlat = "tropics"
else if ($latc < 50) then
    set domlat = "midlat"
else if ($latc < 70) then
    set domlat = "highlat"
else if ($latc < 90) then
    set domlat = "polar"
else
    echo "Unknown latitude  $latc, assign mid-latitude AVN forecast error."
    set domlat = "midlat"
endif
endif

if (-e errtable_avnfct_${domlat}.r3dv.txt) then
 echo "The latitude of MM5 domain is around ${latc} N/S, will use AVN error file:"
 echo "errtable_avnfct_${domlat}.r3dv.txt"
 ln -s -f errtable_avnfct_${domlat}.r3dv.txt errtable_avnfct.r3dv.txt
else
    echo "File errtable_avnfct_${domlat}.r3dv.txt is missing, cannot proceed!"
    exit (2)
endif

## Procees obs by [t-30mn, t+30mn] time intevals, t = start_date,..., end_date

set ccyymmddhh  = $start_date

while ($ccyymmddhh <= $end_date)

echo "Processing  $ccyymmddhh"

## Run the QC program

   if (! -e ${ccyymmddhh}.${obs}) then
       echo "Cannot find obs file ${ccyymmddhh}.${obs}, skip"
   else

#       if ($save_data > 0) then
#       cp ${ccyymmddhh}.${obs} /data/cycles/vandenb/ETA_NEWQ/${this_cycle}/.
#       endif

       if ($debug < 1) then
       echo "$RAP_MM5QC_EXE -mm5 $mm5 -obs ${ccyymmddhh}.$obs -date $ccyymmddhh"
             $RAP_MM5QC_EXE -mm5 $mm5 -obs ${ccyymmddhh}.$obs -date $ccyymmddhh  -log rap_mm5qc_print.out
       else
            $RAP_MM5QC_EXE -mm5 $mm5 -obs ${ccyymmddhh}.$obs -date $ccyymmddhh
       endif

## Check exit status

       if ((! -e ${ccyymmddhh}.${obs}_mm5qc) || (-z  ${ccyymmddhh}.${obs}_mm5qc)) then
           echo "Error processing obs file ${ccyymmddhh}.${obs}"
       else


## Cat the QCed hourly data into a single file

           echo "cat ${ccyymmddhh}.${obs}_mm5qc >>! all.obs_qc-ed_mm5qc"
                 cat ${ccyymmddhh}.${obs}_mm5qc >>! all.obs_qc-ed_mm5qc
       endif

   endif

## Save listing

   if (($debug < 1) && (-e rap_mm5qc_print.out)) then
    cat rap_mm5qc_print.out >> $RUNDIR/$this_cycle/${this_cycle}_rap_rtffdda_print.out
#   if ($save_data > 0) then
#   cat rap_mm5qc_print.out >>  /data/cycles/vandenb/ETA_NEWQ/${this_cycle}/${this_cycle}_rap_rtffdda_print.out
#   endif
   endif

## Increment date

   set ccyymmddhh = `$EXECUTABLE_ARCHIVE/geth_newdate.exe  $ccyymmddhh +1`
#  echo "Processing  $ccyymmddhh"

end

## Clean-up

        mv ${obs} ${obs}.glob
        rm *.${obs}

## Split the obs output file by [t, t+59mn] time intervals,t = start_date,..., end_date

echo "Split file ${obs}_mm5qc into hourly files ([t,t+59mn] time-intervals)."

$CSH_ARCHIVE/Forecast/RT_all.obs_trim-merge.USA ${obs}_qc-ed_mm5qc 59 latlon.txt >  /dev/null


## Rename the output files and move them around

echo ""
#echo "Output files: "

set ccyymmddhh  = $start_date

## Loop from start_date to end_date run

while ($ccyymmddhh <= $end_date)

## Break down the date
   set ccyy = `echo "${ccyymmddhh}"|cut -c 1-4`
   set mm   = `echo "${ccyymmddhh}"|cut -c 5-6`
   set dd   = `echo "${ccyymmddhh}"|cut -c 7-8`
   set hh   = `echo "${ccyymmddhh}"|cut -c 9-10`
   set mn   = "00"
   set ss   = "00"

## Rename the files

   if (-e ${ccyymmddhh}.${obs}_qc-ed_mm5qc) then
#      echo "${ccyymmddhh}.${obs}_mm5qc"

## Reformat the QCed data
       echo "Reformat file ${ccyymmddhh}.${obs}_qc-ed_mm5qc."
       set qc_out = qc_out_${ccyy}-${mm}-${dd}_${hh}:${mn}:${ss}.0000
       mv ${ccyymmddhh}.${obs}_qc-ed_mm5qc $qc_out
       $CSH_ARCHIVE/Forecast/RT_fdda_reformat.pl $qc_out >/dev/null

       echo "Output file qcmm5_out.${ccyy}-${mm}-${dd}_${hh}:${mn}:${ss}.0000 is ok"

   else
       echo "Output file qcmm5_out.${ccyy}-${mm}-${dd}_${hh}:${mn}:${ss}.0000 is missing"
   endif

## Increment time and break down date

   set ccyymmddhh = `$EXECUTABLE_ARCHIVE/geth_newdate.exe  $ccyymmddhh +1`
#  echo "Processing  $ccyymmddhh"

end

## Save the files for GEAPS

if ($debug < 1) then

if(-d $GEAPSTMP) then
    cp *_s ../
    rm $RUNDIR/$this_cycle/${RAP_RTFDDA}
    $MustHaveDir $RUNDIR/$this_cycle/${RAP_RTFDDA}
    cp $GEAPSTMP/${RAP_RTFDDA}/qc_out*       $RUNDIR/${this_cycle}/${RAP_RTFDDA}/
#   cp $GEAPSTMP/${RAP_RTFDDA}/namelist.input   $RUNDIR/${this_cycle}/${RAP_RTFDDA}/
    cp $GEAPSTMP/${RAP_RTFDDA}/namelist.decoded $RUNDIR/${this_cycle}/${RAP_RTFDDA}/
    gzip all.obs.glob
    cp $GEAPSTMP/${RAP_RTFDDA}/all.obs.glob.gz  $RUNDIR/${this_cycle}/${RAP_RTFDDA}/
    cp $GEAPSTMP/${RAP_RTFDDA}/*.all.obs_plotobs  $RUNDIR/${this_cycle}/${RAP_RTFDDA}/
    cp $GEAPSTMP/${RAP_RTFDDA}/*.txt $RUNDIR/${this_cycle}/${RAP_RTFDDA}/
    mv all.obs.glob.gz all.obs.glob${TYPE}.gz
endif
    cp *_s $RUNDIR/${this_cycle}

endif

echo ""

#	Clean up

   if ($save_data > 0) then
     cp *_qc_obs_for_assimilation_s $QC_FILES_DIR/.
     tar -cf ${this_cycle}_qc_out.tar qc_out* 
     gzip ${this_cycle}_qc_out.tar
     mv ${this_cycle}_qc_out.tar.gz ${this_cycle}_qc_out.tar${TYPE}.gz
     if ($TYPE == "_F") then
       cp ${this_cycle}_qc_out.tar_F.gz $QC_FILES_DIR/${this_cycle}_qc_out.tar.gz
       cp all.obs.glob_F.gz $QC_FILES_DIR/${this_cycle}_all.obs.gz
     endif
#     cp all.obs_qc-ed_mm5qc  /data/cycles/vandenb/ETA_NEWQ/${this_cycle}/.
   endif

if ($debug < 1) then
  rm oberr.*.txt
  rm *${obs}_mm5qc
  rm fort.*
endif

rm *3dvar *err *model *_s

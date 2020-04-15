#!/bin/tcsh -f
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- PROF decoder starts  --------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM PROF_FDDA

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
#set echo

#	Check usage

if ( ${#argv} == 2 ) then
    set start_date = $1
    set jdayi = $2
    @ jdaym1i = $jdayi - 1
else
    echo "usage: $0 start_date Julien_day "
    echo "where start_date is CCYYMMDDHH"
    echo  "d Julien_day is number of day"
    exit ( 4 )
endif

if(-d $GEAPSTMP) then
 if( ! -d $GEAPSTMP/DECODE_PROF) then
  $MustHaveDir $GEAPSTMP/DECODE_PROF
  ln -s $GEAPSTMP/DECODE_PROF $RUNDIR/${start_date}/DECODE_PROF
 endif 
else
$MustHaveDir $RUNDIR/${start_date}/DECODE_PROF
endif

cd ${RUNDIR}/${start_date}/DECODE_PROF
echo "Now working in  $cwd"

set yy = `echo $start_date | cut -b 3-4`

if($jdayi < 100) then
  set jday = 0$jdayi
  set jdaym1 = 0$jdaym1i
else if($jdayi < 10) then
  set jday = 00$jdayi
  set jdaym1 = 00$jdaym1i
else
  set jday = $jdayi
  set jdaym1 = $jdaym1i
endif

foreach range (DPG YPG WSMR)

rm prof-924*

if ($range == DPG) then
cat ${PROF_DATA_DPG_DIRA}/W${yy}${jdaym1}.CNS ${PROF_DATA_DPG_DIRA}/W${yy}${jday}.CNS >> prof-924aW
cat ${PROF_DATA_DPG_DIRA}/T${yy}${jdaym1}.CNS ${PROF_DATA_DPG_DIRA}/T${yy}${jday}.CNS >> prof-924aT
cat ${PROF_DATA_DPG_DIRB}/W${yy}${jdaym1}.CNS ${PROF_DATA_DPG_DIRB}/W${yy}${jday}.CNS >> prof-924bW
cat ${PROF_DATA_DPG_DIRB}/T${yy}${jdaym1}.CNS ${PROF_DATA_DPG_DIRB}/T${yy}${jday}.CNS >> prof-924bT
else if ($range == YPG) then
cat ${PROF_DATA_YPG_DIRA}/W${yy}${jdaym1}.CNS ${PROF_DATA_YPG_DIRA}/W${yy}${jday}.CNS >> prof-924aW
touch prof-924aT
touch  prof-924bW
touch prof-924bT
else if ($range == WSMR) then
touch prof-924aW
touch prof-924aT
touch  prof-924bW
touch prof-924bT
#cat ${PROF_DATA_WSMR_DIRA}/W${yy}${jdaym1}.CNS ${PROF_DATA_WSMR_DIRA}/W${yy}${jday}.CNS >> prof-924aW
endif


ln -f -s prof-924aW fort.10
ln -f -s prof-924aT fort.20
${RD_PROF_EXE}  > print.out.$range
mv fort.50 PROF_QC_${jday}_${range}.a

ln -f -s prof-924bW fort.10
ln -f -s prof-924bT fort.20
${RD_PROF_EXE}  >> print.out.$range
mv fort.50 PROF_QC_${jday}_${range}.b

echo "PROFILE DATA for $range is processed"

end
exit 0

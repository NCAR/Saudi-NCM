#!/bin/tcsh -f
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ----------- SAT W & V decoder starts  --------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM SATWV_FDDA

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM}" "Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.$MM5HOST;    
#set echo

#	Check usage

# sat data filename is the script input - Wyymmddhh00.cd or
#  Wyymmddhh00.wv

if ( ${#argv} == 3 ) then
    set start_date = $1
    set obs_date = $2
    set jday = $3
else
    echo "usage: $0 start_date obs_date jday"
    echo "where start_date is CCYYMMDDHH"
    exit ( 4 )
endif

if(-d $GEAPSTMP) then
 if ( ! -d $GEAPSTMP/DECODE_SATWVCD) then
  $MustHaveDir $GEAPSTMP/DECODE_SATWVCD
  ln -s $GEAPSTMP/DECODE_SATWVCD $RUNDIR/${start_date}/DECODE_SATWVCD
 endif
else
$MustHaveDir $RUNDIR/${start_date}/DECODE_SATWVCD
endif

cd ${RUNDIR}/${start_date}/DECODE_SATWVCD
echo "Now working in  $cwd"
rm W* E* GW* GE*

set satwv_date = `echo "${obs_date}" | cut -c 3-10`

set hh = `echo "${obs_date}" | cut -c 9-10`
set satwv_jdayhh = $jday$hh
echo "INFO: DATES=$satwv_date"

cp ${SATWV_DATA_DIR}/W${satwv_date}*.* .
cp ${SATWV_DATA_DIR_1H}/W${satwv_date}*.* .
cp ${SATWV_DATA_DIR}/E${satwv_date}*.* .
cp ${SATWV_DATA_DIR_1H}/E${satwv_date}*.* .

cp ${SATWV_DATA_DIR}/GEwinds${satwv_jdayhh}.txt .
cp ${SATWV_DATA_DIR}/GWwinds${satwv_jdayhh}.txt .

touch aacd
foreach sat (*cd *wv *vi) 
echo $sat
${EXECUTABLE_ARCHIVE}/rd_satwinds.exe << END >wvcd.log
$sat
END
if($sat != "aacd") then
mv fort.50 $RUNDIR/${start_date}/DECODE_SATWVCD/SATWINDS.${obs_date}_$sat.1h
endif
end

touch aatxt
foreach sat (*txt)
echo $sat
${EXECUTABLE_ARCHIVE}/rd_satwinds_wwb.exe << END >wwb.log
$sat
END
if($sat != "aatxt") then
mv fort.50 $RUNDIR/${start_date}/DECODE_SATWVCD/SATWINDS.${obs_date}_$sat.wwb
endif
end

set NOW=`date -u`
echo '   *** All done:  '$NOW' ***'
exit 0

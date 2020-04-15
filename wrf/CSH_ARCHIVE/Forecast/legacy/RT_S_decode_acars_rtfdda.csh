#!/bin/tcsh -f
#
###############################################################################
echo
echo  " ---------------- ACARS decoder starts  --------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM ACARS_FDDA

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

if ( ${#argv} == 3 ) then
    set start_date = $1
    set jdayi = $2
    set obs_date = $3
else
    echo "usage: $0 start_date Julien_day obs_date"
    echo "where start_date and  obs_date are CCYYMMDDHH"
    echo  "d Julien_day is number of day"
    exit ( 4 )
endif

if(-d $GEAPSTMP) then
 if( ! -d $GEAPSTMP/DECODE_ACARS ) then
  $MustHaveDir $GEAPSTMP/DECODE_ACARS
  ln -s $GEAPSTMP/DECODE_ACARS $RUNDIR/${start_date}/DECODE_ACARS 
 endif 
else
$MustHaveDir $RUNDIR/${start_date}/DECODE_ACARS
endif
cd ${RUNDIR}/${start_date}/DECODE_ACARS
echo "Now working in  $cwd"

set ccyy = `echo $obs_date | cut -b 1-4`
set mm = `echo $obs_date | cut -b 5-6`
set dd = `echo $obs_date | cut -b 7-8`
set hh = `echo $obs_date | cut -b 9-10`
set hhc = `echo $start_date | cut -b 9-10`


foreach flight (`ls ${ACARS_DATA_DIR}/$obs_date*`) 

cp $flight .
ls *cdf >input
${RD_ACARS_NETCDF_1H_EXE}  < input >> print.out.acars
rm *cdf
end

cat acars* >> $obs_date.acars
cat $obs_date.acars >> ACARS.time.clean.all
$CSH_ARCHIVE/Forecast/RT_all.obs_trim.domain.USA ACARS.time.clean.all
mv ACARS.time.clean.all.trim ACARS.time.clean.all
rm acars*

echo "1 hourly ACARS is processed"


echo ${ACARS_CDF_DATA_DIR}/$ccyy$mm$dd/$obs_date 
foreach flight (`ls ${ACARS_CDF_DATA_DIR}/$ccyy$jdayi/$ccyy$jdayi$hh*; ls ${ACARS_CDF_DATA_DIR}/$ccyy$mm$dd/$obs_date*`)

echo $flight
cp $flight .
ls *cdf *nc >input
${RD_ACARS_NETCDF_EXE}  < input >> print.out.acars
rm *cdf *nc
end

cat acars* >> $obs_date.acars
cat $obs_date.acars >> ACARS.time.clean.all
$CSH_ARCHIVE/Forecast/RT_all.obs_trim.domain.USA ACARS.time.clean.all
mv ACARS.time.clean.all.trim ACARS.time.clean.all
rm acars*

echo "10 minutes ACARS is processed"

exit 0

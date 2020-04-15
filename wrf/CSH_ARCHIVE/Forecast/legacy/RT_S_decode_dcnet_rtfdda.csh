#!/bin/csh -f
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- DCNET decoder starts  ------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM DCNET_FDDA

#	Check usage

if ( ${#argv} == 2 ) then
    set cycle_date = $1
    set obs_date = $2
else
    echo "usage: $0 cycle_date obs_date"
    echo "where cycle_date obs_date are CCYYMMDDHH"
    exit ( 4 )
endif

set debug = 0  # 0=real-time, 1=specify work dir+printout on stderrout


#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

if ($debug > 0) setenv MM5HOST GRM

$CheckConfigFiles
source ${CFILE}user.mm5sys.${MM5HOST};    


if ($debug > 0) then
set RUNDIR = "/data/cycles/VPGWS/GRM"
if (! -d $RUNDIR/${cycle_date}) mkdir -p $RUNDIR/${cycle_date}
if (! -d $RUNDIR/${cycle_date}/DECODE_DCNET) mkdir -p $RUNDIR/${cycle_date}/DECODE_DCNET
endif

if (-d $GEAPSTMP) then
  $MustHaveDir $GEAPSTMP/DECODE_DCNET
  ln -s $GEAPSTMP/DECODE_DCNET $RUNDIR/${cycle_date}/DECODE_DCNET
else
  $MustHaveDir $RUNDIR/${cycle_date}/DECODE_DCNET
endif

cd ${RUNDIR}/${cycle_date}/DECODE_DCNET
echo "Now working in  $cwd"

rm -rf 20*.txt

echo "INFO: DATES=$cycle_date"

########## HSFW_DATA_DIR

#setenv DCNET_DATA_DIR /data/input/DC/surface

 rm *.txt
 cp ${DCNET_DATA_DIR}/${obs_date}*.txt .
 echo  ${DCNET_DATA_DIR}
 foreach dcfile (`ls *.txt`) 
  set want_date = `echo $dcfile |cut -c 1-12 `
  echo $want_date
# Program runs with the form Read_DCnet.exe CCYYMMDD
    ${EXECUTABLE_ARCHIVE}/Read_DCnet.exe ${want_date}  >> print.out
    if (-e DCNET_${obs_date}) then
       cat DCNET_${obs_date} >> DCNET.${cycle_date}.all_hour
       rm DCNET_${obs_date}
    endif
 end

set lss=`ls DCNET_*`

#rm -rf 20*.txt

echo "${SUBSYSTEM} -- CREATED -> DCNET DATA "

exit 0

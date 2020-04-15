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
source ${CFILE}user.mm5sys.${MM5HOST};    
#set echo

#	Check usage

# sat data filename is the script input - Wyymmddhh00.cd or
#  Wyymmddhh00.wv

if ( ${#argv} == 2 ) then
    set start_date = $1
    set obs_date = $2
else
    echo "usage: $0 start_date obs_date"
    echo "where start_date is CCYYMMDDHH"
    exit ( 4 )
endif

$MustHaveDir $RUNDIR/${start_date}/DECODE_SATWV
$MustHaveDir $RUNDIR/${start_date}/DECODE_SATWV/${obs_date}
cd ${RUNDIR}/${start_date}/DECODE_SATWV/${obs_date}
echo "Now working in  $cwd"

set satwv_date = `echo "${obs_date}" | cut -c 3-10`
echo "INFO: DATES=$satwv_date"

cp ${SATWV_DATA_DIR}/W${satwv_date}*.wv .
cp ${SATWV_DATA_DIR}/W${satwv_date}*.cd .

touch aacd
foreach sat (*cd *wv) 
rm -rf outdata
${EXECUTABLE_ARCHIVE}/satwv << END >wvcd.log
$sat
END
if($sat != "aacd") then
mv outdata $RUNDIR/${start_date}/${obs_date}_$sat
endif
end

#rm  $RUNDIR/${start_date}/DECODE_SATWV/${obs_date}/*cd 
#rm  $RUNDIR/${start_date}/DECODE_SATWV/${obs_date}/*wv 

set NOW=`date -u`
echo '   *** All done:  '$NOW' ***'
exit 0

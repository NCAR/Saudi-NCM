#!/bin/csh -f
#
###############################################################################
echo 
echo  " ----------------------------------------------------------------------"
echo  " ---------------- GTS decoder starts  ---------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################
#

#set echo
set timestamp
setenv SUBSYSTEM GTS_FDDA

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "  ${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST};

setenv USE_ONE_HOUR FALSE

#	Check usage

if ( ${#argv} == 2 ) then
    set start_date = $1
    set obs_date   = $2
else
    echo "usage: $0 start_date obs_date"
    echo "where start_date is CCYYMMDD"
    echo "obs_date is CCYYMMDDHH"
    exit ( 4 )
endif

#	Does the directory exist

$MustHaveDir $RUNDIR/${start_date}

##################################################################
$MustHaveDir $RUNDIR/${start_date}/DECODE_GTS
if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/DECODE_GTS
$MustHaveDir $GEAPSTMP/DECODE_GTS/${obs_date}
ln -s $GEAPSTMP/DECODE_GTS/${obs_date} $RUNDIR/${start_date}/DECODE_GTS/${obs_date}
else
$MustHaveDir $RUNDIR/${start_date}/DECODE_GTS/${obs_date}
endif

#	Have we already done this
## Skip this check, since it  needs to be re-run at each hour... as new obs arrive
##if ( -e $RUNDIR/${start_date}/${obs_date}_GTS_data.${MM5HOST} ) then
##	echo "INFO: already finished the GTS decode"
##	exit ( 0 )
##endif

#	Go to the directory

cd $RUNDIR/${start_date}/DECODE_GTS/${obs_date}
echo "Now working in  $cwd"

#	Get the station ID files

if ( ( -l gts_sttnid_final      ) || ( -e gts_sttnid_final      ) ) rm gts_sttnid_final
echo "INFO: Linking $GTS_STTNID to current directory `pwd`"
ln -s $GTS_STTNID .
set ok = $status
if ( $ok != 0 ) then
    cp $GTS_STTNID .
endif

if ( ( -l gts_sttnid_final.icao ) || ( -e gts_sttnid_final.icao ) ) rm gts_sttnid_final.icao
echo "Linking $GTS_ICAO to current directory `pwd`"
ln -s $GTS_ICAO .
set ok = $status
if ( $ok != 0 ) then
    cp $GTS_ICAO .
endif

#	Get the GTS data over here

if ( -e gts_data ) rm gts_data
set date = ` echo $obs_date `
echo "Using GTS file $GTS_DATA_DIR/${date}.wmo..."
cat $GTS_DATA_DIR/${date}*.wmo >>! gts_data

cat /data/input/greece/surface/${date}*synop* >>! gts_data
cat /data/input/greece/surface/${date}*temp* >>! gts_data

#	Run the program

#echo "$obs_date , 3" >! input
#${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
#set cutoff = `cat output`
set cutoff = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe $obs_date 3`

echo "$cutoff" >! gts.in
#( time $GTS_DECODER_EXE ) < gts.in >& /dev/null
 ( time $GTS_DECODER_EXE ) < gts.in >& gts_decoder.print.out

#	Put output where it is visible

if ( -e $RUNDIR/DECODE_GTS/${start_date}/${obs_date}_GTS_data.${MM5HOST} ) rm $RUNDIR/DECODE_GTS/${start_date}/${obs_date}_GTS_data.${MM5HOST}
cat *.7?? >! ../${obs_date}_GTS_data.${MM5HOST}
cat *.7?? >! $RUNDIR/${start_date}/DECODE_GTS/${obs_date}_GTS_data.${MM5HOST}

#	Clean up house

rm input output gts.in
rm gts_out*
rm gts_sttnid_final.icao gts_sttnid_final gts_data
cd $RUNDIR/${start_date}/DECODE_GTS
echo "Now working in  $cwd"
#rm -r ${obs_date}
exit 0


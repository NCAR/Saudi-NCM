#!/bin/csh -f
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- GTS UAE decoder starts  ---------------------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################
#

#set echo
set timestamp
setenv SUBSYSTEM GTS_FDDA

set debug = 0
#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "  ${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (-1)
endif
source ${CFILE}user.mm5sys.${MM5HOST};    

setenv USE_ONE_HOUR FALSE

#	Check usage

if ( ${#argv} == 2 ) then
    echo
    echo "$0 $argv[*]"
    set cycle_time = $1
    set obs_time   = $2
else
    echo
    echo "usage: $0 cycle_time obs_time"
    echo "where cycle_time is CCYYMMDD"
    echo "obs_time is CCYYMMDDHH"
    exit ( -2 )
endif

echo
echo "Decode GTS UAE obs valid around $obs_time for cycle $cycle_time"

#	Does the directory exist

setenv GTS_UAE_DATA_DIR /raid/input/uae_gts

if ($debug > 0) then
  setenv GTS_UAE_DATA_DIR /raid/input/uae_gts
  set RUNDIR = `pwd`
  $MustHaveDir $RUNDIR/DECODE_GTS_UAE
  cd $RUNDIR/DECODE_GTS_UAE
else
  $MustHaveDir $RUNDIR/${cycle_time}
  if(-d $GEAPSTMP) then
    $MustHaveDir $GEAPSTMP/DECODE_GTS_UAE
    ln -s $GEAPSTMP/DECODE_GTS_UAE/ $RUNDIR/${cycle_time}/DECODE_GTS_UAE
    $MustHaveDir $GEAPSTMP/DECODE_GTS_UAE/${obs_time}
    #ln -s $GEAPSTMP/DECODE_GTS_UAE/${obs_time} $RUNDIR/${cycle_time}/DECODE_GTS_UAE/${obs_time}
  else
    $MustHaveDir $RUNDIR/${cycle_time}/DECODE_GTS_UAE
    $MustHaveDir $RUNDIR/${cycle_time}/DECODE_GTS_UAE/${obs_time}
  endif

  #	Have we already done this
  ## Skip this check, since it  needs to be re-run at each hour... as new obs arrive
  ##if ( -e $RUNDIR/${cycle_time}/${obs_time}_GTS_UAE_data.${MM5HOST} ) then
  ##	echo "INFO: already finished the GTS decode"
  ##	exit ( 0 ) 
  ##endif

  #	Go to the directory
  cd $RUNDIR/${cycle_time}/DECODE_GTS_UAE/${obs_time}
endif

echo
echo "Data directory is $GTS_UAE_DATA_DIR"
echo "Work directory is $RUNDIR/${cycle_time}/DECODE_GTS_UAE/${obs_time}"
echo

#	Get the station ID files

if ( ( -l gts_sttnid_final      ) || ( -e gts_sttnid_final      ) ) rm gts_sttnid_final
echo "INFO: Linking $GTS_STTNID"
echo "To directory: `pwd`"
ln -s $GTS_STTNID .
set ok = $status
if ( $ok != 0 ) then
    cp $GTS_STTNID .
endif

if ( ( -l gts_sttnid_final.icao ) || ( -e gts_sttnid_final.icao ) ) rm gts_sttnid_final.icao
echo "INFO: Linking $GTS_ICAO"
echo "To directory  `pwd`"
ln -s $GTS_ICAO .
set ok = $status
if ( $ok != 0 ) then
    cp $GTS_ICAO .
endif

#	look for data valid at the requested hour

set date = ` echo $obs_time |cut -c3-10`

echo
echo "Using GTS files:"
ls -1 $GTS_UAE_DATA_DIR/gts_${date}*

set ok = $status
if ( $ok != 0 ) then
    echo
    echo "Cannot find in $GTS_UAE_DATA_DIR a single file valid at ${date}" 
    echo
    exit -3
endif

#	Get the GTS data over here

if ( -e gts_data ) rm gts_data
echo "cat $GTS_UAE_DATA_DIR/gts_${date}* >>! gts_data"
cat $GTS_UAE_DATA_DIR/gts_${date}* >>! gts_data

#cat /data/input/greece/surface/${date}*synop* >>! gts_data
#cat /data/input/greece/surface/${date}*temp* >>! gts_data

#	Run the program

echo "$obs_time , 3" >! input
$EXECUTABLE_ARCHIVE/advance_cymdh < input >! output
set cutoff = `cat output`
echo "$cutoff" >! gts.in
#( time $GTS_DECODER_EXE ) < gts.in >& /dev/null
 ( time $GTS_DECODER_EXE ) < gts.in >& gts_uae_decoder.print

#	Put output where it is visible

if ($debug > 0) then

if ( -e $RUNDIR/DECODE_GTS_UAE/${obs_time}_GTS_UAE_data.${MM5HOST} ) then
     rm $RUNDIR/DECODE_GTS_UAE/${obs_time}_GTS_UAE_data.${MM5HOST}
endif

cat *.7?? >! ../${obs_time}_GTS_UAE_data.${MM5HOST}

else
if ( -e $RUNDIR/DECODE_GTS_UAE/${cycle_time}/${obs_time}_GTS_UAE_data.${MM5HOST} )   rm $RUNDIR/DECODE_GTS_UAE/${cycle_time}/${obs_time}_GTS_UAE_data.${MM5HOST}
cat *.7?? >! ../${obs_time}_GTS_UAE_data.${MM5HOST}
cat *.7?? >! $RUNDIR/${cycle_time}/DECODE_GTS_UAE/${obs_time}_GTS_UAE_data.${MM5HOST}

set f = "$RUNDIR/${cycle_time}/DECODE_GTS_UAE/${obs_time}_GTS_UAE_data.${MM5HOST}"

if (! -e ${f}) then
    echo
    echo "Warning: GTS UAE obs file ${obs_time}_GTS_UAE_data.${MM5HOST} was not created!"
    echo "Check directory $RUNDIR/${cycle_time}/DECODE_GTS_UAE"
    echo
else if (-z ${f}) then
    echo
    echo "Warning: GTS UAE obs file ${obs_time}_GTS_UAE_data.${MM5HOST} is empty!"
    echo "Check directory $RUNDIR/${cycle_time}/DECODE_GTS_UAE"
    echo
else
    echo
    ls -alh ${f}
    echo
endif

# Tag the observations for later head counts

#if (-e ${f}) then

#    echo
#    echo "Tagging observation with string GTSUAE"
#    echo

#    ~/bin/tagobs.pl GTSUAE ${f}

#    if (-e ${f}.GTSUAE) then
#        mv ${f}.GTSUAE ${f}
#    else 
#	 echo
#        echo "Tagging operation failed"
#        echo
#        echo "Use observation file without tag."
#        echo
#    endif

#endif 

#	Clean up house

rm input output gts.in
rm gts_out*
rm gts_sttnid_final.icao gts_sttnid_final gts_data

if (-e gts_uae_decoder.print) then
#echo "cat gts_uae_decoder.print >> $RUNDIR/${cycle_time}/${cycle_time}_gts_uae_decoder_print.out"
      cat gts_uae_decoder.print >> $RUNDIR/${cycle_time}/${cycle_time}_gts_uae_decoder_print.out
else
echo "Cannot find file gts_uae_decoder.print"
endif

cd $RUNDIR/${cycle_time}/DECODE_GTS_UAE
rm -rf ${obs_time}

endif

exit 0


#!/bin/tcsh  -f
#
###############################################################################
echo  "\n ----------------------------------------------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
#
# convert NCEP prepbufr raw obs to LittleR
#
###############################################################################

set timestamp
setenv RM "rm -rf"
#
# ENVIRONMENT
#
setenv MM5HOME /home/pmeops/fddahome
setenv CODE_DIR /home/pmeop/datbin/ingest
setenv DATA_DIR /d1/pmeop
setenv EXECUTABLE_ARCHIVE /d1/pmefdda/builds/build_mm_nightly/wrfv3.3.1/cycle_code/EXECUTABLE_ARCHIVE

#set echo
#	Check Arguments

if ( ${#argv} == 1 ) then
    set end_date = $1
else
    set end_date = `date +%Y%m%d%H` 
endif

set PREPBUFR2LITTLER_EXE="$CODE_DIR/RT_DECODER_PREPBUFR/convert_prepbufr2littleR.exe"
set SSRC_EXE="$CODE_DIR/RT_DECODER_PREPBUFR/ssrc.exe"
set INPUT_DIR="$DATA_DIR/datainput_raw/ncep_prepbufr"
set NCEP_PREPBUFR_DIR="$DATA_DIR/datainput/ncep_prepbufr_decoded"
set WORK_DIR="$DATA_DIR/datainput_raw/ncep_prepbufr/work"
$EXECUTABLE_ARCHIVE/MustHaveDir $WORK_DIR

cd ${WORK_DIR}
echo "Now working in  $cwd"

# locate NCEP prepbufr obs files
#
echo "$end_date, -5" >! input
${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
set start_date = `cat output`

echo "searching obs in ${INPUT_DIR} during $start_date $end_date"
set ccyymmddhh = $start_date
while ($ccyymmddhh <= $end_date)
  set ccyy = `echo $ccyymmddhh | cut -c1-4`
  set mm = `echo $ccyymmddhh | cut -c5-6`
  set dd = `echo $ccyymmddhh | cut -c7-8`
  set hh = `echo $ccyymmddhh | cut -c9-10`
  set obsfile = "${INPUT_DIR}/${ccyymmddhh}_gfs.t${hh}z.prepbufr.nr"
  if ( -e $obsfile ) then 
    echo found and process $obsfile
    if ( -e bufrfile) rm -f bufrfile
    rm -f *prepbufr2littleR.txt
    $SSRC_EXE < $obsfile > bufrfile 
    $PREPBUFR2LITTLER_EXE >& log
    $CODE_DIR/RT_DECODER_PREPBUFR/RT_prepbufr_trim-merge.pl prepbufr2littleR.txt
    rm -f prepbufr2littleR.txt

# check to see whether obs for each hour already exist
    set lss=`ls *.prepbufr2littleR.txt`
    foreach fn ( $lss )
      if ( -e $NCEP_PREPBUFR_DIR/$fn ) then
        cp $NCEP_PREPBUFR_DIR/$fn prepbufr2littleR.txt
        cat $fn >> prepbufr2littleR.txt
        $CODE_DIR/RT_DECODER_PREPBUFR/RT_prepbufr_trim-merge.pl prepbufr2littleR.txt
      endif
      mv -f $fn $NCEP_PREPBUFR_DIR/.
    end
    
  endif  
# Increment date
  echo "$ccyymmddhh, 1" >! input
  ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
  set ccyymmddhh = `cat output`
end

echo "NCEP PREPBUFR obs processing finished"

exit

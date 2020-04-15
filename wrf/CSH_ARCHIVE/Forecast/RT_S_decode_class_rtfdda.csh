#!/bin/tcsh -f
#
###############################################################################
echo 
echo  " ----------------------------------------------------------------------"
echo  " ---------------- CLASS decoder starts  --------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM CLASS_FDDA

set debug = 0
#
# ENVIRONMENT
#

if( $debug == 1) then
set DATA_DIR="/raid/input_04"
set RUNDIR="/raid/cycles/G04WSMR/WSMR"
endif

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
    set cycle_date = $1
    set obs_date = $2
else
    echo "usage: $0 cycle_date obs_date "
    echo "where cycle_date and obs_date is CCYYMMDDHH"
    exit ( 4 )
endif


if(-d $GEAPSTMP) then
 if( ! -d $GEAPSTMP/DECODE_CLASS) then
  $MustHaveDir $GEAPSTMP/DECODE_CLASS
  ln -s $GEAPSTMP/DECODE_CLASS $RUNDIR/${cycle_date}/DECODE_CLASS
 endif
else
 $MustHaveDir $RUNDIR/${cycle_date}/DECODE_CLASS
endif

cd ${RUNDIR}/${cycle_date}/DECODE_CLASS
echo "Now working in  $cwd"

foreach range (DPG YPG WSMR ATC CRTC IAF)

 rm *class*

if ($range == DPG) then

else if ($range == YPG) then
 cp ${CLASS_DATA_DIR_YPG}/$obs_date*class .
 touch a.class
 foreach snd (`ls $obs_date*.class`)
 rm fort.50 fort.10
 ln -s -f $snd fort.10
 ${RD_CLASS_EXE} 15 >> print.out.$range
 if (-e fort.50) cat fort.50 >> class_$range.$obs_date
 end
else if ($range == WSMR) then
 cp ${CLASS_DATA_DIR_WSMR}/$obs_date*class .
 touch a.class
 foreach snd (`ls $obs_date*.class`)
 rm fort.50 fort.10
 ln -s -f $snd fort.10
 ${RD_CLASS_EXE} 15 >> print.out.$range
 if (-e fort.50) cat fort.50 >> class_$range.$obs_date
 end
else if ($range == ATC) then
 cp ${CLASS_DATA_DIR_ATC}/$obs_date*class .
 touch a.class
 foreach snd (`ls $obs_date*.class`)
 rm fort.50 fort.10
 ln -s -f $snd fort.10
 ${RD_CLASS_EXE} 15 >> print.out.$range
 if (-e fort.50) cat fort.50 >> class_$range.$obs_date
 end
else if ($range == CRTC) then
 cp ${CLASS_DATA_DIR_CRTC}/$obs_date*class .
 touch a.class
 foreach snd (`ls $obs_date*.class`)
 rm fort.50 fort.10
 ln -s -f $snd fort.10
 ${RD_CLASS_EXE} 15 >> print.out.$range
 if (-e fort.50) cat fort.50 >> class_$range.$obs_date
 end
else if ($range == IAF) then
 cp ${CLASS_DATA_DIR_CRTC}/../../iaf/soundings/$obs_date*class .
 touch a.class
 foreach snd (`ls $obs_date*.class`)
 rm fort.50 fort.10
 ln -s -f $snd fort.10
 ${RD_CLASS_EXE} 15  >> print.out.$range
 if (-e fort.50) cat fort.50 >> class_$range.$obs_date
 end
endif
 cat class_*.$obs_date >> CLASS_$range.all

echo "CLASS SOUNDING DATA for $range is processed"

end
exit 0

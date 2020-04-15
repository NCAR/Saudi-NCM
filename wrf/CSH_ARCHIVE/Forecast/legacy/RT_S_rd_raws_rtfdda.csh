#!/bin/csh -f

#	This shell decodes the data from the University of Utah
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- RAW decoder starts  ---------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

if ( ${#argv} != 2 ) then
	echo "usage: $0 start_date obs_date"
	echo "where start_date is CCYYMMDDHH"
	echo  "d obs_date is CCYYMMDDHH"
	exit ( 4 )
endif

#set echo
set timestamp
setenv SUBSYSTEM RAWS_FDDA

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

set start_date = $1
set obs_date   = $2

#	Does the directory exist

if(-d $GEAPSTMP) then
 if (! -d $GEAPSTMP/RD_RAWS) then
  $MustHaveDir $GEAPSTMP/RD_RAWS
  ln -s $GEAPSTMP/RD_RAWS $RUNDIR/${start_date}/RD_RAWS
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_RAWS
endif

#	Have we already done this

if ( -e $RUNDIR/${start_date}/${obs_date}_RAWS_data.${MM5HOST} ) then
	echo "already decoded the RAWS data set"
endif

#	Go to the directory

cd $RUNDIR/${start_date}/RD_RAWS
echo "Now working in  $cwd"

#	Get the University of Utah data over here

set date = `echo "${obs_date}" | cut -c 3-10`
if ( ! -e ${RAWS_DATA_DIR}/${date}_sflist_all.utah ) then
	echo no Univ of Utah RAWS data found for ${date}
	exit ( 0 )
else
	if ( -e fort.9 ) rm fort.9
        cp  ${RAWS_DATA_DIR}/${date}_sflist_all.utah fort.9
endif

       if ( -e rdin ) rm rdin
       echo ";;;" >! rdin
       echo "$obs_date , 0" >! input
       ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
       set yymmdd = `cat output | cut -c 3-8`
       set hh = `cat output | cut -c 9-`
       echo "$yymmdd , -1 , -1" >> rdin
       $RD_RAWS_EXE  < rdin >! rd_raws_${hh}_print.out
       mv fort.10 rd_raws_${hh}


#	Put output where it is visible

if ( -e $RUNDIR/${start_date}/RD_RAWS/${obs_date}_RAWS_data.${MM5HOST} ) rm $RUNDIR/${start_date}/RD_RAWS/${obs_date}_RAWS_data.${MM5HOST}
mv rd_raws_${hh} $RUNDIR/${start_date}/RD_RAWS/${obs_date}_RAWS_data.${MM5HOST}
cat rd_raws_${hh}_print.out >>! $RUNDIR/${start_date}/${start_date}_raws_print.out.all

#	Clean up house

rm input output
rm fort.9 rdin
rm rd_raws*print.out
rm rd_raws_[01]*
exit ( 0 )

#!/bin/csh -f
##------------------------------------------------------------------------------
## This shell decodes the data from OKLAHOMA Mesonet network (OKMESO).
##
## Francois VANDENBERGHE, May 2003
## Copyright UCAR [RAP] 1996 - 2003. All Rights Reserved.
##------------------------------------------------------------------------------

###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- OKMESO decoder starts  ------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set debug = 0

if ( ${#argv} != 2 ) then
	echo "usage: $0 cycle_date obs_date"
	echo "where start_date is CCYYMMDDHH"
	echo  "d obs_date is CCYYMMDDHH"
	exit ( 4 )
endif

#set echo
set timestamp
setenv SUBSYSTEM OKMESO_FDDA

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

# Testing only

if ($debug > 0) then
    setenv MM5HOST GRM
endif

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
 if (! -d $GEAPSTMP/RD_OKMESO) then
  $MustHaveDir $GEAPSTMP/RD_OKMESO
  ln -s $GEAPSTMP/RD_OKMESO $RUNDIR/${start_date}/RD_OKMESO
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_OKMESO
endif

#	Have we already done this

if ( -e $RUNDIR/${start_date}/${obs_date}_OKMESO_data.${MM5HOST} ) then
	echo "already decoded the OKMESO data set"
endif

# Testing only
if ($debug > 0) then
set RUNDIR = /data/cycles/GM0004/ETA_NEWQ
if (! -d $RUNDIR/${start_date}) mkdir $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_OKMESO) mkdir $RUNDIR/${start_date}/RD_OKMESO
endif

#	Go to the directory


cd $RUNDIR/${start_date}/RD_OKMESO
echo "Now working in  $cwd"

#	Bring obs valid at obs_time and obs_time-1

echo "$obs_date , -1" >! input
${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
set dateb = `cat output | cut -c 1-10`
set datea = `echo "${obs_date}" | cut -c 1-10`

#	Get the Oklahoma climatological survey data (okmeso) here

set filesb = `find ${OKMESO_DATA_DIR} -name ${dateb}\*.d05`
set filesa = `find ${OKMESO_DATA_DIR} -name ${datea}\*.d05`

if ($#filesb <= 0 && $#filesa <= 0) then
	echo "No Oklahoma climatological survey data (okmeso) found for ${obs_date}"
	exit ( 0 )
else
	if ( -e fort.9 ) rm fort.9
        if ($#filesb > 0) then
            cat ${OKMESO_DATA_DIR}/${dateb}*.d05 >>! fort.9
        endif
        if ($#filesa > 0) then
            cat ${OKMESO_DATA_DIR}/${datea}*.d05 >>! fort.9
        endif
        cp  ${OKMESO_DATA_DIR}/info/geomeso.tbl .
endif


       if ( -e rdin ) rm rdin
       echo ";;;;" >! rdin
       echo "$obs_date , 0" >! input
       ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
       set ccyymmdd = `cat output | cut -c 1-8`
       set hh = `cat output | cut -c 9-10`
       echo "$ccyymmdd , -1 , -1" >> rdin
       $RD_OKMESO_EXE  < rdin >! rd_okmeso_${hh}_print.out
       mv fort.10 rd_okmeso_${hh}


#	Put output where it is visible

if ( -e $RUNDIR/${start_date}/RD_OKMESO/${obs_date}_OKMESO_data.${MM5HOST} ) rm $RUNDIR/${start_date}/RD_OKMESO/${obs_date}_OKMESO_data.${MM5HOST}
mv rd_okmeso_${hh} $RUNDIR/${start_date}/RD_OKMESO/${obs_date}_OKMESO_data.${MM5HOST}
cat rd_okmeso_${hh}_print.out >>! $RUNDIR/${start_date}/${start_date}_okmeso_print.out.all

#	Clean up house
if ($debug > 0) then
exit (0)
endif

rm input output
rm fort.9 rdin
rm rd_okmeso*print.out
rm rd_omeso_[01]*
exit ( 0 )

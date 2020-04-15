#! /bin/tcsh -x
#

set timestamp
setenv SUBSYSTEM SAMS_FDDA

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 logger -p local4.emerg -t "${SUBSYSTEM}" "Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST}; 

# ES temporary: on 'newer' clusters ncdump should be in /opt/netcdf/bin
if (-e /opt/netcdf/bin/ncdump) then
  set path = ( /opt/netcdf/bin $path )
else if ( $?NETCDF ) then
  set path = ( $NETCDF/bin $path )
endif
   
#set echo

#	Check usage

if ( ${#argv} == 1 ) then
    set start_date = $1
else
    echo "usage: $0 start_date "
    echo "where start_date is CCYYMMDDHH"
    exit ( 4 )
endif

MustHaveDir $RUNDIR/${start_date}/DECODE_SAMS
cd ${RUNDIR}/${start_date}/DECODE_SAMS

rm -rf sams*

set sams_date = `echo "${start_date}" | cut -c 3-8`
echo "INFO: DATES=$sams_date"

cp ${SAMS_DATA_DIR}/sams*${sams_date}* .

rm -rf input output
set f=`ls -1 sams*${sams_date}* | line`
${EXECUTABLE_ARCHIVE}/samsproc -F $f
mv $f ${f}.old
mv fred $f
echo "$f" > input
chmod 777 *
set fsn="../netcdf.stations"
echo " &OPARAM" > $fsn
echo " strname=" >> $fsn
ncdump -v platform $f | grep \"  | sed 's/;//' >> $fsn
echo " &END" >> $fsn
${EXECUTABLE_ARCHIVE}/crfil_rt < input

set lss=`ls *_SAMS`
#logger -p local4.info -t "${SUBSYSTEM}" "CREATED -> $lss"

@ n = 0
foreach i ( $lss )
@  n++
end
logger -p local4.info -t "${SUBSYSTEM}" "CREATED -> $n FILES OF SAMS DATA"

exit 0

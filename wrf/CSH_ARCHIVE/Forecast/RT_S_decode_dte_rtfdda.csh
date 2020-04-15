#!/bin/tcsh -f
#
###############################################################################
echo  " ----------------------------------------------------------------------"
echo  " ---------------- DTE Stations decoder starts  ------------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

set debug = 0
set timestamp
setenv SUBSYSTEM DTE_FDDA

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

# ES temporary: on 'newer' clusters ncdump should be in /opt/netcdf/bin
if (-e /opt/netcdf/bin/ncdump) then
  set path = ( /opt/netcdf/bin $path )
else if ( $?NETCDF ) then
  set path = ( $NETCDF/bin $path )
endif

if ($debug > 0) then
    setenv MM5HOST GRM
endif

#
# Data source
#
set DATACDF="/raid/input/wsmr/dte"

#
#	Check usage
#
if ( ${#argv} == 2 ) then
    set cycle_date = $1
    set obs_date = $2
else
    echo "usage: $0 cycle_date obs_date "
    echo "where cycle_date and obs_date is CCYYMMDDHH"
    exit ( 4 )
endif

if ($debug == 0) then # Regular statements

if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/RD_DTE
ln -s $GEAPSTMP/RD_DTE $RUNDIR/${cycle_date}/RD_DTE
else
$MustHaveDir $RUNDIR/${cycle_date}/RD_DTE
endif

else # Testing only

setenv  RUNDIR  /raid/cycles/VPGWS/GRM
if (! -d $RUNDIR/${cycle_date}) mkdir $RUNDIR/${cycle_date}
if (! -d $RUNDIR/${cycle_date}/RD_DTE) mkdir $RUNDIR/${cycle_date}/RD_DTE

endif


cd ${RUNDIR}/${cycle_date}/RD_DTE

rm -rf dte*
if (-f ${cycle_date}_dte.out.all) rm -f ${cycle_date}_dte.out.all

set dte_date = `echo "${obs_date}" | cut -c 3-8`

echo "$obs_date , -24" >! input
${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
set dte_dateb = `cat output`
set dte_dateb = `echo "${dte_dateb}" | cut -c 3-8`

echo "${SUBSYSTEM}: DECODE NETCDF DATA FILES FOR DAYS $dte_dateb and $dte_date"

foreach f ( `ls -1 $DATACDF/dte.${dte_dateb}.*.cdf $DATACDF/dte.${dte_date}.*.cdf `)
rm -rf input output

echo "$f" > input
chmod 777 *
set fsn="../netcdf_dte.stations"
echo " &OPARAM" > $fsn
echo " strname=" >> $fsn
ncdump -v platform $f | grep \"  | sed 's/;//' >> $fsn
echo " &END" >> $fsn
${EXECUTABLE_ARCHIVE}/rd_dte.exe < input  >> ${cycle_date}_dte.out.all

end # foreach

set lss=`ls *_DTE`

@ n = 0
foreach i ( $lss )
@  n++
end # foreach

if( -e dte.$dte_date) rm dte.$dte_date
set cycle_date_hh = `echo $cycle_date |cut -c 1-10`

foreach h (-6 -5 -4 -3 -2 -1 0 1 2 3 4)
echo "$cycle_date_hh , $h" >! input
${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
#set want_date = `cat output | cut -c 3-10`
set want_date = `cat output`
echo "${SUBSYSTEM}: want_date = $want_date"
cat ${want_date}*_DTE  >> dte.$dte_date
end # foreach

rm *_DTE fort.*

if (( $n <= 0) || (-z dte.$dte_date)) then
echo "${SUBSYSTEM} -- ERROR in $0 -> NO FILE OF DTE METARS DATA WAS GENERATED"
exit 1
else
echo "${SUBSYSTEM} -- CREATED -> $n FILES OF DTE METARS DATA"
if( -e ${obs_date}_DTE_data.${MM5HOST}) rm ${obs_date}_DTE_data.${MM5HOST}
mv dte.$dte_date ${obs_date}_DTE_data.${MM5HOST}
mv ${cycle_date}_dte.out.all $RUNDIR/${cycle_date}
endif

echo "${SUBSYSTEM} -- Succesful completion of RT_S_decode_dte_rtfdda.csh"
exit 0

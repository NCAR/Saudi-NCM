#!/bin/csh -f

###############################################################################
echo "$0 $argv[*]"
echo  "----------------------------------------------------------------------"
echo  " ---------------- Interp-BACK  for input obs QC     -------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

#set echo
set timestamp
setenv SUBSYSTEM INTERPBV3
setenv RM "rm -rf"

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
source ${CFILE}sizes.mm5sys.${MM5HOST}

if ( ${#argv} < 5 ) then
        echo "usage: $0 start_date end_date this_cycle last_cycle ptop"
        echo "where start_date, end_date, this_cycle and last_cycle are CCYYMMDD"
        echo "      ptop is the model top pressure"
        exit ( 1 )
endif


set start_date   = $1
set end_date     = $2
set this_cycle   = $3
set last_cycle   = $4
set ptop         = $5

set InterpType = 0
if (${#argv} == 6) then
  set InterpType = $6
endif


#	Get to the right directory
#
if (-d $GEAPSTMP) then
  $MustHaveDir $GEAPSTMP/INTERP_BACK 
  ln -s $GEAPSTMP/INTERP_BACK  $RUNDIR/$this_cycle/INTERP_BACK
else
  $MustHaveDir $RUNDIR/$this_cycle/INTERP_BACK
endif

cd $RUNDIR/$this_cycle/INTERP_BACK
echo "Now working in  $cwd"


#       Bring stuff over that we need
#
cp ${INTERPBV3_RT_TEMPLATE}.$ptop namelist.input

#       Which first guess file
#
ln -s ../${last_cycle}_MMOUTPUT_DOMAIN1 InFile

set start_year = `echo $start_date | cut -b 1-4`
set start_month = `echo $start_date | cut -b 5-6`
set start_day = `echo $start_date | cut -b 7-8`
set start_hour = `echo $start_date | cut -b 9-10`

set end_year = `echo $end_date | cut -b 1-4`
set end_month = `echo $end_date | cut -b 5-6`
set end_day = `echo $end_date | cut -b 7-8`
set end_hour = `echo $end_date | cut -b 9-10`

set time_int = 3600
if ($InterpType == 2) set time_int = 21600

ed namelist.input << EOF > /dev/null
g/year_start_nml/s//$start_year/
g/month_start_nml/s//$start_month/
g/day_start_nml/s//$start_day/
g/hour_start_nml/s//$start_hour/
g/year_end_nml/s//$end_year/
g/month_end_nml/s//$end_month/
g/day_end_nml/s//$end_day/
g/hour_end_nml/s//$end_hour/
g/time_int/s//$time_int/
g/!.*/s///
w
q
EOF
cat namelist.input

#	Run the program

( time $INTERPBV3_RT_EXE ) >! INTERPBV3_print.out

#	Is everything the right size

if ( ! -e REGRID_DOMAIN1) then
    echo "ERROR: Not all files created by INTERP"
    exit ( 2 )
endif

#       Move the important files around

echo "\n\n NAMELIST  \n\n" >> INTERPBV3_print.out
cat namelist.input >> INTERPBV3_print.out
mv REGRID_DOMAIN1 ../${this_cycle}_1STGUESSMM5_DOMAIN1.${MM5HOST}
if (-d $GEAPSTMP) then
  mv REGRID_DOMAIN1 $RUNDIR/${this_cycle}_1STGUESSMM5_DOMAIN1.${MM5HOST}
endif
mv INTERPBV3_print.out $RUNDIR/${this_cycle}/${this_cycle}_INTERPBV3_print.out 

#       Clean up

rm MMOUTPUTP* *MMOUTP*


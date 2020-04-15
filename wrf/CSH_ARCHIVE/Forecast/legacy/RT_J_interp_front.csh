#!/bin/csh -f

###############################################################################
echo "$0 $argv[*]"
echo  "----------------------------------------------------------------------"
echo  " ---------------- Interp-Front for BC and IC starts -------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

#set echo
set timestamp
setenv SUBSYSTEM INTERP
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
	echo "usage: $0 start_date end_date this_cycle ptop cold_start"
	echo "where start_date, end_date and this_cycle are CCYYMMDDHH"
	echo "ptop is model top pressure in hPa"
	exit ( 1 )
endif

set start_date = $1
set end_date   = $2
set this_cycle = $3
set ptop       = ${4}00
set cold_start = $5

set JobType  = 0   # Standard operational run
if ( ${#argv} == 6 ) then
   set JobType = $6
endif

#	Get to the right directory

if(-d $GEAPSTMP) then
  $MustHaveDir $GEAPSTMP/INTERP_FRONT
  ln -s $GEAPSTMP/INTERP_FRONT  $RUNDIR/$this_cycle/INTERP_FRONT
else
  $MustHaveDir $RUNDIR/$this_cycle/INTERP_FRONT
endif

cd $RUNDIR/$this_cycle/INTERP_FRONT
echo "Now working in  $cwd"


#	Has this already been run

set odir = $OUTPUT_DATA_DIR

if ( ( -e $odir/${this_cycle}_MMINPUT_DOMAIN1.${MM5HOST} ) && \
     ( -e $odir/${this_cycle}_BDYOUT.${MM5HOST} ) ) then
        echo "INFO: INTERP output already completed"
        exit ( 0 )
endif

set ccdir = $RUNDIR/$this_cycle

#	Bring stuff over that we need

if( -e $GSJOBDIR/namelists/INTERP.namelist.template )then
  cp $GSJOBDIR/namelists/INTERP.namelist.template namelist.input
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/INTERP.namelist.template"
else if ( -e ${INTERP_TEMPLATE} ) then
  cp ${INTERP_TEMPLATE} namelist.input
  echo "${SUBSYSTEM} -- Using ${INTERP_TEMPLATE} "
else
  echo "${SUBSYSTEM} File Missing -> interp namelist.input"
  exit (2)
endif

if( -e $GSJOBDIR/executables/interp.exe )then
  cp $GSJOBDIR/executables/interp.exe .
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/interp.exe"
else if ( -e ${INTERP_FRONT_EXE} ) then
  cp ${INTERP_FRONT_EXE} interp.exe  
  echo "${SUBSYSTEM} -- Using $INTERP_FRONT_EXE "
else
  echo "${SUBSYSTEM} File Missing -> interp.exe "
  exit (2)
endif

#	Which first guess file

if ( ( -l InFile ) || ( -e InFile ) ) rm InFile
if ( -e  ../${this_cycle}_ANALYSIS.${MM5HOST}                    ) then
	ln -s ../${this_cycle}_ANALYSIS.${MM5HOST}                    InFile
	set ok = $status
	if ( $ok != 0 ) then
		cp ../${this_cycle}_ANALYSIS.${MM5HOST}            InFile
	endif
else
	echo "hmmm, cannot find a first guess file"
	exit ( 2 )
endif

#	Modify namelist

set start_year = `echo $start_date | cut -b 1-4`
set start_month = `echo $start_date | cut -b 5-6`
set start_day = `echo $start_date | cut -b 7-8`
set start_hour = `echo $start_date | cut -b 9-10`

set end_year = `echo $end_date | cut -b 1-4`
set end_month = `echo $end_date | cut -b 5-6`
set end_day = `echo $end_date | cut -b 7-8`
set end_hour = `echo $end_date | cut -b 9-10`

# Changed ic_times = 25 to alow for long analysis period under Climo-FDDA
# ANH, 03/26/07

set ic_times = 14     
set time_int = 3600
set less_than_24h = ".TRUE."
if ($cold_start == 1 ) then
  set time_int = 10800
  set ic_times = 1
endif

# CFDDA preprocessing run to generate QC files 
if ($JobType == 1) then
  set less_than_24h = ".FALSE."
  set ic_times = 25
  set time_int = 3600
endif

# CFDDA model run 
if ($JobType == 2) then
  set less_than_24h = ".FALSE."
  set ic_times = 21
  set time_int = 21600
endif

ed namelist.input << EOF > /dev/null
g/less_than_24h_nml/s//$less_than_24h/
g/year_start_nml/s//$start_year/
g/month_start_nml/s//$start_month/
g/day_start_nml/s//$start_day/
g/hour_start_nml/s//$start_hour/
g/year_end_nml/s//$end_year/
g/month_end_nml/s//$end_month/
g/day_end_nml/s//$end_day/
g/hour_end_nml/s//$end_hour/
g/_PTOP/s//$ptop/
g/time_int/s//$time_int/
g/ic_times/s//$ic_times/
g/!.*/s///
w
q
EOF
cat namelist.input

#	Run the program

( time ./interp.exe ) >! interp_front_print.out

#	Move the important files around

mv interp_front_print.out $ccdir/${this_cycle}_interp_front_print.out
if( -d $GEAPSTMP) then
 cp BDYOUT_DOMAIN1 ../${this_cycle}_BDYOUT.${MM5HOST}
 cp MMINPUT_DOMAIN1 ../${this_cycle}_MMINPUT_DOMAIN1.${MM5HOST}
 cp LOWBDY_DOMAIN1 ../${this_cycle}_LOWBDY_DOMAIN1.${MM5HOST}
endif

mv BDYOUT_DOMAIN1 $ccdir/${this_cycle}_BDYOUT.${MM5HOST}
mv MMINPUT_DOMAIN1 $ccdir/${this_cycle}_MMINPUT_DOMAIN1.${MM5HOST}
mv LOWBDY_DOMAIN1 $ccdir/${this_cycle}_LOWBDY_DOMAIN1.${MM5HOST}


#	Clean up

rm input output

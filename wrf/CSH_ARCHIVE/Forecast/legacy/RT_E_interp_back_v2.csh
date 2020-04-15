#!/bin/csh -f

###############################################################################
echo  " ----------------------------------------------------------------------"
echo  " ---------------- Interp-Back for 1st guess starts---------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

#set echo
set timestamp
setenv SUBSYSTEM INTERP_BCK
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

if ( ${#argv} != 5 ) then
	echo "usage: $0 start_date end_date this_cycle last_cycle ptop"
	echo "where start_date, end_date, this_cycle and last_cycle are CCYYMMDD"
	echo "      ptop is the model top pressure "
	exit ( 1 )
endif

set start_datehh = $1
set end_datehh   = $2
set this_cycle = $3
set last_cycle = $4
set ptop       = $5

#	Get to the right directory

$MustHaveDir $RUNDIR/$this_cycle/INTERP_BACK
cd $RUNDIR/$this_cycle/INTERP_BACK
echo "Now working in  $cwd"


#	Need two dates for namelist of V2 interp_back

set end_date   = `echo  $end_datehh   | cut -c 3-10`
set start_date = `echo $start_datehh | cut -c 3-10`

#	Bring stuff over that we need

cp ${INTERP_BCK_RT_TEMPLATE}.$ptop fort.36

#	Which first guess file

# convert V3 to V2 
ln -s ../${last_cycle}_MMOUTPUT_DOMAIN1 ${last_cycle}_MMOUTPUT_DOMAIN1
if( ! -e ${last_cycle}_MMOUTPUT_DOMAIN1.v2) then
${V32V2_EXE} ${last_cycle}_MMOUTPUT_DOMAIN1 >/dev/null
endif

# replace T2, U10 and V10 to first sigma level T, U and V resp. 

ln -s -f ${last_cycle}_MMOUTPUT_DOMAIN1.v2 fort.10
${EXECUTABLE_ARCHIVE}/v2_replace_t2uv10.exe >/dev/null
mv fort.45 ${last_cycle}_MMOUTPUT_DOMAIN1.v2
rm fort.21 fort.10

if ( ( -l fort.50 ) || ( -e fort.50 ) ) rm fort.50
if      ( -e  ${last_cycle}_MMOUTPUT_DOMAIN1.v2 ) then
    ln -s ${last_cycle}_MMOUTPUT_DOMAIN1.v2 fort.50
    set ok = $status
    if ( $ok != 0 ) then
	cp ${last_cycle}_MMOUTPUT_DOMAIN1.v2 fort.50
    endif
else
    echo "hmmm, cannot find a first guess file"
    exit ( 2 )
endif

#	Modify namelist

ed fort.36 << EOF > /dev/null
g/yymmddhh_start/s//$start_date/
g/yymmddhh_end/s//$end_date/
w
q
EOF

#	Run the program

( time $INTERP_BACK_RT_EXE ) >! INTERP_BACK_print.out

#	Is everything the right size

if ( ! -e fort.21 ) then
    echo "ERROR: Not all files created by INTERP"
    exit ( 2 ) 
endif

#	Move the important files around

mv INTERP_BACK_print.out $RUNDIR/$this_cycle/${this_cycle}_INTERP_BACK_print.out
mv fort.21 ${this_cycle}_INTERP_BACK_DOMAIN1.${MM5HOST}.v2

# convert V2 to V3 
${V22V3_EXE} ${this_cycle}_INTERP_BACK_DOMAIN1.${MM5HOST}.v2 >/dev/null
mv ${this_cycle}_INTERP_BACK_DOMAIN1.${MM5HOST}.v3 ${RUNDIR}/${this_cycle}/${this_cycle}_1STGUESSMM5_DOMAIN1.${MM5HOST}

#	Clean up

rm fort.50

#!/bin/csh -f

###############################################################################
echo "$0 $argv[*]"
echo  "----------------------------------------------------------------------"
echo  " ---------------- Nestdown for BC and IC starts -----------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

#set echo
set timestamp
setenv SUBSYSTEM NESTDOWN
setenv RM "rm -rf"

set domains = ( 2 3 4)  
set dm1 = ( 0 1 2 3)
if($?NUM_DOMS) then
 if ($NUM_DOMS == 5) then
   set domains = ( 2 3 4 5)  
   set dm1 = ( 0 1 2 3 4)
 else if ($NUM_DOMS == 4) then
   set domains = ( 2 3 4 )
   set dm1 = ( 0 1 2 3 )
 else if ($NUM_DOMS == 3) then
   set domains = ( 2 3 )
   set dm1 = ( 0 1 2 )
 else if ($NUM_DOMS <= 2) then
   set domains = ( 2 )
   set dm1 = ( 0 1 )
 endif
endif
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

if ( ${#argv} != 3 ) then
	echo "usage: $0 start_date end_date this_cycle"
	echo "where start_date,end_date and this_cycle are CCYYMMDDHH"
	exit ( 1 )
endif

set start_date = $1
set end_date   = $2
set this_cycle = $3

#	Get to the right directory

if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/NESTDOWN
ln -s $GEAPSTMP/NESTDOWN $RUNDIR/$this_cycle/NESTDOWN
else
$MustHaveDir $RUNDIR/$this_cycle/NESTDOWN
endif

cd $RUNDIR/$this_cycle/NESTDOWN
echo "Now working in  $cwd"



set ccdir = $RUNDIR/$this_cycle
 
foreach d ( $domains )

#	Bring stuff over that we need

set in = ${NESTDOWN_TEMPLATE}
if ( ! -e $in ) then
 echo "${SUBSYSTEM} File Missing -> $in"
 exit (2)
endif
cp $in namelist.input

#	Which first guess file

set dprev = $dm1[$d]
if ( ( -l InFile ) || ( -e InFile ) ) rm InFile
if      ( -e  ../${this_cycle}_MMINPUT_DOMAIN${dprev}.${MM5HOST} ) then
	ln -s ../${this_cycle}_MMINPUT_DOMAIN${dprev}.${MM5HOST} InFile
	set ok = $status
	if ( $ok != 0 ) then
		cp ../${this_cycle}_MMINPUT_DOMAIN${dprev}.${MM5HOST}   InFile
	endif
endif
#	Which lowbdy file

if ( ( -l LowBdyFile ) || ( -e LowBdyFile ) ) rm LowBdyFile
if      ( -e  ../${this_cycle}_LOWBDY_DOMAIN${dprev}.${MM5HOST} ) then
	ln -s ../${this_cycle}_LOWBDY_DOMAIN${dprev}.${MM5HOST} LowBdyFile
	set ok = $status
	if ( $ok != 0 ) then
		cp ../${this_cycle}_LOWBDY_DOMAIN${dprev}.${MM5HOST}   LowBdyFile
	endif
endif

# replace TSEASFC (instant) in MMINPUT_DOMAIN1 with TSEASFC (24hour average) in LOWBDY_DOMAIN1
# since nestdown uses TSEASFC to set lake SST
if($d == 2) then
 ln -s -f LowBdyFile fort.10
 echo "1" >input
 $EXECUTABLE_ARCHIVE/sstlbdy2input.exe <input
 ln -s -f InFile fort.10
 echo "2" >input
 $EXECUTABLE_ARCHIVE/sstlbdy2input.exe <input
 mv fort.45 InFile
 if( -d $GEAPSTMP) then
  cp InFile ../${this_cycle}_MMINPUT_DOMAIN1.${MM5HOST}
 endif
 cp InFile  $ccdir/${this_cycle}_MMINPUT_DOMAIN1.${MM5HOST}
 rm fort.10
endif

#	Which terrain file

if ( ( -l TerrainFile ) || ( -e TerrainFile ) ) rm TerrainFile

if ( -e $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN$d ) then
 ln -s $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN$d TerrainFile
 echo "${SUBSYSTEM}  Using Terrain in $GSJOBDIR "
else if ( -e  $TERRAIN_DIR/Domain${d}_New_LU.V.${MM5HOST} ) then
 ln -s $TERRAIN_DIR/Domain${d}_New_LU.V.${MM5HOST} TerrainFile
 echo "${SUBSYSTEM}  Using Terrain in $TERRAIN_DIR"
else
 echo "${SUBSYSTEM}  Missing file -> NO terrain in $GSJOBDIR or $TERRAIN_DIR"
 exit (2)
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

ed namelist.input << EOF > /dev/null
g/year_start_nml/s//$start_year/
g/month_start_nml/s//$start_month/
g/day_start_nml/s//$start_day/
g/hour_start_nml/s//$start_hour/
g/year_end_nml/s//$end_year/
g/month_end_nml/s//$end_month/
g/day_end_nml/s//$end_day/
g/hour_end_nml/s//$end_hour/
g/!.*/s///
w
q
EOF
cat namelist.input

#	Run the program

( time $NESTDOWN_EXE ) >! nestdown${d}_print.out

#	Is everything the right size

if ( ( -e BDYOUT_DOMAIN${d} ) && ( -e MMINPUT_DOMAIN${d} ) && ( -e LOWBDY_DOMAIN${d} ) ) then
	echo "All files created by NESTDOWN"
else
	echo "not all files created by NESTDOWN"
	exit ( 2 ) 
endif

#	Change bay SST for domain 2 3 4 -- both "TSEASFC" in MMINPUT and LOWBDY

#if ($d > 1) then

# firstly, do it for MMINPUT
# mv MMINPUT_DOMAIN${d} mminput.tmp
# $CSH_ARCHIVE/Forecast/RT_rwv3_changeSST_${MM5HOST}.csh mminput.tmp $start_month>> ${this_cycle}_SST_print.out
# $CSH_ARCHIVE/Forecast/RT_rwv3_changeSST_ATC.csh mminput.tmp $start_month>> ${this_cycle}_SST_print.out
# mv mmtmp0 MMINPUT_DOMAIN${d}

# then, do it for LOWBDY also
# ln -s -f MMINPUT_DOMAIN${d} fort.10
# echo "1" >input
# $EXECUTABLE_ARCHIVE/sstlbdy2input.exe <input
# ln -s -f LOWBDY_DOMAIN${d} fort.10
# echo "2" >input
# $EXECUTABLE_ARCHIVE/sstlbdy2input.exe <input
# mv fort.45 LOWBDY_DOMAIN${d}
# rm fort.10

#endif

#	Move the important files around

mv nestdown${d}_print.out $ccdir/${this_cycle}_nestdown${d}_print.out
if ( -d $GEAPSTMP ) then
 cp BDYOUT_DOMAIN${d}  ../${this_cycle}_BDYOUT_DOMAIN${d}.${MM5HOST}
 cp MMINPUT_DOMAIN${d} ../${this_cycle}_MMINPUT_DOMAIN${d}.${MM5HOST}
 cp LOWBDY_DOMAIN${d}  ../${this_cycle}_LOWBDY_DOMAIN${d}.${MM5HOST}
endif
mv MMINPUT_DOMAIN${d} $ccdir/${this_cycle}_MMINPUT_DOMAIN${d}.${MM5HOST}
mv BDYOUT_DOMAIN${d}  $ccdir/${this_cycle}_BDYOUT_DOMAIN${d}.${MM5HOST}
mv LOWBDY_DOMAIN${d}  $ccdir/${this_cycle}_LOWBDY_DOMAIN${d}.${MM5HOST}

end

#	Clean up

#rm input output

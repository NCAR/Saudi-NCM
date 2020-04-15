#!/bin/tcsh -f

#  This shell decodes the GRIB GFS data and gridded it to MM5 DOMAIN1.
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- ungrib and Regrid start ----------------------------"
echo  " ------------------ RT_A_regrid_gfs.csh ------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM REGRID
setenv RM "rm -rf"
#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo " ${SUBSYSTEM} --  Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST}
source ${CFILE}sizes.mm5sys.${MM5HOST}

#	Check usage

if      ( ${#argv} < 6 )  then
	echo "usage: $0 gfs_date cycle_date start_date end_date cold_start"
	echo "where gfs_date is the 0 h of the GFS fcst start"
	echo "cycle_date is the time of this_cycle, and "
	echo "start_date is the time when this_cycle's fcst starts"
	echo "end_date is the needed hour of current GFS fcst"
	echo "where all are given in CCYYMMDDHH"
        echo "NODE: 0 -mpp; 1 - 31 running omp on node"
	exit ( 1 )
endif

#	Choose which GFS data to access (ie, 00 Z or 12 Z), and
#	assign the input variables.

set gfs_date = $1
set cycle_date = $2
set start_date = $3
set end_date = $4
set cold_start = $5
set cgfs_date = $1
set NODE    = $6
set ukmet = "no"
if  ( ${#argv} >= 7 )  set ukmet = $7
set ptop = 5000
set time_to_access = `echo $gfs_date | cut -c 9-10`
set day_to_get = `echo $gfs_date | cut -c 3-10`

$MustHaveDir $RUNDIR/$cycle_date
setenv  DECODING_GFS_DIR  ${RUNDIR}/${cycle_date}/GFS_REGRID

if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/GFS_REGRID
ln -s $GEAPSTMP/GFS_REGRID  $DECODING_GFS_DIR
else
$MustHaveDir $DECODING_GFS_DIR
endif

rm $DECODING_GFS_DIR/*

#
#	What YYMMDDHH date are we after from the 0 h GFS forecast
#
#set dateymdh = `echo $gfs_date | cut -c 3-10`
#

#  normally we should have 17 gfs files for 48 hours
@ num_gfs = 9

#
#       SECOND, look for the gfs data in the "data" directory.
#
pushd $OUTPUT_DATA_DIR
if ( `find . -name ${gfs_date}\*pres\* -print | wc -l` >= $num_gfs ) then
  echo "INFO: using  GFS data on data disk"
 foreach l ( `find . -name ${gfs_date}\*fh\*onedeg -print` )
   set newn = `echo $l | cut -c 3-20` # e.g., 2002013012_fh.0000
   set l1   = `echo $l | cut -c 3-`   # eg.., 2002013012_fh.0000_tl.press_gr.onedeg
   rm -f $RUNDIR/$cycle_date/$newn
   ln -s ${OUTPUT_DATA_DIR}/${l1} $DECODING_GFS_DIR/$newn
 end
 rm -f $DECODING_GFS_DIR/gfs.date.T${time_to_access}Z

 setenv DATA_DIR ${RUNDIR}/${cycle_date}
else
 echo "INFO: Could NOT find $num_gfs files on the DISK of the form ${gfs_date}\*pres\*"
endif
popd

#	Where we want the data to go, and just head over there.

cd $DECODING_GFS_DIR
echo "Now working in  $cwd"


set BCIC="GFS"
set ltrs = ( AAA AAB AAC AAD AAE AAF AAG AAH AAI AAJ AAK AAL AAM \
             AAN AAO AAP AAQ AAR AAS AAT AAU AAV AAW AAX AAY AAZ \
             ABA ABB ABC ABD ABE ABF ABG ABH ABI ABJ ABK ABL ABM \
             ABN ABO ABP ABQ ABR ABS ABT ABU ABV ABW ABX ABY ABZ )
set icount = 0


foreach f (`ls ${OUTPUT_DATA_DIR}/*eta* ${OUTPUT_DATA_DIR}/*tl.press*`)
        @ icount ++
        set ltr = $ltrs[$icount]
        set fn = `echo $f | tr -d =@=`
ls -al $fn
#        if(-e $fn) ln -s -f $fn GRIBFILE.${ltr}
         ln -s -f $fn GRIBFILE.${ltr}
end

ls -al

ln -s -f $GSJOBDIR/wps/Variable_Tables/Vtable.$BCIC.MM5 Vtable

touch _REGRIDv3_+
foreach t ( *_REGRIDv3_+*)
        if ( -e $t ) ${RM} $t
end

#	The TERRAIN file has the record header stuff,
#	plus all of the 2d constant fields (latitude, longitude, etc.).

if ( -e $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN1 ) then
 ln -s $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN1 TERRAIN
 echo "${SUBSYSTEM}  Using Terrain in $GSJOBDIR "
else if ( -e  $TERRAIN_DIR/Domain1_New_LU.V.${MM5HOST} ) then
 ln -s $TERRAIN_DIR/Domain1_New_LU.V.${MM5HOST} TERRAIN
 echo "${SUBSYSTEM}  Using Terrain in $TERRAIN_DIR"
else
 echo "${SUBSYSTEM}  Missing file -> NO terrain in $GSJOBDIR or $TERRAIN_DIR"
 exit (2)
endif

#
# Build the namelists.
#

set yyyy_start = `echo $start_date | cut -b 1-4`
set mm_start = `echo $start_date | cut -b 5-6`
set dd_start = `echo $start_date | cut -b 7-8`
set hh_start = `echo $start_date | cut -b 9-10`

set yyyy_end = `echo  $end_date | cut -b 1-4`
set mm_end = `echo  $end_date | cut -b 5-6`
set dd_end = `echo  $end_date | cut -b 7-8`
set hh_end = `echo  $end_date | cut -b 9-10`

set time_int = 3600
set dom = 1
set BC_INT = 3600
if($cold_start == 1) set time_int = 10800


cp $GSJOBDIR/namelists/wps.nl.template namelist.wps

ed namelist.wps << EOF > /dev/null
g/SYY/s//$yyyy_start/g
g/SMM/s//$mm_start/g
g/SDD/s//$dd_start/g
g/SHH/s//$hh_start/g
g/EYY/s//$yyyy_end/g
g/EMM/s//$mm_end/g
g/EDD/s//$dd_end/g
g/EHH/s//$hh_end/g
g/ITT/s//$BC_INT/g
g/DOM/s//$dom/g
w
q
EOF



cp ${REGRIDDER_NML_TEMPL} namelist.input
ed namelist.input << EOF > /dev/null
g/_YYYY_START/s//$yyyy_start/
g/_MM_START/s//$mm_start/
g/_DD_START/s//$dd_start/
g/_HH_START/s//$hh_start/
g/_YYYY_END/s//$yyyy_end/
g/_MM_END/s//$mm_end/
g/_DD_END/s//$dd_end/
g/_HH_END/s//$hh_end/
g/_INT_SEC/s//$time_int/
g/_TERRAIN_FILE/s//TERRAIN/
g/_PTOP/s//$ptop/
w
q
EOF

# 1. Run UNGRIB

echo "start UNGRGIB ...."
echo "--------------- ungrib -----------------------" >> print.out_${gfs_date}


cp ${EXECUTABLE_ARCHIVE}/ungrib.exe .

./ungrib.exe  > print.out_wrfwps


if ($ukmet == "ukmet" || $ukmet == "UKMET") then
$CSH_ARCHIVE/Forecast/RT_A_regrid_ukmet.csh $argv[*]
endif

echo "--------------- regrid -----------------------" >> print.out_${gfs_date}

if ( $NODE == "0") then
 if ( -e ${REGRIDDER_EXE} ) then
  echo "copying   ${REGRIDDER_EXE} ./regridder.exe"
  cp ${REGRIDDER_EXE} regridder.exe
  echo "${SUBSYSTEM} -- Using ${REGRIDDER_EXE}"
 else
  echo "${SUBSYSTEM} -- Missing regridder.exe executable --  exiting"
  exit (4)
 endif
else
 if(-e $GSJOBDIR/executables/regridder.exe.$NODE) then
  cp $GSJOBDIR/executables/regridder.exe.$NODE regridder.exe
  echo "copying   ${REGRIDDER_EXE} ./regridder.exe"
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/regridder.exe.$NODE"
 else if ( -e $GSJOBDIR/executables/regridder.exe ) then
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/regridder.exe"
 else if ( -e ${REGRIDDER_EXE}.$NODE ) then
  cp ${REGRIDDER_EXE}.$NODE regridder.exe
  echo "${SUBSYSTEM} -- Using  ${REGRIDDER_EXE}.$NODE"
 else if ( -e ${REGRIDDER_EXE} ) then
  cp ${REGRIDDER_EXE} regridder.exe
  echo "${SUBSYSTEM} -- Using ${REGRIDDER_EXE}"
 else
  echo "${SUBSYSTEM} -- Missing regridder.exe executable --  exiting"
  exit (4)
 endif
endif

( time ./regridder.exe ) >> print.out_${gfs_date}

#yliu
ln -s -f REGRID_DOMAIN1 fort.10
set UNIT   = 45
foreach hh (000 003 006 009 012 015 018 021 024 027 030 033 036 039 042 045 048 051 054 057 060 063 066 069 072 075 078 081 084 087 090 093 096 099 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156)
   set Local = REGRID_+${hh}_DOMAIN1
   ln -s -f    $Local             fort.$UNIT
   @ UNIT ++
end
${EXECUTABLE_ARCHIVE}/splitv3.exe  >> print.out_${gfs_date}
 rm fort.*
#yliu

echo "--------------- adjust_eta_sst.exe -----------------------" >> print.out_${gfs_date}
foreach rfile ( REGRID_+* )
   # Reset SST in the Salt Lake region:
   rm -f fort.10 fort.20
   ln -s $rfile fort.10
   ${EXECUTABLE_ARCHIVE}/adjust_eta_sst.exe  >> print.out_${gfs_date}
   rm fort.10
   mv fort.20 $rfile
   if ($status == 1) echo "Error processing ${rfile}"
   # Done with the SST reset.
   set dd = `echo $rfile | cut -b 9-11`
   mv $rfile ${gfs_date}_REGRIDv3_+${dd}h
end

find . -name TERRAIN -type l -exec rm {} \;

#	The directory where the forecast will run is based upon the 0 h
#	of the MM5 forecast, not the AVN date.

## legacy - not supported by ungrib/grib2... LPC 1/9/08
##if ( ! -e eta_date_found ) then           # ideally should be gfs_date_found
##  echo"${SUBSYSTEM} --Program Error - NO eta_date_found produced"
##  exit ( 8 )
##endif
##if ( `cat eta_date_found` != $gfs_date ) then
##	echo "time mismatch"
##	set t = `cat eta_date_found`
##	echo  $t " " $gfs_date
##	echo "${SUBSYSTEM} --AVN date-in-file=${t} - but we want ${gfs_date}"
##	exit ( 5 )
##endif

if(-d $GEAPSTMP) then
cat ${gfs_date}_REGRIDv3_+*h > ../${cycle_date}_REGRIDv3.${MM5HOST}
ls -l ../${cycle_date}_REGRIDv3.${MM5HOST} > $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}.size
else
cat ${gfs_date}_REGRIDv3_+*h > $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}
endif
cp print.out_${gfs_date} $RUNDIR/$cycle_date/${start_date}_regrid_print.out.${MM5HOST}

# Clean up:

##rm eta_date_found          # ideally should be gfs_date_found

exit ( 0 )

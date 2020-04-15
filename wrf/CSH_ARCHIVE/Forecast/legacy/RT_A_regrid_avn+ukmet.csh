#!/bin/tcsh -f

#  This shell decodes the GRIB one-deg AVN data and gridded it to MM5 DOMAIN1.
###############################################################################
echo  " ----------------------------------------------------------------------"    
echo  " ---------------- Pregrid and Regrid start ----------------------------"    
echo  " -------------- RT_A_regrid_avn+ukmet.csh -----------------------------"    
###############################################################################

echo  "$0 $argv[*]"

#set echo
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
	echo "usage: $0 avn_date cycle_date start_date end_date cold_start ptop ukmet"
	echo "where avn_date is the 0 h of the AVN fcst start"
	echo "cycle_date is the time of this_cycle, and "
	echo "start_date is the time when this_cycle's fcst starts"
	echo  $end_date is the needed hour of current AVN fcst"
	echo "ptop is the model top pressure in hPa."
	echo "ukmet indicates that UKMET upper-air data willbe used."
	echo "where all are given in CCYYMMDDHH"
        echo "NODE: 0 -mpp; 1 - 31 running omp on node"
	exit ( 1 ) 
endif


#	Choose which AVN data to access (ie, 00 Z or 12 Z), and
#	assign the input variables.

set avn_date = $1
set cycle_date = $2
set start_date = $3
set end_date = $4
set cold_start = $5
set cavn_date = $1
set NODE    = $6
set ukmet = "no"
set ptop = 5000
if  ( ${#argv} >= 7 )  set ptop = ${7}00
if  ( ${#argv} >= 8 )  set ukmet = $8
set time_to_access = `echo $avn_date | cut -c 9-10`
set day_to_get = `echo $avn_date | cut -c 3-10`

$MustHaveDir $RUNDIR/$cycle_date 
setenv  DECODING_AVN_DIR  ${RUNDIR}/${cycle_date}/AVN_REGRID

if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/AVN_REGRID
ln -s $GEAPSTMP/AVN_REGRID  $DECODING_AVN_DIR
else
$MustHaveDir $DECODING_AVN_DIR
endif

rm $DECODING_AVN_DIR/*

#
#	What YYMMDDHH date are we after from the 0 h AVN forecast
#
#set dateymdh = `echo $avn_date | cut -c 3-10`
#

#  normally we should have 17 avn files for 48 hours
@ num_avn = 9

#
#       SECOND, look for the avn data in the "data" directory.
#
pushd $OUTPUT_DATA_DIR
if ( `find . -name ${avn_date}\*fh\*onedeg -print | wc -l` >= $num_avn ) then
  echo "INFO: using  AVN data on data disk"
 foreach l ( `find . -name ${avn_date}\*fh\*onedeg -print` )
   set newn = `echo $l | cut -c 3-20` # e.g., 2002013012_fh.0000
   set l1   = `echo $l | cut -c 3-`   # eg.., 2002013012_fh.0000_tl.press_gr.onedeg
   rm -f $RUNDIR/$cycle_date/$newn
   ln -s ${OUTPUT_DATA_DIR}/${l1} $DECODING_AVN_DIR/$newn
 end
 rm -f $DECODING_AVN_DIR/avn.date.T${time_to_access}Z

 setenv DATA_DIR ${RUNDIR}/${cycle_date}
else
 echo "INFO: Could NOT find $num_avn files on the DISK of the form ${avn_date}\*fh\*"
endif
popd

#	Where we want the data to go, and just head over there.

cd $DECODING_AVN_DIR
echo "Now working in  $cwd"


#	Build the starting and ending dates to deGRIB.

#echo "$start_date , 48" >! input
#advance_cymdh < input >! output
#set end_date = `cat output`

set ltrs = ( A B C D E F G H I J K L M N O P Q R S T U V W X Y Z )
set icount = 0    

foreach f ( ${avn_date}_fh.0000 \
            ${avn_date}_fh.0003 \
            ${avn_date}_fh.0006 \
            ${avn_date}_fh.0009 \
            ${avn_date}_fh.0012 \
            ${avn_date}_fh.0015 \
            ${avn_date}_fh.0018 \
            ${avn_date}_fh.0021 \
            ${avn_date}_fh.0024 \
            ${avn_date}_fh.0027 \
            ${avn_date}_fh.0030 \
            ${avn_date}_fh.0033 \
            ${avn_date}_fh.0036 \
            ${avn_date}_fh.0039 \
            ${avn_date}_fh.0042 \
            ${avn_date}_fh.0045 \
            ${avn_date}_fh.0051 \
            ${avn_date}_fh.0054 \
            ${avn_date}_fh.0057 \
            ${avn_date}_fh.0060 )

        @ icount ++
        set ltr = $ltrs[$icount]

        if ( -e $DECODING_AVN_DIR/${f} ) then
	  ln -s $DECODING_AVN_DIR/$f GRIBFILE.A${ltr}

  	  #	List the files to read in the degribber.

 	  echo "$DECODING_AVN_DIR/$f" >>! input
        else
          @ icount --
        endif
end


#	Remove any old REGRIDv3 output files.

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
if($cold_start == 1) set time_int = 10800

echo "${PREGRID_NML_TEMPL}"
cp ${PREGRID_NML_TEMPL} pregrid.namelist
ed pregrid.namelist << EOF > /dev/null
g/_YYYY_START/s//$yyyy_start/
g/_MM_START/s//$mm_start/
g/_DD_START/s//$dd_start/
g/_HH_START/s//$hh_start/
g/_YYYY_END/s//$yyyy_end/
g/_MM_END/s//$mm_end/
g/_DD_END/s//$dd_end/
g/_HH_END/s//$hh_end/
g/_INT_SEC/s//$time_int/
g/#.*/s///
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

#
# Get the Vtable:
#
cp $PREGRID_VTABLE_AVN_ALL Vtable

#	Run the GRIB decoder.
if ( ! -e  ${PREGRID_EXE} ) then
 echo "${SUBSYSTEM}  Missing file -> ${DEGRIB_EXE}"
 exit (2)
endif

#setenv MP_SET_NUMTHREADS 4
echo "--------------- pregrid -----------------------" > print.out_${avn_date}

( time ${PREGRID_EXE} ) >> print.out_${avn_date}

rm PFILE* GRIBFILE.A* 

if ($ukmet == "ukmet" || $ukmet == "UKMET") then
$CSH_ARCHIVE/Forecast/RT_A_regrid_ukmetnc.csh $argv[*]
endif
exit
echo "--------------- regrid -----------------------" >> print.out_${avn_date}

if ( $NODE == "0") then
 if ( -e ${REGRIDDER_EXE} ) then
  cp ${REGRIDDER_EXE} regridder.exe
  echo "${SUBSYSTEM} -- Using ${REGRIDDER_EXE}"
 else
  echo "${SUBSYSTEM} -- Missing regridder.exe executable --  exiting"
  exit (4)
 endif
else
 if(-e $GSJOBDIR/executables/regridder.exe.$NODE) then
  cp $GSJOBDIR/executables/regridder.exe.$NODE regridder.exe
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/executables/regridder.exe.$NODE"
 else if ( -e $GSJOBDIR/executables/regridder.exe ) then
  cp $GSJOBDIR/executables/regridder.exe regridder.exe
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

( time ./regridder.exe ) >> print.out_${avn_date}

#yliu
ln -s -f REGRID_DOMAIN1 fort.10
set UNIT   = 45
foreach hh (000 003 006 009 012 015 018 021 024 027 030 033 036 039 042 045 048 051 054 057 060 063 066 069 072 075 078 081 084 087 090 093 096 099 102 105 108 111 114 117 120)
   set Local = REGRID_+${hh}_DOMAIN1
   ln -s -f    $Local             fort.$UNIT
   @ UNIT ++
end
${EXECUTABLE_ARCHIVE}/splitv3.exe  >> print.out_${avn_date}
 rm fort.*
#yliu

echo "--------------- adjust_eta_sst.exe -----------------------" >> print.out_${avn_date}
foreach rfile ( REGRID_+* )
   # Reset SST in the Salt Lake region:
   rm -f fort.10 fort.20
   ln -s $rfile fort.10
   ${EXECUTABLE_ARCHIVE}/adjust_eta_sst.exe  >> print.out_${avn_date}
   rm fort.10
   mv fort.20 $rfile
   # Done with the SST reset.
   set dd = `echo $rfile | cut -b 9-11`
   mv $rfile ${avn_date}_REGRIDv3_+${dd}h
end

find . -name TERRAIN -type l -exec rm {} \;

#	The directory where the forecast will run is based upon the 0 h
#	of the MM5 forecast, not the AVN date.

#	Check that this is the right AVN forecast.

if ( ! -e eta_date_found ) then           # ideally should be avn_date_found
  echo"${SUBSYSTEM} --Program Error - NO eta_date_found produced"
  exit ( 8 )
endif
if ( `cat eta_date_found` != $avn_date ) then
	echo "time mismatch"
	set t = `cat eta_date_found`
	echo  $t " " $avn_date
	echo "${SUBSYSTEM} --AVN date-in-file=${t} - but we want ${avn_date}"
	exit ( 5 )
endif

#	The important files are the ${avn_date}_REGRIDv3_+{00,03,06,09,12,15,18,21,24,27,30,33,36}h

#if( -e ${avn_date}_REGRIDv3_+36h ) then
#set NUMFIL = 0
#set III = 12
#foreach NUMFIL ( 00 03 06 09 12 15 18 21 24 )
#   @ III = 12 + $NUMFIL
#   mv ${avn_date}_REGRIDv3_+${III}h ${avn_date}_REGRIDv3_+${NUMFIL}h
#end
#endif
#unset NUMFIL

#foreach NUMFIL ( 00 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72)
#  mv ${start_date}_REGRIDv3_+${NUMFIL}h $RUNDIR/$cycle_date/${start_date}_REGRIDv3_+${NUMFIL}h.${MM5HOST}
#  mv ${avn_date}_REGRIDv3_+${NUMFIL}h $RUNDIR/$cycle_date/${cavn_date}_REGRIDv3_+${NUMFIL}h.${MM5HOST}
#end
if(-d $GEAPSTMP) then
cat ${avn_date}_REGRIDv3_+*h > ../${cycle_date}_REGRIDv3.${MM5HOST}
#cp ../${cycle_date}_REGRIDv3.${MM5HOST} $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}
ls -l ../${cycle_date}_REGRIDv3.${MM5HOST} > $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}.size
else
cat ${avn_date}_REGRIDv3_+*h > $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}
endif
cp print.out_${avn_date} $RUNDIR/$cycle_date/${start_date}_regrid_print.out.${MM5HOST}

# Clean up:

rm eta_date_found          # ideally should be avn_date_found

exit ( 0 )

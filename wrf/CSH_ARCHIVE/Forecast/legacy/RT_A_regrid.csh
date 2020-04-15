#!/bin/tcsh -f

#  This shell decodes the GRIB ETA AWIP 212 data and gridded it to MM5 DOMAIN1.
###############################################################################
echo  " ----------------------------------------------------------------------"
echo  " ---------------- Pregrid and Regrid start ----------------------------"
echo  " ----------------------------------------------------------------------"
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

if      ( ${#argv} != 5 )  then
	echo "usage: $0 eta_date cycle_date start_date end_date cold_start"
	echo "where eta_date is the 0 h of the ETA fcst start"
	echo "cycle_date is the time of this_cycle, and "
	echo "start_date is the time when this_cycle's fcst starts"
	echo  $end_date is the needed end hour of current ETA fcst"
	echo "where all are given in CCYYMMDDHH"
	exit ( 1 )
endif

#	Choose which ETA data to access (ie, 00 Z or 12 Z), and
#	assign the input variables.

set eta_date = $1
set cycle_date = $2
set start_date = $3
set end_date = $4
set cold_start = $5
set ceta_date = $1
set ptop = 5000
set time_to_access = `echo $eta_date | cut -c 9-10`
set day_to_get = `echo $eta_date | cut -c 3-10`

$MustHaveDir $RUNDIR/$cycle_date
setenv  DECODING_ETA_DIR  ${RUNDIR}/${cycle_date}/ETA_REGRID

if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/ETA_REGRID
ln -s $GEAPSTMP/ETA_REGRID  $DECODING_ETA_DIR
else
$MustHaveDir $DECODING_ETA_DIR
endif

rm $DECODING_ETA_DIR/*

#
#	What YYMMDDHH date are we after from the 0 h ETA forecast
#
set dateymdh = `echo $eta_date | cut -c 3-10`
#

#  normally we should have 9 eta files for 24 hours
@ num_eta = 9

#
#       SECOND, look for the eta data in the "data" directory.
#
pushd $OUTPUT_DATA_DIR
if ( `find . -name eta\*${dateymdh} -print | wc -l` >= $num_eta ) then
  echo "INFO: using  ETA data on data disk"
 foreach l ( `find . -name eta\*${dateymdh} -print` )
   set newn = `echo $l | cut -c 3-24`
   set l1   = `echo $l | cut -c 3-`
   rm -f $RUNDIR/$cycle_date/$newn
   ln -s ${OUTPUT_DATA_DIR}/${l1} $DECODING_ETA_DIR/$newn
 end
 rm -f $DECODING_ETA_DIR/eta.date.T${time_to_access}Z

 setenv DATA_DIR ${RUNDIR}/${cycle_date}
else
 echo "INFO: Could NOT find $num_eta files on the DISK of the form eta\*${dateymdh}"
endif
popd

#	Where we want the data to go, and just head over there.

cd $DECODING_ETA_DIR
echo "Now working in  $cwd"


#	Build the starting and ending dates to deGRIB.

#echo "$start_date , 48" >! input
#${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
#set end_date = `cat output`

set ltrs = ( A B C D E F G H I J K L M N O P Q R S T U V W X Y Z )
set icount = 0

foreach f ( eta.T${time_to_access}Z.AWIP3D00.tm00 \
            eta.T${time_to_access}Z.AWIP3D03.tm00 \
            eta.T${time_to_access}Z.AWIP3D06.tm00 \
            eta.T${time_to_access}Z.AWIP3D09.tm00 \
            eta.T${time_to_access}Z.AWIP3D12.tm00 \
            eta.T${time_to_access}Z.AWIP3D15.tm00 \
            eta.T${time_to_access}Z.AWIP3D18.tm00 \
            eta.T${time_to_access}Z.AWIP3D21.tm00 \
            eta.T${time_to_access}Z.AWIP3D24.tm00 \
            eta.T${time_to_access}Z.AWIP3D27.tm00 \
            eta.T${time_to_access}Z.AWIP3D30.tm00 \
            eta.T${time_to_access}Z.AWIP3D33.tm00 \
            eta.T${time_to_access}Z.AWIP3D36.tm00 \
            eta.T${time_to_access}Z.AWIP3D39.tm00 \
            eta.T${time_to_access}Z.AWIP3D42.tm00 \
            eta.T${time_to_access}Z.AWIP3D45.tm00 \
            eta.T${time_to_access}Z.AWIP3D48.tm00 \
            eta.T${time_to_access}Z.AWIP3D51.tm00 \
            eta.T${time_to_access}Z.AWIP3D54.tm00 \
            eta.T${time_to_access}Z.AWIP3D57.tm00 \
            eta.T${time_to_access}Z.AWIP3D60.tm00 )

        @ icount ++
        set ltr = $ltrs[$icount]

        if ( -e $DECODING_ETA_DIR/${f} ) then
	  ln -s $DECODING_ETA_DIR/$f GRIBFILE.A${ltr}

  	  #	List the files to read in the degribber.

 	  echo "$DECODING_ETA_DIR/$f" >>! input
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
cp $PREGRID_VTABLE Vtable

#	Run the GRIB decoder.
if ( ! -e  ${PREGRID_EXE} ) then
 echo "${SUBSYSTEM}  Missing file -> ${DEGRIB_EXE}"
 exit (2)
endif

#setenv MP_SET_NUMTHREADS 4
echo "--------------- pregrid -----------------------" > print.out_${eta_date}

( time ${PREGRID_EXE} ) >> print.out_${eta_date}

rm PFILE* GRIBFILE.A*

echo "--------------- regrid -----------------------" >> print.out_${eta_date}
( time ${REGRIDDER_EXE} ) >> print.out_${eta_date}

#yliu
ln -s -f REGRID_DOMAIN1 fort.10
set UNIT   = 45
foreach hh (00 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87)
   set Local = REGRID_+${hh}_DOMAIN1
   ln -s -f    $Local             fort.$UNIT
   @ UNIT ++
end
${EXECUTABLE_ARCHIVE}/splitv3.exe  >> print.out_${eta_date}
 rm fort.*
#yliu

echo "--------------- adjust_eta_sst.exe -----------------------" >> print.out_${eta_date}
foreach rfile ( REGRID_+* )
   # Reset SST in the Salt Lake region:
   rm -f fort.10 fort.20
   ln -s $rfile fort.10
   ${EXECUTABLE_ARCHIVE}/adjust_eta_sst.exe  >> print.out_${eta_date}
   rm fort.10
   mv fort.20 $rfile
   # Done with the SST reset.
   set dd = `echo $rfile | cut -b 9-10`
   mv $rfile ${eta_date}_REGRIDv3_+${dd}h
end

find . -name TERRAIN -type l -exec rm {} \;


#	The directory where the forecast will run is based upon the 0 h
#	of the MM5 forecast, not the ETA 104 date.

#	Check that this is the right ETA forecast.

if ( ! -e eta_date_found ) then
  echo"${SUBSYSTEM} --Program Error - NO eta_date_found produced"
  exit ( 8 )
endif
if ( `cat eta_date_found` != $eta_date ) then
	echo "time mismatch"
	set t = `cat eta_date_found`
	echo  $t " " $eta_date
	echo "${SUBSYSTEM} --ETA date-in-file=${t} - but we want ${eta_date}"
	exit ( 5 )
endif

#	The important files are the ${eta_date}_REGRIDv3_+{00,03,06,09,12,15,18,21,24,27,30,33,36}h

#if( -e ${eta_date}_REGRIDv3_+36h ) then
#set NUMFIL = 0
#set III = 12
#foreach NUMFIL ( 00 03 06 09 12 15 18 21 24 )
#   @ III = 12 + $NUMFIL
#   mv ${eta_date}_REGRIDv3_+${III}h ${eta_date}_REGRIDv3_+${NUMFIL}h
#end
#endif
#unset NUMFIL

foreach NUMFIL ( 00 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87)
#  mv ${start_date}_REGRIDv3_+${NUMFIL}h $RUNDIR/$cycle_date/${start_date}_REGRIDv3_+${NUMFIL}h.${MM5HOST}
#  mv ${eta_date}_REGRIDv3_+${NUMFIL}h $RUNDIR/$cycle_date/${ceta_date}_REGRIDv3_+${NUMFIL}h.${MM5HOST}
end
if(-d $GEAPSTMP) then
cat ${eta_date}_REGRIDv3_+*h > ../${cycle_date}_REGRIDv3.${MM5HOST}
#cp ../${cycle_date}_REGRIDv3.${MM5HOST} $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}
ls -l ../${cycle_date}_REGRIDv3.${MM5HOST} > $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}.size
else
cat ${eta_date}_REGRIDv3_+*h > $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}
endif
mv print.out_${eta_date} $RUNDIR/$cycle_date/${start_date}_regrid_print.out.${MM5HOST}

# Clean up:

rm eta_date_found

exit ( 0 )

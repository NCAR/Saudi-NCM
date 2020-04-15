#!/bin/tcsh  -f

#  This shell decodes the GRIB AVN global tile data and gridded it to
#   MM5 DOMAIN1.
###############################################################################
echo  " ----------------------------------------------------------------------"
echo  " ---------------- Pregrid and Regrid start ----------------------------"
echo  " ----------- RT_A_regrid_avn.csh --------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM REGRID
setenv RM "rm -rf"
set ltrs = ( A B C D E F G H I J K L M N O P Q R S T U V W X Y Z )
set tlev = ( 00 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 )
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

if      ( ${#argv} < 4 )  then
	echo "usage: $0 avn_date cycle_date start_date end_date"
	echo "where avn_date is the 0 h of the AVN fcst start"
	echo "cycle_date is the time of this_cycle, and "
	echo "start_date is the time when this_cycle's fcst starts"
	echo "cavn_date is the 0 h of current AVN fcst start"
	echo "where all are given in CCYYMMDDHH"
	exit ( 1 )
endif

#	Choose which AVN data to access (ie, 00 Z or 12 Z), and
#	assign the input variables.

set avn_date = $1
set cycle_date = $2
set start_date = $3
set end_date = $4
set cavn_date = $1
set ukmet = "no"
if  ( ${#argv} >= 5 )  set ukmet = $5
set time_to_access = `echo $avn_date | cut -c 9-10`
set day_to_get = `echo $avn_date | cut -c 1-8`
set avn_time_tag = ${day_to_get}_${time_to_access}
set sst_date = ${day_to_get}

set ptop = 10000


#       SST is a 00Z analysis from the previous day
#       - from ftp://ftp.ncep.noaa.gov/pub/data1/rtg_sst/
#
#set sstn =  ${sst_date}.rtg_sst
set sstn =  sst.grib
set newsstn =  sst.grib

$MustHaveDir $RUNDIR/$cycle_date
setenv  DECODING_AVN_DIR  ${RUNDIR}/${cycle_date}/AVN_REGRID
$MustHaveDir $DECODING_AVN_DIR

rm $DECODING_AVN_DIR/*
ln -s ${OUTPUT_DATA_DIR}/${sstn} ${DECODING_AVN_DIR}/$newsstn

set cen = `echo $avn_date |cut -c 1-2`
set sst_day = `/home/fddasys/datbin/wgrib ${DECODING_AVN_DIR}/$newsstn|cut -c 7-14`
set sst_date = ${cen}${sst_day}

set yyyy_sst = `echo $sst_date | cut -b 1-4`
set mm_sst = `echo $sst_date | cut -b 5-6`
set dd_sst = `echo $sst_date | cut -b 7-8`
set hh_sst = `echo $sst_date | cut -b 9-10`

#
#	What YYMMDDHH date are we after from the 0 h AVN forecast
#
set dateymd = `echo $avn_date | cut -c 1-8`
#

#  normally we should have 72 (8 global tiles x 9 times) avn files for 48 hours
#  at least we should have 56 (8 global tiles x 7 times) avn files (for 36 hours)
@ num_avn = 56

#
#       SECOND, look for the avn data in the "data" directory.
#
pushd $OUTPUT_DATA_DIR

@ avn_count = 0
foreach hr (00 06 12 18 24 30 36 42 48)
  foreach tile (037 038 039 040 041 042 043 044)
    if( $hr == '00') then
      set avn_file=${avn_time_tag}00_AVN_${tile}_${hr}.grib
    else
      set avn_file=${avn_time_tag}00_096_${tile}_${hr}.grib
    endif

    if( -e $avn_file) then
      set avn_size=`ls -l $avn_file | awk '{print $5}'`
      if($avn_size >= 300000) then
        @ avn_count++
      endif
    endif
  end
end

if($avn_count >= $num_avn) then
  foreach hr (00 06 12 18 24 30 36 42 48)

    if($hr == '00') then
      set grid = AVN
    else
      set grid = 096
    endif

    foreach tile (037 038 039 040 041 042 043 044)
      set avnn = ${day_to_get}_${time_to_access}00_${grid}_${tile}_${hr}.grib
      set newn = AVN_${tile}_00_${hr}.grib
      rm -f $RUNDIR/$cycle_date/$newn
      ln -s ${OUTPUT_DATA_DIR}/${avnn} ${DECODING_AVN_DIR}/$newn
    end
  end

  setenv DATA_AVN_DIR ${RUNDIR}/${cycle_date}
# setenv DATA_DIR ${RUNDIR}/${cycle_date}    # never used!
else
  echo "INFO: Could NOT find $num_avn files on the DISK of the form avn\*${dateymd}"
endif

popd

#	Where we want the data to go, and just head over there.

cd $DECODING_AVN_DIR
echo "Now working in  $cwd"


#	Build the starting and ending dates to deGRIB.

#echo "$start_date , 48" >! input
#${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
#set end_date = `cat output`

foreach tile (037 038 039 040 041 042 043 044)

  set icount = 0

  foreach hr (00 06 12 18 24 30 36 42 48)

    @ icount++
    set ltr = $ltrs[$icount]
    set f = AVN_${tile}_00_${hr}.grib

#   Only make the link/echo the filename, if it exists... otherwise
#   pregrid will fail rather than skip the file...

    if ( -e $DECODING_AVN_DIR/${f} ) then
       ln -s $DECODING_AVN_DIR/$f GRIBFILE.A${ltr}

#      List the files to read in the degribber.

       echo "$DECODING_AVN_DIR/$f" >>! input
    else
       @ icount --
    endif

  end     # foreach hr

#	Remove any old REGRIDv3 output files.

  foreach t ( 00 06 12 18 24 30 36 42 48)
    if ( -e ${avn_date}_REGRIDv3_+${t}h.${MM5HOST} ) ${RM} ${avn_date}_REGRIDv3_+${t}h.${MM5HOST}
  end

# The TERRAIN file has the record header stuff,
# plus all of the 2d constant fields (latitude, longitude, etc.).

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
g/_INT_SEC/s//3600/
g/#.*/s///
w
q
EOF

# Get the Vtable:
#
  cp $PREGRID_VTABLE_AVN Vtable

# Run the PREGRIB decoder
  if ( ! -e  ${PREGRID_AVN_EXE} ) then
    echo "${SUBSYSTEM}  Missing file -> ${PREGRID_AVN_EXE}"
    #logger -p local4.emerg -t "${SUBSYSTEM}" "Missing file -> ${PREGRID_AVN_EXE}"
    exit (2)
  endif

# setenv MP_SET_NUMTHREADS 4

  ( time ${PREGRID_AVN_EXE} ) >&! print.out

  foreach pfile ( FILE:* )
    mv $pfile ${tile}_${pfile}
  end

  rm PFILE* GRIBFILE.A* Vtable

end    # foreach tile
mv eta_date_found avn_date_found   # 'eta' is hard-coded in rd_grib.F

# This ends the giant loop to run pregrid on each of the AVN tiles.  Now,
# put the tiles together before running regridder.
#
set h  = 0
while ($h < 49)
   echo "$avn_date , $h" >! input
   ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
   set l_year = `cat output | cut -c 1-4`
   set l_mon  = `cat output | cut -c 5-6`
   set l_day  = `cat output | cut -c 7-8`
   set l_hour = `cat output | cut -c 9-10`
   set nm37 = 037_FILE:${l_year}-${l_mon}-${l_day}_${l_hour}
   set nm38 = 038_FILE:${l_year}-${l_mon}-${l_day}_${l_hour}
   set nm39 = 039_FILE:${l_year}-${l_mon}-${l_day}_${l_hour}
   set nm40 = 040_FILE:${l_year}-${l_mon}-${l_day}_${l_hour}
   set nm41 = 041_FILE:${l_year}-${l_mon}-${l_day}_${l_hour}
   set nm42 = 042_FILE:${l_year}-${l_mon}-${l_day}_${l_hour}
   set nm43 = 043_FILE:${l_year}-${l_mon}-${l_day}_${l_hour}
   set nm44 = 044_FILE:${l_year}-${l_mon}-${l_day}_${l_hour}
   ( time ${MOSAIC_EXE} $nm37 $nm38 $nm39 $nm40 $nm41 $nm42 $nm43 $nm44 ) >&! mosaic_print.out
   @ h ++
end

set icount = 0
set f = sst.grib

set ltr = A

# OK, so the data is there, copy it to a holding place

# Only make the link/echo the filename, if it exists... otherwise
# pregrid will fail rather than skip the file...

ln -s $DECODING_AVN_DIR/$f GRIBFILE.A${ltr}

# List the files to read in the degribber.

echo "$DECODING_AVN_DIR/$f" >>! input

#
# Set the namelist for the SST file - this is always a 00Z SST analysis
echo "${PREGRID_NML_TEMPL}"
cp ${PREGRID_NML_TEMPL} pregrid.namelist
ed pregrid.namelist << EOF > /dev/null
g/_YYYY_START/s//$yyyy_sst/
g/_MM_START/s//$mm_sst/
g/_DD_START/s//$dd_sst/
g/_HH_START/s//$hh_sst/
g/_YYYY_END/s//$yyyy_sst/
g/_MM_END/s//$mm_sst/
g/_DD_END/s//$dd_sst/
g/_HH_END/s//$hh_sst/
g/_INT_SEC/s//21600/
g/#.*/s///
w
q
EOF

# Get the Vtable:
#
cp $PREGRID_VTABLE_AVN Vtable

#       Run the GRIB decoder for SST.

( time ${PREGRID_AVN_EXE} ) >&! print.out

foreach pfile ( FILE:* )
  mv $pfile SSTFILE
end

rm PFILE* GRIBFILE.A* pregrid.namelist Vtable

if ($ukmet == "ukmet" || $ukmet == "UKMET") then
$CSH_ARCHIVE/Forecast/RT_A_regrid_ukmet.csh $argv[*]
endif

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
g/_INT_SEC/s//10800/
g/_TERRAIN_FILE/s//TERRAIN/
g/_PTOP/s//$ptop/
w
q
EOF

#

( time ${REGRIDDER_AVN_EXE} ) >> print.out

#yliu
ln -s -f REGRID_DOMAIN1 fort.10
set UNIT   = 45
foreach hh (00 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72)
   set Local = REGRID_+${hh}_DOMAIN1
   ln -s -f    $Local             fort.$UNIT
   @ UNIT ++
end
${EXECUTABLE_ARCHIVE}/splitv3.exe  >> print.out_${avn_date}
 rm fort.*
#yliu

#echo "--------------- adjust_avn_sst.exe -----------------------" >> print.out_${avn_date}
foreach rfile ( REGRID_+* )
#   # Reset SST in the Salt Lake region:
#   rm -f fort.10 fort.20
#   ln -s $rfile fort.10
#   ${EXECUTABLE_ARCHIVE}/adjust_avn_sst.exe  >> print.out_${avn_date}
#   rm fort.10
#   mv fort.20 $rfile
#   # Done with the SST reset.
    set dd = `echo $rfile | cut -b 9-10`
   mv $rfile ${avn_date}_REGRIDv3_+${dd}h
end

find . -name TERRAIN -type l -exec rm {} \;


#	Check that this is the right AVN forecast.

if ( ! -e avn_date_found ) then
  echo"${SUBSYSTEM} --Program Error - NO avn_date_found produced"
  exit ( 8 )
endif
if ( `cat avn_date_found` != $avn_date ) then
	echo "time mismatch"
	set t = `cat avn_date_found`
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

foreach NUMFIL ( 00 03 06 09 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72)
   mv ${avn_date}_REGRIDv3_+${NUMFIL}h $RUNDIR/$cycle_date/${cavn_date}_REGRIDv3_+${NUMFIL}h.${MM5HOST}
end
mv print.out_${avn_date} $RUNDIR/$cycle_date/${start_date}_regrid_print.out.${MM5HOST}

# Clean up:

rm avn_date_found

exit ( 0 )

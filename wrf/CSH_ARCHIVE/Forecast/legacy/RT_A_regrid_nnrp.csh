#! /bin/csh -f

#  This shell decodes the GRIB 2.5-deg NNRP data and gridded it to MM5 DOMAIN1.
###############################################################################
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"    
echo  " ---------------- Pregrid and Regrid start ----------------------------"    
echo  " ------------------ RT_A_regrid_nnrp.csh ------------------------------"    
###############################################################################

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

#------------------------------------------------------------------------------#
# Necessary arguments
#	Check usage

if ( ${#argv} < 6 )  then
   echo "usage: $0 avn_date cycle_date start_date end_date cold_start"
   echo "where avn_date is the 0 h of the AVN fcst start"
   echo "cycle_date is the time of this_cycle, and "
   echo "start_date is the time when this_cycle's fcst starts"
   echo "end_date is the needed hour of current AVN fcst"
   echo "where all are given in CCYYMMDDHH"
   echo "NODE: 0 -mpp; 1 - 31 running omp on node"
   exit (1) 
endif

# Define variables locally

set PREGRID_EXE = $EXECUTABLE_ARCHIVE/pregrid_grib.exe
set REGRIDDER_EXE = $EXECUTABLE_ARCHIVE/regridder.exe

set PREGRID_NML_TEMPL = $CONSTANT_FILES/PREGRID.namelist.template
set REGRIDDER_NML_TEMPL = $CONSTANT_FILES/REGRIDDER.namelist.template

set PREGRID_VTABLE_NNRP_ALL = $CONSTANT_FILES/PREGRID_VTABLE.NNRP

# Choose which NNRP data to access (ie, 00 Z or 12 Z), and
# assign the input variables.

set avn_date   = $1
set cycle_date = $2
set start_date = $3
set end_date   = $4
set cold_start = $5
set cavn_date  = $1
set NODE       = $6

set InterpType = 1
if (${#argv} == 7 )  then
  set InterpType = $7
endif

set ptop = 5000

set DECODING_AVN_DIR  = ${RUNDIR}/${cycle_date}/NNRP_REGRID

# Goto workdir

$MustHaveDir $RUNDIR/$cycle_date 

if (-d $GEAPSTMP) then
  $MustHaveDir $GEAPSTMP/NNRP_REGRID
  if (! -e $DECODING_AVN_DIR) ln -s $GEAPSTMP/NNRP_REGRID $DECODING_AVN_DIR
else
  $MustHaveDir $DECODING_AVN_DIR
endif

#
#	What YYMMDDHH date are we after from the 0 h AVN forecast
#
# Make sure the starting and ending time are either 00z, 06z, 12z or 18z

echo "start_date = $start_date"
echo "end_date   = $end_date"
echo "interp_type = $InterpType"

set h = `echo $start_date |cut -c9-10`
@ q = $h / 6
@ r = $h - 6 * $q
set start_date = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $start_date -$r` 

set h = `echo $end_date |cut -c9-10`
@ q = $h / 6
@ r = $h - 6 * $q
if ($r != 0) @ r = 6  - $r
set end_date = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $end_date +$r` 

echo "start_date = $start_date"
echo "end_date   = $end_date"

#  normally we should have 9 upper-air NNRP files 48 hours

@ num_avn = 1

#
#       SECOND, look for the NNRP upper-air data in the "data" directory.
#
pushd $DECODING_AVN_DIR

if ( `find . -name UPANNRP\* -print | wc -l` >= $num_avn ) then
  echo "INFO: using UPA NNRP data on data disk"
else
  echo "INFO: Could NOT find any files on the DISK of the form UPANNRP\*"
endif

if ( `find . -name SFCNNRP\* -print | wc -l` >= $num_avn ) then
  echo "INFO: using SFC NNRP data on data disk"
else
  echo "INFO: Could NOT find any files on the DISK of the form SFCNNRP.\*"
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
set icount1 = 0
set icount2 = 0

find . -name GRIBFILE.\* -exec rm {} \;

#foreach f (`ls data/UPANNRP* data/SFCNNRP*` )
foreach f (`ls UPANNRP* SFCNNRP*` )

# First suffix letter
        @ icount ++

        @ q  = $icount / 26
        @ qq = $q * 26
        @ r  = $icount - $qq
        @ r  = $r - 1

       if ($r == 0) then
           @ icount1 = $icount1 + 1
           @ icount2 = 0
       endif

       set ltr1 = $ltrs[$icount1]

# Second suffix letter
       @ icount2 ++
       set ltr2 = $ltrs[$icount2]

#      set ltr = $ltrs[$icount]

        if ( -e $DECODING_AVN_DIR/${f} ) then
	  ln -s $DECODING_AVN_DIR/$f GRIBFILE.${ltr1}${ltr2}

  	  #	List the files to read in the degribber.

 	  echo "$DECODING_AVN_DIR/$f" >>! input
        else
          @ icount2 --
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

set yyyy_end = `echo $end_date | cut -b 1-4`
set mm_end = `echo $end_date | cut -b 5-6`
set dd_end = `echo $end_date | cut -b 7-8`
set hh_end = `echo $end_date | cut -b 9-10`

set time_int = 3600
if ($cold_start == 1) set time_int = 10800
if ($InterpType == 2) set time_int = 21600

echo "INFO: using ${PREGRID_NML_TEMPL}"
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
cp $PREGRID_VTABLE_NNRP_ALL Vtable

#	Run the GRIB decoder.
if ( ! -e  ${PREGRID_EXE} ) then
 echo "${SUBSYSTEM}  Missing file -> ${DEGRIB_EXE}"
 exit (2)
endif

#setenv MP_SET_NUMTHREADS 4
echo "--------------- pregrid -----------------------" > print.out_${avn_date}

( time ${PREGRID_EXE} ) >> print.out_${avn_date}

rm PFILE* GRIBFILE.* 

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


echo "--------------- split the REGRID file by time level --------" >> print.out_${avn_date}

# How many time levels we should expect

set h  = 0
set HH = "00"
@ time_int_mn = $time_int / 3600
echo "$start_date $HH"
set ccyymmddhh = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $start_date +$time_int_mn`

while ($ccyymmddhh <= $end_date)
  @ h ++
  if ($h < 10) then
      set hh = "0${h}"
  else if ($h < 100) then
      set hh = "${h}"
  else
      echo "ERROR: h = $h > 99"
      echo "The interval [$start_date, $end_date] includes more than 100 time levels"
      echo ""
      exit -1
  endif
  echo "$ccyymmddhh $hh"
  set HH = "$HH $hh"
  set ccyymmddhh = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $ccyymmddhh +$time_int_mn`
end

echo "$HH"

ln -s -f REGRID_DOMAIN1 fort.10
set UNIT   = 45
foreach hh ($HH)
   set Local = REGRID_+${hh}_DOMAIN1
   ln -s -f $Local fort.$UNIT
   @ UNIT ++
end

${EXECUTABLE_ARCHIVE}/splitv3.exe  >> print.out_${avn_date}

rm fort.*
set here = `pwd`

foreach rfile ( REGRID_+* )
   set dd = `echo $rfile | cut -b 9-11`
   mv $rfile ${avn_date}_REGRIDv3_+${dd}h
end

if (-d $GEAPSTMP) then
  cat ${avn_date}_REGRIDv3_+*h > ../${cycle_date}_REGRIDv3.${MM5HOST}
  ls -l ../${cycle_date}_REGRIDv3.${MM5HOST} > $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}.size
  echo "$SUBSYSTEM -- Output file is ${here}/${cycle_date}_REGRIDv3.${MM5HOST}"
else
  cat ${avn_date}_REGRIDv3_+*h > $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}
  echo "$SUBSYSTEM -- Output file is $RUNDIR/$cycle_date/${cycle_date}_REGRIDv3.${MM5HOST}"
endif

cp print.out_${avn_date} $RUNDIR/$cycle_date/${start_date}_regrid_print.out

# Clean up:

if (-e eta_date_found) rm eta_date_found

cd $RUNDIR/$cycle_date

exit ( 0 )

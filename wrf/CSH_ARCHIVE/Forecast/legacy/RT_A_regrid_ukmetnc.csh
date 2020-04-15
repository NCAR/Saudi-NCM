#!/bin/tcsh -f

## This shell applies pregrif to the ECMWF files provided by the UK Met Office.
###############################################################################
echo  " ----------------------------------------------------------------------"    
echo  " --------------------- UKMET_PREGRID start ----------------------------"
echo  " ----------------------------------------------------------------------"    
###############################################################################
#set echo

## Set debugging option (0 no debugging)

set debug = 0

## Subsystem name 

set timestamp
setenv SUBSYSTEM PREGRID_UKMET
setenv RM "rm -rf"

## ENVIRONMENT

set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo " ${SUBSYSTEM} --  Missing ConfigFile -> exiting" 
 exit (1)
endif
source ${CFILE}user.mm5sys.${MM5HOST}
source ${CFILE}sizes.mm5sys.${MM5HOST}



## Check usage

	echo "$0 $argv[*]"
	echo ""

if      ( ${#argv} <= 4 )  then
	echo "USAGE: RT_A_regrid_ukmetnc.csh  ukmet_date cycle_date start_date end_date cold_start"
	echo "where ukmet_date is the 0 h of the UKMET fcst start"
	echo "cycle_date is the time of this_cycle, and "
	echo "start_date is the time when this_cycle's fcst starts"
	echo  $end_date is the needed hour of current UKMET fcst"
	echo "where all are given in CCYYMMDDHH"
	echo ""
	exit ( 2 ) 
endif

## Assign the input variables.

set avn_date = $1
set cycle_date = $2
set start_date = $3
set end_date = $4
set cold_start = $5


## Choose which UKMET data to access (ie, 00 Z or 12 Z), and 

set day_to_get = `echo $avn_date | cut -c 1-8`
set time_to_access = `echo $avn_date | cut -c 9-10`

if ($time_to_access >= 12) then
set  time_to_access =  12 
else
set  time_to_access =  00
endif

## Debugging only

if ($debug > 0) then
 setenv RUNDIR /data/cycles/vandenb/ukmet
 setenv OUTPUT_DATA_DIR /data/input/ukmet
 setenv GEAPSTMP GEAPSTMP
 setenv GSJOBDIR GSJOBDIR
endif

## Check directories existence

$MustHaveDir $RUNDIR/$cycle_date 
setenv  DECODING_AVN_DIR  ${RUNDIR}/${cycle_date}/AVN_REGRID

if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/AVN_REGRID
ln -s $GEAPSTMP/AVN_REGRID  $DECODING_AVN_DIR
else
$MustHaveDir $DECODING_AVN_DIR
endif

## Make sure the executable exists

if ( ! -e  ${EXECUTABLE_ARCHIVE}/pregrid_ukmetnc.exe ) then
 echo
 echo "${SUBSYSTEM}  Missing file -> ${EXECUTABLE_ARCHIVE}/pregrid_ukmetnc.exe"
 exit (6)
endif

## Split input files

#/raid/fddahome/datbin/split_ukmetnc.pl ${avn_date} ${OUTPUT_DATA_DIR}

## Normally we should have 11 ukmet (6hr) files for 60 hours

@ num_avn = 11

## Go to the data directory

set OUTPUT_DATA_DIR = /raid/ukmet-input-nc/${day_to_get}
pushd $OUTPUT_DATA_DIR

## Look for UKMET NETCDF FILES

#if (! -e  ukmet.${day_to_get}.i${time_to_access}00.nc) then
#echo "ERROR: Cannot file ukmet.${day_to_get}.i00${time_to_access}.nc in $OUTPUT_DATA_DIR"
#exit (1)
#endif

## Split file into single time levels

#${EXECUTABLE_ARCHIVE}/split_ukmetnc.exe -i ukmet.${day_to_get}.i${time_to_access}00.nc -k ukmet -debug

## Look for single time level files

if ( `find . -name ${day_to_get}\*_ukmet.\*.nc -print | wc -l` >= $num_avn ) then
  echo "INFO: using  UKMET data on data disk"
 foreach l ( `find . -name ${avn_date}_ukmet.\*.nc -print` )
   set newn = `echo $l | cut -c 3-24` # e.g., 2002013012_ukmet.0000.nc
   set l1   = `echo $l | cut -c 3-`   # eg.., 2002013012_ukmet.0000.nc
   rm -f ${RUNDIR}/${cycle_date}/${newn}
   ln -s ${OUTPUT_DATA_DIR}/${l1} $DECODING_AVN_DIR/$l1
 end

else
 echo "${SUBSYSTEM}: Could NOT find $num_avn files on the DISK of the form ${avn_date}_ukmet.\*.nc"
  exit (3)
endif

## Go back to original directory

popd

## Where we want the data to go, and just head over there.

cd $DECODING_AVN_DIR
echo "Now working in  $cwd"


## Build the starting and ending dates to deGRIB.

#echo "$start_date , 48" >! input
#advance_cymdh < input >! output
#set end_date = `cat output`

set ltrs = ( A B C D E F G H I J K L M N O P Q R S T U V W X Y Z )
set icount = 0

## Use UKMET files every 6hr until +60hr

foreach f ( ${avn_date}_ukmet.0000.nc \
            ${avn_date}_ukmet.0006.nc \
            ${avn_date}_ukmet.0012.nc \
            ${avn_date}_ukmet.0018.nc \
            ${avn_date}_ukmet.0024.nc \
            ${avn_date}_ukmet.0030.nc \
            ${avn_date}_ukmet.0036.nc \
            ${avn_date}_ukmet.0042.nc \
            ${avn_date}_ukmet.0048.nc \
            ${avn_date}_ukmet.0054.nc \
            ${avn_date}_ukmet.0060.nc )

        @ icount ++
        set ltr = $ltrs[$icount]

        if ( -e $DECODING_AVN_DIR/${f} ) then
          ln -s $DECODING_AVN_DIR/$f GRIBFILE.A${ltr}

          #     List the files to read in the degribber.

          echo "$DECODING_AVN_DIR/$f" >>! input
        else
          @ icount --
        endif

end

## The TERRAIN file has the record header stuff,
## plus all of the 2d constant fields (latitude, longitude, etc.).

if (! -e TERRAIN) then
if ( -e $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN1 ) then
  ln -s $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN1 TERRAIN
 echo "${SUBSYSTEM}: Using Terrain in $GSJOBDIR "
else if ( -e $TERRAIN_DIR/Domain1_New_LU.V.${MM5HOST} ) then
       ln -s $TERRAIN_DIR/Domain1_New_LU.V.${MM5HOST} TERRAIN
 echo "${SUBSYSTEM}: Using Terrain in $TERRAIN_DIR"
else
 echo "${SUBSYSTEM}: Missing file -> NO terrain in $GSJOBDIR or $TERRAIN_DIR"
 exit (4)
endif
endif

## Build the namelists.

set yyyy_start = `echo $start_date | cut -b 1-4`
set mm_start = `echo $start_date | cut -b 5-6`
set dd_start = `echo $start_date | cut -b 7-8`
set hh_start = `echo $start_date | cut -b 9-10`

## Time difference in s

set yyyy_end = `echo  $end_date | cut -b 1-4`
set mm_end = `echo  $end_date | cut -b 5-6`
set dd_end = `echo  $end_date | cut -b 7-8`
set hh_end = `echo  $end_date | cut -b 9-10`

## Output every hour

set time_int = 3600
if($cold_start == 1) set time_int = 10800

## Bring regrid namelist template

if (-f pregrid.namelist) mv pregrid.namelist AVN.pregrid.namelist
if (-f Vtable) mv Vtable AVN.Vtable

cp ${PREGRID_NML_TEMPL} pregrid.namelist

## Update namelist template with parameters

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

#g/\//s//ORDERED_BY_DATE = .FALSE. \//

## Get the Vtable:

#echo "${SUBSYSTEM}: Use table $CONSTANT_FILES/PREGRID_VTABLE.UKMET3D"
#   cp $CONSTANT_FILES/PREGRID_VTABLE.UKMET3D Vtable

## Rename previous output files.

if (-e eta_date_found) mv eta_date_found avn_date_found
 
set FILES = `find . -name FILE\* -print`

if ($#FILES > 0) then 
 foreach ff ($FILES)
   set f = `echo $ff |cut -c3-`
   if (-f $f) then
       mv $f AVN${f}
   endif
 end
else

echo "AVN File FILES:CCYY-MM-DD_HH are missing from previous regrid."
exit (5)

endif

## Run the UKMET GRIB decoder.


echo "--------------- pregrid UKMET -----------------------" >! print_ukmet.out_${avn_date}

( time ${EXECUTABLE_ARCHIVE}/pregrid_ukmetnc.exe ) >>! print_ukmet.out_${avn_date}

## Clean-up

rm GRIBFILE.* 

#find . -name TERRAIN -type l -exec rm {} \;

## Catenate UKMET upper-air with AVN surface fields

echo ""
echo "-- Catenate UKMET upper-air with AVN surface fields --" >>! print_ukmet.out_${avn_date}

set FILES = `find . -name FILE\* -print`

echo $FILES

if ($#FILES > 0) then 

 foreach ff ($FILES)

   set f = `echo $ff |cut -c3-`

## Remove UKMET upper-air fields from AVN files

   if (-f AVN${f}) then
       ${EXECUTABLE_ARCHIVE}/notinukmet.exe AVN$f >>! print_ukmet.out_${avn_date}
   else
       echo "Problems in regrid: file AVN$f is missing."
       exit (7)
   endif

## Catenate files

   if ((-f ${f}) && (-f AVN${f}.notinukmet)) then
         echo "cat AVN${f}.notinukmet >>! $f" >>! print_ukmet.out_${avn_date}
               cat AVN${f}.notinukmet >>! $f
             rm -f AVN${f}.notinukmet AVN${f}.inukmet 
   else
         echo "File ${f} or AVN${f}.notinukmet is missing."
         exit (9)
   endif

 end

else
echo "UKMET File FILES:CCYY-MM-DD_HH are missing, problems in regrid."
exit (10)
endif

## The directory where the forecast will run is based upon the 0 h
## of the MM5 forecast, not the AVN date. Check that this is the right AVN 
## forecast.

echo "$avn_date" >&! eta_date_found

if ( ! -e eta_date_found ) then           # ideally should be eta_date_found
  echo "${SUBSYSTEM} -- Program Error - NO eta_date_found produced"
# exit ( 11 )
else
  mv eta_date_found ukmet_date_found
endif

if ( `cat ukmet_date_found` != $avn_date ) then
	echo "time mismatch"
	set t = `cat ukmet_date_found`
	echo  "$t $avn_date"
	echo "${SUBSYSTEM} --AVN date-in-file=${t} - but we want ${avn_date}"
	exit ( 12 )
endif

if ( -e avn_date_found ) then           # ideally should be eta_date_found
    mv avn_date_found eta_date_found
endif

## Clean up

if ($debug == 0) then
find . -name ukmet.${day_to_get}.i${time_to_access}00.nc -exec rm {} \;
endif
find . -name AVNFILE\* -exec rm {} \;

#rm eta_date_found          # ideally should be named avn_date_found

## end

echo "${SUBSYSTEM}: Succesful completion for ${avn_date}"

exit ( 0 )

#!/bin/csh -f

#  This shell extract GRIB 2.5-deg NNRP data for a given time period.
###############################################################################
echo
echo   " ----------------------------------------------------------------------"
echo   " --------------------- RT_get_nnrp starts -----------------------------"
echo "$0 $argv[*]"
echo   " ----------------------------------------------------------------------"
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

if ($#argv != 5) then
  echo ""
  echo "Usage: RT_get_nnrp.csh cycle_date startdate enddate itimint DATADIR"
  echo "-----"
  echo "With startdate/enddate =  CCYYMMDDHH"
  echo "and  itimint is a multiple of 6."
  echo "NNRP data shall reside at DATADIR/NNRP"
  exit (1)
endif

set cycle_date = $1 # 1998120102
set startdate  = $2 # 1998120100
set enddate    = $3 # 1998120200
set itimint    = $4 # 6
set DATADIR    = $5 # fddasys@ngic-c1:/input

# Paths to input data ($DATADIR/NNRP would represent /DSS in MSS)
#                     (ncardata is actually the name of NCAR ftp server)
set ncardata = $DATADIR/ncardata/datasets/ds090.0/inventories
set LocalDataDir = $DATADIR/NNRP

#------------------------------------------------------------------------------#
# Go to work dir and clean

$MustHaveDir $RUNDIR/$cycle_date
setenv DECODING_NNRP_DIR  ${RUNDIR}/${cycle_date}/NNRP_REGRID

if (-d $GEAPSTMP) then
  $MustHaveDir $GEAPSTMP/NNRP_REGRID
  ln -s $GEAPSTMP/NNRP_REGRID  $DECODING_NNRP_DIR
else
  $MustHaveDir $DECODING_NNRP_DIR
endif

cd ${RUNDIR}/${cycle_date}/NNRP_REGRID
echo "Now working in  $cwd"


find . -name FLWANT_\* -exec rm {} \;

#------------------------------------------------------------------------------#
# Former script fetch.csh:
# Set up the lists "mdate" and "sfcdate".  "mdate" will hold all the 
# upper-air times we need to get.  "sfcdate" will hold all the surface
# times we need to get.  "mdate" and "sfcdate" are not the same, because
# some of the surface analyses are actually 6-hour forecasts.

set curn = `echo ${startdate} | cut -c 1-4,5-6,7-8,9-10`    # CCYYMMDDHH
set endn =   `echo ${enddate} | cut -c 1-4,5-6,7-8,9-10`

set num = 0
set idth = -$itimint
set mdate = ( )
set sfcdate = ( )
while ( $curn < $endn )
   @ idth = $idth + $itimint
   @ num ++
   set ndate = `$EXECUTABLE_ARCHIVE/geth_newdate.exe ${startdate} ${idth}`
   set sdate = `$EXECUTABLE_ARCHIVE/geth_newdate.exe ${ndate} -6`
   set mdate = ( ${mdate} ${ndate} )
   set sfcdate = ( ${sfcdate} ${sdate} )
   set curn = `echo ${ndate} | cut -c 1-4,5-6,7-8,9-10`   # CCYYMMDDHH
end

echo "Requested dates for NNRP upper-air fields: $mdate"
echo "Requested dates for NNRP surface fields:   $sfcdate"

# Set up the ftp command-script to get the archive lists.  First, set 
# up to login anonymously to ncardata.ucar.edu

# Construct the dates as they appear in the upper-air archive titles.

set YearSave = 0

set DssFile3d = ( )
set DssFile2d = ( )

foreach ndate ( $mdate ) 
    
   set YYYY = `echo $ndate | cut -b 1-4`
   set YYMM = `echo $ndate | cut -b 3-4,5-6` # CCYYMMDDHH

   if ( $YYYY != $YearSave ) then
      set YearSave = $YYYY

      set File3d = ${YYYY}_A.list
      if ( $YYYY == 1976 || $YYYY == 1977 || $YYYY == 1978 || $YYYY == 2004 ) then
        set File3d = ${YYYY}rerun_A.list
      endif
      if ( $YYYY == 1997 || $YYYY == 1998 || $YYYY == 1999 || $YYYY == 2000 ) then
        set File3d = ${YYYY}tovsrerun_A.list
      endif

      echo "Get ${File3d} from $ncardata"
      cp $ncardata/${File3d} ${File3d} 
   endif

end

# Construct the dates as they appear in the surface archive titles.

set YearSave = 0
foreach ndate ( $sfcdate ) 

   set YYYY = `echo $ndate | cut -b 1-4`
   set YYMM = `echo $ndate | cut -b 3-4,5-6` # CCYYMMDDHH

   if ( $YYYY != $YearSave ) then
      set YearSave = $YYYY

      set File3d = ${YYYY}_A.list
      if ( $YYYY == 1976 || $YYYY == 1977 || $YYYY == 1978 || $YYYY == 2004) then
        set File3d = ${YYYY}rerun_A.list
      endif
      if ( $YYYY == 1997 || $YYYY == 1998 || $YYYY == 1999 || $YYYY == 2000 ) then
        set File3d = ${YYYY}tovsrerun_A.list
      endif
      set File2d = ${YYYY}.2D

      echo "Get ${File3d} from $ncardata"
      cp $ncardata/${File3d} ${File2d}

   endif

end

# Use awk to get the actual MSS filenames from the archive lists.

# Get the upper-air analysis tar files.

echo "Upper-air data files to be used:"

set YearSave = 0
foreach ndate ( $mdate ) 

   set YYYY = `echo $ndate | cut -b 1-4`
   set YYMM = `echo $ndate | cut -b 3-4,5-6` # CCYYMMDDHH

   if ( $YYMM != $YearSave ) then
      set YearSave = $YYMM

      set File3d = ${YYYY}_A.list
      if ( $YYYY == 1976 || $YYYY == 1977 || $YYYY == 1978 || $YYYY == 2004 ) then
        set File3d = ${YYYY}rerun_A.list
      endif
      if ( $YYYY == 1997 || $YYYY == 1998 || $YYYY == 1999 || $YYYY == 2000 ) then
        set File3d = ${YYYY}tovsrerun_A.list
      endif

      if ( ! -e $File3d ) then
         echo "Archive listing file $File3d not found"
         exit (2)
      endif

      set ssFile3d = `awk -v YYMM=$YYMM '{ if ( $6 == YYMM".pgb.f00" ) { print substr($1,3,6) } }' $File3d `

      echo $ssFile3d
      if ( $ssFile3d == ) then
         echo "*******  Date $YYMM not found in archive list.   *******"
         exit (1)
      endif

      set local = NNRP_GRIB_UPA.${YYMM}

      if (! -e $local && -e $LocalDataDir/$ssFile3d) then
         echo "Found $ssFile3d in local NNRP directory. Copy over"
         cp $LocalDataDir/$ssFile3d $local
      endif

      if (! -e $local) then
         echo "Could not find $ssFile3d in local directory"
         echo "ERROR: file ${ssFile3d} is missing"
	 exit (3)
      endif
   endif
end
sleep 5

# Get the surface analysis tar files.

echo "Surface data files to be used:"

set YearSave = 0
foreach ndate ( $sfcdate ) 

    set YYYY = `echo $ndate | cut -b 1-4`
    set YYMM = `echo $ndate | cut -b 3-4,5-6` # CCYYMMDDHH

   if ( $YYMM != $YearSave ) then
      set YearSave = $YYMM

      set File2d = ${YYYY}.2D
      set ssFile2d = `awk -v YYMM=$YYMM  '{ if ( $6 == YYMM".2D" ) { print substr($1,3,6) } }' $File2d `
      echo $ssFile2d

      set local = NNRP_GRIB_SFC.${YYMM}
      if ( ! -e $local && -e $LocalDataDir/$DssFile2d ) then 
         echo "Found $ssFile2d in local NNRP directory. Copy over"
	 cp $LocalDataDir/$ssFile2d $local
      endif

      if ( ! -e $local ) then
         echo "ERROR: file ${ssFile2d} is missing"
	 exit(2)
      endif
   endif
end
sleep 5

# Untar the upper-air analyses that we want from the tarfiles.

foreach  ndate ( $mdate )

   set YYMM = `echo $ndate | cut -b 3-4,5-6` # CCYYMMDDHH
   set mdd = `echo $ndate | cut -b 3-4,5-6,7-8,9-10` # CCYYMMDDHH
   set flwant = 'pgb.f00'${mdd}

   if ( ! -e $flwant ) then
      tar xf NNRP_GRIB_UPA.${YYMM} ${flwant}
      if ( ! -e ${flwant} ) then
          tar xf NNRP_GRIB_UPA.${YYMM} ./${flwant}
      endif
   endif

   if ( ! -e ${flwant} ) then
        echo "ERROR: cannot extract file ${flwant}"
        exit (2)
   endif

   mv  ./$flwant ./UPANNRP_${flwant}

end

# Untar the surface analyses that we want from the tarfiles.

foreach ndate ( $sfcdate )
   set YYMM = `echo $ndate | cut -b 3-4,6-7` # CCYY-MM-DD+HH
   set YYMM = `echo $ndate | cut -b 3-4,5-6` # CCYYMMDDHH
   set sdate = `$EXECUTABLE_ARCHIVE/geth_newdate.exe ${ndate} 6`

   if ( ! -e FLWANT_SFC.${YYMM} ) then

     set flwant = ( )
     set flwant = ( $flwant 'TMPhag.2.'${YYMM} )  # Temperature at 2 m AGL
     set flwant = ( $flwant 'TMPsfc.'${YYMM} )
     set flwant = ( $flwant 'SOILWdlr.10.'${YYMM} )
     set flwant = ( $flwant 'SOILWdlr.200.'${YYMM} )
     set flwant = ( $flwant 'SPFHhag.2.'${YYMM} )
     set flwant = ( $flwant 'TMPdlr.10.'${YYMM} )
     set flwant = ( $flwant 'TMPdlr.200.'${YYMM} )
     set flwant = ( $flwant 'TMPdpl.300.'${YYMM} )
     set flwant = ( $flwant 'ICECsfc.'${YYMM} )
     set flwant = ( $flwant 'LANDsfc.'${YYMM} )     # Land(1)/Sea(0) Mask
     set flwant = ( $flwant 'PRESsfc.'${YYMM} )
     set flwant = ( $flwant 'UGRDhag.10.'${YYMM} )  # U at 10 m AGL
     set flwant = ( $flwant 'VGRDhag.10.'${YYMM} )  # V at 10 m AGL
     set flwant = ( $flwant 'WEASDsfc.'${YYMM} )    # Water Equivalent Snow Depth
     echo $flwant

     foreach file ( $flwant )

        echo $file

        if ( ! -e ${file} ) then
	    tar xf NNRP_GRIB_SFC.${YYMM} ${file}
	endif

        if ( ! -e ${file} ) then
	    tar xf NNRP_GRIB_SFC.${YYMM} ./${file}
	endif

        if ( ! -e ${file} ) then
                echo "ERROR: cannot extract file ${file}"
                exit (2)
        endif

	mv  ./$file ./SFCNNRP_${file}

     end
 endif
end

cd $RUNDIR/$cycle_date
echo "Now working in  $cwd"

exit (0)

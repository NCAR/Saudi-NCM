#!/bin/tcsh -f
#------------------------------------------------------------------------------#
#	This shell fetches data from the NCAR MSS system and
#	converts it into a format suitable for the little_r
#	program. 
#
#       Three types of data files are created:
#             obs:DATE             : Upper-air and surface data used as
#                                    input to little_R
#             surface_obs_r:DATE   : Surface data needed for FDDA in lillte_r
#                                    (if no FDDA will be done, these are not
#                                    needed, since they are also contained
#                                    in obs:DATE)
#             upper-air_obs_r:DATE : Upper-air data (this file is contained
#                                    in obs:DATE file, and is not needed for
#                                    input to little_r)
#
#------------------------------------------------------------------------------#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- ADP decoder starts  ---------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################
#

#set echo
set timestamp
setenv SUBSYSTEM ADP_FDDA

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
  echo "  ${SUBSYSTEM} -- Missing ConfigFile -> exiting"
  exit (1)
endif
source ${CFILE}user.mm5sys.${MM5HOST};    

#
#
# Environment from MM
#
if(-e $GSJOBDIR/tmp/$this_cycle/cshrc) then
  source $GSJOBDIR/tmp/$this_cycle/cshrc
  ${PERL_ARCHIVE}/MMPerlLog.perl "INFO" "Found and Sourced $GSJOBDIR/tmp/$this_cycle/cshrc"
endif

#	Check usage

if ( ${#argv} == 3 ) then
   set cycle_date = $1
   set start_date = $2
   set end_date   = $3
else
   echo "usage: $0 cycle_date start_date end_date"
   echo "where dates are CCYYMMDDHH"
   exit (2)
endif

#	Does the directory exist

$MustHaveDir $RUNDIR/${cycle_date}

if (-d $GEAPSTMP) then
   $MustHaveDir $GEAPSTMP/RD_ADP
   if (! -e $RUNDIR/${cycle_date}/RD_ADP) \
      ln -s $GEAPSTMP/RD_ADP/ $RUNDIR/${cycle_date}/RD_ADP
else
   $MustHaveDir $RUNDIR/${cycle_date}/RD_ADP
endif

# Remove any existing output files

if (-d $GEAPSTMP) then
  find $GEAPSTMP -name \*_ADP_data.${MM5HOST} -exec rm {} \;
endif

# Go to the work directory

cd $RUNDIR/${cycle_date}/RD_ADP
echo "Now working in  $cwd"

find . -name \*_ADP_data.${MM5HOST} -exec rm {} \;
find . -name upper-air_obs_r:\* -exec rm {} \;
find . -name surface_obs_r:\* -exec rm {} \;
find . -name  print.out_adp_sfc -exec rm {} \;
find . -name  print.out_adp_upa -exec rm {} \;

#------------------------------------------------------------------------------#

set starting_date = $start_date
set ending_date   = $end_date

# The amount of CPU time spent unpacking the entire
# globe is embarrassing.  As much as possible, restrict the 
# bounds.

set lon_e         =  180
set lon_w         = -180
set lat_s         =  -90
set lat_n         =   90

##############################################
#	End of user modification
##############################################

set LETTERS = (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)

# Bring the namelist template

if (! -f $CONSTANT_FILES/ADPSFC.namelist.template) then
    echo ""
    echo "ERROR: cannot find file: $CONSTANT_FILES/ADPSFC.namelist.template"
    exit (2)
else
    cp -f $CONSTANT_FILES/ADPSFC.namelist.template .
endif

#  Build the conversion routine, set the namelist up.

#make
m4 -Dxstryy=`echo $starting_date | cut -c1-4`  \
   -Dxstrmm=`echo $starting_date | cut -c5-6`  \
   -Dxstrdd=`echo $starting_date | cut -c7-8`  \
   -Dxstrhh=`echo $starting_date | cut -c9-10` \
   -Dxendyy=`echo $ending_date | cut -c1-4`  \
   -Dxendmm=`echo $ending_date | cut -c5-6`  \
   -Dxenddd=`echo $ending_date | cut -c7-8`  \
   -Dxendhh=`echo $ending_date | cut -c9-10` \
   -Dxloe=$lon_e -Dxlow=$lon_w -Dxlas=$lat_s -Dxlan=$lat_n \
   ADPSFC.namelist.template >! namelist.input

# Run the program (convert data to little_r format).
# At least a single file must exist for input.

if ( -e A.1     || -e B.1     || -e E.1     || -e F.1     || \
     -e SFC_A.1 || -e SFC_B.1 || -e SFC_E.1 || -e SFC_F.1 ) then

  ( $EXECUTABLE_ARCHIVE/adp_sfc.exe SFC* ) >>! print.out_adp_sfc

# Put output data where they can be found	

  set sfc_files = `find . -name surface_obs_r\:\* -size +100c -print`

  if ($#sfc_files <= 0) then
    echo "No surface files were generated for cycle $cycle_date"
  else

    foreach fil ( surface_obs_r:* )
      set ccyy = `echo $fil |cut -c15-18`
      set mm   = `echo $fil |cut -c20-21`
      set dd   = `echo $fil |cut -c23-24`
      set hh   = `echo $fil |cut -c26-27`
      echo "Find surface file: $fil"

      if (! -e ${ccyy}${mm}${dd}${hh}_ADP_data.${MM5HOST}) then
        cp $fil ${ccyy}${mm}${dd}${hh}_ADP_data.${MM5HOST}
      else
        cat $fil >> ${ccyy}${mm}${dd}${hh}_ADP_data.${MM5HOST}
      endif
    end

  endif
else
  echo "Cannot find an input file for adp_sfc.exe, skip to upper-air..."
endif

# Pull ADP upper-air data from the MSS (done in RT_get_obs_ADP_UPA.csh)
# Bring the namelist template

if (! -f $CONSTANT_FILES/ADPUPA.namelist.template) then
  echo ""
  echo "ERROR: cannot find file: $CONSTANT_FILES/ADPUPA.namelist.template"
  exit (2)
else
  cp -f $CONSTANT_FILES/ADPUPA.namelist.template .
endif

# Build the conversion routine, set the namelist up.

m4 -Dxstryy=`echo $starting_date | cut -c1-4`  \
   -Dxstrmm=`echo $starting_date | cut -c5-6`  \
   -Dxstrdd=`echo $starting_date | cut -c7-8`  \
   -Dxstrhh=`echo $starting_date | cut -c9-10` \
   -Dxendyy=`echo $ending_date | cut -c1-4`  \
   -Dxendmm=`echo $ending_date | cut -c5-6`  \
   -Dxenddd=`echo $ending_date | cut -c7-8`  \
   -Dxendhh=`echo $ending_date | cut -c9-10` \
   -Dxloe=$lon_e -Dxlow=$lon_w -Dxlas=$lat_s -Dxlan=$lat_n \
   ADPUPA.namelist.template >! namelist.input

# Run the program (convert data to little_r format).
# At least a single file must exist for input.

if (  -e A.1     || -e B.1     || -e D.1     || -e F.1       || \
      -e UPA_A.1 || -e UPA_B.1 || -e UPA_D.1 || -e UPA_F.1 ) then

  ( $EXECUTABLE_ARCHIVE/adp_upa.exe UPA* ) >>! print.out_adp_upa

# Put output data where they can be found	

  set adp_files = `find . -name upper-air_obs_r\:\* -size +100c -print`

  if ($#adp_files <= 0) then
    echo "No upper-air files were generated for cycle $cycle_date"
  else

    foreach fil ( upper-air_obs_r:* )
      set ccyy = `echo $fil |cut -c17-20`
      set mm   = `echo $fil |cut -c22-23`
      set dd   = `echo $fil |cut -c25-26`
      set hh   = `echo $fil |cut -c28-29`
      echo "Find upper-air file: $fil"

      if (! -e ${ccyy}${mm}${dd}${hh}_ADP_data.${MM5HOST}) then
        cp $fil ${ccyy}${mm}${dd}${hh}_ADP_data.${MM5HOST}
      else
        cat $fil >> ${ccyy}${mm}${dd}${hh}_ADP_data.${MM5HOST}
      endif

    end
  endif

else
  echo "Cannot find an input file for adp_upa.exe."
endif

#------------------------------------------------------------------------------#
# Clean up house

#echo "Content of directory $RUNDIR/${cycle_date}/RD_ADP:"
#ls -al

cd $RUNDIR/${cycle_date}

exit (0)


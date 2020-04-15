#!/bin/csh -f
#------------------------------------------------------------------------------#
#
# RT_G_rtfdda_wrfqc.csh: Main driver for WRF QC applications.
#
# Usage: RT_G_rtfdda_wrfqc.csh this_cycle last_cycle start_date node stage"
# -----                        <save_data>
#
# Where: 
# -----
#  this_cycle = current cycle  date and hour (CCYYMMDDHH)
#  last_cycle = previous cycle date and hour (CCYYMMDDHH)
#  start_date = date and time of the first observations to be QCed (CCYYMMDDHH)
#  stage: WRF_F = final analysis; (WRF_F) or a forecast (WRF_P)
#  save_data = 1 will create scatter plots (optional)
#
#
# Output:
# ------
#  Files cyymmddhh_qc_obs_for_assimilation_s where ccyymmddh is the time
#  of validity for QCed observations.
#
# Files/functions already used by WRF FDDA and called from this script:
# --------------------------------------------------------------------
#
#  ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe: Compute new date from a date string 
#                                          (CCYYMMDDHH) and a time in hours.
#
#  $GSJOBDIR/namelists/wrf.nl.template: WRF namelist template for the WRF job.
#
#  $CONSTANT_FILES/wrfrun/LANDUSE.TBL:  The land use table used by thes WRF run.
#
#  $EXECUTABLE_ARCHIVE/wrf.nl.nest.pl:  WRF namelist utility used by the WRF run
#
#
# Additional files/Functions:
# --------------------------
#
#  $CONSTANT_FILES/WRFVAR.namelist.template: Template for WRF_VAR namelist
#                                            with QC records appended. 
#                                            Available from CVS at:
#                                        "apps/4dwx/RTFDDA/SOURCE/WRFV2_VAR/run"
#
#  $EXECUTABLE_ARCHIVE/RT_all.obs_trim-merge.USA: Utility to sort obs available
#                                                 from CVS at:
#                                            "apps/4dwx/RTFDDA/scripts/Forecast"
#
#  $CSH_ARCHIVE/../POSTPROCS/plot_scatterqc.ncl: NCL script to create scatter 
#                                                plots of QCed data. 
#                                                Available from CVS at:
#                                        "apps/4dwx/RTFDDA/SOURCE/WRFV2_VAR/run"
#
#  $EXECUTABLE_ARCHIVE/wrfqc.exe:     Executable built from source code
#                                     WRFV2_VAR available from CVS at:
#                                     "apps/4dwx/RTFDDA/SOURCE/WRFV2_VAR"
#                                     Executable can be build by entering
#                                     the sequence of commands under WRFV2_VAR
#                                     - "configure qc"(Select "1" when prompted)
#                                     - "compile qc"
#                                     The executable "wrfqc.exe" will be
#                                     created under "WRFV2_VAR/main"
#
#  $EXECUTABLE_ARCHIVE/latlon_wrf.ex: Executable built from source code
#                                     WRFV2_VAR available from CVS at:
#                                     "apps/4dwx/RTFDDA/SOURCE/WRFV2_VAR"
#                                     Executable can be build by entering the
#                                     command "make latlon_wrf.exe" under
#                                     "WRFV2_VAR/external/io_netcdf"
#
#
# This script is designed should be called by rtfddaflex.pl where the MM5
# companion script RT_G_rtfdda_mm5qc.csh is called, but under the "WRF"
# conditional switch. An example to call the script with plots
# for analysis and no plot for forecast is:
#
#  print FILEL "\n   starting F-stage  rap_rtfdda.csh at ", &ctime(time);
#  if($MODEL eq "MM5") {
#   system("$CSH_ARCHIVE/Forecast/RT_G_rap_rtfdda_mm5qc.csh $OBS_start $this_cycle $this_cycle $last_cycle");
#  } elsif ($MODEL eq "WRF") {
#   system("$CSH_ARCHIVE/Forecast/RT_G_rtfdda_wrfqc.csh $this_cycle $last_cycle $OBS_start $NUM_PROCS WRF_F"); 
#  }
#
# and
#
#  print FILEL "\n   starting P-stage rap_rtfdda.csh at ", &ctime(time);
#  if($MODEL eq "MM5") {
#   system("$CSH_ARCHIVE/Forecast/RT_G_rap_rtfdda_mm5qc.csh $p_fdda_start $p_fdda_end $this_cycle $last_cycle");
#  } elsif ($MODEL eq "WRF") {
#   system("$CSH_ARCHIVE/Forecast/RT_G_rtfdda_wrfqc.csh $this_cycle $last_cycle $OBS_start $NUM_PROCS WRF_P");
#  }
#                                        
#------------------------------------------------------------------------------#
# Copyright UCAR (c) 2005 - 2006.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Laboratory (RAL),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#
# Francois Vandenberghe, vandenb@ucar.edu, November 2006.
#------------------------------------------------------------------------------#
echo ""
echo " -----------------------------------------------------------------------"
echo " --------------------------------- WRF QC  -----------------------------"
echo " -----------------------------------------------------------------------"
echo ""
echo "$0 $argv[*]"
echo ""
#------------------------------------------------------------------------------#
# Environment
#------------------------------------------------------------------------------#
# set echo

set timestamp
setenv SUBSYSTEM WRF
setenv RM "rm -rf"

set DOMS = ( 1 2 3 4 )
if($?NUM_DOMS) then
 if ($NUM_DOMS == 5) then
  set DOMS = ( 1 2 3 4 5 )
 else if ($NUM_DOMS == 4) then
  set DOMS = ( 1 2 3 4)
 else if ($NUM_DOMS == 3) then 
  set DOMS = ( 1 2 3)
 else if ($NUM_DOMS == 2) then 
  set DOMS = ( 1 2 )
 else if ($NUM_DOMS == 1) then 
  set DOMS = ( 1 )
 endif
endif

set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit -1
endif

source ${CFILE}user.mm5sys.${MM5HOST};    

#------------------------------------------------------------------------------#
# Parse arguments
#------------------------------------------------------------------------------#

if ( ${#argv} < 6) then
        echo ""
	echo "Usage: RT_G_rtfdda_wrfqc.csh this_cycle last_cycle start_date node stage <plot_scatterqc>"
        echo "-----"
        echo ""
	echo "Where:" 
        echo "-----"
        echo " this_cycle = current cycle  date and hour (CCYYMMDDHH)"
        echo " last_cycle = previous cycle date and hour (CCYYMMDDHH)"
        echo " start_date = WRF initialization data and time (CCYYMMDDHH)"
	echo " node:  0 = mpp; 1 = running omp on node"
        echo " stage: WRF_F = final analysis; (WRF_F) or a forecast (WRF_P)"
        echo " save_data = 1 will save input/output data in directory RUNDIR/saveObs"
	exit -1
endif

## Parse mandatory arguments

set this_cycle = $1
set last_cycle = $2
set start_date = $3
set node       = $4
set stage      = $5

## Parse optional arguments
if (${#argv} >= 6) then
    set save_data = $6
else
    set save_data = 0
endif


# Add more print-out
set debug = 1

# Create scatter plots after processing data (can be slow)
set plotobs = 0

#-----------------------------------------------------------------------------#
echo \
"------------------------------------------------------------------------------"
echo "Pre-processing:"

if ($stage == "WRF_F") then
## Process all observation files from start_time to cycle_time - 1hr
   echo "$this_cycle , -1" >! input
   ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
   set end_date = `cat output`

else if ($stage == "WRF_P") then
## Process all observation files from start_time to cycle_time + 1hr
   echo "$this_cycle , +1" >! input
   ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
   set end_date = `cat output`
endif

## Print-out
echo
echo "This cycle time is:   $this_cycle"
echo "Previous cycle was:   $last_cycle"
echo "Model will start at:  $start_date"
echo "Model will stop  at:  $end_date"
echo

## Break down date string
set y_start = `echo $start_date | cut -b 1-4`
set m_start = `echo $start_date | cut -b 5-6`
set d_start = `echo $start_date | cut -b 7-8`
set h_start = `echo $start_date | cut -b 9-10`

set y_end = `echo $end_date | cut -b 1-4`
set m_end = `echo $end_date | cut -b 5-6`
set d_end = `echo $end_date | cut -b 7-8`
set h_end = `echo $end_date | cut -b 9-10`

#------------------------------------------------------------------------------#
# QC data files are stored on the main disk under RAP_RTFDDA
#------------------------------------------------------------------------------#

set CYCDIR = $RUNDIR/$this_cycle
set CYCDIR_LAST = $RUNDIR/$last_cycle

set RAP_RTFDDA = $CYCDIR/RAP_RTFDDA

if (! -d $RAP_RTFDDA) $mkdir -p $RAP_RTFDDA

echo
echo "QC files stored unbder $RAP_RTFDDA"

#------------------------------------------------------------------------------#
# Workdir on compute node's local disk
#------------------------------------------------------------------------------#

if ($stage == "WRF_P") then   # Preliminary + Forecast FDDA cycle
    set tmp_work = WRFQC_P
else                          # Final analsys FDDA cycle
    set tmp_work = WRFQC_F
endif

if (-d $GEAPSTMP) then
# Will work on local disk
   set up_dir = $GEAPSTMP
   set dir_work = $up_dir/$tmp_work
   rm -rf $dir_work
   if (! -d $dir_work) $mkdir -p $dir_work
   ln -sf $dir_work $CYCDIR/$tmp_work
else
# Will work on main disk
   set up_dir = $CYCDIR
   set dir_work = $up_dir/$tmp_work
   rm -rf $dir_work
   $mkdir -p $dir_work
endif

#------------------------------------------------------------------------------#
# Go to Work dir
#------------------------------------------------------------------------------#

echo
echo "WORKDIR is $tmp_work"
cd  $tmp_work

#------------------------------------------------------------------------------#
# Bring ncl plot script if requested
#------------------------------------------------------------------------------#

if ($plotobs > 0) then
    if (! -e $CSH_ARCHIVE/../POSTPROCS/plot_scatterqc.ncl) then
        echo
        echo "Script $CSH_ARCHIVE/../POSTPROCS/plot_scatterqc.ncl is missing"
        echo "Plots won't be generated"
        echo
        set plotobs = 0
    else
        cp -f $CSH_ARCHIVE/../POSTPROCS/plot_scatterqc.ncl .
    endif
endif

#------------------------------------------------------------------------------#
# Bring wrf and wrfvar namelists
#------------------------------------------------------------------------------#

echo

if( $node == 0 ) then

 if( -e $GSJOBDIR/namelists/wrf.nl.template ) then
  cp -f $GSJOBDIR/namelists/wrf.nl.template wrf.nl
  echo "Will use $GSJOBDIR/namelists/wrf.nl.template"
 else
  echo "Missing file $GSJOBDIR/namelists/wrf.nl.template, Cannot proceed!"
  exit -1
 endif

else

 if(-e $GSJOBDIR/namelists/wrfqc.nl.template.$node) then
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/wrfqc.nl.template.$node"
  cp -f $GSJOBDIR/namelists/wrfqc.nl.template.$node wrf.nl
 else if ( -e $GSJOBDIR/namelists/wrfqc.nl.template) then
  echo "${SUBSYSTEM} -- Using $GSJOBDIR/namelists/wrfqc.nl.template"
  cp -f $GSJOBDIR/namelists/wrfqc.nl.template wrf.nl
 else
  echo "Cannot find any of the files:"
  echo " $GSJOBDIR/namelists/wrfqc.nl.template.$node"
  echo " $GSJOBDIR/namelists/wrfqc.nl.template"
  echo "Cannot proceed!"
  exit -1
 endif
endif

# Those parameters are not used but need to be initialized to some values.

 set ifrest  = "FALSE"
 set savefrq = 44600                 
 set FCST_LENGTH = 1000              
 set out_int = 60                    
 set d4y_end = $y_end
 set d4m_end = $m_end
 set d4d_end = $d_end
 set d4h_end = $h_end

set NDOM = $NUM_DOMS

# build the wrf and real namelist

echo $y_start $m_start $d_start $h_start
echo $y_end $m_end $d_end $h_end $NUM_DOMS
echo $savefrq $ifrest $d4y_end $d4m_end $d4d_end $d4h_end

ed wrf.nl << EOF > /dev/null
g/SYY/s//$y_start/g
g/SMM/s//$m_start/g
g/SDD/s//$d_start/g
g/SHH/s//$h_start/g
g/EYY/s//$y_end/g
g/EMM/s//$m_end/g
g/EDD/s//$d_end/g
g/EHH/s//$h_end/g
g/D4YY/s//$d4y_end/g
g/D4MM/s//$d4m_end/g
g/D4DD/s//$d4d_end/g
g/D4HH/s//$d4h_end/g
g/DOM/s//$NDOM/g
g/FCSTH/s//$FCST_LENGTH/g
g/SaveFrq/s//$savefrq/g
g/OUTINT/s//$out_int/g
g/IfRest/s//$ifrest/g
w
q
EOF

#create namelists for real for domain 1: namelist.input01, 03, etc.

cp -f wrf.nl namelist.input01

#create namelists for real for each sub-domains: namelist.input02, 03, etc.

$EXECUTABLE_ARCHIVE/wrf.nl.nest.pl

#------------------------------------------------------------------------------#
# Determine largest domain's four corners and select the observational error 
# tables according to the latitude of the largest domain's center point.
#------------------------------------------------------------------------------#
#
# Only topographic data are needed, so use the "cold" input file,
# which is alway present. 

set wrfinput_d01 =  ${CYCDIR}/${this_cycle}_wrfinput_d01_cold

ln -sf $wrfinput_d01 wrfinput_d01

# ln -sf $CYCDIR/${this_cycle}_wrfbdy_d01 wrfbdy_d01

## Look for the domain 1 input file to grab the four corners lat and lon

if (! -e wrfinput_d01) then
    echo ""
    echo "ERROR Cannot find input file:"
    echo "$dir_work/wrfinput_d01 -> $wrfinput_d01"
    echo ""
    exit -1
endif

# Determine the lat/lon of the domain four corners

if (-e latlon.txt) rm -f latlon.txt

echo \
"$EXECUTABLE_ARCHIVE/latlon_wrf.exe wrfinput_d01 -latlon latlon.txt"
 $EXECUTABLE_ARCHIVE/latlon_wrf.exe wrfinput_d01 -latlon latlon.txt

# In case of error WRF_latlon, use obs over the full globe
# but assume the domain center at mid latitude

if (! -e latlon.txt) then
    echo "Cannot find file: latlon.txt, assume full globe."
    echo "   -90   90" >   latlon.txt
    echo "  -180  180" >>  latlon.txt
    set latc = 45
else
   # Find the center latitude
   set latlon = `cat latlon.txt`
   @ latc = $latlon[1] + $latlon[2]
   @ latc = $latc / 2
endif

# Use positive latitude (error tables are hemisphere independant).

set sign = `echo $latc |cut -c1-1`
if ($sign =~ -) then
    set  latc = `echo $latc |cut -c2-`
endif


# Pick the right table

if ($latc !~ [0-9]*) then
    echo "Unknown latitude $latc, assign mid-latitude AVN forecast error."
    set domlat = "midlat"
else
    if ($latc < 10) then
        set domlat = "equator"
    else if ($latc < 30) then
        set domlat = "tropics"
    else if ($latc < 50) then
        set domlat = "midlat"
    else if ($latc < 70) then
        set domlat = "highlat"
    else if ($latc < 90) then
        set domlat = "polar"
    else
        echo "Unknown latitude  $latc, assign mid-latitude AVN forecast error."
        set domlat = "midlat"
    endif
endif

echo

if (-e ${CONSTANT_FILES}/errtable_avnfct_${domlat}.r3dv.txt) then
    echo "The latitude of largest domain is around ${latc} N/S, will use AVN error file:"
    echo "${CONSTANT_FILES}/errtable_avnfct_${domlat}.r3dv.txt"
    cp -f ${CONSTANT_FILES}/errtable_avnfct_${domlat}.r3dv.txt errtable_avnfct.r3dv.txt
else
    echo "File ${CONSTANT_FILES}/errtable_avnfct_${domlat}.r3dv.txt is missing, cannot proceed!"
    exit -1
endif

# Unlink cold stat input file

#if (-e wrfinput_d01) rm -f wrfinput_d01

#------------------------------------------------------------------------------#
# Bring the land use table
#------------------------------------------------------------------------------#

echo
if (-e ${CONSTANT_FILES}/wrfrun/LANDUSE.TBL) then
echo "Will use land use table from file:"
echo "${CONSTANT_FILES}/wrfrun/LANDUSE.TBL"
      cp -f ${CONSTANT_FILES}/wrfrun/LANDUSE.TBL .
else
echo "File ${CONSTANT_FILES}/errtable_avnfct_${domlat}.r3dv.txt is missing, cannot proceed!"
      exit -1
endif

#------------------------------------------------------------------------------#
# Collect observations
#------------------------------------------------------------------------------#

# Generic name for the input obs file
  
set obs = all.obs

# Collect all obs from the different souces (after decoding)

if ( -e ${obs}) rm -f ${obs}

set echo
cat ../DECODE_BLP_PROF/PROF_* ../DECODE_NPN_PROF/PROF_* >>! ${obs}
cat ../DECODE_SATWVCD/SATWINDS.* ../DECODE_ACARS/ACARS.*all >> ${obs}
cat ../DECODE_CLASS/CLASS*all ../DECODE_SAMS/*SAMS.allrange >> ${obs}
cat ../DECODE_PROF/PROF_QC* >> ${obs}
cat ../DECODE_GTS/*_GTS_data.* ../RD_RAWS/*_RAWS_data.* >> ${obs}
cat ../RD_OKMESO/*_OKMESO_data.* ../DECOD*_VAD_PROF/NIDSVAD* >> ${obs}
cat ../RD_WVR/WVR.*  >> ${obs}
cat ../RD_SODAR/*SODAR* >> ${obs}
cat ../RD_PWIDS/*PWIDS* >> ${obs}
cat ../RD_LIDARVAD/*LIDARVAD* >> ${obs}
cat ../RD_MADIS/MADIS.* >> ${obs}
cat ../RD_NIDSVAD/NIDSVAD.* >> ${obs}
cat ../DECODE_HISFCW/HISFCW* >> ${obs}
cat ../RD_QWND/QWND* >> ${obs}
cat ../RD_WMO/WMO* >> ${obs}
cat ../RD_LIDPROF/*LIDPROF*data >> ${obs}
cat ../RD_MICROSTEPS/*MICROSTEPS* >> ${obs}
cat ../RD_DTE/*DTE* >> ${obs}
cat ../RD_DCNET/DCNET.* >> ${obs}
cat ../RD_QCOUT/QCOUT* >> ${obs}
cat ../RD_SPECIAL/SPECIAL* >> ${obs}
cat ../RD_TAMDAR/TAMDAR* >> ${obs}
cat ../RD_ADP/*_ADP_data.* >> ${obs}
cat ../RD_MODIS_POLAR/MODIS_POLAR_*.* >> ${obs}
cat ../RD_ALLOBS/ALLOBS* >> ${obs}
    
## Add any special obs in decoded format 
    
cat /data/inputspecial/* >> ${obs}

#endif
ls -alh ${obs}
unset echo
## Stop here in absence of any observation

if (! -e ${obs}) then
  echo
  echo "Cannot find observation file:"
  echo "$RUNDIR/$this_cycle/$RAP_RTFDDA/${obs}"
  exit -1 
endif

#------------------------------------------------------------------------------#
# Split the obs file into hourly files [HH-30mn,HH+30mn[
#------------------------------------------------------------------------------#
# Split obs by [t-30mn,t+30mn] time intervals, t = start_date,..., end_date

echo
echo "Splitting file: ${obs} into hourly files [t-30mn,t+30mn] time-intervals:"

if ($debug) echo \
"$EXECUTABLE_ARCHIVE/RT_${obs}_trim-merge.USA ${obs} 30 latlon.txt"
 $EXECUTABLE_ARCHIVE/RT_${obs}_trim-merge.USA ${obs} 30 latlon.txt >/dev/null

#------------------------------------------------------------------------------#
# Loop over time levels
#------------------------------------------------------------------------------#

## Clean-up

if (-e ${obs}_wrfqc_output)    rm -f ${obs}_wrfqc_output
if (-e ${obs}_wrfqc_virtual)   rm -f ${obs}_wrfqc_virtual

## Procees obs by [t-30mn, t+30mn] time intevals, t = start_date,..., end_date

set ccyymmddhh  = $start_date

while ($ccyymmddhh <= $end_date)

   echo
   echo \
"------------------------------------------------------------------------------"
   set ccyy = `echo $ccyymmddhh | cut -b 1-4`
   set mm = `echo $ccyymmddhh | cut -b 5-6`
   set dd = `echo $ccyymmddhh | cut -b 7-8`
   set hh = `echo $ccyymmddhh | cut -b 9-10`
   set mn = "00"
   set ss = "00"

   echo "Processing ${ccyy}-${mm}-${dd}_${hh}:${mn}:${ss}"

   set analysis_date = ${ccyy}-${mm}-${dd}_${hh}:${mn}:${ss}

## Check obs file

   if (! -e ${ccyymmddhh}.${obs}) then
       echo
       echo "Cannot find input obs file:  ${ccyymmddhh}.${obs}"
       echo "Skip QC operations for time: ${ccyymmddhh}"
       echo "$ccyymmddhh, 1" >! input
       ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
       set ccyymmddhh = `cat output`
       continue
   endif

   ln -sf ${ccyymmddhh}.${obs} obs_wrfqc_input

#------------------------------------------------------------------------------#
# Loop over the domains
#------------------------------------------------------------------------------#

   set d = 1
   while ($d <= $NUM_DOMS)

          echo "Looking for first guess files for domain $d"
          echo

## FVDB process domain 1 only for the moment
          if ($d > 1) break

          if ($d < 10) then
              set dom = "0$d"   
          else
              set dom = "$d"   
          endif

#------------------------------------------------------------------------------#
# Look for final analysis output file from previous cycle
#------------------------------------------------------------------------------#

          set TYPE = "F" # Use final analasis first
          set wrf = ${CYCDIR_LAST}/wrfout_d${dom}_${ccyy}-${mm}-${dd}_${hh}:${mn}:${ss}.${MM5HOST}_${TYPE}

          echo "Trying file:"
          echo "$wrf"

          if (! -e $wrf) then
              echo
              echo "Cannot find first guess file:"
              echo "$wrf"
              echo
              echo "Final analysis files contained in directory ${CYCDIR_LAST}:"
              ls ${CYCDIR_LAST}/wrfout_d${dom}_*.${MM5HOST}_${TYPE}
              echo

#------------------------------------------------------------------------------#
# Look for forecast output file from previous cycle
#------------------------------------------------------------------------------#

              set TYPE = "P+FCST" # Use forecast only
              set wrf = ${CYCDIR_LAST}/wrfout_d${dom}_${ccyy}-${mm}-${dd}_${hh}:${mn}:${ss}.${MM5HOST}_${TYPE}

              echo "Trying file:"
              echo "$wrf"

              if (! -e $wrf) then
                  echo
                  echo "Cannot find first guess file:"
                  echo "$wrf"
                  echo
                  echo "Forecast files contained in directory ${CYCDIR_LAST}:"
                  ls ${CYCDIR_LAST}/wrfout_d${dom}_*.${MM5HOST}_${TYPE}
                  echo

#------------------------------------------------------------------------------#
# Use WRF input file when output files are missing
#------------------------------------------------------------------------------#

                  set wrf = ${CYCDIR}/${this_cycle}_wrfinput_d${dom}

                  echo "Trying file:"
                  echo "$wrf"

                  if (! -e $wrf) then
                      echo
                      echo "Cannot find first guess file:"
                      echo "$wrf"
                      echo
                      echo "Input files contained in directory ${CYCDIR}:"
                      ls ${CYCDIR}/${this_cycle}_wrfinput_d${dom}*
                      echo
#------------------------------------------------------------------------------#
# Use WRF cold start input file when output and input files are missing
#------------------------------------------------------------------------------#

                      set wrf = ${CYCDIR}/${this_cycle}_wrfinput_d${dom}_cold

                      echo "Trying file:"
                      echo "$wrf"

                      if (! -e $wrf) then
                          echo
                          echo "Cannot find first guess file:"
                          echo "$wrf"
                          echo
                          echo "Cold start input files contained in directory ${CYCDIR}:"
                          ls ${CYCDIR}/${this_cycle}_wrfinput_d${dom}*
                          echo

                          echo "Skip QC operations for domain $dom at time ${ccyymmddhh}"
                          @ d ++
                          continue
                      endif
                  endif
              endif
          endif

          echo
          echo "Using first guess file:"
          echo "$wrf"

          ln -sf $wrf wrf_wrfqc_input


#------------------------------------------------------------------------------#
# Pick the WRF namelist for the domain
#------------------------------------------------------------------------------#

          ln -s -f namelist.input$dom namelist.input

#------------------------------------------------------------------------------#
# Build the WRFVAR namelist
#------------------------------------------------------------------------------#

          echo
          echo "Updating namelist file from template:"
          echo "${CONSTANT_FILES}/WRFVAR.namelist.template"

          cp -f ${CONSTANT_FILES}/WRFVAR.namelist.template namelist.3dvar


ed namelist.3dvar << EOF > /dev/null
g/FILL_ANALYSIS_DATE/s//$analysis_date/g
g/FILL_FG_FORMAT/s//1/g
w
q
EOF

#------------------------------------------------------------------------------#
# Save the Input data if requested
#------------------------------------------------------------------------------#

          if ($save_data > 0) then
              set save_data_dir = ${RUNDIR}/saveObs/${this_cycle}/${stage}/${ccyymmddhh}/d${dom} 
              echo
              echo "Option save_data = $save_data, will back-up input/output data files in directory:"
              echo "$save_data_dir"

              if (! -d ${save_data_dir}) mkdir -p ${save_data_dir}
              cp -f latlon.txt     ${save_data_dir}/.
              cp -f namelist.input ${save_data_dir}/.
              cp -f namelist.3dvar ${save_data_dir}/.
              cp -f errtable_avnfct.r3dv.txt ${save_data_dir}/.
              cp -f LANDUSE.TBL              ${save_data_dir}/.
              cp -f $wrf                     ${save_data_dir}/.
              cp -f ${ccyymmddhh}.${obs}     ${save_data_dir}/.
              set here = `pwd`
              cd $save_data_dir
              ln -sf ${ccyymmddhh}.${obs} obs_wrfqc_input
              ln -sf wrfout_d${dom}_${ccyy}-${mm}-${dd}_${hh}:${mn}:${ss}.${MM5HOST}_${TYPE} wrf_wrfqc_input
              if (-e $CSH_ARCHIVE/../POSTPROCS/plot_scatterqc.ncl) \
                  ln -sf $CSH_ARCHIVE/../POSTPROCS/plot_scatterqc.ncl .
              if (-e $EXECUTABLE_ARCHIVE/wrfqc.exe ) \
                  ln -sf $EXECUTABLE_ARCHIVE/wrfqc.exe .
              cd $here
          endif
          
#------------------------------------------------------------------------------#
# Run the QC program
#------------------------------------------------------------------------------#

          echo
          echo "Running wrfqc program:"
          if ($debug) echo \
         "$EXECUTABLE_ARCHIVE/wrfqc.exe"
          $EXECUTABLE_ARCHIVE/wrfqc.exe >&! $dir_work/wrfqc_${ccyymmddhh}_d${dom}_print.out

#------------------------------------------------------------------------------#
# Plot the results
#------------------------------------------------------------------------------#

          if ($plotobs > 0) then

              echo
              echo "Ploting QCed data:"

              set t = plotobs

              if (-e obs_wrfqc_${t} && ! -z  obs_wrfqc_${t}) then

                  if ($debug) echo \
                 "$NCARG_ROOT/bin/ncl < plot_scatterqc.ncl"
                  $NCARG_ROOT/bin/ncl < plot_scatterqc.ncl >&! plot_scatterqc_${ccyymmddhh}_d${dom}_print.out
                  set NCLS = `find . -name obs_wrfqc_\*.ncgm -print` 

                  if ($#NCLS > 0) then
                      foreach f (obs_wrfqc_*.ncgm)
                         mv -f $f ${ccyymmddhh}_${f}
                      end
                  else
                      echo 
                      echo "WARNING: no graphics have been generated for domain $dom at $ccyymmddhh"
                      echo "         Check log file: plot_scatterqc_${ccyymmddhh}_d${dom}_print.out" 
                      echo
                  endif

              else
 
                  echo
                  echo "WARNING: missing output file: obs_wrfqc_${t} for domain $dom at $ccyymmddhh"

              endif
        
          endif

#------------------------------------------------------------------------------#
# Check output files and rename with time and domain tags
#------------------------------------------------------------------------------#

          echo
          echo "Renaming output files:"

          foreach t ("output" "virtual" "error" "pairs")

             if (! -e obs_wrfqc_${t} || -z  obs_wrfqc_${t}) then
                 echo "WARNING: missing output file: obs_wrfqc_${t} for domain $dom at $ccyymmddhh"
                 echo "         Check log file: wrfqc_${ccyymmddhh}_d${dom}_print.out" 
             else
                 echo \
                "mv -f obs_wrfqc_${t}  ${ccyymmddhh}.${obs}_wrfqc_${t}_d${dom}"
                 mv -f obs_wrfqc_${t}  ${ccyymmddhh}.${obs}_wrfqc_${t}_d${dom}

             endif

             # Copy all output data files
             if ($save_data > 0) then

                if(-e ${ccyymmddhh}.${obs}_wrfqc_${t}_d${dom}) then
                   cp -f ${ccyymmddhh}.${obs}_wrfqc_${t}_d${dom} ${save_data_dir}/.
                   if ($t =~ "pairs") then
                       ln -sf ${save_data_dir}/${ccyymmddhh}.${obs}_wrfqc_${t}_d${dom} ${save_data_dir}/obs_wrfqc_${t}
                    endif
                endif
             endif

          end

          # Copy output data files for plot only
#         if ($save_data > 0) then
#            cp -f ${ccyymmddhh}.${obs}_wrfqc_pairs_$d{dom} ${save_data_dir}/.
#            ln -sf ${save_data_dir}/${ccyymmddhh}.${obs}_wrfqc_pairs_d${dom} ${save_data_dir}/obs_wrfqc_pairs
#         endif

#------------------------------------------------------------------------------#
## Catenate hourly QCed observation files
#------------------------------------------------------------------------------#

          echo
          echo "Resampling QCed data:"

          foreach t ("output")

             if ( -e ${ccyymmddhh}.${obs}_wrfqc_${t}_d${dom}) then
                 echo \
                "cat ${ccyymmddhh}.${obs}_wrfqc_${t}_d${dom} >>!  ${obs}_wrfqc_${t}_d${dom}"
                 cat ${ccyymmddhh}.${obs}_wrfqc_${t}_d${dom} >>! ${obs}_wrfqc_${t}_d${dom}
             else
                 echo "WARNING: missing output file: ${ccyymmddhh}.${obs}_wrfqc_${t}_d${dom}"
                 echo "         Check log file: wrfqc_${ccyymmddhh}_d${dom}_print.out" 
             endif

          end

          if (-e wrfinput_d$dom)   rm -f wrfinput_d$dom

#------------------------------------------------------------------------------#
# Go to next domain
#------------------------------------------------------------------------------#

          @ d ++

   end

#------------------------------------------------------------------------------#
# Increment date
#------------------------------------------------------------------------------#

   #set ccyymmddhh = `${EXECUTABLE_ARCHIVE}/geth_newdate.csh  $ccyymmddhh +1`
   echo "$ccyymmddhh, 1" >! input
   ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
   set ccyymmddhh = `cat output`

end

#------------------------------------------------------------------------------#
# Merge obs file for different domains into one single file
#------------------------------------------------------------------------------#

echo
echo \
"------------------------------------------------------------------------------"
echo "Combining files from different domains:" 
echo

set dom = "01"

foreach t ("output")

  if (! -e ${obs}_wrfqc_${t}_d${dom}) then
      echo "QCed obs file: ${obs}_wrfqc_${t}_d${dom} cannot be found"
      continue
  endif

  echo \
 "mv -f ${obs}_wrfqc_${t}_d${dom} ${obs}_wrfqc_${t}"
  mv -f ${obs}_wrfqc_${t}_d${dom} ${obs}_wrfqc_${t}

end

#------------------------------------------------------------------------------#
# Split the output and virtual catenated QCed observation file into hourly 
# [t,t+59mn] files
#------------------------------------------------------------------------------#

echo
echo \
"------------------------------------------------------------------------------"
echo "Splitting the data into hourly [t,t+59mn] files:" 
echo

foreach t ("output")

   if (! -e ${obs}_wrfqc_${t}) then
       echo "QCed obs file: ${obs}_wrfqc_${t} cannot be found"
       continue
   endif

   ln -s -f ${obs}_wrfqc_${t} ${obs}_wrfqc_${t}_trimmed

## Split the obs output file by [t, t+59mn] time intervals,t = start_date,..., end_date

   if ($debug) echo \
  "$EXECUTABLE_ARCHIVE/RT_${obs}_trim-merge.USA ${obs}_wrfqc_${t}_trimmed 59 latlon.txt"
   $EXECUTABLE_ARCHIVE/RT_${obs}_trim-merge.USA ${obs}_wrfqc_${t}_trimmed 59 latlon.txt >  /dev/null

end

#------------------------------------------------------------------------------#
# Rename and reformat the output hourly files and move files around
#------------------------------------------------------------------------------#

echo
echo \
"------------------------------------------------------------------------------"
echo "Reformating hourly [t,t+59mn] files:"
echo

set t = "output"
set ccyymmddhh  = $start_date

## Loop from start_date to end_date run 

while ($ccyymmddhh <= $end_date)

## Break down the date
       set ccyy = `echo "${ccyymmddhh}"|cut -c 1-4`
       set mm   = `echo "${ccyymmddhh}"|cut -c 5-6`
       set dd   = `echo "${ccyymmddhh}"|cut -c 7-8`
       set hh   = `echo "${ccyymmddhh}"|cut -c 9-10`
       set mn   = "00"
       set ss   = "00"

#------------------------------------------------------------------------------#
# Rename qced obs file
#------------------------------------------------------------------------------#

# Output file from wrfqc
       set qc_out  = ${ccyy}${mm}${dd}${hh}.${obs}_wrfqc_${t}_trimmed
# Input file name for RT_fdda_reformat
       set qc_int  = qc_out_${ccyy}-${mm}-${dd}_${hh}:${mn}:${ss}.0000
# Output file name of RT_fdda_reformat
       set qc_fdda = ${ccyymmddhh}_qc_obs_for_assimilation_s

       if (! -e ${qc_out}) then
           echo "WARNING: Hourly QCed obs file: ${qc_out} cannot be found"
           echo "$ccyymmddhh, 1" >! input
           ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
           set ccyymmddhh = `cat output`
           continue
       endif

       if ($debug) echo \
      "mv -f ${qc_out} $qc_int"
       mv -f ${qc_out} $qc_int


#------------------------------------------------------------------------------#
# Reformat the QCed data into the format read by MM5
#------------------------------------------------------------------------------#

## Output file name from reformatting program


       if ($debug) echo \
      "$EXECUTABLE_ARCHIVE/RT_fdda_reformat.pl $qc_int"
       $EXECUTABLE_ARCHIVE/RT_fdda_reformat.pl $qc_int >/dev/null

## Move qced files to the cycle directory

       if (-e ${qc_fdda}) then
          echo "Output file: ${qc_fdda} is ok"
          echo \
         "mv -f ${qc_fdda} $CYCDIR/."
          mv -f ${qc_fdda} $CYCDIR/.
       else
          echo "Output file: ${qc_fdda} is missing"
       endif

## Move other files needed for verifications to the cycle directory/RAP_RTFDDA

       echo \
      "cp -f ${qc_int}  $RAP_RTFDDA/."
       cp -f ${qc_int}  $RAP_RTFDDA/.
       echo \
      "cp -f ${obs}     $RAP_RTFDDA/${this_cycle}.${obs}"
       cp -f ${obs}     $RAP_RTFDDA/${this_cycle}.${obs}

#------------------------------------------------------------------------------#
# Increment time and break down date
#------------------------------------------------------------------------------#

#      set ccyymmddhh = `${EXECUTABLE_ARCHIVE}/geth_newdate.csh  $ccyymmddhh +1`
       echo "$ccyymmddhh, 1" >! input
       ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe < input >! output
       set ccyymmddhh = `cat output`

#------------------------------------------------------------------------------#
# End loop over hours
#------------------------------------------------------------------------------#
end

#------------------------------------------------------------------------------#
# Clean out
#------------------------------------------------------------------------------#

set ccyy = `echo $this_cycle | cut -b 1-4`
find . -name \*.${obs}\* -exec rm {} \;
find . -name \*_trimmed  -exec rm {} \;
if (-e obs_wrfqc_loaded)  rm -f obs_wrfqc_loaded
if (-e obs_wrfqc_merged)  rm -f obs_wrfqc_merged
if (-e obs_wrfqc_input)   rm -f obs_wrfqc_input
if (-e wrf_wrfqc_input)   rm -f wrf_wrfqc_input
if (-e input)  rm -f input
if (-e output) rm -f output

# Tag the obs input/output file with cycle date.
if (-e ${obs}) then
    ln -sf ${obs} ${this_cycle}.${obs}_wrfqc_input
endif

if (-e ${obs}_wrfqc_output) then
    ln -sf ${obs}_wrfqc_output ${this_cycle}.${obs}_wrfqc_output
endif

#------------------------------------------------------------------------------#
# Unlink Link local workdir dir from main disk
#------------------------------------------------------------------------------#
#if (-d $GEAPSTMP) then
#    rm $dir_work $GEAPSTMP/$tmp_work
# endif

echo 
echo "End of $0"
echo " -----------------------------------------------------------------------"
echo
#------------------------------------------------------------------------------#

#!/usr/bin/perl

##############################################################################
# Recommended cycling hour
#
#Cron hour: 1hcyc: 0 1 2 3 ..... 23         worst cold-start: 0Z 12Z
#           2hcyc: 0 2 4 6 ..... 22         worst cold-start: 0Z 02Z 12Z 14Z
#           3hcyc: 2 5 8 11 14 17 20 23     Best cold-start: 5Z and 17Z
#           4hcyc: 2 6 10 14 18 22          Best cold-start: 6Z and 18Z
#           6hcyc: 0 6 12 18                Best cold-start: 6Z and 18Z
#          12hcyc: 0 12                     Equal best
##############################################################################

use Time::gmtime 'gmctime';
use Time::Local;

use lib "$ENV{MODULESHOME}/init";
use perl;

print "PID: $$\n";

#&module(load,'python/gnu/2.7.9');

# $FLEXINPUT must be an env-var
$FLEXINPUT = $ENV{FLEXINPUT};

# Is FLEXINPUT set? If not, then exit.
&usage();

# This input file defines the configuration for the job
# Make a copy of this file fist, because we will be adding params to it later
system ("cp $FLEXINPUT $FLEXINPUT.save");

require $FLEXINPUT.".save";

if (-e $FLEXINPUT)
{
  print "Using job configuration in $FLEXINPUT\n";
  print "Starting pre-processing on $ENV{HOST}\n";
}
else
{
  print "\nFile $FLEXINPUT is missing..... EXITING\n";
  exit(-1);
}

# Module management
if (@MODULE_LOAD || @MODULE_SWAP) {
   use lib "$ENV{MODULESHOME}/init";
   use perl;
}

if (@MODULE_LOAD_PRE_F) {
   foreach $mod (@MODULE_LOAD_PRE_F) {
      &module(load,$mod);
      print "Loading PRE_F Modules $mod\n";
   }
}

if (@MODULE_SWAP_PRE_F) {
   foreach $mod (@MODULE_SWAP_PRE_F) {
      &module(swap,$mod);
   }
}
if (@MODULE_LOAD_POSTPROC) {
   foreach $mod (@MODULE_LOAD_POSTPROC) {
      &module(load,$mod);
   }
}

# This script contains the hh_advan_date and mm_advan_date subroutines
require $PERL_ARCHIVE.'/tools/advan_date.pl';

# This script contains &makeSubDirs("FULL_PATH_DIR")
require $PERL_ARCHIVE.'/tools/makeSubDirs.pl';

# helper array
%LMODEL = ("MM5","mm5","WRF","wrf");

# The ICBC-preprocessor perl script
require $ICBC_PREPROCESSOR;

# This file contains the subroutine to collect obs
require $OBS_PROCESSOR;

# This script contains the setEnvVars subroutine
require $PERL_FLEX.'/setEnvVars.pl';

# Set the environment variables
print "\nSetting env vars\n";
&setEnvVars();

print "\n\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
print " starting RTFDDA system for cycle=$this_cycle at ",gmctime();
print "                    for ${RANGE} on $ENV{HOST}                              \n";
print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n";

# Cycle time variables
# same as this_cycle
$this_cycle_time = 0;

# this cycle's date
$this_cycle_date = 0;

# this cycle's year
$yy = 0;

# this cycle's month - 1
$mo = 0;

# this cycle's day of month
$dd = 0;

# this cycle's hour
$hh = 0;

# this cycle's minute
$mm = 0;

# this cycle's second
$sec = 0;

# EPOCH seconds for cycle hour
$cycle_time = 0;

# last cycle's time
$last_cycle = 0;

# the cycle's time that ran before the last cycles
$llast_cycle = 0;

# last cycle's date
$last_cycle_date = 0;

# the cycle's time that ran 24 hrs ago
$yesterday_cycle = 0;

# the cycle's date that ran 24 hrs ago
$yesterday_date = 0;

# Time frames of the FDDA cycle:

# Final analysis start time
$f_fdda_start = 0;

# Prelim analysis start time
$p_fdda_start = 0;

# Prelim analysis end time
$p_fdda_end = 0;

# Set above time variables
print "\nSetting cycle time variables\n";
&setCycleTimes();

# Check for a crash in the previous cycle, if found, clean up
print "\nChecking for crash in prev. cycle\n";
&checkForCrashInPrevCycle();

# keep for now
$THISNODE = "node$NODE";
$MPNODE1 = $THISNODE;

# Make sure the temp-dirs are there
print "\nMaking $GEAPSTMP and $GEAPSKEP\n on $ENV{HOST}\n\n";
&makeSubDirs($GEAPSTMP);
&makeSubDirs($GEAPSKEP);

# Create a bridge between the main RUNDIR and the local disk
if (-e $GEAPSTMP && -e $GEAPSKEP){
    if (-e "$GEAPSKEP"){
        system("rm -r -f $GEAPSKEP/*");
        system("ln -s -f $RUNDIR/$this_cycle $GEAPSKEP/bridge");
    }
    if (-e "$GEAPSTMP"){
        system("ln -s -f $GEAPSTMP $RUNDIR/$this_cycle/bridge");
    }
}else{
    print "\nCannot create workdir GEAPSTMP = $GEAPSTMP\n";
    print "         and/or workdir GEAPSKEP = $GEAPSKEP\n";
    print "All computations will be done in $RUNDIR/$this_cycle\n";
}

system("rm -rf $GEAPSTMP/DECODE_AMV") if (-d "$GEAPSTMP/DECODE_AMV");
system("rm -rf $GEAPSTMP/DECODE_IAF") if (-d "$GEAPSTMP/DECODE_IAF");
system("rm -rf $GEAPSTMP/DECODE_IAFBUFR") if (-d "$GEAPSTMP/DECODE_IAFBUFR");
system("rm -rf $GEAPSTMP/DECODE_IAFWORLD") if (-d "$GEAPSTMP/DECODE_IAFWORLD");
system("rm -rf $GEAPSTMP/DECODE_PROF") if (-d "$GEAPSTMP/DECODE_PROF");

chdir $RUNDIR."/".$this_cycle;

# Make the log file in RUNDIR/this_cycle
print "\nCreating the log file in $RUNDIR/$this_cycle/timing\n";
&makeRUNDIRLogFile("timing");

print FILEL "Starting pre-prossing-RTFDDA system on $ENV{HOST} for ${RANGE} at ",gmctime();

#
# rescuing for MM5 case.
#
if($re_run == 1 && $MODEL eq "MM5")
{
  &rescueMode();
}

print FILEL "\n  ------ Pre-processing BEGIN---------\n";

#
#FVDB 20081117 Find 24hr running average of Salt Lake T based on obs at HAT
#
#FVDB 20081117 print FILEL "Find 24hr running average of Salt Lake T based on obs at HAT\n";
#FVDB 20081117 print "Find 24hr running average of Salt Lake T based on obs at HAT\n";
#FVDB 20081117 system("$CSH_ARCHIVE/Forecast/RT_0_calcavg.csh $this_cycle");

# Setting rest_time and rest_time_Str_old
$rest_time = 0;
$rest_time_str_old = 0;

open (CRITIC, "$RUNDIR/critic.time");
$rest_time = <CRITIC>;
close (CRITIC);

if($MPPJOB eq "yes")
{
    $rest_time_str_old = sprintf("%7.7d",$rest_time);
}

#
# Check if current cycle is normal or not
#
if ($normal == 0)
{
  print FILEL "Calling getCycleStatus \n";
  print "Calling getCycleStatus\n";

  $normal = &getCycleStatus();

  if ($DEBUG)
  {
    print FILEL "getCycleStatus returns normal=$normal\n";
    print "getCycleStatus returns normal=$normal\n\n";
  }
}

# cycle time of ICBC data
$BCS_date = 0;

# ToDo: description
$MM5_start = 0;

# ToDo: description
$RAP_start = 0;

# ToDo: description
$RAP_end = 0;

# use all obs that are timestamped after OBS_start
$OBS_start = 0;

$RAP_hour = 0;

# Define and initialize variables that will be used to find the proper
# eta/avn/other (hh) files to start:

# This is a helper array and defines the frequency in hours of the input data files
# ToDo: May need to go to flexinput.pl
%BCSDT=("ETA",3,"AVN",6,"AVNFTP",3,"GFS004",3,"NNRP",6,"NNRP2",6,"CFSR",6,"CFSF",6,"CFSV1",6,"CFSV2",6,"FNL",6,"DWD",6,"UKMO",6,"GEM",3,"ECMWF",3);

# For "ETA" and "AVNFTP": BCSHM = {0,0,0,3,3,3,6,6,6,9,9,9,....}
%BCSHM=();

# For "ETA" and "AVNFTP": BCSHP = {3,3,3,6,6,6,9,9,9,12,12,12,...}
%BCSHP=();

$ii=0;
# 20081120 FVDB for($i=0;$i<96;$i++) C-FDDA has much longer cycles
for($i=0;$i<412;$i++)
{
  $ii += $BCSDT{$BCS} if($i%$BCSDT{$BCS} == 0 && $i != 0);
  $BCSHM{$i}=$ii;
  $BCSHP{$i}=$ii;
  $BCSHP{$i}=$ii + $BCSDT{$BCS} if($i%$BCSDT{$BCS} != 0);
}

# Calculate the BCS_DATE
if ($hh >= 5 && $hh < 17)
{
  $BCS_date = sprintf("%08d%02d",$this_cycle_date,0);
}
elsif ($hh < 5 || $hh >= 17)
{
  if ($hh < 5)
  {
    $BCS_date = sprintf("%08d%02d",$yesterday_date,12);
  }
  else
  {
    $BCS_date = sprintf("%08d%02d",$this_cycle_date,12);
  }
}

# Calculate RAP_start, MM5_start....
&setStartTimes($normal);

if ($FCST_LENGTH) {
} else {
   $FCST_LENGTH = $D1_LENGTH;
}

if ($BCS ne 'NNRP' && $BCS ne 'NNRP2' && $BCS ne 'CFSR' && $BCS ne 'CFSF' && $BCS ne 'CFSV2' && $BCS ne 'FNL'){
$FCST_LENGTH = 2 if($normal != 1 && $NUM_PROCS <= 6 && $CYC_INT < 3 );  #cold-start is special
$FCST_LENGTH = $D1_COLD_LENGTH if($normal != 1);
}

#===================================== Case Study ==============================
#
if($CASE_STUDY)
{
  # There is no such perl script currently, kept here for hist. reasons
  require $PERL_FLEX.'/case_fsct_length.pl' ;

  if ($case_cyc_fcst{$this_cycle})
  {
    $FCST_LENGTH =$case_cyc_fcst{$this_cycle}
  }
}
#===============================================================================

# Calculate RAP_end
#if ($BCS eq 'NNRP' || $BCS eq 'NNRP2' || $BCS eq 'FNL'){
#$RAP_end  = &hh_advan_date($RAP_start,$BCSHP{$FCST_LENGTH+$CYC_INT-$FIN_END+6});
#}else{
#$RAP_end  = &hh_advan_date($RAP_start,$BCSHP{$FCST_LENGTH+$CYC_INT-$FIN_END+3});
#}
##$RAP_end  = &hh_advan_date($RAP_start,$BCSHP{$FCST_LENGTH+$CYC_INT-$FIN_END});

$hours_offset = findHoursOffset();
$RAP_end = hh_advan_date($RAP_start,$BCSHP{$FCST_LENGTH+$hours_offset});
print "RAP_start: $RAP_start ; RAP_end: $RAP_end\n";

# Special case for 6-hourly cycling that does 48-hour cold-start analysis
if ($COLD_0012 == 2) {
   die "\$WRFQC_BACK is not defined!\n" if (! $WRFQC_BACK);
   $QC_start = hh_advan_date($this_cycle,-$WRFQC_BACK)
}

# Calculate $OBS_start
$OBS_start = $f_fdda_start;
if( $RAP_start < $f_fdda_start || $normal == 1)
{
  $OBS_start = &hh_advan_date($f_fdda_start, -1);
}

# For non-normal cycle write "1" to the $RUNDIR/critic.time
if($normal != 1)
{
  open (CRITIC, ">$RUNDIR/critic.time");
  print  CRITIC 1;
  $rest_time_str_old = sprintf("%7.7d",1);
  close (CRITIC);
}

if($normal != 5 && $normal != 17 && $normal != 1 && $COLD_0012 == 1)
{
#    Too far from analysis time (00Z or 12Z), fdda does not make sense or
#    too close to analysis time (00Z or 12Z), ETA data is not available.
#    In both cases, abort the mission of this cycle
  print FILEL "  Previous cycle failed and this is not a good time for cold-start \n";
  print FILEL "  So abort this cycle and look forward to next cycle time ... \n";
  print FILEL "  Abort at ",gmctime();
  print "  Previous cycle failed and this is not a good time for cold-start \n";
  print "  So abort this cycle and look forward to next cycle time ... \n";
  print "  Abort at ",gmctime();
  print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n";
  print FILEL "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n";
  system("rm -r $RUNDIR/$this_cycle") if( -e "$RUNDIR/$this_cycle");
  exit (2);
}
elsif ( $normal == 1)
{
  print FILEL "Previous cycle is normal. So this cycle restarts normally \n";
  print "  Previous cycle is normal. So this cycle restarts normally          \n";
  print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n";
  print FILEL "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n";
}
else
{
  # For flex -- do not care the above factors, start the FDDA anyway from now!
  print FILEL "  Previous cycle failed and cold-start from now anyway \n";
  print "  Previous cycle failed and cold-start from the current cycle anyway \n";
  print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n";
  print FILEL "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n";
}

print "RAP_start=$RAP_start RAP_end=$RAP_end \n";
print "${MODEL}_start=$MM5_start BCS_date=$BCS_date \n";
print "F_start=$f_fdda_start P_start=$p_fdda_start \n";
print "OBS_start=$OBS_start FCST_length=${FCST_LENGTH} hours\n";
print "Rest_time=$rest_time NORMAL=$normal\n";

print FILEL "\n-----------------------------------------------------------------------------\n";
print FILEL "RAP_start=$RAP_start RAP_end=$RAP_end \n";
print FILEL "${MODEL}_start=$MM5_start BCS_date=$BCS_date \n";
print FILEL "F_start=$f_fdda_start P_start=$p_fdda_start \n";
print FILEL "OBS_start=$OBS_start FCST_length=${FCST_LENGTH} hours\n";
print FILEL "Rest_time=$rest_time NORMAL=$normal                  \n";
print FILEL "-----------------------------------------------------------------------------\n\n";


if($normal == 1)
{
  # Concat output files from prev. cycle and move them to the desired location
  &getRestartFilesForNormalCycle();
}

if ( $NODE != 0 && -d $GEAPSTMP )
{
  system("rm -r -f $GEAPSTMP/*");
  system("mv -f $GEAPSKEP/* $GEAPSTMP/");
}

#
#---------------------------------------------------------------------
#	Processing  ETA-AWIPS / AVN /... data
#---------------------------------------------------------------------
#
# Collect ETA-AWIP / AVN data -- 24 - 48 hours needed

print FILEL "\n   Collect $BCS data at ", gmctime();
#       Specify which ETA/AVN cycle we should use for first guess, BC and IC

# take SST at the nearest time:
if ( -e "$DATA_SST_DIR/$this_cycle_date.rtg_sst")
{
  system ("cp $DATA_SST_DIR/$this_cycle_date.rtg_sst $DATA_DIR/sst.grib");
}
elsif ( -e "$DATA_SST_DIR/$yesterday_date.rtg_sst" )
{
  system ("cp $DATA_SST_DIR/$yesterday_date.rtg_sst $DATA_DIR/sst.grib");
}

$time_this = 0;
open (CRITIC, "$RUNDIR/critic.time");
$time_this = <CRITIC>;
close (CRITIC);

$time_ctrl = 0;
open (CRITIC1, "$RUNDIR/../ETA_CTRL/critic.time");
$time_ctrl = <CRITIC1>;
close (CRITIC1);

if ($time_this != $time_ctrl)
{
  $CLONEIN = 0;
  print "\ncycle critic_times mismatch. Reset  CLONEIN = $CLONEIN \n";
}

if ($CLONEIN)
{ ######## CLONE INPUT CLONE INPUT   ###########################################
  #clone the input from parallel test run
  if($MODEL eq "MM5")
  {
    print "\n   starting clone mm5input at ", gmctime();
    print FILEL "\n   starting clone mm5input at ", gmctime();
    system("$GSJOBDIR/clone_mm5input.csh $GSJOBID $this_cycle $NODE");
  }
  elsif ($MODEL eq "WRF")
  {
    print "\n   starting clone wrfinput at ", gmctime();
    print FILEL "\n   starting clone wrfinput at ", gmctime();
    system("$GSJOBDIR/clone_wrfinput.csh $GSJOBID $this_cycle $NODE");
  }
} # if ($CLONEIN)
else
{ ####### NOT CLONE INPUT, NOT CLONE INPUT ####################################

  # This calls the ICBC pre-processor
  print "calling processData() in $ICBC_PREPROCESSOR\n";
  &processData();

  #
  # For MM5: RT_C_no_super, RT_E_interp_back_v3, RT_E_time_interpETA,
  #
  if( $MODEL eq "MM5")
  {
    print FILEL "\n   starting no_super.csh at ", gmctime();
    system("$CSH_ARCHIVE/Forecast/RT_C_no_super.csh $BCS_date $this_cycle");
    print FILEL "   ending   no_super.csh at ", gmctime();

    # Simon tested and found this is not necessary.
    #print FILEL "\n   starting new_sfc.csh at ", gmctime();
    #system("$CSH_ARCHIVE/Forecast/RT_D_new_sfc.csh $BCS_date $this_cycle");
    #print FILEL "   ending   new_sfc.csh at ", gmctime();

    # Time interpolation for first guess (6 time levels from $hh-4 to $hh+1)
    if ((-s "${last_cycle}_MMOUTPUT_DOMAIN1") || (-s "$GEAPSTMP/${last_cycle}_MMOUTPUT_DOMAIN1"))
    {
      print FILEL "\n   starting first guess from previous cycle at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_E_interp_back_v3.csh $OBS_start $p_fdda_end $this_cycle $last_cycle $PTOP");
      print FILEL "   ending   first guess from previous cycle at ", gmctime();
    }
    else
    {
      print FILEL "\n   starting time_interp.csh for ETA first guess at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_E_time_interpETA.csh $this_cycle $RAP_start $p_fdda_end $PTOP");
      print FILEL "   ending   time_interp.csh for ETA first guess at ", gmctime();
    }
  } #if MODEL is MM5

  #
  # Collect and decode OBS upto most recent time
  #
  print FILEL "\n   start the processing of OBS for C-stage at ", gmctime();

  &get_and_decode_OBS ("C-stage",$OBS_start,$this_cycle_time,$this_cycle);
  print FILEL "   complete the processing of OBS for C-stage at ", gmctime();

  if($MODEL eq "MM5")
  {
    $RAP_hour=42+$ETA_STATUS * 12;
    $RAP_hour -= 3 if($hh == 14 || $hh == 2);
    #$RAP_end = &hh_advan_date($RAP_start,$RAP_hour);  #get 36 h analysis for cold start

    #
    # Perform objective analysis
    #
    
    if ($BCS ne 'NNRP' && $BCS ne 'NNRP2' && $BCS ne 'CFSR' && $BCS ne 'CFSF' && $BCS ne 'CFSV2' && $BCS ne 'FNL')
    {
      print FILEL "\n   starting RT_G_little_r.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_G_little_r.csh $BCS_date $RAP_start $RAP_end $this_cycle $normal");
      print FILEL "   ending   RT_G_little_r.csh at ", gmctime();
    }
    else
    {
      print FILEL "\n  starting $CSH_ARCHIVE/Forecast/RT_G_rap.csh at ", gmctime();
      print "Calling $CSH_ARCHIVE/Forecast/RT_G_rap.csh $BCS_date $RAP_start $RAP_end $this_cycle $normal at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_G_rap.csh $BCS_date $RAP_start $RAP_end $this_cycle $normal");
      print FILEL "   ending   RT_G_rap.csh at ", gmctime();
    }
    
    #
    # Perform vertically interpolation and get B.C. for this cycle $hh-4 to $hh+8
    #
    print FILEL "\n   starting interp_front.csh at ", gmctime();
    system("$CSH_ARCHIVE/Forecast/RT_J_interp_front.csh $MM5_start $RAP_end $this_cycle $PTOP $normal");
    print FILEL "   ending   interp_front.csh at ", gmctime();

    #
    # Perform nestdown to get I.C. for cold-start run
    #
    print FILEL "\n   starting nestdown.csh at ", gmctime();
    system("$CSH_ARCHIVE/Forecast/RT_K_nestdown.csh $MM5_start $MM5_start $this_cycle");
    print FILEL "   ending   nestdown.csh at ", gmctime();
  } # if MM5

  if($MODEL eq "WRF")
  {
    print FILEL "\n   starting wrf real at ", gmctime();
#    system("$CSH_ARCHIVE/Forecast/RT_K_wrfreal_rtfdda.csh $this_cycle $RAP_start $RAP_end $NODE $normal");
    system("$CSH_ARCHIVE/Forecast/RT_K_wrfreal_wps_rtfdda.csh $this_cycle $RAP_start $RAP_end $NODE $normal $NUM_PROCS $BCS");
    print FILEL "   ending  wrf real at ", gmctime();
  }

  # If this is a 'normal' cycle, then use previous input and lowbdy for MM5 or WRF
  if ( $normal == 1 )
  {
    for ($i=1; $i <= $NUM_DOMS; $i++)
    {
      if ($MODEL eq "MM5")
      {
        $fn_mminputo = "$RUNDIR/$last_cycle/${last_cycle}_MMINPUT_DOMAIN$i.${RANGE}";
        $fn_mminputn = "${this_cycle}_MMINPUT_DOMAIN$i.${RANGE}";
        $fn_mminputs = "${this_cycle}_MMINPUT_DOMAIN$i.${RANGE}_${RAP_start}";
        system("mv $fn_mminputn $fn_mminputs");
        system("cp $fn_mminputo $fn_mminputn");

        $fn_lowbdyo = "$RUNDIR/$last_cycle/${last_cycle}_LOWBDY_DOMAIN$i.${RANGE}";
        $fn_lowbdyn = "${this_cycle}_LOWBDY_DOMAIN$i.${RANGE}";
        $fn_lowbdys = "${this_cycle}_LOWBDY_DOMAIN$i.${RANGE}_${RAP_start}";

        system("mv $fn_lowbdyn $fn_lowbdys");
        system("cp $fn_lowbdyo $fn_lowbdyn");
        $fn_slmo = $RUNDIR."/".$last_cycle."/slmtmp.dat";
        $fn_slmn = "slmtmp.dat";
        system("cp $fn_slmo $fn_slmn");

        if ( -d $GEAPSTMP )
        {
          $fn_mminputo = "$GEAPSTMP/${last_cycle}_MMINPUT_DOMAIN$i.${RANGE}";
          $fn_mminputo = "${this_cycle}_MMINPUT_DOMAIN$i.${RANGE}" if( ! -s "$fn_mminputo");
          $fn_mminputn = "$GEAPSTMP/${this_cycle}_MMINPUT_DOMAIN$i.${RANGE}";
          $fn_mminputs = "$GEAPSTMP/${this_cycle}_MMINPUT_DOMAIN$i.${RANGE}_${RAP_start}";

          system("mv $fn_mminputn $fn_mminputs");
          system("cp $fn_mminputo $fn_mminputn");

          $fn_lowbdyo = "$GEAPSTMP/${last_cycle}_LOWBDY_DOMAIN$i.${RANGE}";
          $fn_lowbdyo = "${this_cycle}_LOWBDY_DOMAIN$i.${RANGE}" if( ! -s "$fn_lowbdyo");
          $fn_lowbdyn = "$GEAPSTMP/${this_cycle}_LOWBDY_DOMAIN$i.${RANGE}";
          $fn_lowbdys = "$GEAPSTMP/${this_cycle}_LOWBDY_DOMAIN$i.${RANGE}_${RAP_start}";

          system("mv $fn_lowbdyn $fn_lowbdys");
          system("cp $fn_lowbdyo $fn_lowbdyn");
        }
      }
      elsif ($MODEL eq "WRF")
      {
        $fn_mminputo = "$RUNDIR/$last_cycle/${last_cycle}_wrfinput_d0${i}_cold";
        $fn_mminputn = "${this_cycle}_wrfinput_d0${i}_cold";
        $fn_mminputs = "${this_cycle}_wrfinput_d0${i}_cold_${RAP_start}";

        system("mv -f $fn_mminputn $fn_mminputs");
        system("cp -f $fn_mminputo $fn_mminputn");

        if ( -d $GEAPSTMP )
        {
          $fn_mminputo = "$GEAPSTMP/${last_cycle}_wrfinput_d0${i}_cold";
          $fn_mminputo = "${this_cycle}_wrfinput_d0${i}_cold" if( ! -s "$fn_mminputo");
          $fn_mminputn = "$GEAPSTMP/${this_cycle}_wrfinput_d0${i}";
          $fn_mminputs = "$GEAPSTMP/${this_cycle}_wrfinput_d0${i}_cold_${RAP_start}";
          system("mv -f $fn_mminputn $fn_mminputs");
          system("cp -f $fn_mminputo $fn_mminputn");
        }
      }
    } # for
  } #if ( normal == 1)

# For climo, replace NNRP SSTs with higher resolution data

  if ( $BCS eq 'NNRP' || $BCS eq 'NNRP2' || $BCS eq 'FNL' )
  {
   # At the moment this is available for MM5 only
   if ($MODEL eq "MM5")
   {
    # Try to replace with 1dg SST if available
     print FILEL "\n   starting RT_M_oisst_mm5.csh at ", gmctime();
     system("$CSH_ARCHIVE/Forecast/RT_M_oisst_mm5.csh $MM5_start $this_cycle $CYC_INT $DATADIR");
     print FILEL "   ending  RT_M_oisst_mm5.csh at ", gmctime();
   }
   elsif ($MODEL eq "WRF")
   {
    # Try to replace with 1dg SST if available
     print FILEL "\n   starting RT_M_oisst_wrf.csh at ", gmctime();
     system("$CSH_ARCHIVE/Forecast/RT_M_oisst_wrf.csh $MM5_start $this_cycle $CYC_INT $DATADIR");
     print FILEL "   ending  RT_M_oisst_wrf.csh at ", gmctime();
   }

# For climo, replace NNRP Land Cover with higher resolution data

   print FILEL "\n   starting GLDAS replacement at ", gmctime();

   if ($MODEL eq "MM5") 
   { 
     $systat = system("$CSH_ARCHIVE/Forecast/RT_M_gldas_mm5.csh $MM5_start $this_cycle $CYC_INT $DATADIR");
   }
   elsif ($MODEL eq "WRF")
   {
     $systat = system("$CSH_ARCHIVE/Forecast/RT_M_gldas_wrf.csh $MM5_start $this_cycle $CYC_INT $DATADIR");
   }
   if ($systat != 0)
   {
#    print ("Unable to do GLDAS sfc replacement for cycle $this_cycle, stop here!\n");
#    exit -1;
     print ("Unable to do GLDAS sfc replacement for cycle $this_cycle, continue without GLDAS!\n");
   }
   print FILEL "   ending GLDAS replacement at ", gmctime();
  }

  print FILEL "   ------ Pre-processing END ------------";
  print FILEL "\n\n   ------ F-stage BEGIN -------------";

  #---------------------------------------------------------------------
  #	Collect and decode OBS upto most recent time
  #	 Do not do this for a RT_lite on 1 hourly cycling
  #---------------------------------------------------------------------

  if($CYC_INT > 1)
  {
    print FILEL "\n   start the processing of OBS for F-stage at ", gmctime();
    &get_and_decode_OBS ("F-stage",$p_fdda_start,$p_fdda_end,$this_cycle);
    print FILEL "   complete the processing of OBS for F-stage at ", gmctime();
  }

  #---------------------------------------------------------------------
  #       QC for OBS with MM5/WRF D1 or ETA analysis as first quess
  #       Prepare the fdda data input for MM5/WRF-FDDA
  #---------------------------------------------------------------------

  #
  # QC obs and reformat into FDDA input format
  #
  print FILEL "\n   starting F-stage  RT_G_rap_rtfdda_mm5qc.csh $OBS_start $this_cycle $this_cycle $last_cycle MM5_F at ", gmctime();
  if($MODEL eq "MM5")
  {
    # system("$CSH_ARCHIVE/Forecast/RT_G_rap_rtfdda.csh $OBS_start $this_cycle $this_cycle ");
    system("$CSH_ARCHIVE/Forecast/RT_G_rap_rtfdda_mm5qc.csh $OBS_start $this_cycle $this_cycle $last_cycle MM5_F");
  }
  elsif ($MODEL eq "WRF")
  {
     if ( $NO_WRFQC == 1 ) {
       # Grab the obs-qc files from a parallel MM5-RTFDDA system
       # ASSUME this script has been configured already!!!
       print FILEL "\n   Clone obs from MM5-RTFDDA: $GSJOBDIR/scripts/clone_obs.csh $this_cycle $RUNDIR\n ", gmctime();
       system("$GSJOBDIR/scripts/clone_obs.csh $this_cycle $RUNDIR > $GSJOBDIR/clone_obs.log 2>&1");
     } else {
       # This is the default...
       if ( $COLD_0012 != 2 ) { # Regular case
          print FILEL "\n   WRF QC of obs: $CSH_ARCHIVE/Forecast/RT_G_rtfdda_wrfqc.csh \n ", gmctime();
        # To save obs for QC pass in 1 instead of 0 as the last arg
          system("$CSH_ARCHIVE/Forecast/RT_G_rtfdda_wrfqc.csh $this_cycle $last_cycle $RAP_start $NODE WRF_F 0");
       } else { # 6-hourly cycling with 48-hr coldstart analysis
          print FILEL "\n   WRF QC of obs: $CSH_ARCHIVE/Forecast/RT_G_rtfdda_wrfqc.csh \n ", gmctime();
        # To save obs for QC pass in 1 instead of 0 as the last arg
          system("$CSH_ARCHIVE/Forecast/RT_G_rtfdda_wrfqc.csh $this_cycle $last_cycle $QC_start $NODE WRF_F 0");
       }
    }
  }

  print FILEL "   ending   F-stage   rap_rtfdda.csh at ", gmctime();
} ### end of ######## NOT CLONE INPUT, NOT CLONE INPUT ########################

#
# F-Stage-MM5/WRF starts here
#
print FILEL "   starting F-stage model $MODEL analysis at ", gmctime();

if ( $normal == 1 )
{
  # Normal cycle: Run MM5 from $hh-4 to $hh-1 with FDDA
  open (CRITIC, "$RUNDIR/critic.time");
  $rest_time = <CRITIC>;
  close(CRITIC);
  $time_max = $rest_time + $CYC_INT*60;
  print "     time_max=", $time_max, "  rest_time=", $rest_time;
  if ( $time_max >= 2*$CYC_INT*60 && $rest_time >= $CYC_INT*60 )
  {
    print FILEL "     this is a normal start (restart from last cycle) \n";
    chomp($time_max);
    chomp($rest_time);
    $fcst_id = 2;    # 1 -> cold-start final, 2 -> restart final, 3 -> restart this cycle
  }
  else
  {
    print "Exiting.....";
    exit (0);
  }
}
elsif ( $normal == 5)
{
  # Cold start from 00Z today
  # in 3hcyc - Run MM5/WRF from 00Z to 01Z. Then start FDDA upto 04Z, just like the normal cycle.
  $time_max = ($hh-0+$FIN_END)*60;
  $time_max = $time_max - 12*60 if($hh-$CYC_INT+$FIN_END-12 >= 0);
  $time_max = $time_max + 48*60 if ($COLD_0012 == 2 && $hh < 12);
  $time_max = $time_max + 36*60 if ($COLD_0012 == 2 && $hh >= 12);
  $rest_time = 0;
  $fcst_id = 1;    # 1 -> cold-start final, 2 -> restart final, 3 -> restart this cycle
  print FILEL "     this is a cold start from 00Z \n";
}
elsif ($normal == 17)
{
  # Cold start from 12Z today
  # in 3hcyc - Run MM5/WRF from 12Z to 13Z.
  # Then start FDDA upto 16Z, just like the normal cycle.

  # $hh_tmp = $hh; $hh_tmp += 12 if ($hh < 12 && $hh-$CYC_INT+$FIN_END >= 0);
  $hh_tmp = $hh; $hh_tmp += 24 if ($hh < 5);
  $time_max = ($hh_tmp-12+$FIN_END)*60;
  $time_max = 36*60 + ($hh_tmp-12+$FIN_END)*60 if ($COLD_0012 == 2);
  $time_max = 48*60 + ($hh_tmp-12+$FIN_END)*60 if ($COLD_0012 == 2 && $hh >= 17);
  $rest_time = 0;
  $fcst_id = 1;    # 1 -> cold-start final, 2 -> restart final, 3 -> restart this cycle
  print FILEL "     this is a cold start from 12Z \n";
}
elsif ($normal == 99)
{
  $time_max = $CYC_INT*60;
  $rest_time = 0;
  $fcst_id = 1;    # 1 -> cold-start final, 2 -> restart final, 3 -> restart this cycle
  print FILEL "     this is a cold start from now \n";
}

$rest_time_str = sprintf("%7.7d",$time_max);

#
# Call mm5/wrf_rtfdda.csh
#
if($MODEL eq "MM5")
{

  print FILEL "Writing needed mm5_F envars to $RUNDIR/$this_cycle/cshrc.MM5_F\n";
  print "Writing needed mm5_F envars to $RUNDIR/$this_cycle/cshrc.MM5_F\n";
  open(CSHRC,">$RUNDIR/$this_cycle/cshrc.MM5_F");
  print CSHRC "setenv fcst_id $fcst_id\n";
  print CSHRC "setenv timemax $time_max\n";
  print CSHRC "setenv resttime $rest_time\n" ;
  close(CSHRC);
}
elsif ($MODEL eq "WRF")
{
  if($normal != 1)
  {
    $CF_LENGTH = $time_max/60.;
  }
  else
  {
    $CF_LENGTH = $CYC_INT;
  }

  #Same as mm5_F: write envars file and change wrf-rtfdda
  # ARGS in RT_L_wrf_rtfdda.csh are
  # set this_cycle = $1 = $this_cycle - in tmp/$this_cycle/cshrc
  # set fcst_id    = $2 = $fcst_id
  # set FCST_H     = $3 = $CF_LENGTH
  # set start_date = $4 = $f_fdda_start
  # set cpun       = $5 = $NUM_PROCS - in tmp/$this_cycle/cshrc
  # set d4start    = $6 = $D4_start - in tmp/$this_cycle/cshrc
  # set out_int    = $7 = $OUT_INT - in tmp/$this_cycle/cshrc
  # set NODE       = $8 = $NODE - in tmp/$this_cycle/cshrc
  # set ETAAVN     = $9 = $BCS - in tmp/$this_cycle/cshrc

  print FILEL "Writing needed wrf_F envars to $RUNDIR/$this_cycle/cshrc.WRF_F\n";
  print "Writing needed wrf_F envars to $RUNDIR/$this_cycle/cshrc.WRF_F\n";
  open(CSHRC,">$RUNDIR/$this_cycle/cshrc.WRF_F");
  print CSHRC "setenv fcst_id $fcst_id\n";
  print CSHRC "setenv FCST_H $CF_LENGTH\n";
  print CSHRC "setenv start_date $f_fdda_start\n" ;
  print CSHRC "setenv start_date_P $p_fdda_start\n" ;
  
  close(CSHRC);

  #system("$CSH_ARCHIVE/Forecast/RT_L_wrf_rtfdda.csh $this_cycle $fcst_id $CF_LENGTH $f_fdda_start $NUM_PROCS $D4_start $OUT_INT $NODE $BCS");
}

print "\nWrite all needed Perl-parameters to $FLEXINPUT\n";

open (PRE_PROCESS_IN, ">>$FLEXINPUT");
print PRE_PROCESS_IN "\$FCST_LENGTH=$FCST_LENGTH;\n";
print PRE_PROCESS_IN "\$this_cycle_time=$this_cycle_time;\n";
print PRE_PROCESS_IN "\$this_cycle_date=$this_cycle_date;\n";
print PRE_PROCESS_IN "\$yy=\"$yy\";\n";
print PRE_PROCESS_IN "\$mo=\"$mo\";\n";
print PRE_PROCESS_IN "\$dd=\"$dd\";\n";
print PRE_PROCESS_IN "\$hh=\"$hh\";\n";
print PRE_PROCESS_IN "\$cycle_time=$cycle_time;\n";
print PRE_PROCESS_IN "\$last_cycle=$last_cycle;\n";
print PRE_PROCESS_IN "\$llast_cycle=$llast_cycle;\n";
print PRE_PROCESS_IN "\$last_cycle_date=$last_cycle_date;\n";
print PRE_PROCESS_IN "\$yesterday_cycle=$yesterday_cycle;\n";
print PRE_PROCESS_IN "\$yesterday_date=$yesterday_date;\n";
print PRE_PROCESS_IN "\$f_fdda_start=$f_fdda_start;\n";
print PRE_PROCESS_IN "\$p_fdda_start=$p_fdda_start;\n";
print PRE_PROCESS_IN "\$p_fdda_end=$p_fdda_end;\n";
print PRE_PROCESS_IN "\$normal=$normal;\n";
print PRE_PROCESS_IN "\$rest_time_str=\"$rest_time_str\";\n";
print PRE_PROCESS_IN "\$rest_time_str_old=\"$rest_time_str_old\";\n";
print PRE_PROCESS_IN "\$rest_time=$rest_time;\n";
print PRE_PROCESS_IN "\$time_max=$time_max;\n";

#$tempi = "";
#foreach $NN (@nodes)
#{
#  if ($NN eq $nodes[0])
#  {
#    $tempi = "(\"$NN\"";
#  }
#  else
#  {
#    $tempi = "$tempi, \"$NN\"";
#  }
#}
#$tempi = "$tempi)";
#print PRE_PROCESS_IN "\@nodes=$tempi;\n";

print PRE_PROCESS_IN "\$CLONEIN=$CLONEIN;\n";
print PRE_PROCESS_IN "\$THISNODE=\"$THISNODE\";\n";
print PRE_PROCESS_IN "\$MPNODE1=\"$MPNODE1\";\n";
print PRE_PROCESS_IN "\$BCS_date=$BCS_date;\n";
print PRE_PROCESS_IN "\$MM5_start=$MM5_start;\n";
print PRE_PROCESS_IN "\$RAP_start=$RAP_start;\n";
print PRE_PROCESS_IN "\$RAP_end=$RAP_end;\n";
print PRE_PROCESS_IN "\$OBS_start=$OBS_start;\n";
print PRE_PROCESS_IN "\$RAP_hour=$RAP_hour;\n";
print PRE_PROCESS_IN "1;\n";

close(PRE_PROCESS_IN);

if ( ${CLEAN_GEAPSTMP} )
{
   print FILEL "Removing  $GEAPSTMP after F-analysis at ", gmctime();
   print "Removing  $GEAPSTMP after F-analysis\n";
   system ("rm -rf $GEAPSTMP");
}

if ( ${CLEAN_GEAPSKEP} )
{
   print FILEL "Removing  $GEAPSKEP after F-analysis at ", gmctime();
   print "Removing  $GEAPSKEP after F-analysis\n";
   system ("rm -rf $GEAPSKEP");
}

print FILEL "Finished pre-processing for F-analysis at ", gmctime();
print "Exiting pre_process_F.pl\n";

close (FILEL);

exit(0);

########################### SUB ROUTINES #######################################

#-------------------------------------------------------------------------------
# Name: usage
# Arguments: none
# Return: none
# Description: Gives a description of how to run this script
#-------------------------------------------------------------------------------

sub usage
{
  if (!$FLEXINPUT)
  {
    print "Configuration file is not specified ......Exiting\n";
    exit(-1);
  }
  print "Usage check passed\n";
}


#-------------------------------------------------------------------------------
# Name: setCycleTimes
# Arguments: none
# Return: none
# Description: This method calculates times and dates that are necessary for a
#              model run
#-------------------------------------------------------------------------------

sub setCycleTimes
{
  # same as this_cycle
  $this_cycle_time = substr($this_cycle,0,10);

  # this cycle's date
  $this_cycle_date = substr($this_cycle,0,8);

  # this cycle's year
  $yy = substr($this_cycle,2,2);

  # this cycle's month - 1
  $mo = substr($this_cycle,4,2)-1;

  # this cycle's day of month
  $dd = substr($this_cycle,6,2);

  # this cycle's hour
  $hh = substr($this_cycle,8,2);

  # this cycle's minute
  $mm = substr($this_cycle,10,2);

  # EPOCH seconds for cycle hour
  $cycle_time = timegm(0,$mm,$hh,$dd,$mo,$yy);

  # last cycle's time
  $last_cycle = &hh_advan_date($this_cycle, -$CYC_INT);

  # the cycle's time that ran before the last cycles
  $llast_cycle = &hh_advan_date($last_cycle, -$CYC_INT);

  # last cycle's date
  $last_cycle_date = substr($last_cycle,0,8);

  # the cycle's time that ran 24 hrs ago
  $yesterday_cycle = &hh_advan_date($this_cycle, -24);

  # the cycle's date that ran 24 hrs ago
  $yesterday_date =  substr(&hh_advan_date($this_cycle, -24),0,8);

  # Time frames of the FDDA cycle:

  # Final analysis start time
  $f_fdda_start = &hh_advan_date($this_cycle, (-$CYC_INT+$FIN_END));

  # the time of f_fdda_start in YYYY-MM-DD_HH
  $f_fdda_start_str = substr($f_fdda_start,0,4).'-'.
                      substr($f_fdda_start,4,2).'-'.
                      substr($f_fdda_start,6,2).'_'.
                      substr($f_fdda_start,8,2);

  # Prelim analysis start time
  $p_fdda_start = &hh_advan_date($this_cycle, $FIN_END);

  # Prelim analysis end time
  $p_fdda_end = &hh_advan_date($this_cycle, 1);

  if ($DEBUG)
  {
    print "\n****************Cycle times **************\n";
    print "this_cycle_time=$this_cycle_time\n";
    print "this_cycle_date=$this_cycle_date\n";
    print "yy=$yy\n";
    print "mo=$mo\n";
    print "dd=$dd\n";
    print "hh=$hh\n";
    print "mm=$mm\n";
    print "cycle_time=$cycle_time\n";
    print "last_cycle=$last_cycle\n";
    print "llast_cycle=$llast_cycle\n";
    print "last_cycle_date=$last_cycle_date\n";
    print "yesterday_cycle=$yesterday_cycle\n";
    print "yesterday_date=$yesterday_date\n";
    print "f_fdda_start=$f_fdda_start\n";
    print "p_fdda_start=$p_fdda_start\n";
    print "p_fdda_end=$p_fdda_end\n";
  }
}

#-------------------------------------------------------------------------------
# Name: checkForCrashInPrevCycle
# Arguments: none
# Return: none
# Description: This methods checks for a crash in the prev.cycle and removes
#              prev. cycle's restart-files and large rsl.out.* and rsl.error.*
#              files
# UPDATED 5/1/2012.  Don't remove restart-files (ie, don't force a cold
# start) if log files are big - they may be big for a legit reason.
# Still scrub big log files to avoid filling small disks.  RSR
#-------------------------------------------------------------------------------

sub checkForCrashInPrevCycle
{
  # If there is a crash in last_cycle, clean junk files
  if (-e $RUNDIR."/".$last_cycle."/".${MODEL}."_F")
  {
    if ($DEBUG)
    {
      print "copying $last_cycle/${MODEL}_F/rsl.out.0000 to zrsl.out.0000\n";
    }
    system("cp $RUNDIR/$last_cycle/${MODEL}_F/rsl.out.0000 $RUNDIR/$last_cycle/${MODEL}_F/zrsl.out.0000");
  }

  foreach $last_rsl (<$RUNDIR/$last_cycle/${MODEL}_F/rsl*>)
  {
    # last_rsl is either rsl.out.*  or rsl.error.*
    if ( -s "$last_rsl" > 10000000)
    {
      if ($DEBUG)
      {
#        print "Previous cycle crashed, removing $RUNDIR/restart_files/r-*\n";
        print "Removing large log files from previous cycle\n";
      }
      if($MODEL eq "MM5"){
#         system("rm $RUNDIR/restart_files/r-*");   #force to abort the restart
      }else{
#         system("rm $RUNDIR/restart_files/${last_cycle}.SAVE_DOMAIN*");   #force to abort the restart
#         system("rm $RUNDIR/restart_files/${last_cycle}.RESTART_DOMAIN*");   #force to abort the restart
      }
      unlink $last_rsl;
    }
  }

  foreach $last_rsl (<$RUNDIR/$last_cycle/${MODEL}_P/rsl*>)
  {
    unlink $last_rsl if ( -s "$last_rsl" > 10000000);
  }
}

#-------------------------------------------------------------------------------
# Name: makeRUNDIRLogFile
# Arguments: name of the logfile
# Return: none
# Description: This method creates the 'timing' logfile in RUNDIR/this_cycle
#-------------------------------------------------------------------------------

sub makeRUNDIRLogFile
{
  my $fileName = $_[0];

  if(-e $RUNDIR."/".$this_cycle."/".$fileName)
  {
    system("rm $fileName");
  }
  select(FILEL);
  $| =1;
  open(FILEL, ">>$fileName") || do { warn "Can't open file $fileName: $!\n" };
  select(STDERR);
}

#-------------------------------------------------------------------------------
# Name: rescueMode
# Arguments: none
# Return: none
# Description:
#   This method removes the current restart_files, copies the prev. cycle's MM5-P
#   restart files to the current restart_file dir, and writes some number into
#   RUNDIR/critic.time
#-------------------------------------------------------------------------------

sub rescueMode
{
  print "          Rescue mode ..................                           \n";
  print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n";
  system("mv $RUNDIR_ROOT/restart_files/r* $RUNDIR_ROOT/restart_files/zold/.");
  system("cp $RUNDIR/$last_cycle/MM5_P/restrts/r* $RUNDIR/restart_files/ ");
  system("cd $RUNDIR/restart_files; gunzip *gz");

  opendir(IN,"$RUNDIR_ROOT/restart_files");
  ($one_rest)=grep(/0000$/,readdir(IN));
  close(IN);

  $recov_res_time = substr($one_rest,6,6)+0;

  open(CRITIC,">$RUNDIR/critic.time");
  print CRITIC $recov_res_time;
  close(CRITIC);
}

#-------------------------------------------------------------------------------
# Name: getCycleStatus
# Arguments: none
# Return: none
# Description:
#   This method determines the status (normality) of the current cycle:
#   Returns :
#    0
#    1  - normal
#    5  - previous cycle failed and $COLD_0012 = 1, i.e., cold start from 00Z
#    17 - previous cycle failed and $COLD_0012 = 1, i.e., cold start from 12Z
#    99 - cold start now
#-------------------------------------------------------------------------------

sub getCycleStatus
{
  my $norm = 0;

  if($MPPJOB eq "yes")
  {
    $rest_time_str_old = sprintf("%7.7d",$rest_time);
    if($MODEL eq "MM5")
    {
      @res_files = <$RUNDIR/restart_files/*${rest_time_str_old}-0000>;
    }
    elsif ($MODEL eq "WRF")
    {
      if($RESTART_PER_CORE) {
        @res_files = <$RUNDIR/${last_cycle}/WRF_F/r-*-${f_fdda_start_str}*>;
      } else {
        @res_files = <$RUNDIR/${last_cycle}/WRF_F/wrfrst_d*_${f_fdda_start_str}*>;
      }
    }
    $cnt =  @res_files;
    if($RESTART_PER_CORE) {
      $NFAC = $NUM_PROCS
    } else {
      $NFAC = 1;
    }
    if ($cnt >= $NUM_DOMS*$NFAC && <$RUNDIR/${last_cycle}/*.${RANGE}_F> )
    {
      # if there are at least as many restart-files as there are domains
      # and there are F-OUTPUT-files in last_cycle
      # find all savefiles for all $NUM_DOMS domains
      $norm = 1;         # Normal cycle             # from last_cycle
      print FILEL "Found $cnt MPP restart-files from previous cycle, so start normally \n";
      print FILEL "        RESTART = $rest_time                                  \n";
    }
  }
  else
  {
    @res_files = <$RUNDIR/$last_cycle/*SAVE_DOMAIN*> if($MODEL eq "MM5");
    @res_files = <$RUNDIR/$last_cycle/WRF/wrfrst_d*_${f_fdda_start_str}*> if($MODEL eq "WRF");
    $cnt =  @res_files;
    if ($cnt >= $NUM_DOMS && -e "$RUNDIR/${last_cycle}/*.${RANGE}_F" && $rest_time > 1)
    {
      # find all savefiles for all $NUM_DOMS domains
      $norm = 1;         # Normal cycle
      print FILEL "Found $cnt restart-files from previous cycle, so start normally \n";
    }
  }

  if($norm == 0)
  {
    print FILEL "Could not find restart-files from $last_cycle, setting normal to 5,17,99\n";
    print "Could not find restart-files from $last_cycle, setting normal to 5,17,99\n";

    if($MPPJOB eq "yes" && -d "$RUNDIR/${last_cycle}")
    {
      # clean mpp restart directory
      if( $MODEL eq "MM5")
      {
        system("rm $RUNDIR/${last_cycle}/MM5_F/restrts");
        system("$MustHaveDir $RUNDIR/${last_cycle}/MM5_F/restrts");
        if($NODE > 0)
        {
          foreach $NN (@nodes)
          {
            system("ssh $NN mv $RUNDIR/restart_files/* $RUNDIR/restart_files/zold/.");
          }
        }
        else
        {
          system("mv $RUNDIR/restart_files/r-* $RUNDIR/restart_files/zold/.");
        }
      }
      elsif ($MODEL eq "WRF")
      {
        if($RESTART_PER_CORE) {
          print "Not moving per-core restart files!\n";
        } else {
          print "Not moving restart files!\n";
        }
      }
    }

    if($COLD_0012 == 1 || $COLD_0012 == 2)
    {
      # cold-start at either 00Z or 12Z only
      if ($hh >= 5 && $hh < 17)
      {
        #   Cold start from 00Z today
        #   Run MM5 from 00Z to 01Z. Then start FDDA upto 04Z, just like the normal cycle.
        $norm = 5;   # For this case, cold-start is needed (from 00Z)
        print FILEL " !* Previous cycle failed, so do cold start from 00Z \n";
      }
      elsif ($hh < 5 || $hh >= 17)
      {
        #   Cold start from 12Z today
        #   Run MM5 from 12Z to 13Z. Then start FDDA upto 16Z, just like the normal cycle.
        $norm = 17;   # For this case, cold-start is also needed (from 12Z)
        print FILEL " !* Previous cycle failed, so do cold start from 12Z \n";
      }
    }
    else
    {
      # cold start from the current hour
      $norm  = 99;
      print FILEL " !* Previous cycle failed, so do cold start from current hour $hh \n";
    }
  } #norm == 0

  return $norm;
}

#-------------------------------------------------------------------------------
# Name: setStartTimes
# Arguments: $normal
# Return:
# Description: This method sets RAP_START, MM5_START, etc.
#-------------------------------------------------------------------------------

sub setStartTimes
{
  my $norm = $_[0];

  if ($norm == 1)
  {
    $ffsh =substr($f_fdda_start,8,2);
    $RAP_start = substr($f_fdda_start,0,8).sprintf("%02d",$BCSHM{$ffsh+0});
    $RAP_start = &hh_advan_date($RAP_start, 0);
    $MM5_start = $RAP_start;   # normal restart: for BC only
  }

  if ($norm == 5)
  {
    $MM5_start = $BCS_date;
    $MM5_start = &hh_advan_date($BCS_date, 12) if($hh-$CYC_INT+$FIN_END-12 >= 0);
    $MM5_start = &hh_advan_date($BCS_date, -48) if( $COLD_0012 == 2 && $hh < 12);
    $MM5_start = &hh_advan_date($BCS_date, -36) if( $COLD_0012 == 2 && $hh >= 12);
    $RAP_start = $MM5_start;
    $BCS_date  = $RAP_start if( $COLD_0012 == 2 );
    $f_fdda_start = $MM5_start;
  }

  if ($norm == 17)
  {
    $MM5_start = $BCS_date;
    $MM5_start = &hh_advan_date($BCS_date, 12) if($hh < 5 && $hh-$CYC_INT+$FIN_END >= 0);
    $MM5_start = &hh_advan_date($BCS_date, -36) if($hh < 5 && $COLD_0012 == 2);
    $MM5_start = &hh_advan_date($BCS_date, -48) if($hh > 17 && $COLD_0012 == 2);
    $RAP_start = $MM5_start;
    $BCS_date  = $RAP_start if( $COLD_0012 == 2 );
    $f_fdda_start = $MM5_start;
  }

  if ($norm  == 99)
  {
    $MM5_start = $f_fdda_start;
    $ffsh =substr($f_fdda_start,8,2);
    $RAP_start = substr($f_fdda_start,0,8).sprintf("%02d",$BCSHM{$ffsh+0});
    $RAP_start = &hh_advan_date($RAP_start, 0);
  }
}

#-------------------------------------------------------------------------------
# Name: getRestartFilesForNormalCycle
# Arguments: none
# Return: none
# Description:
#   If a cycle is normal, i.e., we found restart files from the previous
#   cycle, then this helper method moves those files to the needed places.
#-------------------------------------------------------------------------------

sub getRestartFilesForNormalCycle
{

# FVDB 20081117 Clean up the zold dir, before moving restart files from cycle
# before last one.
  print "\nDeleting old restart file from old $llast_cycle\n";
  if($MODEL eq "WRF") {
    if($RESTART_PER_CORE) {
      print "Not deleting old per-core restart files!\n";
    } else {
      print "Not deleting old restart files!\n";
    }
  }

  for ($i=1; $i <= $NUM_DOMS; $i++)
  {
    if($MODEL eq "MM5")
    {
      if($NODE != 0 && -d $GEAPSTMP && MPPJOB ne "yes")
      {
        system("mv $GEAPSTMP/$last_cycle.SAVE_DOMAIN${i}_F $GEAPSKEP/${this_cycle}.RESTART_DOMAIN$i");
        if( ! -s "$GEAPSKEP/${this_cycle}.RESTART_DOMAIN$i")
        {
          system("cp $RUNDIR/$last_cycle/$last_cycle.SAVE_DOMAIN${i}_F $GEAPSKEP/${this_cycle}.RESTART_DOMAIN$i");
        }
      }
      $fn_save = $RUNDIR."/".$last_cycle."/".$last_cycle.".SAVE_DOMAIN".$i."_F";
      $fn_rest = $this_cycle.".RESTART_DOMAIN".$i;
      system("ln -s ${fn_save} ${fn_rest}");
    }
    elsif ($MODEL eq "WRF")
    {
      if ($RESTART_PER_CORE) {
         print "Not linking per-core restart files in restart_files directory!\n";
      } else {
         print "Not linking restart files in restart_files directory!\n";
      }
    }
  }#for

  if ($MODEL eq "MM5")
  {
    if (-s "$RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_P+FCST" ||
        -s "$GEAPSTMP/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_P+FCST" )
    {
      system("cat $RUNDIR/${llast_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F     \\
                  $RUNDIR/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F      \\
                  $RUNDIR/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_P+FCST \\
                > $GEAPSKEP/${last_cycle}_MMOUTPUT_DOMAIN1");
      if( ! -s "$GEAPSKEP/${last_cycle}_MMOUTPUT_DOMAIN1")
      {
        if( -e $GEAPSKEP )
        {
          system("cat $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F      \\
                      $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_P+FCST  \\
                    > $GEAPSKEP/${last_cycle}_MMOUTPUT_DOMAIN1");
        }
        else
        {
          system("cat $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F      \\
                      $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_P+FCST  \\
                    > ${last_cycle}_MMOUTPUT_DOMAIN1");
        }
      }

      if( $CYC_INT < 2)
      {
        system("cat $RUNDIR/$llast_cycle/${llast_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F    \\
                    $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F      \\
                    $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_P+FCST \\
                  > $RUNDIR/$this_cycle/${last_cycle}_MMOUTPUT_DOMAIN1");
      }
      else
      {
        system("cat $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F      \\
                    $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_P+FCST  \\
                  > $RUNDIR/$this_cycle/${last_cycle}_MMOUTPUT_DOMAIN1");
      }
    } # if (-s...)
    elsif (<$RUNDIR/$last_cycle/MM5_P/MMOUT_DOMAIN1*> || <$GEAPSTMP/MM5_P/MMOUT_DOMAIN1*>)
    {
      system("cat $RUNDIR/${llast_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F \\
                  $RUNDIR/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F  \\
                  $RUNDIR/MM5_P/MMOUT_DOMAIN1*                       \\
                > $GEAPSKEP/${last_cycle}_MMOUTPUT_DOMAIN1");

      if( ! -s "$GEAPSKEP/${last_cycle}_MMOUTPUT_DOMAIN1")
      {
        if( -e $GEAPSKEP)
        {
          system("cat $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F   \\
                      $RUNDIR/$last_cycle/MM5_P/MMOUT_DOMAIN1*                        \\
                    > $GEAPSKEP/${last_cycle}_MMOUTPUT_DOMAIN1");
        }
        else
        {
          system("cat $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F   \\
                      $RUNDIR/$last_cycle/MM5_P/MMOUT_DOMAIN1*                        \\
                    > ${last_cycle}_MMOUTPUT_DOMAIN1");
        }
      }

      if( $CYC_INT < 2)
      {
        system("cat $RUNDIR/$llast_cycle/${llast_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F \\
                    $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F   \\
                    $RUNDIR/$last_cycle/MM5_P/MMOUT_DOMAIN1*                        \\
                  > ${last_cycle}_MMOUTPUT_DOMAIN1");
      }
      else
      {
        system("cat $RUNDIR/$last_cycle/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F   \\
                    $RUNDIR/$last_cycle/MM5_P/MMOUT_DOMAIN1*                        \\
                  > ${last_cycle}_MMOUTPUT_DOMAIN1");
      }
    } # elsif
  } # if MODEL == MM5

  if ( -d $GEAPSTMP )
  {
    if($MODEL eq "MM5")
    {
      system("mv $GEAPSTMP/${last_cycle}_MMOUTPUT_DOMAIN1.${RANGE}_F $GEAPSKEP/") if($CYC_INT < 2);
      for ($i=1; $i <= $NUM_DOMS; $i++)
      {
        $fn_mminputo = "$GEAPSTMP/${last_cycle}_MMINPUT_DOMAIN$i.${RANGE}";
        system("mv $fn_mminputo $GEAPSKEP");
        $fn_lowbdyo = "$GEAPSTMP/${last_cycle}_LOWBDY_DOMAIN$i.${RANGE}";
        system("mv $fn_lowbdyo $GEAPSKEP");
      }
    }
    elsif ($MODEL eq "WRF")
    {
      system("mv $GEAPSTMP/wrfout_d01* $GEAPSKEP/") if($CYC_INT < 2);
      for ($i=1; $i <= $NUM_DOMS; $i++)
      {
        $fn_mminputo = "$GEAPSTMP/${last_cycle}_wrfinput_d0${i}_cold";
        system("mv -f $fn_mminputo $GEAPSKEP");
      }
    }
  }
}
#
#
#
sub findHoursOffset {

  my ($year,$mon,$day,$hour);
  my ($year0,$mon0,$day0,$hour0);
  my ($secs_diff,$hours_diff);

  $year = substr($this_cycle,0,4);
  $mon  = substr($this_cycle,4,2)-1;
  $day  = substr($this_cycle,6,2);
  $hour = substr($this_cycle,8,2);

  $year0 = substr($RAP_start,0,4);
  $mon0  = substr($RAP_start,4,2)-1;
  $day0  = substr($RAP_start,6,2);
  $hour0 = substr($RAP_start,8,2);

  $secs_diff = timegm(0,0,$hour,$day,$mon,$year) + $FIN_END*3600 -
               timegm(0,0,$hour0,$day0,$mon0,$year0);

  $hours_diff = $secs_diff/3600;

  print "Hours offset from RAP_start = $hours_diff hours\n";

  return $hours_diff;

}

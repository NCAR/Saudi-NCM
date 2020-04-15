#!/usr/bin/perl
use Time::gmtime 'gmctime';
use Time::Local;

print "PID: $$\n";

$FLEXINPUT  =  $ENV{FLEXINPUT};

if (-e $FLEXINPUT)
{
  print "pre_process_P+FCST: Using job configuration in $FLEXINPUT\n";
  print "Starting pre-processing for P+FCST on $ENV{HOST}\n";
}
else
{
 print "\nFile $FLEXINPUT is missing..... EXITING\n";
 exit(-1);
}

# This input file defines the configuration for the job
require $FLEXINPUT;

# This script contains the hh_advan_date and mm_advan_date subroutines
require $PERL_ARCHIVE.'/tools/advan_date.pl';

# This file contains the subroutine to collect obs
require $OBS_PROCESSOR;

# This script contains the setEnvVars subroutine
require $PERL_FLEX.'/setEnvVars.pl';

# Set the environment variables
print "\nSetting env vars\n";
&setEnvVars();

chdir $RUNDIR."/".$this_cycle;

$p_fdda_start_str = substr($p_fdda_start,0,4).'-'.
                    substr($p_fdda_start,4,2).'-'.
                    substr($p_fdda_start,6,2).'_'.
                    substr($p_fdda_start,8,2);

# Special case for 6-hourly cycling that does 48-hour cold-start analysis
if ($COLD_0012 == 2) {
   die "\$WRFQC_BACK is not defined!\n" if (! $WRFQC_BACK);
   $QC_start = hh_advan_date($this_cycle,-$WRFQC_BACK) 
}
# Opening the log file in RUNDIR/this_cycle
print "\nOpening the log file in $RUNDIR/$this_cycle/timing_pre_process_P+FCST\n";
&makeRUNDIRLogFile("timing_pre_process_P+FCST");

if ($MODEL eq "WRF")
{
  chdir ("$RUNDIR/$this_cycle/WRF_F");

  #This file contains the *_end times from F-Analysis
  require "$GSJOBDIR/tmp/$this_cycle/wrf_end_dates.pl";

 ###### This moved from RT_L_MM_WRF_rtfdda.csh
 #NO, it's in RT_L_wrf_rtfdda.csh, keep this there!
 #for( $d=1 ;$d <= $NUM_DOMS; $d++ )
 #{
 #  print "cp wrfrst_d0${d}_${y_end}-${m_end}-${d_end}_${h_end}:00:00 $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN$d\n";
 #  system ( "cp wrfrst_d0${d}_${y_end}-${m_end}-${d_end}_${h_end}:00:00 $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN$d" );
 #  ##mv wrfrst_d0${i}*  $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN${1}_F$FCST_HH
 #  ##ln -s $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN${i}_F$FCST_HH wrfrst_d0${i}_${y_end}-${m_end}-${d_end}_${h_end}:00:00
 #}
}

chdir $RUNDIR."/".$this_cycle;

$rest_time = $time_max;

open (CRITIC, ">$RUNDIR/critic.time");

if( ($MPPJOB eq "yes" && (-e "$RUNDIR/restart_files/r-01-${rest_time_str}-0000"))
    || (-e "$RUNDIR/${this_cycle}/${this_cycle}.SAVE_DOMAIN1_F"))
{
  print  CRITIC $rest_time;
}
elsif ($MODEL eq "WRF" && (-e "$RUNDIR/$this_cycle/WRF_F/wrfrst_d01_${p_fdda_start_str}:00:00" 
       || -e "$RUNDIR/${this_cycle}/WRF_F/r-01-${p_fdda_start_str}:00:00_0000"))
{
  print  CRITIC $rest_time;
}
else
{
  print  CRITIC 0;
  print  CRITIC "\n ${MODEL}_F -- Failure for restart at 0+${rest_time_str_old}";
  print  CRITIC "\n         Wait for the next cold-start time.";
  close (CRITIC);
  print       "       !!!${MODEL}_F -- Failure -- Wait for next cold-start!!!  \n";
  print FILEL "       !!!${MODEL}_F -- Failure -- Wait for next cold-start!!!  \n";
  exit(-1)
}

close (CRITIC);

if($MODEL eq "MM5" && $MPPJOB eq "yes")
{
  # clean mpp restart directory
  system("rm $RUNDIR/${this_cycle}/MM5_F/restrts");
  system("$MustHaveDir $RUNDIR/${this_cycle}/MM5_F/restrts");

  if ($NODE == 0)
  {
    #system("mv $RUNDIR/restart_files/r*${rest_time_str_old}* $RUNDIR/${this_cycle}/MM5_F/restrts");
    system("rm $RUNDIR/restart_files/zold/*");
    system("mv $RUNDIR/restart_files/r*${rest_time_str_old}* $RUNDIR/restart_files/zold");
  }
  else
  {
    foreach $NN (@nodes)
    {
      system("ssh $NN rm $RUNDIR/restart_files/zold/*");
      system("ssh $NN mv $RUNDIR/restart_files/r*${rest_time_str_old}* $RUNDIR/restart_files/zold");
    }
  }
}
print FILEL "   ending clean-up after F-stage at ", gmctime;

# The following is commented out because it duplicates the funtion
# set in 'submitCycleMM.csh'
#if ($VERI3HCYC == 1)
#{
#  &veri3hCyc();
#}

if ($CLIMOCOMPACTRT == 1)
{
  &climoCompactRT();
}

#  print FILEL "           \n******** SLEEP as needed ********** \n\n";
#  sleep(60.0) while(time < $cycle_time + 3600 + 1200);
#  sleep(60.0) while(time < $cycle_time + 3600 + 0);

if ( -e "$RUNDIR/cold_start.steps" ) {
   open(COLD,"$RUNDIR/cold_start.steps");
   ($steps_cf,$cold_cycle) = split " ",<COLD>;
   close(COLD);
} else {
   print FILEL " Warning: File $RUNDIR/cold_start.steps does not exist!\n";
   $cold_cycle = 0;
}

if ($FCST_LENGTH > 0)
{
  print FILEL "   ------ P-stage BEGIN -------------\n";

  if( $CLONEIN == 0 && $ETKF eq "no")
  { ######CLONE INPUT CLONE INPUT #########

    # Skip QC in CFDDA

    if ($BCS ne "NNRP" && $BCS ne "NNRP2" && $BCS ne "CFSR" && $BCS ne "CFSF" && $BCS ne "CFSV1" && $BCS ne "CFSV2" && $BCS ne "FNL"){

    #---------------------------------------------------------------------
    #       Collect OBS for most-recent 1-2 hours ($hh-1 to $hh+1)
    #        pass this for RT_lite on 1 hourly cycling
    #       Don't collect and qc obs, when $PRELIM is set to 0
    #---------------------------------------------------------------------

    if ($CYC_INT > 1 && $PRELIM == 1)
    {
      print FILEL "\n   start the processing of OBS for P-stage at ", gmctime;
      &get_and_decode_OBS ("P-stage",$p_fdda_start,$p_fdda_end,$this_cycle);
      print FILEL "   complete the processing of OBS for P-stage at ", gmctime;

      #---------------------------------------------------------------------
      #       QC OBS with WRF/MM5 D1 or ETA analysis as first quess
      #       Output: 2 hour ($hh-1 ~ $hh+1) FDDA data input for MM5/WRF-FDDA
      #       QC with little_r
      #---------------------------------------------------------------------
      if($MODEL eq "MM5")
      {
        print FILEL "\n   starting P-stage RT_G_rap_rtfdda_mm5qc.csh $p_fdda_start $p_fdda_end $this_cycle $last_cycle MM5_P at ", gmctime;
        #system("$CSH_ARCHIVE/Forecast/RT_G_rap_rtfdda.csh $p_fdda_start $p_fdda_end $this_cycle ");
        system("$CSH_ARCHIVE/Forecast/RT_G_rap_rtfdda_mm5qc.csh $p_fdda_start $p_fdda_end $this_cycle $last_cycle MM5_P");
      }
      elsif ($MODEL eq "WRF")
      {
        if ( $NO_WRFQC == 1 ) {
          # Grab the obs-qc files from a parallel MM5-RTFDDA system
          # ASSUME this script has been configured already!!!
          system("$GSJOBDIR/scripts/clone_obs.csh $this_cycle $RUNDIR");
        } elsif ( $COLD_0012 == 2 && $this_cycle == $cold_cycle) {
          if ($SKIP_WRFQC_P) {
             print FILEL "\n Skip WRFQC_P stage \n";
          } else {
             print FILEL "\n   WRF QC of obs: $CSH_ARCHIVE/Forecast/RT_G_rtfdda_wrfqc.csh \n ", gmctime;
             system("$CSH_ARCHIVE/Forecast/RT_G_rtfdda_wrfqc.csh $this_cycle $last_cycle $QC_start $NODE WRF_P 0");
          }
        } else {
          # This is the default...
          print FILEL "\n   WRF QC of obs: $CSH_ARCHIVE/Forecast/RT_G_rtfdda_wrfqc.csh \n ", gmctime;
          system("$CSH_ARCHIVE/Forecast/RT_G_rtfdda_wrfqc.csh $this_cycle $last_cycle $RAP_start $NODE WRF_P 0");
        }
      }
      print FILEL "   ending   P-stage  rap_rtfdda.csh at ", gmctime;
    } # if ($CYC_INT > 1 && $PRELIM == 1)
   }
  }  ######CLONE INPUT CLONE INPUT #########

  #---------------------------------------------------------------------
  #      Normal cycle: Run MM5 with FDDA for 2 hours upto $hh + 1,
  #                    and then continue fcst for 6 hours
  #      Output: 2 hours FDDA output and 6 hour FCST for 3 domains
  #---------------------------------------------------------------------

  print FILEL "   starting P-stage mm5.csh at ", gmctime;

  open (CRITIC, "$RUNDIR/critic.time"); $rest_time = <CRITIC>; close(CRITIC);
  chomp($rest_time);

  $time_max = $rest_time + ($RAP_hour - 5)*60;
  $time_max = $rest_time + ($FCST_LENGTH-$FIN_END)*60;
# $time_max = $rest_time + 900;
# $time_max = $rest_time + 720;
# $time_max = $rest_time + 540;
# $time_max = $rest_time + 360;
# $time_max = $rest_time + 1500;

  $fcst_id = 3;    # 1 -> cold-start final, 2 -> restart final, 3 -> restart this cycle

  $cnt = 0;
  @res_files = ();

  if($MPPJOB eq "yes")
  {
    @res_files = <$RUNDIR/restart_files/*${rest_time}-00*> if($MODEL eq "MM5");
    if($MODEL eq "WRF") {
      if($RESTART_PER_CORE) {
        @res_files = <$RUNDIR/${this_cycle}/WRF_F/r-*-${p_fdda_start_str}*>;
      } else {
        @res_files = <$RUNDIR/${this_cycle}/WRF_F/wrfrst_d*_${p_fdda_start_str}*>;
      }
    }
    $cnt = @res_files;

    if($RESTART_PER_CORE) {
      $NFAC = $NUM_PROCS;
    } else {
      $NFAC = 1;
    }

    if ($cnt >= $NUM_DOMS*$NFAC)
    {
      # find savefiles for all domains
      print FILEL "   Find MPP restart-files from final fdda (MM5/WRF_F) cycle --> Run MM5/WRF_P \n";
    }
    else
    {
      print FILEL "   Not find MPP restart-files from final fdda (MM5/WRF_F) cycle --> Skip MM5_P \n";
      close(FILEL);
      print       "   Not find MPP restart-files from final fdda (MM5/WRF_F) cycle --> Skip MM5_P \n";
      exit(-1);
    }
  }

  if( $MODEL eq "MM5")
  {
    open(CSHRC,">$RUNDIR/$this_cycle/cshrc.MM5_P+FCST");
    print CSHRC "setenv fcst_id $fcst_id\n";
    print CSHRC "setenv timemax $time_max\n";
    print CSHRC "setenv resttime $rest_time\n" ;
    close(CSHRC);
    print FILEL " Wrote $RUNDIR/$this_cycle/cshrc.MM5_P+FCST\n";
    #system("$CSH_ARCHIVE/Forecast/RT_L_mm5_rtfdda.csh $this_cycle $fcst_id $time_max $rest_time $NUM_PROCS $D4_start $OUT_INT $NODE $BCS");
  }
  elsif ($MODEL eq "WRF")
  {
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

    open(CSHRC,">$RUNDIR/$this_cycle/cshrc.WRF_P+FCST") || die ("Cannot open file $RUNDIR/$this_cycle/cshrc.WRF_P+FCST");
    print CSHRC "setenv fcst_id $fcst_id\n";
    print CSHRC "setenv FCST_H $FCST_LENGTH\n";
    print CSHRC "setenv start_date $p_fdda_start\n" ;
    print CSHRC "setenv NORMAL $normal\n";
    close(CSHRC);
    #system("$CSH_ARCHIVE/Forecast/RT_L_wrf_rtfdda.csh $this_cycle $fcst_id $FCST_LENGTH $p_fdda_start $NUM_PROCS $D4_start $OUT_INT $NODE $BCS");
  }
} #if ($FCST_LENGTH > 0)
else
{
   print FILEL " FCST_LENGTH = $FCST_LENGTH: No P+FCST-stage done\n";
}

if ( ${CLEAN_GEAPSTMP} )
{
   print FILEL "Removing  $GEAPSTMP after P+FCST at ", gmctime;
   print "Removing  $GEAPSTMP after P+FCST\n";
   system ("rm -rf $GEAPSTMP");
}

if ( ${CLEAN_GEAPSKEP} )
{
   print FILEL "Removing  $GEAPSKEP after P+FCST at ", gmctime;
   print "Removing  $GEAPSKEP after P+FCST\n";
   system ("rm -rf $GEAPSKEP");
}

print FILEL "\nending   pre_process_P+FCST.pl at ", gmctime;
close(FILEL);
exit(0);


###################### SUB ROUTINES ############################################

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
  if (! open(FILEL, ">>$fileName")){
      print "ERROR: cannot open file $fileName\n";
      exit -1;
  }
# select(FILEL);
# $| =1;
# open(FILEL, ">>$fileName") || do { warn "Can't open file $fileName: $!\n" };
# select(STDERR);
}

#-------------------------------------------------------------------------------
# Name: veri3hCyc
# Arguments: none
# Return: none
# Description: Calls $GSJOBDIR/perl/veri_rtfdda3hcyc.pl
#-------------------------------------------------------------------------------

sub veri3hCyc
{
  print FILEL "     ++ Submit verification script at ", gmctime;
 #system("$GSJOBDIR/perl/veri_rtfdda3hcyc.pl $this_cycle $RANGE $NODE $GSJOBID > $RUNDIR/zout_cron.rt3h_veri 2>&1 &");
  system("$PERL_FLEX/veri_rtfdda3hcyc_range_wrf.pl > $RUNDIR/zout_cron.rt3h_veri 2>&1 &");
}

#-------------------------------------------------------------------------------
# Name: climoCompactRT
# Arguments: none
# Return: none
# Description: Calls /data/climo/run/run_compact_RT.pl
#-------------------------------------------------------------------------------

sub climoCompactRT
{
  print FILEL "     ++ Submit climo process at ", gmctime;
  system("/data/climo/run/run_compact_RT.pl -f $RUNDIR/${this_cycle}/${this_cycle}_MMOUTPUT_DOMAIN3.${MM5HOST}_F &");
}

#-------------------------------------------------------------------------------
# Name: createMM5VMET
# Arguments: "F" or "P+FCST"
# Return: none
# Description: Creates compact MM5 raw files for VMET display
#              Calls '/data/mm5_vmet/script/run_compact_RT.pl'
#              Copies MM5 VMET files
#-------------------------------------------------------------------------------

sub createMM5VMET
{
  my $postfix = $_[0];

  print FILEL "   start creating MM5 raw files for VMET display ($postfix)\n";
  foreach $domain (1..$NUM_DOMS)
  {
    $file="${this_cycle}_MMOUTPUT_DOMAIN${domain}.${RANGE}_$postfix";
    next if(-z $file);
    print FILEL "  Calling /data/mm5_vmet/script/run_compact_RT.pl -f $file $\n";
    system("/data/mm5_vmet/script/run_compact_RT.pl -f $file");
  }
  print FILEL "   ending creating MM5 raw files for VMET display ($postfix)\n";

  print FILEL "   start remote copying og MM5 VMET files\n";
  if ($postfix eq 'F')
  {
    system("/data/mm5_vmet/script/scp.pl.${RANGE} -c $this_cycle -t final > $RUNDIR/zout_cron_vmet_scp 2>&1 &");
  }
  else
  {
    system("/data/mm5_vmet/script/scp.pl.${RANGE} -c $this_cycle -t fcst > $RUNDIR/zout_cron_vmet_scp 2>&1 &");
  }
  print FILEL "     end remote coping of MM5 VMET files\n";
}

#!/usr/bin/perl

use Time::gmtime;
use Time::Local;

print "PID: $$\n";

$FLEXINPUT  =  $ENV{FLEXINPUT};


if (-e $FLEXINPUT)
{
  print "post_process_clean.pl: Using job configuration in $FLEXINPUT\n" if ($DEBUG);
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

# This script contains the setEnvVars subroutine
require $PERL_FLEX.'/setEnvVars.pl';

# Import postprocinput.pl
if ( -e "$GSJOBDIR/postprocinput.pl") {
   require "$GSJOBDIR/postprocinput.pl";
} else {
   print "\nWarning: $GSJOBDIR/postprocinput.pl does not exist!\n";
}

# Set the environment variables
print "\nSetting env vars\n";
&setEnvVars();


# cleanup if a forecast job is terminated.
# - Move the forecast files to cycle_dir
my $cycle_dir = $RUNDIR."/".$this_cycle;
chdir $cycle_dir."/WRF_P";
my $wrfout_patterns = "wrfout_d0*";
my @wrfout_files = < $wrfout_patterns >;
for my $file ( @wrfout_files ) {
  next if ( -l $file );
  my $file_size = -s $file;
  next if ( $file_size < 100000 );
  
  my $new_file = "${cycle_dir}/${file}.${MM5HOST}_P+FCST";
  system ( "mv $file $new_file" );
  symlink( $new_file, $file );
}

chdir $RUNDIR."/".$this_cycle;

# Opening the log file in RUNDIR/this_cycle
print "\nOpening the log file in $RUNDIR/$this_cycle/timing_post_process_clean\n";

&makeRUNDIRLogFile("timing_post_process_clean");
print FILEL "   Starting clean up at ", gmctime();


##### Write log files: mm5_print.out or wrf_print.out

##### MOVED FROM RT_L_PBS_$MODEL_rtfdda.csh to here ############################
# ES - 20071218: According to Francois and Rong, this causes problems with QC and 
#     verification, so commented out
#if ( $FCST_LENGTH == 0)
#{
#  if ( $MODEL eq "MM5")
#  {
#    # write the mm5_print.out file for jobs with FCST_LENGTH = 0
#    chdir ("$RUNDIR/$this_cycle/MM5_F");
#    system ( "ls -l >> mm5_print.out" );
#    system ( "echo \" \n\n --------------- MMLIF ------------------ \n\n\" >> mm5_print.out" );
#    system ( "cat mmlif >> mm5_print.out" );
#    system ( "echo \" \n --------------- OBS   ------------------ \n\n\" >> mm5_print.out" );
#    system ( "cat obs_* >> mm5_print.out" );
#    system ( "cp mm5_print.out $RUNDIR/$this_cycle/${this_cycle}_mm5_f_print.out" );
#  }
#
#  if ($MODEL eq "WRF")
#  {
#    chdir ("$RUNDIR/$this_cycle/WRF_F");
#    system ( "ls -l >> wrf_print.out" );
#    system ( "echo \" \n\n --------------- wrf namelist------------ \n\n\"" );
#    system ( "cat namelist.input >> wrf_print.out" );
#    system ( "echo \" \n --------------- OBS   ------------------ \n\n\"" );
#    system ( "cat obs_* >> wrf_print.out" );
#    system ( "cp wrf_print.out $RUNDIR/$this_cycle/${this_cycle}_wrf_f_print.out" );
#  }
#}
#
#if ( $FCST_LENGTH > 0 )
#{
#  if ( $MODEL eq "MM5")
#  {
#    # write the mm5_print.out file for jobs with FCST_LENGTH > 0
#    chdir ("$RUNDIR/$this_cycle/MM5_P");
#    system ( "ls -l >> mm5_print.out" );
#    system ( "echo \" \n\n --------------- MMLIF ------------------ \n\n\" >> mm5_print.out" );
#    system ( "cat mmlif >> mm5_print.out" );
#    system ( "echo \" \n --------------- OBS   ------------------ \n\n\" >> mm5_print.out" );
#    system ( "cat obs_* >> mm5_print.out" );
#    system ( "cp mm5_print.out $RUNDIR/$this_cycle/${this_cycle}_mm5_p+fcst_print.out" );
#  }
#
#  if ($MODEL eq "WRF")
#  {
#    chdir ("$RUNDIR/$this_cycle/WRF_P");
#    system ( "ls -l >> wrf_print.out" );
#    system ( "echo \" \n\n --------------- wrf namelist------------ \n\n\"" );
#    system ( "cat namelist.input >> wrf_print.out" );
#    system ( "echo \" \n --------------- OBS   ------------------ \n\n\"" );
#    system ( "cat obs_* >> wrf_print.out" );
#    system ( "cp wrf_print.out $RUNDIR/$this_cycle/${this_cycle}_wrf_p+fcst_print.out" );
#  }
#}
#
##### Concat MMOUT files #######################################################
##### MOVED FROM RT_L_PBS_MM5_rtfdda.csh to here
#if ( $MODEL eq "MM5" )
#{
#  print FILEL "   Start Concat/Copying MMOUT F-analysis files at ", gmctime();
#
#  $HOURLY_OUT =  60/$OUT_INT;
#
#  # Find all MMOUT-files in $RUNDIR/$this_cycle/MM5_F
#  opendir (DIR_MM5_F, "$RUNDIR/$this_cycle/MM5_F");
#  @files = readdir (DIR_MM5_F);
#  @sorted_files = sort { $a cmp $b } @files;
#
#  chdir ("$RUNDIR/$this_cycle/MM5_F");
#
#  for( $d=1 ;$d <= $NUM_DOMS; $d++ )
#  {
#    $count = 0;
#    foreach $file (@sorted_files)
#    {
#      if ( index( $file, "MMOUT_DOMAIN$d") > -1 )
#      {
#        $count++;
#        if ( $count == $HOURLY_OUT ) #save HOURly Final analysis for all domains
#        {
#          if ($CONCAT_MMOUT)
#          {
#            print " Adding $file to $RUNDIR/$this_cycle/${this_cycle}_MMOUTPUT_DOMAIN$d.${MM5HOST}_F\n";
#            system ("cat $file >> $RUNDIR/$this_cycle/${this_cycle}_MMOUTPUT_DOMAIN${d}.${MM5HOST}_F");
#
#            # keep hourly D1 analysis only at GEAPSTMP directory
#            if( -d $GEAPSTMP && $d == 1 )
#            {
#              system ( "cat $file >> $GEAPSTMP/${this_cycle}_MMOUTPUT_DOMAIN${d}.${MM5HOST}_F");
#            }
#          }
#          else
#          {
#            # cp MMOUT_DOMAIN* to $RUNDIR/$this_cycle/MMOUT_DOMAIN*.${MM5HOST}_F
#            system("cp $file $RUNDIR/$this_cycle/$file.${MM5HOST}_F");
#          }
#          $count = 0;
#        }
#      }
#    }
#    # keep partial SAVE file in $RUNDIR/$this_cycle
#    system ( "cat SAVE_DOMAIN$d | head -1000 > $RUNDIR/${this_cycle}/${this_cycle}.SAVE_DOMAIN${d}_F" );
#  } #for 1 <= d <= NUM_DOMS
#
#  closedir (DIR_MM5_F);
#
#  print FILEL "   End Concat/Copying MMOUT F-Analysis files at ", gmctime();
#
#  if ( $FCST_LENGTH > 0 )
#  {
#    print FILEL "   Start Concat/Copying MMOUT P+FCST  files at ", gmctime();
#
#    # Find all MMOUT-files in $RUNDIR/$this_cycle/MM5_P
#    opendir (DIR_MM5_P, "$RUNDIR/$this_cycle/MM5_P");
#    @files = readdir (DIR_MM5_P);
#    @sorted_files = sort { $a cmp $b } @files;
#
#    chdir ("$RUNDIR/$this_cycle/MM5_P");
#
#    for( $d=1 ;$d <= $NUM_DOMS; $d++ )
#    {
#      $cnt = 0;
#      $cnt1 = 0;
#      $cnt3 = 0;
#      foreach $file (@sorted_files)
#      {
#        if ( index( $file, "MMOUT_DOMAIN$d") > -1 )
#        {
#          $cnt++;
#          $cnt1++;
#          $cnt3++;
#
#          if( ($cnt1 == $HOURLY_OUT && $cnt <= 24) || ($cnt3 == 3 && $cnt > 24))
#          {
#            if ( $CONCAT_MMOUT )
#            {
#              print " Adding $file to $RUNDIR/$this_cycle/${this_cycle}_MMOUTPUT_DOMAIN$d.${MM5HOST}_P+FCST\n";
#              system( "cat $file | head -210m >> $RUNDIR/$this_cycle/${this_cycle}_MMOUTPUT_DOMAIN$d.${MM5HOST}_P+FCST" );  #all domains on /data
#            }
#            else
#            {
#              # cp MMOUT_DOMAIN* to $RUNDIR/$this_cycle/MMOUT_DOMAIN*.${MM5HOST}_P+FCST
#              system("cp $file $RUNDIR/$this_cycle/$file.${MM5HOST}_P+FCST");
#            }
#            # keep hourly D1 FCST only at /d1/GEAPSTMP with short cut-off
#            #if( -d $GEAPSTMP && $d == 1 && $cnt < $CYC_INT + 2 )
#            #{
#            #  system( "cat $file >> $GEAPSTMP/${this_cycle}_MMOUTPUT_DOMAIN$d.${MM5HOST}_P+FCST" );
#            #}
#
#            $cnt1 = 0;
#            $cnt3 = 0;
#          }
#        }
#      }
#    }
#    closedir (DIR_MM5_P);
#    print FILEL "   End Concat/Copy MMOUT P+FCST  files at ", gmctime();
#  }
#} ###### END Concat for MMOUT files ############################################
#
##### Move wrfout and wrf-restart files around #################################
##### MOVED FROM RT_L_PBS_WRF_rtfdda.csh to here
#if ( $MODEL eq "WRF" )
#{
#  print FILEL "   Start Moving wrf output files at ", gmctime();
#
#  $HOURLY_OUT =  60/$OUT_INT;
#
#  opendir (DIR_WRF_F, "$RUNDIR/$this_cycle/WRF_F");
#  @files = readdir (DIR_WRF_F);
#
#  chdir ("$RUNDIR/$this_cycle/WRF_F");
#
#  #This file contains the *_end times from F-Analysis
#  require "$GSJOBDIR/tmp/$this_cycle/wrf_end_dates.pl";
#
#  for( $d=1 ;$d <= $NUM_DOMS; $d++ )
#  {
#    ##### This moved to pre_process_P+FCST.pl
#    #print "cp wrfrst_d0${d}_${y_end}-${m_end}-${d_end}_${h_end}:00:00 $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN$d\n";
#    #system ( "cp wrfrst_d0${d}_${y_end}-${m_end}-${d_end}_${h_end}:00:00 $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN$d" );
#    ##mv wrfrst_d0${i}*  $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN${1}_F$FCST_HH
#    ##ln -s $RUNDIR/restart_files/${this_cycle}.SAVE_DOMAIN${i}_F$FCST_HH wrfrst_d0${i}_${y_end}-${m_end}-${d_end}_${h_end}:00:00
#
#    $cnt = 0;
#    $cnt2 = 0;
#    foreach $file (@files)
#    {
#      if ( index( $file, "wrfout_d0${d}") > -1 )
#      {
#        #save Final analysis for all domains all times? ----
#        $cnt++;
#        if( $cnt == $HOURLY_OUT )
#        {
#          print "moving  $file $RUNDIR/$this_cycle/${file}.${MM5HOST}_F\n";
#          system ( "mv  $file $RUNDIR/$this_cycle/${file}.${MM5HOST}_F");   #save HOURly Final analysis for all domains  ----
#
#          # keep hourly D1 analysis only at GEAPSTMP directory
#          #
#          #if( -d $GEAPSTMP && $d == 1 )
#          #{
#          #  cp $file $up_dir/
#          #}
#
#          $cnt = 0;
#          $cnt2++;
#
#          if( $cnt2 == 3 && $ETKF eq "yes" && $NODEMEM eq "CTRL")
#          {
#            system ("cp $file $RUNDIR/../etkf_in/${this_cycle}_final$BCS$NODEMEM.WRFOUT_DOMAIN${d}" );
#          }
#        }
#      }
#    }
#  } #for 1 <= d <= NUM_DOMS
#
#  system ("rm wrf_real_input_em*");
#  closedir (DIR_WRF_F);
#
#  if ($FCST_LENGTH > 0)
#  {
#    opendir (DIR_WRF_P, "$RUNDIR/$this_cycle/WRF_P");
#    @files = readdir (DIR_WRF_P);
#
#    chdir ("$RUNDIR/$this_cycle/WRF_P");
#
#    for( $d=1 ;$d <= $NUM_DOMS; $d++ )
#    {
#      $cnt = 0;
#      $cnt1 = 0;
#      $cnt2 = 0;
#      # keep hourly fcsts only
#      foreach $file (@files)
#      {
#        if ( index( $file, "wrfout_d0${d}") > -1 )
#        {
#          $cnt++;
#          if( $cnt == $HOURLY_OUT )
#          {
#            print "moving $file $RUNDIR/$this_cycle/${file}.${MM5HOST}_P+FCST\n";
#            system ("mv $file $RUNDIR/$this_cycle/${file}.${MM5HOST}_P+FCST");  #all domains on /data
#            # keep hourly D1 FCST only at /d1/GEAPSTMP with short cut-off
#            #if( -d $GEAPSTMP && $d == 1 && $cnt1 < $CYC_INT + 2 ) then
#            #  cp $f $up_dir/
#            #endif
#            $cnt = 0;
#            $cnt1++;
#            $cnt2++;
#          }
#
#          #if( $ETKF == "yes" &&  ($ETKFMEM == "YES" || $NODEMEM == "CRTL")) then
#          if( $ETKF eq "yes" )
#          {
#            if( $cnt2 == 6 )
#            {
#              system ("cp $file $RUNDIR/../etkf_in/${this_cycle}_06hfcst$BCS$NODEMEM.WRFOUT_DOMAIN${d}");
#            }
#            #if( $cnt2 == 12) cp $f $RUNDIR/../etkf_in/${this_cycle}_12hfcst$ETAAVN$NODEMEM.WRFOUT_DOMAIN${d}
#          }
#        }
#      }
#    }#for 1 <= d <= NUM_DOMS
#    system ("rm wrf_real_input_em*");
#    closedir (DIR_WRF_P);
#  }
#  print FILEL "   End Moving wrf output files at ", gmctime();
#}

##### OLD post_process_clean.pl code starts here: ##############################
chdir $RUNDIR."/".$this_cycle;

if ($FCST_LENGTH == 0)
{
  $rest_time = $time_max;

  open (CRITIC, ">$RUNDIR/critic.time");

  if ( ($MPPJOB eq "yes" && (-e "$RUNDIR/restart_files/r-01-${rest_time_str}-0000"))
    || (-e "$RUNDIR/${this_cycle}/${this_cycle}.SAVE_DOMAIN1_F"))
  {
    print  CRITIC $rest_time;
  }
  elsif ($MODEL eq "WRF" && -e "$RUNDIR/restart_files/$this_cycle.SAVE_DOMAIN1")
  {
    print CRITIC $rest_time;
  }
  else
  {
    print  CRITIC 0;
    print  CRITIC "\n ${MODEL}_F -- Failure for restart at 0+${rest_time_str_old}";
    print  CRITIC "\n         Wait for the next cold-start time.";
    print FILEL "       !!!${MODEL}_F -- Failure -- Wait for next cold-start!!!  \n";
    close (CRITIC);
    exit(-1)
  }
  close (CRITIC);

  if ($MODEL eq "MM5" && $MPPJOB eq "yes")
  {
    # clean mpp restart directory
    system("rm $RUNDIR/${this_cycle}/MM5_F/restrts");
    system("$MustHaveDir $RUNDIR/${this_cycle}/MM5_F/restrts");
    #system("mv $RUNDIR/restart_files/r*${rest_time_str_old}* $RUNDIR/${this_cycle}/MM5_F/restrts");
    system("rm $RUNDIR/restart_files/zold/*");
    system("mv $RUNDIR/restart_files/r*${rest_time_str_old}* $RUNDIR/restart_files/zold");
  }
}

if ($FCST_LENGTH > 0)
{
  # clean (save) mpp restart files in cycle directory
  if($MODEL eq "MM5" && $MPPJOB eq "yes")
  {
    system("rm $RUNDIR/${this_cycle}/MM5_P/restrts");
    system("$MustHaveDir $RUNDIR/${this_cycle}/MM5_P/restrts");
  }

  if ($MM5FORVMET == 1)
  {
    &createMM5VMET("P+FCST");
  }
  
  if(defined $AnEn && $AnEn) {
    if (! defined $PYTHONPATH) {
      $PYTHONPATH = "$MM5HOME/cycle_code/PYTHON/modules" if ( defined ($MM5HOME));
    }
    if ( defined ($ENV{PYTHONPATH}) ) {
      $ENV{PYTHONPATH} = "$PYTHONPATH:$ENV{PYTHONPATH}";
    } else {
      $ENV{PYTHONPATH} = $PYTHONPATH;
    }
    if ( defined ($ENV{PYTHONPATH}) ) {
      my $range_lc = lc ${RANGE};
      my $lead_hours = 24;
      if (defined $VERI_PAIR_LEAD_HOURS) {
         $lead_hours = $VERI_PAIR_LEAD_HOURS;
      }
      system("pwd");
      my $my_cmd = "$MM5HOME/cycle_code/CSH_ARCHIVE/verif/run_veri2anen.sh ${this_cycle} $range_lc $PYTHON_ARCHIVE/flex/veri_pair lead_hours=$lead_hours";
      print ("     $my_cmd\n");
      my $systat = system("$my_cmd");
      print ("=== ERROR === Unable to export verification pair and process AnEn for cycle $this_cycle\n") if ($systat != 0);
    }
    else {
       print "  post_process_clean.pl ENV{PYTHONPATH} is not defined!!!\n";
    }
  }
}

# Special IAF processing

if ($AT_IAF) {
    $user = $ENV{LOGNAME};
    if ($IAF_SLEEP) {
       sleep $IAF_SLEEP;
       chdir "$GRIB_DEST_TMP/$user";
       system("mv *.gb $GRIB_DEST_ROOT/$user/.");
    }
}

# WRFVAR

if (-d "$RUNDIR/WRFVAR") {
   system("cp -r $RUNDIR/WRFVAR $RUNDIR/${this_cycle}/.");
}
#

if ($NAPS == 1)
{
  &toNaps3hCyc();
}

if($CLEANDIR >= 1 )
{
  &cleanDir();
}

print FILEL "   Ending clean up at ", gmctime();
print FILEL "\nending   FDDA system at ", gmctime();
print "\n\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
print "  ending RTFDDA system of cycle=$this_cycle at ",gmctime(),      "\n";
print "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++n\n";

close(FILEL);

############## The cycle stops here #############################################
exit(0);
############## The cycle stops here #############################################


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
  select(FILEL);
  $| =1;
  open(FILEL, ">>$fileName") || do { warn "Can't open file $fileName: $!\n" };
  select(STDERR);
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

#-------------------------------------------------------------------------------
# Name: toNaps3hCyc
# Arguments: none
# Return: none
# Description: Calls $RUNDIR/to_naps_rtfdda3hcyc.pl
#-------------------------------------------------------------------------------

sub toNaps3hCyc
{
  #create NAPS output at standard site (FDDA):
  print FILEL "   start creating NAPS output at standard site (FDDA)\n";
  system("$RUNDIR/to_naps_rtfdda3hcyc.pl $this_cycle 3 > $RUNDIR/zout_cron.rt3h_naps3 2>&1");
}

#-------------------------------------------------------------------------------
# Name: cleanDir
# Arguments: none
# Return: none
# Description: Removes the input data from RUNDIR
#-------------------------------------------------------------------------------

sub cleanDir
{
  print FILEL "\n -----Starting clean disk at ", gmctime();

# CLEANDIR = 0 -> nothing is scrubbed 

  if ($CLEANDIR == 0) {
      return 0;
  } 

# CLEANDIR = 1 -> Everything is scrubbed except: 
# log , namelists, wrfinput, wrfbdy, wrfout files, 
# WRF_WPS, WRF_REAL, WRF_F WRF_P, WRFQC_F, QRFQC_P, RAP_FDDA directories

  if ($CLEANDIR >= 1){ 

  system("echo ================= OBS ================= >> ${this_cycle}_clean");
  system("ls -l *GTS_data*  *RAWS_data* >> ${this_cycle}_clean");
  system("rm  *GTS_data* *RAWS_data*");

  system("ls -l ${DATA_DIR}/gts/*wmo >> ${this_cycle}_clean");
  system("rm  ${DATA_DIR}/gts/*wmo");

  system("ls -l DECODE_GTS/* >> ${this_cycle}_clean");
  system("rm -r DECODE_GTS/*");

  system("gzip RAP_RTFDDA/*all.obs*");

  system("ls -l ${DATA_DIR}/eta* >> ${this_cycle}_clean");
  system("rm  ${DATA_DIR}/eta*");

  system("ls -l ${DATA_DIR}/[0-9]*_*grib* >> ${this_cycle}_clean");
  system("rm  ${DATA_DIR}/[0-9]*_*grib*");

  system("ls -l ${DATA_DIR}/*gb >> ${this_cycle}_clean");
  system("rm  ${DATA_DIR}/*gb");

  system("ls -l ${DATA_DIR}/[0-9]*_*onedeg >> ${this_cycle}_clean");
  system("rm  ${DATA_DIR}/[0-9]*_*onedeg");

  system("rm -f input output");

  system("echo ============ INTERMEDIATE ============= >> ${this_cycle}_clean");

  if($MODEL eq "MM5")
  {
    #system("ls -l *1ST* *ANA* *REGRIDv3* >> ${this_cycle}_clean");
    #system("rm  *1ST* *ANA* *REGRIDv3*");

    system("ls -l *1ST* *ANA* >> ${this_cycle}_clean");
    system("rm  *1ST* *ANA* ");

    system("ls -l ETA_REGRID/REGRID_DOMAIN1 >> ${this_cycle}_clean");
    system("rm  ETA_REGRID/REGRID_DOMAIN1");
    if($normal != 1)
    {
      system("ls -l ETA_1GUESS/FILE* >> ${this_cycle}_clean");
      system("rm  ETA_1GUESS/FILE*");

      system("ls -l ETA_1GUESS/MOSAIC* >> ${this_cycle}_clean");
      system("rm  ETA_1GUESS/MOSAIC*");
    }
    else
    {
      system("ls -l ETA_REGRID/FILE* >> ${this_cycle}_clean");
      system("rm  ETA_REGRID/FILE*");

      system("ls -l AVN_REGRID/MOSAIC* >> ${this_cycle}_clean");
      system("rm  AVN_REGRID/MOSAIC*");

      #  system("ls -l INTERP_BACK/* >> ${this_cycle}_clean");
      #  system("rm  INTERP_BACK/*");

      system("echo ============ BY PRODUCTS ============== >> ${this_cycle}_clean");
      system("ls -l *MMOUTPUT_DOMAIN1 >> ${this_cycle}_clean");
      system("rm  *MMOUTPUT_DOMAIN1");
    }

    system("ls -l RAP_RTFDDA/[cdru]* >> ${this_cycle}_clean");
    system("rm  RAP_RTFDDA/[cdru]*");

    system("ls -l MM5_F/MMOUT* >> ${this_cycle}_clean");
    system("rm  MM5_F/MMOUT*");


    if ($POSTPROCESS == 1)
    {
      print FILEL "         \n******** SLEEP 2 minutes for plotting to be finished ********** \n\n";
      sleep  120;
    }

    if ($FCST_LENGTH > 0 )
    {
      system("ls -l MM5_P/MMOUT* >> ${this_cycle}_clean");
      system("rm  MM5_P/MMOUT*");
    }
  } # MM5
 } # CLEANDIR = 1

# CLEANDIR = 2 -> Everything is scrubbed except: 
# - log, namelists, wrfinput, wrfbdy, wrfout files 
# - WRF_F WRF_P, RAP_RTFDDA directories

  if ($CLEANDIR >= 2){
      # WRF dirs
      system("rm -rf WRFQC_F")        if (-e "WRFQC_F");
      system("rm -rf WRFQC_P")        if (-e "WRFQC_P");
      system("rm -rf WRF_REAL")       if (-e "WRF_REAL");
      system("rm -rf WRF_WPS")        if (-e "WRF_WPS");
      # Input data dirs
      system("rm -rf NNRP2_REGRID")   if (-e "NNRP2_REGRID");
      system("rm -rf GLDAS")          if (-e "GLDAS");
      system("rm -rf OISST")          if (-e "OISST");
      # Obs dirs
      system("rm -rf DECODE_GTS")     if (-e "DECODE_GTS");
      system("rm -rf RD_ADP")         if (-e "RD_ADP");
  } # CLEANDIR = 2

# CLEANDIR = 3 -> Everything is scrubbed except: 
# - logs, namelists, wrfinput, wrfbdy, wrfout files 
# - RAP_RTFDDA directory 

  if ($CLEANDIR >= 3){
      system("rm -rf WRF_F")      if (-e "WRF_F");
      system("rm -rf WRF_P")      if (-e "WRF_P");
  } # CLEANDIR = 3

# CLEANDIR = 4-9 -> Everything is scrubbed except: 
# - wrfout files 
# - RAP_RTFDDA directory 

  if ($CLEANDIR >= 4 && $CLEANDIR <= 9){
      system("rm -f *_wrfinput_*");
      system("rm -f *_wrfbdy_*");
      system("rm -f *_wrffdda*");
      system("rm -f *_qc_obs_for_assimilation_s");
      system("rm -f cshrc.WRF_F");
      system("rm -f wrf.nl");
  } # CLEANDIR = 4

# CLEANDIR = 10 -> Everything is scrubbed

  if ($CLEANDIR >= 10){
      system("rm -rf RAP_RTFDDA")      if (-e "RAP_RTFDDA");
      system("rm -f wrfout_*");
  } # CLEANDIR = 10

  print FILEL "   Ending clean up disk at ", gmctime();
  return 0;
}


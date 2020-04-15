#!/usr/bin/perl 

# This script executes from cron, and looks for
# a variety of status items for each cycle.  It outputs 
# a one-line message to a status file which is then parsed
# for display on a web page.
 
require 'stat.pl';
require "flush.pl";
require "ctime.pl";
use Time::Local;

#
# ENVIRONMENT
#

die "Usage: $0 GSJOBDIR rangename <check completeness flag, optional>\n" unless ($#ARGV >= 1 && $#ARGV <= 3);

my ($GSJOBDIR,  $rangename, $CHECK_COMPLETE, $check_cycle_time) = @ARGV;

require "$GSJOBDIR/flexinput.pl";
require "$GSJOBDIR/postprocinput.pl";

$CSH_ARCHIVE = $MM5HOME.'/cycle_code/CSH_ARCHIVE';
$EXECUTABLE_ARCHIVE = $MM5HOME.'/cycle_code/EXECUTABLE_ARCHIVE';
$CONSTANT_FILES = $MM5HOME.'/cycle_code/CONSTANT_FILES';
$MM5HOST = $RANGE;
$HOST = $ENV{'HOST'};
$NUM_NODES = $NUM_PROCS/$PPN;

$count_max = 30; # check up to this many times before it times out
$WRF_SLEEP = 60; # sleep seconds between checks

if ( ! -e $RUNDIR ) {
   print "Exiting -- no $RUNDIR\n";
   exit (1);
}

if ($DO_DISTRIB) {
   if ($DO_TAR_SUM_FOR_DISTRIB) { # only do if DO_TAR_SUM_FOR_DISTRIB is on
      if (! defined($DISTRIB_ROOT)) {  # best if defined in postprocinput.pl
         $DISTRIB_ROOT = "/model/$ENV{LOGNAME}/distrib";
      }
      $DISTRIB_DIR = "$DISTRIB_ROOT/$GSJOBID";
      $work_dir = "$DISTRIB_DIR/status";
      system( "mkdir -p $work_dir" );
   }
}

if ($CHECK_COMPLETE > 20000000) {
  $check_cycle_time = $CHECK_COMPLETE;
  $CHECK_COMPLETE = 0;
}

chdir ( $RUNDIR );

$this_cycle = $check_cycle_time;
$this_cycle = get_cycle_time($CYC_INT) if not $this_cycle;
print "Cycle = $this_cycle\n";
if ( ! -e "$this_cycle" ) {
  my $cycle;
  my $last_cycle;

  foreach $cycle (`ls -d ?????????? | sort -r`) {
    chomp($cycle);
    if ($cycle =~ /\d{10}/) {
       $last_cycle = $cycle;
       last;
    }
  }

  if ( ! -e "$RUNDIR\/$last_cycle" ) {
    print "The cycle $this_cycle does not exist\n";
    exit 1;
  } else {
    print "The cycle $this_cycle does not exist. Checking the last cycle, $last_cycle\n";
    $this_cycle = $last_cycle;
  }
}

$report_filename = "$RUNDIR/$rangename.report";
$sams_report_filename = "$RUNDIR/$rangename.sams.report";

if ($CHECK_COMPLETE) {
  &check_complete();
} else {

  my $is_cold_start = 0;
  my $is_previous_eta = 0;
  my $is_missing = 0;
  my $is_completed = is_completed($this_cycle);
  
  if ( -e "$report_filename" ) {
    my $search_str = "cycle=$this_cycle";
    my $tmp_name = "$report_filename.tmp.$$";
    system("grep -v $search_str $report_filename > $tmp_name");
    if ( -e "$tmp_name" ) {
      system ("mv $tmp_name $report_filename");
    }
  }
  if ( -e "$sams_report_filename" ) {
    my $search_str = "cycle=$this_cycle";
    my $tmp_name = "$sams_report_filename.tmp.$$";
    system("grep -v $search_str $sams_report_filename > $tmp_name");
    if ( -e "$tmp_name" ) {
      system ("mv $tmp_name $sams_report_filename");
    }
  }

  open(REPORT_FILE, ">>$report_filename");
  print REPORT_FILE "cycle=",$this_cycle;
  
  open(SAMS_REPORT_FILE, ">>$sams_report_filename");
  print SAMS_REPORT_FILE "cycle=",$this_cycle;
  
#	Cycle_status

  $run_status = "blue";
  $is_cold_start = "no";
  if ( ! -e "$RUNDIR\/$this_cycle/timing" ) {
    $is_missing = 1;
    print "Missing timing file $RUNDIR $this_cycle\n";
    $run_status = "red";
  } else { 
    open (TIMING,"$RUNDIR\/$this_cycle/timing");
    print "Reading timing file\n";
    while ( <TIMING>) {
      if ( m!cold!) {
        $is_cold_start = "yes";
      }
      if ( m!1ST Guess! ) {
        $is_previous_eta = 1;
        $run_status = "yellow";
      }
    } # end while <TIMING>

    close (TIMING);

    if ( ! -e "$RUNDIR\/$this_cycle/timing_pre_process_P+FCST" ) {
       print "Missing timing_pre_process_P+FCST file $RUNDIR $this_cycle\n";
       $run_status = "orange";
    } else { 
       open (PFCST,"$RUNDIR\/$this_cycle/timing_pre_process_P+FCST");
       print "Reading timing_pre_process_P+FCST file\n";
       while ( <PFCST>) {
          if ( m!Failure! ) {
            $run_status = "orange";
          }
       } # end of while <PFCST>
       close (PFCST);
     } # end if -e timing_pre_process file
  } # end if -e timing file

  if ($run_status == "blue" && 1 == $is_completed) {
    $run_status = "green";
  }
  print REPORT_FILE "&status=", $run_status;
  print REPORT_FILE "&cold_start=", $is_cold_start;

  print "Finished timing file\n";

#  Now check obs counts
  my $sams_sum;
  my $sams_sum_final = 0;
  my $sams_sum_forecast = 0;
  $cycle_dir = "${RUNDIR}/${this_cycle}";
  if ( -e $cycle_dir ) { 
    opendir(IDIR,$cycle_dir);
    @files = grep(/qc/, readdir(IDIR));
    closedir(IDIR);

    chdir ( $cycle_dir );
#    SAMS first
#    print "  SAMS observation counts \n";
    my $filename_f = "WRFQC_F/all.obs_wrfqc_output";
    my $filename_p = "WRFQC_P/all.obs_wrfqc_output";
    
    $sams_sum_final    = `grep -i $rangename $filename_f | grep -c "SAMS "` if (-e $filename_f);
    $sams_sum_forecast = `grep -i $rangename $filename_p | grep -c "SAMS "` if (-e $filename_p);
    if ($rangename eq "rtc" || $rangename eq "RTC") {
      $sams_sum_final    = `grep -i "rttc" $filename_f | grep -c "SAMS "` if (0 == $sams_sum_final);
      $sams_sum_forecast = `grep -i "rttc" $filename_p | grep -c "SAMS "` if (0 == $sams_sum_forecast);
    }
    chomp($sams_sum_final);
    chomp($sams_sum_forecast);
    
    $sams_sum = 0;
    foreach $filename (@files) {
#        $sams_cnt = `grep -c "SAMS " $filename`;
      $sams_cnt = `grep -i $rangename $filename | grep -c "SAMS "`;
      if ($rangename eq "rtc" || $rangename eq "RTC") {
        $sams_cnt = `grep -i rttc $filename | grep -c "SAMS "` if ($sams_cnt==0);
      }
      if ($sams_cnt == 20) {
        print "qc_files:\n";
        system ("ls -l $filename");
      }
      chomp($sams_cnt);
	  $sams_sum += $sams_cnt;
	  $timetag = substr($filename,0,10);
#	   print "     Hour Processed: $timetag  $sams_cnt\n";
      if ($sams_cnt == 20) {
        print " === ERROR === Hour Processed: $timetag  $sams_cnt\n";
        system ("ls -l $filename");
      }
    }
#    $sams_sum /=2;
    print "     Total Range SAMS: $sams_sum   final: $sams_sum_final, forecast: $sams_sum_forecast\n";
    if ($sams_sum < 20) {
      print " === ERROR === qc_files: [@files]\n";
    }
    #$sams_sum = $sams_sum_final if ($sams_sum < $sams_sum_final);
    #$sams_sum = $sams_sum_forecast if ($sams_sum < $sams_sum_forecast);
    print REPORT_FILE "&range_sams=", $sams_sum;
    print SAMS_REPORT_FILE "&range_sams=", $sams_sum;

#    next IAF special obs
#    print "  IAF  observation counts \n";
    $iafs_sum = 0;
    foreach $filename (@files) {
      $iafs_cnt = `grep -c " from IAF" $filename`;
      chomp($iafs_cnt);
	  $iafs_sum += $iafs_cnt;
	  $timetag = substr($filename,0,10);
#	   print "     Hour Processed: $timetag  $iafs_cnt\n";
    }
	print "     Total IAF obs: $iafs_sum\n";
    print REPORT_FILE "&local_obs=", $iafs_sum;

#    print "  Range Soundings observation counts \n";
    $sdgs_sum = 0;
    foreach $filename (@files) {
#      $sdgs_cnt = `grep -c sdg $filename`;
      $sdgs_cnt = `grep -i $rangename $filename | grep -c sdg`;
      chomp($sdgs_cnt);
	  $sdgs_sum += $sdgs_cnt;
      $timetag = substr($filename,0,10);
#	   print "     Hour Processed: $timetag  $sdgs_cnt\n";
    }
    print "     Total Range SDGS: $sdgs_sum\n";
    print REPORT_FILE "&range_sdgs=", $sdgs_sum;
    $wmo_sum = 0;
    foreach $filename (@files) {
      $wmo_cnt = `grep -c FM- $filename`;
      chomp($wmo_cnt);
	  $wmo_sum += $wmo_cnt;
	  $timetag = substr($filename,0,10);
#	   print "     Hour Processed: $timetag  $wmo_cnt\n";
    }
#    $wmo_sum -= $sams_sum;
#    $wmo_sum -= $sdgs_sum;

    print "     Total Other obs : $wmo_sum\n";
    print REPORT_FILE "&other_obs=", $wmo_sum;

  }
  
  print REPORT_FILE "&host=", $HOST;
  print REPORT_FILE "&nodes=", $NUM_NODES;
  print REPORT_FILE "&upnodes=", $NUM_NODES;
  print REPORT_FILE "\n";
  close REPORT_FILE;
  
  print SAMS_REPORT_FILE "&sams_f=", $sams_sum_final;
  print SAMS_REPORT_FILE "&sams_p=", $sams_sum_forecast;
  print SAMS_REPORT_FILE "&host=", $HOST;
  print SAMS_REPORT_FILE "\n";
  close SAMS_REPORT_FILE;
}    


if ($DEST_SERVER =~ m/localhost/) {
   system("mkdir -p $JOB_LOC/status") if (! -d "$JOB_LOC/status");
   system("cp -p $report_filename $JOB_LOC/status");
   system("cp -p $sams_report_filename $JOB_LOC/status");
} else {
   system ("ssh $DEST_SERVER mkdir -p $JOB_LOC/status");
   system("rsync -e 'ssh -i $KEY' -avzC $report_filename $DEST_SERVER:$JOB_LOC/status");
   system("rsync -e 'ssh -i $KEY' -avzC $sams_report_filename $DEST_SERVER:$JOB_LOC/status");
}
if ($DO_DISTRIB) {
   if ($DO_TAR_SUM_FOR_DISTRIB) { # only do if DO_TAR_SUM_FOR_DISTRIB is on
      chdir ( $RUNDIR );
      system( "sha256sum $rangename.report > $rangename.report.sum");
      system( "cp -p $rangename.report* $work_dir");
   }
}

exit (0);

sub hh_advan_date {

  %mon_days = (1,31,2,28,3,31,4,30,5,31,6,30,7,31,8,31,9,30,10,31,11,30,12,31);
  (my $s_date, my $advan_hh) = @_ ;

  my $yy = substr($s_date,0,4);
  my $mm = substr($s_date,4,2);
  my $dd = substr($s_date,6,2);
  my $hh = substr($s_date,8,2);

  my $feb = 2;
  $mon_days{$feb} = 29 if ($yy%4 == 0 && ($yy%400 == 0 || $yy%100 != 0));

  $hh = $hh + $advan_hh;
  while($hh > 23) {
    $hh -= 24;
    $dd++;
  }
  while($dd > $mon_days{$mm+0}) {
    $dd = $dd - $mon_days{$mm+0};
    $mm++;
    while($mm > 12) {
      $mm -= 12;
      $yy++;
    }
  }

  while($hh < 0) {
    $hh += 24;
    $dd--;
  }
  if($dd < 1) {
    $mm--;
    $dd += $mon_days{$mm+0};
    while($mm < 1) {
      $mm += 12;
      $dd += $mon_days{$mm+0};
      $yy--;
    }
  }

  my $new_date = sprintf("%04d%02d%02d%02d",$yy,$mm,$dd,$hh);
}

sub is_completed {
  (my $this_cycle, $repeat) = @_ ;

  #my $cycle;
  #my $this_cycle;
  my $proceed;
  my $cold_start;
  my $fcst_length;
  my ($yy,$mm,$dd,$hh);
  my $cycle_time;
  my @fcst_end;
  my ($yy_end,$mm_end,$dd_end,$hh_end,$mn_end,$ss_end);
  my $wrf_end;
  my $count;
  my $completed = 0;

  open(REPORT_FILE, "$rangename.report");

  $cold_start = 0;
  $proceed = 0;
  while (<REPORT_FILE>) {
    if (/$this_cycle/) {
      if (/blue/ || /orange/ || /green/ || /yellow/) {
        $cold_start = 1 if (/cold_start=yes/);
        $proceed = 1;
        last;
      }
    }
  }
  close(REPORT_FILE);

  if ($proceed) {
    if ($cold_start) {
       if (defined @COLD_LENGTH) {
          $fcst_length = $COLD_LENGTH[0]+$FIN_END;
       } else {
          $fcst_length = $COLD_START_FCST+$FIN_END;
       }
    } else {
       if (defined @DOM_LENGTH) {
          $fcst_length = $DOM_LENGTH[0]+$FIN_END;  # use domain 1 forecast length
       } else {
          $fcst_length = $FCST_LENGTH+$FIN_END;
       }
    }
    
    $hh = substr($this_cycle,8,2);
    $dd = substr($this_cycle,6,2);
    $mm = substr($this_cycle,4,2) -1;
    $yy = substr($this_cycle,0,4);
    
    $cycle_time = timegm(0,0,$hh,$dd,$mm,$yy);  # EPOCH seconds for cycle hour
    @fcst_end = gmtime($cycle_time+$fcst_length*3600);
    
    $yy_end = $fcst_end[5] + 1900;
    $mm_end = sprintf("%02d",$fcst_end[4]+1);
    $dd_end = sprintf("%02d",$fcst_end[3]);
    $hh_end = sprintf("%02d",$fcst_end[2]);
    $mn_end = sprintf("%02d",$fcst_end[1]);
    $ss_end = sprintf("%02d",$fcst_end[0]);
    
    $wrf_end = "wrfout_d01_${yy_end}-${mm_end}-${dd_end}_${hh_end}:${mn_end}:${ss_end}.${RANGE}_P+FCST";
    
    print "Final output file to check for is: $this_cycle/$wrf_end\n";
    
    $count = 0;
    my $tmp_count_max = 1;
    $tmp_count_max = $count_max if ($repeat);
    while ($count < $tmp_count_max) {
      if ( -e "$this_cycle/$wrf_end") { # cycle completed fine, turn status green
         $completed = 1;
         last;
      }
      
      last if ($tmp_count_max <= 1);
      sleep $WRF_SLEEP;
      $count++;
    }
    
    if ($count == $count_max) {
       $now = `date`;
       print "Forecast completeness check timed out:  $now\n";
    }
  } else {
    print "Cycle $this_cycle failed; no need to check completeness!\n";
    $completed = -1;
  }
  return $completed;
}

sub check_complete {
  my $cycle;
  my $this_cycle;
  my $is_completed, $repeat;
  
  chdir ( $RUNDIR );
  
  foreach $cycle (`ls -d ?????????? | sort -r`) {
    chomp($cycle);
    if ($cycle =~ /\d{10}/) {
       $this_cycle = $cycle;
       last;
    }
  }

  if ( ! -e "$rangename.report") {
     print "Status file ${rangename}.report does not exist!\n";
     return;
  }

  $repeat = 1;
  $is_completed = is_completed($this_cycle, $repeat);
  if ($is_completed == 1) {
    open(REPORT_FILE, "$rangename.report");
    open(OUT,">tmp.report");
    while (<REPORT_FILE>) {
      if (/$this_cycle/) {
        $now = `date`;
        if (/blue/) {
          print "Change blue status to green status:  $now \n";
          s/blue/green/;
        }
        if (/orange/) {
          print "Change orange status to green status:  $now \n";
          s/orange/green/;
        }
      }
      print OUT;
    }
    close(REPORT_FILE);
    close(OUT);
       
    system("mv tmp.report $rangename.report");

#       system ("ssh $DEST_SERVER mkdir -p $JOB_LOC/status");
#       system("rsync -e 'ssh -i $KEY' -avzC $rangename.report $DEST_SERVER:$JOB_LOC/status");

#if ($DO_DISTRIB) {
#   if ($DO_TAR_SUM_FOR_DISTRIB) { # only do if DO_TAR_SUM_FOR_DISTRIB is on
#      system( "sha256sum $rangename.report > $rangename.report.sum");
#      system( "cp $rangename.report* $work_dir");
#   }
#}
 
  }

}


sub get_cycle_time {
  (my $cyc_int) = @_ ;

  #	Build the UTC date as yy mm dd hh
  my $ttime = time  - 0*3600;
  
  #   GMT time information
  ($sec,$mm,$hh,$dd,$mm,$yy,@_) = gmtime($ttime);
  if ($yy<50){
    $yy+=2000;
  } else {
    $yy+=1900;
  }
  
  #  print "Hour = $hh\n";
  # For 3-hourly cycling:
  if ( $cyc_int == 3 ) {
    if ( $hh >= 3) { $hh = (int($hh/3) - 1) * 3 + 2;}  # report 02Z cycle at 03, 04 and 05Z.
    else {
    # switch to previous day 
      $hh = 2;
      $next_cycle =  sprintf("%04d%02d%02d%02d",$yy,$mm+1,$dd,$hh);
      if ($hh < 0) {  # checd 20Z cycle at 23Z
        $this_cycle = `${MM5HOME}/cycle_code/CSH_ARCHIVE/util/advance_cymdh.pl $next_cycle -6`;
      } else {
        $this_cycle = `${MM5HOME}/cycle_code/CSH_ARCHIVE/util/advance_cymdh.pl $next_cycle -3`;
      }
      $hh = substr($this_cycle,8,2);
      $dd = substr($this_cycle,6,2);
      $mm = substr($this_cycle,4,2) -1;
      $yy = substr($this_cycle,0,4);
    }
  }
  # for 6-hourly cycling:
  if ( $cyc_int == 6 ) {
    if ( $hh < 1 ) { 
      $hh = 0;
      $next_cycle =  sprintf("%04d%02d%02d%02d",$yy,$mm+1,$dd,$hh);
      $this_cycle = `${MM5HOME}/cycle_code/CSH_ARCHIVE/util/advance_cymdh.pl $next_cycle -6`;
      $hh = substr($this_cycle,8,2);
      $dd = substr($this_cycle,6,2);
      $mm = substr($this_cycle,4,2) -1;
      $yy = substr($this_cycle,0,4);
    }
    else { $hh = int(($hh - 1) / 6) * 6;} # report 06Z cycle between 7 and 12Z
  }
  
  my $this_cycle =  sprintf("%04d%02d%02d%02d",$yy,$mm+1,$dd,$hh);
  return $this_cycle;
}

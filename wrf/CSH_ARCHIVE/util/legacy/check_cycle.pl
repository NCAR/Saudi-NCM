#!/usr/bin/perl

# This script executes from cron every 3 hours, and looks for
# log messages indicating a cold-start cycle, or a cycle which used
# previous ETA files.  If either of these occurred, an email msg is
# sent.
#
# example usage:
# check_cycle.pl GMDPG  DPG  "GMOD DPG"
#
 
use Time::Local;

if ( $#ARGV >= 2 ) {
  $GSJOBDIR = @ARGV[0];
  $NODE  =  @ARGV[1];
  $TITLE =  @ARGV[2];
  if ( $#ARGV == 3 ) {
	  $OFFSET = @ARGV[3];
  } else {
	  $OFFSET = 0;
  }
  print ( " $0  $GSJOBDIR  $NODE  $TITLE\n");
} else {
  print ( " Usage:   $0  GSJOBDIR  NODE  TITLE\n");
  exit (1);
}


#
# ENVIRONMENT
#
require "$GSJOBDIR/flexinput.pl";
require "$GSJOBDIR/postprocinput.pl";


my $MAIL_TO=$EMAIL_RECIPIENT;       # Email list for sending notifications.
my $hostname = $ENV{HOST};  
my $user = $ENV{LOGNAME};
my $sender = $user.'@'.$hostname;
system("mkdir -p $GSJOBDIR/cycle_log");

$CSH_ARCHIVE = $MM5HOME.'/cycle_code/CSH_ARCHIVE';
$EXECUTABLE_ARCHIVE = $MM5HOME.'/cycle_code/EXECUTABLE_ARCHIVE';
$CONSTANT_FILES = $MM5HOME.'/cycle_code/CONSTANT_FILES';

if ( ! -e $RUNDIR ) {
   exit (1);
}

#	Build the UTC date as yy mm dd hh
$ttime = time  - $OFFSET*3600;

#   GMT time information

($sec,$mm,$hh,$dd,$mm,$yy,@_) = gmtime($ttime);
if ($yy<50){
  $yy+=2000;
} else {
  $yy+=1900;
}

$cycle_time = timegm(0,0,$hh,$dd,$mm,$yy);  # EPOCH seconds for cycle hour

$this_cycle =  sprintf("%04d%02d%02d%02d",$yy,$mm+1,$dd,$hh);
$is_cold_start = 0;
$is_previous_eta = 0;
$is_missing = 0;

#	Cycle_status

   if ( ! -e "$RUNDIR\/$this_cycle/timing" ) {
     $is_missing = 1;
# This does not take into account the COLD_012 flag! but assumes
# the next cycle can cold-start!
     $next_cold = &hh_advan_date($this_cycle, $CYC_INT);
   } else { 
   open (TIMING,"$RUNDIR\/$this_cycle/timing");
   while ( <TIMING>) {

      if ( m!cold!) {
        $is_cold_start = 1;
      }
      if ( m!earlier ETA! ) {
        $is_previous_eta = 1;
      }
   }
   }

# This may be different on some systems...
my $mailprog = 'sendmail';

if ( $is_cold_start == 1 ) {

    open(MESSAGE, "|$mailprog -t") ||
        die "Couldn't open mail program: $!";

    select (MESSAGE);  #print commands now go to MESSAGE

    printf( "Subject:  %s RT-FDDA %s Cold Start\n",
           $TITLE, $this_cycle);

    print "From:  $sender\n";
    print "To:  $MAIL_TO";

    printf( "\n\n  Cold Start at %d\n",$this_cycle);
    printf( "\n\n  The nature of the data assimilation engine of the\n");
    printf( "  RTFDDA system requires some time, at least 12 hours, for\n");
    printf( "  its analyses and forecasts to reach a good balance and \n");
    printf( "  accuracy after each cold-start\n");
    printf( "\n  Please use the RT-FDDA products during this \n");
    printf( "  spin-up period with extra caution.\n");


##    close (MESSAGE)|| die "Couldn't send mail message: $!";

    open(LOGFILE,">>$GSJOBDIR/cycle_log/${TITLE}_${this_cycle}_cold") || do { warn "Can't open log file"};
    printf(LOGFILE "\n\n  Cold Start at %d\n",$this_cycle);
    close(LOGFILE);
}

if ( $is_previous_eta == 1 ) {

    open(MESSAGE, "|$mailprog -t") ||
        die "Couldn't open mail program: $!";

    select (MESSAGE);  #print commands now go to MESSAGE

    printf( "Subject:  %s RT-FDDA %s Previous ETA used\n",
           $TITLE, $this_cycle);

    print "From:  $sender\n";
    print "To:  $MAIL_TO";

    printf( "\n\n  Previous ETA (>12hrs old) was used at %d\n",$this_cycle);
    printf( "\n\n  When the current ETA data are late or missing, for any reason,\n");
    printf( "  the RTFDDA system will make use of the earlier ETA data during the\n");
    printf( "  4 cycles in the following 12 hours. The use of the 12-hour longer\n");
    printf( "  ETA forecast may affect the accuracy of the RTFDDA product.\n");

##    close (MESSAGE)|| die "Couldn't send mail message: $!";

    open(LOGFILE,">>$GSJOBDIR/cycle_log/${TITLE}_${this_cycle}_eta") || do { warn "Can't open log file"};
    printf(LOGFILE "\n\n  Previous ETA (>12hrs old) was used at %d\n",$this_cycle);
    close(LOGFILE);
}

if ( $is_missing == 1 ) {

    open(MESSAGE, "|$mailprog -t") ||
        die "Couldn't open mail program: $!";

    select (MESSAGE);  #print commands now go to MESSAGE

    printf( "Subject:  %s RT-FDDA %s Missing cycle\n",
           $TITLE, $this_cycle);

    print "From:  $sender\n";
    print "To:  $MAIL_TO";

    printf( "\n\n  This cycle is missing --  %d\n",$this_cycle);
    printf( "\n\n  The RTFDDA cycles have crashed --\n");
    printf( "  It is expected to come back with a cold start at \n");
    printf( "    %s \n",$next_cold);

##    close (MESSAGE)|| die "Couldn't send mail message: $!";

    open(LOGFILE,">>$GSJOBDIR/cycle_log/${TITLE}_${this_cycle}_missing") || do { warn "Can't open log file"};
    printf(LOGFILE "\n\n  Cycle missing -- %d\n",$this_cycle);
    close(LOGFILE);
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


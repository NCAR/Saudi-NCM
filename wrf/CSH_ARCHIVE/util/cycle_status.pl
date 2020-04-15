#!/usr/bin/perl 

# This script executes from cron every 3 hours, and looks for
# log messages indicating a cold-start cycle, or a cycle which used
# previous ETA files.  If either of these occurred, an email msg is
# sent.
use Time::Local;

#
# ENVIRONMENT
#



die "Usage: $0 ROOT_DIR JOB_ID RANGE\n" unless (@ARGV == 3);


my ($ROOT_DIR, $JOB_ID, $RANGE) = @ARGV;

$GSJOBDIR = "$ROOT_DIR/$JOB_ID";
$GSJOBID  = "$JOB_ID";
$ENV{'GSJOBID'} = "$GSJOBID";
$ENV{'GSJOBDIR'} = "$GSJOBDIR";

require "$GSJOBDIR/flexinput.pl";
require "$GSJOBDIR/postprocinput.pl";

chdir ( $RUNDIR );
$CSH_ARCHIVE = $MM5HOME.'/cycle_code/CSH_ARCHIVE';
$EXECUTABLE_ARCHIVE = $MM5HOME.'/cycle_code/EXECUTABLE_ARCHIVE';
$CONSTANT_FILES = $MM5HOME.'/cycle_code/CONSTANT_FILES';

if ( ! -e $RUNDIR ) {
   print "Exiting -- no $RUNDIR\n";
   exit (1);
}

#	Build the UTC date as yy mm dd hh
$ttime = time  - 0*3600;

#   GMT time information

($sec,$mm,$hh,$dd,$mm,$yy,@_) = gmtime($ttime);
if ($yy<50){
  $yy+=2000;
} else {
  $yy+=1900;
}

$cycle_time = timegm(0,0,$hh,$dd,$mm,$yy);  # EPOCH seconds for cycle hour

   print "Hour = $hh\n";
# For 3-hourly cycling:
if ( $CYC_INT == 3 ) {
  if ( $hh == 3 || $hh == 4 ) { $hh = 2;}
  if ( $hh == 6 || $hh == 7 )  {$hh = 5;}
  if ( $hh == 9 || $hh == 10 )  {$hh = 8;}
  if ( $hh == 12 || $hh == 13 )  {$hh = 11;}
  if ( $hh == 15 || $hh == 16 ) { $hh = 14;}
  if ( $hh == 18 || $hh == 19 )  {$hh = 17;}
  if ( $hh == 21 || $hh == 22 )  {$hh = 20;}
  if ( $hh == 0 || $hh == 1 ) {
     $hh = 23;
     $dd -= $dd;
  }
}  
# For 6-hourly cycling:
if ( $CYC_INT == 6 ) {
  if ( $hh == 1 || $hh == 2|| $hh == 3 || $hh == 4 || $hh == 5 ) { $hh = 0;}
  if ( $hh == 7 || $hh == 8|| $hh == 9 || $hh == 10 || $hh == 11 ) { $hh = 6;}
  if ( $hh == 13 || $hh == 14|| $hh == 15 || $hh == 16 || $hh == 17 ) { $hh = 12;}
  if ( $hh == 19 || $hh == 20|| $hh == 21 || $hh == 22 || $hh == 23 ) { $hh = 18;}
}
$this_cycle =  sprintf("%04d%02d%02d%02d",$yy,$mm+1,$dd,$hh);
   print "Cycle = $this_cycle\n";
$is_cold_start = 0;
$is_previous_eta = 0;
$is_missing = 0;

#	Cycle_status

   if ( ! -e "$RUNDIR\/$this_cycle/timing" ) {
     $is_missing = 1;
     if ($hh > 3 && $hh < 17 ) {
        $next_cold = sprintf("%04d%02d%02d17Z",$yy,$mm+1,$dd);
     } else {
        if ( $hh < 3 ) {
        $next_cold = sprintf("%04d%02d%02d05Z",$yy,$mm+1,$dd);
        } else {
           $base = sprintf("%04d%02d%02d05",$yy,$mm+1,$dd);
           $next_cold = &hh_advan_date($base, 24);
        }
     }
   print "Missing timing file $RUNDIR $this_cycle\n";
   } else { 
   open (TIMING,"$RUNDIR\/$this_cycle/timing");
   print "Reading timing file\n";
   while ( <TIMING>) {

      if ( m!cold!) {
        $is_cold_start = 1;
      }
      if ( m!starting RTFDDA system!) {
        @lineparts = split(' ', $_);
        $start_cycle = substr($_,37,8);
        $Starting = 1;
        $startdw = @lineparts[4];
        $startm = @lineparts[5];
        $startd = @lineparts[6];
        $starthms = @lineparts[7];
        $starty = @lineparts[8];
      }
      if ( m!start the processing of OBS for C!) {
        @lineparts = split(' ', $_);
        $start_Cobs = substr($_,57,8);
        $Cobs = 1;
        $Cobsdw = @lineparts[4];
        $Cobsm = @lineparts[5];
        $Cobsd = @lineparts[6];
        $Cobshms = @lineparts[7];
        $Cobsy = @lineparts[8];
      }
      if ( m!start the processing of OBS for F!) {
        @lineparts = split(' ', $_);
        $start_Fobs = substr($_,57,8);
        $Fobs = 1;
        $Fobsdw = @lineparts[4];
        $Fobsm = @lineparts[5];
        $Fobsd = @lineparts[6];
        $Fobshms = @lineparts[7];
        $Fobsy = @lineparts[8];
      }
      if ( m!model wrf!) {
        @lineparts = split(' ', $_);
        $start_Fmm5 = substr($_,57,8);
        $Fmm5 = 1;
        $Fmm5dw = @lineparts[6];
        $Fmm5m = @lineparts[7];
        $Fmm5d = @lineparts[8];
        $Fmm5hms = @lineparts[9];
        $Fmm5y = @lineparts[10];
      }
      if ( m!start the processing of OBS for P!) {
        @lineparts = split(' ', $_);
        $start_Pobs = substr($_,57,8);
        $Pobs = 1;
        $Pobsdw = @lineparts[4];
        $Pobsm = @lineparts[5];
        $Pobsd = @lineparts[6];
        $Pobshms = @lineparts[7];
        $Pobsy = @lineparts[8];
      }
      if ( m!starting P-stage mm5!) {
        @lineparts = split(' ', $_);
        $start_Pmm5 = substr($_,42,8);
        $Pmm5 = 1;
        $Pmm5dw = @lineparts[4];
        $Pmm5m = @lineparts[5];
        $Pmm5d = @lineparts[6];
        $Pmm5hms = @lineparts[7];
        $Pmm5y = @lineparts[8];
      }
      if ( m!ending   P-stage mm5!) {
        @lineparts = split(' ', $_);
        $start_Pend = substr($_,42,8);
        $Pend = 1;
        $Penddw = @lineparts[4];
        $Pendm = @lineparts[5];
        $Pendd = @lineparts[6];
        $Pendhms = @lineparts[7];
        $Pendy = @lineparts[8];
      }
      if ( m!earlier ETA! ) {
        $is_previous_eta = 1;
      }
   }
   }

   print "Finished timing file\n";
system("rm index.*");

open(IN,"$GSJOBDIR/postprocs/status.template");
open(OUT,">status.html");

if ($is_missing) {
   $start_time_str = 'MISSING';
} else {
   $start_time_str = $start_cycle;
}

if ($Cobs == 1) {
   $status1_str = $start_Cobs;
} else {
   $status1_str = '';
}

if ($Fobs == 1) {
   $status2_str = $start_Fobs;
} else {
   $status2_str = '';
}

if ($Fmm5 == 1) {
   $status3_str = $start_Fmm5;
} else {
   $status3_str = '';
}

if ($Pobs == 1) {
   $status4_str = $start_Pobs;
} else {
   $status4_str = '';
}

if ($Pmm5 == 1) {
   $status5_str = $start_Pmm5;
} else {
   $status5_str = '';
}

if ($Pend == 1) {
   $status6_str = $start_Pend;
} else {
   $status6_str = '';
}

if ($is_cold_start == 1) {
   $status7_str = 'ColdStart';
} else {
   $status7_str = '';
}

while (<IN>) {
  s/CYCLE/$this_cycle/g;
  s/STARTTIME/$start_time_str/g;
  s/STATUS1/$status1_str/g;
  s/STATUS2/$status2_str/g;
  s/STATUS3/$status3_str/g;
  s/STATUS4/$status4_str/g;
  s/STATUS5/$status5_str/g;
  s/STATUS6/$status6_str/g;
  s/COLDSTART/$status7_str/g;
  print OUT $_;
}
close(IN);
close(OUT);

print "Finished writing status.html\n";
system("rsync  -e 'ssh -i $KEY' -avzC status.html  $DEST_SERVER:$JOB_LOC/.");

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


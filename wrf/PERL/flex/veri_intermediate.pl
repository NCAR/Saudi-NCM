#!/usr/bin/perl
use strict;
no strict 'refs';
use vars qw($opt_j $opt_t $opt_m);
use Getopt::Std;

our $JOBHOME;
our $FLEXINPUT;
our $GSJOBDIR;
our $GSJOBID;
our $DATADIR;
our $RUNDIR;
our $DATA_ICBC_DIR;
our $VERIFY_DIR;
our $SFC_DIR;
our $UPR_DIR;
our $EXECUTABLE_ARCHIVE;
our $VERI_PAIRS_DIR;
our $NUM_DOMS;
our @UPR_HOURS;
our $VERI_LENGTH;
our $VERI_ARCHIVE_ROOT;

my $timeString;
my ($veriDate,$veriHour,$veriTime);
my $refModel;
my $BCIC;
my $refFile;
my $refFile2;
my $dwdFile;
my $refModelTime;
my ($yyyymmdd,$hh);
my $refModelEndTime;
my $interm;
my $intermTime;
my $fcstTime;
my $interval=3;
my $globalModel;
my $EC_prefix;
my $validTime;
my $lastTime;
my $recordLength=64;
my $refModelCount;
my ($h,$hr);
my $skip;
my ($i,$j,$k,$x);
my $suffix;
my $pattern;
my @letters = ('A','B','C','D','E','F','G','H','I','J','K','L','M',
               'N','O','P','Q','R','S','T','U','V','W','X','Y','Z');

print "PID: $$\n";

getopts('j:t:m:');

if (! $opt_j || ! $opt_t || ! $opt_m) {
   die "Usage: $0 -j <job home> -t <time string> -m <reference model>\n";
} else {
  $JOBHOME = $opt_j;
  $timeString = $opt_t;
  $refModel = $opt_m;
}

$veriTime = `date --date='$timeString' +%Y%m%d%H`;

chomp($veriTime);

#$refModelTime = date_adjust($veriTime,-6);
$refModelTime = $veriTime;

$yyyymmdd = substr($refModelTime,0,8);
$hh = substr($refModelTime,8,2);

print "veriTime = $veriTime; refModelTime = $refModelTime\n";

$FLEXINPUT = "$JOBHOME/tmp/$veriTime/pre_process_in_donotedit.pl";

if (-e "$FLEXINPUT") {
   require "$FLEXINPUT";
} else {
   die "$FLEXINPUT does not exist!\n";
}

if ( -e  $GSJOBDIR."/verifyinput.pl" )
{
   require $GSJOBDIR."/verifyinput.pl";
} else {
   print ( " verifyinput.pl does not exist!  $GSJOBDIR \n");
   exit (1);
}

$globalModel = uc($refModel);
print "globalModel = $globalModel\n";

$VERIFY_DIR = "$RUNDIR/verify_interm";
$SFC_DIR = "$RUNDIR/veri_dat_interm/sfc";
$UPR_DIR = "$RUNDIR/veri_dat_interm/upr";
print "VERIFY_DIR = $VERIFY_DIR\n";
print "SFC_DIR    = $SFC_DIR\n";
print "UPR_DIR    = $UPR_DIR\n";

system("mkdir -p $VERIFY_DIR");
system("mkdir -p $SFC_DIR");
system("mkdir -p $UPR_DIR");
if ($VERI_ARCHIVE_ROOT) {
   system("mkdir -p $VERI_ARCHIVE_ROOT/$ENV{LOGNAME}/$GSJOBID/veri_dat_interm/{sfc,upr}");
}

chdir "$VERIFY_DIR";

# Add code for refModel decoding
system("mkdir -p $refModelTime");
chdir $refModelTime;
system("cp -r $JOBHOME/wps/Variable_Tables .");

if ($refModel =~ /gfs/i) {
   $BCIC = 'GFS';
} elsif ($refModel =~ /ecmwf/i) {
   $BCIC = 'ECMWF';
} else {
   $BCIC = uc($refModel);
} 

system("ln -s Variable_Tables/Vtable.${BCIC} Vtable");

$refModelCount = 0;
if ($BCIC eq 'GFS') {
  foreach $refFile (<$DATADIR/$refModel/${refModelTime}*>) {
# foreach $refFile (<$DATADIR/models/gribs/model-ncep-*-${yyyymmdd}_${hh}*>) {
    $k = $refModelCount%26;
    $x = int($refModelCount/26);
    $j = $x%26;
    $i = int($x/26);

    $suffix = "$letters[$i]$letters[$j]$letters[$k]";

    system("ln -sf $refFile GRIBFILE.$suffix");

    $refModelCount++;
  } 
} elsif ($BCIC eq 'UKMO') {
  foreach $refFile (<$DATADIR/$refModel/model-UKMET-*-${yyyymmdd}_${hh}*>) {
# foreach $refFile (<$DATADIR/models/gribs/model-bracknell-*-${yyyymmdd}_${hh}*>) {
    $k = $refModelCount%26;
    $x = int($refModelCount/26);
    $j = $x%26;
    $i = int($x/26);

    $suffix = "$letters[$i]$letters[$j]$letters[$k]";

    system("ln -sf $refFile GRIBFILE.$suffix");

    $refModelCount++;
  } 
} elsif ($BCIC eq 'DWD') {
  foreach $refFile (<$DATADIR/models/gribs/model-offenbach-1*-${yyyymmdd}_${hh}*>) {
    $refFile2 = $refFile;
    $refFile2 =~ s/model-offenbach-1/model-offenbach-2/;

    $dwdFile = $refFile;
    $dwdFile =~ s/model-offenbach-1/model-offenbach-/;
    $dwdFile =~ /(model-offenbach\S+)$/;
    $dwdFile = $1;

    system("cat $refFile $refFile2 > $dwdFile");

    $k = $refModelCount%26;
    $x = int($refModelCount/26);
    $j = $x%26;
    $i = int($x/26);

    $suffix = "$letters[$i]$letters[$j]$letters[$k]";

    system("ln -sf $dwdFile GRIBFILE.$suffix");

    $refModelCount++;
  } 
} elsif ($BCIC eq 'ECMWF') {
  if ($refModel eq 'ECMWF') {
     $EC_prefix='model-ecmwf2';
  } elsif ($refModel eq 'ECMWF9KM') {
     $EC_prefix='model-ecmwf';
  }
  foreach $refFile (<$DATADIR/lc($BCIC)/${EC_prefix}-*-${yyyymmdd}_${hh}*>) {
# foreach $refFile (<$DATADIR/models/gribs/${EC_prefix}-*-${yyyymmdd}_${hh}*>) {
    $k = $refModelCount%26;
    $x = int($refModelCount/26);
    $j = $x%26;
    $i = int($x/26);

    $suffix = "$letters[$i]$letters[$j]$letters[$k]";

    system("ln -sf $refFile GRIBFILE.$suffix");

    $refModelCount++;
  } 

} else {
  die "Wrong IC/BC selection: $BCIC !\n";
}

system("cp $JOBHOME/namelists/wps.nl.template .");

my $syy = substr($refModelTime,0,4);
my $smm = substr($refModelTime,4,2);
my $sdd = substr($refModelTime,6,2);
my $shh = substr($refModelTime,8,2);

$refModelEndTime = date_adjust($refModelTime,$VERI_LENGTH);

my $eyy = substr($refModelEndTime,0,4);
my $emm = substr($refModelEndTime,4,2);
my $edd = substr($refModelEndTime,6,2);
my $ehh = substr($refModelEndTime,8,2);

my $interval_s = $interval * 3600;

system("sed -e 's/SYY/'$syy'/g' -e 's/SMM/'$smm'/g' \\
            -e 's/SDD/'$sdd'/g' -e 's/SHH/'$shh'/g' \\
            -e 's/EYY/'$eyy'/g' -e 's/EMM/'$emm'/g' \\
            -e 's/EDD/'$edd'/g' -e 's/EHH/'$ehh'/g' \\
            -e 's/DOM/'$NUM_DOMS'/g' \\
            -e 's/ITT/'$interval_s'/' wps.nl.template > namelist.wps");

system("$EXECUTABLE_ARCHIVE/ungrib.exe > print_out.wrfwps");
#

chdir '../';

if (-s "obs.dat.$refModelTime") {
   system("ln -sf obs.dat.$refModelTime obs.dat");
}
if (-s "fort.61.$refModelTime") {
   system("ln -sf fort.61.$refModelTime fort.61");
}

my $WRF_sfcPair = "$VERI_PAIRS_DIR/sfc/fcst/${refModelTime}_veri_dat_GRM_P+FCST";

if (-s "$WRF_sfcPair") {
   $lastTime = lastRecordTime($WRF_sfcPair,$recordLength);
} else {
   die "WRF verification for cycle $refModelTime does not exist. Exit!!\n";
}

##$veriTime=2010012700;

$fcstTime = 0;
foreach $interm (<$refModelTime/FILE*>) {

   $interm =~ /FILE:(\d+)-(\d+)-(\d+)_(\d+)/;
   $intermTime = "$1$2$3$4";
   $hr = $4;

   next if($intermTime < $refModelTime);
   last if($intermTime > $lastTime);
   last if($fcstTime > $VERI_LENGTH);

   print  "$EXECUTABLE_ARCHIVE/v_intermediate.exe $interm -add_stid -add_hr\n";
   system("$EXECUTABLE_ARCHIVE/v_intermediate.exe $interm -add_stid -add_hr");

   print  "cat pairs_domain1 >> $SFC_DIR/${refModelTime}_veri_dat_${globalModel}\n" if (-e 'pairs_domain1');
   system("cat pairs_domain1 >> $SFC_DIR/${refModelTime}_veri_dat_${globalModel}") if (-e 'pairs_domain1');
   system("rm pairs_domain1") if (-e 'pairs_domain1');

   $skip = 1;
   if (@UPR_HOURS) {
      foreach $h (@UPR_HOURS) {
         $skip = 0 if ($h == $hr);
      }
   } else {
      $skip = 0;
   }

   if($skip) {
     $fcstTime += $interval;
     system("rm snd_pairs_domain1") if (-e 'snd_pairs_domain1');
     next;
   }

   print  "$EXECUTABLE_ARCHIVE/v_wrf_read_snd_pairs.exe snd_pairs_domain1 -height\n";
   system("$EXECUTABLE_ARCHIVE/v_wrf_read_snd_pairs.exe snd_pairs_domain1 -height");

   if (-s 'fort.81') {
      $validTime=`head -1 fort.81 | awk '{print \$1}'`;
      chomp($validTime);

      if ($VERI_ARCHIVE_ROOT) {
         print "cp fort.81 $VERI_ARCHIVE_ROOT/$ENV{LOGNAME}/$GSJOBID/veri_interm/upr/${refModelTime}_${validTime}_veri_dat_upr_${globalModel}\n";
         system("cp fort.81 $VERI_ARCHIVE_ROOT/$ENV{LOGNAME}/$GSJOBID/veri_interm/upr/${refModelTime}_${validTime}_veri_dat_upr_${globalModel}");
      }
      print  "mv fort.81 $UPR_DIR/${refModelTime}_${validTime}_veri_dat_upr_${globalModel}\n"; 
      system("mv fort.81 $UPR_DIR/${refModelTime}_${validTime}_veri_dat_upr_${globalModel}"); 
   }

   print  "rm -f snd_pairs_domain1\n" if (-e 'snd_pairs_domain1');
   system("rm -f snd_pairs_domain1") if (-e 'snd_pairs_domain1');

   $fcstTime += $interval;

}

if ($VERI_ARCHIVE_ROOT) {
   print "cp $SFC_DIR/${refModelTime}_veri_dat_${globalModel} $VERI_ARCHIVE_ROOT/$ENV{LOGNAME}/$GSJOBID/veri_dat_interm/sfc/.\n";
   system("cp $SFC_DIR/${refModelTime}_veri_dat_${globalModel} $VERI_ARCHIVE_ROOT/$ENV{LOGNAME}/$GSJOBID/veri_dat_interm/sfc/.");

}

# clean up

system("rm -rf $refModelTime");
system("rm -f obs.dat* fort.61*");

exit;

sub date_adjust {

  #
  # $orig_date in YYYYMMDDHH;
  # $hours_offset: hours into the future, or hours in the past (negative)
  # Returns a new date in YYYYMMDDHH
  #

  my ($orig_date,$hours_offset)=@_;
  my ($year,$month,$day,$hour);
  my ($year_m,$month_m,$day_m,$hour_m);

  my @mdays=(31,31,28,31,30,31,30,31,31,30,31,30,31);

  $year=        substr($orig_date,0,4);
  $month=       substr($orig_date,4,2);
  $day=         substr($orig_date,6,2);
  $hour=        substr($orig_date,8,2);

  if($year%4 == 0) {
    if($year%100 == 0) {
      $mdays[2]=29 if($year%400 == 0);
    } else {
      $mdays[2]=29;
    }
  }

  $year_m=$year;
  $month_m=$month;
  $day_m=$day;
  $hour_m=$hour+$hours_offset;

  while($hour_m < 0) {
# while($hour_m <= 0) {              ## obs hours are 1-24 !
    $hour_m += 24;
    $day_m -= 1;
    while($day_m <= 0) {
      $day_m += $mdays[$month_m-1];
      $month_m -= 1;
      while($month_m <= 0) {
        $month_m += 12;
        $year_m -= 1;
      }
    }
  }

  while($hour_m >= 24) {
    $hour_m -= 24;
    $day_m++;
    while($day_m > $mdays[$month_m]) {
      $day_m -= $mdays[$month_m];
      $month_m++;
      while ($month_m > 12) {
        $month_m -= 12;
        $year_m++;
      }
    }
  }

  my $date_m=$year_m*1000000+$month_m*10000+$day_m*100+$hour_m;

  return $date_m;

}
#
#
#
sub lastRecordTime {

  my ($file,$bytesPerRecord) = @_;

  my ($year,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
   $psfc_m,$psfc_o,$psfc_qc,
   $slp_m,$slp_o,$slp_qc,
   $ter_m,$ter_o,
   $t_m,$t_o,$t_qc,
   $q_m,$q_o,$q_qc,
   $ws_m,$ws_o,$ws_qc,
   $wd_m,$wd_o,$wd_qc,$st_id);
  
  open(TMP,"$file");
  seek(TMP,-$bytesPerRecord,2);

  my $buf;

  read(TMP,$buf,$bytesPerRecord);

  ($year,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
   $psfc_m,$psfc_o,$psfc_qc,
   $slp_m,$slp_o,$slp_qc,
   $ter_m,$ter_o,
   $t_m,$t_o,$t_qc,
   $q_m,$q_o,$q_qc,
   $ws_m,$ws_o,$ws_qc,
   $wd_m,$wd_o,$wd_qc,$st_id) = unpack("s6a4s20a8",$buf);

   my $time = $year*100000000 + $monthday*10000 + $hourmin;

   my $timeHour = substr($time,0,10);

   return $timeHour;

}

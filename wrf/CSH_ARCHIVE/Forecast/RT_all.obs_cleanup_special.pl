#!/usr/bin/perl

##########################################################################
##  This program is written to process the obs data of the intermediate ##
##  format for little_r of mm5 system. It can be used to                ##
##    1). extract the obs on a particular domain defined by lat and lon ##
##        which will save much of little_r CPU time                     ##
##    2). extract special obs (eg: Metar, snd, profile and so on)       ##
##    3). extract obs in a special time window                          ##
##    4). Remove duplicate obs, merge sounding (pilot,temp...) and sort ##
##        and sort the sounding level according to pressure             ##
##    5). sort reports according to time, lat, long and station height  ##
##    6). Any combination of 1) to 5).                                  ##
##    7). List all obs stations                                         ##
##    8). List unique obs stations                                      ##
##             by Yubao Liu   in June 2000 at RAP/NCAR                  ##
##########################################################################
#

use Getopt::Std;

use lib '/data/fddahome/cycle_code/SOURCE/RT_VERIFY';
use Time;

getopts('Sf:');

if(length($opt_f) == 0) {
  print "Usage: $0 -f input_filename [-S (for soundings)]\n";
  exit;
}

open(IN,"$opt_f");
if($opt_S == 1) {
  open(ME,">soundings_obs.cleanup");
} else {
  open (ME, ">surface_obs.cleanup");
}

open (SK, ">all.obs.unique_station");

if($opt_S == 1) {
  $S_flag='T';         #### soundings
} else {
  $S_flag='F';
}

my %tmp;
%header=();
%body=();
%tail=();
while($aline = <IN>) {
  if ( substr($aline,0,10)  == '          ') {
   ($junk, $lat, $lon, @dd) = split(/\s+/,$aline);
   $Sounding = substr($aline,279,1);
   $Date = substr($aline,326,10);  # date
   if ($Sounding EQ $S_flag) {
    $k1 = substr($lat,0,7);   
    $k2 = substr($lon,0,11);
    $k3 = substr($aline,209,7);   # height
    $k4 = substr($aline,326,14);  # date and time 
    $k5 = $Sounding;                                
#   $kk = $k4." ".$k1." ".$k2." ".$k3." ".$k5;     # for preprocessing to little_r
#   $kk = $k1." ".$k2." ".$k4." ".$k3." ".$k5;     # for verification
    $kk = $k1." ".$k2." ".$k4." ".$k5;             # height complicates things!
    $kk = $k4." ".$k1." ".$k2." ".$k5 if($S_flag eq 'T');
#   $kk = $k1." ".$k2." ".$k3;                     # for unique station
#   $k0 = substr($aline,85,5);            
#   $kk = $k1." ".$k2." ".$k3." ".$k0;                     # for test
#   $kk = $aline;                                  # for all report
#   print $kk, "\n";
    $header{$kk} = $aline;
    $bline=<IN>;
    while (substr($bline,0,7) NE '-777777') {
     if($Sounding EQ "T") {
      $body{$kk} = join("",$body{$kk}, $bline);  # keep all level of sounding
     }
     else {
      $body{$kk} = $bline;     # only one surface report is retained
     }
      $bline=<IN>;
    }
     $cline=<IN>;
    $tail{$kk} = join("",$bline, $cline);

    if($aline=~ /PROFILER/ || $aline=~ /ACARS/) {
      delete $header{$kk};
      delete $body{$kk};
      delete $tail{$kk};
    }

    if($aline=~ /SAMS ATEC/) {
      $mmss=substr($k4,10,4);
      if($mmss <= 1000 or $mmss >= 5000) {
        $sams_info=$k1 . " " . $k2 . " " . $k4;
        push (@sams_list,$sams_info);
      }
    }

   }

  }
}

#
# Delete duplicate within short time period
#
$id_old='';
foreach $identifier (sort keys %header) {
  ($lat_old,$lon_old,$date_old,$sflag_old)=split " ",$id_old;
  ($lat,$lon,$date,$sflag)=split " ",$identifier;

  $mmss=substr($date,10,4);

  if($opt_S != 1) {
    if(($mmss > 1000) && ($mmss < 5000)) {       ## keep only those within +- 10
      delete $header{$identifier};               ## min window of top of hour
      delete $body{$identifier};
      next;
    }
  }

  if($header{$identifier}=~ /METNET\= *3/) {   ## Remove UUTAH's duplicate
                                               ## SAMS report
    $match=check_sams($identifier,\@sams_list);
    if($match == 1) {
      delete $header{$identifier};
      delete $body{$identifier};
    }
#   next;
  }

  ($year_old,$month_old,$day_old,$hour_old)=nearest_hour($date_old);
  ($year,$month,$day,$hour)=nearest_hour($date);

  if(($year_old == $year) && ($month_old == $month) &&
     ($day_old == $day) && ($hour_old == $hour) &&
     ($lat_old == $lat) && ($lon_old == $lon)) {

#
#   Determine which one is closer to top of the hour, $flag=0 means older
#
    $flag=whos_closer($year,$month,$day,$hour,$date_old,$date);

    if($flag == 0) {
      delete $header{$identifier};
      delete $body{$identifier};
    } else {
      delete $header{$id_old};
      delete $body{$id_old};
      $id_old=$identifier;
    }
  } else {
    $id_old=$identifier;
  }
}


foreach $k (sort keys %header) {
  %tmp=();                                 # remove duplicate reports on one level
 foreach $t ( split(/\n/,$body{$k})) {     # and sort the level according to pres
  $p = substr($t,0,7);
  $tmp{$p} = $t;
 }
  @btmp=();
 foreach $p ( sort keys %tmp) {
  @btmp = (@btmp, $tmp{$p});
 }
 $body{$k} = join("\n",@btmp);
 $rec_num = @btmp;
 substr($tail{$k},40,7) = sprintf("%7d",$rec_num);
 
# remove 0- and 1-level sounding which is normally bad report anyway!
# $header{$k} =~ tr/T         F         F/F         F         F/ ;        
  if(($rec_num > 1 && substr($header{$k},279,1) EQ "T") ||
      substr($header{$k},279,1) EQ "F") {        
#   if($header{$k}=~ / SAMS / && substr($header{$k},336,4) eq '0730') {
#     print "Skipping SAMS at ...5230!\n";
#   } else {
      print ME $header{$k}, $body{$k},"\n", $tail{$k};  
#   }
  }
}

close(IN);
close(ME);
#system("mv all.obs.trim all.obs.${a}z");

#exit;
foreach $k (sort keys %header) {             # log the unique station list
 $k0 = substr($header{$k},85,5);            
 $kn = $k;
 chop($kn);
 print SK $k0," ", $kn,"\n" if ($kn NE $ko);
# print SK $k0, $k,"\n";
 $ko = $kn
}

#
#
#
#
#

sub nearest_hour {

  my $date=$_[0];
  my ($year,$month,$day,$hour,$minute,$sec);
  my @days=(31,31,28,31,30,31,30,31,31,30,31,30,31);

  $year=substr($date,0,4);
  $month=substr($date,4,2);
  $day=substr($date,6,2);
  $hour=substr($date,8,2);
  $minute=substr($date,10,2);
  $sec=substr($date,12,2);

  if($year%400 == 0) {
    $days[2]=29;
  } else {
    $days[2]=29 if(($year%4 == 0) && ($year%100 != 0));
  }

  if($sec >= 30) {
    $minute += 1;
    if($minute >= 60) {
      $minute -= 60;
      $hour += 1;
      if($hour >= 24) {
        $hour -= 24;
        $day += 1;
        if($day > $days[$month]) {
          $day -= $days[$month];
          $month += 1;
          if($month > 12) {
            $month -= 12;
            $year += 1;
          }
        }
      }
    }
  }

  if($minute >= 30) {
    $minute = 0;
    $hour += 1;
    if($hour >= 24) {
      $hour -= 24;
      $day += 1;
      if($day > $days[$month]) {
        $day -= $days[$month];
        $month += 1;
        if($month > 12) {
          $month -= 12;
          $year += 1;
        }
      }
    }
  }

  return ($year,$month,$day,$hour);

}

#
#
#
#
#

sub whos_closer {

  my ($year,$month,$day,$hour,$date0,$date1)=@_;
  my $ref,$flag;

  $ref=date2secs($year,$month,$day,$hour,0,0,0);

  $yy=substr($date0,0,4);
  $mm=substr($date0,4,2);
  $dd=substr($date0,6,2);
  $hh=substr($date0,8,2);
  $mn=substr($date0,10,2);
  $ss=substr($date0,12,2);

  $sec0=date2secs($yy,$mm,$dd,$hh,$mn,$ss,0);

  $yy=substr($date1,0,4);
  $mm=substr($date1,4,2);
  $dd=substr($date1,6,2);
  $hh=substr($date1,8,2);
  $mn=substr($date1,10,2);
  $ss=substr($date1,12,2);

  $sec1=date2secs($yy,$mm,$dd,$hh,$mn,$ss,0);

  $d0=abs($ref-$sec0);
  $d1=abs($ref-$sec1);

  if($d0 < $d1) {
    $flag=0;
  } else {
    $flag=1;
  }

  return $flag;
}
#
#
#
sub check_sams {

  my ($key_m3,$r_sams_list)=@_;
  my ($time_m3,$lat_m3,$lon_m3,$year_m3,$month_m3,$hour_m3,$min_m3,$sec_m3,$secs_m3);
  my $match;
  my $sams_key;
  my ($time,$lat,$lon,$year,$month,$hour,$min,$sec,$secs);
  my ($loc_diff,$time_diff);

  ($lat_m3,$lon_m3,$time_m3)=split " ",$key_m3;
  $year_m3=substr($time_m3,0,4);
  $month_m3=substr($time_m3,4,2);
  $day_m3=substr($time_m3,6,2);
  $hour_m3=substr($time_m3,8,2);
  $min_m3=substr($time_m3,10,2);
  $sec_m3=substr($time_m3,12,2);
  $secs_m3=date2secs($year_m3,$month_m3,$day_m3,$hour_m3,$min_m3,$sec_m3,0);

  $match=0;
  foreach $sams_key (@$r_sams_list) {
    ($lat,$lon,$time)=split " ",$sams_key;
    $year=substr($time,0,4);
    $month=substr($time,4,2);
    $day=substr($time,6,2);
    $hour=substr($time,8,2);
    $min=substr($time,10,2);
    $sec=substr($time,12,2);
    $secs=date2secs($year,$month,$day,$hour,$min,$sec,0);
    $loc_diff=sqrt(($lat_m3-$lat)**2+($lon_m3-$lon)**2);
    $time_diff=abs($secs_m3-$secs);

#yliu   if($time_diff < 500 && $loc_diff < 0.005) {
    if($time_diff < 300 && abs($lat_m3-$lat) < 0.005 && abs($lon_m3-$lon) < 0.005) {
      $match=1;
      last;
    }
  }

  return $match;
}

#!/usr/bin/perl

##########################################################################
##  This program is written to process the obs data of the intermediate ##
##  format for little_r of mm5 system. It is used to                    ##
##    1). extract all sounding obs or all surface obs for verification  ##
##    2). extract onlr those close to the hours (with 10 minutes)       ##
##    3). Remove duplicate obs and always keep SAMS                     ##
##    4). sort reports according to time, lat, long and station height  ##
##    5). List all obs stations                                         ##
##    6). List unique obs stations                                      ##
##             by Yubao Liu   in June 2000 at RAP/NCAR                  ##
##             revised by Rong Sheu in Dec 2000 at RAP/NCAR             ##
##             revised by Yubao Liu in Dec 2003 at RAP/NCAR             ##
##               for faster and more accurate "check_sams" and          ##
##                                           "who_is_closer"            ##
##########################################################################
#

use Getopt::Std;

getopts('Sf:l:');

if(length($opt_f) == 0) {
  print "Usage: $0 -f input_filename [-S (for soundings)]\n";
  exit;
}

print "PERL_ARCHIVE = $ENV{PERL_ARCHIVE}\n";

if($opt_l) {
  require "${opt_l}/TimeUtil.pm";
  print "Use TimeUtil in ${opt_l}\n";
} else {
  require '/data/fddahome/cycle_code/PERL/TimeUtil.pm';
}

open(IN,"$opt_f");
if($opt_S == 1) {
  open(ME,">soundings_obs.cleanup");
} else {
  open (ME, ">surface_obs.cleanup");
}


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
   if ($Sounding eq $S_flag) {
    $k1 = substr($lat,0,7);   
    $k2 = substr($lon,0,9);
    $k3 = substr($aline,209,7);   # height
    $k4 = substr($aline,326,14);  # date and time 
    $k5 = $Sounding;                                
#   $kk = $k4." ".$k1." ".$k2." ".$k3." ".$k5;     # for preprocessing to little_r
#   $kk = $k1." ".$k2." ".$k4." ".$k3." ".$k5;     # for verification
    $kk = $k1." ".$k2." ".$k4." ".$k5;             # height complicates things!
    $kk = $k4." ".$k1." ".$k2." ".$k5 if($S_flag eq 'T');
    $kk = $k4." ".$k1." ".$k2." ".$k5 ;
#   $kk = $k1." ".$k2." ".$k3;                     # for unique station
#   $k0 = substr($aline,85,5);            
#   $kk = $k1." ".$k2." ".$k3." ".$k0;                     # for test
#   $kk = $aline;                                  # for all report
#   print $kk, "\n"; 

#yliu start -- work -- kk1 to replace the costly "check_sams" below
    $mmss_b=substr($k4,10,4);
    $lat_b=sprintf("%8.3f",$k1);
    $lon_b=sprintf("%8.3f",$k2);
  if ($Sounding eq 'F') {
   if($mmss_b <= 1000 || $mmss_b >= 5000) {    # do not save non-on-hour obs
    $verify_h = $Date;
    $ssss = sprintf("%4.4d",60*substr($mmss_b,0,2)+substr($mmss_b,2,2));
    if ($mmss_b >= 5000) {                      #shift 1h hour for mmss > 5000 for right "who_close"
     $verify_h = &hh_advan_date($Date,1) if ($mmss_b >= 5000);  
     $ssss = sprintf("%4.4d",3600-$ssss); 
    }
    $kk1 = $verify_h."0000"." ".$lat_b." ".$lon_b." ".$k5; #Mesowest SAMS has only two decimal lat/lon
    $kk = $verify_h.$ssss." ".$lat_b." ".$lon_b." ".$k5;   # use seconds instead of mmss for "who_close"
 
    if($aline =~ /SAMS ATEC/ || ! $flag_sams{$kk1}) {  # keep only SAMS if with
     $flag_sams{$kk1} = 1 if($aline =~ /SAMS ATEC/);   # the same lat, lon and time window
#yliu end -- work -- kk1 to replace the costly "check_sams" below
     $header{$kk} = $aline;                            # keep all other obs
     $bline=<IN>;
     while ($bline && substr($bline,0,7) ne '-777777') {
      if($Sounding eq "T") {
       $body{$kk} = join("",$body{$kk}, $bline);  # keep all level of sounding
      }
      else {
       $body{$kk} = $bline;     # only one surface report is retained
      }
      delete $header{$kk} if(length($bline) > 201); # remove bad record
      $bline=<IN>;
     }
     $cline=<IN>;
     $tail{$kk} = join("",$bline, $cline);
    }
   }
  } else {  # soundings
    $header{$kk} = $aline;
    $bline=<IN>;
    while ($bline && substr($bline,0,7) ne '-777777') {
      $body{$kk} = join("",$body{$kk}, $bline);  # keep all level of sounding
      $bline=<IN>;
    }
    $cline=<IN>;
    $tail{$kk} = join("",$bline, $cline);
  }

   delete $header{$kk} if(length($header{$kk}) > 601); # remove bad record
   delete $header{$kk} if(length($tail{$kk}) > 402); # remove bad record

    if($aline=~ /PROFILER/ || $aline=~ /ACARS/ || $aline=~ /SATW/ || $body{$kk} =~ "nan" || $aline=~ /AMV/ || $aline=~ /RADIOMETER/) {
      delete $header{$kk};
      delete $body{$kk};
      delete $tail{$kk};
    }
   }
  }
}

#
# Delete duplicate within short time period
#
$id_old='';
foreach $identifier (sort keys %header) {
#   print "READ_IN DATA $identifier \n" ;

  ($date_old,$lat_old,$lon_old,$sflag_old)=split " ",$id_old;
  ($date,$lat,$lon,$sflag)=split " ",$identifier;
  ($year_old,$month_old,$day_old,$hour_old)=nearest_hour2($date_old);
  ($year,$month,$day,$hour)=nearest_hour2($date);

  if(($year_old == $year) && ($month_old == $month) &&
     ($day_old == $day) && ($hour_old == $hour) &&
     ($lat_old == $lat) && ($lon_old == $lon)) {

#
#   Determine which one is closer to top of the hour, $flag=0 means older
#
    $flag=whos_closer($year,$month,$day,$hour,$date_old,$date);
    print "$identifier <--> $id_old \n"; 
    print "$lat_old $lat $lon_old $lon"; 

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
 #print "key = $k \n";
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
  if(($rec_num > 1 && substr($header{$k},279,1) eq "T") ||
      substr($header{$k},279,1) eq "F") {        
#   if($header{$k}=~ / SAMS / && substr($header{$k},336,4) eq '0730') {
#     print "Skipping SAMS at ...5230!\n";
#   } else {
      print ME $header{$k}, $body{$k},"\n", $tail{$k};  
     $latlon=substr($k,14,18);
     push(@latlon, $latlon);
#   }
  }
}

close(IN);
close(ME);
#system("mv all.obs.trim all.obs.${a}z");

#exit;
open (SK, ">all.obs.unique_station");
foreach $k (sort keys %header) {             # log the unique station list
 $k0 = substr($header{$k},81,5);            
 $kn = $k;
 chop($kn);
 print SK $k0," ", $kn,"\n" if ($kn ne $ko);
 $ko = $kn;
}

if($opt_S == 1) {
open (SND, ">rip_sndout.in");
print SND " &userin \n";
#print SND " ptimes=0,12,24,36,\n";
print SND " ptimes=1,-24,1,\n";
print SND " ptimeunits='h',tacc=120,timezone=-7,iusdaylightrule=1,\n";
print SND " rip_root='/data/fddahome/cycle_code/SOURCE/RIP4' \n";
print SND " &end\n";
print SND "======================================================\n";
print SND "---------- rip soundings -----------------------------\n";
print SND "======================================================\n";
foreach $k (sort @latlon) {
 if(! $exist{$k}) {
  $klat=sprintf("%6.3f",substr($k,0,9));
  $klon=sprintf("%6.3f",substr($k,9,9));
  print SND "feld=uuu,vvv; ptyp=sv; sloc=",$klat,"lat,",$klon,"lon; bogs\n";
  print SND "======================================================\n";
  $exist{$k}= 1
 }
}
}

if($opt_S != 1) {
open (SLL, ">tserstn.dat");
 foreach $k (sort @latlon) {
  if(! $exist{$k}) {
   $klat=sprintf("%6.3f",substr($k,0,9));
   $klon=sprintf("%6.3f",substr($k,9,9));
   print SLL $klat,"lat,",$klon,"lon\n";
   $exist{$k}= 1
  }
 }
 open (SFC, ">rip_sfcout.in");
 print SFC " &userin \n";
 print SFC " ptimes=1,-24,1,\n";
 print SFC " ptimeunits='h',tacc=120,timezone=-7,iusdaylightrule=1,\n";
 print SFC " idotser=1,\n";
 print SFC " rip_root='/data/fddahome/cycle_code/SOURCE/RIP4' \n";
 print SFC " &end\n";
 print SFC "======================================================\n";
 print SFC "---------- rip sfc station time series ---------------\n";
 print SFC "======================================================\n";
 print SFC "feld=sfp; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=slp; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=ter; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=t2c; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=sdpk; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=wsp; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=wdr; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=q2m; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=qvp; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=U10; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=V10; ptyp=hc\n";
 print SFC "======================================================\n";
 print SFC "feld=tmk; ptyp=hc\n";
 print SFC "======================================================\n";
}


#
#
#
#
#

sub nearest_hour2 {

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
  while($dd < 1) {
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


#!/usr/bin/perl
#

$f_in = $ARGV[0]; 
$f_in = "all.obs" if( ! $f_in);
$MIN_END=$ARGV[1]; 
 #$MIN_END=59:  0 -> 59;  $MIN_END=30: -30 -> 30; $MIN_END=61: all in one file
$MIN_END=59 if( ! $MIN_END); 
$LATLON_FILE=$ARGV[2];
$LATLON_FILE="jjunk_ddummy" if(! $ARGV[2]);
#open (ME, ">$f_out");
$RM_DUB=1;

# withhold data according to their lat lon
%withh=();
open (IN, "../../withholdlatlonsams");
while($aline=<IN>) {
 chomp ($aline);
 ($v1,$vv) = split(/ +/,$aline);
 $withh{$v1}=$vv;
}
close(IN);
#print %withh; exit;
#
 
$flag = 0;
@ALLKK=();
%tmp=();
%header=();
%body=();
%tail=();

## Get the MM5 domains limits, when requested.
$latmin = -90; $latmax =  90; $lonmin = -180; $lonmax =  180;
if(-s "$LATLON_FILE") {
 open(IN,"$LATLON_FILE");
 $aline=<IN>; chomp($aline);
 ($latmin, $latmax) = split(' ',$aline); 
 $aline=<IN>; chomp($aline);
 ($lonmin, $lonmax) = split(' ',$aline);
 close(IN);
}

print "Limit obs from ${latmin} to ${latmax} North and from ${lonmin} to ${lonmax} East. \n";

open (IN, "$f_in");
while($aline = <IN>) {
  $record_start = substr($aline,0,10);
  if ( $record_start == '          ') {
   $lat = substr($aline,10,10);
   $lon = substr($aline,30,10);
   if ($lat < $latmax && $lat > $latmin && $lon < $lonmax && $lon > $lonmin
#  if (1
#  if ($lat < 60 && $lat > 15 && $lon < -55 && $lon > -155  # cover USA
#  if ($lat < 50 && $lat > 25 && $lon < -55 && $lon > -95   #atc
#  if ($lat < 45 && $lat > 5  && $lon < -110 && $lon > -179  # cover Hawaii  
#  && $aline =~ /METAR/ 
#  && $aline =~ /19980209/
#  && $aline =~ /PILOT/
    ) {
   $elev = substr($aline,209,7);
   $nlev = substr($aline,226,4);
   $issnd = substr($aline,279,1);
   $seqid = substr($aline,256,4);
   $date = substr($aline,326,14);
   $stid = substr($aline,40,5);
   $stna = substr($aline,80,12);
   $fm = substr($aline,120,12);
   $kk = $date."#".$lat."#".$lon."#".$elev."#".$issnd
               ."#".$stid."#".$stna
#              ."#".$fm
#              ."#".$nlev 
#              ."#".$seqid
               ."#";
   $kk = $kk.$fm."#" if($MIN_END < 31);
#   $kk.=$RM_DUB;
#   $RM_DUB++ if ($RM_DUB > 0);
#    print $kk,"\n";  #exit;
    %tmp=();       
    $test_dub=0;
    if($header{$kk}) {       # find an obs with same key, so prepare to merge them
     $test_dub=1; 
     if($issnd eq "T") {     # retrieve the levels of exist upr data
      foreach $t ( split(/\n/,$body{$kk})) {     # and sort the level according to pres
       $p1 = substr($t,0,7) ;
       if($p1 > 0) {$p  = "P".$p1;} else { $p  = "P       ";} 
       $h1 = substr($t,20,7);
       if($h1 >= 0 && $h1 < 288887 ) {$h  = "H".$h1;} else { $h  = "H       ";} 
#      if($header{$kk} =~ "TEMP") {$hp = $p.$h;} else {$hp = $h.$p;} 
       if($header{$kk} =~ "TEMP") {$hp = $p.$h;} else {$hp = $p.$h;} 
       $tmp{$hp} = $t."\n";
#        print "hp1=$hp \n";
      }
     }
    }
   $header{$kk} = $aline if(length($aline) == 601 || length($aline) == 621);
   $bline=<IN>;
    while ($bline && substr($bline,0,7) ne '-777777') {
    delete $header{$kk} if(length($bline) > 201);
     if($issnd eq "T") {  # keep all good data from all sounding reports
        $p1 = substr($bline,0,7) ;
        if($p1 > 0) {$p  = "P".$p1;} else { $p  = "P       ";} 
        $h1 = substr($bline,20,7);
        if($h1 >= 0 && $h1 < 288887) {$h  = "H".$h1;} else { $h  = "H       ";} 
       if($test_dub == 0) {   # a fresh sounding, record it in
         if($header{$kk} =~ "TEMP") {$hp = $p.$h;} else {$hp = $p.$h;}
#         if($header{$kk} =~ "TEMP") {$hp = $p.$h;} else {$hp = $h.$p;}
         $tmp{$hp} = $bline;
#       $body{$kk} = join("",$body{$kk}, $bline); 
       } else {

        $line_dub=0;
        if( $p1 > 0  && grep(/$p/,keys(%tmp))) {  
           ($hp)=grep(/$p/,keys(%tmp));
           $line_dub++;
         #  print "merge hp=$hp  + $h1 $p1\n";
        }
        if( $h1 >= 0 && $h1 < 288887 && grep(/$h/,keys(%tmp))) {
          ($hp)=grep(/$h/,keys(%tmp));
           $line_dub++;
         #  print "merge hp=$hp  - $h1 $p1\n";
        } 
        $phtmp=$p.$h;
        if($line_dub == 2 && ! grep(/$phtmp/,keys(%tmp))) { 
         print "inconsistent Pressure and Height, by-pass this level \n";
        } elsif ($line_dub > 0) {    #two dub levels, merge them
#        print "merge hp=$hp  - $h1 $p1\n";
         substr($tmp{$hp},  0,20)=substr($bline, 0,20) if(substr($bline, 0,10) > -100000);
         substr($tmp{$hp}, 20,20)=substr($bline,20,20) if(substr($bline,20,10) > -100000);
         substr($tmp{$hp}, 40,20)=substr($bline,40,20) if(substr($bline,40,10) > -100000);
         substr($tmp{$hp}, 60,20)=substr($bline,60,20) if(substr($bline,60,10) > -100000);
         substr($tmp{$hp}, 80,20)=substr($bline,80,20) if(substr($bline,80,10) > -100000);
         substr($tmp{$hp},100,20)=substr($bline,100,20) if(substr($bline,100,10) > -100000);
        } else {       # find a new level, just add it in
         if($header{$kk} =~ "TEMP") {$hp = $p.$h;} else {$hp = $p.$h;} 
#         if($header{$kk} =~ "TEMP") {$hp = $p.$h;} else {$hp = $h.$p;} 
         $tmp{$hp} = $bline;
         print "new level hp=$hp \n";
        }
       }
       #"TEMP" DECODER SOMETIMES WRITE A WRONG MISSING VALUE -888888 -> 888888
       substr($tmp{$hp}, 20,20)="-888888.00000-888888" if(substr($bline,20,10) > 888000);
     } else { # only retain one (later) surface report 
      $body{$kk} = $bline;     # only one late surface report is retained
# sams withhold
       foreach $klat (keys %withh) {
         if(abs($lat - $klat) < 0.005 && abs($lon - $withh{$klat}) < 0.005) {
         substr($body{$kk}, 13, 7)=" 666666";
         substr($body{$kk}, 33, 7)=" 666666";
         substr($body{$kk}, 53, 7)=" 666666";
         substr($body{$kk}, 73, 7)=" 666666";
         substr($body{$kk}, 93, 7)=" 666666";
         substr($body{$kk},113, 7)=" 666666";
         substr($body{$kk},133, 7)=" 666666";
         substr($body{$kk},153, 7)=" 666666";
         substr($body{$kk},173, 7)=" 666666";
         substr($body{$kk},193, 7)=" 666666";
         }
       }
#
     }
     $bline=<IN>;
    }
   $cline=<IN>;
   delete $header{$kk} if(length($cline) > 201);
    if($issnd eq "T") {  # reconstrut body with merged levels and sort the levels
      @btmp=();
#    if($header{$kk} =~ "TEMP") {
      foreach $p ( reverse sort keys %tmp) { @btmp = (@btmp, $tmp{$p}) if($tmp{$p} !~ " nan"); }
#    } else {
#     foreach $p ( sort keys %tmp) { @btmp = (@btmp, $tmp{$p}); }
#    }
     $body{$kk} = join("",@btmp);
     $rec_num = @btmp;
#      print "rec_num = $rec_num \n"; #exit;
     substr($bline,40,7) = sprintf("%7d",$rec_num);
     substr($cline,0,7) = sprintf("%7d",$rec_num);
     substr($header{$kk},226,4) = sprintf("%4d",$rec_num) if($header{$kk});
    }
   $tail{$kk} = $bline.$cline;
   if(substr($bline,40,7) == 0) { delete $header{$kk}; delete $body{$kk};delete $tail{$kk};}
# MX - delete prepbufr data in addtional column (such as GPSIPW)
   $column14=substr($header{$kk},600,7);
   if($column14 ne '-888888' && length($header{$kk}) > 608) { delete $header{$kk}; delete $body{$kk};delete $tail{$kk};}
   }  # if lat lon
  } # if new record 
}  # while read 

@ALLKEYS = keys %header;
#@ALLUPR = grep(/#T#/,@ALLKEYS);
#@ALLSFC = grep(/#F#/,@ALLKEYS);

if($MIN_END > 60) {
 $f_out = "$f_in.trim";
 open (ME, ">$f_out");
 $qcoutdate = substr($f_in,7,4).substr($f_in,12,2).substr($f_in,15,2).substr($f_in,18,2);
#print $qcoutdate ; exit;
 foreach $k (sort grep(/$qcoutdate/,@ALLKEYS)) {
# foreach $k (sort @ALLKEYS) {
  if( $body{$k} !~ '\*' && $body{$k} !~ ' nan') {
#  print ME $header{$k};
   print ME substr($header{$k},0,600),"\n";
   print ME $body{$k};
   print ME $tail{$k};
  }
 }
 close(ME);close(IN); exit;
} 
#
$k=shift(@ALLKEYS);
push(@ALLKEYS,$k);
$datehour=substr($k,0,10);
$minute=substr($k,10,2);
$datehour=&hh_advan_date($datehour,1) if($minute > $MIN_END);
$f_out=$datehour.".$f_in";
#print "open $f_out\n";
open (ME, ">$f_out");
foreach $k (sort @ALLKEYS) {
 $minute=substr($k,10,2);
 $datehour_n=substr($k,0,10);
 $datehour_n=&hh_advan_date($datehour_n,1) if($minute > $MIN_END);
 if($datehour_n != $datehour) {
  close (ME);
  $f_out=$datehour_n.".$f_in";
  open (ME, ">$f_out");
  $datehour=$datehour_n;
 }
 if( $body{$k} !~ '\*' || $body{$k} !~ '  nan') {
# print ME $header{$k};
  print ME substr($header{$k},0,600),"\n";
  print ME $body{$k};
  print ME $tail{$k};
 }
}
 close(ME);close(IN);
 print "Done sorting and merging \n";


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
  $yy--;
  }
  }
 
  my $new_date = sprintf("%04d%02d%02d%02d",$yy,$mm,$dd,$hh);
}

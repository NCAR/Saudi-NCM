#!/usr/bin/perl
$source='_WMO_';
<>;
open (OUT, ">gts_sttnid_input.wmo.txt");
while (<>) {
  next if($. == 1);
  ($RegId,$CountryArea,$Id,$StationName,$alat,$alon,$alt1,$alt2,$alt3,$alt4)=split "\t";
  @Area=split "/", $CountryArea;
  $Area_English=$Area[0];
  chomp($Area_English);
  $alat=~ /(\d+) (\d+) (\d+)(\w+)/;
  $lat=$1+$2/60;
  $lat *= 100;
  $lat= int($lat);

  $lat += 10000;
  $alat=substr($lat,1,4) . 'N';
  $alat='-' . $alat if($4 eq 'S');

  $alon=~ /(\d+) (\d+) (\d+)(\w+)/;
  $lon=$1+$2/60;
  $lon *= 100;
  $lon= int($lon);

  $lon += 100000;
  $alon=substr($lon,1,5) . 'E';
  $alon='-' . $alon if($4 eq 'W');

  if($alt1=~ /\d+/) {
    $alt=$alt1;
  } else {
    if($alt2=~ /\d+/) {
      $alt=$alt2;
    } else {
      if($alt3=~ /\d+/) {
        $alt=$alt3;
      } else {
        if($alt4=~ /\d+/) {
          $alt=$alt4;
        } else {
# set elevation to zero if there is no height infor at all.
#         $alt=0;  
          next;
        }
      }
    }
  }

  $alt += 10000;
  $aalt=substr($alt,1,4) . 'm';
 if($Id > 1 && $Id < 99999) {
  $aline = sprintf "%5s%7s%8s%7s%2d%6s %s / %s",
    $Id,$alat,$alon,$aalt,$RegId,$source,$StationName,$Area_English;
  $line='                                                                                                    ';
  $leng=length($aline);
  substr($line,0,$leng)=$aline;
  print OUT "$line\n";
 }
}
close(OUT);
exit;

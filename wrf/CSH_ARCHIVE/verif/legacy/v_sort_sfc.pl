#!/usr/bin/perl 

open(IN,"fort.31");
while ($aline=<IN>) {
 $mdate=substr($aline,0,10);
 $min=substr($aline,10,2);
 $mdate=&hh_advan_date($mdate,1) if($min > 30); 
 substr($aline,0,10)=$mdate;
 substr($aline,10,4)="0000";
 $k=$mdate.substr($aline,14,18);
 $rec{$k}=$aline;
}

open(ME,">obs.dat");
foreach $k (sort keys %rec) {
 print ME $rec{$k};
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

#!/usr/bin/perl

#
# This script contains the subroutines mm_advan_date() and hh_advan_date()
#


#-------------------------------------------------------------------------------
# Name: hh_advan_date
# Arguments: 1) a date as yyyymmddhh
#            2) number of minutes as an integer
# Return: a date in 'yyyymmddhh'-form
# Description: advances given date in 1st argument by the number of min given
#              in the second argument
#-------------------------------------------------------------------------------

sub mm_advan_date
{
  %mon_days = (1,31,2,28,3,31,4,30,5,31,6,30,7,31,8,31,9,30,10,31,11,30,12,31);
  (my $s_date, my $advan_mm) = @_ ;

  my $yy = substr($s_date,0,4);
  my $mo = substr($s_date,4,2);
  my $dd = substr($s_date,6,2);
  my $hh = substr($s_date,8,2);
  my $mm = substr($s_date,10,2);

  my $feb = 2;
  $mon_days{$feb} = 29 if ($yy%4 == 0 && ($yy%400 == 0 || $yy%100 != 0));

  $mm = $mm + $advan_mm;
  while($mm > 59) {
  $mm -= 60;
  $hh++;
  }
  while($hh > 23) {
  $hh -= 24;
  $dd++;
  }
  while($dd > $mon_days{$mo+0}) {
  $mo++;
  while($mo > 12) {
  $mo -= 12;
  $yy++;
  }
  $dd = $dd - $mon_days{$mo+0};
  }

  while($mm < 0) {
  $mm += 60;
  $hh--;
  }
  while($hh < 0) {
  $hh += 24;
  $dd--;
  }
  if($dd < 1) {
  $mo--;
  while($mo < 1) {
  $mo += 12;
  $yy--;
  }
  $dd += $mon_days{$mo+0};
  }

  my $new_date = sprintf("%04d%02d%02d%02d%02d",$yy,$mo,$dd,$hh,$mm);
}

#-------------------------------------------------------------------------------
# Name: hh_advan_date
# Arguments: 1) a date as yyyymmddhh
#            2) number of hours as an integer
# Return: a date in 'yyyymmddhh'-form
# Description: advances given date in 1st argument by the number of hours given
#              in the second argument
#-------------------------------------------------------------------------------

sub hh_advan_date_bug
{
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
  while($mm < 1) {
  $mm += 12;
  $yy--;
  }
  $dd += $mon_days{$mm+0};
  }

  my $new_date = sprintf("%04d%02d%02d%02d",$yy,$mm,$dd,$hh);

  return $new_date;
}

1;

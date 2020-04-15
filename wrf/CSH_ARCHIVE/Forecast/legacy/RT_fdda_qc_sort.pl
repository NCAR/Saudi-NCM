#!/usr/bin/perl
#
# sort obs_input of rt-fdda mm5 according to date 
#

$f_in = $ARGV[0];
$f_out = $f_in."_s";
open (IN, "$f_in");
open (ME, ">$f_out");
my %tmp;
$ii = 1;
$| = 1;
while($aline = <IN>) {
  $kk = $aline;
  chomp($kk);
  $kk .= sprintf("%6d",$ii);
  $ii++;
  $tmp{$kk}=$aline;
  $ll = <IN>;
  $tmp{$kk} = join("",$tmp{$kk}, $ll);
  $stn = <IN>;
  $tmp{$kk} = join("",$tmp{$kk}, $stn);
  $aline = <IN>;
  $tmp{$kk} = join("",$tmp{$kk}, $aline);
  $sfc_flag = substr($aline, 51, 1);
  $level = substr($aline, 62, 3)+0;
  for ($i=0; $i < $level; $i++) {
   $aline = <IN>;
   $tmp{$kk} = join("",$tmp{$kk}, $aline);
  }
#  print $kk,"\n";
}

foreach $k (sort keys %tmp) {
 print ME $tmp{$k};
}

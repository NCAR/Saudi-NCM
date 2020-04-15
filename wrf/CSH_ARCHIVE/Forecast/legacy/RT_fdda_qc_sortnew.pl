#!/usr/bin/perl
#
# sort obs_input of rt-fdda mm5 according to date 
#

$f_in = $ARGV[0];
$f_out = $f_in."_s";
open (IN, "$f_in");
open (ME, ">$f_out");
my %tmp;
my $tmp;
$ii = 1;
$| = 1;
while($aline = <IN>) {
  $kk = $aline;
  chomp($kk);
  $kk .= sprintf("%6d",$ii);
# $ii++;   # comment out if remove redundancy
  $tmp=$aline;
  $ll = <IN>;
  $tmp = join("",$tmp, $ll);
  $stn = <IN>;
  $tmp = join("",$tmp, $stn);
  $aline = <IN>;
  $tmp = join("",$tmp, $aline);
  $fm = substr($aline, 0,30);
  $sfc_flag = substr($aline, 51, 1);
  $type=substr($aline, 1, 10);
  $level = substr($aline, 62, 3)+0;
  for ($i=0; $i < $level; $i++) {
   $aline = <IN>;
   $tmp = join("",$tmp, $aline);
  }
  $kk=$kk.$ll.$fm.$sfc_flag.$type;
  $tmp{$kk}=$tmp;
#  print $kk,"\n";
}

foreach $k (sort keys %tmp) {
  print $k,"\n";
 if($k =~ "PRETEN") {
  print ME $tmp{$k};
 }
}

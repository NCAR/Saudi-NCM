#!/usr/bin/perl

$nbytes=56;
$lstid=0;

foreach $arg (@ARGV) {
  if ($arg =~ /-add_stid/) {
     $nbytes=64;
     $lstid = 1;
     last;
  }
}

foreach $fn (@ARGV) {
next if ($fn =~ /add_stid/);
open(IN,"$fn");
seek(IN,0,0);
 while (read(IN,$buf,$nbytes) > 0) {
  seek(IN,0,1);
  if (lstid) {
     ($year,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
      $psfc_m,$psfc_o,$psfc_qc,
      $slp_m,$slp_o,$slp_qc,
      $ter_m,$ter_o,
      $t_m,$t_o,$t_qc,
      $q_m,$q_o,$q_qc,
      $ws_m,$ws_o,$ws_qc,
      $wd_m,$wd_o,$wd_qc,$st_id)=unpack("s6a4s20a8",$buf);
      $keyid=sprintf("%04d%04d%04d%8s%06d%06d%1d%15s",$year,$monthday,$hourmin,$st_id,$lat,$lon,$domain_id,$platform);
  } else {
     ($year,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
      $psfc_m,$psfc_o,$psfc_qc,
      $slp_m,$slp_o,$slp_qc,
      $ter_m,$ter_o,
      $t_m,$t_o,$t_qc,
      $q_m,$q_o,$q_qc,
      $ws_m,$ws_o,$ws_qc,
      $wd_m,$wd_o,$wd_qc)=unpack("s6a4s20",$buf);
      $keyid=sprintf("%04d%04d%04d%06d%06d%1d%15s",$year,$monthday,$hourmin,$lat,$lon,$domain_id,$platform);
  }
   $alldat{$keyid}=$buf;
  # print $keyid, "\n";
  # print "$year $monthday $hourmin $lat $lon $domain_id $platform $t_m $t_o $ws_m $ws_o\n";
 }
close(IN);
}

open(OUT,">$ARGV[0].out");
foreach $keyone (sort keys %alldat) {
  print OUT $alldat{$keyone};
}
close(OUT);
exit;

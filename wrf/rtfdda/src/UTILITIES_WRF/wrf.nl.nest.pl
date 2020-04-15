#!/usr/bin/perl 

open(IN, "wrf.nl");
open(OUT2,">namelist.input02");
open(OUT3,">namelist.input03");
open(OUT4,">namelist.input04");
open(OUT5,">namelist.input05");

while($bline=<IN>) {
 $aline=$bline;
 if($aline =~ "e_we" || $aline =~ "e_sn" || $aline =~ "dx " || $aline =~ "dy  ") {
  substr($aline,39,6)=substr($aline,46,6);
 }
 print OUT2 $aline;

 $aline=$bline;
 if($aline =~ "e_we" || $aline =~ "e_sn" || $aline =~ "dx " || $aline =~ "dy  ") {
  substr($aline,39,6)=substr($aline,52,8);
 }
 print OUT3 $aline;

 $aline=$bline;
 if($aline =~ "e_we" || $aline =~ "e_sn" || $aline =~ "dx " || $aline =~ "dy  ") {
  substr($aline,39,6)=substr($aline,59,6);
 }
 print OUT4 $aline;

 $aline=$bline;
 if($aline =~ "e_we" || $aline =~ "e_sn" || $aline =~ "dx " || $aline =~ "dy  ") {
  substr($aline,39,6)=substr($aline,66,6);
 }
 print OUT5 $aline;

}


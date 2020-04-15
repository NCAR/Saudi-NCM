#!/usr/bin/perl
#
$f_in = $ARGV[0]; 
$f_in = "all.obs" if( ! $f_in);
if ($f_in =~ /d0/){
    # qc_out_d01_ccyy-mm-dd-hh:00:00
    $f_out = substr($f_in,11,4).substr($f_in,16,2).substr($f_in,19,2).substr($f_in,22,2)
        ."_qc_obs_for_assimilation_s";
}else{
    # qc_out_ccyy-mm-dd-hh:00:00
  $f_out = substr($f_in,7,4).substr($f_in,12,2).substr($f_in,15,2).substr($f_in,18,2)
        ."_qc_obs_for_assimilation_s";
}
open (IN, "$f_in");
open (ME, ">$f_out");
$MISSING="-888888";
while($aline = <IN>) {
  $record_start = substr($aline,0,10);
  if ( $record_start == '          ') {
#   print $aline;
   $lat = sprintf("%9.2f",substr($aline,8,10));
   $lon = sprintf("%9.2f",substr($aline,28,10));
   $stid = substr($aline,40,5);
   $stna = substr($aline,45,75);
   $fm = substr($aline,120,18);
   $st_type = substr($aline,160,16);
   $elev = substr($aline,207,8);
   $nlev = substr($aline,226,4);
   $issnd = substr($aline,279,1);
   $isbug = substr($aline,289,1);
   $date = substr($aline,326,14);
   $seqid = substr($aline,256,4);
   $fm = "SAMS ATEC         " if($aline =~ "SAMS ATEC");
   $nlev="   1" if($issnd eq "F");
   print ME " $date\n";
   print ME "$lat $lon\n";
   print ME "  $stid   $stna\n";
   print ME "  $fm$st_type  $elev     $issnd     $isbug   $nlev\n";
   if($issnd eq "T") {
      for ($i=0; $i  < $nlev; $i++) {
       $aline = <IN>;
       substr($aline, 0,11)=$MISSING.".000" if(substr($aline,0,13) =~ "nan");
       substr($aline, 13,7)=$MISSING if(substr($aline,  0,11) < -80000.);
       substr($aline, 33,7)=$MISSING if(substr($aline, 20,11) < -80000.);
       substr($aline, 53,7)=$MISSING if(substr($aline, 40,11) < -80000.);
       substr($aline,133,7)=$MISSING if(substr($aline,120,11) < -80000.);
       substr($aline,153,7)=$MISSING if(substr($aline,140,11) < -80000.);
       substr($aline,173,7)=$MISSING if(substr($aline,160,11) < -80000.);
       print ME " ".substr($aline,  0,11)." ".substr($aline, 13,7).".000"
               ." ".substr($aline, 20,11)." ".substr($aline, 33,7).".000" 
               ." ".substr($aline, 40,11)." ".substr($aline, 53,7).".000" 
               ." ".substr($aline,120,11)." ".substr($aline,133,7).".000" 
               ." ".substr($aline,140,11)." ".substr($aline,153,7).".000" 
               ." ".substr($aline,160,11)." ".substr($aline,173,7).".000\n";
      }
   } else {
     $ref_p=" ".$MISSING.".000 ".$MISSING.".000";
     $preci=" ".$MISSING.".000 ".$MISSING.".000";
     $ref_h="       0.000"." "."      0.000";
     $bline=<IN>;
     print $bline if ($bline =~ "nan");
       substr($bline, 0,11)=$MISSING.".000" if(substr($bline,0,13) =~ "nan");
       substr($bline, 53,7)=$MISSING if(substr($bline, 40,11) < -80000.);
       substr($bline,133,7)=$MISSING if(substr($bline,120,11) < -80000.);
       substr($bline,153,7)=$MISSING if(substr($bline,140,11) < -80000.);
       substr($bline,173,7)=$MISSING if(substr($bline,160,11) < -80000.);
       substr($aline,353,7)=$MISSING if(substr($aline,340,11) < -80000.);
       $slp=" ".substr($aline,340,11)." ".substr($aline,353,7).".000";

       print ME $slp.$ref_p.$ref_h
               ." ".substr($bline, 40,11)." ".substr($bline, 53,7).".000"
               ." ".substr($bline,120,11)." ".substr($bline,133,7).".000" 
               ." ".substr($bline,140,11)." ".substr($bline,153,7).".000" 
               ." ".substr($bline,160,11)." ".substr($bline,173,7).".000" 
               ." ".substr($bline,  0,11)." ".substr($bline, 13,7).".000" 
               .$preci."\n";
   }
  }
}
close(IN);close(ME);

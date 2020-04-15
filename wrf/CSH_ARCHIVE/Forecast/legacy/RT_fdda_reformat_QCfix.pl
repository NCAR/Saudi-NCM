#!/usr/bin/perl
#------------------------------------------------------------------------------#
# This script reformat the observations at the MM5 little_r format, see 6.12 of
# http://www.mmm.ucar.edu/mm5/documents/MM5_tut_Web_notes/OA/OA.htm
# into the input format of MM5/WRF FDDA. 
# Typically observations are QCed before being reformatted. This QC
# is a value between 0 (worst) and 10 (best).  A functionality to threshold 
# the QC values have been added. The variable threshold between 0 and 10
# below will set the threshold for the QC values:
# - if threshold >= 0, all QC values lower or equal than threshold will be set 
#   to 0, QC values strictly greater than threshold will be set to 10. 
# - Setting threshold_qc < 0 will result in not thresholding QC values. 
#------------------------------------------------------------------------------#
# Copyright UCAR (c) 2007.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Laboratory (RAL),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#------------------------------------------------------------------------------#

$threshold = 0;

# Input file (output of QC program)
$f_in = $ARGV[0]; 
$f_in = "all.obs" if( ! $f_in);

# Output file (input of MM5 program)
$f_out = substr($f_in,7,4).substr($f_in,12,2).substr($f_in,15,2).substr($f_in,18,2)
        ."_qc_obs_for_assimilation_s";
print "Reformatting Qced data file for FDDA\n";
print "Opening file for read  $f_in\n";
open (IN, "$f_in");
print "Opening file for write $f_out\n";
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

# Remove nan for pressure
       substr($aline, 0,11)=$MISSING.".000" if(substr($aline,0,13) =~ "nan");

# Fill with missing flag negative QC
       substr($aline, 13,7)=$MISSING if(substr($aline,  0,11) < -80000.);
       substr($aline, 33,7)=$MISSING if(substr($aline, 20,11) < -80000.);
       substr($aline, 53,7)=$MISSING if(substr($aline, 40,11) < -80000.);
       substr($aline,133,7)=$MISSING if(substr($aline,120,11) < -80000.);
       substr($aline,153,7)=$MISSING if(substr($aline,140,11) < -80000.);
       substr($aline,173,7)=$MISSING if(substr($aline,160,11) < -80000.);

# Thresholding QC
       if ($threshold >= 0) {
           $aline = &threshold_qc ($aline,$threshold,$MISSING);
       }

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

       if ($threshold >= 0) {
           $bline = &threshold_qc ($bline,$threshold,$MISSING);
       }

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

sub threshold_qc {

       use strict;

       my ($aline,$threshold,$MISSING) = @_;

# Pressure QC can be 100010
       if (substr($aline,  13,7) != $MISSING){
           if (substr($aline,  18,2) <= $threshold){  # Pressure QC
               substr($aline,  13,7) =  "      0"; 
           }else{
               substr($aline,  13,7) =  "     10"; 
           }
       }

# Height QC can be 400000
       if (substr($aline,  33,7) != $MISSING){
           if (substr($aline,  38,2) <= $threshold){ # Height QC
               substr($aline,  33,7) = "      0"; 
           }else{
               substr($aline,  33,7) = "     10";
           }
       }

       print substr($aline,  53,7);
       if (substr($aline,  53,7) != $MISSING){
           if (substr($aline,  53,7) <= $threshold){ # Temperature QC
               substr($aline,  53,7) = "      0";
           }else{
               substr($aline,  53,7) = "     10";
           }
       }
       print "\n";
       print substr($aline,  53,7);
       print "\n";

       if (substr($aline,  73,7) != $MISSING) {
           if (substr($aline,  73,7) <= $threshold){ # Dew point QC
               substr($aline,  73,7) = "      0";
           }else{
               substr($aline,  73,7) = "     10";
           }
       }

       if (substr($aline,  93,7) != $MISSING){
           if (substr($aline,  93,7) <= $threshold){ # Wind speed QC
               substr($aline,  93,7) = "      0";
           }else{
               substr($aline,  93,7) = "     10";
           }
       }

       if (substr($aline, 113,7) != $MISSING){
           if (substr($aline, 113,7) <= $threshold){ # Wind direction QC
               substr($aline, 113,7) = "      0";
           }else{
               substr($aline, 113,7) = "     10";
           }
       }

       if (substr($aline, 133,7) != $MISSING){
           if (substr($aline, 133,7) <= $threshold){ # Wind U QC
               substr($aline, 133,7) = "      0";
           }else{
               substr($aline, 133,7) = "     10";
           }
       }

       if (substr($aline, 153,7) != $MISSING){
           if (substr($aline, 153,7) <= $threshold){ # Wind V QC
               substr($aline, 153,7) = "      0";
           }else{
               substr($aline, 153,7) = "     10";
           }
       }

       if (substr($aline, 173,7) != $MISSING){
           if (substr($aline, 173,7) <= $threshold){ # RH QC
               substr($aline, 173,7) = "      0";
           }else{
               substr($aline, 173,7) = "     10";
           }
       }

       if (substr($aline, 193,7) != $MISSING){
           if (substr($aline, 193,6) <= $threshold){ # Thickness QC
               substr($aline, 193,6) = "      0";
           }else{
               substr($aline, 193,6) = "     10";
           }
       }

       return ($aline);
}

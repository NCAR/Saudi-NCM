#!/usr/bin/perl
#         Filter Observation files based on lat lon bounding boxes. 
#
# RT_all.obs_trim.domain.pl
# Usage: RT_all.obs_trim.domain.pl Input_File [LATLON_FILE]
# Lat lon file is 2 lines (space delimited);
#    latmin latmax
#    lonmin lonmax
#
# Produces sorted output in: InputFile.trim


# Grab first two arguments
$f_in = $ARGV[0]; 
$f_in = "all.obs" if( ! $f_in);
$f_out = "$f_in.trim";
$LATLON_FILE=$ARGV[1];
$LATLON_FILE="jjunk_ddummy" if(! $ARGV[1]);

# Unspecified logic variable
$RM_DUB=1;

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
open (ME, ">$f_out");
$flag = 0;
my %tmp;

# Process each line in the input file.
while($aline = <IN>) {
 $record_start = substr($aline,0,10);
  if ( $record_start == '          ') {
   $lat = substr($aline,10,10);
   $lon = substr($aline,30,10);

# Check valididy
   if ($lat < $latmax && $lat > $latmin && $lon < $lonmax && $lon > $lonmin
#   if (1
#  if ($lat < 60 && $lat > 15 && $lon < -55 && $lon > -155  # cover USA
#  if ($lat < 50 && $lat > 25 && $lon < -55 && $lon > -95   #atc
#  if ($lat < 45 && $lat > 5  && $lon < -110 && $lon > -179  # cover Hawaii  
#  && $aline =~ /METAR/ 
#  && $aline =~ /19980209/
#  && $aline =~ /PILOT/
    ) {
   # Process line
   $k1 = $lat;
   $k2 = $lon;
   $k3 = substr($aline,209,80);
   $k4 = substr($aline,326,12);
   $k5 = substr($aline,40,5);
   $k6 = substr($aline,80,12);
   $k7 = substr($aline,120,12);
   
   # Build an ordering key
   $kk = $k4." ".$k1." ".$k2." ".$k5." ".$k6." ".$k7." ".$k3;
#       $kk = $aline;

# Unspecified logic
   if($RM_DUB == 1) {
#   Use the Original Line.
     $tmp{$kk} = $aline;  #late obs is retained
   } else {
# Append the current line
     $tmp{$kk} = join("",$tmp{$kk}, $aline);
   }
   $flag = 1;
#   $flag = 0 if ($tmp{$kk} NE "");
   }
   else {
   $flag = 0;
   }
 }
else {
# print ME $aline if ($flag == 1);
 $tmp{$kk} = join("",$tmp{$kk}, $aline) if ($flag == 1);
}
}

# Sort and output the Obs array
foreach $k (sort keys %tmp) {
 print ME $tmp{$k};
}

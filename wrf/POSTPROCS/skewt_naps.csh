#!/bin/csh
#      
#
   setenv NCARG_ROOT /opt/ncarg

   if(-e fort.29) rm fort.29
   set SndData = $1
   ln  -s  $SndData       fort.29

/data/fddahome/cycle_code/EXECUTABLE_ARCHIVE/skewt
rm fort.29

echo 'SkewT is plotted'


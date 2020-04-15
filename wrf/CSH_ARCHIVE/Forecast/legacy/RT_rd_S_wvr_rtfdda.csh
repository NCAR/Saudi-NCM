#!/bin/csh -f

##This shell decodes the data from Water Vapor Radiometers (*.prf files)
## at locations specified in the "LOCATIONS" variables.
###############################################################################
echo  " ----------------------------------------------------------------------"
echo  " ---------------- WVR decoder starts  ------------------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

set debug = 0

if ( ${#argv} != 2 ) then
	echo "usage: $0 start_date obs_date"
	echo "where start_date is CCYYMMDDHH"
	echo  "d obs_date is CCYYMMDDHH"
	exit ( 4 )
endif

## Current location of Radiometers

set LOCATIONS = "lamont  boulder"

## Decode for each location

foreach l ($LOCATIONS)
  $RT_S_rd_wvr_location.csh $1 $2 $l
endif

exit (0)

#!/bin/tcsh -f
##------------------------------------------------------------------------------
## This shell decodes data files from SODAR at locations specified in the 
## "LOCATIONS" variables.
##
## Copyright UCAR (c) 1992 - 2004.
## University Corporation for Atmospheric Research (UCAR),
## National Center for Atmospheric Research (NCAR),
## Research Applications Program (RAP),
## P.O.Box 3000, Boulder, Colorado, 80307-3000, USA. 
##
## Francois Vandenberghe, vandenb@ucar.edu, April 2004.
##------------------------------------------------------------------------------

echo
echo  " ----------------------------------------------------------------------"
echo   " ---------------- SODAR decoder starts --------------------------------"
echo "$0 $argv[*]"
echo   " ----------------------------------------------------------------------"

if ( ${#argv} != 2 ) then
	echo "usage: $0 start_date obs_date"
	echo "where start_date is CCYYMMDDHH"
	echo  "d obs_date is CCYYMMDDHH"
	exit ( 4 )
endif

## Current location of Radiometers

set LOCATIONS = "Pentagon"

## Decode for each location

foreach l ($LOCATIONS)
    tcsh -f $CSH_ARCHIVE/Forecast/RT_S_rd_sodar_location.csh $1 $2 $l
endif

exit (0)

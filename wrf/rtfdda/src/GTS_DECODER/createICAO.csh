#!/bin/csh

###################################################################
# Original script notes below. Script was split into two
# scripts (one for GTS and one for ICAO) in June, 2017 by
# Jaimi Yee
####################################################################
# Process WMO Pub9 station location files and ICAO observation 
#   location lists
# New WMO files need to be merged with previous ones, new stations
#   are added, locations and station elevations are updated
# A new WMO Pub9 and ICAO_STATION_LOCATION files need to be
#   downloaded; the loop below will process new files one at a
#   time
# Script also generates global and North American maps with 
#   new, moved, and changed elevation stations
# Makefile needs to be run first to generate required executables
#
# Andrea Hahmann, June 2006
# hahmann@ucar.edu
# Copyright UCAR [RAP] 1996 - 2006. All Rights Reserved.
#
####################################################################
# Modifications by Jaimi Yee, April 2017
#
# $Id: createICAO.csh,v 1.1 2017/07/05 23:58:06 jaimi Exp $
####################################################################

#-------------------------------------------------------------------
# Do cleanup if requested
#-------------------------------------------------------------------
if ($#argv > 0) goto CLEAN

#------------------------------------------------------------
# Create ICAO station list
#------------------------------------------------------------
ICAO:

#
# Process ICAO table of obs
#   Input: ICAO_STATION_LOCATION
#   Output: gts_sttnid_input.icao.txt
#           gts_sttnid_final.icao (direct access file)
#
sid_icao.exe

#
# Make a copy of the input ICAO file with today's date and then
# save a copy
#
set today = `date +%y%m%d`
cp ICAO_STATION_LOCATION ICAO_STATION_LOCATION.${today}
mv ICAO_STATION_LOCATION new_ICAO_STATION_LOCATION

#
# Merge the ICAO files. UpdateICAO takes two files on input.
#
set ICAO_files = `ls ICAO*`
echo $ICAO_files
echo "UpdateICAO $ICAO_files"
UpdateICAO $ICAO_files

#
# Create plots
#
set date = `echo $ICAO_files[2] | cut -b 23-30`
echo $date
setenv DATE NOW
setenv ODATE $date
setenv WORLD T
ncl PlotStationsICAO.ncl
mv Stations.ps Stations_ICAO_${today}.ps

#
# Move saved copy of the original ICAO station file back to
# the original name.
mv new_ICAO_STATION_LOCATION ICAO_STATION_LOCATION
exit 2

#
# Clean up directories
#
CLEAN:
if ($#argv > 0) then
  if ("$argv[1]" == "clean") then
    rm -f Pub9-volA.format-info
    rm -f gts_sttnid_input.wmo_?.txt
    rm -f gts_sttnid_input.wmo.txt
    rm -f gts_sttnid_cmpct.wmo
    rm -f gts_sttnid_final.wmo
    rm -f gts_sttnid_input.wmo
    rm -f gts_sttnid_input.wmo.cc 
    rm -f *.dat
    rm -f *.ps
  else
    echo $0\: Do not understand argument
    exit 2
  endif
endif

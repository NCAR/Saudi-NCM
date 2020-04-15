#!/bin/csh
# ------------------------------------------------------------------
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
# ------------------------------------------------------------------

if ($#argv > 0) goto CLEAN
##goto ICAO
## Create intermediate files with old Pub9 format
#  Old file format for files before 2000

rm -f Pub9-volA.flatfile
cat Pub9volA9*.flatfile > Pub9-volA.flatfile
ln -s Pub9volA9605.codetabl Pub9-volA.format-info
set old_date = 960501 
if (-e UpdateGTS.log) rm -f UpdateGTS.log
sid_wmo.csh
sid_wmo.exe >> UpdateGTS.log
sort gts_sttnid_input.wmo.txt > gts_sttnid_input.wmo_old.txt

## Create intermediate files with new Pub9 format: 

rm -f Pub9-volA.flatfile
@ file = 0
foreach flatfile (Pub9volA??????.flatfile)
    @ file ++
    set date = `echo $flatfile | cut -b 9-14`
    echo "Processing file $file $flatfile for date: $date"
    cut -f1,3,6,8,9,10,11-14 $flatfile | sid_wmo.pl
    sort gts_sttnid_input.wmo.txt > gts_sttnid_input.wmo_${file}.txt

## Use previously generated file if it exits
    if (-e gts_sttnid_input.wmo_old.txt) mv gts_sttnid_input.wmo_old.txt gts_sttnid_input.wmo_0.txt
    @ ofile = $file - 1

## Generate new file with merged stations; new file is "gts_sttnid_input.wmo.txt"  
    UpdateGTS gts_sttnid_input.wmo_${ofile}.txt gts_sttnid_input.wmo_${file}.txt >> UpdateGTS.log
    cp gts_sttnid_input.wmo.txt gts_sttnid_input.wmo_${file}.txt

## Set up environmental variables for NCL plots
    setenv DATE $date
    setenv ODATE $old_date

    setenv WORLD T
    ncl PlotStations.ncl
    mv Stations.ps Stations_World_${date}.ps

    setenv WORLD F
    ncl PlotStations.ncl
    mv Stations.ps Stations_NA_${date}.ps

    set old_date = $date
    set old_flatfile = $flatfile
end

write_stnid_into_bn.exe
mv gts_sttnid_final.wmo gts_sttnid_final

ICAO:
# Process ICAO table of obs
# Need the current ICAO list file named 'ICAO_STATION_LOCATION'
sid_icao.exe

set today = `date +%y%m%d`
cp ICAO_STATION_LOCATION ICAO_STATION_LOCATION.${today}
mv ICAO_STATION_LOCATION new_ICAO_STATION_LOCATION
set ICAO_files = `ls ICAO*`
echo $ICAO_files
echo "UpdateICAO $ICAO_files"
UpdateICAO $ICAO_files
set date = `echo $ICAO_files[2] | cut -b 23-30`
echo $date
setenv DATE NOW
setenv ODATE $date
setenv WORLD T
ncl PlotStationsICAO.ncl
mv Stations.ps Stations_ICAO_${today}.ps

mv new_ICAO_STATION_LOCATION ICAO_STATION_LOCATION
exit 2

## Clean up directories
CLEAN:
if ($#argv > 0) then
  if ("$argv[1]" == "clean") then
    rm -f gts_sttnid_input.wmo_?.txt
    rm -f *.dat
    rm -f *.ps
  else
    echo $0\: Do not understand argument
    exit 2
  endif
endif

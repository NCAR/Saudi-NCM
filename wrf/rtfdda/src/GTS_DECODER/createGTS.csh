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
# $Id: createGTS.csh,v 1.1 2017/07/05 23:58:06 jaimi Exp $
####################################################################

#-------------------------------------------------------------------
# Do cleanup if requested
#-------------------------------------------------------------------
if ($#argv > 0) goto CLEAN

#-------------------------------------------------------------------
# Create intermediate files with old Pub9 format
#  (Old file format for files before 2000)
#
#  The goal is to create a file called gts_sttnid_input.wmo_old.txt 
#  which is a full list of stations from all the files with the old
#  style format. 
#-------------------------------------------------------------------

#
# Remove any existing files that we will reuse
#
rm -f Pub9-volA.flatfile
if (-e UpdateGTS.log) rm -f UpdateGTS.log

#
#  Concatinate all the old style flatfiles into
#  a single flatfile
#
cat Pub9volA9*.flatfile > Pub9-volA.flatfile

#
# Call another script now:
#   This script takes the old Pub9-volA format information and
#   automatically edits it to create a list of country codes and 
#   matching countries, which it puts into a file called 
#   gts_sttnid_input.wmo.cc. Note that the .cc extension is a bit
#   misleading - it does not appear to be C++ code.
#
#   The script also pulls out any lines with a '02' in them from
#   the Pub9-volA.flatfile we created above and puts them into
#   a file called gts_sttnid_input.wmo 
#
ln -s Pub9volA9605.codetabl Pub9-volA.format-info
set old_date = 960501
sid_wmo.csh

#
# This code is built from sid_wmo.f90. It takes 
# gts_sttnid_input.wmo and gts_sttnid_input.wmo.cc as input
# and creates gts_sttnid_input.wmo.txt, gts_sttnid_cmpct.wmo
# and gts_sttnid_final.wmo as output.
#
# Note that the only one of these files that actually
# gets used in the form it is now is gts_sttnid_input.wmo.txt.
# These leads one to believe that much of this software was
# written for the old format and not updated with the new
# format. The newly formatted files are handled differently.
#
# Now we have our gts_sttnid_input.wmo_old.txt file with
# a list of all the stations in the old style flatfiles.
#
sid_wmo.exe >> UpdateGTS.log
sort gts_sttnid_input.wmo.txt > gts_sttnid_input.wmo_old.txt

#-------------------------------------------------------------
# Now merge the old format station id's with the newer format
# station id's.
#------------------------------------------------------------

#
# Create intermediate files with new Pub9 format: 
#
#   Note that we moved gts_sttnid_input.wmo.txt to a different file
#   above. That file name is used again by the sid_wmo.pl script. The
#   sid_wmo.pl script takes certain fields from the given flatfile
#   and processes them and writes them to the gts_sttnid_input.wmo.txt
#   file. This file then gets written to gts_sttnid_input.wmo_${file}.txt
#   where ${file} gives us a file index number. 
#
#   Note also that this is where we ran into problems with our
#   new file. The file name format was of this form Pub9volA???????.flatfile
#   rather than Pub9volA??????.flatfile (7 ?'s instead of 6). This means
#   that without a file name change, the file won't get into this section
#   and doesn't get written to gts_sttnid_inpu.wmo.txt.
#

#
# Prepare for loop
#
rm -f Pub9-volA.flatfile
if (-e gts_sttnid_input.wmo_old.txt) mv gts_sttnid_input.wmo_old.txt gts_sttnid_input.wmo_0.txt
set old_date = 960501 
@ file = 0

#
# Loop through the flatfiles
#
foreach flatfile (Pub9volA??????.flatfile)

    #
    # Increment file counter
    #
    @ file ++

    #
    # Set the date and tell the user what we are doing
    #
    set date = `echo $flatfile | cut -b 9-14`
    echo "Processing file $file $flatfile for date: $date"

    #
    # Send certain fields from flatfile to sid_wmo.pl, which
    # creates gts_sttnid_input.wmo.txt. We then put that in
    # a new file with a file counter on it.
    #
    cut -f1,3,6,8,9,10,11-14 $flatfile | sid_wmo.pl
    sort gts_sttnid_input.wmo.txt > gts_sttnid_input.wmo_${file}.txt

    # 
    # Set a counter for the previous file
    #
    @ ofile = $file - 1

    #
    # Generate new file with merged stations; 
    #
    #    UpdateGTS is an executable that we build as part of this
    #    Makefile. It is created from UpdateGTS.f90. It appears to
    #    take two input files and merges them into one file called
    #    gts_sttnid_input.wmo.txt. It will also remove duplicates
    #    and so on. The idea here is that we are incrementally merging
    #    all of these files into one. The first time through, we are using
    #    gts_sttnid_input.wmo_old.txt as our first file and merging that
    #    with the first one of the files we just processed. We then
    #    create a new file called gts_sttnid_input.wmo_1.txt (which
    #    overwrites the file we created above using sid_wmo.pl). The
    #    next time through we merge file 1 with the new file 2 created
    #    with sid_wmo.pl above. We continue in this fashion all through
    #    our list of flatfiles. The end result is a text file.
    #
    #    Note that every time we run UpdateGTS we create a merged file
    #    called gts_sttnid_input.wmo.txt which is then copied into the
    #    gts_sttnid_input.wmo_${file}.txt, which is then used on the
    #    next pass at merging files. At the end though, the 
    #    gts_sttnid_input.wmo.txt file will be the complete merged file.
    #    (And will have a copy called gts_sttnid_input.wmo_${file}.txt.)
    #
    UpdateGTS gts_sttnid_input.wmo_${ofile}.txt gts_sttnid_input.wmo_${file}.txt >> UpdateGTS.log
    \cp gts_sttnid_input.wmo.txt gts_sttnid_input.wmo_${file}.txt

    #
    # Set up environmental variables for NCL plots
    #
    setenv DATE $date
    setenv ODATE $old_date
    setenv WORLD T

    #
    # Create NCL plots
    #
    ncl PlotStations.ncl
    mv Stations.ps Stations_World_${date}.ps

    setenv WORLD F
    ncl PlotStations.ncl
    mv Stations.ps Stations_NA_${date}.ps

    #
    # Get ready for next pass
    #
    set old_date = $date
    set old_flatfile = $flatfile
end

foreach flatfile (Pub9volA??????x.flatfile)

    #
    # Increment file counter
    #
    @ file ++

    #
    # Set the date and tell the user what we are doing
    #
    set date = `echo $flatfile | cut -b 9-14`
    echo "Processing file $file $flatfile for date: $date"

    #
    # Send certain fields from flatfile to sid_wmo.pl, which
    # creates gts_sttnid_input.wmo.txt. We then put that in
    # a new file with a file counter on it.
    #
    cut -f1,3,6,8,9,10,11-14 $flatfile | sid_wmo_x.pl
    sort gts_sttnid_input.wmo.txt > gts_sttnid_input.wmo_${file}.txt

    # 
    # Set a counter for the previous file
    #
    @ ofile = $file - 1

    #
    # Generate new file with merged stations; 
    #
    #    UpdateGTS is an executable that we build as part of this
    #    Makefile. It is created from UpdateGTS.f90. It appears to
    #    take two input files and merges them into one file called
    #    gts_sttnid_input.wmo.txt. It will also remove duplicates
    #    and so on. The idea here is that we are incrementally merging
    #    all of these files into one. The first time through, we are using
    #    gts_sttnid_input.wmo_old.txt as our first file and merging that
    #    with the first one of the files we just processed. We then
    #    create a new file called gts_sttnid_input.wmo_1.txt (which
    #    overwrites the file we created above using sid_wmo.pl). The
    #    next time through we merge file 1 with the new file 2 created
    #    with sid_wmo.pl above. We continue in this fashion all through
    #    our list of flatfiles. The end result is a text file.
    #
    #    Note that every time we run UpdateGTS we create a merged file
    #    called gts_sttnid_input.wmo.txt which is then copied into the
    #    gts_sttnid_input.wmo_${file}.txt, which is then used on the
    #    next pass at merging files. At the end though, the 
    #    gts_sttnid_input.wmo.txt file will be the complete merged file.
    #    (And will have a copy called gts_sttnid_input.wmo_${file}.txt.)
    #
    UpdateGTS gts_sttnid_input.wmo_${ofile}.txt gts_sttnid_input.wmo_${file}.txt >> UpdateGTS.log
    \cp gts_sttnid_input.wmo.txt gts_sttnid_input.wmo_${file}.txt

    #
    # Set up environmental variables for NCL plots
    #
    setenv DATE $date
    setenv ODATE $old_date
    setenv WORLD T

    #
    # Create NCL plots
    #
    ncl PlotStations.ncl
    mv Stations.ps Stations_World_${date}.ps

    setenv WORLD F
    ncl PlotStations.ncl
    mv Stations.ps Stations_NA_${date}.ps

    #
    # Get ready for next pass
    #
    set old_date = $date
    set old_flatfile = $flatfile
end

#-------------------------------------------------------------
# Create final gts station list
#-------------------------------------------------------------
    
#
# Write the final text file (gts_sttnid_input.wmo.txt) to
# a binary file (gts_sttnid_final.wmo). Then move this file
# to a new file name (gts_sttnid_final).
#
write_stnid_into_bn.exe
mv gts_sttnid_final.wmo gts_sttnid_final

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

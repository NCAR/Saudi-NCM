#! /usr/bin/perl -s
#------------------------------------------------------------------------------#
#
# Script to write MADIS NETCDF files into ASCII "decoded" format, 
# ie the input format for observations of MM5 RAWINS, LITTLE_R and 3D-VAR.
# 
# Usage: 
# ------
#  madis_decoder.pl: Decode MADIS data valid at current UTC time 
#                    (rounded to the closest past hour)
#
#  madis_decoder.pl lag 1: Decode MADIS data valid at current UTC time minus 1hr
#                          (rounded to the closest past hour) 
#
#  madis_decoder.pl CCYYMMDDHH: Decode MADIS data valid at current CCYYMMDDHH
#                              (lag does not apply)  
#
# The variable fsl_analysis_type in the namelist shall be set to 'FDDA' or 
# '3D-VAR' depending of the analysis system that will be used.
#
# Enter: "madis_decoder.pl help" for help.
# -----
#
# The script assume that the MADIS NETCDF files have previously been ftped
# and copied into the directory defined by the MADIS_DATA variable.
# 
# The decoded data are copied to the path defined by variable DECODIR
#
# Data arrive arrive on the MADIS server on a continuous, asynchronous schedule,# and the current and previous hour's data are processed every 5 minutes. 
# The data are segmented into hourly files, with the mesonet file for hour HH 
# containing data for HH00 through HH59. In order to get regularly-scheduled 
# hourly reports from the national data sources (METAR, SAO, and maritime) 
# these files contain data starting 15 minutes before the hour and ending 
# 44 minutes after the hour (e.g., the 0000 file covers 2345 - 0044). 
# The most complete data for a given hour is available a little after 2 hours 
# following the file time. The user should also understand that the mesonet 
# data isn't as timely as the national data sources. The lag time of these 
# reports (lag = time available from FSL - observation time) ranges from about 
# 8 to 45 minutes, and can sometimes be longer.
#
# See http://www-sdd.fsl.noaa.gov/MADIS/index.html for more details
#
#------------------------------------------------------------------------------!
# Copyright UCAR (c) 1992 - 2004.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Program (RAP),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
# 
# Francois Vandenberghe, vandenb@ucar.edu, April 2004.
#------------------------------------------------------------------------------!
use Time::Local;

# Local directory where to copy the MADIS data after decoding

$DECODIR     = "/scratch/project/k1206/datainput/madis_decoded";
$DECODIR_TMP = "/scratch/project/k1206/datainput/.madis_decoded";

# Directory where MADIS raw data reside and path to MADIS library static files.

$ENV{MADIS_DATA}   = "/scratch/project/k1206/datainput/madis-input-nc";
$ENV{MADIS_STATIC} = "/usr/local/madis-3.9/static";

# Directory where the executable madis_decoder.exe reside

$EXECUTABLE_ARCHIVE = "/home/x_fisherh/datbin/ingest/decoder/EXECUTABLE_ARCHIVE";

# Directory where the namelist file namelist.madise resides

$CONSTANT_FILES = "$EXECUTABLE_ARCHIVE/../CONSTANT_FILES";

# Use current UTC time minus as default

($ss , $mm , $th , $td , $tm , $ty , $wday , $yday , $isdst  ) = gmtime(time());

# Parse arguments

if ( $#ARGV >=  0 ) { # Time passed as argument

$i = 0;
$DEBUG =   "";

while($i <= $#ARGV) {

 if ($ARGV[$i] eq  "debug" ) { # debug option
     $DEBUG =   "-debug";
 }elsif ($ARGV[$i] eq  "help" ){
     &help;
 }elsif ($ARGV[$i] eq  "lag" ){
     $i += 1;
     $lag = $ARGV[$i];
    ($ss , $mm , $th , $td , $tm , $ty , $wday , $yday , $isdst  ) = gmtime(time()-$lag*3600);
 }else{
     $ccyymmddhh = @ARGV[$i];
     if ($ccyymmddhh < 1000000000 || $ccyymmddhh > 9999999999) {
	 &help;
     } 
     $ty = int ($ccyymmddhh / 1000000);
     $mmddhh = $ccyymmddhh % 1000000;
     $tm = int ($mmddhh / 10000);
     $td = int (($mmddhh % 10000) / 100);
     $th = $mmddhh % 100;
     $mn = "00";
     $ss = "00";
     $ty = $ty - 1900;
     $tm = $tm - 1;
     last;
   }

  $i++
 }


} 

# Analysis time.

$data_hour  = sprintf("%04d%02d%02d%02d", $ty+1900, $tm+1, $td, $th);

# Decode
&madis_decoder ($data_hour, $DECODIR_TMP, $DECODIR, $CONSTANT_FILES, $DEBUG, $EXECUTABLE_ARCHIVE, $MADIS_DATA, $MADIS_STATIC);

#------------------------------------------------------------------------------#
# Routines

sub madis_decoder {

# Arguments

my ($data_hour, $DECODIR_TMP, $DECODIR, $CONSTANT_FILES, $DEBUG, $EXECUTABLE_ARCHIVE, $MADIS_DATA, $MADIS_STATIC) = @_;

# Decode the data

print &ptime, "-------------------------------------------------------------\n";
print &ptime, "Decoding MADIS data for $data_hour\n";

# Use a random sub-directory as work space

$subdir = "tmp$$";

`mkdir -p $DECODIR_TMP/$subdir` if (! -d "$DECODIR_TMP/$subdir");

if (! (chdir "$DECODIR_TMP/$subdir")) {
    print &ptime, "Error: Can't cd to $DECODIR_TMP/$subdir $!\n";
    print &ptime, "Ending: exit status = -1.\n";
    exit -1;
}

# Bring the namelist:

system("cp -p $CONSTANT_FILES/namelist.madis .");

# Set the environment variables

    print &ptime,"madis_decoder.exe $DEBUG -date $data_hour\n";
    system("$EXECUTABLE_ARCHIVE/madis_decoder.exe $DEBUG -date $data_hour");

# Scrub raw data files

#`find $MADIS_DATA -name $data_date -exec rm \{\} \\; ` if ($DEBUG ne "-debug");


# Scrub intermediate files

    `rm -f namelist.madis` if ($DEBUG ne "-debug");

# Check the results


    if (! -e "madis_${data_hour}.decoded") {
        print &ptime, "Error in madis_decoder.exe!\n";
	print &ptime, "Check log file\n";
print &ptime, "-------------------------------------------------------------\n";
        print &ptime,"decode_madis.pl ending with exit status = 1\n";
        system ("rm -rf $DECODIR_TMP/$subdir")  if (-d "$DECODIR_TMP/$subdir" && $DEBUG ne "-debug");
	exit -1;
    }
    if ( -z  "madis_${data_hour}.decoded") {
       `rm -f madis_${data_hour}.decoded` if ($DEBUG ne "-debug") ;
        system ("rm -rf $DECODIR_TMP/$subdir")  if (-d "$DECODIR_TMP/$subdir" && $DEBUG ne "-debug");
        print &ptime, "Empty output file removed!\n";
        print &ptime, "Check log file.\n";
        print &ptime, "-------------------------------------------------------------\n";
        print &ptime,"decode_madis.pl ending with exit status = 1\n";
        exit -1;
    }

    `gzip -f madis_${data_hour}.decoded`;

     if (! -e "madis_${data_hour}.decoded.gz") {
         print &ptime,"Problem compressing file $DECODIR_TMP/$subdir/madis_${data_hour}.decoded\n";
        system ("rm -rf $DECODIR_TMP/$subdir")  if (-d "$DECODIR_TMP/$subdir" && $DEBUG ne "-debug");
         exit -1;
     }

    ($dev_tmp,$ino_tmp,$mode_tmp,$nlink_tmp,$uid_tmp,$gid_tmp,$rdev_tmp,$size_tmp,$atime_tmp,$mtime_tmp,$ctime_tmp,$blksize_tmp,$blocks) = stat ("$DECODIR_TMP/$subdir/madis_${data_hour}.decoded.gz");

     $size_tmp_Mb = int ($size_tmp/1000000);

     print &ptime,"Created file $DECODIR_TMP/$subdir/madis_${data_hour}.decoded.gz with size ${size_tmp_Mb}Mb\n";

#------------------------------------------------------------------------------#
# Moving file to final directory

    `mkdir -p $DECODIR`  if (! -d $DECODIR);

     if (! -d $DECODIR){
         print &ptime,"Moving file $DECODIR_TMP/$subdir/madis_${data_hour}.decoded.gz   into $DECODIR\n";
         print &ptime,"Problem creating directory $DECODIR\n";
         system ("rm -rf $DECODIR_TMP/$subdir")  if (-d "$DECODIR_TMP/$subdir" && $DEBUG ne "-debug");
         exit -1;
     }

# Check file size

     $size = 0;
     $ctime = "unknown";

     if (-e "$DECODIR/madis_${data_hour}.decoded.gz"){
        ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = stat ("$DECODIR/madis_${data_hour}.decoded.gz");
     }
  
     print &ptime,"New file $DECODIR_TMP/$subdir/madis_${data_hour}.decoded.gz\n";
     print &ptime,"was created at ${ctime_tmp}(UNIX time) with size ${size_tmp}c\n";
     print &ptime,"Old file $DECODIR/madis_${data_hour}.decoded.gz\n";
     print &ptime,"was created at ${ctime}(UNIX time) with size ${size}c\n";

# Copy only if new file is bigger than existing one

     if ($size >= $size_tmp){
         print &ptime,"New file will not be copied to $DECODIR\n";
         system ("rm -rf $DECODIR_TMP/$subdir")  if (-d "$DECODIR_TMP/$subdir" && $DEBUG ne "-debug");
         exit -1;
     }
         
# Look for lock on the file before moving

     if (-e "$DECODIR_TMP/madis_${data_hour}.decoded.gz.lock"){
         print &ptime,"Found lock file $DECODIR_TMP/madis_${data_hour}.decoded.gz.lock\n";
         print &ptime,"New file will not be copied to $DECODIR\n";
         system ("rm -rf $DECODIR_TMP/$subdir")  if (-d "$DECODIR_TMP/$subdir" && $DEBUG ne "-debug");
         exit -1
     }
    
# Lock the file
     open (ou,">$DECODIR_TMP/madis_${data_hour}.decoded.gz.lock");
     print ou "$DECODIR_TMP/madis_${data_hour}.decoded.gz\n";
     close (ou);

# Move file to final directory

     print &ptime,"Moving new file madis_${data_hour}.decoded.gz into $DECODIR\n";

     if ( ! ((system("mv -f $DECODIR_TMP/$subdir/madis_${data_hour}.decoded.gz $DECODIR/madis_${data_hour}.decoded.gz") >> 8) == 0) ){
         print &ptime,"Error moving file $DECODIR_TMP/$subdir/madis_${data_hour}.decoded.gz into $DECODIR\n";
         system ("rm -rf $DECODIR_TMP/$subdir")  if (-d "$DECODIR_TMP/$subdir" && $DEBUG ne "-debug");
         system ("rm -f $DECODIR_TMP/madis_${data_hour}.decoded.gz.lock")  if (-e "DECODIR_TMP/madis_${data_hour}.decoded.gz.lock");
         exit -1;
     }

# Unlock file
     system ("rm -f $DECODIR_TMP/madis_${data_hour}.decoded.gz.lock")  if (-e "$DECODIR_TMP/madis_${data_hour}.decoded.gz.lock");

# Clean-up workdir

     system ("rm -rf $DECODIR_TMP/$subdir")  if (-d "$DECODIR_TMP/$subdir" && $DEBUG ne "-debug");

# Exit

     print &ptime,"Output is file $DECODIR/madis_${data_hour}.decoded.gz\n";
     print &ptime,"MADIS data decoding successful for $data_hour\n";

     print &ptime, "-------------------------------------------------------------\n";
     print &ptime,"decode_madis.pl ending with exit status = 0\n";

     exit 0;

}
##-----------------------------------------------------------------------------#

sub ptime {
    $ts = sprintf("%02d:%02d:%02d ", (gmtime())[2],(gmtime())[1],(gmtime())[0]);
    return $ts;
}

sub help {
         print ("\n\nUsage:\n");
         print ("-----");
         print ("\n madis_decoder.pl: Decode MADIS data valid at current UTC time\n");
         print ("                   (rounded to the closest past hour)\n");

         print ("\n madis_decoder.pl lag 1: Decode MADIS data valid at current UTC time minus 1hr\n");
         print ("                         (rounded to the closest past hour)\n");

         print ("\n madis_decoder.pl CCYYMMDDHH: Decode MADIS data valid at current CCYYMMDDHH\n");
         print ("                             (lag does not apply)\n\n");

         exit -1;
 }

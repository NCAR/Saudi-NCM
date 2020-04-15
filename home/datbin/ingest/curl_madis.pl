#! /usr/bin/perl -s
#------------------------------------------------------------------------------#
#
# Script to ftp data files from the FSL MADIS data base.
#
# Usage: ftp_madis.pl [CCYYMMDDHH]
# -----
#
# To pull FSL-MADIS data files valid at CCYYMMDDHH, CCYYMMDDHH-1hr and
# CCYYMMDDHH-2hr
#
# The script lists all the files available for the times defined above
# and the observational platforms requested in variable SUBSCRIBED on the sever,
# examines it to see what it needs and then gets only the files its need 
# (ie those that are not already been ftped or those who size has changed
# since the previous ftp). It keeps an inventory of all files retrieved so 
# that we don't get them redundantly. 
#
# Enter: "ftp_madis.pl help" for help.
# -----
#
# Data arrive arrive on the MADIS server on a continuous, asynchronous schedule,
# and the current and previous hour's data are processed every 5 minutes. 
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
#------------------------------------------------------------------------------#
# Copyright UCAR (c) 1992 - 2004.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Program (RAP),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#
# Francois Vandenberghe, vandenb@ucar.edu, April 2004.
#------------------------------------------------------------------------------#
#
#------------------------------------------------------------------------------#
use Time::Local;

# Directory where the MADIS raw data will be copied before decoding
# and path to MADIS library static files.

$MADIS_DATA = "/lustre/project/k1206/datainput/madis-input-nc";

# MADIS Subscribed Platforms (will try to pull data from all these directories)

@SUBSCRIBED = ("LDAD/hcn", "LDAD/mesonet", "LDAD/nepp", "LDAD/profiler", "point/HDW", "point/HDW1h", "point/POES", "point/maritime", "point/metar", "point/profiler", "point/raob", "point/sao");

# Fill in the MADIS account username and password here

# $ftpHost = "rftp.madis-fsl.org"; # (66.45.28.37)
# $ftpHost = "66.45.28.37";
#$ftpHost = "pftp.madis-data.noaa.gov"; # old ftp site
$ftpHost = "madis-data.ncep.noaa.gov"; # new ftp site
#$user = "rap3_madis_research";
#$pwd = "U2eBQfHa8Aw4";

# # Maximum number of days to go back for pulling data
$NDAYS = 1;

# Intermediate file names

$dir_list = "remote_dir_list.asc";
$inv_file = "inventory.asc";
$max_ftp = 24;


# Use current UTC time minus 1hr as default
 ($ss , $mm , $th , $td , $tm , $ty , $wday , $yday , $isdst  ) = gmtime(time()-0*3600) ;
 ($ss1, $mm1, $th1, $td1, $tm1, $ty1, $wday1, $yday1, $isdst3 ) = gmtime(time()-1*3600) ;
 ($ss2, $mm2, $th2, $td2, $tm2, $ty2, $wday2, $yday2, $isdst3 ) = gmtime(time()-2*3600) ;
 ($ss3, $mm3, $th3, $td3, $tm3, $ty3, $wday3, $yday3, $isdst3 ) = gmtime(time()-3*3600) ;

# Parse arguments

if ( $#ARGV >=  0 ) { # Time passed as argument

$i = 0;

while($i <= $#ARGV) {

 if ($ARGV[$i] eq  "help" ){
     &help;
 }elsif ($ARGV[$i] eq  "debug" ) { # debug option
     $i += 1;
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
     $ty1 = $ty;
     $ty2 = $ty;
     $ty3 = $ty;
     $tm1 = $tm;
     $tm2 = $tm;
     $tm3 = $tm;
     $td1 = $td;
     $td2 = $td;
     $td3 = $td;
     $th1 = $th;
     $th2 = $th;
     $th3 = $th;
     last;

   }

  $i++
 }


} 

# Determine the data date.

$data_month = sprintf("%04d%02d", $ty+1900, $tm+1);
$data_day   = sprintf("%04d%02d%02d", $ty+1900, $tm+1, $td);
$data_hour  = sprintf("%04d%02d%02d%02d", $ty+1900, $tm+1, $td, $th);
$data_date  = sprintf("%04d%02d%02d_%02d%02d", $ty +1900, $tm +1, $td,  $th, 0);
$data_date1 = sprintf("%04d%02d%02d_%02d%02d", $ty1+1900, $tm1+1, $td1, $th1,0);
$data_date2 = sprintf("%04d%02d%02d_%02d%02d", $ty2+1900, $tm2+1, $td2, $th2,0);
$data_date3 = sprintf("%04d%02d%02d_%02d%02d", $ty3+1900, $tm3+1, $td3, $th3,0);

# Regular expression pattern which defines files we want. 

$fpattern  = $data_date;
$fpattern1 = $data_date1;
$fpattern2 = $data_date2;
$fpattern3 = $data_date3;

# Count the total number of new files brought on the disk

$ftped_files = 0;

# Loop over the platforms types

foreach $data (@SUBSCRIBED) {

print &ptime, "-------------------------------------------------------------\n";
print &ptime, "Pull $data files for time $fpattern from MADIS\n";

$ldad = $data;
$ldad =~ s/\/.*//; 

if ($ldad eq "LDAD") {
$datanc = "$data/netCDF";
}else{
$datanc = "$data/netcdf";
}

# Set up file paths

$ftpDir  = "$datanc";
$localDir  = "$MADIS_DATA/$datanc";

#$grboutf = "$localDir/$data_date.gz";
$logFile = "$localDir/$data_date.log";
$ftplock = "$localDir/$data_date.lock";

print &ptime, "into $localDir\n";

`mkdir -p $localDir` if (! -d "$localDir");

# Create an inventory file if it doesn't already exist

if (! -e "$localDir/$inv_file") {
open INVENTORY, ">$localDir/$inv_file";
close INVENTORY;
}

# Trim the inventory to the last NDAYS (*24hr * 3 files/hour lines)

$hours_back = $NDAYS * 24 * 3;

@wcParts = split(' ', `wc -l $localDir/$inv_file`);
if ($wcParts[0] > $hours_back) {
  system ("tail -n$hours_back $localDir/$inv_file > $localDir/${inv_file}.tmp");
  system ("mv $localDir/${inv_file}.tmp $localDir/$inv_file") 
      if (-e "$localDir/${inv_file}.tmp");
}

# Open lock file

open FTPLOCK, ">$ftplock";
exit unless(flock (FTPLOCK, 2|4));  # Non-blocking lock

# Open log file
#print &ptime, "Logs in $logFile\n";
#open LOG, ">>$logFile";
#select(LOG); $| = 1;

print &ptime, "ftp output in file $localDir/$dir_list\n";
print &ptime, "files inventory in $localDir/$inv_file\n";

if (! (chdir $localDir)) {
    print &ptime, "Error: Can't cd to $localDir: $!\n";
    print &ptime, "Ending: exit status = 1.\n";
    exit 1;
} 


# Start the ftp process to get a directory listing from which we
# can decide which files to get.

#print &ptime,"ncftpls -l -d stdout -u $user -p $pwd ftp\://$ftpHost/$ftpDir/\n";

#$ftpstat = system "ncftpls -l -d stdout -u $user -p $pwd ftp\://$ftpHost/$ftpDir/ > $dir_list";

$cmd = "curl --disable-epsv ftp://$ftpHost/$ftpDir/ > $dir_list";

print &ptime, "$cmd\n";

$ftpstat = system "$cmd";

# Main loop to determine files we need from the server

undef @ftp_list;
undef @inv_list;
undef @inv_size;

if ($ftpstat == 0) {

    # Read the inventory for files we already have
    $icnt = 0;
    open INVENTORY, "<$inv_file";
    while (<INVENTORY>) {
       chomp;
       $file =~ s#.*/##; 
      ($file, $file_size) = split /\s+/;
       $file =~ s#.*/##; 
       print &ptime, "File $file was copied ($file_size blocks)\n";
       $inv_list{$file} = 1;
       $inv_size{"$ftpDir/$file"} = "$file_size";
       $icnt++;
    }
    close INVENTORY;

    # Open listing file, loop though lines

    open LIST, "<$dir_list";
    while (<LIST>) {

        # Skip entries which do not match the file pattern
#       next if (! /$fpattern/ && ! /$fpattern1/ && ! /$fpattern2/ && ! /$fpattern3/ ) ;
        next if (! /$fpattern/ && ! /$fpattern1/ && ! /$fpattern2/ ) ;
        # grab the name of the file if pattern is matched.
	chomp $_;
        ($prv, $lnk, $uid, $gid, $size, $month, $fd, $time, $file) =
	    split /\s+/;
        ($fh, $fmn) = split /\:/, $time;

	# Decide if this file is from the data date

#       if ( /$fpattern/ || /$fpattern1/ || /$fpattern2/ || /$fpattern3/ ) {
        if ( /$fpattern/ || /$fpattern1/ || /$fpattern2/ ) {

            # See if we already have this in the inventory
            if (exists($inv_list{"$file"})){
               $file_size = $inv_size{"$ftpDir/$file"};
	       next if ($size <= $file_size);
            } 

	    push @ftp_list, $file;

            if (! exists($inv_list{"$file"})){ 
                  print &ptime, "Get  $file new file   ($size blocks)\n";
	    }elsif ($size >  $file_size) {
                  $file_size = $inv_size{"$ftpDir/$file"};
            print &ptime, "Get  $file new size is $size blocks (was $file_size previously)\n";
            }
	}
        else {
             print &ptime, "Skip $ftpDir/$file\n";
	}
    }
    close LIST;
    `rm -f $dir_list`;
}
else {
    print &ptime, "Could not get server directory listing, or no files available for $data.\n";
}

# If we have a file list, get the files. Break up a large list into batches

if (defined(@ftp_list)) {

    # Ftp a set number of files at a time.
    for ($i=0; $i<=$#ftp_list; $i+=$max_ftp) {

	$last_index = $i+$max_ftp-1;
	$last_index = $#ftp_list if ($i+$max_ftp-1 > $#ftp_list);
        @sublist = @ftp_list[$i .. $last_index];

        for ($j=$i; $j<=$last_index; $j+=1) {
             $sublistwithpath[$j-$i] = "$ftpDir/@ftp_list[$j]";
             $subUrlList[$j-$i]="-O ftp://$ftpHost/$sublistwithpath[$j-$i]";
        }
        print &ptime, "Pull @sublistwithpath\n";

        #print &ptime, "ncftpget -d stdout -u $user -p $pwd -F -Z $ftpHost $localDir\n";

	#$ftpstat = system "ncftpget -d stdout -u $user -p $pwd -F -Z $ftpHost $localDir @sublistwithpath > /dev/null 2>&1";

        $cmd = "curl --disable-epsv @subUrlList";
        
        print &ptime, "$cmd\n";

        $ftpstat = system "$cmd > /dev/null 2>&1";

	    # Add files to inventory
	    if ( $ftpstat == 0 ) {
	    open INVENTORY, ">>$inv_file";
	    foreach $file (@sublistwithpath) {
              $file_sizes = `ls -s --block-size=1 $MADIS_DATA/$file`;
	       $_ = $file_sizes;
              ($file_size, $file_name) = split /\s+/;
               print INVENTORY "$file $file_size\n";
	    }
	    close INVENTORY;

	    print &ptime, "gunzip -f @sublist\n";
             `gunzip -f @sublist`;
	}
	else {
	    print &ptime, "Error: Could not get @sublistwithpath\n";
 	    last;
	}
	undef @sublistwithpath;
	undef @sublist;
        undef @subUrlList;
    }
    print &ptime, "Info: Got ", $#ftp_list+1, " file(s), new inventory count: ", $#ftp_list+1+$icnt, "\n";
    $ftped_files = $ftped_files + $#ftp_list+1;
    undef @ftp_list;
}
else {
    print &ptime, "Info: No new files available, current inventory count: $icnt\n";
}

# Remove lock file
flock (FTPLOCK, 8); # Release lock
close FTPLOCK;

`rm -f $localDir/$data_date.lock`;

}

# Remove any lock file that could have been left

`find $MADIS_DATA -name \*.lock -exec rm \{\} \\;  ` ;

print &ptime, "-------------------------------------------------------------\n";

print &ptime, "Ending: exit status = 0.\n";
exit 0;

#-----------------------------------------------------------------------------#

sub ptime {
    $ts = sprintf("%02d:%02d:%02d ", (gmtime())[2],(gmtime())[1],(gmtime())[0]);
    return $ts;
}

#-----------------------------------------------------------------------------#

sub help {
         print ("\nftp_madis.pl [debug decode/nodecode CCYYMMDDHH]\n\n");
         print ("To pull FSL-MADIS data files and decode the data:\n\n");
         print ("CCYYMMDDHH: time of data file [+0,+59mn] (default is current time minus 1hr).\n");
         print ("nodecode:   never decode after ftp (default is decoding).\n");
         print ("decode:     force decode after ftp (default is decoding when new files only).\n");
         print ("debug:      additional print-out and no scrubbing (default is no debugging).\n\n");
         exit 1;
	 return 1;
 }

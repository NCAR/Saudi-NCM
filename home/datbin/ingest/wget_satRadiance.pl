#!/usr/bin/perl -s

#	This shell pulls in the satellite radiance data from NCEP's
#       ftp site: ftp.ncep.noaa.gov.  The data files are produced
#       approximately 5 hours after the model time.  Eg., 
#          model start time 0000 GMT = file time 0500 GMT
#          model start time 0600 GMT = file time 1100 GMT
#          model start time 1200 GMT = file time 1700 GMT
#          model start time 1800 GMT = file time 2300 GMT
#

use Time::Local;
$debug = 0;

# directory to use
#
$DATADIR = "/lustre/project/k1206/datainput/sat_radiance";
if ( ! -d $DATADIR ) { system("mkdir -p $DATADIR") or die "Could not makedir $DATADIR"; }

# figure out the appropriate model start time by
#   a) get the current hour ($hh)
#   b) subtract 5 hours ($model_hh) from the current hour
#   c) if $model_hh is greater than $hh, then we want yesterday's data 
#   d) then work back by hours: if $model_hh isn't in @modelhrs, subtract
#      one hour and check again 
#
$hh=`date +%H`; chomp $hh;
$model_hh=`date --date='-5 hour' +%H`; chomp $model_hh;

if ($debug) { print "Current hour is: $hh, 5 hours ago is: $model_hh\n" }; 

if ( $model_hh > $hh ) { $date2get=`date --date='-1 day' +%Y%m%d`; }
else { $date2get=`date +%Y%m%d`; }
chomp $date2get; 
$gfsdir="gfs." . $date2get;

@modelhrs = ('00','06','12','18');
@filesubstrs = ('1bamua', '1bamub', '1bmhs');

while (! grep $_ eq $model_hh, @modelhrs) { 
     $model_hh--; $model_hh=sprintf("%02d", $model_hh)
}
if ($debug) { print "Desired model hour is $model_hh\n"; }

# Now that we have everything we need, build the directory and filename
# and fetch the files.
# Note: since the filename does not include a date, we add the date
# to the filename after the file is downloaded
# 
chdir $DATADIR;

local $SIG{ALRM} = sub { die "timeout happened attempting to \n" };

$gfsdir="gfs." . $date2get . $model_hh;
foreach $str2use (@filesubstrs) {

   $file2get="gfs.t" . $model_hh . "z." . $str2use . ".tm00.bufr_d";

   if ($debug) { print "file2get is $file2get\n"; }

   if ( ! -e "$DATADIR/$date2get.$file2get" ) {
      if ($debug) { print "string is:  wget -t 3 ftp://ftp.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/${gfsdir}/${file2get}\n"; }
      else {
         system("wget -t 3 ftp://ftp.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/$gfsdir/$file2get");
         system("mv $DATADIR/$file2get $DATADIR/$date2get.$file2get");
      }
   }
}

exit;

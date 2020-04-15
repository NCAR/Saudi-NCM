#!/bin/sh
#
##############################################################################
# script to tar up pme obs by day - put into an archive directory for
# pickup to archive to the HPSS 
#
##############################################################################
# 
# make sure we've been given a config file containing all necessary info
#
if [ $# -ne 1 -o ! -e $1 ]; then
   echo "Usage:  $0 [config_file]"
   echo "FATAL: missing or undeclared config file: $1"
   exit 1
fi
config_file=$1

today=`date +%Y%m%d`
date_list=/home/pmeop/datbin/archive/archive_input_date.list

echo "Starting $0 on `date`"

##############################################################################
# set variables per $config_file
 
# we only archive input data OLDER than this date/time
# this value is generally set to -168
# -168 = 24 hrs * 7 days = 1 week
# 
min_hours_back=`grep min_hours_back $config_file | awk '{ print $2 }'`

# logfile
logfile=`grep logfile $config_file | awk '{ print $2 }'`

##############################################################################
#
# where are scripts located?
#
local_bin_dir=`grep local_bin_dir $config_file | awk '{ print $2 }'`
#
return_date=${local_bin_dir}/advance_cymdh.pl

# make sure needed scripts & files exist!
#
for fn in $return_date
do
  if [ ! -s $fn ]; then
    echo "FATAL: could not file $fn"
    exit 1
  fi
done

# remove old files so we don't get confused
#
rm -f $date_list $logfile  >&/dev/null

##############################################################################
# create list of wanted dates
# by default we start with the last_date_processed and grab everything
# newer than that and older than $min_hours_back.  
#
last_date=`grep last_date_processed $config_file | awk '{ print $2 }'`

sdate=`$return_date ${last_date}00 24 | cut -c1-8`
edate=`$return_date ${today}00 $min_hours_back | cut -c1-8`

if [ $sdate -gt $edate ]; then
   echo "DATE ERROR: $sdate greater than $edate"
   exit 1
else
  while [ $sdate -le $edate ]
  do
    echo $sdate >> $date_list 
    sdate=`$return_date ${sdate}00 24 | cut -c1-8`
  done
fi

##############################################################################
# tar up archive files by day and move to archive directory
#

obs_dir=`grep obs_dir $config_file | awk '{ print $2 }'`
archive_dir=`grep archive_dir $config_file | awk '{ print $2 }'`

#tar -cvf Saudi-metpme-obs-$yr$mon$dy.tar *$yr$mon$dy* && gzip Saudi-metpme-obs-$yr$mon$dy.tar && mv Saudi-metpme-obs-$yr$mon$dy.tar.gz ../observations_archive && rm *$yr$mon$dy*

for wanted_day in `cat $date_list`
do
  cd $obs_dir
  echo $wanted_day
  tar -cvf Saudi-metpme-obs-$wanted_day.tar *$wanted_day* && gzip Saudi-metpme-obs-$wanted_day.tar && mv Saudi-metpme-obs-$wanted_day.tar.gz $archive_dir && rm *$wanted_day* 
  if [ $? -ne 0 ]; then
    echo "FATAL: error tarring $wanted_day obs" >> $logfile
  fi
done

##############################################################################
# Cleanup & check for errors in the logfile
#
mailto=`grep mailto $config_file | awk '{ print $2 }'`
if [ -e $logfile ]; then
      mailx -s "$workdate Saudi pme ops tarring failure" $mailto < $logfile
fi
#if [ ! -e $logfile  ]; then 
   # if no logfile, 
   # then update the last_date_processed info in the config file
   
   new_last=`cat $date_list | cut -c1-8 | sort | uniq | sort -n | tail -1`

   sed "s/last_date_processed ${last_date}/last_date_processed ${new_last}/" < $config_file > /home/pmeop/datbin/archive/tmp.$$

   mv $config_file ${config_file}.sav
   mv /home/pmeop/datbin/archive/tmp.$$ $config_file
#fi

echo "Ending $0 on `date`"

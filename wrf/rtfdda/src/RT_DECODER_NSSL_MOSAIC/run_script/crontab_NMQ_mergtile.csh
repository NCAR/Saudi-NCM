#!/bin/csh -f
set job_dir = "/raid/GMODJOBS/GMWSMI"

cd $job_dir
#run the WSMR data
set str = `ps -ef |grep crontab_NMQ_mergtile.pl|grep -v grep`
if( $#str == 0 ) then
  echo "ok to run"
$job_dir/crontab_NMQ_mergtile.pl WSMR 2 
$job_dir/crontab_NMQ_mergtile.pl WSMR 3 
endif

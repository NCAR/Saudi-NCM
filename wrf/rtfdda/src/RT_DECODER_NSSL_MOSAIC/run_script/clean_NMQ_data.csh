#!/bin/csh -f

set job_dir = "/raid/GMODJOBS/GMWSMI"
set out_dir = "/raid/radar_ranges/WSMR"
cd $job_dir 

# clean the data of fifth day.
$job_dir/clean_NMQ_data.pl $out_dir -120

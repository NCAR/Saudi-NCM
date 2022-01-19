#!/bin/bash -l

#Cron entries that run this script need to be +8 from when the cycles starts 
#Or change the -f 8 part
module load cray-netcdf/4.6.3.2 
module load python/2.7.17-cdl
/lustre/project/k1206/x_fisherh/datbin/py4wrf/meteograms14.py -f 8 -d 2

#!/bin/csh
#       This shell pulls in the observation data from the
#       internet from some web sites.

set echo
set timestamp
setenv SUBSYSTEM GET_SST
setenv RM "rm -rf"

#
# ENVIRONMENT
# 
#source ${HOME}/datbin/ingest/ftpsites

#       Where is the data supposed to reside locally?
#
#set sstDir = "${PROJECTS_HOME}/${USER}/datainput/SST083"
set sstDir = "/scratch/project/k1206/datainput/SST083"
mkdir -p $sstDir
cd $sstDir

#       Choose which time to access, SST is only available at 00Z
set hh = 00

#set date = `date +%Y%m%d` 
set date  = `date --date "-1 days" +'%Y%m%d'`

#       The NCEP SST 0.083deg analysis
#
set ftp_site = ftpprd.ncep.noaa.gov
set root_dir = /data/nccf/com/gfs/prod
set dirname = ${root_dir}/sst.${date}/
set fname = rtgssthr_grb_0.083.grib2

pushd $sstDir

echo $fname
echo wget http\://$ftp_site\:/${dirname}${fname}
wget http\://$ftp_site\:/${dirname}${fname}


if ( -e $fname ) then
  set newfname = ${date}.rtg_sst083
  mv $fname ${sstDir}/$newfname
endif

popd

exit (0)


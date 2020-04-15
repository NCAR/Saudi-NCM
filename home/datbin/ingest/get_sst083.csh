#!/bin/csh
#	This shell pulls in the observation data from the
#	internet from some web sites.

set echo
set timestamp
setenv SUBSYSTEM GET_SST
setenv RM "rm -rf"

#
# ENVIRONMENT
# 
source ${HOME}/datbin/ingest/ftpsites

#	Where is the data supposed to reside locally?
#
#set sstDir = ${SST083DIR}
set sstDir = /lustre/project/k1206/datainput/sst083 
cd $sstDir

#	Choose which time to access, SST is only available at 00Z
set hh = 00

set date = `date +%Y%m%d -d 'now -1 day'`

#	The NCEP SST 0.083deg analysis
#
set ftp_site = $SST_FTPSITE
set root_dir = $SST_FTPDIR
set dirname = ${SST_FTPDIR}/sst.${date}/
set fname = rtgssthr_grb_0.083.grib2

pushd $sstDir

echo $fname
${HOME}/datbin/ingest/GetSSTFile.pl $SST_FTPSITE ${dirname} $fname


if ( -e $fname ) then
  set newfname = ${date}.rtg_sst083
  mv $fname ${sstDir}/$newfname
endif

popd

exit (0)

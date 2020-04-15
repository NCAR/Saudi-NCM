#!/bin/csh
#	This shell pulls in the observation data from the
#	internet from some web sites.

#set echo 
set timestamp
setenv SUBSYSTEM GET_NCEP
setenv RM "rm -rf"

#
# ENVIRONMENT
# 
source ${HOME}/datbin/ingest/ftpsites

#	Where is the data supposed to reside locally?
#
set NCEPDir = ${NCEPPreBDIR}
cd $NCEPDir

#	Choose which time to access, SST is only available at 00Z

set date = `date +%Y%m%d` 
#set date1 = `date +%Y%m%d%h` 

set datehh = ${date}18

set yesterday = ` /home/pmeop/datbin/advance_cymdh.pl $datehh -24 `  

set yesterday_date = `echo  $yesterday  |  cut  -c 1-8 `

echo $yesterday_date


#	The NCEP SST 0.083deg analysis
#
set ftp_site = $SST_FTPSITE
set root_dir = $SST_FTPDIR
#set fname = bufr_d 
set fname = prepbufr.nr 

pushd $NCEPDir

#  set newdir = ${date}
#if( ! -d ${NCEPDir}/$newdir ) then

#   mkdir ${NCEPDir}/$newdir
#endif

  cd ${NCEPDir}

echo $fname
set dirname = ${SST_FTPDIR}/gdas.${date}/
${HOME}/datbin/ingest/GetNCEPPreB.pl $SST_FTPSITE ${dirname} $fname $date
set dirname = ${SST_FTPDIR}/gdas.${yesterday_date}/
${HOME}/datbin/ingest/GetNCEPPreB.pl $SST_FTPSITE ${dirname} $fname $yesterday_date
foreach hh ( 00 06 12 18)
 set dirname_gfs = ${SST_FTPDIR}/gfs.${date}${hh}/
 ${HOME}/datbin/ingest/GetNCEPPreB.pl $SST_FTPSITE ${dirname_gfs} $fname $date
 set dirname_gfs = ${SST_FTPDIR}/gfs.${yesterday_date}${hh}/
 ${HOME}/datbin/ingest/GetNCEPPreB.pl $SST_FTPSITE ${dirname_gfs} $fname $yesterday_date
end



popd

exit (0)

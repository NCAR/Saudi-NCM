#! /bin/csh -f
#set echo
#------------------------------------------------------------------------------#
# save_tomss.csh [ccyymmdd]
# 
# Save MM5 input/output/boundary files and observation files in MSS for all 
# the cycle valid for day ccyymmdd. When ccyymmdd is not passed as the command 
# line argument, yesterday's date is used.
#------------------------------------------------------------------------------#

set LOCAL_DIR  = /data/cycles
set RANGE = WSMR

set REMOTE_SERVER = vandenb@dataproc.ucar.edu
set REMOTE_DIR = /ptmp/vandenb/WSMR
set MSS_DIR = WSMR

set ndays = 1  # How many days back we want to save.

if ($#argv >= 1) then
set ccyymmddhh = $1
set cc = `echo $ccyymmddhh |cut -c1-2`
set yy = `echo $ccyymmddhh |cut -c3-4`
set mm = `echo $ccyymmddhh |cut -c5-6`
set dd = `echo $ccyymmddhh |cut -c7-8`
set hh = `echo $ccyymmddhh |cut -c9-10`
set mn = "00"
set ss = "00"
set ccyymmdd = ${cc}${yy}${mm}${dd}
else
set cc = `date -u '+%C'`
set yy = `date -u '+%y'`
set mm = `date -u '+%m'`
set dd = `date -u '+%d'`
set hh = `date -u '+%H'`
set mn = `date -u '+%M'`
set ss = `date -u '+%S'`
set ccyymmdd_current = ${cc}${yy}${mm}${dd}
# Go back one day before in real-time
set ccyymmdd = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe ${ccyymmdd_current} -1`
endif

 set current_date = `date -u +%D`
 set current_time = `date -u +%T`
 echo  "${current_date} at ${current_time}:"
 echo  "Save RT FDDA data for day ${ccyymmdd}"

cd $LOCAL_DIR

foreach d ( 1 2 3)

foreach ff (REGRIDv3 MMOUTPUT_DOMAIN1 MMINPUT_DOMAIN1 LOWBDY_DOMAIN1 BDYOUT) 

 echo  "Copy files ${ff} to MSS $MSS_DIR"
#set FILES = `find . -name \*${ff}.${RANGE}\* -atime -${ndays} -print`
 set FILES = `find . -name \*${ff}.${RANGE}\* -print |grep ${ccyymmdd}`

 if ($#FILES > 0) then
     set f = ${ccyymmdd}_${ff}_${RANGE}.tar
     tar cvf ${f} ${FILES}
     set fz = ${f}.gz
 else
  echo  "No MM5 ${ff} files available for ${ccyymmdd}"
 endif 

echo "scp ${f} ${REMOTE_SERVER}:${REMOTE_DIR}"
scp ${f} ${REMOTE_SERVER}:${REMOTE_DIR}
( ssh    ${REMOTE_SERVER} gzip ${REMOTE_DIR}/${f}; ssh ${REMOTE_SERVER} mswrite -C reliability=economy -t 900 ${REMOTE_DIR}/${fz} ${MSS_DIR}/${fz}; ssh ${REMOTE_SERVER} /usr/bin/rm ${REMOTE_DIR}/${fz} ; echo "File ${fz} was put in MSS:${REMOTE_DIR}" ; ) &

rm ${f}

end

end

foreach ff (all.obs qc_obs_for_assimilation_s)

#set FILES = `find . -name \*${ff} -atime -${ndays} -print`
 set FILES = `find . -name \*${ff} -print |grep ${ccyymmdd}`

 if ($#FILES > 0) then
     set f = ${ccyymmdd}_${ff}_${RANGE}.tar
     tar cvf ${f} ${FILES}
     set fz = ${f}.gz
 else
  echo  "o ${ff} files available for ${ccyymmdd}"
 endif 

echo "scp ${f} ${REMOTE_SERVER}:${REMOTE_DIR}"
scp ${f} ${REMOTE_SERVER}:${REMOTE_DIR}
( ssh    ${REMOTE_SERVER} gzip ${REMOTE_DIR}/${f}; ssh ${REMOTE_SERVER} mswrite -C reliability=economy -t 900 ${REMOTE_DIR}/${fz} ${MSS_DIR}/${fz}; ssh ${REMOTE_SERVER} /usr/bin/rm ${REMOTE_DIR}/${fz} ; echo "File ${fz} was put in MSS:${REMOTE_DIR}" ; ) &

rm ${f}

end

 set current_date = `date -u +%D`
 set current_time = `date -u +%T`
 echo  "${current_date} at ${current_time}:"

exit (0)

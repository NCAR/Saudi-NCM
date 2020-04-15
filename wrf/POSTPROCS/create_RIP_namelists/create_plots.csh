#! /bin/csh -f
# This wrapper script runs through all of the RIP processes to
# create the RIP namelists and some example plots

set dmn = "3"
set season = "summer"
set range = "WSMR"
set RIPEXEDIR = "/model/ops/build_mm_released/wrfv3.5.1/cycle_code/EXECUTABLE_ARCHIVE/"

if ($range == "ATC") then
 set plttime = 89
 #set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2010-08-18_17:00:00.ATC_P+FCST"
 #set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2017-08-01_17:00:00.ATC_P+FCST"
 set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2017-09-02_05:00:00.ATC_P+FCST"
else if ($range == "CRTC") then
 set plttime = 84
 #set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2010-12-03_00:00:00.CRTC_P+FCST"
 set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2017-09-20_00:00:00.CRTC_P+FCST"
else if ($range == "DPG") then
 #set plttime = 189
 #set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2010-11-21_21:00:00.DPG_P+FCST"
 set plttime = 84
 set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2017-09-15_00:00:00.DPG_P+FCST"
else if ($range == "EPG") then
 #set plttime = 189
 #set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2010-11-21_21:00:00.EPG_P+FCST"
 set plttime = 97
 set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2017-09-24_13:00:00.EPG_P+FCST"
else if ($range == "NVL") then
 set plttime = 189
 set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2010-11-21_21:00:00.NVL_P+FCST"
else if ($range == "RTC") then
 #set plttime = 57
 #set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2010-11-21_21:00:00.RTC_P+FCST"
 set plttime = 48
 set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2017-09-23_12:00:00.RTC_P+FCST"
else if ($range == "PMRF") then
 set plttime = 30
 set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2011-08-18_12:00:00.GRM_P+FCST"
else if ($range == "SNI") then
 set plttime = 84
 set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2011-07-21_00:00:00.SNI_P+FCST"
else if ($range == "WSMR") then
 set plttime = 9
 #set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2010-11-21_21:00:00.WSMR_P+FCST"
 set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2017-09-22_21:00:00.WSMR_P+FCST"
else if ($range == "YPG") then
 set plttime = 9
 #set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2010-11-21_21:00:00.YPG_P+FCST"
 set filename = "/modelrd/grim/rip/wrfout_d0${dmn}_2017-09-07_21:00:00.YPG_P+FCST"
else
 echo "Unavailable range"
endif

eval ncl 'range_location=\"$range\"' plttime=$plttime create_RIP.ncl
rm -f domain$dmn.in
rm -f domain$dmn.cgm
cp testMdomain$dmn.$season.$range domain$dmn.in
cd ../ripdp
rm -f test_0*

${RIPEXEDIR}/ripdp_wrf.exe test all $filename

cd ../rip
${RIPEXEDIR}/rip.exe ../ripdp/test domain$dmn.in

idt domain$dmn.cgm

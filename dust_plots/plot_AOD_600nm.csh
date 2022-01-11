#!/bin/csh

 set echo 

set indir = "/d2/pmefdda/cycles/GWRFDUST/GFS_WCTRL/"
set cycle = "2013123118"

set dom = 1

set dir = ${indir}${cycle}

rm -f filelist
ls -1 ${dir}/wrfout_d01*F > filelist

foreach fs (`cat filelist`)
set ncl = "ncl  file_in="${fs}" dom=1 indir='${indir}' cycle='${cycle}' plot_AOD_600nm.ncl"
#set ncl = "ncl 'file_in=\"${fs}\"' 'dom=1' 'indir=\"${indir}\"' 'cycle=\"${cycle}\"' plot_AOD_600nm.ncl >& log_${cycle}.log"
$ncl
end


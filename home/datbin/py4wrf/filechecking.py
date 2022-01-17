#!/usr/bin/python 

""" script to merge gfs4 six-hourly cycle forecast for WRF
    W. WU Sept. 14 2012 wanliwu@ucar.edu   """

import os
import sys
import glob

def time2cycle(wrfcycle = 6, offhours = 0):
    from datetime import datetime, timedelta
    curtime = str(datetime.now())
    bkptime = str(datetime.now() + timedelta(hours=offhours))
    # split stime into year, month, day and hour
    (year, month, day, hour) = (bkptime[0:4], bkptime[5:7], bkptime[8:10], bkptime[11:13])
    ihour = int(hour)
    ihour -= ihour%wrfcycle
    if ihour < 10:
       shour = "0" + str(ihour)
    else:
       shour = str(ihour)
    curcycle = year + month + day + shour
    return curtime, bkptime, curcycle 

# main progam 

fcstlen = 72                           # minimum file in MB
wrfcycle = 6                           # interval for gfs4 cycling in hours
wrfdir = "/d2/pmefdda/cycles/GWPME/GRM"    # directory holds downloaded GFS4 forecasts
wrfend = ".GRM_P+FCST"   # gfs4 file ending phrase
wrfplt = "/d1/wanliwu/WRFPLT"

bkhours = int(sys.argv[1])
print('Your current directory:', os.getcwd())
(curtime, bkptime, curcycle) = time2cycle(wrfcycle = 6, offhours = bkhours)
print('Current time (UTC):', curtime)
print('Backward time (UTC):', bkptime)
print('WRF cycle to be processed: ', curcycle)
#(pretime, precycle) = time2cycle(offhours = -6)
#print('Previous GFS cycle: ', precycle)
dom = sys.argv[2]
fbeg = 'wrfout_' + dom
fend = 'FCST'

# check if directory exists
if os.path.isdir(wrfplt):
   filelist = os.listdir(wrfplt)
else:
#  os.makedirs(wrfplt)
   pass    

""" determine latest GFS4 cycle hours
    and assume the checking is within the cycling window (gfscycle)
    so no crossing day happens  """

prefile = curcycle + '*'       # file name starts with  curcycle 
wrfout = wrfdir + '/' + curcycle
numfiles = 0 
for fone in os.listdir(wrfout):
    if fone.startswith(fbeg) and fone.endswith(fend):
#      print fone 
       numfiles += 1
#print ('total available files:', numfiles, ' for cycle ', curcycle)

if numfiles < 49:
   print 'cycle %s sounds imcomplete cycle' %curcycle
elif numfiles == 49:
   print 'cycle %s sounds a coldstart cycle' %curcycle
else:
   print 'cycle %s has %s outputs in domain: %s' %(curcycle, numfiles, dom)

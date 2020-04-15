#!/usr/bin/env python2.7
'''scp OISST data from panassa into model runing directory 
   WWU wanliwu@ucar.edu Jan. 06 2015
'''

import os
import sys
from datetime import datetime, timedelta
from wpylib.modules4cfs import runcmd, cymdh2date

# /ngic/static/OI-daily-v2/data/2000/01
# oisstv2.025deg.19870115.nc
print 'inside scpoisst.py: current directory -> %s'%os.getcwd()

if len(sys.argv) < 6:
   print 'at least five  arguments are required but given %d' %(len(sys.argv)-1)
   print 'Usage: scpoisst.py startdate enddate datainterval rundir localhost datain'
   sys.exit()
else:
   startdate = sys.argv[1]
   enddate   = sys.argv[2]
   intval    = int(sys.argv[3])
   rundir    = sys.argv[4].strip()
   lhost     = sys.argv[5].strip()      # local host of CFSR data (e.g. ngic-c2-int2
   if len(sys.argv) == 6:
      datain = '/ngic/static/OI-daily-v2'           
   else:
      datain = sys.argv[6].strip()
   datain = os.path.join(datain, 'OI-daily-v2')

sdate = cymdh2date(startdate)
edate = cymdh2date(enddate)

# scp oisst files from panassas to WPS run directory 
print 'scp OISST files from local host to WRF run directory.'
rundir = os.path.join(rundir,'data/OISST')
print 'inside scpoisst: ', startdate, enddate, intval, lhost, datain, rundir 
if not os.path.isdir(rundir):
   os.makedirs(rundir)
# clean the existing files
os.system('rm -f %s/*'%rundir)  

cdate = sdate
while cdate <= edate:
    year = cdate.year
    month = cdate.month
    ymdir = '%04d/%02d'%(year, month)
    cymd = '%04d%02d%02d'%(cdate.year,cdate.month,cdate.day)

    srcdir = os.path.join(datain, 'data', ymdir)
    print 'data source dir: ', srcdir
    flnd = 'oisstv2.025deg.%s.nc'%cymd
    floc = os.path.join(srcdir, flnd)
    flnk = os.path.join(rundir, flnd)
    cmd = 'scp -p %s:%s %s'%(lhost, floc, flnk)
    out, err = runcmd(cmd) 
    if os.path.isfile(flnk):
       print '%s is ready.'%flnk

    cdate = cdate + timedelta(days = intval)

print 'Done with %s'%sys.argv[0]

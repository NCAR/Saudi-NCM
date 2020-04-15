#!/usr/bin/env python2.7
'''scp CFSR data from panassa into model runing directory 
   WWU wanliwu@ucar.edu Nov. 24 2014
'''

import os
import sys
#import shutil
#import argparse              # require python2.7 or later 
from datetime import datetime, timedelta
from wpylib.modules4cfs import runcmd, cymdh2date


print 'inside scpcfs.py: current directory -> %s'%os.getcwd()

if len(sys.argv) < 6:
   print 'at least five arguments are required, but got %d' %(len(sys.argv)-1)
   print 'Usage: scpcfs.py startdate enddate datainterval rundir localhost datain'
   sys.exit()
else:
   startdate = sys.argv[1]
   enddate   = sys.argv[2]
   intval    = int(sys.argv[3])
   rundir    = sys.argv[4].strip()
   lhost     = sys.argv[5].strip()      # local host of CFSR data (e.g. ngic-c2-int2
   if len(sys.argv) == 6:
      datain = '/ngic/static/CFSR/data'
   else:
      datain = sys.argv[6].strip()
      datain = os.path.join(datain, 'CFSR', 'data')

#datain = '/ngic/static/CFSR/data'
sdate = cymdh2date(startdate)
edate = cymdh2date(enddate)
#edate = edate + timedelta(hours=intval)

# scp cfs files from local host to WPS run directory 
print 'scp cfs files from local host to WRF run directory.'
rundir = os.path.join(rundir,'data/CFSR')
print 'inside scpcfs: ', startdate, enddate, intval, lhost, datain, rundir 
if not os.path.isdir(rundir):
   os.makedirs(rundir)
# clean the existing files
os.system('rm -f %s/*'%rundir)  

cdate = sdate
while cdate <= edate:
    year = cdate.year
    month = cdate.month
    ymdir = '%04d/%02d'%(year, month)
    cymdh = '%04d%02d%02d%02d'%(cdate.year,cdate.month,cdate.day,cdate.hour)

    # upper air files:
    fupr = 'cfs.upr.%s.grb2'%cymdh
    floc = os.path.join(datain, ymdir, fupr)
    flnk  = os.path.join(rundir, fupr)
    cmd = 'scp -p %s:%s %s'%(lhost, floc, flnk)
    out, err = runcmd(cmd) 
    if os.path.isfile(flnk):
       print '%s is ready.'%flnk
    else:
       print 'Cannot find file %s'%flnk

    # surface files:
    fsfc = 'cfs.sfc.%s.grb2'%cymdh
    floc = os.path.join(datain, ymdir, fsfc)
    flnk  = os.path.join(rundir, fsfc)
    cmd = 'scp -p %s:%s %s'%(lhost, floc, flnk)
    out, err = runcmd(cmd) 
    if os.path.isfile(flnk):
       print '%s is ready.'%flnk
    else:
       print 'Cannot find file %s'%flnk

    cdate = cdate + timedelta(hours = intval)

print 'Done with %s'%sys.argv[0]

#!/usr/bin/env python2.7
'''scp GLDAS data from panassa into model runing directory 
   WWU wanliwu@ucar.edu Jan. 06 2015
'''

import os
import sys
from datetime import datetime, timedelta
from wpylib.modules4cfs import runcmd, cymdh2date

# /ngic/static/GLDAS/1deg/data/1997/10
# /ngic/static/GLDAS/025deg/data/2000/02
# NOAH025SUBP.2000022400.nc
print 'inside scpgldas.py: current directory -> %s'%os.getcwd()

if len(sys.argv) < 6:
   print 'at least five  arguments are required but given %d' %(len(sys.argv)-1)
   print 'Usage: scpgldas.py startdate enddate datainterval rundir localhost datain'
   sys.exit()
else:
   startdate = sys.argv[1]
   enddate   = sys.argv[2]
   intval    = int(sys.argv[3])
   rundir    = sys.argv[4].strip()
   lhost     = sys.argv[5].strip()      # local host of CFSR data (e.g. ngic-c2-int2
   if len(sys.argv) == 6:
      datain = '/ngic/static/GLDAS'           
   else:
      datain = sys.argv[6].strip()
   datain = os.path.join(datain, 'GLDAS')

sdate = cymdh2date(startdate)
edate = cymdh2date(enddate)

# scp gldas files from panassas to WPS run directory 
print 'scp GLDAS files from local host to WRF run directory.'
rundir = os.path.join(rundir,'data/GLDAS')
print 'inside scpgldas: ', startdate, enddate, intval, lhost, datain, rundir 
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

    if cdate < datetime(2000, 2, 24, 0):
       degdir = '1deg'
    else:
       degdir = '025deg'
    srcdir = os.path.join(datain, degdir, 'data', ymdir)
    print 'data source dir: ', srcdir
  
    if '025' in degdir:
       flnd = 'NOAH025SUBP.%s.nc'%cymdh
    else:
       flnd = 'NOAH10SUBP.%s.nc'%cymdh
    floc = os.path.join(srcdir, flnd)
    flnk = os.path.join(rundir, flnd)
    cmd = 'scp -p %s:%s %s'%(lhost, floc, flnk)
    out, err = runcmd(cmd) 
    if os.path.isfile(flnk):
       print '%s is ready.'%flnk

    cdate = cdate + timedelta(hours = intval)

print 'Done with %s'%sys.argv[0]

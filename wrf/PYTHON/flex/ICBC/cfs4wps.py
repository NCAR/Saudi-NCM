#!/usr/bin/env python2.7
'''prepare CFSR data for WRF ungrib program.
   check local disk first, then HPSS and webserver if necessary 

   WWU wanliwu@ucar.edu Jan. 04 2014, revised Apr. 2014 
'''

import os
import sys
import shutil
import argparse              # require python2.7 or later 
from datetime import datetime, timedelta
#from wpylib import forsh, forcfs
from wpylib.modules4cfs import runcmd, cymdh2date


print 'inside cfsdata.py: current directory -> %s'%os.getcwd()

if len(sys.argv) < 6:
   print 'five arguments are required but given %d' %(len(sys.argv)-1)
   print 'Usage: cfsdata.py startdate enddate datain rundir'
   sys.exit()
else:
   startdate = sys.argv[1]
   enddate   = sys.argv[2]
   intval    = int(sys.argv[3])
   datain    = sys.argv[4].strip()
   rundir    = sys.argv[5].strip()

print 'inside cfs4wps: ', startdate, enddate, intval 
sdate = cymdh2date(startdate)
edate = cymdh2date(enddate)
edate = edate + timedelta(hours=intval)
print 'inside cfs4wps: ', sdate, edate

datain = os.path.join(datain, 'CFSR', 'data')
print datain

# soft link cfs files from local disk to WPS run directory 
print 'Doing soft link cfs files from local disk to WRF run directory.'
rundir = os.path.join(rundir,'data/CFSR')
cmd = 'mkdir %s'%rundir  
out, err = runcmd(cmd)
cmd = 'rm -f %s/*'%rundir  
out, err = runcmd(cmd)

cdate = sdate
while cdate <= edate:
    year = cdate.year
    month = cdate.month
    ymdir = '%04d/%02d'%(year, month)
    cymdh = '%04d%02d%02d%02d'%(cdate.year,cdate.month,cdate.day,cdate.hour)

    fupr = 'cfs.upr.%s.grb2'%cymdh
    floc = os.path.join(datain, ymdir, fupr)
    flnk  = os.path.join(rundir, fupr)
    if os.path.isfile(floc):
       cmd = 'ln -sf %s %s'%(floc, flnk)
       out, err = runcmd(cmd)
       print cmd

    fsfc = 'cfs.sfc.%s.grb2'%cymdh
    floc = os.path.join(datain, ymdir, fsfc)
    flnk  = os.path.join(rundir, fsfc)
    if os.path.isfile(floc):
       cmd = 'ln -sf %s %s'%(floc, flnk)
       out, err = runcmd(cmd)
       print cmd

    cdate = cdate + timedelta(hours = intval)

print 'Done with %s'%sys.argv[0]

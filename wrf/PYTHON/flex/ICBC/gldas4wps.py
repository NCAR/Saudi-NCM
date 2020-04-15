#!/usr/bin/env python2.7
'''prepare GLDAS data for WRF/WPS metgrid program 
   check local disk first, then the data webserver if necessary 

   WWU wanliwu@ucar.edu Feb. 24 2014 
'''

import os
import sys
import glob
import shutil
import argparse              # require python2.7 or later 
from datetime import datetime, timedelta
#from wpylib import forsh, forcfs
from wpylib.modules4cfs import runcmd, cymdh2date


print 'inside gldas4wps.py: current directory -> %s'%os.getcwd()

forexe = 'gldas2met.exe'                                  # fortran executable for netcdf to WPS interm. file

if len(sys.argv) < 6:
   print 'six arguments are required but given %d' %(len(sys.argv)-1)
   print ''
   print 'Usage: gldas4wps.py startdate enddate intval datain rundir PYTHON_ARCHIVE'
   print ''
   sys.exit()
else:
   startdate = sys.argv[1]
   enddate   = sys.argv[2]
   intval    = int(sys.argv[3])
   datain    = sys.argv[4].strip()
   rundir    = sys.argv[5].strip()


sdate = cymdh2date(startdate)
edate = cymdh2date(enddate)
edate = edate + timedelta(hours=intval)

rundir = os.path.join(rundir,'GLDAS')

if not os.path.isdir(rundir):
   cmd = 'mkdir %s'%rundir  
   out, err = runcmd(cmd)
else:
   cmd = 'rm -f %s/*'%rundir  
   out, err = runcmd(cmd)

print 'inside gldas4wps: ', datain, rundir, sdate, edate 
cdate = sdate
while cdate <= edate:
    year = cdate.year
    month = cdate.month
    ymdir = '%04d/%02d'%(year,month)
    mdate = '%04d%02d%02d%02d'%(cdate.year,cdate.month,cdate.day,cdate.hour)

    if cdate < datetime(2000, 2, 24, 0):
       f = 'NOAH10SUBP.%s'%mdate
    else:
       f = 'NOAH025SUBP.%s'%mdate
    fc = f + '.nc'

    floc = os.path.join(datain, fc)

    if os.path.isfile(floc):

       if os.path.isfile('gldas.nc'):
          mycmd = 'rm -f gldas.nc'
          out,err = runcmd(mycmd)

       mycmd = 'ln -sf %s gldas.nc'%floc 
       out,err = runcmd(mycmd)                   # netcdf to WPS intermediate file


       mycmd = './%s gldas.nc %s'%(forexe,mdate)
       out,err = runcmd(mycmd)                   # netcdf to WPS intermediate file

    cdate = cdate + timedelta(hours = intval)

for f in glob.glob('DAS:*'):
    fn = 'GLDAS/' + f 
    shutil.move(f, fn)
print 'Done with %s'%sys.argv[0]

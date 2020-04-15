#!/usr/bin/env python2.7
####!/usr/bin/env python
'''Download CFS, GLDAS, OISSTV2, and RTGSST
   Requirements: python2.7 or later, lftp, ncl_convert2nc
   Tested on yellowstone, ngic-c2 
   Usage: ./datainquiry4wrf -h 
   WWU (wanliwu@ucar.edu) Apr. 2014 
'''

import os
import sys
from datetime import datetime, timedelta
import glob
import argparse
import shutil
import subprocess

def runcmd(mycmd):
    p = subprocess.Popen(mycmd, shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
    return p.communicate()

parser = argparse.ArgumentParser(description='require forcing data for WRF')
parser.add_argument('-i', dest='cycletime', type=str, default='',
                    help = 'start date ccyymmddhh')
parser.add_argument('-l', dest='fcstlength', type=str, default='', 
                    help = 'forecast length in hours')
parser.add_argument('-d', dest='locdir', type=str, default='./', 
                    help = 'local directory to hold downloaded data')
parser.add_argument('-r', dest='rundir', type=str, default='./', 
                    help = 'local directory to hold data processing')
parser.add_argument('-p', dest='pydir', type=str, default='./', 
                    help = 'python code directory:PYTHON_ARCHIVE')

args = parser.parse_args()
sdate   = datetime.strptime(args.cycletime,"%Y%m%d%H")
edate   = sdate + timedelta(hours=int(args.fcstlength))
sdate   = sdate - timedelta(hours=int(args.fcstlength)) 

print 'inside icbcdata.py: ', args.cycletime, args.fcstlength
print 'inside icbcdata.py: ', sdate, edate 

hh = int(sdate.strftime('%H'))
if hh != 0: 
   sdate = sdate - timedelta(hours=hh)
startdate = sdate.strftime('%Y%m%d')
hh = int(edate.strftime('%H'))
if hh != 0: 
   hh = 24 - hh
   edate = edate + timedelta(hours=hh)
enddate   = edate.strftime('%Y%m%d')

print 'inside icbcdata.py: ', startdate, enddate 
#pydir = '/home/fddasys/pyutil/'
PYTHON_ARCHIVE = args.pydir
pydir = PYTHON_ARCHIVE + '/flex/ICBC/'

print pydir 

#wkdir = '/raid2/fddasys/tmp/datatmp'
wkdir = args.rundir + '/data/datatmp'
if not os.path.isdir(wkdir):
   os.makedirs(wkdir)

os.chdir(wkdir)
print os.getcwd()

alldata = ['CFS','GLDAS','OISST','RTGSST']
print 'Downloaded these data: ', alldata

locdir = args.locdir

if 'CFS' in alldata:
   # download CFS data:
   print 'Inqury CFS data...'
   ldir = locdir + '/CFSR/data'
   mycmd = pydir + 'cfsinquiry.py -b %s -e %s -o %s -i H:/home/nntdata/tmp4cfs '%(startdate,enddate,ldir)
   print mycmd 
   out, err = runcmd(mycmd)
   print out 
if 'GLDAS' in alldata:
   # download GLDAS data:
   print 'Inqury GLDAS data...'
   ldir = locdir + '/GLDAS'
   mycmd = pydir + 'gldasinquiry.py -b %s -e %s -d %s'%(startdate,enddate,ldir)
   print mycmd 
   out, err = runcmd(mycmd)
   print out 
if 'RTGSST' in alldata:
   # download RTGSST data:
   print 'Inqury RTGSST data...'
   ldir = locdir + '/RTGSST/data'
   mycmd = pydir + 'rtgsstinquiry.py -b %s -e %s -d %s'%(startdate,enddate,ldir)
   print mycmd 
   out, err = runcmd(mycmd)
   print out 
if 'OISST' in alldata:
   # download OISSTV2 data:
   print 'Inqury OISSTV2 data...'
   ldir = locdir + '/OISST/data'
   mycmd = pydir + 'oisstinquiry.py -b %s -e %s -d %s'%(startdate,enddate,ldir)
   print mycmd 
   out, err = runcmd(mycmd)
   print out 

# clean up working directory 
for root, dirs, files in os.walk(wkdir):
    for f in files:
    	os.unlink(os.path.join(root, f))
    for d in dirs:
    	shutil.rmtree(os.path.join(root, d))

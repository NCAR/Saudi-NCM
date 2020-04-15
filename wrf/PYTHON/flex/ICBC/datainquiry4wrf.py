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
parser.add_argument('-b', dest='startdate', type=str, default='',
                    help = 'start date ccyymmdd')
parser.add_argument('-e', dest='enddate', type=str, default='', 
                    help = 'end   date ccyymmdd')
parser.add_argument('-v', dest='vfields', type=str, default='CFS', 
                    help = 'data type to be downloaded: CFS,GLDAS')
parser.add_argument('-d', dest='locdir', type=str, default='./', 
                    help = 'local directory to hold downloaded data')

args = parser.parse_args()
sdate   = datetime.strptime(args.startdate,"%Y%m%d")
edate   = datetime.strptime(args.enddate,"%Y%m%d")

datatypes = args.vfields
alldata = datatypes.strip().upper().split(',')

pydir = '/home/fddasys/pyutil/'

wkdir = '/raid2/fddasys/tmp/datatmp'
os.chdir(wkdir)
print os.getcwd()

print 'Downloaded these data: ', alldata

locdir = args.locdir

if 'CFS' in alldata:
   # download CFS data:
   print 'Inqury CFS data...'
   ldir = locdir + '/CFSR/data'
   mycmd = pydir + 'cfsinquiry.py -b %s -e %s -o %s -i H:/home/nntdata/tmp4cfs '%(args.startdate,args.enddate,ldir)
   print mycmd 
   out, err = runcmd(mycmd)
   print out 
if 'GLDAS' in alldata:
   # download GLDAS data:
   print 'Inqury GLDAS data...'
   ldir = locdir + '/GLDAS'
   mycmd = pydir + 'gldasinquiry.py -b %s -e %s -d %s'%(args.startdate,args.enddate,ldir)
   print mycmd 
   out, err = runcmd(mycmd)
   print out 
if 'RTGSST' in alldata:
   # download RTGSST data:
   print 'Inqury RTGSST data...'
   ldir = locdir + '/RTGSST/data'
   mycmd = pydir + 'rtgsstinquiry.py -b %s -e %s -d %s'%(args.startdate,args.enddate,ldir)
   print mycmd 
   out, err = runcmd(mycmd)
   print out 
if 'OISST' in alldata:
   # download OISSTV2 data:
   print 'Inqury OISSTV2 data...'
   ldir = locdir + '/OISST/data'
   mycmd = pydir + 'oisstinquiry.py -b %s -e %s -d %s'%(args.startdate,args.enddate,ldir)
   print mycmd 
   out, err = runcmd(mycmd)
   print out 

# clean up working directory 
for root, dirs, files in os.walk(wkdir):
    for f in files:
    	os.unlink(os.path.join(root, f))
    for d in dirs:
    	shutil.rmtree(os.path.join(root, d))

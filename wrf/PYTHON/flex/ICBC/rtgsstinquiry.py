#!/usr/bin/env python2.7
'''Download daily 1/12 Deg RTGSST data in grib1 from 
   ftp://polar.ncep.noaa.gov/pub/history/sst/rtg_high_res
   for WRF/WPS SST field initialization.
   Downloaded files will have name convention of rtgsst_0.083_ccyymmdd.grb.
   Requirements: python2.7 or later, lftp, ncl_convert2nc
   Tested on yellowstone, ngic-c2 
   Usage: ./gldasinquiry -h 
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

parser = argparse.ArgumentParser(description='require RTGSST data from archives')
parser.add_argument('-b', dest='startdate', type=str, default='',
                    help = 'start date ccyymmdd')
parser.add_argument('-e', dest='enddate', type=str, default='', 
                    help = 'end   date ccyymmdd')
parser.add_argument('-d', dest='locdir', type=str, default='./', 
                    help = 'local directory to hold downloaded GLDAS data')

args = parser.parse_args()
sdate   = datetime.strptime(args.startdate,"%Y%m%d")
edate   = datetime.strptime(args.enddate,"%Y%m%d")

ldir = args.locdir
sstres = '0083DEG'   # sst resolution at 1/12 (0.083DEG)

datasite = 'ftp://polar.ncep.noaa.gov/pub/history/sst/rtg_high_res'
#filepref = 'rtg_sst_grb_hr_0.083.20120810'
filepref = 'rtg_sst_grb_hr_0.083.'

curdir = os.getcwd()
print 'Current diectory: %s'%curdir 
print 'Downloaded RTGSST data in grib will be in %s with name convention of rtgsst.%s.ccyymmdd.grb'%(ldir, sstres) 

cdate = sdate
while cdate <= edate:
      mydate = cdate.strftime('%Y%m%d')

      outdir = os.path.join(ldir, cdate.strftime('%Y'))
      outdir = os.path.join(outdir, cdate.strftime('%m'))
    
      if not os.path.isdir(outdir):  # check if local directory exists
         os.makedirs(outdir)
      # check if file avaiale in local disk: outdir
      lfs = 'rtgsst.' + sstres + '.' + mydate + '.grb'
      lfile = os.path.join(outdir,lfs)

      if not os.path.isfile(lfile): 
         print 'downloading RTGSST data on %s from the web ...'%mydate

         fname = filepref + mydate 
         rfile = os.path.join(datasite,fname)
         cmd = "lftp -e 'set net:timeout 10; get -e %s -o %s; bye'"%(rfile,lfile)
         out, err = runcmd(cmd)

      else: print 'RTGSST data on %s is available on the local disk already ...'%mydate

      cdate = cdate + timedelta(days=1)

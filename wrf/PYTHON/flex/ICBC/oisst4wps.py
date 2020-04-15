#!/usr/bin/env python2.7
'''download OI V2 SST from NOAA web server
   WWU wanliwu@ucar.edu
   12/20/2013
   For usage: ./oiv2sst.py -h
'''
import os
import sys
from datetime import datetime, timedelta
import argparse
import glob 
import shutil 
from wpylib.modules4cfs import runcmd 

parser = argparse.ArgumentParser(description='require OI V2 SST from archives')
parser.add_argument('-b', dest='startdate', type=str, default='',
                    help = 'start date ccyymmdd')
parser.add_argument('-e', dest='enddate', type=str, default='', 
                    help = 'end   date ccyymmdd')
parser.add_argument('-t', dest='intval', type=int, default=6, 
                    help = 'intermediate file frequency in hours')
parser.add_argument('-d', dest='locdir', type=str, default='', 
                    help = 'local disk that holds OIV2 SST')

#locdir = '/raid1/static/OI-daily-v2/data'
sstdir = 'OISST'

curdir = os.getcwd()

args = parser.parse_args()
sdate = args.startdate
edate = args.enddate
intvh = args.intval
locdir = args.locdir  

if len(args.startdate) > 8:
   sdate   = datetime.strptime(args.startdate,"%Y%m%d%H")
   hh   = int(sdate.hour)
   if hh != 0: sdate = sdate -timedelta(hours=hh)
else: 
   sdate   = datetime.strptime(args.startdate,"%Y%m%d")

if len(args.enddate) > 8:
   edate   = datetime.strptime(args.enddate,"%Y%m%d%H")
   hh   = int(edate.hour)
   while hh != 0: 
         edate = edate + timedelta(hours=intvh)
         hh = int(edate.hour)
else: 
   edate   = datetime.strptime(args.enddate,"%Y%m%d")

fstdate = sdate
lstdate = edate
if lstdate < fstdate: lstdate = fstdate + timedelta(days=1)

if fstdate < datetime(1981,9,1):
   print 'OIV2 SST avaialble from 19810901 to current (with two weeks delay)'
   print 'but you require data on %04d%02d%02d'%(fstdate.year,fstdate.month,fstdate.day)
   sys.exit()

cdate = fstdate 
while cdate <= lstdate:
    yearmon = cdate.strftime('%Y/%m')
    cymd = cdate.strftime('%Y%m%d')

    f = 'oisstv2.025deg.%s.nc'%cymd

#   cdir = os.path.join(locdir, yearmon)
    lf = os.path.join(locdir, f)

  # convert NetCDF into WPS intermediate format by converter: oi_daily_v2.nc.exe
    concmd = os.path.join(curdir, 'oi_daily_v2_nc.exe -i %s -t %s'%(lf,intvh))
    out, err = runcmd(concmd) 

    cdate = cdate + timedelta(days=1)
# rename files
oprefx = 'FILE:*'
nprefx = 'SST:'
ofiles = glob.glob(oprefx)

sstdir = os.path.join(curdir,sstdir)
if not os.path.isdir(sstdir):
   os.mkdir(sstdir)

for f in ofiles:
    prefx, cdate = f.split(':')
    fnew = nprefx + cdate
    fnew = os.path.join(sstdir, fnew)
    shutil.move(f, fnew)

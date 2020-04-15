#!/usr/bin/env python2.7
'''download OISSTV2 in netcdf from NOAA web server
   Another Fortran program will be needed to convert
   netcdf for WRF/WPS intermidate file
   downloaded files will have name convention of oisstv2_ccyymmdd.nc
   For usage: ./oiv2sst.py -h
   WWU wanliwu@ucar.edu
   12/20/2013
'''
import os
import sys
from datetime import datetime, timedelta
import argparse
import shutil 
import subprocess

def runcmd(mycmd):
    p = subprocess.Popen(mycmd, shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
    return p.communicate()

parser = argparse.ArgumentParser(description='require OI V2 SST from archives')
parser.add_argument('-b', dest='startdate', type=str, default='',
                    help = 'start date ccyymmdd')
parser.add_argument('-e', dest='enddate', type=str, default='', 
                    help = 'end   date ccyymmdd')
parser.add_argument('-t', dest='intval', type=int, default=6, 
                    help = 'intermediate file frequency in hours')
parser.add_argument('-d', dest='locdir', type=str, default='', 
                    help = 'local disk that holds OIV2 SST')

sstweb = 'ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2'
compresdir = 'NetCDF'
ucompresdir = 'NetCDF-uncompress'
sdir = 'AVHRR'
sstres = '025deg'    # sst resolution at 1/4 (0.25) DEG 

curdir = os.getcwd()

args = parser.parse_args()
sdate = args.startdate
edate = args.enddate
intvh = args.intval
ldir = args.locdir  

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

print 'Downloaded OISST data in netcdf  will be in %s with name convention of oisstv2.%s.ccyymmdd.nc'%(ldir, sstres) 
cdate = fstdate 
while cdate <= lstdate:
    year = cdate.strftime('%Y')
    month = cdate.strftime('%m')
    cymd = cdate.strftime('%Y%m%d')

    f = 'avhrr-only-v2.%s.nc'%cymd
    fl = 'oisstv2.%s.%s.nc'%(sstres,cymd)

    cdir = os.path.join(ldir, year, month)
    if not os.path.isdir(cdir):
       os.makedirs(cdir)
    fl = os.path.join(cdir, fl)

  # if file is not in local disk: locdir, download it from webserver 
    if not os.path.isfile(fl):
       rf = os.path.join(sstweb,ucompresdir,year,sdir,f)
       print 'Downloading %s'%f
       dlcmd = "lftp -e 'set net:timeout 10; get -e %s -o %s; bye'"%(rf,fl) 
       out, err = runcmd(dlcmd) 

    else:
       print '%s is available in local disk.'%fl 
    cdate = cdate + timedelta(days=1)

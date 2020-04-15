#!/usr/bin/env python2.7
''' Inquiry CFS data (reanalysis, analysis, or forecast) for WRF/WPS model
      Extract data from local disk, HPSS or website archives
      CFS reanalysis (CFSR): 01/01/1979 - 03/31/2011 as of Nov. 2013
                     available on CISL yellowstone disk & HPSS 
            analysis (CFSV2): 04/01/2011 - present  
                     available on CISL yellowstone disk & HPSS 
            forecast (CFSV2): the recent 7 days - future
                     available on NCEP CFS website for download 
      CFS data are packaged in blockes with file size of about 10GB/day. 
      This python program extracts only necessary fields/times:
        pressure and surface fields at 00, 06, 12 and 18Z required by WRF/WPS
        that results in ~500MB/day with file renamed in  WRF/WPS convention:
             cfs.upa.ccyymmddhh.grb2
             cfs.sfc.ccyymmddhh.grb2
             where ccyymmddhh is valid time.

      Default setting is to require pressure level (re)analysis if available and 6-hours 
      surface forecast. The default setting can be overwritten through commend-line 
      arguments to allow users to use 6-hours pressure level forecast or 
      to add surface (re)analysis fields, too.
 
      For usage: python cfsdatainquiry.py -h
 
      WWU wanliwu@ucar.edu Oct. 29 2013 
      WWU wanliwu@ucar.edu Nov. 14 2013 revised  
      WWU wanliwu@ucar.edu Nov. 16 2013 revised using python class
      WWU wanliwu@ucar.edu Nov. 19 2013 revised to add HPSS option
      WWU wanliwu@ucar.edu Dec. 10 2013 revised to add website option

      Thanks to Badrinath Nagarajan (badri@ucar.edu) for HPSS and lfp tips
'''

__version__ = "$Revision: 1.1 $"

import os
import sys
import time 
import argparse
import tarfile 
import re
import calendar
import subprocess
import shutil
import glob
from datetime import datetime, timedelta
from urllib2 import urlopen, HTTPError
from wpylib.modules4cfs import *

#
# main program here 

start_time = time.clock()
parser = argparse.ArgumentParser(description='require CFS data from archives')
parser.add_argument('-b', dest='startdate', type=str, default='',
                    help = 'start date ccyymmdd')
parser.add_argument('-e', dest='enddate', type=str, default='', 
                    help = 'end   date ccyymmdd')
parser.add_argument('-f', dest='fcsthrs', type=int, default=24, 
                    help = 'forecast hours from the startdate if endate is not given. default:24')
parser.add_argument('-r', dest='radate', type=str, default='20110331', 
                    help = 'last day of available reanalysis ccyymmdd. default: 20110331')
parser.add_argument('-u', dest='upranl',  default=True,     
                    help = 'if pressure level analysis is required (True) default: True')
parser.add_argument('-s', dest='sfcanl',  default=False,     
                    help = 'if surface analysis is required (True) default: False')
parser.add_argument('-i', dest='cfsdir',type=str, default='H:/home/nntdata/tmp4cfs',     
                    help = 'local disk holds CFS archives: H:/home/nntdata/tmp4cfs;' +  \
                        'or temporary HPSS directory for CFS file index: H:/home/$USER/tmp4cfs')
parser.add_argument('-o', dest='locdir',type=str, default='./cfsdata',     
                    help = 'directory holds user inquired CFS data. default: ./cfsdata')
parser.add_argument('-m', dest='ensmem',type=int, default=1,     
                    help = 'CFS ensemble member: 1, 2, 3, 4. default: 1')

args = parser.parse_args()

cfsfcstdays = 45             # maximum CFS forecast days
cfsdelay = 8                 # hours CFS forecast available online post-intial time 
cfsint = 6                   # CFS forecast output frequency in hours 
dhrs = cfsdelay - cfsint
cfsmem = args.ensmem         # CFS ensemble member (1, 2, 3, 4)

tryweb = False

curtime = datetime.utcnow()
ftime = curtime + timedelta(days = cfsfcstdays-1)
print 'current time (UTC): ',  curtime 
if args.startdate == '':
   sdate = datetime(int(curtime.strftime('%Y')),int(curtime.strftime('%m')),int(curtime.strftime('%d')))
   bkhrs = 2*cfsint if int(curtime.strftime('%H')) < dhrs else cfsint
   tmptm = sdate - timedelta(hours=bkhrs)
   initime = tmptm.strftime('%Y%m%d%H')
   edate = sdate + timedelta(hours=args.fcsthrs) 
   tryweb = True 
else:
   print 'inside cfsinquiry.py: ', args.startdate, args.enddate 
   if len(args.startdate) < 10:
      sdate = datetime.strptime(args.startdate,"%Y%m%d")
   else:
      sdate = datetime.strptime(args.startdate,"%Y%m%d%H")

   if args.enddate == '': 
      edate = sdate + timedelta(hours=args.fcsthrs)
   else:
      if len(args.enddate) < 10:
         edate = datetime.strptime(args.enddate,"%Y%m%d")  
         edate = edate + timedelta(days=1)
      else:
         edate = datetime.strptime(args.enddate,"%Y%m%d%H")  

if sdate > ftime:
   sdate = ftime -timedelta(days=1)
if edate > ftime: edate = ftime

if not tryweb and sdate > curtime-timedelta(days=1):
   initime = findinitime(cfsdelay,cfsint)
   tryweb = True 

rdate   = datetime.strptime(args.radate,"%Y%m%d")  
uprdat  = args.upranl 
sfcdat  = args.sfcanl
cfsdir  = args.cfsdir

cfsdir = cfsdir.strip()
if re.search(r'\w+:',cfsdir):
   dkw, dloc = cfsdir.split(':')
   if dkw == 'L' or dkw == 'H':
      cfsdir = dloc.strip()
else:
   dkw = 'L' 

localdisk = True if dkw == 'L' else False 

locdir  = args.locdir

if edate < sdate:
   print 'Your ending date %s is earlier than the starting date %s'%(edate,sdate)
   print 'Please make change and retry!'
   sys.exit()

if sdate < datetime(1979,1,1):
   print 'CFS data available from 19790101 to %s. Your request of %s is not available' \
         %(curtime.strftime('%Y%m%d'),sdate.strftime('%Y%m%d'))
   sys.exit()

if not os.path.exists(locdir):
   print '%s does not exist. Create it now... '%locdir
   os.makedirs(locdir)
print 'Current directory: %s'%os.getcwd()
print 'You request CFS data for period %s - %s'%(sdate.strftime('%Y-%m-%d'),edate.strftime('%Y-%m-%d'))
print 'downloaded data will be in %s with name convention of cfs.upr.ccyymmddhh.grb2 or cfs.sfc.ccyymmddhh.grb2'%locdir

# check if the data are already in local disk then no download necessary
# any missing file will request download the whole period!
nfiles = 4  # expect 4 files/day (6-hourly data)
ckday = sdate
dataOK = True          
while ckday <= edate:
    ymdir = ckday.strftime('%Y/%m') 
    ymd = ckday.strftime('%Y%m%d')
    fs = 'cfs.sfc.' + ymd + '*'
    fu = 'cfs.upr.' + ymd + '*'
    fdir = os.path.join(locdir, ymdir, fs)
    sfiles = glob.glob(fdir)
    print 'Looking in ',fdir,' for CFS sfc file ',sfiles
    fdir = os.path.join(locdir, ymdir, fu)
    ufiles = glob.glob(fdir)
    print 'Looking in ',fdir,' for CFS upr file ',ufiles
    print ckday, len(sfiles), len(ufiles) 
    if len(sfiles) < nfiles or len(ufiles) < nfiles:
       dataOK = False 
    if not dataOK: break 

    ckday = ckday + timedelta(days=1)

if dataOK:
   print 'Reqested CFS data are already available in local disk. No more download.'
   sys.exit(0)

if tryweb:
   fetchcfsforecast(initime, sdate, edate,locdir, mem = '%02d'%cfsmem)
   sys.exit()

if dkw == 'H':
   print 'Your designed CFS archive is in HPSS'
   hsicmd = 'hsi "ls %s"'%cfsdir
   out,err = runcmd(hsicmd)
   if re.search(r'No such file or directory', out):
      print 'HPSS directory: %s does not exist' %cfsdir
      print "let's create it"
      if cfsdir == '': cfsdir = 'tmp4cfs' 
      hsicmd = 'hsi "mkdir %s"'%cfsdir 
      out,err = runcmd(hsicmd)
      hsicmd = 'hsi "ls %s"'%cfsdir
      out,err = runcmd(hsicmd)
      if not re.search(r'No such file or directory', out):
         print 'HPSS directory: %s is ready'%cfsdir 
      else:
         print 'Please make sure %s exist, then retry'%cfsdir 
else:
   print 'Your designed CFS archive is in %s' %cfsdir 

cfs = cfsdata(uprdat, sfcdat, locdir, cfsdir, localdisk, rdate, cfsdelay, cfsint, cfsmem) 

if sdate <= rdate and edate > rdate:   # data period requested is crossing last day of reanalysis (March 31 2011),
                                       # shall be treated seperately. reanalysis (gdas) data are in 5-day block
                                       # while analysis (cdas) data are archived as individual day, and they are also
                                       # different in file name convention 
   bdate = rdate + timedelta(days = 1)
   if localdisk:
      porf, udate = cfs.gdas(sdate = sdate, edate = rdate)
      if porf == 'F': print 'cfs.gdas Failed on %s' %udate 
      porf, udate = cfs.cdas(sdate = bdate, edate = edate)
      if porf == 'F': print 'cfs.cdas Failed on %s' %udate 
   elif dkw == 'H':
      porf, udate = cfs.remotegdas(sdate = sdate, edate = rdate)
      if porf == 'F': print 'HPSS: cfs.remotegdas Failed on %s' %udate 
      porf, udate = cfs.remotecdas(sdate = bdate, edate = edate)
      if porf == 'F': print 'HPSS: cfs.remotecdas Failed on %s' %udate 
   else:
      fetchcfsforecast(initime, sdate, edate,locdir, mem = '%02d'%cfsmem)
elif edate <= rdate: 
   if localdisk:
      porf, udate = cfs.gdas(sdate = sdate, edate = edate)
      if porf == 'F': print 'cfs.gdas Failed on %s' %udate
   else:
      porf, udate = cfs.remotegdas(sdate = sdate, edate = edate)
      if porf == 'F': print 'HPSS: cfs.remotegdas Failed on %s' %udate 
elif sdate >  rdate: 
   if localdisk:
      porf, udate = cfs.cdas(sdate = sdate, edate = edate)
      if porf == 'F': print 'cfs.cdas Failed on %s' %udate
   else:
      porf, udate = cfs.remotecdas(sdate = sdate, edate = edate)
      if porf == 'F': print 'HPSS: cfs.remotecdas Failed on %s' %udate

print 'Finish time: %s'%datetime.utcnow()

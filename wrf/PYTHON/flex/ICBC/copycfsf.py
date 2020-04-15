#!/usr/bin/env python2.7
####!/usr/bin/env python
'''Soft-Link CFSF data for running CFDDA.
   Usage: ./linkcfsf.py -b ccyymmdd -e ccymmdd -i ccyymmddhh -d desdiretory 
   WWU (wanliwu@ucar.edu) Apr. 2014 
'''

import os
import sys
from datetime import datetime, timedelta
import glob
import argparse
import shutil
import subprocess
import re

def runcmd(mycmd):
    p = subprocess.Popen(mycmd, shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
    return p.communicate()

parser = argparse.ArgumentParser(description='copy CFSF data for WRF')
parser.add_argument('-b', dest='startdate', type=str, default='',
                    help = 'start date ccyymmdd')
parser.add_argument('-e', dest='enddate', type=str, default='', 
                    help = 'end   date ccyymmdd')
parser.add_argument('-i', dest='inidate', type=str, default='', 
                    help = 'CFSF initial date')
parser.add_argument('-d', dest='locdir', type=str, default='/raid1/static/CFSR/data', 
                    help = 'destination directory for copied CFSF data')

args = parser.parse_args()
if len(args.startdate) and len(args.enddate) == 8:
   sdate   = datetime.strptime(args.startdate,"%Y%m%d")
   edate   = datetime.strptime(args.enddate,"%Y%m%d")
elif len(args.startdate) and len(args.enddate) == 10:
   sdate   = datetime.strptime(args.startdate,"%Y%m%d%H")
   edate   = datetime.strptime(args.enddate,"%Y%m%d%H")
else:
   print 'inside linkcfsf.py: date should be specified as ccyymmdd or ccyymmddhh.'
   print 'But dates are given as: ', args.startdate, args.enddate
   sys.exit()

inidate = args.inidate 
if inidate == '':
   curtime = datetime.utcnow() - timedelta(days=1)
   inidate = curtime.strftime("%Y%m%d") + "00"

print datetime.utcnow() 
print ' CFSF initial time @ %s to be used. '%inidate 

#wkdir = '/raid2/fddasys/tmp/datatmp'
#os.chdir(wkdir)
#print os.getcwd()

locdir = args.locdir

datatypes = ['flxf','pgbf']
cfstypes = ['cfs.sfc','cfs.upr']

enmem = '01'

cfsfdir = '/raid1/static/CFSF/data'

cdate = sdate 
while cdate <= edate:
      cymd = cdate.strftime('%Y%m%d')
      year = cdate.strftime('%Y')
      month = cdate.strftime('%m')

#     FDIR = os.path.join(cfsfdir,year)
      FDIR = os.path.join(cfsfdir,inidate)

      for x, y in zip(datatypes, cfstypes):
          f = x + cymd + '*.' + enmem + '.' + inidate + '.grb2'
        # f = x + cymd + '*.' + enmem + '.*.grb2'
          print f, len(glob.glob(os.path.join(FDIR,x,f)))
          if len(glob.glob(os.path.join(FDIR,x,f))) <= 0: sys.exit()
          for eachf in glob.glob(os.path.join(FDIR,x,f)):
              match = re.search(r'\w(\d+)\.',eachf.split('/')[-1])
              if match:
                 newf = y + '.' + match.group(1) + '.grb2'
                 datapath = os.path.join(locdir,year,month)

                 if not os.path.isdir(datapath):
                    os.makedirs(datapath)

                 newf = os.path.join(datapath,newf)
                 print 'copy... ', eachf, newf 

                 os.system("rm -f %s"%newf)

                 shutil.copy2(eachf, newf)

      cdate = cdate + timedelta(days=1)

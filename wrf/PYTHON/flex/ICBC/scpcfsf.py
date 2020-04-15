#!/usr/bin/env python2.7
'''scp CFSR data from panassa into model runing directory 
   WWU wanliwu@ucar.edu Nov. 24 2014
   WWU wanliwu@ucar.edu Nov. 23 2015 fro CFSF
'''

import os
import sys
import argparse              # require python2.7 or later 
from datetime import datetime, timedelta
import subprocess

def runcmd(mycmd):
    p = subprocess.Popen(mycmd, shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
    return p.communicate()

print 'inside scpcfs.py: current directory -> %s'%os.getcwd()

parser = argparse.ArgumentParser(description='SCP  CFSF data for WRF')
parser.add_argument('-b', dest='startdate', type=str, default='',
                    help = 'start date ccyymmdd')
parser.add_argument('-e', dest='enddate', type=str, default='', 
                    help = 'end   date ccyymmdd')
parser.add_argument('-t', dest='inidate', type=str, default='', 
                    help = 'CFSF initial date')
parser.add_argument('-i', dest='datadir', type=str, default='/ngic/static/CFSF/data', 
                    help = 'CFSF data source directory on panasas')
parser.add_argument('-o', dest='rundir', type=str, default='/raid1/fddasys/cycles/GCW0202/WRF/GRM/data/CFSF', 
                    help = 'destination directory for copied CFSF data')

# data source:
# ngic-c2-int1.rap.ucar.edu:/ngic/static/CFSF/data/2015112200
#data destination directory:
#/raid1/fddasys/cycles_dev/GCW0202/WRF/GRM/data/CFSF
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

lhost = "ngic-c2-int2"
rdatdir = os.path.join(args.datadir, inidate)
ldatdir = args.rundir
intval  = 6
ensmem = 1

# scp cfs files from local host to WPS run directory 
print 'scp cfs files from %s:%s to WRF run directory.'%(lhost,rdatdir)
print 'inside scpcfs: ', sdate, edate, intval, lhost, rdatdir, ldatdir 
if not os.path.isdir(ldatdir):
   os.makedirs(ldatdir)
# clean the existing files
os.system('rm -f %s/*'%ldatdir)  

vtypedic = {}
vtypedic['flxf'] = 'sfc'
vtypedic['pgbf'] = 'upr'

cdate = sdate
while cdate <= edate:
    year = cdate.year
    month = cdate.month
    ymdir = '%04d/%02d'%(year, month)
    cymdh = '%04d%02d%02d%02d'%(cdate.year,cdate.month,cdate.day,cdate.hour)

    for vtype in ['flxf', 'pgbf']:
        frmt = '%s%s.%02d.%s.grb2'%(vtype, cymdh, ensmem, inidate)
        floc = 'cfs.%s.%s.grb2'%(vtypedic[vtype],cymdh)
        
        fr = os.path.join(rdatdir, vtype, frmt)
        fl = os.path.join(ldatdir, floc)
        cmd = 'scp -p %s:%s %s'%(lhost, fr, fl)
        out, err = runcmd(cmd) 
        if os.path.isfile(fl):
           print '%s is ready.'%fl
        else:
           print 'Cannot find file %s'%fl

    cdate = cdate + timedelta(hours = intval)

print 'Done with scpcfsf.py!'

#!/usr/bin/env python
"""
assemble wrf output per analysis/fcst hours: 1-6hours, 7-12, 13-18 amd 19-24,
then make soft link of them for statistics and plot
assume wrf model has 6 hourly cycles at [00, 06 12 and 18] UTC

  /d2/pmefdda/archive/softlinks/
  anal0106/ccyymmdd
  fcst0106/ccyymmdd
W. Wu (wanliwu@ucar.edu) May 1, 2013
"""
import os
import sys
import argparse
import glob
import pandas as pd 
from pandas import date_range 
from datetime import datetime, timedelta

parser = argparse.ArgumentParser(description='WRF output archive')
parser.add_argument('-i', dest='wrfidir', type=str, default='/d2/pmefdda/cycles/GWPME/GRM',
                    help = 'directory of wrfoutput')
parser.add_argument('-o', dest='wrfodir', type=str, default='/d2/pmefdda/archive/softlinks',
                    help = 'directory to archive wrfoutput')
parser.add_argument('-day', dest='offday', type=str, default='2',
                    help = 'offset days from present')
parser.add_argument('-hour', dest='offhour', type=str, default='0',
                    help = 'offset hours from present to make the starting time as one of cycle times')
parser.add_argument('-c', dest='cycles', type=str, nargs='*', default=['00','06','12','18'],
                    help = 'WRF model cycles to be archived')
parser.add_argument('-d', dest='dom', type=str, nargs='*', default=['d02','d03'],
                    help = 'WRF model domains to be archived')

args = parser.parse_args()

wrfoutdir = args.wrfidir
softlinkd = args.wrfodir

if type(args.cycles) == list:     
   wrfcycles = args.cycles                      # WRF model cycle time (UTC)
else:
   wrfcycles = []
   wrfcycles.append(args.cycles)

if type(args.dom) == list:
   doms = args.dom
else:
   doms = []
   doms.append(args.dom)

offday = int(args.offday) 
offhour = 24*offday + int(args.offhour) 

# cycle interval in hours
cintv = 6

if os.path.exists(softlinkd):
  pass
else:
  os.mkdir(softlinkd)

dom = doms[0]
aorf = ['WRF_P', 'WRF_F']

categ4 = ['fcst0106', 'fcst0712', 'fcst1318', 'fcst1924', 'anal0106']

ctimeutc = datetime.utcnow()
ptimeutc = datetime.utcnow() - timedelta(hours = offhour)
ftimeutc = datetime.utcnow() + timedelta(days = 1)       # one day to the future 
pdate =  ptimeutc.strftime('%Y%m%d%H')
fdate =  ftimeutc.strftime('%Y%m%d%H')
print pdate, fdate  
times = pd.date_range(start=ptimeutc, end=ftimeutc, freq = 'H')

for n, cate in enumerate(categ4):

  ddes = os.path.join(softlinkd, cate)
  if os.path.exists(ddes):
     cmd = 'rm -f ' + ddes + '/wrfout_d0*'              
     os.system(cmd)
  else:
       os.mkdir(ddes)

  if n >= 4:
     af = aorf[1]
     for k, onetime in enumerate(times):
        ih = int(onetime.strftime('%H'))
        if ih%6 == 0: 
           btimeutc = onetime + timedelta(hours = cintv)
           cycle = btimeutc.strftime('%Y%m%d%H')
        timstr = onetime.strftime('%Y-%m-%d_%H')
        for dom in doms:
          fwrf = 'wrfout_' + dom + '_' + timstr + ':00:00' 
          fsrc = os.path.join(wrfoutdir, cycle, af, fwrf)
          if os.path.exists(fsrc):
            fdes = os.path.join(ddes, fwrf)
            if os.path.exists(fdes): os.remove(fdes)
            cmd = 'ln -sf %s %s' %(fsrc,fdes)
            os.system(cmd)
  else:
     af = aorf[0]
     btimeutc = times[0] - timedelta(hours = (n+1) * cintv)
     cycle = btimeutc.strftime('%Y%m%d%H')
     for k, onetime in enumerate(times):
        ih = int(onetime.strftime('%H'))
        timstr = onetime.strftime('%Y-%m-%d_%H')
        for dom in doms:
          fwrf = 'wrfout_' + dom + '_' + timstr + ':00:00'
          fsrc = os.path.join(wrfoutdir, cycle, af, fwrf)
          if os.path.exists(fsrc):
            fdes = os.path.join(ddes, fwrf)
            if os.path.exists(fdes): os.remove(fdes)
            cmd = 'ln -sf %s %s' %(fsrc,fdes)
            os.system(cmd)
        if ih%6 == 0: 
           btimeutc = onetime - timedelta(hours = n*cintv)
           cycle = btimeutc.strftime('%Y%m%d%H')

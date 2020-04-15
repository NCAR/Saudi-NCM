#!/usr/bin/env python
'''update wps namelist: namelist.wps or wrf namelist: namelist.input
   edit namelist.input based on an template one 
   usage: python updatenamelist.py -h  for help 
   files directories specified in this script should be carefully edited 
   before executing the script.

   WWU wanliwu@icar.edu Nov. 05 2013 
   WWU wanliwu@ucar.edu Dec. 20 2013 Revised 
'''

import os
import sys
#import subprocess
import shutil
import argparse              # require python2.7 or later 
#import re
#from collections import OrderedDict
from datetime import datetime, timedelta
from wpylib import fornml

#main program here
# set up default vlaues for some fileds
parser = argparse.ArgumentParser(description='update WPS namelist')
parser.add_argument('-i', dest='nmlin', type=str, default='./namelist.wps', \
                    help = 'input: namelist.wps or namelist.input. default: ./namelist.wps')
parser.add_argument('-o', dest='nmlout', type=str, default='', \
                    help = 'output: namelist.wps or namelist.input. default: overwrite input')
parser.add_argument('-s', dest='secname', type=str, default='', help = 'namelist name &name')
parser.add_argument('-k', dest='varname', type=str, default='', help = 'namelist varaible')
parser.add_argument('-v', dest='varvalue', type=str, default=[], help = 'varaible values')
parser.add_argument('-n', dest='domains', type=int, default=0, \
                    help = 'number of domains. default: max_dom in the template namelist.wps')    
parser.add_argument('-b', dest='starttime', type=str, default='', help = 'WPS/WRF start time:ccyymmddhh')
parser.add_argument('-e', dest='endtime', type=str, default='', help = 'WPS/WRF end time:ccyymmddhh')

tmpnmldir = os.getcwd()
print 'Current directory: %s'%tmpnmldir

args = parser.parse_args()

numdoms = args.domains

args = parser.parse_args()
fnml = args.nmlin    
fnml = fnml.strip()
wrfnmldict = fornml.readwrfnml(cwrfnml=fnml)   # read in namelist file 

nmlout = args.nmlout 
if nmlout == '': nmlout = fnml 

sec = args.secname
var = args.varname
val = args.varvalue

if sec == 'share' or sec == 'domains':
   if numdoms <= 0:   # use the default max_dom defined in the template namelist file 
      numdoms = int(wrfnmldict[sec]['max_dom'])
   # update max_dom in the namelist file 
   wrfnmldict = fornml.updatewrfnml(wrfnmldict, nmlrec = sec, varname = 'max_dom', varvalu = str(numdoms))

if len(args.starttime) >= 4:  # update start and end time in the namelist file  
   btime, byear, bmonth, bday, bhour, bminute, bsecond = fornml.sdate2ymd(args.starttime)
   if len(args.endtime) <= 5:
      etime = btime + timedelta(hours=int(args.endtime))
      eyear, emonth, eday, ehour, eminute, esecond = \
      etime.year, etime.month, etime.day, etime.hour, etime.minute, etime.second
   else:
      etime, eyear, emonth, eday, ehour, eminute, esecond = fornml.sdate2ymd(args.endtime)
      if etime < btime:
         etime = btime + timedelta(hours=24)
         eyear, emonth, eday, ehour =  etime.year, etime.month, etime.day, etime.hour, etime.minute, etime.second
   if sec == 'share':
      #start_date = '2013-12-08_00:00:00', '2010-06-11_00:00:00', 
      rvalb = '%04d-%02d-%02d_%02d:%02d:%02d'%(byear, bmonth, bday, bhour, bminute, bsecond)
      rvale = '%04d-%02d-%02d_%02d:%02d:%02d'%(eyear, emonth, eday, ehour, eminute, esecond)
      print rvalb, rvale
      timerange = [rvalb, rvale]

      #update start and end time 
      vars = ['start_date', 'end_date']
      for var, v in zip(vars, timerange):
          val = []
          for n in range(numdoms):  
              val.append(v)
          val = fornml.list2str(val)
          wrfnmldict = fornml.updatewrfnml(wrfnmldict, nmlrec = sec, varname = var, varvalu = val)
   elif sec == 'domains':        # for WPF: namelist.input 
      pass                       # reserved place for further development 

else:
   wrfnmldict = fornml.updatewrfnml(wrfnmldict, nmlrec = sec, varname = var, varvalu = val)

# write out wrf namelist
fornml.writewrfnml(nmldict = wrfnmldict, wrfnml = nmlout)

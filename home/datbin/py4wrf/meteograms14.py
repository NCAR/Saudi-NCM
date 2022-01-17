#!/usr/bin/env python


"""
drive the python program to plot out real-time WRF meteograms.        
W. WU (wu80012@gmail.com)      
2013-05-08
used python dictionary 

""" 
import os
import sys
import math 
import time 

from datetime import datetime, timedelta
import argparse

parser = argparse.ArgumentParser(description='drive python program for meteograms plots ')
parser.add_argument('-f', dest='offhours', type=str, default='0',
                    help = 'offset hours from present')
parser.add_argument('-d', dest='dom', type=str, default='3',
                    help = 'domain to be processed')
args = parser.parse_args()
offhrs = int(args.offhours)
dnum = int(args.dom)

doms = [2,3]
dcsv = ['pmewrftable14_d02.csv', 'pmewrftable14_d03.csv']
cfreq = 6  # assume model runs every 6 hours at 00 06 12 18
ctimeutc = datetime.utcnow()
print ('start time: ', ctimeutc)
ptimeutc = datetime.utcnow() - timedelta(hours = offhrs)
cycle =  ptimeutc.strftime('%Y%m%d%H')
chr =  ptimeutc.strftime('%H')

ptype = 'png'

pysdir = '/home/x_fisherh/meteograms/py4wrf'
topdir = '/scratch/x_fisherh/cycles/GWPME/GRM'
outdir = topdir + '/meteograms'

pycmd = "python " + pysdir + "/plotmeteogram14.py "

if math.fmod(float(chr),float(cfreq)) != 0.0:
   sys.exit('no model run available at this time: %s' %cycle)
else:
   print ' plot meteograms for  forecasts issued at %s' %cycle
   # direction for png files
   pngdir = outdir + '/' + cycle
   if os.path.isdir(pngdir):
      pass
   else:
      cmd = 'mkdir -p %s' %pngdir 
      os.system(cmd)
   pngdirc = outdir + '/Current'
   if os.path.isdir(pngdirc):
      cmd = 'rm -rf %s' %pngdirc
      os.system(cmd)
   elif os.path.islink(pngdirc):
      os.unlink(pngdirc)
   cmd = 'ln -sf %s %s' %(pngdir,pngdirc)
   os.system(cmd)

   wrfdir = topdir + '/' + cycle 
   for dom, fcsv in zip (doms, dcsv):
       if dnum == int(dom): 
          dom = 'd%02d'%dom
          fcsv = os.path.join(pysdir, fcsv)
          pyexe = pycmd + dom + ' ' + fcsv + ' ' + wrfdir + ' ' + pngdir
          print pyexe 
          os.system(pyexe)
       else:
          pass 
ctimeutc = datetime.utcnow()
print ('end   time: ', ctimeutc)


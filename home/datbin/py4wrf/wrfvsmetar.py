#!/usr/bin/env python
""" 
WRF forecast timeseries for inter-domain comparison 
W. Wu (wu80012@gmail.com)
09-30-2012
Modified 05-2013 for WRF vs METAR comparison W. WU (wu80012@gmail.com)
"""
import numpy as np
import matplotlib; matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter, FormatStrFormatter
import os 
import sys
import pylab 
import pandas as pd 
import datetime 

import wwlpylib as wupy  

# === main program ====

import argparse

parser = argparse.ArgumentParser(description='WRF vs METAR')
parser.add_argument('-d', dest='wrfdom', type=str, default='2',
                    help = 'rank of wrf domain')
parser.add_argument('-c', dest='wrfcyc', type=str, default='2013052400',
                    help = 'wrf cycle: ccyymmddhh')

args = parser.parse_args()

domr = args.wrfdom 
fcyc = args.wrfcyc          

# direction for gif files
dirout = '/d2/pmefdda/cycles/GWPME/GRM/timesgraphs/' + fcyc
diroutc = '/d2/pmefdda/cycles/GWPME/GRM/timesgraphs/' + '/Current'
#dirout = '/home/pmefdda/py4wrf/testcodes/' + fcyc
#diroutc = '/home/pmefdda/py4wrf/testcodes/' + '/Current'

if not os.path.isdir(dirout):
   cmd = 'mkdir -p %s' %dirout
   os.system(cmd)

if os.path.isdir(diroutc):
   cmd ='rm -rf %s' %diroutc
   os.system(cmd)
elif os.path.islink(diroutc):
   os.unlink(diroutc)
cmd = 'ln -sf %s %s' %(dirout,diroutc)
os.system(cmd)

#if os.listdir(dirout) == []:
#    pass         
#else:
#   cmd = 'cp -p %s\/OE*.gif %s\/' %(dirout,diroutp)
#   os.system(cmd)

dom = 'd0' + domr
fwrf = 'wrfout_' + dom + '*:00:00'

varlist = ['T2', 'PSFC', 'Q2', 'U10', 'V10']
#offlist = [0.0, 0.0, 0.0, 0.0, 0.0]
scalelt = [1.0, 0.01, 1.0, 1.0, 1.0]
offlist = [273.16, 0.0, 0.0, 0.0, 0.0]
#scalelt = [1.0, 0.01, 1.0, 1.0, 1.0]
vardic = dict()
for k, o, s in zip(varlist, offlist, scalelt):
  vardic[k] = [o,s]

xintv = 6 

#fcsv = './d02_table.csv'
fcsv = '/home/pmefdda/py4wrf/pmewrftable_' + dom + '.csv'
#print 'fcsv = ', fcsv 
wrfdir = '/d2/pmefdda/archive/softlinks'
categ4 = ['anal0106', 'fcst0106', 'fcst0712', 'fcst1318', 'fcst1924']

datable = pd.read_csv(fcsv, sep=',', header = 0, index_col = 0)
stations = datable.index
#sitename = stations[int(sys.argv[1])]
for n, sitename in enumerate(stations):
  slat = datable.LAT[sitename]
  slon = datable.LON[sitename]
  j0   = int(datable.J[sitename])
  i0   = int(datable.I[sitename])
  print n+1, sitename, slat, slon, j0, i0  

# WRF time series 
  wrffiles = os.path.join(wrfdir, categ4[0], fwrf)
  print 'process: ' + wrffiles
  colname = ['T2F0', 'PS0', 'QF0', 'U100', 'V100', 'WSF0', 'WDF0', 'RHF0', 'TDF0']
  cate0 = wupy.mwrfvartimeseries(wrffiles, vardic=vardic, colnames=colname, j=j0, i=i0)
  wrffiles = os.path.join(wrfdir, categ4[1], fwrf)
  print 'process: ' + wrffiles
  colname = ['T2F1', 'PS1', 'QF1', 'U101', 'V101', 'WSF1', 'WDF1', 'RHF1', 'TDF1']
  cate1 = wupy.mwrfvartimeseries(wrffiles, vardic=vardic, colnames=colname, j=j0, i=i0)
  wrffiles = os.path.join(wrfdir, categ4[2], fwrf)
  print 'process: ' + wrffiles
  colname = ['T2F2', 'PS2', 'QF2', 'U102', 'V102','WSF2', 'WDF2', 'RHF2', 'TDF2']
  cate2 = wupy.mwrfvartimeseries(wrffiles, vardic=vardic, colnames=colname, j=j0, i=i0)
  wrffiles = os.path.join(wrfdir, categ4[3], fwrf)
  print 'process: ' + wrffiles
  colname = ['T2F3', 'PS3', 'QF3', 'U103', 'V103','WSF3', 'WDF3', 'RHF3', 'TDF3']
  cate3 = wupy.mwrfvartimeseries(wrffiles, vardic=vardic, colnames=colname, j=j0, i=i0)
  wrffiles = os.path.join(wrfdir, categ4[4], fwrf)
  print 'process: ' + wrffiles
  colname = ['T2F4', 'PS4', 'QF4', 'U104', 'V104','WSF4', 'WDF4', 'RHF4', 'TDF4']
  cate4 = wupy.mwrfvartimeseries(wrffiles, vardic=vardic, colnames=colname, j=j0, i=i0)

# METAR OBS timeseries 
  print 'request METAR data on the fly and process it now.'
  try:
      obsdat = wupy.metarobs(icao=sitename, hours=80)
  except:
      print 'no METAR data at %s'%sitename
      obsdat = np.nan

# print obsdat.columns
# align  WRF and METAR timeseries 
# concatnating:
  print 'prepare for plotting now'
  T2 = pd.concat([np.float32(obsdat['T']),cate0['T2F0'],cate1['T2F1'],cate2['T2F2'],cate3['T2F3'],cate4['T2F4']], axis=1, join='outer')
  PS = pd.concat([np.float32(obsdat['SLP']),cate0['PS0'],cate1['PS1'],cate2['PS2'],cate3['PS3'],cate4['PS4']], axis=1, join='outer')
  TD = pd.concat([np.float32(obsdat['TD']),cate0['TDF0'],cate1['TDF1'],cate2['TDF2'],cate3['TDF3'],cate4['TDF4']], axis=1, join='outer')
  RH = pd.concat([np.float32(obsdat['RH']),cate0['RHF0'],cate1['RHF1'],cate2['RHF2'],cate3['RHF3'],cate4['RHF4']], axis=1, join='outer')
  WS = pd.concat([np.float32(obsdat['WSPD']),cate0['WSF0'],cate1['WSF1'],cate2['WSF2'],cate3['WSF3'],cate4['WSF4']], axis=1, join='outer')
  WD = pd.concat([np.float32(obsdat['WDIR']),cate0['WDF0'],cate1['WDF1'],cate2['WDF2'],cate3['WDF3'],cate4['WDF4']], axis=1, join='outer')
  csvout = sitename + '_' + dom + '_T2'
  filecsv = os.path.join(dirout, csvout + '.txt')
  T2.to_csv(filecsv,sep=' ')
  csvout = sitename + '_' + dom + '_PS'
  filecsv = os.path.join(dirout, csvout + '.txt')
  PS.to_csv(filecsv,sep=' ')
  csvout = sitename + '_' + dom + '_TD'
  filecsv = os.path.join(dirout, csvout + '.txt')
  TD.to_csv(filecsv,sep=' ')
  csvout = sitename + '_' + dom + '_RH'
  filecsv = os.path.join(dirout, csvout + '.txt')
  RH.to_csv(filecsv,sep=' ')
  csvout = sitename + '_' + dom + '_WS'
  filecsv = os.path.join(dirout, csvout + '.txt')
  WS.to_csv(filecsv,sep=' ')
  csvout = sitename + '_' + dom + '_WD'
  filecsv = os.path.join(dirout, csvout + '.txt')
  WD.to_csv(filecsv,sep=' ')
  
  times = T2.index
  nfiles = len(times)

  hourticks = range(0,nfiles)

  ctimeutc = datetime.datetime.utcnow()
  chh = ctimeutc.strftime('%Y%m%d%H')
# print 'chh = ', chh 
  for i, xh in enumerate(times):
    hh = xh.strftime('%Y%m%d%H')
    if hh == chh:
       xi = i
       break
# print 'xi = ', xi 

  xlabs = []
  for xh in times:
    hh = xh.strftime('%H')
    if int(hh)%xintv == 0:
       if hh == '00':
          xlabs.append(xh.strftime('%m/%d'))
       else:
          xlabs.append(hh)
    else:
       xlabs.append(' ')

# graphics:
  print 'ready for graphics now'  
  fig = plt.figure(figsize=(12,10))
  ax = fig.add_subplot(411)

  ax.plot(hourticks, T2['T'], 'r*', T2['T2F0'], 'ko', hourticks, T2['T2F1'], 'bo', hourticks, T2['T2F2'], 'go', hourticks, T2['T2F3'], 'yo',  hourticks, T2['T2F4'],'ro')
  ax.set_xlim([-3,nfiles+2])

  plt.xticks(hourticks, xlabs)
  ax.grid(True)
  ax.set_ylabel('Air Temperature (C) ')
  x1,x2,y1,y2 = plt.axis()
  plt.vlines(xi,y1,y2, color='black', lw=2)    # add vertical line indicating current time

  gtitle = '(%5.2fN,%5.2fE)' %(float(slat), float(slon))
  gtitle = ' WRF and METAR timeseries ' + sitename + ' ' + gtitle
  ax.set_title(gtitle)


  leg = ax.legend(('metar obs', 'analysis', '6 hrs FCST ', '12 hrs FCST', '18 hrs FCST', '24 hrs FCST'),
#          loc=0, shadow=True, prop={'size':6})
            'upper right', shadow=True, prop={'size':6},bbox_to_anchor=(1.10,1.55))

  ax = fig.add_subplot(412)

# ax.plot(hourticks, PS['PS0'], 'ko', hourticks, PS['PS1'], 'bo', hourticks, PS['PS2'], 'go', hourticks, PS['PS3'], 'yo',  hourticks, PS['PS4'],'ro')
# ax.plot(hourticks, TD['TD'], 'r*', TD['TDF0'], 'ko', hourticks, TD['TDF1'], 'bo', hourticks, TD['TDF2'], 'go', hourticks, TD['TDF3'], 'yo',  hourticks, TD['TDF4'],'ro')
  ax.plot(hourticks, RH['RH'], 'r*', RH['RHF0'], 'ko', hourticks, RH['RHF1'], 'bo', hourticks, RH['RHF2'], 'go', hourticks, RH['RHF3'], 'yo',  hourticks, RH['RHF4'],'ro')

  ax.set_xlim([-3,nfiles+2])
  plt.xticks(hourticks, xlabs)
  ax.grid(True)
# ax.set_xlabel('TIME (UTC) ')
  ax.yaxis.tick_right()
  ax.yaxis.set_label_position("right")
  ax.yaxis.set_major_formatter(FormatStrFormatter('%0.0f'))
  ax.set_ylabel('Relative Humidity (%)')
  x1,x2,y1,y2 = plt.axis()
  plt.vlines(xi,y1,y2, color='black', lw=2)

  ax = fig.add_subplot(413)
  ax.plot(hourticks, WS['WSPD'], 'r*', WS['WSF0'], 'ko', hourticks, WS['WSF1'], 'bo', hourticks, WS['WSF2'], 'go', hourticks, WS['WSF3'], 'yo',  hourticks, WS['WSF4'],'ro')

  ax.set_xlim([-3,nfiles+2])
  plt.xticks(hourticks, xlabs)
  ax.grid(True)
  ax.yaxis.set_major_formatter(FormatStrFormatter('%0.0f'))
  ax.set_ylabel('Wind Speed (m/s)')
  x1,x2,y1,y2 = plt.axis()
  plt.vlines(xi,y1,y2, color='black', lw=2)

  ax = fig.add_subplot(414)
  ax.plot(hourticks, WD['WDIR'], 'r*', WD['WDF0'], 'ko', hourticks, WD['WDF1'], 'bo', hourticks, WD['WDF2'], 'go', hourticks, WD['WDF3'], 'yo',  hourticks, WD['WDF4'],'ro')

  ax.set_xlim([-3,nfiles+2])
  plt.xticks(hourticks, xlabs)
  ax.grid(True)
  ax.set_xlabel('TIME (UTC) ')
  ax.yaxis.tick_right()
  ax.yaxis.set_label_position("right")
  ax.yaxis.set_major_formatter(FormatStrFormatter('%0.0f'))
  ax.set_ylabel('Wind Direction')
  ax.set_ylim([0,360])
  pylab.yticks([0.,45.,90.,135.,180.,225.,270.,315.,360], ['N','NE','E','SE','S','SW','W','NW','N'])

  x1,x2,y1,y2 = plt.axis()
  plt.vlines(xi,y1,y2, color='black', lw=2)
# the matplotlib.patches.Rectangle instance surrounding the legend
  frame  = leg.get_frame()
  frame.set_facecolor('0.80')    # set the frame face color to light gray

# matplotlib.text.Text instances
  for t in leg.get_texts():
    t.set_fontsize('small')    # the legend text fontsize

# matplotlib.lines.Line2D instances
  for l in leg.get_lines():
    l.set_linewidth(1.5)  # the legend line width

# fileout = sitename + '_wrfvsobs'
  fileout = sitename + '_' + dom 
# filepng = fileout + '.png'
  filepng = os.path.join(dirout, fileout + '.png')
  if os.path.exists(filepng):
    os.system('rm -f %s' %filepng)

  plt.savefig(filepng)
# os.system('convert -render -flatten %s %s' % (filepng, filegif))
  os.system('rm -f /home/pmefdda/metar_OE*.txt')
  plt.show()
  plt.close()

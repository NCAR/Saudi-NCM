#!/usr/bin/env python
""" 
WU PYTHON LIBRARY: wwlpylib
W. Wu (wu80012@gmail.com)
Version 0.01 09-30-2012
Version 0.05 06-01-2013
"""

import numpy as np
#import matplotlib; matplotlib.use('Agg')
#import matplotlib.pyplot as plt
#from matplotlib.ticker import ScalarFormatter, FormatStrFormatter
import os 
import sys
from netCDF4 import MFDataset
#import pylab 
import pandas as pd 
import math 

def wdate2pdate(wrffile):
#   convert WRF date stamps (as characters) to python datetime
    from datetime import datetime
    from netCDF4 import chartostring

    datewrf = wrffile.variables['Times']
    ndates = len(datewrf[:,0])
    
    datelist = []
    for n in range(ndates):
        wrfdate =str(chartostring(datewrf[n,:]))
        ones = wrfdate.replace("_", " ")
        pytm = datetime.strptime(ones, "%Y-%m-%d %H:%M:%S")
        datelist.append(pytm)
    return datelist

def wrf_timeseries(wrffile, varname, i, j, offs, scale):
    from netCDF4 import Dataset

# generate time series for given location (i,j indices)
# offs: offset; scale: scale 

    k  = 0 

    varts = []
    times = []
    for ncfile in wrffile:
        wrfdat = Dataset(ncfile)
        a =scale * (wrfdat.variables[varname][k,j,i] -offs)
        varts.append(a)
        times.append(wrfdat.variables['Times'][k])
    return varts, times

def wrf_wind(sfcU, sfcV):
# wind speed, wind direction:

    import numpy as np

    import math 
    wspd = np.power(np.add(np.power(sfcU,2),np.power(sfcV,2)),0.5)      

    # Calculate a wind direction from u and v
    rfactor = 180.0 / math.pi 
    wdir = []
    for u,v in zip(sfcU, sfcV):    
        if u == 0 and v >= 0:
           wdir.append(180)
        elif u == 0 and v < 0:
           wdir.append(0)
        else:
           dir0 = math.atan2(-v, u) * rfactor + 270 
           if dir0 >= 360:
              wdir.append(dir0 - 360)
           else:
              wdir.append(dir0)

#   for u, v, s, d in zip(sfcU, sfcV, wspd, wdir):
#       print u, v, s, d
    return wspd, wdir

def uv2wswd(u, v):
    """Convert u, v wind to wind speed and wind direction.
    u , v : u, v wind.
    Returns: ws, wd
    ws: wind speed
    wd: wind direction (in degrees, north wind is 0, east wind is 90, etc)
    """
    ws = np.hypot(u, v)
    wd = np.fmod(np.rad2deg(np.arctan2(u, v)) + 180.0 , 360.0)
#   wd = np.fmod(np.rad2deg(np.arctan2(-v,u)) + 270.0 , 360.0)
    return ws, wd

def wrf_timelable(wrftimes):

    hours = []
    dates = []
    dateticks = []
    n = -1
    for time in wrftimes:
        syear = str(time[0])+str(time[1])+str(time[2])+str(time[3])
        smon  = str(time[5])+str(time[6])
        sday  = str(time[8])+str(time[9])
        shour = str(time[11])+str(time[12])
        symdh = syear + smon + sday + shour
        n += 1
        if n%6 == 0:               #tick mark every three
           if shour == '00': hours.append(sday + '/' + smon)
           else: hours.append(shour)
        else:
           hours.append(" ")
        if shour == '00':
           dateticks.append(n)
           dates.append(sday + '/' + smon)
    return hours, dates, dateticks 

def wrf2dvartimeseries(wrfall, wrfvar = 'T2', offset = 0, scale = 1, j = 0, i = 0):
    import numpy as np 
    from pandas import Series 

# extracting WRF 2-D fields timeseries by given grid index (j,i) "

    wval = np.subtract(wrfall.variables[wrfvar][:,j,i], offset)
    wval = np.multiply(wval, scale)

    datelist = wdate2pdate(wrfall)

    return  Series(wval, index=datelist)

def mwrfvartimeseries(wrffiles, vardic = {'T2':[273.16,1.0]}, colnames=['T2','PS','U10','V10'], j = 0, i = 0):
    from netCDF4 import MFDataset
    import wufortranm 

#" extracting WRF 2-D fields timeseries by given grid index (j,i) "

    wrfall = MFDataset(wrffiles) 

    for n, varname in enumerate(vardic.keys()):
        offset = vardic[varname][0]
        scale  = vardic[varname][1]
    #   print varname, offset, scale, j, i  
        TimeS = wrf2dvartimeseries(wrfall, wrfvar = varname, offset = offset, scale = scale, j = j, i = i)
        if n == 0:
           wrf2dts = pd.DataFrame(TimeS, index=TimeS.index, columns=[varname])
        else:
           wrf2dts[varname] = TimeS
# replace (u.v) with wind speed and wind direction 
    ws, wd = uv2wswd(wrf2dts['U10'],wrf2dts['V10'])
    wrf2dts['WS'] = ws
    wrf2dts['WD'] = wd

    wrfall.close()

# computer RH, TD
#   SUBROUTINE RHANDTD(QV, PRES, T, RH, TD, DIMSZ)
    rh, td = wufortranm.rhandtd(wrf2dts['Q2'], wrf2dts['PSFC'], wrf2dts['T2'])
    wrf2dts['RH'] = rh
    wrf2dts['TD'] = td 
#   for a1, a2, a3, a4, a5 in zip(wrf2dts['T2'], wrf2dts['Q2'], wrf2dts['PSFC'], wrf2dts['RH'], wrf2dts['TD']):
#       print a1, a2, a3, a4, a5

    wrf2dts = wrf2dts.sort_index(axis=1)
#   print wrf2dts.head()
    
    alist = []
    for col in wrf2dts.columns:
        alist.append(col)
   
    blist = colnames.sort() 
#   print blist 

    coldic = dict(zip(alist, colnames))
#   print coldic
#   print coldic
    wrf2dts = wrf2dts.rename(columns=coldic)
#   print wrf2dts.head()
#   sys.exit() 

    return wrf2dts 

def get_metardata(icao='OEJN',hours=1, outfpre = 'metar'):
    import urllib2
    import re
    import os
    import datetime
    
    metarurl = 'http://aviationweather.gov/adds/metars/?station_ids=%s&std_trans=standard&chk_metars=on&hoursStr=past+%d+hours&submitmet=Submit' %(icao,hours)
    tempfile = outfpre + '.txt'
    
    req = urllib2.Request(metarurl)
    try: 
        f = urllib2.urlopen(req)
        content = f.read()
        f.close()
        with open(tempfile,'wt')as fout:
            fout.write(content)
        ctimeutc = datetime.datetime.utcnow()
        cdateh = ctimeutc.strftime('%Y%m%d%H')
        fname = outfpre + '_' + cdateh + '.txt'
        if os.path.isfile(fname): os.remove(fname)
        metardat = open(fname,'at')
        with open(tempfile, 'r') as fdat:
            for line in fdat.readlines():
                if re.search(icao,line):
                    bline = re.search(icao,line)
                    bpos = bline.start()
                    if re.search('</FONT>',line):
                       eline = re.search('</FONT>',line)
                       epos = eline.start()
                    #print line[bpos:epos]
                       metardat.write(line[bpos:epos] + '\n')
                    else:
                        print line 
        metardat.close()
        if os.path.isfile(tempfile): os.remove(tempfile)
                        
            
    except urllib2.URLError as e:
        print e.reason
        fname = ' '
    return fname 

def metardecode(fmetar='metar_OEJN_2013051219.txt'):
    import re
    decodedat = []
    firstrow = 'DDHHMM' + ' SLP' + ' T' + ' TD' + ' WDIR' + ' WSPD' + ' WUNI'
    decodedat.append(firstrow)
    with open(fmetar,'r') as fi:
        for line in fi.readlines():
          mz = re.search("\d+Z", line)
          mp = re.search("Q\d+", line)
          if mz == None or mp == None:
             print 'this record was discarded in decoding:\n', line 
             pass
          else:
            ddhhmm, pres = mz.group(0)[:-1], mp.group(0)[1:]

#VRB: wind direction indicator when windspeed is less than 3KT
#                              or 
#     during a violent thunderstorm when wind direction can not be determined.
            mw = re.search("VRB\d+KT",line)
            if mw == None:
               mw = re.search("VRB\d+G\d+KT",line)
            if mw == None:
               mw = re.search("\d+G\d+KT",line)
            if mw == None:
               mw = re.search("\d+KT", line)
            if mw == None:
               print 'this record was discarded in decoding:\n', line 
               pass
            else: 
               wdir, wspd, wuni = mw.group(0)[0:3],  mw.group(0)[3:5],  mw.group(0)[-2:]

               mt = re.search("\d+/\d+", line)
               if mt == None:
                  mt = re.search("\d+/M\d+", line)
               if mt == None:
                  mt = re.search("M\d+/M\d+", line)
               if mt != None:
                  tc, tdc = mt.group(0).split('/')
                  if tc.startswith('M'): tc = '-' + tc[1:]
                  if tdc.startswith('M'): tdc = '-' + tdc[1:]
               if mt == None:
                  print 'this record was discarded in decoding:\n', line 
                  pass
               else:
                  allflds = ddhhmm + ' ' + pres + ' ' + tc + ' ' + tdc + ' ' +  wdir + ' ' +  wspd + ' ' + wuni
                  decodedat.append(allflds)
    return decodedat

def metarobs(icao='OEJN', hours=99):

    import pandas as pd
    import numpy as np
    import os
    import sys
    import datetime 
    import wufortranm

    outfpre = 'metar_' + icao
    fdata = get_metardata(str(icao), hours, outfpre)

    if os.path.isfile(fdata):
       decodedf = metardecode(fdata)
#decodedf = metardecode('metar_OEJN_2013051219.txt')
       ctimeutc = datetime.datetime.utcnow()
       cyymm = ctimeutc.strftime('%Y%m')
       iday0 = int(ctimeutc.strftime('%d'))

       atimes = []
       slp = np.zeros(shape=len(decodedf)-1)
       t = np.zeros(shape=len(decodedf)-1)
       td = np.zeros(shape=len(decodedf)-1)
       wsp = np.zeros(shape=len(decodedf)-1)
       wdr = np.zeros(shape=len(decodedf)-1)
       datcols = decodedf[0]

 #     for n, line in enumerate(sorted(decodedf[1:])):
       for n, line in enumerate(decodedf[1:]):
           ddhhmm, pres, tc, tcd, wdir, wspd, wuni = line.split(' ')
           iday = int(ddhhmm[0:2])
           if iday <= iday0:
              fdate = cyymm + ddhhmm[0:4]
           else:
              today = datetime.datetime.utcnow()
              firstofmonth = datetime.datetime(today.year, today.month, 1)
              lastmonth = firstofmonth - datetime.timedelta(days=1)
              cyymm = lastmonth.strftime('%Y%m')
              fdate = cyymm + ddhhmm[0:4]
              iday0 = iday
#             sys.exit()
           atimes.append(datetime.datetime(int(fdate[0:4]), int(fdate[4:6]), int(fdate[6:8]), int(fdate[8:10])))
         # slp[n] = int(pres)
         # t[n] = int(tc)
         # td[n] = int(tcd)
         # wdr[n] = int(wdir)
         # wsp[n] = int(wspd)
           slp[n] = np.float32(pres)
           t[n] = np.float32(tc)
           td[n] = np.float32(tcd)
           if wdir == 'VRB':
              wdr[n] = np.nan
           else:
              wdr[n] = np.float32(wdir)
           wsp[n] = np.float32(wspd)
       rh = wufortranm.rhfromtd(t, td)        # derive RH from T and Td using fortran module
       mdata = pd.DataFrame(slp, index=atimes, columns=['SLP'])
       mdata['T'] = t
       mdata['TD'] = td
       mdata['RH'] = rh 
       mdata['WDIR'] = wdr
       if wuni == 'KT':
          print 'converted METAR wind speed in knot (KT) into m/s (MPS) !'
          mdata['WSPD'] = wsp * 0.514444           # knot -> m/s 
       else:
          mdata['WSPD'] = wsp
     # print 'checking if duplicated'
       mdata['index'] = mdata.index
       mdata.drop_duplicates(cols='index',take_last=True, inplace=True)
       mdata = mdata.drop(['index'],axis=1)
#      print mdata.index[0], mdata.index[-1]
#      print mdata.head()
#      print mdata.tail()
       times = pd.date_range(start=mdata.index[-1], end=mdata.index[0], freq='H')  
#      if len(atimes) < len(times): print ('missing data !')
       fdat = pd.DataFrame(np.nan, index=times, columns=['FDAT'])
       obsdat = pd.concat([fdat,mdata], axis=1)
       obsdat = obsdat.drop(['FDAT'],axis=1)
       os.remove(fdata)
    return obsdat 

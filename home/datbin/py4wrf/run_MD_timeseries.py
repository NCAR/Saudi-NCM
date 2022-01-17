#!/usr/bin/env python 

""" generate WRF forecast timeseries at stations/cities
    W. WU Sept. 14 2012 wanliwu@ucar.edu   """

import sys
import os 

def time2cycle(wrfcycle = 6, offhours = 0):
# for given time (date) and offset hours to derive the closest WRF cycle

    from datetime import datetime, timedelta

    curtime = str(datetime.now())
    bkptime = str(datetime.now() + timedelta(hours=offhours))
    
    # split stime into year, month, day and hour
    (year, month, day, hour) = (bkptime[0:4], bkptime[5:7], bkptime[8:10], bkptime[11:13])
    ihour = int(hour)
    ihour -= ihour%wrfcycle
   
    if ihour < 10:
       shour = "0" + str(ihour)
    else:
       shour = str(ihour)
    curcycle = year + month + day + shour
  
    return curtime, bkptime, curcycle 

# main progam 

# specify city/staton names to be processed for time series

cities = ['Abha', 'Madinah', 'Jeddah', 'Riyadh', 'Dammam']

# negative means backward offset hours 
bkhours = -int(sys.argv[1])

(curtime, bkptime, curcycle) = time2cycle(wrfcycle = 6, offhours = bkhours)

print 'Current time (UTC):   %s' %curtime
print 'WRF cycle to be processed: %8d' %int(curcycle)

# path for python source codes:
wupydir = '/home/pmefdda/py4wrf'

pyfile = os.path.join(wupydir, 'plot_MD_timeseries.py')

# process timeseries plots city by city (station by station)

for city in cities:

   os.system('python %s %s %s' %(pyfile, curcycle, city))

#  --- the end --- 

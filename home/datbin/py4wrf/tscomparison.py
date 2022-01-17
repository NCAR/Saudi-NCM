#!/usr/bin/env python

# usage: python ts....py 3 5 2
#                        backwad 3 days and 5 hours for domain 2 
import os
import sys
from datetime import datetime, timedelta

srcdir = '/home/pmefdda/py4wrf'

pylinks = 'assemblewrffcst.py'
timess  = 'wrfvsmetar.py'

flink = os.path.join(srcdir,pylinks)
ftime = os.path.join(srcdir,timess)

# make soft links WRF files
cmd = 'python ' + flink + ' -day %s' %sys.argv[1] + ' -hour %s' %sys.argv[2]
print cmd 
os.system(cmd) 

# timeseries and graphics 
timeutc = datetime.utcnow() - timedelta(hours = int(sys.argv[2]))
fcycle =  timeutc.strftime('%Y%m%d%H')
print 'WRF most recent finished cycle: ', fcycle 

cmd = 'python ' + ftime + ' -d %s -c %s' % (sys.argv[3], fcycle)  
print cmd 
os.system(cmd) 


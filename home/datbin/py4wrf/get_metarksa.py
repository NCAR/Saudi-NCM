#!/usr/bin/env python 

""" 
program to get METAR OBS on the fly, then process and save in text file for
decoding.
May 12 2013
W. Wu (wu80012@gmail.com)
"""

def get_metardata(icao='OEJN',hours=1, outfpre = 'metar'):
    import urllib2
    import re
    import os
    from datetime import datetime, timedelta
    
    metarurl = 'http://aviationweather.gov/adds/metars/?station_ids=%s&std_trans=standard&chk_metars=on&hoursStr=past+%d+hours&submitmet=Submit' %(icao,hours)
    #tempfile = icao + '.txt'
    tempfile = outfpre + '.txt'
    
    req = urllib2.Request(metarurl)
    try: 
        f = urllib2.urlopen(req)
        content = f.read()
        f.close()
        with open(tempfile,'wt')as fout:
            fout.write(content)
        ctimeutc = datetime.utcnow()
        cdateh = ctimeutc.strftime('%Y%m%d%H')
        #metardat = open('metar_' + icao + '_' + cdateh + '.txt','wt')
        metardat = open(outfpre + cdateh + '.txt','at')
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
    
    
import pandas as pd
import numpy as np
import os
import sys

icao = 'OEMK'
#icao = '~sa'
hours = 1
fcsv = '/home/pmefdda/py4wrf/pmewrftable_d02.csv'
datable = pd.read_csv(fcsv, sep=',', header = 0, index_col = 0)
stations = datable.index

#stations = ['OEJN','OEMA','OEMK','OERR']
#output text file:
outfpre = '/d1/pmefdda/metarksa/metar_ksa'
os.chdir('/home/pmefdda/py4wrf')
for n, icao in enumerate(stations):
# print n, icao
  if ( str(icao) == 'OEJB'):
     # non METAR station
     pass
  else: get_metardata(str(icao), hours, outfpre)

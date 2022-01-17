#!/usr/bin/env python

import sys
import re
import pandas as pd 

stationinfo = './metar_sa_stations.txt'
icao = []
wmoid = []
city = []
contry =[]
latdms = []
latdec = []
londms = []
londec = []
eleinm = []
eleinf = []

sf = open(stationinfo, 'r')
for rowl in sf.readlines():
    line = rowl.strip()
    if line.startswith('OE'):
      astr = line.split(' ')
      icao.append(astr[0][0:4])
    elif line.startswith('Lat'):
      p1 = line.find(':')
      p2 = line.find('N')
      latdms.append(line[p1+2:p2+1])
      p1 = line.find('),')
      p2 = line.find('(decimal)')
      latdec.append(line[p1+3:p2-2]) 
    elif line.startswith('Lon'):
      p1 = line.find(':')
      p2 = line.find('E')
      londms.append(line[p1+2:p2+1])
      p1 = line.find('),')
      p2 = line.find('(decimal)')
      londec.append(line[p1+3:p2-2]) 
    elif line.startswith('Ele'):
      p1 = line.find(':')
      p2 = line.find('metres')
      eleinm.append(line[p1+2:p2-1])
      p1 = line.find('(')
      p2 = line.find('feet')
      eleinf.append(line[p1+1:p2-1])
    elif line.startswith('WMO'):
      astr = line.split(':')
      wmoid.append(astr[1])
    elif line.startswith('Loc'):
      p1 = line.find(':')
      p2 = line.find(',')
      city.append(line[p1+2:p2])
      contry.append(line[p2+2:])
#     sys.exit('checking 4th line !')
numsta = len(icao)
for n, oe, nu, lat, lon, ele, cy, con in zip(range(numsta), icao, wmoid, latdec, londec, eleinm, city, contry):
    print n+1, oe, nu, lat, lon, ele, cy, con
statable = pd.DataFrame(wmoid, index = list(icao), columns='WMOID')
statable['LAT'] = latdec
statable['LON'] = londec
statable['ELE'] = eleinm 
statable['CITY'] = city   
statable['CONTRY'] = contry 
print statable 

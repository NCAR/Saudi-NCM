def wmostations(wmofile = './icao_sa.txt'):
# build a dictionary holds WMO station infomation
# WMO station information was put in a python dictionary: stationlist
# the text file: wmofile,  was from http://weather1.pme.gov.sa/cfo_default.html 
# Wanli Wu (wu80012@gmail.com)
# 09-14-2012

    icaofile = open(wmofile)
    headerline = icaofile.readline()
    lowline = headerline.lower()
    kcols = lowline.split()
    kcols[1] = 'wmoid'
    kcols[3] = 'lon'
    kcols[4] = 'elev'

    stationlist = {}
    while 1:

        line = icaofile.readline()
        cols = line.split()
        numcols = len(cols)
        if numcols == 0:
           break 

        try:
            wmoid = int(cols[2])
            wmoid = True
        except:
            wmoid = False 

        if wmoid:
           country = cols[-2] + ' ' + cols[-1]       # country is named in two words

           if numcols > 9:
              city = cols[6] + ' ' + cols[7]
           else:
              city = cols[6]
           print city, country 

           lats = cols[3]
           latdeg = float(lats[0:2])   
           latmin = float(lats[3:5])   
           nshemi = lats[-1]

           if nshemi == 'N':
              latf = latdeg + latmin / 60.0
           elif nshemi == 'S':
              latf = - (latdeg + latmin / 60.0)
           else:
              print ' only "N" or "S" can be in latitudes, but you have %s ' % nshemi
              sys.exit()
           lats = '%0.4f' % latf

           lons = cols[4]
           if len(lons) < 7:
              londeg = float(lons[0:2])   
              lonmin = float(lons[3:5])   
           else:
              londeg = float(lons[0:3])   
              lonmin = float(lons[4:6])   
           ewhemi = lons[-1]

           if ewhemi == 'E':
              lonf = londeg + lonmin / 60.0
           elif ewhemo == 'W':
              lonf = - (londeg + lonmin / 60.0)
           else:
              print ' only "E" or "W" can be in longitudes, but you have %s ' % ewhemi
              sys.exit()
           lons = '%0.4f' % lonf

           terrain = '%04d' % int(cols[5])
           stationinfo = {kcols[0]:cols[0],kcols[1]:cols[2],kcols[2]:lats,kcols[3]:lons,kcols[4]:terrain}
           stationone = {city:stationinfo}
#          print stationone 
           stationlist.update(stationone)
#          print stationlist 
        else:
           pass
    return stationlist


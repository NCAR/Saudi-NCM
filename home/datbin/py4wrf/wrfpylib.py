"""
utility python codes for WRF.
Wanli Wu (wu80012@gmail.com)
09-2012
"""

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
        if n%3 == 0:               #tick mark every three
           hours.append(shour)
        else:
           hours.append(" ")
        if shour == '00':
           dateticks.append(n)
           dates.append(sday + '/' + smon)
    return hours, dates, dateticks 

def lonlat2ij(wrffile, stationlist):

    import numpy as np
    from netCDF4 import Dataset

    sitelist = stationlist.keys()
    sitelist.sort()

    gfile = Dataset(wrffile)

    try:
        lat2d = gfile.variables['XLAT_M'][0]
        lon2d = gfile.variables['XLONG_M'][0]
    except:
        lat2d = gfile.variables['XLAT'][0]
        lon2d = gfile.variables['XLONG'][0]

    siteij = {}
    for sitename in sitelist:   
        sinfo = stationlist[sitename]
        print sinfo['lat'], sinfo['lon']
        (lat0, lon0) = float(sinfo['lat']), float(sinfo['lon'])
        diflat = np.subtract(lat2d, lat0)
        diflon = np.subtract(lon2d, lon0)
        latp2 = np.power(diflat,2)
        lonp2 = np.power(diflon,2)
        dists = np.sqrt(np.add(latp2,lonp2))
#       print np.ma.min(dists)
        (ny,nx) = np.size(dists, axis=0), np.size(dists, axis=1)
#       print dists.ndim, ny, nx
        index_min = dists.argmin()
        (j,i)= np.unravel_index(index_min,(ny,nx))
#       print sitename, lat0, lon0, j, i
        ijinfo = {'ilon':i, 'jlat':j}
        siteone = {sitename:ijinfo}
        siteij.update(siteone)
    return siteij 

def wrflonlat2ij(wrfdir, alldoms, stationlist):      

    import os

    domsiteij = {}

    for dom in alldoms:
        file  = 'geo_em.' + dom + '.nc'
        geofile = os.path.join(wrfdir,file)
        print geofile
        siteij = lonlat2ij(geofile,stationlist)
        domsiteij.update({dom:siteij})

# printing

#   for sitename in sitelist:
#       sinfo = stationlist[sitename]
#       for dom in alldoms:
#           dsite = domsiteij[dom]
#           site = dsite[sitename]
#           print sitename, sinfo['lat'], sinfo['lon'], dom, site['jlat'], site['ilon']

    return domsiteij

def dewpointT(tc, rh):
# """ Buck, A. L. (1981), "New equations for computing vapor pressure and enhancement factor"
#                          J. Appl. Meteorol. 20: 1527-1532 """
    import math

    if tc > 0.0:
        a, b, c = (6.1121, 17.368, 238.88)
    else:
        a, b, c = (6.1121, 17.966, 247.15)
    psat = a * math.exp(b * tc / (c+ tc))
    pair = rh * psat * 0.01
    v = math.log(pair/a)
    td = c * v / (b - v)
    return td

def wrf_wvapor(sfcT, sfcQ, sfcP):

# following calculation on water vapor related variables are based on
# formulas in http://www.geog.ucsb.edu/~joel/g266_s10/lecture_notes/chapt03/oh10_3_01/oh10_3_01.html
#
# sfcQ: g/Kg
# sfcP: mb
# sfcT: C
    import numpy as np

    from atmoscons import atmconstants
    from cales import svptk_ukmet as svptk

    atmcon = atmconstants()

# calculate specific humidity:

    shum = np.divide(sfcQ, np.add(sfcQ, 1.0))

    # calculate vapor pressure:
    bterm = np.add(sfcQ, atmcon['epsilon'])
    uterm = np.multiply(sfcQ,sfcP)
    ep = np.divide(uterm,bterm)

    #calculate saturation vapor prssure:
    es = []
    for tk in sfcT:
       es.append(svptk(np.add(tk,atmcon['T0'])))

    # calculate relative humidity:
    qs = np.multiply(np.divide(es,np.subtract(sfcP,es)),atmcon['epsilon'])
    rh = np.multiply(np.divide(ep,es),100)

    tc = np.subtract(sfcT, atmcon['T0'])
    td = []
    for t, r in zip(tc, rh):
        td.append(dewpointT(t,r))
    #for t, d, r in zip(tc, td, rh):
#        print t, d, r 
    return rh, td, tc 

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

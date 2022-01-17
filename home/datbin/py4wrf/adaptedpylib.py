def draw_meteo(cycle, siteid, temp,td,wind_v,dirs,psfc,rain,swdown,times):

# this python function was adapted from Dr. Luke Madaus' timeseries plot:
#   http://www.atmos.washington.edu/~lmadaus/pyscript/plot_wrf_meteo.txt
# with modifications by Wanli Wu (wu80012@gmail.com) 09-2012

    import sys
#   import matplotlib
    import matplotlib as mpl; mpl.use('Agg')
    import pylab
    import os
    import numpy as np

    from wrfpylib import *

    hourticks = range(0,len(times))

    (hours, dates, dateticks) = wrf_timelable(times)

# Get rid of extremely low precip values
    
    for r in range(len(rain)):
        if rain[r] < 0.01:
            rain[r] = 0.0
    if np.ma.min(rain) <= 0.0:
       pltrain = False
    else:
       pltrain = True 

# Begin plotting
    pylab.figure(figsize=(10,8), frameon = False)

# Now make temperature plot
    pylab.subplot(511)
    tempplot = pylab.plot(hourticks,temp,'r-')
    dewpplot = pylab.plot(hourticks,td  ,'g-')
#    mesodewpplot1 = pylab.plot(obtime,meso_dewp_F,'go-')
    ymin = pylab.axis()[2]
    tempfplot = pylab.fill_between(hourticks,ymin,temp, facecolor = 'pink')
    dewpfplot = pylab.fill_between(hourticks,ymin,td,   facecolor = 'palegreen')
#    mesotempplot = pylab.plot(obtime,meso_temp,'ro-')
#    mesodewpplot = pylab.plot(obtime,meso_dewp_F,'go-')
    pylab.grid(True)
    cleardate = cycle[0:4] + '-' + cycle[4:6] + '-' + cycle[6:8] + ' ' + cycle[8:10] + 'UTC'
    pylab.title('WRF Forecast Meteogram for %s  from %s' % (siteid, cleardate))
    ax = pylab.gca()
    ax.set_xlim([0,hourticks[-1]])
    pylab.xticks(hourticks, hours)
    for label in ax.get_yticklabels():
        label.set_fontsize(8)
    for label in ax.get_xticklabels():
        label.set_fontsize(8)
#    pylab.text(-0.07,0.5,'Air Temperature [C]', rotation='vertical',fontsize = 8, color = 'r', verticalalignment='center', transform = ax.transAxes)
    pylab.text( 1.02,0.5,'Dewpoint Temp [C]', rotation='vertical',fontsize = 8, color = 'g', verticalalignment='center', transform = ax.transAxes)
    pylab.ylabel('Air Temp [C]', fontsize = 8, color = 'r')

    # Now for the wind speed / direction plot
    ax1 = pylab.subplot(512)
    windvplot = ax1.plot(hourticks,wind_v, 'b-')
#    mesowindvplot1 = ax1.plot(obtime,meso_wspd, 'bo-')
    ymin = ax1.axis()[2]
    windvfplot = ax1.fill_between(hourticks,ymin,wind_v, facecolor = 'lightskyblue')
#    mesowindvplot = ax1.plot(obtime,meso_wspd, 'bo-')
    ax1.grid(True)
    for label in ax1.get_yticklabels():
        label.set_fontsize(8)
    ax1.set_ylabel('Wind Speed [mph]', fontsize = 8, color = 'b')

    ax2 = ax1.twinx()
    winddplot = ax2.plot(hourticks, dirs, marker = '^', mec = 'darkslateblue', mfc = 'darkseagreen', linestyle = 'none')
#    mesowinddplot = ax2.plot(obtime, meso_wdir, marker = 'o', mec='darkslategray', mfc = 'darkgray', linestyle = 'none')
    ax2.set_ylim([0,360])
    ax2.set_xlim([0,hourticks[-1]])
    pylab.xticks(hourticks, hours)
    for label in ax2.get_yticklabels():
        label.set_fontsize(8)
    for label in ax2.get_xticklabels():
        label.set_fontsize(8)
    for label in ax1.get_xticklabels():
        label.set_fontsize(8)
    pylab.yticks([0.,45.,90.,135.,180.,225.,270.,315.,360], ['N','NE','E','SE','S','SW','W','NW','N'])
    ax2.set_ylabel('Wind Direct. [deg]', fontsize = 8, color = 'brown')

    # Pressure Plot
    pylab.subplot(513)
#    print meso_press
    press_plot = pylab.plot(hourticks, psfc, color='brown', linestyle='solid')
#    mesopressplot1 = pylab.plot(obtime, meso_press, color = 'brown', linestyle = '-', marker = 'o')
#   pmin = 940
    ax = pylab.gca()
    ax.set_ylim([940,1020])
    ymin = pylab.axis()[2]
    pressfplot = pylab.fill_between(hourticks, ymin, psfc, facecolor = 'rosybrown')
#    mesopressplot = pylab.plot(obtime, meso_press, color = 'brown', linestyle = '-', marker = 'o')
    pylab.grid(True)
    ax = pylab.gca()
    ax.set_xlim([0,hourticks[-1]])
    pylab.xticks(hourticks, hours)
    for label in ax.get_yticklabels():
        label.set_fontsize(8)
    for label in ax.get_xticklabels():
        label.set_fontsize(8)
    pylab.ylabel('Pressure [mb]', fontsize = 8, color = 'brown')

    # SRad Plot
    pylab.subplot(514)
    srad_plot = pylab.plot(hourticks, swdown, 'y-')
    pylab.grid(True)
    ax = pylab.gca()
    ax.set_ylim([0,1000])
    ymin = pylab.axis()[2]
    sradfplot = pylab.fill_between(hourticks,ymin,swdown, facecolor = 'khaki')
#    mesorainplot = pylab.plot(obtime, meso_srad,color='goldenrod', marker = 'o', linestyle='solid')
    ax.set_xlim([0,hourticks[-1]])
    pylab.xticks(hourticks, hours)
    for label in ax.get_yticklabels():
        label.set_fontsize(8)
    for label in ax.get_xticklabels():
        label.set_fontsize(8)
    for d in range(len(dates)):
        pylab.text(dateticks[d] , -325., dates[d],rotation='horizontal',fontsize = 10, color = 'k', horizontalalignment = 'center')
    pylab.ylabel('Solar Radiation [W m-2]', fontsize = 8, color = 'y')

    if pltrain:
    # Rainfall Plot
       pylab.subplot(515)
       rain_plot = pylab.plot(hourticks, rain, 'g-')
       pylab.grid(True)
       ax = pylab.gca()
#       ax.set_ylim([0,1.])
       ymin = pylab.axis()[2]
       rainfplot = pylab.fill_between(hourticks,ymin,rain, facecolor = 'mediumaquamarine')
#       mesorainplot = pylab.plot(obtime, meso_rain, 'go-')
       ax.set_xlim([0,hourticks[-1]])
       pylab.xticks(hourticks, hours)
       for label in ax.get_yticklabels():
           label.set_fontsize(8)
       for label in ax.get_xticklabels():
           label.set_fontsize(8)
       pylab.ylabel('Hourly Rainfal [mm]', fontsize = 8, color = 'g')
    
    fpx = 'wrf_timeseries_'
    filename = fpx + '%s_%s' % (cycle, siteid)
    pylab.savefig(filename)
    pylab.show()
    pylab.close()
    wrfpltdir = '/d1/wanliwu/WRFPLT/'
    giffile = wrfpltdir + filename
    os.system('convert -render -flatten %s.png %s.gif' % (filename,giffile))
    os.system('rm %s.png' % filename)

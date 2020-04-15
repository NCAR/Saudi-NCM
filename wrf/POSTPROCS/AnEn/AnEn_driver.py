#!/usr/bin/python

import sys, stat, os
import grp, pwd
import locale
import time
import datetime
import subprocess
import operator
from optparse import OptionParser


def package_images(root_dir,cycle_date,dest):
  tar_file = "%s.tgz" % (cycle_date)
  tar_command = "cd %s; tar -czvf %s %s " % (root_dir, tar_file, cycle_date)
  print "tar command %s" % (tar_command)
  proc = subprocess.Popen([tar_command],shell=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  output = proc.communicate()[0]

  ### Create sha sums on newly created file
  sum_file = tar_file+".sum"
  sha_command = "cd %s; sha256sum %s > %s" % (root_dir, tar_file, sum_file)  
  proc = subprocess.Popen([sha_command],shell=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  sha_output = proc.communicate()[0]

  ### Move the files to the destination/stage directory 
  move_command = "mv %s/%s*  %s" % (root_dir, tar_file, dest)  
  proc = subprocess.Popen([move_command],shell=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
  move_output = proc.communicate()[0]
  print move_command

def main():

  usage_str = "%prog [options]"
  parser = OptionParser(usage = usage_str)
  parser.add_option("-c", dest="cycle", help=" Cycle YYYYMMDDHH - required")
  parser.add_option("-r", dest="range", help=" GSJOBID (GWATC,GWCRTC,GEDPG, GWEPG, GWNVL,GWRTC,GWWSMR,GWYPG) - required")
  parser.add_option("-e", dest="mm5home", help=" MM5HOME not required")
  parser.add_option("-d", dest="plotdir", help=" plot_dir not required, only if $DO_DISTRIB is turned on")

  (options, args) = parser.parse_args()

  # make sure we have been given a valid cycle
  #   # 
  if options.cycle == None:
    parser.print_help()
    sys.exit(2)
  
  if options.mm5home == None:
    mm5home = "/home/atecuser/"
  else:
    mm5home = options.mm5home+"/cycle_code/POSTPROCS/"


  cycle = options.cycle
  gsjobid = options.range
  range = gsjobid[2:].lower()
  print range

  #range_caps = os.environ.get('RANGE')
  GSJOBID = os.environ.get('GSJOBID')
  MM5HOME = os.environ.get('MM5HOME')
  user = os.environ.get('USER')
  if user == 'None':
    user = 'atecuser'
  print GSJOBID,MM5HOME

  #Change to upper case for directory naming ATC2 causes issues
  range1_caps = range.upper()
  #if range_caps == "None":
  range_caps = range.upper()
  if range_caps == "ATC2":
    range_caps = "ATC"

  if len(cycle) > 2:
    year = int(cycle[0:4])
    month = int(cycle[4:6])
    day = int(cycle[6:8])
    hour = int(cycle[8:10])
  else:
    d=datetime.date.today()
    year = int(d.strftime("%Y"))
    month = int(d.strftime("%m"))
    day = int(d.strftime("%d"))
    hour = int(cycle)
 
 
  cycle_date  = datetime.datetime(year,month,day,hour)
  if range == "crtc" or range == "wsmr":
    cycle_interval = 6
    total_hours = 24 
  else:
    cycle_interval = 3
    total_hours = 24

  #Parameters for submitCycleMM require giving the current cycle
  #AnEn plots are based on previous cycle because of data calculations
  cycle_date = cycle_date - datetime.timedelta(hours=cycle_interval)

  #Start plots at ~1 day ago with most recent cycle being them most recent plot
  plot_date = cycle_date - datetime.timedelta(hours=total_hours)
  image_index = 1

  while plot_date <= cycle_date: 
   
    plot_year  = plot_date.strftime("%Y")
    plot_month = plot_date.strftime("%m")
    plot_day   = plot_date.strftime("%d")
    plot_hour  = plot_date.strftime("%H")
    plot_cycle = plot_year+plot_month+plot_day+plot_hour
    plot_day = plot_year+plot_month+plot_day

    #plot_date_compact = plot_year+plot_month+plot_day+plot_hour+plot_cycle

    plot_dir = "/model/%s/cycles/GW%s/%s/web/AnEn/%s" % ( user, range1_caps, range_caps, plot_cycle)
    if os.path.exists("/p/work1"):
      plot_dir = "/p/work1/%s/cycles/GW%s/%s/web/AnEn/%s" % ( user, range1_caps, range_caps, plot_cycle)
    elif os.path.exists("/p/work2"):
      plot_dir = "/p/work2/%s/cycles/GW%s/%s/web/AnEn/%s" % ( user, range1_caps, range_caps, plot_cycle)

    print "Output directory ",plot_dir

    # Make sure plot_dir exists; if not, create it!
    if not os.path.exists(plot_dir):
      os.makedirs(plot_dir)

    #change directory to the fddasites dir of range
    os.chdir(plot_dir)

    #### there is no /datainput/atc2 for the netcdf files so we need to look in atc
    if range == "atc2":
      range = "atc"
    ncl_command = "eval /opt/ncl/bin/ncl 'range=\\\"%s\\\"' cycle=%s day_to_plot=%s 'meas=\\\"\\\"' %sAnEn/plot_analog_ensemble_time_series.ncl" % (range, plot_hour, plot_day, mm5home)
    print ncl_command
    proc = subprocess.Popen([ncl_command],shell=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    output = proc.communicate()[0]
    print output
    ncl_command = "eval /opt/ncl/bin/ncl 'range=\\\"%s\\\"' cycle=%s day_to_plot=%s 'meas=\\\"Met\\\"' %sAnEn/plot_analog_ensemble_time_series.ncl" % (range, plot_hour, plot_day, mm5home)
    print ncl_command
    proc = subprocess.Popen([ncl_command],shell=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    output = proc.communicate()[0]
    print output
    ncl_command = "eval /opt/ncl/bin/ncl 'range=\\\"%s\\\"' cycle=%s day_to_plot=%s 'meas=\\\"Eng\\\"' %sAnEn/plot_analog_ensemble_time_series.ncl" % (range, plot_hour, plot_day, mm5home)
    print ncl_command
    proc = subprocess.Popen([ncl_command],shell=True,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    output = proc.communicate()[0]
    print output

    if options.plotdir != None:
      distrib_dir = "%s/GW%s/anen/" % ( options.plotdir, range1_caps)
      if not os.path.exists(distrib_dir):
        os.makedirs(distrib_dir)
      root_dir = "/model/%s/cycles/GW%s/%s/web/AnEn/" % (user, range1_caps, range_caps)
      if os.path.exists("/p/work1"):
        root_dir = "/p/work2/%s/cycles/GW%s/%s/web/AnEn/" % (user, range1_caps, range_caps)
      elif os.path.exists("/p/work2"):
        root_dir = "/p/work2/%s/cycles/GW%s/%s/web/AnEn/" % (user, range1_caps, range_caps)
      package_images(root_dir, plot_cycle, distrib_dir )

    plot_date = plot_date + datetime.timedelta(hours=cycle_interval)
    image_index = image_index + 1
    #print cycle_date,plot_date


if __name__ == '__main__': main()


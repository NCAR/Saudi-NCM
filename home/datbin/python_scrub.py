import os, time, sys
import datetime
from datetime import date, timedelta
import operator
import subprocess
import socket
import string
import glob
import sys
from subprocess import *
from optparse import OptionParser

def remove_files (path) :
  print "Removing files in %s" % (path)
  now = time.time()
  for f in os.listdir(path):
      f = os.path.join(path,f)
      print "File %s" % (f)
      if os.stat(f).st_mtime < time.time() - (scrub_days * 24 * 60 * 60):
          if os.path.isfile(f):
              if verbose:
                  print "Removing %s" % (f)
              if no_action:
                  print "rm %s" % (f)
              else:
                  print "trying to remove %s" % (f)
                  os.remove(f)
          elif os.path.isdir(f) and recursive:
              print "recursive %s" % (f)
              remove_files(f)
          elif os.path.islink(f):
              print "Symlink %s" % (f)
              os.unlink(f)
          else:
              print "Not removing contents of directory %s " % (f)
              

def main():

  usage_str = "%prog [options]"
  parser = OptionParser(usage = usage_str)
  parser.add_option("-v", dest="verbose", action="store_true", default=False, help="Verbose = prints the files it is deleting")
  parser.add_option("-r", dest="recursive", action="store_true", default=False, help="Recursive")
  parser.add_option("-n", dest="no_action", action="store_true", default=False, help="Lists what will be done but does not perform the action ")
  parser.add_option("-d", dest="days", help="Number of days to keep - required ")
  parser.add_option("-p", dest="path", help="Path to scrub - required ")

  (options, args) = parser.parse_args()
  global verbose
  verbose = options.verbose
  global no_action 
  no_action = options.no_action
  global recursive 
  recursive = options.recursive
  #recursive = False
  

  # make sure we have been given a valid cycle
  #   #
  if options.days == None:
    parser.print_help()
    sys.exit(2)
  if options.path == None:
    parser.print_help()
    sys.exit(2)

  global scrub_days
  scrub_days = int(options.days)
  path = options.path

  remove_files(path)


if __name__ == '__main__': main()


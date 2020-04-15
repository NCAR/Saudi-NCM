#!/bin/csh -f

#---------------------------------------------------------
# 
# check if directory exists.
# Tries to make if not.
#
#---------------------------------------------------------

if ( $#argv != 1 ) then
   echo "Usage:" $0 DirectoryName
   exit 2
endif
#setenv SUBSYSTEM MM5_UTILITIES
if ( ! -d $1 ) then
#  echo "${SUBSYSTEM} -- Making directory $1"
  mkdir -p $1
endif

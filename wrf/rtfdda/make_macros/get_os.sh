#!/bin/sh
#
# Function: 
#
#  This Bourne Shell script attempts to determine what operating system
#  you are currently running on by comparing the output of uname to a
#  known list of operating system search strings.
#
# Access:
#
#  This file and the other files in this directory are accessed via an
#  environment variable which must be set correctly to this
#  directory: MS_MAKE_MACROS_DIR (Modeling System Make Definitions).
#
#  This script writes an output .ostype file in the current directory. 
#  This .ostype can be used in one of 2 ways:
#      (1) set your MS_OS_TYPE environment variable to the contents
#          of .ostype, or 
#      (2) on your command line use the command
#            % make MS_OS_TYPE=`$MS_MAKE_MACROS_DIR/get_os.sh; cat .ostype` 
#
# Future:
#
#  Note from Tres: Could look into uname options so make parsing easier.
#  e.g., uname with no options returns Linux (may not be enough for
#        distinguishing between Debian and RedHat)
#
# Mar 2007
#
#========================================================================

# Define the list of supported OS's

OS_LIST="CRAY HP Linux IBM"

# Define the temporary files created by this script

TMPFILE=.tmpfile
OSFILE=.ostype

# Generate the uname output and parse it to set the current OS

uname -a > ${TMPFILE}
found=FALSE
for find_os in ${OS_LIST}; do
    grep -q ${find_os} ${TMPFILE}
    if [ $? = 0 ] ; then
	echo "Compiling for ${find_os}" >&2
	found=TRUE
	break
    fi
done

if [ ${found} = FALSE ] ; then
	echo "Do not know how to compile for this machine" >&2
	find_os=UNKNOWN
fi

# Force output to be in lower case so that the compiler_macros.<os_type>
# files all have lower case <os_type> filename extensions and do not have
# to worry about upper/mixed case

echo ${find_os} | tr [A-Z] [a-z] > ${OSFILE}

rm -f ${TMPFILE}

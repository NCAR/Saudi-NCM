#!/bin/sh
# This script rewrites _Makefile.intel, if $DO_INTEL env var is set, to expand
# $(NETCDF) and $(LD_LIBRARY_PATH) for use with Intel compiler.
#===============================================================================

if [ ${DO_INTEL} ] ; then
   if [ -e ${INTEL_SOURCE_FILE} ] ; then
      source ${INTEL_SOURCE_FILE}
   else
      echo "Warning: Intel source file: ${INTEL_SOURCE_FILE} does not exist!"
   fi
else
   echo "Not configuring Intel Make file!"
   exit
fi

if [ -e _Makefile.intel ] ; then
   sed -e "s|\$(NETCDF)|${NETCDF}|g" \
       -e "s|\$(LD_LIBRARY_PATH)|${LD_LIBRARY_PATH}|g" \
       _Makefile.intel > _Makefile.intel.tmp
   mv _Makefile.intel _Makefile.intel.orig
   mv _Makefile.intel.tmp _Makefile.intel
else
   echo "_Makefile.intel file missing!"
fi

exit

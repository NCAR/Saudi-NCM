#!/bin/csh

set CODE = /model/chengw/dpg/analog/analog_code_rong.20150722_new_flags_add_pgi/anen

setenv NETCDF_ROOT /opt/netcdf

# ==============
setenv NETCDFLIB ${NETCDF_ROOT}/lib
setenv NETCDFINC ${NETCDF_ROOT}/include

#export WRF_OS=ARCH      # for wrfqc only
#export WRF_MACH=Intel   # for wrfqc only

# ======= this works for sequential ========
configure FC=pgf95 NETCDF_ROOT=$NETCDF_ROOT --prefix=$CODE
#
make

#make install

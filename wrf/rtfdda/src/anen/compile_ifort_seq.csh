#!/bin/csh

set INTEL_PATH = /opt/intel/composer_xe_2013_sp1.3.174

set CODE = /model/chengw/dpg/analog/analog_code_rong.20150722_new_flags_add_gnu_no_pgi_code/anen

setenv NETCDF_ROOT /opt/netcdf-20130827-intel14.0.3

setenv HDF /opt/hdf5-1.8.9-intel14.0.3

setenv NCARG_ROOT_intel /opt/ncl_ncarg-6.2.0-intel14.0.3

setenv MPI /opt/openmpi-1.8.1-intel14.0.3

# ==============
source ${INTEL_PATH}/bin/compilervars.csh intel64

setenv NETCDFLIB ${NETCDF_ROOT}/lib
setenv NETCDFINC ${NETCDF_ROOT}/include

setenv MPI_TOP $MPI
setenv OMPI_CC icc
setenv OMPI_FC ifort
#export WRF_OS=ARCH      # for wrfqc only
#export WRF_MACH=Intel   # for wrfqc only
set PATH = ( ${INTEL_PATH}/bin/intel64:$MPI/bin:${PATH} )
setenv LD_LIBRARY_PATH  ${NETCDFLIB}:${HDF}/lib:${MPI_TOP}/lib:${NCARG_ROOT_intel}/lib:${LD_LIBRARY_PATH}

which ifort

# ======= this works for sequential ========
configure FC=ifort NETCDF_ROOT=$NETCDF_ROOT --prefix=$CODE
#
make

#make install

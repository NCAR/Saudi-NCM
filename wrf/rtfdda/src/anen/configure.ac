#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([ANLOG-ENSEMBLE], [1.0], [lucadm@ucar.edu])

AM_INIT_AUTOMAKE([foreign dist-bzip2])

# Required influential environment variables
AC_ARG_VAR([NETCDF_ROOT], [The root directory where NetCDF libraries (lib/) and include (include/) files exist.])
 
# Optional influential environment variables
AC_ARG_VAR([MPI], [The root directory where MPI libraries (lib/) and include (include/) files exist.])

if test "x${NETCDF_ROOT}" = "x" ; then
      AC_MSG_ERROR([NETCDF_ROOT environment varialbe is not set. Please set it as an environment variable or run configure with "NETCDF_ROOT=<path>" as a command-line option.])
fi

# Optional influential environment variable

# Do we want fast mode or not?
AC_ARG_ENABLE([fast],[AS_HELP_STRING([--enable-fast],[Enable fast mode])],
              [:],[enable_fast=check])

# Do we want mpi mode or not?
AC_ARG_ENABLE([mpi],[AS_HELP_STRING([--enable-mpi],[Enable mpi mode])],
              [:],[enable_mpi=check])

# Do we want openmp mode or not?
AC_ARG_ENABLE([openmp],[AS_HELP_STRING([--enable-openmp],[Enable openmp mode])],
              [:],[enable_openmp=check])

if test "$enable_mpi" = "yes" -o "$enable_openmp" = "yes"; then
   if test "xMPI" = "x"; then
      AC_MSG_ERROR([MPI environment varialbe is not set. Please set it as an environment variable or run configure with "MPI=<path>" as a command-line option.])
   fi
fi

# Where do we want to install lib and include files?
if test "x$prefix" = "xNONE"; then
   prefix='$(abs_top_srcdir)'
fi

# Keep libtool macros in an m4 directory.
AC_CONFIG_MACRO_DIR([m4])

# Find out about the host we're building on.
AC_CANONICAL_HOST

# Checks for programs.
AC_PROG_FC(ifort pgf95 gfortran)
AC_PROG_MAKE_SET

# If F90 is set, and FC isn't, then set FC to whatever F90 was set to.
if test "${F90+set}" = set -a "${FC+set}" != set; then
   FC=$F90
fi

if test "x$FC" = "x"; then
   AC_MSG_ERROR([Cannot find Fortran compiler])
fi

if test "x$FC" = "xgfortran"; then
   version=`$FC --version | head -1 | egrep -o '[[0-9]]+\.[[0-9]]+' | head -1`
   major=`echo $version | cut -d '.' -f1`
   minor=`echo $version | cut -d '.' -f2`
   iversion=$((100*$major+$minor))
   if test $iversion -lt 501; then
      AC_MSG_ERROR([gfortran version too old. requires version 5.1 or newer.])
   fi
fi

# Use libtool
LT_INIT([disable-shared])
AC_SUBST([LIBTOOL_DEPS])

# Checks for libraries.

# Checks for header files.
#AC_CONFIG_HEADER(config.h)

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile
                 lib/Makefile
                 code/Makefile])

NETCDFINC_FLAGS="-I $NETCDF_ROOT/include"
ANENINC_FLAGS='-I $(abs_top_srcdir)/lib'
LIBnetcdf="-L$NETCDF_ROOT/lib -lnetcdff -lnetcdf"
LIBanen='$(abs_top_srcdir)/lib/libanen.a'
LIBmpi=""

case $FC in

  ifort)
    ff1="-g -heap-arrays -assume byterecl -assume noold_maxminloc"
    ff2="-traceback -check all -check noarg_temp_created"
    ff3="-warn all -warn errors"
    ff4="-fstack-security-check"

    ffnormal="-O2 -fpe-all=0"
    
    F03_ALLOCATE="-assume realloc_lhs"

    if test "$enable_fast" = "yes"; then
       FCFLAGS="$ff1 $ff3 $ff4 $ffnormal"
    else
       FCFLAGS="$ff1 $ff2 $ff3 $ff4 $ffnormal"
    fi

    if test "$enable_mpi" = "yes" -o "$enable_openmp" = "yes"; then
       FC=mpif90
       FCFLAGS+=" -I $MPI/include"
       AM_CPPFLAGS="-fpp -DMPI -save-temps"
      #RPATH_FLAGS="-Wl,-rpath,$NETCDF_ROOT/lib:$MPI/lib"
       if test "$enable_openmp" = "yes"; then
          FCFLAGS+=" -openmp"
       fi
    fi

    FCFLAGS+=" $F03_ALLOCATE"
    ;;
  pgf95)
    ff1="-g"
    ff2="-Mbounds -Minfo=all -traceback -Mchkfpstk -Mchkstk -Mdalign 
-Mdclchk -Mdepchk -Miomutex -Mrecursive -Msave -Ktrap=fp -O0"
    ff3="-Minform,inform"
    ff4=""

    ffnormal="-O2 -Knoieee"
    
    F03_ALLOCATE=""

    if test "$enable_fast" = "yes"; then
       FCFLAGS="$ff1 $ff3 $ff4 $ffnormal"
    else
       FCFLAGS="$ff1 $ff2 $ff3 $ff4 $ffnormal"
    fi

    if test "$enable_mpi" = "yes" -o "$enable_openmp" = "yes"; then
       FC=mpif90
       FCFLAGS+=" -I $MPI/include"
       AM_CPPFLAGS="-fpp -DMPI -save-temps"
      #RPATH_FLAGS="-Wl,-rpath,$NETCDF_ROOT/lib:$MPI/lib"
       if test "$enable_openmp" = "yes"; then
          FCFLAGS+=" -openmp"
       fi
    fi

    FCFLAGS+=" $F03_ALLOCATE"

    ;;
  gfortran)
    ff1="-g -ffree-form -ffree-line-length-none 
-fno-unsafe-math-optimizations -frounding-math -fsignaling-nans"
    ff2="-fno-backtrace -frecursive ffpe-trap=overflow -O0"
    ff3="-Waliasing"
    ff4=""

    ffnormal="-O2"
    
    F03_ALLOCATE=""

    if test "$enable_fast" = "yes"; then
       FCFLAGS="$ff1 $ff3 $ff4 $ffnormal"
    else
       FCFLAGS="$ff1 $ff2 $ff3 $ff4 $ffnormal"
    fi

    if test "$enable_mpi" = "yes" -o "$enable_openmp" = "yes"; then
       FC=mpif90
       FCFLAGS+=" -I $MPI/include"
       AM_CPPFLAGS="-cpp -DMPI -save-temps"
      #RPATH_FLAGS="-Wl,-rpath,$NETCDF_ROOT/lib:$MPI/lib"
       if test "$enable_openmp" = "yes"; then
          FCFLAGS+=" -openmp"
       fi
    fi

    FCFLAGS+=" $F03_ALLOCATE"
    ;;
  *)
    AC_MSG_ERROR([Fortran compiler $FC is not supported. Please use Intel (ifort) compilers])
    ;;

esac

#AC_SUBST(RPATH_FLAGS)
AC_SUBST(NETCDFINC_FLAGS)
AC_SUBST(ANENINC_FLAGS)
AC_SUBST(LIBnetcdf)
AC_SUBST(LIBanen)
AC_SUBST(AM_CPPFLAGS)
AC_OUTPUT

MAINTAINERCLEANFILES    = Makefile.in
bin_PROGRAMS = main_analog

main_analog_SOURCES = \
	main_analog.F90 \
	shell-sort.bc.f90 \
	std_dev.f90 \
	stdevTNcirc.f90 \
	wind_dir_error.f90 \
	read_point_data.f90 \
	find_analog_v1.f90 \
	kf_luca.f90 \
	only_analog.f90

AM_FCFLAGS = $(NETCDFINC_FLAGS) $(ANENINC_FLAGS)
AM_LDFLAGS = $(LIBanen) $(LIBnetcdf)

CLEANFILES = *.o *.mod $(bindir)/$(bin_PROGRAMS) *__*.f90

# DEPENDENCIES
include Makefile.dep

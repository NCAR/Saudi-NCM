#!/bin/tcsh
#
# This script builds the required executables for POSTPROCS,
# copies them to $EXECUTABLE_ARCHIVE, and copies the range-specific 
# POSTPROCS files to a working/deployment directoy
#
# Input is <RANGE_NAME>

# Get arguments from command line
# -------------------------------

if ($#argv < 1) then
 # Error in command line arguments
      echo "Missing arguments: build_postprocs.csh <RANGE>"
      echo ""
      exit(1)
else if ($#argv = 1) then
      set RANGE = $1
endif

# Executables...

set build_dir = "/data/fddahome/cycle_code/EXECUTABLE_ARCHIVE"
set dest_dir = "/data/fddahome_port/cycle_code/EXECUTABLE_ARCHIVE"


# Utilities
cp ${build_dir}/splitv3.exe ${dest_dir}
cp ${build_dir}/readv3 ${dest_dir}
# RIP/plotting
cp ${build_dir}/rip_new.exe ${dest_dir}
cp ${build_dir}/rip_obs.exe ${dest_dir}
cp ${build_dir}/ripdp_new.exe ${dest_dir}
cp ${build_dir}/ripdp_obs.exe ${dest_dir}
# Tabular sites
cp ${build_dir}/sites_new.exe ${dest_dir}
# Scipuff/medoc
cp ${build_dir}/medoc_driver.exe ${dest_dir}
cp ${build_dir}/mm52scipuf ${dest_dir}

# Scripts & params

set build_dir = "/data/fddahome/cycle_code/POSTPROCS"
set range_dir = "/data/fddahome/cycle_code/${RANGE}/POSTPROCS"
set dest_dir = "/data/fddahome_port/cycle_code/POSTPROCS"

cp ${range_dir}/* $dest_dir

# Overwrite the standard scripts/controls with range-specific if needed.
if ( -e $range_dir ) then
cp ${range_dir}/* $dest_dir
endif

exit


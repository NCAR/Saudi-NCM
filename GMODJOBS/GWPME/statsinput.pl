#!/usr/bin/perl
################################################################################
# Required variables for monthly (or any long-term) verification stats         #
# calculation.                                                                 #
################################################################################
#
# Where to find WRF pairs data?
#
$WRF_PAIRS_DIR = "/home/$ENV{LOGNAME}/data/cycles/GWMAGEN_01/GRM/veri_dat";
#
# Where to find reference model pairs data?
#
$REF_PAIRS_DIR = "/home/$ENV{LOGNAME}/data/cycles/GWMAGEN_01/GRM/veri_dat_interm";
#
# What is the working directory for monthly stats calculation?
#
$MONTHLY_DIR = "/home/$ENV{LOGNAME}/data/cycles/GWMAGEN_01/GRM/monthly_stats";
#
# Where are the executables?
#
$EXECUTABLE_ARCHIVE = "/home/$ENV{LOGNAME}/data/cycle_code/EXECUTABLE_ARCHIVE";
#
# Plot ranges for different varibles.
# Example: $b_RANGE_t is for bias range for variable t.
# Example: $rm_RANGE_rh is for rmse (amd mae) range for variable rh.
# The values separated by '/' are lower bound and upper bound.
#
%plot_range = (
  t    => { bias => '-5/5', rmse => '0/5' },
  q    => { bias => '-2/2', rmse => '0/4' },
  rh   => { bias => '-10/10', rmse => '0/25' },
  ws   => { bias => '-5/5', rmse => '0/5' },
  wd   => { bias => '-20/20', rmse => '0/100' },
  psfc => { bias => '-5/5', rmse => '0/10' },
  slp  => { bias => '-5/5', rmse => '0/10' },
  gh  => { bias => '-20/20', rmse => '0/30' },
);

1;

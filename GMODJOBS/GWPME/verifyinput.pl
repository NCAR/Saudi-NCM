#!/usr/bin/perl
#
# Specify the working directory for verification
#
$VERI_WORK_DIR = "$RUNDIR/verify";
#
# Where should verification pairs be saved to?
$VERI_PAIRS_DIR = "$RUNDIR/veri_dat";
#
# Should we verify against surface obs? (most likely yes)
#
$VERI_SFC = 1;
#
# Should we verify against upper-air obs? (most likely yes)
#
$VERI_UPR = 1;
#
# Should we plot the verification results? (most like yes)
#
$VERI_PLT = 1;
#
# Should upper-air verification contain height pairs? Use 1 for
# height pairs addition. Use 0 for no.
#
$VERI_HGT        = 1;
#
# Forecast verification length in hours
$VERI_LENGTH     = 24;
#
# Are we going to verify other larger-scale model? 1 = yes; 0 or bland = no.
#
$VERI_INTERM = 0;
#
# Is there a predefined list of stations for verification, instead of
# verifying against all stations? If yes, list the full path of the
# configuration file (same format as GO-Index config file). If not, just leave
# it blank.
$VERI_LIST = "";
#
# For verification caculation, QC flag lower than this value will not be
# considered.
#
$QC_CUT = 3;
#
# For upper-air verification, whether we want to verify certain hours only.
# If all hours are considered, leave blank.
#
@UPR_HOURS =
#
# Color specification /R/G/B for curves repesenting different cycles.
# The colors are green, blue4, blue1, MediumPurple, purple, orchid, magenta,
# HotPink, red, tomato, orange, firebrick, SandyBrown, DarkGoldenrod.
# Run 'xcolors' and see /etc/X11/rgb.txt.
#
@colors = ('/0/255/0','/0/0/139','/0/0/255','/147/112/219','/160/32/240',
           '/218/112/214','/255/0/255','/255/105/180','/255/0/0','/255/99/71',
           '/255/165/0','/178/34/34','/244/164/96','/184/134/11');

#
# Plot ranges for different varibles.
# Example: $b_RANGE_t is for bias range for variable t.
# Example: $rm_RANGE_rh is for rmse (amd mae) range for variable rh.
# The values separated by '/' are lower bound and upper bound.
#
%plot_range = (
  t    => { bias => '-5/5', rmse => '0/5' },
  q    => { bias => '-2/2', rmse => '0/4' },
  rh   => { bias => '-20/20', rmse => '0/40' },
  ws   => { bias => '-5/5', rmse => '0/5' },
  wd   => { bias => '-50/50', rmse => '0/100' },
  psfc => { bias => '-5/5', rmse => '0/10' },
  slp  => { bias => '-5/5', rmse => '0/10' },
);

1;

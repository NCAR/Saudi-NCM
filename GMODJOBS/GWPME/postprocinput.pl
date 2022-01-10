#!/usr/bin/perl

#
# This is not used in the modular/moab/torque version of RTFDDA, but is still used
# in the rtfddaflex.pl version - to create the destination directory on the web-server.
#
$USER = "ncaruser";

#
# Is this a WRF run? (0=MM5, 1=WRF)
#
$IS_WRF = 1;

#
# Will MM5 generate a new file for each output time step (i.e. each hour)?
#
$HOURLY_OUTPUT_FLAG = 1;

#
# Run the plotting post-processor (RIP) and send gifs to the web-dest-host
#
$DO_RIP_1 = 1;
$DO_RIP_2 = 1;
$DO_RIP_3 = 1;
$DO_RIP_4 = 0;

#
# Flag to select RIP4 (vs RIP3 - RIP3 is no longer supported)
#
$IS_RIP4 = 1;

#
# The time interval, in minutes. After every such interval model and obs plots
# are generated. For example, a value of 60 means model and obs plots are
# generated every 60 minutes. If not set, defaults to (60,60,60,60).
#
@RIP_INTRVL = (60,60,60,60);

#
# The maximum model/obs plotting length in hours, beyond which model/obs
# plotting is not executed.
#
@RIP_LENGTH = (60,60,60,60);

#
# Flag to set intermediate output type (ps or cgm - default is cgm, select
# ps to avoid ctrans segfaults)
#
$RIP_IMG = "ps";

#
# Flag to select image type (gif or png - default is gif)
$WEB_IMG = "gif";

# Flag to set when to use winter/summer namelist:  starting with Jan
@SEASONS=( 'winter', 'winter', 'winter', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'winter', 'winter');

#
# Save all grib and raw netcdef files in cycles directories
# Deprecated by $DO_DISTRIB
#$DO_CYCLES = 1;

#
#  flags to copy files to area outside of cycles for streamlined distribs
# Copy files to distrib area
$DO_DISTRIB = 1;
# location of distrib area (on bigmacb, is cross-mounted with atec-exposed)
$DISTRIB_ROOT = "/lustre/project/k1206/$ENV{LOGNAME}/distrib";
# create sha sums on output files (implies tarring of image directories first)
$DO_TAR_SUM_FOR_DISTRIB = 0;
# if distribing from cross-mount, turn on flag (postprocs run from torque)
if ($ENV{PBS_O_HOST} =~ /bigmacb/) {
    $DO_TAR_SUM_FOR_DISTRIB = 1;
}
# if distribing from cross-mount, turn on flag (status monitor run from cron)
if ($ENV{HOST} =~ /bigmacb/) {
    $DO_TAR_SUM_FOR_DISTRIB = 1;
}

#
# Organize images only by cycles and send them to the web-dest-host
# Copy to distrib area if $DO_DISTRIB and $DO_TAR_SUM_FOR_DISTRIB
#
$DO_CYCLES_IMAGES = 1;

#
# Run Gridded Bias Correction output generation ('corrected' netcdf and images)
#     actual bias creation is controlled by flexinput.pl
# Copy to distrib area if $DO_DISTRIB and $DO_TAR_SUM_FOR_DISTRIB
#
$DO_GBC = 0;

#
# Copy netcdf output to distrib area if $DO_DISTRIB
#
$DO_NETCDF_1 = 1;
$DO_NETCDF_2 = 1;
$DO_NETCDF_3 = 1;
$DO_NETCDF_4 = 0;

#
# Send output to a destination server where the convert-to-MDV process is running
# currently available for MM5 output only
#
$DO_MDV_1 = 0;
$DO_MDV_2 = 0;
$DO_MDV_3 = 0;
$DO_MDV_4 = 0;

#
# Convert WRF output to grib using wrfpost; copy to distrib area if $DO_DISTRIB
#
$DO_GRIB_1 = 1;
$DO_GRIB_2 = 1;
$DO_GRIB_3 = 1;
$DO_GRIB_4 = 0;

#
# 1 = write GRIB output in one directory; 0 = split output into directories
#
$GRIB_ONEDIR = 1;

#
# The time interval, in minutes. After every such interval GRIB files are
# generated. For example, a value of 180 means GRIB files are generated every
# 3 hours only. If not set, defaults to (60,60,60,60)
#
@GRIB_INTRVL = (60,60,60,60);

#
# Should we add a label on model plots that says 'Cold-start' when the cycle
# is a cold-start cycle? 1 = yes; 0 = no (default)
#
$COLD_START_LABEL = 1;
#
# When COLD_START_LABEL = 1, where is the label placed? Use the offset in
# points from the top-left corner of the plot in the form of {+-}x{+-}y.
# positive x means the label will be placed to the right of the left edge;
# positive y means the label will be placed below the top edge. See
# Imagemagick '-geometry' command-line reference.
#
@CS_LABEL_OFFSET = qw(+110+163 +110+110 +110+110 +110+110 +110+110);

#
#  Send the MMOUTPUT or wrfout files to the web-dest-host for the NAPS-GUI
#
$DO_NAPS_2 = 0;
$DO_NAPS_3 = 0;
$DO_NAPS_4 = 0;

#
# Run NAPS on a default blast-case and send the output to the web-dest-host
#
$DO_NAPS_DEFAULT = 0;

#
# Run the convert-to-MEDOC post-processor and send the files to the web-dest-host
# (these are for SCIPUFF/HPAC)
# copy to distrib area if $DO_DISTRIB and $DO_TAR_SUM_FOR_DISTRIB
#
$DO_MEDOC_1 = 0;
$DO_MEDOC_2 = 0;
$DO_MEDOC_3 = 0;
$DO_MEDOC_4 = 0;
$DO_MEDOC_5 = 0;

#
# Run the tabular sites generation post-processor and send the output to
# the web-dest-host (for MM5 runs - deprecated for WRF runs when the
# portal is available)
#
$DO_SITES = 0;

#
# Run the verification statistics package (deprecated) - now run
# independently from the rest of the postprocessing and set in flexinput.pl
#
$DO_VERIF = 0;

#
# Run the convert-to-stereoVis5d format and send the output to the stereo-dest-host
#
$DO_STEREO = 0;

#
# Run the RIP graphics, and MEDOC postprocessors on tiles (subsets) of the domain output
# --- Not recently tested!!!
#
$DO_TILES = 0;

#
# Should RTFDDA generate domain configuration maps every time it's cold-started?
#
$DO_MAP   = 0;

#
# Flag to do sub-section plots of MAGEN D3, output as D4/D5
#
$DO_MAGEN4 = 0;

#
# Flag to indicate generating model surface varialbes at SAMS sites
#
$DO_SAMS_SITES = 0;

#
# For MOAB/TORQUE jobs, this MUST BE set to localhost!!!
#  The compute nodes do not have external network connectivity.
#  Run an independent script to rsync/copy the web-output to an
#  exposed host or web server if necessary
#  (see /home/fdda-atec/datbin/DistribFiles.pl for an example)
#
$DEST_SERVER = "$ENV{LOGNAME}\@localhost";
#
# All rsyncs use an ssh-key --- this can be a command-restricted key if necessary
$KEY = "/home/$ENV{LOGNAME}/.ssh/id_dsa";
#
#  Destination path for the web products - can be a full-path
# (i.e. /www/htdocs/images) or relative (i.e. $RUNDIR) if using localhost.
#
$JOB_LOC = "$RUNDIR/web";

#
# Allows MDV files to be sent directly to dest machine as they are created.
# Must be localhost for MOAB/TORQUE jobs.
# Currently bypassed (instead use DistribFiles.pl to rsync from
# $RUNDIR/postprocs) because do_mdv.pl is a) only called in MM5 runs and
# b) is using LdataWriter which won't work for file-copy situations that
# are restricted to rsync-only connections)
#
$MDV_DEST_HOST = "4dwxdata\@localhost";
$MDV_DEST_ROOT = "/lustre/project/k1206/x_fisherh/mm5/raw";
$MDV_DEST_KEY  = "/home/$ENV{LOGNAME}/.ssh/rsync-atec";

#
# Allows GRIB files to be sent directly to dest machine as they are created.
# Must be localhost for MOAB/TORQUE jobs.
#
#$GRIB_DEST_HOST = "4dwxdata\@localhost";
$GRIB_DEST_HOST = " ";
$GRIB_DEST_ROOT = "/lustre/project/k1206/x_fisherh/distrib/GWPME/grib";
$GRIB_DEST_KEY  = "/home/$ENV{LOGNAME}/.ssh/output";

#
# The time interval, in seconds, between model output file checking.
#
$SLEEP_TIME = 30;

#
# The time interval, in seconds, between final analysis and forecast output file
# checking (sometimes need a delay to allow final analysis to finish).
#
$SLEEP_BETWEEN_F_AND_P = 450;

#
# Which domain of model output file should be checked to trigger postprocs?
# Useful in the case, for example, where coarser grid runs longer forecast
# then fine grid.
#
$CHECK_DOMAIN = 3;

#
# Flag to work with database for verification pairs.
#
$DO_VERI_PAIR_DATABASE = 1;

#
# The lead time in hours for AnEn (verification pairs).
#
$VERI_PAIR_LEAD_HOURS = 48;

1;

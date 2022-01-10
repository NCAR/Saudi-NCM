#!/usr/bin/perl
#
# Override the job specific configurations
#

#
# The number of CPUs per node
# must be specified
#
$PPN = 32;

#
# The number of processors needed for this job
# must be specified
#
$NUM_PROCS = 32;

#
# MOAB/TORQUE, plus various RTFDDA monitor scripts, will use this
# Email recipient (REQUIRED for group accounts!!! DEFAULT is login-username)
#
$EMAIL_RECIPIENT = "fisherh\@ucar.edu";

# If this is a model job running over a range, then $RANGE=rangeId
# If this is a model job running over some other location, then $RANGE=GRM
# Defaults to GRM
#
$RANGE = "GRM";

#
# The number of domains
# If $NUM_DOMS < 4, then init.pl will set D4_start = 44640 ("never" start domain 4)
# Must be specifed
#
$NUM_DOMS  = 3;

#
# Forecast length in hours for D4
# Defaults to 0
#
$D4_LENGTH = 48;

#
# The time when to start processing domain 4 (in min)
# Defaults to 0
#
$D4_start = 0;

#
# The cycle interval length in hours: 1  2   3   6  12
# Defaults to 1
#
$CYC_INT = 6;

#
#             cut-off final analysis: 0  0  -1   0   0
# Defaults to 0
# THIS MUST MATCH the setting for CYC_INT:
#     $FIN_END=0 for $CYC_INT=1,2,6,12
#     $FIN_END=-1 for $CYC_INT=3
#
$FIN_END = 0;

#
# 1 - this job can only cold-start from 00Z and 12Z;
# 0 - this job can cold-start from nowhour
# COLD_0012 will be set to 0, if CYC_INT > 3
# Defaults to 0
#
$COLD_0012 = 1;


#
# Forecast length in hours from the current hour
# In order to run final analysis only, set FCST_LENGTH to 0
#
$FCST_LENGTH = 72;

#
# The parent directory for the cycle output directory
# Important: Mind the "N" in /raidN
#
# Defaults to /data/cycles. "/raidN/$ENV{LOGNAME}/cycles" from flexinput.pl
#
$RUNDIR_ROOT =  "$BASE_DIR/$ENV{LOGNAME}/cycles";

#
# Path to mpiexec (PBS mode)  or mpirun (INTER mode)  command
# Defaults to /opt/mpich/bin
#
#$MPICMD_BIN_DIR = "/opt/cray/alps/5.2.4-2.0502.9822.32.1.ari/bin/";
#$MPICMD_BIN_DIR = "/usr/local/openmpi/bin";
$MPICMD_BIN_DIR = "/opt/slurm/default/bin/";

#
# Path to GMT_BIN if not in /opt/GMT
# Defaults to /opt/GMT/bin
#
$GMT_BIN = "$BASE_DIR/opt/gmt-4.5.15/bin";

$RTFDDA_LOG_DIR = "$GSJOBDIR/log";

########### END: Most commonly changed parameters ####################

1;

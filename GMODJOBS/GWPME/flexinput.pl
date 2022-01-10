#!/usr/bin/perl
use Cwd;
use File::Basename;

require "stat.pl";
require "flush.pl";
require "ctime.pl";

my $cur_dir = getcwd;
my $log_message = "";
my $flexinput_dir = "";
my $job_flexinput_name = "flexinput.job.pm";
if ( ! -e $flexinput_dir . $job_flexinput_name) {
  $flexinput_dir = dirname( __FILE__ ) . "/";
  if ( ! -e $flexinput_dir . $job_flexinput_name && (defined $GSJOBDIR)) {
    $flexinput_dir = $GSJOBDIR . "/";
  }
  if ( ! -e $flexinput_dir . $job_flexinput_name && (defined $ENV{GSJOBDIR})) {
    $flexinput_dir = $ENV{GSJOBDIR} . "/";
  }
}
if ( ! -e $flexinput_dir . $job_flexinput_name) {
  $log_message = "\n ============== WARN on using flexinput.pl ===============\n"
               . " === Please set the environment variable GSJOBDIR !!!  ===\n"
               . " ============== WARN on using flexinput.pl ===============\n\n";
  print STDERR $log_message;
  print        $log_message;
}
if ( ! defined $BASE_DIR ) {
  $BASE_DIR = "/lustre/project/k1206/"
}

########### Most commonly changed parameters ####################
#
# The resource manager used. Possible values are
# PBS - NSAP clusters, smac-c2, nyc-c1, etc. (default)
# LSF - CISL computers
# INTER - interactive for ngic-c1
#
$BATCH_SYSTEM = "SLURM";

#
# The number of CPUs per node
# must be specified
#
$PPN = 2;

#
# The number of processors needed for this job
# must be specified
#
$NUM_PROCS = 0;

#
# MOAB/TORQUE, plus various RTFDDA monitor scripts, will use this
# Email recipient (REQUIRED for group accounts!!! DEFAULT is login-username)
#
$EMAIL_RECIPIENT = "";

# If this is a model job running over a range, then $RANGE=rangeId
# If this is a model job running over some other location, then $RANGE=GRM
# Defaults to GRM
#
$RANGE = "";

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
$D4_LENGTH = 0;

#
# The time when to start processing domain 4 (in min)
# Defaults to 0
#
$D4_start = 44640;

#
# The cycle interval length in hours: 1  2   3   6  12
# Defaults to 1
#
$CYC_INT = 3;

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
$COLD_0012 = 0;

#
# Forecast length in hours from the current hour
# In order to run final analysis only, set FCST_LENGTH to 0
#
$FCST_LENGTH = 24;

#
# The parent directory for the data feeds
# Defaults to /data/input and is also a environment variable
# Defaults to /data/static when $BCS = "NNRP";
#
#$DATADIR = "/scr/projects/$ENV{LOGNAME}/datainput";
#$DATADIR = "/p/work2/$ENV{LOGNAME}/datainput";
$DATADIR = "$BASE_DIR/datainput";

#
# The parent directory for the cycle output directory
# Important: Mind the "N" in /raidN
#
# Defaults to /data/cycles
#
#$RUNDIR_ROOT =  "/raidN/$ENV{LOGNAME}/cycles";
$RUNDIR_ROOT =  "/lustre/project/k1206/$ENV{LOGNAME}/cycles";

#
# Path to mpiexec (PBS mode)  or mpirun (INTER mode)  command
# Defaults to /opt/mpich/bin
#
############### This has moved to scripts/env_vars.csh ##############
#$MPICMD_BIN_DIR = "/site/intel/impi/5.0.2.044/intel64/bin";
#$MPICMD_BIN_DIR = "/opt/pbs/default/bin";

#
# As of WRF V3.5, WRF output can be in NetCDF4 native compressed format.
# However, RAL's V3.5.1 hangs if input files (wrfbdy, wrfinput, restart files)
# are also in NetCDF4 native compressed format. In order to output compressed
# format, input files need to be converted to be in non-compressed format.
# Setting $NETCDF_CONVERT to 1 will convert the restart files and force the
# wrfbdy, wrfinput files to be written in non-compressed format.
#
$NETCDF_CONVERT = 1;

#
# To convert WRF restart files, the tool nccopy in $NETCDF/bin will be used.
# Do not rely on a default environment NETCDF variable, because it may not
# be able to handle the conversion, or nccopy may not even exist.
#
#$NETCDF = "/site/applic/netcdf/4.1.3-intel";
$NETCDF = "/opt/cray/netcdf/4.4.0";


########### END: Most commonly changed parameters ####################

#
# To see more debugging messages, set this flag to 1
#
$DEBUG = 0;

#
# The GID of the job -
# Must be specified in GSJOBDIR/GSJOBID/scripts/env_vars.csh
#
$GSJOBID = $ENV{GSJOBID};

#
# The job's execution directory directory, i.e. where flexinput.pl resides
# Must be specified in GSJOBDIR/GSJOBID/scripts/env_vars.csh
#
$GSJOBDIR = $ENV{GSJOBDIR};

$job_flexinput = $flexinput_dir . $job_flexinput_name;
if ( -e $job_flexinput ) {
  require "$job_flexinput" ;
}
else {
  $log_message = " === WARNING ===  $job_flexinput does not exist. Could fail to run!\n";
  print        $log_message;
  print STDERR $log_message;
}

$log_message = "     Please create $job_flexinput or $cur_dir/$job_flexinput_name and override it!\n";
die " === ERROR === RUNDIR_ROOT [$RUNDIR_ROOT] does not exist.\n" . $log_message if (! -e $RUNDIR_ROOT);
die " === ERROR === RANGE is not defined.\n" . $log_message if ($RANGE eq "");

########################## Resource Manager Specifics ##########################
#
#
#
# The number of nodes needed for this job
#
$NUM_NODES = $NUM_PROCS/$PPN;

# NO LONGER USED
# Reservation name (MOAB only)
#
$RES_NAME = "";

#
# Queue type (CISL computers only)
# Default: regular
#
$QUEUE_TYPE = "regular";

#
# NO LONGER USED for MOAB
# Project Account Key (MOAB and CISL only, on CISL use 48500032 = ATEC)
#
$ACCOUNT_KEY = "";

#
# MOAB/TORQUE notifications only
# Receive email notification upon each job's start, failure & completion
#
$EMAIL_JOBSTART = 0;
$EMAIL_JOBABORT = 1;
$EMAIL_JOBEND   = 0;

#
# NO LONGER USED
# For WRF2.2 only, should WPS pre-processing run on multiple nodes, in parallel?
# ES: As of 20070717, only "real" can be run on mulitple processors.
#     ungrib and metgrid are serial executables and are called in
#     CSH_ARCHIVE/Forecast/RT_A_wrf_wps.csh
# 1 - yes
# 0 - no
# Default: 1
#
$MPI_PRE_PROCESS = 1;

#
# NO LONGER USED
# Do you need to submit your pre-processing script to a node with more memory?
# This is not used for WRF2.2, when $MPI_PRE_PROCESS == 1.
# 1 - yes
# 0 - no
# Default: 0
#
$BIG_MEM_PRE_PROCESS = 0;

#
# NO LONGER USED
# If you need a bigmem node for pre- or post-processing, set the name of
# the name of the bigmem queue here
# Default: "bigmem"
#
$BIG_MEM_QUEUE_NAME = "bigmem";

#
# NO LONGER USED
# Do you need to submit your post-processing script to a node with more memory?
# Torque must be configured with 'bigmem' nodes
# 1 - yes
# 0 - no
# Default: 0
#
$BIG_MEM_POST_PROCESS = 0;

#
# The pre-processing job's wall time in seconds
# Moab/Torque will kill the job if it exceeds this time limit
#
# Caution: Please be considered and set this to a reasonable value!
# Setting this value too high might waste valuable resources!
#
# Defaults to 60 minutes
#
$PRE_PROC_WALLTIME = "02:00:00";

#
# The MPI job's wall time in seconds
# Moab/Torque will kill the job if it exceeds this time limit
#
# Caution: Please be considered and set this to a reasonable value!
# Setting this value too high might waste valuable resources!
#
# This should be set to the same length as the RTFDDA "cycle", i.e. 3hrs, 6hrs, etc
# Defaults to 60 minutes
#
$MPI_WALLTIME = "05:30:00";

#
# The post-processing job's wall time in seconds
# Moab/Torque will kill the job if it exceeds this time limit
#
# Caution: Please be considered and set this to a reasonable value!
# Setting this value too high might waste valuable resources!
#
# This should be set to the same length as the RTFDDA "cycle", i.e. 3hrs, 6hrs, etc
# Defaults to 60 minutes
#
$POST_PROC_WALLTIME = "06:00:00";

#
# Will normally be set by system clock/cron for real-time
# What is the earliest time the pre-processing job should run?
# Format is YYYYMMDDhhmm.ss
$JOB_START_TIME = "";

######################## Model Configurations #################################
#
#
# The model: "WRF" or "MM5"
# Must be specified
#
$MODEL = "WRF";

#
# Will normally be set by system clock/cron for real-time, or command-line-arg
# The cycle analysis time
#
$this_cycle = "";

#
# The offset used to calculate a real-time cycle analysis time
# For example, to start an 18Z cycle at 19:30, set this ==1
#
$CYCLE_START_OFFSET = 0;

#
# normal - Is this a normal cycle?
#
#  0  - default: In this case $normal will be computated during runtime and
#       will be assigned one of the values below:
#  1  - normal
#  5  - previous cycle failed and $COLD_0012 = 1, i.e., cold start from 00Z
#  17 - previous cycle failed and $COLD_0012 = 1, i.e., cold start from 12Z
#  99 - cold start now
#
$normal = 0;

# CFDDA *ONLY*:
# Job type preproc only should use InterpType = 1
#          preproc for MM5 runs InterpType = 2
$InterpType = 2;

#
# Output intervals in minutes
# Defaults to 60
#
$OUT_INT = 60;

#
# Cold Start Forecast length in hours from the current hour
# In order to run final analysis only, set FCST_LENGTH to 0
# The forecast length for a cold-start cycle may need to be
# shorter than for a restart cycle, due to computing limitations
#
$COLD_START_FCST = 24;

#
# ETKF ensemble
# Defaults to "no"
#
$ETKF = "no";

#
# Set to 1, this parameter allows to reuse already pre-processed ICBC and
# obs data. Note, that the code checks whether reusing of the data is possible.
# That is, even when set to 1 here, during runtime the code may set this
# parameter's value to 0.
#
# Defaults to 0
#
$CLONEIN = 0;

#
# 1 - case study
#
$CASE_STUDY = 0;

#
# 1 - re_run
#
$re_run =  0;

#
# Prelim. Analysis
# 1 - run prelim. analysis
# 0 - do not run prelim. analysis
# Defaults to 1
#
$PRELIM = 1;

#################### FDDA install location #####################################
#
# The FDDA install directory
# $MM5HOME is an environment variable and
# must be specified in GSJOBDIR/GSJOBID/scripts/env_vars.csh
#
$MM5HOME = $ENV{MM5HOME};

#
# The FDDA perl directory
# (containing perl modules and scripts, such as, advan_date.pl)
# Must be specified in GSJOBDIR/GSJOBID/scripts/env_vars.csh
#
$PERL_ARCHIVE = $ENV{PERL_ARCHIVE};

#
# The MM-flex directory
# (containing the flex-perl scripts, such as pre_process_F.pl)
# Must be specified in GSJOBDIR/GSJOBID/scripts/env_vars.csh
#
$PERL_FLEX = $ENV{PERL_FLEX};

################################# SST ##########################################
#
# Path to SST directory
# Defaults to /data/input/sst
#
$DATA_SST_DIR = $DATADIR."/sst";

################################# MODIS SST ####################################
#
# Flag to turn on the MODIS SST files
# Default is 0
# USE_MODIS = 1 will allow the model to use the MODIS SST data
$USE_MODIS = 0;

################################## ICBC ########################################
#
# The global model data
# Possible values are ETA, AVNFTP, GFS004, AVN, NNRP, NNRP2, GEM;
# Defaults to AVNFTP (== gfs3, "avn" is a legacy name for the GFS model)
#
$BCS = "AVNFTP"; # "GEM"

#
# Pressure Top Level of ICBC data
# Defaults to 100 for 'AVN', 'AVNFTP', 'GFS004', 'NNRP', 'NNRP2'
# otherwise to 50 for 'ETA'
# can be 50mb with gfs3/avn now...
#
$PTOP = 50;

#
# The location directory for the ICBC data files
# Defaults to /data/input/avnftp for AVNFTP
#          to /data/input/gfs3 for GFS003-grib2
#          to /data/input/eta for ETA
#          to /data/input/nam212 for NAM212-grib2
#          to /data/input/avn for AVN
#          to /data/input/gfs4 for GFS004-grib2
#          to /data/input/GFS004 for GFS004
#          to /data/static/NNRP for NNRP
#          to /data/static/NNRP2 for NNRP2
#$DATA_ICBC_DIR = $DATADIR."/gfs3";

# The following strings will be substituted by the ICBC script:
#   "CYCLE" == model cycle time - usually 00 or 12
#   "FF"    == forecast hour offset - exact number of digits to use is included
#   "CC"    == century
#   "YY"    == year
#   "MM"    == month
#   "DD"    == day
#   "HH"    == hour
#    I.e. "eta.T00Z.AWIP3D00.tm00.07121700";
#    I.e. "2007121712_fh.0003_tl.press_gr.1p0deg.grib2";
# GFS003-grib2
# $ICBC_NAME_TEMPLATE = "CCYYMMDDHH_fh.FFFF_tl.press_gr.1p0deg.grib2";
#
# complete list of ICBC_NAME_TEMPLATE options:
# GFS003-grib1
#$ICBC_NAME_TEMPLATE = "CCYYMMDDHH_fh.FFFF_tl.press_gr.onedeg";
# GFS003-grib2
#$ICBC_NAME_TEMPLATE = "CCYYMMDDHH_fh.FFFF_tl.press_gr.1p0deg.grib2";
# GFS004-grib2
#$ICBC_NAME_TEMPLATE = "CCYYMMDDHH_fh.FFFF_tl.press_gr.0p5deg.grib2";
# NAM212-grib2
#$ICBC_NAME_TEMPLATE = "CCYYMMDDHH_fh.FFFF_tl.press_gr.awip3d.grib2";
# NAM212-grib1
#$ICBC_NAME_TEMPLATE = "eta.TCYCLEZ.AWIP3DFF.tm00.YYMMDDHH";

#
# IC/BC pre-processor perl script to use
#
# A custom pre-processor script must implement the subroutine 'processData()'
#
# defaults to NAM-preprocessor.pl when $BCS = ETA
# defaults to GFS-preprocessor.pl when $BCS = AVNFTP
# defaults to AVN-preprocessor.pl when $BCS = AVN
# defaults to GFS0004-preprocessor.pl when $BCS = GFS004
# defaults to NNRP-preprocessor.pl when $BCS = NNRP
# defaults to NNRP2-preprocessor.pl when $BCS = NNRP2
# otherwise: must be specified
#
$ICBC_PREPROCESSOR_GEM = $PERL_FLEX.'/ICBC/GEM-preprocessor.pl';
$ICBC_PREPROCESSOR_GFS = $PERL_FLEX.'/ICBC/GFS-preprocessor.pl';
$ICBC_PREPROCESSOR_DWD = $PERL_FLEX.'/ICBC/DWD-preprocessor.pl';
$ICBC_PREPROCESSOR_UKMO = $PERL_FLEX.'/ICBC/UKMO-preprocessor.pl';

$ICBC_NAME_GEM = "CCYYMMDDHH_fh.FFFF.GEM.grib2";
$ICBC_NAME_DWD = "model-offenbach-*FFF-CCYYMMDD_HH0000.gb";
$ICBC_NAME_UKMO = "model-UKMET-*-CCYYMMDD_HH0000.gb";
$ICBC_NAME_GFS = "CCYYMMDDHH_fh.FFFF_tl.press_gr.1p0deg.grib2";

$DATA_ICBC_GEM = $DATADIR."/gem";
$DATA_ICBC_DWD = $DATADIR."/dwd";
$DATA_ICBC_UKMO = $DATADIR."/ukmo";
$DATA_ICBC_GFS = $DATADIR."/gfs3";

#
# Default settings are for GFS-only ICBC!
#
$ICBC_NAME_TEMPLATE  = $ICBC_NAME_GFS; # $ICBC_NAME_GEM
$ICBC_NAME_TEMPLATE2 = $ICBC_NAME_GFS; # always
$DATA_ICBC_DIR = $DATA_ICBC_GFS; # $DATA_ICBC_GEM
$DATA_ICBC_DIR2 = $DATA_ICBC_GFS; # always
$ICBC_PREPROCESSOR = $ICBC_PREPROCESSOR_GFS; # $ICBC_PREPROCESSOR_GEM

############################### Observations ###################################
#
# The observation data processor script
#
# A custom obs processor script must implement the subroutine
# get_and_decode_OBS($stage, $s_date, $e_date, $this_cycle)
#
# Defaults to /data/fddahome/cycle_code/PERL/flex/Observations/Obs-processor.pl
#
$OBS_PROCESSOR = $PERL_FLEX.'/Observations/Obs-processor.pl';

#
# NO LONGER USED
# Flag to opt-out of wrf-obs-qc processing
# If set to true, a script will be run to copy qc_obs files from a parallel
# MM5-RTFDDA system.... this script MUST BE CONFIGURED by the user!
# $GSJOBDIR/scripts/clone_obs.csh $this_cycle $RUNDIR
#
$NO_WRFQC = 0;

#
# The observation data sets
# 1 - use this data set
# 0 - don't use this data set
#
# These are the observation sets used by GWDPGT:
$MADIS = 1;
$RANGE_PROFILER = 1;
$SAMS = 1;
$DTE = 0;
$GTS = 0;
$WMO = 1;
$RAWS = 0;
$OKMESO = 0;
$WVR = 0;
$SAT = 0;
$CLASS = 1;
$ACARS = 0;
$SATWINDS = 0;
$NPN_PROF = 0;
$NIDSVAD = 0;
$BLP_PROF = 0;
$DARPA_SODAR = 0;
$DARPA_PWIDS = 0;
$DARPA_LIDARVAD = 0;
$DARPA_DCNET = 0;
$QWND = 0;
$UAE_MICROSTEP = 0;
$SPECIAL = 0;
$QCOUT = 0;
$TAMDAR = 0;
$IAF_WORLD = 0;
$IAF_BUFR = 0;
$IAF = 0;
$AMV = 0;
$SPDB = 0;

# CFDDA *ONLY*:
$ADP = 0;
$AFCCC = 0;

################################# NCARG ##########################################
#
# Path to ncarg root
# Defaults to /opt/ncl
# init.pl sets $NCARG_LIB = $NCARG_ROOT."/lib"
#
#$NCARG_ROOT = "/scr/projects/COST/ncl_ncarg/6.3.0/intel";
$NCARG_ROOT = "$BASE_DIR/opt/ncl_ncarg-6.3.0/";

#
# Path to ncarg rangs (hi-resolution coastline data)
# Defaults to /opt/ncl/rangs
#
#$NCARG_RANGS_DIR = $NCARG_ROOT."/rangs";
#$NCARG_RANGS_DIR = $MM5HOME."/cycle_code/CONSTANT_FILES/RIP4/rangs";
$NCARG_RANGS_DIR = $NCARG_ROOT."/lib/ncarg/database/rangs";

############################ POST-PROCESSING ###################################
#
# Post-processing: rtfdda_postproc.pl
# 1 - run rtfdda_postproc.pl
# 0 - do not run rtfdda_postproc.pl
# Defaults to 1
#
$POSTPROCESS = 1;

#
# For MM5 only: Concatenate MMOUT-files
# 1 - yes
# 0 - no
# Defaults to 1
# ES - 12182007: Do not use 0 = "no", until QC and verification are adjusted.
$CONCAT_MMOUT = 1;

#
# Post-processing: create MM5 raw files for VMET
# 1 - calls subroutine createMM5VMET()
# 0 - doesn't call above subroutine
# Defaults to 0
#
$MM5FORVMET = 0;

#
# Hybrid 3DVAR control
# "TRUE" - use hybrid 3DVAR
# "FALSE" - do not use hybrid 3DVAR
$HYBRID = "FALSE";

#
# Post-processing: run veri_rtfdda_3hcyc.pl
# 1 - submits the script ${PERL_FLEX}/veri_rtfdda3hcyc_range_wrf.pl
# 0 - doesn't sumbit above script
# Defaults to 0
#
$VERI3HCYC = 0;

#
# The verification job's wall time in seconds
# Moab/Torque will kill the job if it exceeds this time limit
#
# Caution: Please be considered and set this to a reasonable value!
# Setting this value too high might waste valuable resources!
#
# Defaults to 60 minutes
#
$VERI_WALLTIME = "02:00:00";

#
# Post-processing: Whether to use multi-cycle verification or not
# 1 - Use multi-cycle verification
# 0 - Use single-cycle verification
$VERI_MC = 1;

#
# Post-processing: used in veri_rtfdda_3hcyc.pl
# 1 - plot verification stats
# 0 - doesn't plot verification stats
#$VERIPLOT = 0; (deprecated, now in verifyinput.pl)

#
# Post-processing: Create Gridded Bias correction stats and output files
# 1 - run GriddedBiasCorrection and CorrectForecast
# 0 - doesn't run GriddedBiasCorrection and CorrectForecast
$GBC = 0;

#
# Variable to change if you are saving WRF output files in a
# different directory than $RUNDIR for GBC to use
# default is $RUNDIR
$GBCRUNDIR = $RUNDIR_ROOT."/$GSJOBID/$RANGE";

#
# Post-processing: Create Analog Ensemble plots
# Done here instead of postprocs because it is based on previous cycle
# 1 - run Analog Ensemble plots 
# 0 - doesn't run AnEn 
$AnEn = 0;

#
# Post-processing: run /data/climo/run/run_compact_RT.pl
# 1 - calls subroutine climoCompactRT()
# 0 - doesn't call above subroutine
# Defaults to 0
#
$CLIMOCOMPACTRT = 0;

#
# Post-processing: create NAPS output at standard site (FDDA)
#                  run to_naps_rtfdda3hcyc.pl
# 1 - calls subroutine toNaps3hCyc()
# 0 - doesn't call above subroutine
# Defaults to 0
#
$NAPS = 0;

#
# Remove the input data
# 1 - yes
# 0 - no, don't remove input data
# Defaults to 1
#
$CLEANDIR = 1;


############################ More Environment Variables ########################
#
# This env var is being used in many lower level scripts, need to keep
# Will be set to 1
#
$NODE = 1;

#
# This env var is being used in many lower level scripts, need to keep
#
$MM5HOST = $RANGE;

#
# This cycles output directory
# Defaults to /data/cycles/GSJOBID/RANGE
#
$RUNDIR  = $RUNDIR_ROOT."/$GSJOBID/$RANGE";

#
# The data directory in the cycle directory
# Defaults to /data/cycles/GSJOBID/RANGE/data
#
$DATA_DIR = $RUNDIR."/data";

#
# The parent dir for "restart_files"
# Will be set to $RUNDIR
#
$RESTART_ROOT = $RUNDIR;

#
# The parent dir for GEAPSTMP, GEAPSKEP
# Will be set to /d1/$ENV{LOGNAME}/$GSJOBID - on the compute node
# ***FOR JOBS, that run F AND P+FCST, set $GEAP_ROOT to $RUNDIR***
#
$GEAP_ROOT = $RUNDIR;

#
# A temporary directory on the local disk of the compute node
# Defaults to /d1/$ENV{LOGNAME}/$GSJOBID/GEAPSTMP/$this_cycle
#
$GEAPSTMP = $GEAP_ROOT."/GEAPSTMP";

#
# Remove $GEAP_ROOT/GEAPSTMP on the compute node after F-Analysis and P+FCST
# ***FOR JOBS, that run F AND P+FCST, set $CLEAN_GEAPSTMP to 0***
#  1 = "yes" (default)
#  0 = "no"
#
$CLEAN_GEAPSTMP = 0;

#
# A temporary directory on the local disk of the compute node
# Defaults to /d1/$ENV{LOGNAME}/$GSJOBID/GEAPSKEP/$this_cycle
#
$GEAPSKEP = $GEAP_ROOT."/GEAPSKEP";

#
# Remove $GEAP_ROOT/GEAPSKEP on the compute node after F-Analysis and P+FCST
# ***FOR JOBS, that run F AND P+FCST, set $CLEAN_GEAPSKEP to 0***
#  1 = "yes" (default)
#  0 = "no"
#
$CLEAN_GEAPSKEP = 0;

#
# The directory to run the postprocessing scripts - temporary
# working directory...
# Defaults to $RUNDIR
#
$POSTPROCS_TMP_DIR = $RUNDIR;

#
# The directory to save the postprocessing output from this cycle/final-analysis
# for use by the next cycle (for hourly precip diffs)
# Defaults to $RUNDIR/postprocs
#
$POSTPROCS_SAV_DIR = $RUNDIR;

#
# The directory containing RTFDDA shell scripts
# Defaults to /data/fddahome/cycle_code/CSH_ARCHIVE
#
$CSH_ARCHIVE = $MM5HOME."/cycle_code/CSH_ARCHIVE";

#
# The dircetory containing MM5 or WRF executables and other executables
# Defaults to /data/fddahome/cycle_code/EXECUTABLE_ARCHIVE
#
$EXECUTABLE_ARCHIVE = $MM5HOME."/cycle_code/EXECUTABLE_ARCHIVE";

#
# The 'MustHaveDir' script
#
$MustHaveDir = $EXECUTABLE_ARCHIVE."/MustHaveDir";

#
# The 'CheckConfigFiles' script
#
$CheckConfigFiles = $EXECUTABLE_ARCHIVE."/CheckConfigFiles";

#
# Is the model run going to use MPI or OpenMP?
#
# MPI: $MPPJOB = "yes" (default)
# OpenMP: $MPPJOB = "no"
#
$MPPJOB    = "yes";

#
# Are WRF restart files written out and read in per core?
# Note, this requires that variable io_form_restart in WRF namelist
# be set to 102.
#
$RESTART_PER_CORE = 1;

#
# Include RDA realted setting if rda/flexinput.rda.pl exists
#
$rda_flexinput = $flexinput_dir . "rda/flexinput.rda.pl";
if ( -e $rda_flexinput ) {
  require "$rda_flexinput" ;
}
else {
  $log_message = " === INFO === Not included $rda_flexinput\n";
  print        $log_message;
  print STDERR $log_message;
}


#
# Override other variables
#
$opt_flexinput = $flexinput_dir . "flexinput.config.pm";
if ( -e $opt_flexinput ) {
  require "$opt_flexinput" ;
}
else {
  $log_message = " === INFO === Not included $opt_flexinput\n";
  print        $log_message;
  print STDERR $log_message;
}

1;

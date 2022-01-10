#
# env_vars.csh
#
# This file contains necessary environment variables for the submit c-shell
# scripts. They are also needed in and used by the MM-flex perl scripts *AND*
# scripts in CSH_ARCHIVE/Forecast
# All variables MUST BE SET.

# The path to Moab/Torque commands
setenv QSUB_PATH "/opt/slurm/default/bin/"

# The job's name:
setenv GSJOBID "GWPME"

# Base directory for RUNDIR could be  /p/work1 or /p/work2
setenv BASE_DIR "/lustre/project/k1206"

# The job's configuration directory
# (the directory where flexinput.pl, TERRAIN, namelists, etc. are located)
setenv GSJOBDIR "$BASE_DIR/${LOGNAME}/GMODJOBS/${GSJOBID}"

# The job's configuration file
setenv FLEXINPUT "$GSJOBDIR/flexinput.pl"

# The FDDA-install location
setenv MM5HOME "$BASE_DIR/build_mm_released/wrfv3.8.1" 

# The location of the FDDA perl scripts and modules
setenv PERL_ARCHIVE "$MM5HOME/cycle_code/PERL"

# The location of the MM-flex perl scripts (pre_process_F.pl, rtfdda_postproc.pl, etc)
setenv PERL_FLEX "$MM5HOME/cycle_code/PERL/flex"
setenv PERL5LIB ${PERL5LIB}:$BASE_DIR/build_mm_released/wrfv3.8.1/cycle_code/PERL/perl5

# Project account on DPGUT38332XWX on excalibur DPGUT38332X4D on gordon
setenv ACCOUNT_KEY "-A DPGUT38332XWX"

#QUEUE name -q xwx on excalibur  -q DSP0152 on gordon
setenv QUEUE "-q xwx"

#DSP name - currently only used on excalibur should be blank for gordon
setenv DSP "-l dsp=XWX"

#!/usr/bin/perl

###############################################################################
#
# This script contains the subroutine setEnvVars, which is called in
#    init.pl
#    pre_process_F.pl
#    pre_process_P+FCST.pl
#    post_process_clean.pl
#
################################################################################


#-------------------------------------------------------------------------------
# Name: setEnvVars
# Arguments: none
# Return: none
# Description: This method sets environment variables
#-------------------------------------------------------------------------------

sub setEnvVars
{
  $ENV{'GSJOBID'} = "$GSJOBID";
  $ENV{'GSJOBDIR'} = "$GSJOBDIR";
  $ENV{'this_cycle'} = "$this_cycle";
  $ENV{'NUM_DOMS'} = "$NUM_DOMS";
  $ENV{'CYC_INT'} = "$CYC_INT";
  $ENV{'OUT_INT'} = "$OUT_INT";
  $ENV{'FCST_LENGTH'} = "$FCST_LENGTH";
  $ENV{'D4_LENGTH'} = "$D4_LENGTH";
  $ENV{'MPPJOB'} = "$MPPJOB";
  $ENV{'MODEL'} = "$MODEL";
  $ENV{'BCS'} = "$BCS";
  $ENV{'InterpType'} = "$InterpType";

  $ENV{'MM5HOST'} = "$RANGE";
  $ENV{'MM5HOME'} = "$MM5HOME";
  $ENV{'EXECUTABLE_ARCHIVE'} = "$EXECUTABLE_ARCHIVE";
  $ENV{'CSH_ARCHIVE'} = "$CSH_ARCHIVE";
  $ENV{'RUNDIR_ROOT'} =  "$RUNDIR_ROOT";
  $ENV{'RUNDIR'} =  "$RUNDIR";
  $ENV{'DATA_DIR'} = "$DATADIR";
  $ENV{'RESTART_ROOT'} = "$RESTART_ROOT";

  $ENV{'GEAP_ROOT'} = "$GEAP_ROOT";
  $ENV{'GEAPSTMP'} = "$GEAPSTMP";
  $ENV{'GEAPSKEP'} = "$GEAPSKEP";

  $ENV{'CheckConfigFiles'} = "$CheckConfigFiles";
  $ENV{'MustHaveDir'} = "$MustHaveDir";
  
  $ENV{'CONFIGURE_FILES'} = "$CONFIGURE_FILES";
  $ENV{'UTILS'} = "$UTILS";

  $ENV{'ETKF'} = "$ETKF";
}
1;

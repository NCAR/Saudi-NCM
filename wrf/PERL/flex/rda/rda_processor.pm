#!/usr/bin/perl

$ENV{"NCARG_ROOT"} = $NCARG_ROOT;

# Check following variables before calling this:
# - $RDA_NSSL_MOSAIC
# - $RDA_HYBRID
# - $CYC_INT
# - $RUNDIR
# - $GSJOBDIR
# - $RANGE
# - $NUM_PROCS
# - $DATADIR
# - $MM5_HOME
# - $NUM_DOMS
# - $fcst_id
#

#
#---------------------------------------------------------------------
#       Collect and decode Radar data up to the most recent time
#  Level II data first and NSSL MRMS MOSAIC data
#---------------------------------------------------------------------
#

# Turn on Level II Radar through 3DVar.
# The other alternate location would be RT_L_rda.csh (call hy3dvarDriver.pl).
$DO_3DVAR_AT_PRE_F = "TRUE";

sub get_and_decode_Radar_data
{
  (my $s_date, my $e_date, my $this_cycle) = @_ ;

  if ( $RDA_HYBRID eq "TRUE" ) {
    $_indent = "   ";
    print " ----------- rda_processor.pm -----------------------------------------\n";
    #print " DEBUG: RDA_HYBRID: [$RDA_HYBRID]\n";
    #print "        RDA_NSSL_MOSAIC: [$RDA_NSSL_MOSAIC], RDA_LEVEL_II: [$RDA_LEVEL_II]\n";
    
    my $rda_start_time = time();
    my $level_II_duration = 0;
    my $Mosaic_duration = 0;
    if ( $RDA_LEVEL_II && $DO_3DVAR_AT_PRE_F eq "TRUE" ) {
      my $level_II_start_time = time();
      $EXECUTABLE_HOME = "$MM5HOME/cycle_code";
      $rda_script_name = "hy3dvarDriver.pl";
      $RDA_hybrid_script = "$EXECUTABLE_HOME/HY3DVAR/$rda_script_name";
      print "$_indent RDA_hybrid_script: $RDA_hybrid_script\n";
      #print "$_indent LEVELII_RADAR_DATA_DIR from Env: [$ENV{LEVELII_RADAR_DATA_DIR}]\n";
      if ( -e "$RDA_hybrid_script" ) {
        print "  3DVAR analysis start for RDA .....\n";
        #print "$_indent starting $rda_script_name at ", gmctime(), "\n";
        # Note: Do not use fcst_id (it's not available here!)
        # Check start_time and end_time:
        #print "       s_date: $s_date e_date: $e_date, f_fdda_start: $f_fdda_start p_fdda_start: $p_fdda_start\n";
        $RDA_hybrid_arguments = "$this_cycle $CYC_INT $RUNDIR $GSJOBDIR $RANGE $NUM_PROCS $DATADIR $BATCH_SYSTEM $EXECUTABLE_HOME $NUM_DOMS";
        $RDA_hybrid_command = "perl $RDA_hybrid_script $RDA_hybrid_arguments";
        print "$_indent calling $RDA_hybrid_command\n";
        system("$RDA_hybrid_command");
        #print "$_indent ended $rda_script_name at ", gmctime(), "\n";
      }
      $level_II_duration = time() - $level_II_start_time;
    }
    
    if ( $RDA_NSSL_MOSAIC ) {
      print FILEL "  processing radar mosaic at ", gmctime();
      print       "  processing radar mosaic at ", gmctime();
      my $Mosiac_start_time = time();
      my $script_dir = "$CSH_ARCHIVE/rda";
      my $script_name = "RT_S_decode_nsslMosaic_rtfdda_wrf.csh";
      my $out_put_interval = 60;
      print       "$_indent starting $script_dir/$script_name at ", gmctime(), "\n";
      print       "$_indent          args: s_date: [$s_date] e_date: [$e_date]\n";
      print       "$_indent                this_cycle: [$this_cycle] last_cycle: [$last_cycle]\n";
      print       "$_indent                RANGE: $RANGE, NUM_DOMS: $NUM_DOMS interval: $out_put_interval\n";
      system("$script_dir/$script_name $s_date $e_date $this_cycle $last_cycle $RANGE $NUM_DOMS $out_put_interval");
      $Mosaic_duration = time() - $Mosiac_start_time;
      print FILEL "$_indent   ending $script_name at ", gmctime();
      print       "$_indent   ending $script_name at ", gmctime();
    }
    my $rda_duration = time() - $rda_start_time;
    print FILEL "  rda_processor.pm duration: $rda_duration, level_II: $level_II_duration, Mosaic: $Mosaic_duration\n";
    print       "  rda_processor.pm duration: $rda_duration, level_II: $level_II_duration, Mosaic: $Mosaic_duration\n";
  }

}
1;

#!/usr/bin/perl

sub processData
{
  print " this_cycle = $this_cycle\n";
  print " last_cycle = $last_cycle\n";
  print " BCS_date = $BCS_date\n";
  print " RUNDIR = $RUNDIR\n";
  print " DATA_ICBC_DIR = $DATA_ICBC_DIR\n";
  print " DATA_DIR = $DATA_DIR\n";
  print " MODEL = $MODEL\n";
  print " RAP_start = $RAP_start\n";
  print " RAP_end = $RAP_end\n";
  print " normal = $normal\n";
  print " NODE = $NODE\n";
  print " GSJOBID = $GSJOBID\n";
  print " CYC_INT = $CYC_INT\n";
  print " INTERP_TYPE = $InterpType\n";

  print "\n";
  print &ctime(time), "Removing the following old symbolic links:\n";
  system ("find $DATA_DIR -type l -print");
  system ("find $DATA_DIR -type l -exec rm -f \{\} \\;");
  print "\n";
  print &ctime(time), "$DATA_DIR now contains only:\n";
  system ("ls $DATA_DIR");
  print "\n";

#------------------------------------------------------------------------------#
# Bring the NNRP input files to the local work dir

      print FILEL "   Starting $BCS data collection at ", &ctime(time);

      $systat = system("$CSH_ARCHIVE/Forecast/RT_get_nnrp.csh $this_cycle $RAP_start $RAP_end $BCSDT{$BCS} $DATADIR");

      if ($systat != 0)
      {
	  print ("Unable to find NNRP data for cycle $this_cycle, stop here!\n");
	  exit (1)
	  }
      print FILEL "   Finish $BCS data collection at ", &ctime(time);

#------------------------------------------------------------------------------#
# Regridding of NNRP data onto MM5 horizontal grid

  if($MODEL eq "MM5")
  {
      print FILEL "\n   starting $BCS regrid.csh at ", &ctime(time);
      system("$CSH_ARCHIVE/Forecast/RT_A_regrid_nnrp.csh $BCS_date $this_cycle $RAP_start $RAP_end $normal $NODE $InterpType");
      print FILEL "   ending $BCS regrid.csh at ", &ctime(time);

#------------------------------------------------------------------------------#
# Regridding of NNRP data onto WRF horizontal grid

  }elsif($MODEL eq "WRF"){

      print FILEL "\n   starting $BCS wps at ", &ctime(time);
      $BCSINT=10800;
      system("$CSH_ARCHIVE/Forecast/RT_A_wrf_wps.csh $this_cycle $RAP_start $RAP_end $NUM_PROCS $NODE $BCS $BCSINT $NUM_DOMS");
      print FILEL "\n   ending  $BCS wps at ", &ctime(time);

  }

} # sub

1;

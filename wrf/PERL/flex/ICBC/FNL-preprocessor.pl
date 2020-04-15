#!/usr/bin/perl
# 
# Call WPS for FNL input data. See http://dss.ucar.edu/datasets/ds083.2/
# For details. FNL files are expected in the input directory DATA_ICBC_DIR
# to be named:
#  CCYY/fnl_CCYYMMDD_HH_00.grib1 before 2007.12.06 (begin 1999.07.30)
#  CCYY/fnl_CCYYMMDD_HH_00.grib2 after  2007.12.06
# Where CCYYMMDDHH is the analysis time (every 6 hours).
#
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
# Bring the FNL input files to the local work dir

      print FILEL "   Starting $BCS data collection at ", &ctime(time);

      $systat = system("$CSH_ARCHIVE/Forecast/RT_get_FNL.csh $this_cycle $RAP_start $RAP_end $BCSDT{$BCS} $DATA_ICBC_DIR");

      if ($systat != 0)
      {
	  print ("Unable to find FNL data for cycle $this_cycle, stop here!\n");
	  exit (1);
	  }
      print FILEL "   Finish $BCS data collection at ", &ctime(time);

#------------------------------------------------------------------------------#
# Regridding of FNL data onto MM5 horizontal grid

  if($MODEL eq "MM5")
  {
      print "\n   ERROR $CSH_ARCHIVE/Forecast/RT_A_regrid_FNL.csh does not exit ", &ctime(time);
      print FILEL "\n   ERROR $CSH_ARCHIVE/Forecast/RT_A_regrid_FNL.csh does not exit ", &ctime(time);
#     print FILEL "\n   starting $BCS regrid.csh at ", &ctime(time);
#     system("$CSH_ARCHIVE/Forecast/RT_A_regrid_FNL.csh $BCS_date $this_cycle $RAP_start $RAP_end $normal $NODE $InterpType");
#     print FILEL "   ending $BCS regrid.csh at ", &ctime(time);

#------------------------------------------------------------------------------#
# Regridding of FNL data onto WRF horizontal grid

  }elsif($MODEL eq "WRF"){

      print FILEL "\n   starting $BCS wps at ", &ctime(time);
# Should be: 
#     $BCSINT=$BCSDT{$BCS}*3600; 
# But interval_seconds is hard coded to 10800 in namelists/wrf.nl.template
      $BCSINT=10800;
      system("$CSH_ARCHIVE/Forecast/RT_A_wrf_wps.csh $this_cycle $RAP_start $RAP_end $NUM_PROCS $NODE $BCS $BCSINT $NUM_DOMS");
      print FILEL "\n   ending  $BCS wps at ", &ctime(time);

  }

} # sub

1;

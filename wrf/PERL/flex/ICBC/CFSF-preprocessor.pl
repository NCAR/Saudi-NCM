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
# We need to set the PYTHONPATH in the environment to call the PYTHON subscript

  $ENV{PYTHONPATH} = $PYTHONPATH;

#------------------------------------------------------------------------------#
# Bring the CFSF input files to the local work dir
#------------------------------------------------------------------------------#

  print FILEL "   Starting $BCS data collection at ", &ctime(time);

  print "\n$PYTHON_ARCHIVE/flex/ICBC/scpcfsf.py -b $RAP_start -e $RAP_end -i /ngic/static/CFSF/data -o $RUNDIR/data/$BCS\n\n";
  $systat = system("$PYTHON_ARCHIVE/flex/ICBC/scpcfsf.py -b $RAP_start -e $RAP_end -i /ngic/static/CFSF/data -o $RUNDIR/data/$BCS");
# print "\n$PYTHON_ARCHIVE/flex/ICBC/linkcfsf.py -b $RAP_start -e $RAP_end -i $DATA_ICBC_DIR/data -o $RUNDIR/data/$BCS\n\n";
# $systat = system("$PYTHON_ARCHIVE/flex/ICBC/linkcfsf.py -b $RAP_start -e $RAP_end -i $DATA_ICBC_DIR/data -o $RUNDIR/data/$BCS");

  if ($systat != 0)
  {
      print ("Unable to find CFSF data for cycle $this_cycle, stop here!\n");
      exit (1)
  }

  print FILEL "   Finish $BCS data collection at ", &ctime(time);

#------------------------------------------------------------------------------#
# Regridding of CFSR data onto MM5 horizontal grid

  if($MODEL eq "MM5")
  {
      print "\nCFSF not implemented for MM5, exiting...\n\n";
      exit -1;

#------------------------------------------------------------------------------#
# Regridding of CFSF data onto WRF horizontal grid
# argument $BCS controls forcing data to be processed within RT_A_wrf_wps.csh 

  }elsif($MODEL eq "WRF"){

      print FILEL "\n   starting $BCS wps at ", &ctime(time);
      $BCSINT=21600;
      system("$CSH_ARCHIVE/Forecast/RT_A_wrf_wps.csh $this_cycle $RAP_start $RAP_end $NUM_PROCS $NODE $BCS $BCSINT $NUM_DOMS");
      print FILEL "\n   ending  $BCS wps at ", &ctime(time);

  }

} # sub

1;

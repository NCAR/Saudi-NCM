#!/usr/bin/perl
if ( defined $ENV{FLEXINPUT} ) {
  $FLEXINPUT = $ENV{FLEXINPUT};

  if (! -e $FLEXINPUT) {
   print "\nFile $FLEXINPUT is missing..... EXITING\n";
   exit(-1);
  }
  
  print "$_script_name: Using job configuration in $FLEXINPUT\n" if ($DEBUG);
  
  # This input file defines the configuration for the job
  require $FLEXINPUT;

  $RANGE_DOMAIN = "${RANGE_NAME}${D_ID}";  
 
  #$run_dir = "/raid/fieldData/nexrad_level2/Run";
  #$out_dir = "/raid/radar_ingest/nexrad_level2/LittleR/${RANGE_NAME}${D_ID}";
  
  if (defined $ENV{LEVELII_RADAR_DATA_WORK_DIR}) {
    $data_dir = $ENV{LEVELII_RADAR_DATA_WORK_DIR};
  }
  
  if (defined $ENV{LEVELII_RADAR_DATA_DIR}) {
    $out_dir = $ENV{LEVELII_RADAR_DATA_DIR}."/${RANGE_DOMAIN}";
  }
  #elsif (defined $ENV{DATADIR}) {
  #  $out_dir = $ENV{DATADIR}."/radar/rda/levelII/".$ENV{GSJOBID}."/${RANGE_DOMAIN}";
  #}
  else {
    $out_dir = $RDA_LEVELII_MODEL_INPUT_DIR."/".$ENV{GSJOBID}."/${RANGE_DOMAIN}";
  }
  
  $run_dir = "$GSJOBDIR/rda";
  if (defined $ENV{RDA_RUN_DIR}) {
    $run_dir = $ENV{RDA_RUN_DIR};
  }
  
  if ( defined $ENV{RDA_PARAMS_DIR} ) {
    $params_dir = $ENV{RDA_PARAMS_DIR}; 
  } else {
    $params_dir = $run_dir;
  }
  
  $src_dir = $params_dir;
}

#!/usr/bin/perl

if ( defined $ENV{FLEXINPUT} ) {
  $FLEXINPUT  =  $ENV{FLEXINPUT};
  
  #print "\nEnvironment varibale FLEXINPUT is not defined. Do nothing\n";
  if ( -e $FLEXINPUT) {
    #print "$_script_name: Using job configuration in $FLEXINPUT\n" if ($DEBUG);
    require $FLEXINPUT;
    
    if ( defined $RDA_MOSAIC_MODEL_INPUT_DIR ) {
      # out_dir is for merged NSSL output location for domains
      $out_dir = "$RDA_MOSAIC_MODEL_INPUT_DIR/${GSJOBID}";
    }
    else {
      print "$_script_name: \$RDA_MOSAIC_MODEL_INPUT_DIR is not defined\n" if ($DEBUG);
      #$out_dir = "/rdadata/resampled/mrmsMosaic/${GSJOBID}";
    }
    if ( defined $RDA_MOSAIC_INPUT_DIR ) {
      # tile_dir is the input directory (NSSL tiles)
      $tile_dir = "$RDA_MOSAIC_INPUT_DIR";
    }
    else {
      print "$_script_name: \$RDA_MOSAIC_INPUT_DIR is not defined\n" if ($DEBUG);
      #$tile_dir = "/rdadata/inputd/mrmsMosaic";
    }
      #
      #
    $out_dir = "$RDA_MOSAIC_MODEL_INPUT_DIR/${GSJOBID}";
    # tile_dir is the input directory (NSSL tiles)
    #     $tile_dir = "$RDA_MOSAIC_INPUT_DIR";
    #      
    
    if ( defined $ENV{GSJOBDIR} ) {
      $GSJOBDIR = $ENV{GSJOBDIR} ;
      $ter_file = "${GSJOBDIR}/wps/geo_em.d0${D_ID}.nc";
      $namelist = "${GSJOBDIR}/rda/namelist.mosaic_${RANGE}${D_ID}";
    }
    $src_dir = "$EXECUTABLE_ARCHIVE";
  }
}


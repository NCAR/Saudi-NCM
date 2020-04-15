#
# Process one hour of MMOUTPUT to medoc files and scp to the web server 
#
#
sub do_scipuf
{
    my ($hourly_file, $final, $do_output, $dest) = @_;

print "\nin do_scipuf $hourly_file, $final, $do_output, $dest\n\n";

@LETTERS=( 'A', 'B', 'C', 'D' );
@parts = split("DOMAIN",$hourly_file);
$this_domain= substr( @parts[1], 0, 1);
$WEBDIR_MEDOC="$RUN/web/medoc"; # RS

system("$MustHaveDir $SCIPUF_DIR");
system("$MustHaveDir $SCIPUF_ARCHIVE_DIR");
system("mkdir -p $WEBDIR_MEDOC") if (! -e $WEBDIR_MEDOC); # RS
chdir $SCIPUF_DIR;
system("rm -f $SCIPUF_DIR/*");
#
# Executable files
#
  $MM52SCIPUF = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/mm52scipuf";
  $WRF2SCIPUF = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/wrf2ascii4medoc.exe";
  $ASCII2MEDOC = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/medoc_driver.exe";
  $scp_output= $do_output;

if ( -s $hourly_file ) {

  if ( $IS_WRF ) {
    # Convert the WRF data to ASCII format
    &debug($DEBUG, "Converting ${hourly_file} to ASCII format\n");
    system("ln -s $GSJOBDIR/wrfrun/LANDUSE.TBL LANDUSE.TBL");
    &wrf_2_ascii($SCIPUF_DIR, $hourly_file);

  } else {
    # Convert the MM5 data to ASCII format
    &debug($DEBUG, "Converting ${hourly_file} to ASCII format\n");
    &mm5_2_ascii($SCIPUF_DIR, $hourly_file);
  }

  # Get the name of the most recent ASCII output file
  $ascii_file = &get_ascii_file_name($SCIPUF_DIR);
  $config_file = &get_config_file_name( );
  &debug($DEBUG, "ascii_file: $ascii_file\nconfig_file: $config_file\n");

  # Convert the ASCII file to MEDOC format
  &debug($DEBUG, "Converting $ascii_file to MEDOC format\n");
  if ( $final == 1 ) {
  &ascii_2_medoc( $ascii_file, $config_file, "final" );
  &move_medoc_file( $ascii_file.".final.fmt", $dest );
  } else {
  &ascii_2_medoc( $ascii_file, $config_file, "fcst" );
  &move_medoc_file( $ascii_file.".fcst.fmt", $dest );
  }

  # clean up all the files in the $SCIPUF_DIRectory
  &debug($DEBUG, "cleaning up $SCIPUF_DIR directory\n");
  system("rm -f ${SCIPUF_DIR}/*");

} else {
  &debug($DEBUG, "File $hourly_file does not exist. No MEDOC file is created.\n");
}# end if ( -s $hourl_file )

}
#
# Get the name of the configuration file
#
sub get_config_file_name
{
    @cf = qx / ls ${SCIPUF_DIR}\/*.cfg / ;
    chop(@cf[0]);

    return @cf[0];
}



#
# Return the file name with the latest date since we only want to convert the last time period
#
# @args - 0 - Directory to look in
#
sub get_ascii_file_name
{
    local($dir) = @_[0];
    local(@file_stems) = qx / ls $dir / ;
    local($prefix) = substr(@file_stems[0], 0, 1);
    local($y) = 0;

    # Convert the files to dates by chopping off the first character in the string
    for( local($x) = 0; $x < @file_stems; $x++ )
    {
        chomp(@file_stems[$x]);

        # Do not include the .cfg file
        if( length(@file_stems[$x]) == 13 )
        {
            @dates[$y] = substr(@file_stems[$x], 1, length(@file_stems[$x]));
            $y++;
        }
    }

    local($return_value) = @dates[0];


    for( $x = 1; $x < @dates; $x++ )
    {
        if( $return_value < @dates[$x] )
        {
            $return_value = @dates[$x];
        }
    }

    return ${prefix}.${return_value};
}



#
# Convert this MM5 file to an ascii file
#
# @args - 0 - directory
# @args - 1 - MM5 file
#
sub mm5_2_ascii
{
    local($dir) = @_[0];
    local($file) = @_[1];

    &debug($DEBUG, "    Executing: $MM52SCIPUF $file\n");
    system( "cd $dir; $MM52SCIPUF $file 30" );
}

#
# Convert this WRF file to an ascii file
#
# @args - 0 - directory
# @args - 1 - WRF file
#
sub wrf_2_ascii
{
    local($dir) = @_[0];
    local($file) = @_[1];

    &debug($DEBUG, "    Executing: $WRF2SCIPUF $file\n");
    system( "cd $dir; $WRF2SCIPUF $file 30" );
}



#
# Convert this ascii file to a medoc file
#
# @args - 0 - ASCII data file
# @args - 1 - config file
# @args - 2 - file type (final, prelim, or fcst)
#
sub ascii_2_medoc
{
    $ASCII = @_[0];
    $CONFIG = @_[1];
    $FILETYPE = @_[2];

    # make the file called list which contains a list of ASCII MM5 data files to be converted
  ##open(LIST, ">${SCIPUF_DIR}/list");
  ##print( LIST $ASCII, "\n" );
  ##close(LIST);

    # create a symbolic link to the configuration file
  ##system("cd ${SCIPUF_DIR}; rm scipuff.cfg; ln -s $CONFIG scipuff.cfg");
    system("cd ${SCIPUF_DIR}");

    # run the conversion program
    if ( $JOB_ID=~ /ITALY/ || $JOB_ID=~ /GWTORI/ || $JOB_ID=~ /CRTC/ ) {
    system("cd ${SCIPUF_DIR}; ${ASCII2MEDOC} -inp $ASCII -cfg $CONFIG -P");
    } else {
    system("cd ${SCIPUF_DIR}; ${ASCII2MEDOC} -inp $ASCII -cfg $CONFIG -L");
    }

    unlink $ASCII;
    unlink $CONFIG;

    # change the file name of the newly created medoc file
    system("cd ${SCIPUF_DIR}; mv medoc.fmt ${ASCII}.${FILETYPE}.fmt");

}


#
# G'zip the MEDOC files and scp them to the web server
#
# @args - 0 - MEDOC file
# @args - 1 - Destination string for scp command
#
sub move_medoc_file
{
    $medoc_file = $_[0];
    $dest = $_[1];
    $medoc_file_s = $medoc_file; # D200601301700.final.fmt
    $Dletter = @LETTERS[$this_domain-1];
    substr($medoc_file_s,0,1)=$Dletter;
    $move_command = "mv $dest/medoc_tmp${this_domain} $dest/${medoc_file_s}.gz";
    $del_command = "rm $dest/".substr($medoc_file, 0, 13)."\*";

#GCAT naming begin
#RS $ccyymmddhhmn = substr($medoc_file,1,12);
#RS $medoc_file_gcat = "D${this_domain}.${ccyymmddhhmn}.final.medoc";
#GCAT naming end

   if(-e ${SCIPUF_DIR}."/".${medoc_file}) {
    system( "cd ${SCIPUF_DIR}; gzip $medoc_file" );
    if ( $scp_output ) {

#LPC    system( "ssh $DEST_SERVER $del_command" );
#LPC    system( "cd ${SCIPUF_DIR}; scp ${medoc_file}.gz $DEST_SERVER:${dest}/medoc_tmp${this_domain}" );
#LPC    system( "ssh $DEST_SERVER $move_command"); 

        print "\n ssh $RELAY_USER \"cd ${SCIPUF_DIR}; rsync -e 'ssh -i $KEY' -avzC ${medoc_file}.gz $DEST_SERVER:${dest}/\"\n";

#       system( "ssh $RELAY_USER \"cd ${SCIPUF_DIR}; rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC ${medoc_file}.gz $DEST_SERVER:${dest}/\"" );
        system("chmod g+w ${medoc_file}.gz ");
        system( "cd ${SCIPUF_DIR}; rsync -e 'ssh -i $KEY' -avzC ${medoc_file}.gz $DEST_SERVER:${dest}/" );

        if ($DO_DISTRIB) {
# proceed only if DO_TAR_SUM_FOR_DISTRIB is turned on
           if ($DO_TAR_SUM_FOR_DISTRIB) {
              if ( -e "${SCIPUF_DIR}/${medoc_file}.gz") {
                 print  "\npreparing for distrib:  cd ${SCIPUF_DIR}; sha256sum ${medoc_file}.gz > ${medoc_file}.gz.sum; cp ${medoc_file}.gz* $DISTRIB_DIR/medoc\n";
                 system( "cd ${SCIPUF_DIR}; sha256sum ${medoc_file}.gz > ${medoc_file}.gz.sum; cp ${medoc_file}.gz* $DISTRIB_DIR/medoc" );
              } else {
                 print "\n${SCIPUF_DIR}/${medoc_file}.gz doesn't exist - not copying to $DISTRIB_DIR/medoc\n";
              }
           }
        }

#GCAT naming begin
#RS print "\n ssh $RELAY_USER \"cd ${SCIPUF_DIR}; rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC ${medoc_file}.gz $DEST_SERVER:${dest}/${medoc_file_gcat}.gz\"\n";
#    system( "ssh $RELAY_USER \"cd ${SCIPUF_DIR}; rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC ${medoc_file}.gz $DEST_SERVER:${dest}/${medoc_file_gcat}.gz\"" );
#RS  system("chmod g+w ${medoc_file}.gz ");
#RS system( "cd ${SCIPUF_DIR}; rsync -e 'ssh -i $KEY' -avzC ${medoc_file}.gz $DEST_SERVER:${dest}/${medoc_file_gcat}.gz" );
#GCAT naming end

    }
    system( "cd ${SCIPUF_DIR}; cp ${medoc_file}.gz ${SCIPUF_ARCHIVE_DIR}" );
    system( "scrub 1 ${SCIPUF_ARCHIVE_DIR}" );
    system( "cd ${SCIPUF_DIR}; cp ${medoc_file}.gz ${WEBDIR_MEDOC}" ); # RS
   }

print "\nexiting do_scipuf $hourly_file, $final, $do_output, $dest\n\n";
}

1;

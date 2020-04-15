#
# Process one hour of MMOUTPUT to SCIPUF PRF files and scp to the web server 
#
#
sub do_scipuf_prf_gcat
{
    my ($hourly_file, $final, $do_output, $dest, $JOB_ID) = @_;

@LETTERS=( 'A', 'B', 'C', 'D' );
@parts = split("DOMAIN",$hourly_file);
$this_domain= substr( @parts[1], 0, 1);

$SCIPUF_PRF_DIR = $SCIPUF_DIR;
$SCIPUF_PRF_ARCHIVE_DIR = $SCIPUF_ARCHIVE_DIR;

system("$MustHaveDir $SCIPUF_PRF_DIR");
system("$MustHaveDir $SCIPUF_PRF_ARCHIVE_DIR");
chdir $SCIPUF_PRF_DIR;
system("rm -f $SCIPUF_PRF_DIR/*");
#
# Executable files
#
  $MM52SCIPUF = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/mm52scipuf";
  $ASCII2PRF = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ascii2prf_d";
  $ASCII2PRF = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/prf_driver.exe";
  $scp_output= $do_output;

  # Convert the MM5 data to ASCII format
  &debug($DEBUG, "Converting ${hourly_file} to ASCII format\n");
  &mm5_2_ascii($SCIPUF_PRF_DIR, $hourly_file);

  # Get the name of the most recent ASCII output file
  $ascii_file = &get_ascii_file_name($SCIPUF_PRF_DIR);
  $config_file = &get_config_file_name( );
  &debug($DEBUG, "ascii_file: $ascii_file\nconfig_file: $config_file\n");

  # Convert the ASCII file to MEDOC format
  &debug($DEBUG, "Converting $ascii_file to PRF format\n");
  if ( $final == 1 ) {
  &ascii_2_medoc( $ascii_file, $config_file, "final" );
  &move_medoc_file( $ascii_file.".final.fmt", $dest );
  } else {
  &ascii_2_medoc( $ascii_file, $config_file, "fcst" );
  &move_medoc_file( $ascii_file.".fcst.fmt", $dest );
  }

  # clean up all the files in the $SCIPUF_PRF_DIR
###TEST  &debug($DEBUG, "cleaning up $SCIPUF_PRF_DIR directory\n");
###TEST  system("rm -f ${SCIPUF_PRF_DIR}/*");

}
#
# Get the name of the configuration file
#
sub get_config_file_name
{
    @cf = qx / ls ${SCIPUF_PRF_DIR}\/*.cfg / ;
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
    system( "cd $dir; $MM52SCIPUF $file" );
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
    open(LIST, ">${SCIPUF_PRF_DIR}/list");
    print( LIST $ASCII, "\n" );
    close(LIST);

    # create a symbolic link to the configuration file
    system("cd ${SCIPUF_PRF_DIR}; rm scipuff.cfg; ln -s $CONFIG scipuff.cfg");

    # run the conversion program
    system("cd ${SCIPUF_PRF_DIR}; ${ASCII2PRF} -L");

    # change the file name of the newly created medoc file
    system("cd ${SCIPUF_PRF_DIR}; mv prf.fmt ${ASCII}.${FILETYPE}.fmt");

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
    $medoc_file_s = $medoc_file;
    $Dletter = @LETTERS[$this_domain-1];
    substr($medoc_file_s,0,1)=$Dletter;
    $move_command = "mv $dest/medoc_tmp${this_domain} $dest/${medoc_file_s}.gz";
    $del_command = "rm $dest/".substr($medoc_file, 0, 13)."\*";

   if(-e ${SCIPUF_PRF_DIR}."/".${medoc_file}) {
    system( "cd ${SCIPUF_PRF_DIR}; gzip $medoc_file" );
    if ( $scp_output ) {
#LPC    system( "ssh $DEST_SERVER $del_command" );
#LPC    system( "cd ${SCIPUF_PRF_DIR}; scp ${medoc_file}.gz $DEST_SERVER:${dest}/medoc_tmp${this_domain}" );
#LPC    system( "ssh $DEST_SERVER $move_command"); 
    system( "cd ${SCIPUF_PRF_DIR}; rsync -e 'ssh -i /data/GMODJOBS/$JOB_ID/rtfdda.key' -avzC ${medoc_file}.gz $DEST_SERVER:${dest}/" );
    }
    system( "cd ${SCIPUF_PRF_DIR}; cp ${medoc_file}.gz ${SCIPUF_PRF_ARCHIVE_DIR}" );
    system( "/home/fddasys/bin/scrub 1 ${SCIPUF_PRF_ARCHIVE_DIR}" );
   }
}

1;

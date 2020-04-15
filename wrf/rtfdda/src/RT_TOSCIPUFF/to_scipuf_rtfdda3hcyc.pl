#! /usr/bin/perl

#
# David Hahn
# hahnd@ucar.edu
# 2000-11-07 14:10:02
# c-tux:/home/hahnd/rt-fdda/to_scipuf.pl
#
# $Id: to_scipuf_rtfdda3hcyc.pl,v 1.1.1.2 2007/10/19 21:14:30 fisherh Exp $
#





# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#               C O N S T A N T S                     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

#
# Turn the debugging information on(1) and off(0)
#
$DEBUG = 1;


#
# The number of seconds to sleep between file size checks
#
$SLEEP_TIME = 60;

#
# Directory with the MMOUT files
#
$OUTPUT_DIR = "/d1/fdda/cycles";

#
# Directory for scipuff files
#
$SCIPUF_DIR = "/d1/fdda/cycles/medoc";

#
# Date and time of this cycle
#
$THIS_CYCLE = @ARGV[0];

#
# Critical time file
#
$CRITICAL_TIME_FILE = "/d1/fdda/cycles/critic.time";

#
# Domains to include in this processing
#
@DOMAINS = ( 3 );
@DOMAIN_PREFIX = ( "C" );
$FIRST_DOMAIN = @DOMAINS[0];


#
# Executable files
#
$MM52SCIPUF = "/d1/fddasys/fddahome/cycle_code/EXECUTABLE_ARCHIVE/mm52scipuf";
$ASCII2MEDOC = "/d1/fddasys/fddahome/cycle_code/EXECUTABLE_ARCHIVE/medoc_driver.exe";

#
# Number of time steps in the output
#
$FINAL_TIME_STEPS = 3;
$PRELIM_TIME_STEPS = 15;



# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                      M A I N                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

if( $THIS_CYCLE == "system" )
{
    print( "Debug information is ", $DEBUG, "\n" );

    local(@stat) = lstat(${ASCII2MEDOC}."1");
    if( @stat[2] == 33261 )
    {
	print( ${ASCII2MEDOC}."1...ok.\n" );
    }
    else
    {
	print( ${ASCII2MEDOC}."1...missing.\n" );
    }

    local(@stat) = lstat(${ASCII2MEDOC}."2");
    if( @stat[2] == 33261 )
    {
	print( ${ASCII2MEDOC}."2...ok.\n" );
    }
    else
    {
	print( ${ASCII2MEDOC}."2...missing.\n" );
    }

    local(@stat) = lstat(${ASCII2MEDOC}."3");
    if( @stat[2] == 33261 )
    {
	print( ${ASCII2MEDOC}."3...ok.\n" );
    }
    else
    {
	print( ${ASCII2MEDOC}."3...missing.\n" );
    }

    local(@stat) = lstat(${ASCII2MEDOC}."4");
    if( @stat[2] == 33261 )
    {
	print( ${ASCII2MEDOC}."4...ok.\n" );
    }
    else
    {
	print( ${ASCII2MEDOC}."4...missing.\n" );
    }

    local(@stat) = lstat(${MM52SCIPUF});
    if( @stat[2] == 33261 )
    {
	print( ${MM52SCIPUF}."...ok.\n" );
    }
    else
    {
	print( ${MM52SCIPUF}."...missing.\n" );
    }

    print( "MM5 output found in: ", $OUTPUT_DIR, "\n" );
    print( "MEDOC data written to: ", $SCIPUF_DIR, "\n" );
    print( "Critical time file: ", $CRITICAL_TIME_FILE, "\n" );
}
else
{
#   while( true )
#   {
    	&main;
#   	$THIS_CYCLE = &hh_advan_date($THIS_CYCLE, 3);
#   }
}

exit(0);





#
#
sub main
{
    # Construct the file names
    ($FOUT_SUFFIX, $POUT_SUFFIX) = &get_file_suffix; 

    # Get the domain
    $this_domain = @DOMAINS[0];

    $FINAL_FILE = "${OUTPUT_DIR}/${THIS_CYCLE}/MM5_F/MMOUT_DOMAIN${this_domain}_${FOUT_SUFFIX}";
    $PRELIM_FILE = "${OUTPUT_DIR}/${THIS_CYCLE}/MM5_P/MMOUT_DOMAIN${this_domain}_${POUT_SUFFIX}";

    # Process the final analysis cycle
    for( $time_step = 0; $time_step < $FINAL_TIME_STEPS; $time_step++ )
    {
	# Wait for the file size to change (indicating that there was a new output)
	&debug($DEBUG, "Waiting for file ${FINAL_FILE}\n");
	&wait_for_file(${FINAL_FILE}, ${SLEEP_TIME});

	# Convert the MM5 data to ASCII format
	&debug($DEBUG, "Converting ${FINAL_FILE} to ASCII format\n");
	&mm5_2_ascii($SCIPUF_DIR, $FINAL_FILE);

	# Get the name of the most recent ASCII output file
	$ascii_file = &get_ascii_file_name($SCIPUF_DIR);
	$config_file = &get_config_file_name( );
	&debug($DEBUG, "ascii_file: $ascii_file\nconfig_file: $config_file\n");

	# Convert the ASCII file to MEDOC format
	&debug($DEBUG, "Converting $ascii_file to MEDOC format\n");
	&ascii_2_medoc( $ascii_file, $config_file );
	&move_medoc_file( $ascii_file.".fmt", "4dwx\@atec-server.rap.ucar.edu:/DPG/scipuff/fdda/" );
	#&move_medoc_file( $ascii_file.".fmt", "4dwx\@dpg-server.rap.ucar.edu:/DPG/scipuff/fdda/" );

    	# clean up all the files in the $SCIPUF_DIRectory
	&debug($DEBUG, "cleaning up $SCIPUF_DIR directory\n");
    	system("rm -f ${SCIPUF_DIR}/*");
    }


    # Process the preliminary & forecast cycle
#    for( $time_step = 0; $time_step < $PRELIM_TIME_STEPS; $time_step++ )
    while( 1 )
    {
        # Wait for the file size to change (indicating that there was a new output)
        &debug($DEBUG, "Waiting for file ${PRELIM_FILE}\n");
        &wait_for_file(${PRELIM_FILE}, ${SLEEP_TIME});

        # Convert the MM5 data to ASCII format
        &debug($DEBUG, "Converting ${PRELIM_FILE} to ASCII format\n");
        &mm5_2_ascii($SCIPUF_DIR, $PRELIM_FILE);

        # Get the name of the most recent ASCII output file
        $ascii_file = &get_ascii_file_name($SCIPUF_DIR);
        $config_file = &get_config_file_name( );

        # Convert the ASCII file to MEDOC format
        &debug($DEBUG, "Converting $PRELIM_FILE to MEDOC format\n");
        &ascii_2_medoc( $ascii_file, $config_file );                           
        &move_medoc_file( $ascii_file.".fmt", "4dwx\@atec-server.rap.ucar.edu:/DPG/scipuff/fdda/" );
        #&move_medoc_file( $ascii_file.".fmt", "4dwx\@dpg-server.rap.ucar.edu:/DPG/scipuff/fdda/" );

        # clean up all in the $SCIPUF_DIRectory
        system("rm -f ${SCIPUF_DIR}/*");
    }
}






# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#               S U B R O U T I N E S                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

#
# Sleep until the size of the specified file changes 
#
sub wait_for_file
{
    local($filename) = @_[0];
    local($sleep)    = @_[1];

    # Get the initial file size
    $start_size   = &get_file_size($filename);
    $current_size = &get_file_size($filename);

    # Sleep while the file size has not changed
    while( $start_size == $current_size )
    {
	sleep($sleep);
	$current_size = &get_file_size($filename);
	&debug($DEBUG, "    Checking file $filename\n");
    }
    
    # File is removed when this cycle is finished. So, this program exit. 
    exit if($current_size < $start_size);

    # Sleep for an additional 20 seconds to make sure the output is finished writing
    sleep(20);

    $current_size = &get_file_size($filename);
    &debug($DEBUG, "About to process MM5 out file with size $current_size\n");
}



#
# Returns the file size of the specified file
#
sub get_file_size
{
    local($filename) = @_[0];

    # If the file doesn't exist, then its size is zero
    if( ! -e $filename )
    {
	return 0;
    }

    # Stat the file and get the size
    local(@file_info) = lstat( $filename );
    local($file_size) = @file_info[7];

    return $file_size;
}



#
# Get the name of the configuration file
#
sub get_config_file_name
{
    @cf = qx / ls ${SCIPUF_DIR}\/*.cfg /;
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
    local(@file_stems) = qx / ls $dir /;
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
#
sub ascii_2_medoc
{
    $ASCII = @_[0];
    $CONFIG = @_[1];

    # make the file called list which contains a list of ASCII MM5 data files to be converted
    open(LIST, ">${SCIPUF_DIR}/list");
    print( LIST $ASCII, "\n" );
    close(LIST);

    # create a symbolic link to the configuration file
    system("cd ${SCIPUF_DIR}; ln -s $CONFIG scipuff.cfg");

    # run the conversion program
    system("cd ${SCIPUF_DIR}; ${ASCII2MEDOC}${this_domain}");

    # change the file name of the newly created medoc file
    system("cd ${SCIPUF_DIR}; mv medoc.fmt ${ASCII}.fmt");

    # clean up
    #system("cd ${SCIPUF_DIR}; rm list scipuff.cfg $ASCII");
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

    system( "cd ${SCIPUF_DIR}; gzip $medoc_file" );
    system( "cd ${SCIPUF_DIR}; scp ${medoc_file}.gz ${dest}" );
    system( "cd ${SCIPUF_DIR}; rm ${medoc_file}.gz" );
}


#
# Advance the date by the given number of hours
# 
# @args - 0 - Date
# @args - 1 - Number of hours to advance the date
#
sub hh_advan_date
{
    %mon_days = (1,31,2,28,3,31,4,30,5,31,6,30,7,31,8,31,9,30,10,31,11,30,12,31);
    (my $s_date, my $advan_hh) = @_ ;

    my $yy = substr($s_date,0,4);
    my $mm = substr($s_date,4,2);
    my $dd = substr($s_date,6,2);
    my $hh = substr($s_date,8,2);

    my $feb = 2;
    $mon_days{$feb} = 29 if ($yy%4 == 0 && ($yy%400 == 0 || $yy%100 != 0));

    $hh = $hh + $advan_hh;

    while($hh > 23)
    {
        $hh -= 24;
        $dd++;
    }

    while($dd > $mon_days{$mm+0})
    {
        $dd = $dd - $mon_days{$mm+0};
        $mm++;
    }

    while($mm > 11)
    {
        $mm -= 12;
        $yy++;
    }

    while($hh < 0)
    {
        $hh += 24;
        $dd--;
    }

    if($dd < 1)
    {
        $mm--;
        $dd += $mon_days{$mm+0};
    }

    while($mm < 1)
    {
        $mm += 12;
        $yy--;
    }

    my $new_date = sprintf("%04d%02d%02d%02d",$yy,$mm,$dd,$hh);

    return $new_date;
}



#
# Determine the suffix of the file to look at
#
sub get_file_suffix
{
    open(CRITIC, $CRITICAL_TIME_FILE);

    $time_max = <CRITIC>;
    chomp($time_max);

    if( ($time_max == 0) || (! $time_max) )
    {
	&debug($DEBUG, "Previous cycle failed and $this_cycle is not good for cold-start\n");
	exit(0);
    }

    elsif( $time_max == 1 )
    {
	&debug($DEBUG, "Previous cycle failed and $this_cycle is a cold-start\n");
	$FOUT_SUFFIX = "";
	$POUT_SUFFIX = "01";
    }

    else
    {
	&debug($DEBUG, "The cycle $THIS_CYCLE is a normal cycle\n");
	$FOUT_SUFFIX = sprintf( "%02d", ($time_max - 240) / 180 + 1 );
	$POUT_SUFFIX = sprintf( "%02d", ($FOUT_SUFFIX + 1) );
    }

    return ($FOUT_SUFFIX, $POUT_SUFFIX);
}



#
# If the debugging information is turned on, then print the message
#
sub debug
{
    $debug_on = @_[0];
    $debug_message = @_[1];

    if( $debug_on == 1 )
    {
	$| =1;
	print( $debug_message );
    }
}

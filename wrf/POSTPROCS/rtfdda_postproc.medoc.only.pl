#! /usr/bin/perl

#
# This script is the master control script for all of the post-processors
# from the RT-FDDA system.  
#
# 1. Initialize and make any needed directories
# 2. Check for an MMOUTPUT file to process
# 3. Copy the MMOUTPUT file to a date-stamped file name
# 4. Call each post-processor, selected by config file
#    RIP graphics
#    MEDOC (domains 3&4)
#    VERIF stats (final and fcst)
#    NAPS (output files for interactive naps)
#    NAPS (default case -- at the end of the cycle)
#

# Command line args required:
#  $RANGE  
#  $NODE_MEM (0 for MPP/non-ensemble runs)
#  $JOB_ID 
#  $THIS_CYCLE

$DO_OUTPUT = 0;

# Command line args 
if ( $#ARGV >= 2 ) {
  $RANGE = @ARGV[0];
  $NODE_MEM = @ARGV[1];
  $JOB_ID =  @ARGV[2];
  $THIS_CYCLE =  @ARGV[3];
  $RUNDIR = "/data/cycles/$JOB_ID/$NODE_MEM";
#  Be sure the job-control script has started so that directories are made
  $cnt = 0;
  while ( ! -e $RUNDIR && $cnt < 100 ) {
    $cnt++;
   sleep (30);
  }
  if ( ! -e $RUNDIR ) {
   print ( " RUNDIR does not exist!  $RUNDIR \n");
   exit (1);
  }
} else {
  print ( " Usage:   $0  RANGE NODE_MEM JOB_ID <THIS_CYCLE>\n");
  exit (1);
}
#
# code and run-time directories
$MM5HOME = "/data/fddahome";
$RUNDIR = "/data/cycles/$JOB_ID/$NODE_MEM";
$POSTPROCS_DIR = "$MM5HOME/cycle_code/POSTPROCS";
$WORK_DIR = "$RUNDIR/postprocs";
$MM5HOST = $RANGE;
#
# Subroutines for each post-processor
require "$POSTPROCS_DIR/do_naps.pl";
require "$POSTPROCS_DIR/do_scipuf.pl";
require "$POSTPROCS_DIR/do_sites.pl";
require "$POSTPROCS_DIR/do_plots+obs.pl";
require "$POSTPROCS_DIR/conv2gif.pl";
require "$POSTPROCS_DIR/conv2gif_obs.pl";
require "$POSTPROCS_DIR/veri_rtfdda3hcyc.pl";
require "$POSTPROCS_DIR/do_stereo_vis.pl";
require "$POSTPROCS_DIR/do_html.pl";
#
# Read these from the configuration file
#
require "/data/GMODJOBS/$JOB_ID/flexinput.pl";
###TESTINGrequire "$POSTPROCS_DIR/flexinput.pl";
if ( -e  "/data/GMODJOBS/$JOB_ID/postprocinput.pl" ) {
require "/data/GMODJOBS/$JOB_ID/postprocinput.pl";
} else {
   $USER = "ncaruser";
}
###TESTINGrequire "$POSTPROCS_DIR/postprocinput.pl";

# Read the run_status file to see if this is a GEAPS job
open(INFILE, "/data/GMODJOBS/$JOB_ID/run_status");
$IS_GEAPS=0;
while (<INFILE>) {
    if (m/GEAPS/) {
      $IS_GEAPS = 1;
    }
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#               C O N S T A N T S                     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Turn the debugging information on(1) and off(0)
#
$DEBUG = 1;
#
# The number of seconds to sleep between file checks
#
$SLEEP_TIME = 20;
#
# Will MM5 generate a new file for each output time step (i.e. each hour)?
$HOURLY_OUTPUT_FLAG = 1;
#
# Which post processors to run
$DO_NAPS_2 = 0;
$DO_NAPS_3 = 0;
$DO_NAPS_DEFAULT = 0;
$DO_RIP = 0;
$DO_SITES = 0;
$DO_MEDOC_1 = 0;
$DO_MEDOC_2 = 0;
$DO_MEDOC_3 = 1;
$DO_MEDOC_4 = 0;
$DO_VERIF = 0;
$DO_STEREO = 0;
$DO_TILES = 0;
#
# cycle control constants
$FINAL_TIME_STEPS = $CYC_INT*60.0 / $OUT_INT;
$PRELIM_TIME_STEPS = $FCST_LENGTH*60.0 / $OUT_INT ;
$NDOMAINS = $NUM_DOMS;
$MAX_ITER=120;
#
# Set an array of domain names -- set the length of the array to the
# actual number of domains for this case
@domains = ('1', '2', '3','4' );
$#domains = $NDOMAINS;	
# Domain to use to check for new output files
$DOMAIN = 2;

# Set environment vars for other scripts
$ENV{'MM5HOST'} = $MM5HOST;
$ENV{'MM5HOME'} = $MM5HOME;
$ENV{'RUNDIR'} = $RUNDIR;
$ENV{'DATADIR'} = $DATADIR;
$ENV{'DATA_DIR'} = $DATADIR;

$CSH_ARCHIVE = $MM5HOME.'/cycle_code/CSH_ARCHIVE';
$EXECUTABLE_ARCHIVE = $MM5HOME.'/cycle_code/EXECUTABLE_ARCHIVE';
$MustHaveDir = "/home/fddasys/bin/musthavedir";
#
# Make the working directory
system("$MustHaveDir $WORK_DIR");

# Make some directories on atec-server, so the web page points to a real directory
#
$DEST_SERVER = "4dwx\@atec-server.rap.ucar.edu";
$DEST_SERVER_STEREO = "fddasys\@4dwx-stereo-atec";
$USER_DIR = "/$RANGE/${USER}";
$JOB_LOC = "/$RANGE/${USER}/${JOB_ID}";
$JOB_LOC_STEREO = "/d1/V5D/${JOB_ID}";
###CONUS
###   $JOB_LOC = "/GRM/conus";

if ( $DO_OUTPUT ) {
$mkdir_command = "mkdir $USER_DIR";
system( "ssh 4dwx\@128.117.200.214 $mkdir_command" );
$mkdir_command = "mkdir $JOB_LOC";
system( "ssh 4dwx\@128.117.200.214 $mkdir_command" );

$JOB_DEST = "$DEST_SERVER:$JOB_LOC";
&do_html( $USER, $JOB_ID, $NDOMAINS, $JOB_DEST);
}
# End if this script is doing the primary output... or just the ensemble medoc...
#
# Directory for NAPS files
#
if ( $DO_NAPS_3 ) {
   $NAPS_DIR = "$RUNDIR/naps${DOMAIN}";
   $NAPS_DEST_LOC = "$JOB_LOC/naps/mmoutput";
   $NAPS_DEST_DIR = "$DEST_SERVER:$NAPS_DEST_LOC/";
   $mkdir_command = "mkdir $JOB_LOC/naps";
   system( "ssh $DEST_SERVER $mkdir_command" );
   $mkdir_command = "mkdir $JOB_LOC/naps/mmoutput";
   system( "ssh $DEST_SERVER $mkdir_command" );
}
if ( $DO_NAPS_2 ) {
   $NAPS_DIR = "$RUNDIR/naps${DOMAIN}";
   $NAPS_DEST_LOC = "$JOB_LOC/naps/mmoutput";
   $NAPS_DEST_DIR = "$DEST_SERVER:$NAPS_DEST_LOC/";
   $mkdir_command = "mkdir $JOB_LOC/naps";
   system( "ssh $DEST_SERVER $mkdir_command" );
   $mkdir_command = "mkdir $JOB_LOC/naps/mmoutput";
   system( "ssh $DEST_SERVER $mkdir_command" );
}
#
# Directory for NAPS default case files
#
if ( $DO_NAPS_DEFAULT_3 ) {
   $NAPS_DIR = "$RUNDIR/to_naps3";
   $NAPS_DEST_LOC = "$JOB_LOC/naps/default";
   $NAPS_DEST_DIR = "$DEST_SERVER:$NAPS_DEST_LOC/";
   $mkdir_command = "mkdir $JOB_LOC/naps";
   system( "ssh $DEST_SERVER $mkdir_command" );
   $mkdir_command = "mkdir $JOB_LOC/naps/default";
   system( "ssh $DEST_SERVER $mkdir_command" );
}
#
# Directory for medoc/scipuf files
#
if ( $DO_MEDOC_2 ) {
   $SCIPUF_DIR2 = "$RUNDIR/medoc2";
   $SCIPUF_ARCHIVE_DIR2 = "$RUNDIR/medoc2_archive";
   $SCIPUF_DEST_LOC = "$JOB_LOC/scipuff";
   $SCIPUF_DEST_LOC1 = "$JOB_LOC/scipuff1";
   $SCIPUF_DEST_LOC2 = "$JOB_LOC/scipuff2";
   $SCIPUF_DEST_LOC3 = "$JOB_LOC/scipuff3";
   $SCIPUF_DEST_LOC4 = "$JOB_LOC/scipuff4";
   $SCIPUF_DEST_LOC5 = "$JOB_LOC/scipuff5";
   $SCIPUF_DEST_LOC6 = "$JOB_LOC/scipuff6";
   $SCIPUF_DEST_DIR = "$DEST_SERVER:$SCIPUF_DEST_LOC/";
   $SCIPUF_DEST_DIR1 = "$DEST_SERVER:$SCIPUF_DEST_LOC1/";
   $SCIPUF_DEST_DIR2 = "$DEST_SERVER:$SCIPUF_DEST_LOC2/";
   $SCIPUF_DEST_DIR3 = "$DEST_SERVER:$SCIPUF_DEST_LOC3/";
   $SCIPUF_DEST_DIR4 = "$DEST_SERVER:$SCIPUF_DEST_LOC4/";
   $SCIPUF_DEST_DIR5 = "$DEST_SERVER:$SCIPUF_DEST_LOC5/";
   $SCIPUF_DEST_DIR6 = "$DEST_SERVER:$SCIPUF_DEST_LOC6/";
   $mkdir_command = "mkdir $JOB_LOC/scipuff";
   system( "ssh $DEST_SERVER $mkdir_command" );
}
if ( $DO_MEDOC_3 ) {
   $SCIPUF_DIR3 = "$RUNDIR/medoc3";
   $SCIPUF_ARCHIVE_DIR3 = "$RUNDIR/medoc3_archive";
   $SCIPUF_DEST_LOC3 = "$JOB_LOC/medoc";
   $SCIPUF_DEST_DIR3 = "$DEST_SERVER:$SCIPUF_DEST_LOC3/";
   $mkdir_command = "mkdir $JOB_LOC/medoc";
   system( "ssh $DEST_SERVER $mkdir_command" );
}
if ( $DO_MEDOC_4 ) {
   $SCIPUF_DIR_4 = "$RUNDIR/medoc4";
   $SCIPUF_ARCHIVE_DIR_4 = "$RUNDIR/medoc4_archive";
   $SCIPUF_DEST_LOC4 = "$JOB_LOC/medoc";
   $SCIPUF_DEST_DIR4 = "$DEST_SERVER:$SCIPUF_DEST_LOC4/";
   $mkdir_command = "mkdir $JOB_LOC/medoc";
   system( "ssh $DEST_SERVER $mkdir_command" );
}
#
# Directory for RIP plots/files
#
if ( $DO_RIP ) {
   $PLOTS_DIR = "$RUNDIR/plots";
   $PLOT_DEST_LOC = "$JOB_LOC/gifs";
   $PLOT_DEST_DIR = "$DEST_SERVER:$PLOT_DEST_LOC/";
   $mkdir_command = "mkdir $JOB_LOC/gifs";
   system( "ssh $DEST_SERVER $mkdir_command" );
}
#
# Directory for Stereo Vis files -- different destination host also
#
if ( $DO_STEREO ) {
   $STEREO_DIR = "$RUNDIR/stereo";
   $STEREO_DEST_LOC = "$JOB_LOC_STEREO";
   $STEREO_DEST_DIR = "$DEST_SERVER_STEREO:$STEREO_DEST_LOC/";
   $mkdir_command = "mkdir $JOB_LOC_STEREO";
   system( "ssh fddasys\@4dwx-stereo-atec $mkdir_command" );
}
#
# Directory for tabular site fcsts
#
if ( $DO_SITES ) {
   $SITES_DIR = "$RUNDIR/sites";
   $SITES_ARCH_DIR = "$RUNDIR/sites_archive";
   $SITES_DEST_LOC = "$JOB_LOC/fddasites";
   $SITES_DEST_DIR = "$DEST_SERVER:$SITES_DEST_LOC/";
   $mkdir_command = "mkdir $JOB_LOC/fddasites";
   system( "ssh $DEST_SERVER $mkdir_command" );
   $mkdir_command = "mkdir $JOB_LOC/fddasites/archive";
   system( "ssh $DEST_SERVER $mkdir_command" );
   $mkdir_command = "mkdir $JOB_LOC/fddasites/work";
   system( "ssh $DEST_SERVER $mkdir_command" );
}

#
#       Build the UTC date as yy mm dd hh for this cycle
$ttime = time - 0 *3600;
($sec,$mm,$hh,$dd,$mm,$yy,@_) = gmtime($ttime);

if ($yy<50){
  $yy+=2000;
} else {
  $yy+=1900;
}

$THIS_CYCLE =      sprintf("%04d%02d%02d%02d",$yy,$mm+1,$dd,$hh) if(! $THIS_CYCLE);

#
# Critical time file
#
$CRITICAL_TIME_FILE = "$RUNDIR/critic.time";

open(CRITIC, $CRITICAL_TIME_FILE); $time_max = <CRITIC>; close(CRITIC);
chomp($time_max);

#
# Executable files
#
$MM5SPLIT = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/splitv3.exe";

#
# Scrub the working directory to 1-day...
#
system("/home/fddasys/bin/scrub .5 $WORK_DIR");

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                      M A I N                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

&main;

exit(0);



#
#
#
sub main
{
    # cd to a working directory which will have all of the hourly files in it...
    chdir "$WORK_DIR";
    # Construct the file names
    ($FOUT_SUFFIX, $POUT_SUFFIX) = get_file_suffix();

    # Get the domain
    $this_domain = $DOMAIN;

    # Process the final analysis cycle
    for( $time_step = 0; $time_step <= $FINAL_TIME_STEPS; $time_step++ )
    {
       $FINAL_FILE = "${RUNDIR}/${THIS_CYCLE}/MM5_F/MMOUT_DOMAIN${this_domain}${FOUT_SUFFIX}";

        next if(-e "${RUNDIR}/${THIS_CYCLE}/MM5_P");
	# Wait for the file size to change (indicating that there was a new output)
	&debug($DEBUG, "Waiting for file ${FINAL_FILE}\n");
        ($file_ok) = &wait_for_file(${FINAL_FILE}, ${SLEEP_TIME});

        if ( $file_ok == 0 ) {
        foreach $d (@domains) {
           $MM5_FILE = "${RUNDIR}/${THIS_CYCLE}/MM5_F/MMOUT_DOMAIN$d${FOUT_SUFFIX}";
           if ( $HOURLY_OUTPUT_FLAG ) {
 	      # Name the latest hourly file with valid time 
              $MM5_FILE = "${RUNDIR}/${THIS_CYCLE}/MM5_F/MMOUT_DOMAIN$d${FOUT_SUFFIX}";
              &debug($DEBUG, "Get valid time from ${MM5_FILE}\n");
              $HOURLY_FILE = &mm5name($WORK_DIR, $MM5_FILE);
           } else {
              # Splitting MM5 output into hourly files
              &debug($DEBUG, "Spliting ${MM5_FILE} into hourly files\n");
              $HOURLY_FILE = &mm5split($WORK_DIR, $MM5_FILE);
           }
           if ( $d == 1 ) { $FILED1 = $HOURLY_FILE; }
           if ( $d == 2 ) { $FILED2 = $HOURLY_FILE; }
           if ( $d == 3 ) { $FILED3 = $HOURLY_FILE; }
           if ( $d == 4 ) { $FILED4 = $HOURLY_FILE; }

           if ( $DO_TILES != 0 ) {
              $FILED21 = "MMOUTPUT_D21";
              $FILED22 = "MMOUTPUT_D22";
              $FILED23 = "MMOUTPUT_D23";
              $FILED24 = "MMOUTPUT_D24";
              $FILED25 = "MMOUTPUT_D25";
              $FILED26 = "MMOUTPUT_D26";
              $fort10 = $WORK_DIR . "/" .$FILED2 ;
              system("rm fort.10");
              system("ln -s  $fort10 fort.10");
              system("$EXECUTABLE_ARCHIVE/tile1.exe ");
              system("mv fort.45 $FILED21");
              system("$EXECUTABLE_ARCHIVE/tile2.exe ");
              system("mv fort.45 $FILED22");
              system("$EXECUTABLE_ARCHIVE/tile3.exe ");
              system("mv fort.45 $FILED23");
              system("$EXECUTABLE_ARCHIVE/tile4.exe ");
              system("mv fort.45 $FILED24");
              system("$EXECUTABLE_ARCHIVE/tile5.exe ");
              system("mv fort.45 $FILED25");
              system("$EXECUTABLE_ARCHIVE/tile6.exe ");
              system("mv fort.45 $FILED26");
           }

        }

        $valid_time = substr( $FILED2, 0, 12);
        system("cp ${RUNDIR}/${THIS_CYCLE}/*qc_obs_for_assimilation_s $WORK_DIR");

        if ( $DO_RIP ) {
#          Run the RIP script to generate graphics
           &do_plots( $WORK_DIR . "/" . $FILED2, $NDOMAINS, $THIS_CYCLE, $valid_time, $PLOT_DEST_DIR);
        }
        if ( $DO_SITES ) {
#          Run the tabular site forecast script
           &do_sites( $WORK_DIR . "/" . $FILED2, $NDOMAINS, $THIS_CYCLE, $SITES_DEST_DIR);
        }
#        if ( $DO_MEDOC_2 ) {
#          Run the scipuf converter to generate medoc files
#           &do_scipuf( $WORK_DIR . "/" . $FILED2, 1, 0, $SCIPUF_DEST_LOC2);
#        }
        if ( $DO_TILES != 0 ) {
           $SCIPUF_DIR = $SCIPUF_DIR2;
           $SCIPUF_ARCHIVE_DIR = $SCIPUF_ARCHIVE_DIR2;
           &do_scipuf( $WORK_DIR . "/" .$FILED21, 1, 0, $SCIPUF_DEST_LOC1);
           &do_scipuf( $WORK_DIR . "/" .$FILED22, 1, 0, $SCIPUF_DEST_LOC2);
           &do_scipuf( $WORK_DIR . "/" .$FILED23, 1, 0, $SCIPUF_DEST_LOC3);
           &do_scipuf( $WORK_DIR . "/" .$FILED24, 1, 0, $SCIPUF_DEST_LOC4);
           &do_scipuf( $WORK_DIR . "/" .$FILED25, 1, 0, $SCIPUF_DEST_LOC5);
           &do_scipuf( $WORK_DIR . "/" .$FILED26, 1, 0, $SCIPUF_DEST_LOC6);
        }
        if ( $DO_MEDOC_3 ) {
#          Run the scipuf converter to generate medoc files
           $SCIPUF_DIR = $SCIPUF_DIR3;
           $SCIPUF_ARCHIVE_DIR = $SCIPUF_ARCHIVE_DIR3;
           &do_scipuf( $WORK_DIR . "/" . $FILED3, 1, 0, $SCIPUF_DEST_LOC3);
        }
        if ( $DO_MEDOC_4 ) {
#          Run the scipuf converter to generate medoc files
           $SCIPUF_DIR = $SCIPUF_DIR4;
           $SCIPUF_ARCHIVE_DIR = $SCIPUF_ARCHIVE_DIR4;
           &do_scipuf( $WORK_DIR . "/" . $FILED4, 1, 0, $SCIPUF_DEST_LOC3);
        }
        if ( $DO_NAPS_2 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_naps( $WORK_DIR . "/" . $FILED2, $NAPS_DEST_DIR);
        }
        if ( $DO_NAPS_3 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_naps( $WORK_DIR . "/" . $FILED3, $NAPS_DEST_DIR);
        }
        if ( $DO_STEREO ) {
#          Create the stereo vis5d files
           foreach $d (1..$NDOMAINS) {
              if ( $d == 1 ) { $FN = $FILED1 }
              if ( $d == 2 ) { $FN = $FILED2 }
              if ( $d == 3 ) { $FN = $FILED3 }
              if ( $d == 4 ) { $FN = $FILED4 }
              &do_stereo_vis( $WORK_DIR . "/" . $FN, 1, $STEREO_DEST_DIR);
           }
        }
## End if the output file exists
        }
        if ( $HOURLY_OUTPUT_FLAG ) { 
           ($FOUT_SUFFIX) = increment_file_suffix($FOUT_SUFFIX);
        }

    }

    if ( $DO_VERIF ) {
#      Run the verif stats on the final analysis
#
       print ( "calling verif script -- $THIS_CYCLE \n");
       &veri_rtfdda3hcyc( $THIS_CYCLE , "CONUS" );
    }

    # Process the preliminary & forecast cycle
    $j = 0; 
    for( $time_step = 0; $time_step < $PRELIM_TIME_STEPS; $time_step++ )
###    while( 1 && $j < $MAX_ITER)
    {
       $PRELIM_FILE = "${RUNDIR}/${THIS_CYCLE}/MM5_P/MMOUT_DOMAIN${this_domain}${POUT_SUFFIX}";
	# Count the number of output times (the first three are preliminary analysis
	$j++; 

        # Wait for the file size to change (indicating that there was a new output)
        &debug($DEBUG, "Waiting for file ${PRELIM_FILE}\n");
        ($file_ok) = &wait_for_file(${PRELIM_FILE}, ${SLEEP_TIME});

        if ( $file_ok == 0 ) {
        foreach $d (1..$NDOMAINS) {
           $MM5_FILE = "${RUNDIR}/${THIS_CYCLE}/MM5_P/MMOUT_DOMAIN$d${POUT_SUFFIX}";
           if ( $HOURLY_OUTPUT_FLAG ) {
              # Name the latest hourly file with valid time 
              $MM5_FILE = "${RUNDIR}/${THIS_CYCLE}/MM5_P/MMOUT_DOMAIN$d${POUT_SUFFIX}";
              &debug($DEBUG, "Get valid time from ${MM5_FILE}\n");
              $HOURLY_FILE = &mm5name($WORK_DIR, $MM5_FILE);
           } else {
              # Splitting MM5 output into hourly files
              &debug($DEBUG, "Spliting ${MM5_FILE} into hourly files\n");
              $HOURLY_FILE = &mm5split($WORK_DIR, $MM5_FILE);

           }
           if ( $d == 1 ) { $FILED1 = $HOURLY_FILE; }
           if ( $d == 2 ) { $FILED2 = $HOURLY_FILE; }
           if ( $d == 3 ) { $FILED3 = $HOURLY_FILE; }
           if ( $d == 4 ) { $FILED4 = $HOURLY_FILE; }

           if ( $DO_TILES ) {
              $FILED21 = "MMOUTPUT_D21";
              $FILED22 = "MMOUTPUT_D22";
              $FILED23 = "MMOUTPUT_D23";
              $FILED24 = "MMOUTPUT_D24";
              $FILED25 = "MMOUTPUT_D25";
              $FILED26 = "MMOUTPUT_D26";
              system("rm fort.10");
              system("ln -s $FILED2 fort.10");
              system("$EXECUTABLE_ARCHIVE/tile1.exe ");
              system("mv fort.45 $FILED21");
              system("$EXECUTABLE_ARCHIVE/tile2.exe ");
              system("mv fort.45 $FILED22");
              system("$EXECUTABLE_ARCHIVE/tile3.exe ");
              system("mv fort.45 $FILED23");
              system("$EXECUTABLE_ARCHIVE/tile4.exe ");
              system("mv fort.45 $FILED24");
              system("$EXECUTABLE_ARCHIVE/tile5.exe ");
              system("mv fort.45 $FILED25");
              system("$EXECUTABLE_ARCHIVE/tile6.exe ");
              system("mv fort.45 $FILED26");
           }

        }

        $valid_time = substr( $FILED2, 0, 12);
        system("cp ${RUNDIR}/${THIS_CYCLE}/*qc_obs_for_assimilation_s $WORK_DIR");
        if ( $DO_RIP ) {
#          Run the RIP script to generate graphics
           &do_plots( $WORK_DIR . "/" . $FILED2, $NDOMAINS, $THIS_CYCLE, $valid_time, $PLOT_DEST_DIR);
        }
        if ( $DO_SITES ) {
#          Run the tabular site forecast script
           &do_sites( $WORK_DIR . "/" . $FILED2, $NDOMAINS, $THIS_CYCLE, $SITES_DEST_DIR);
        }
#        if ( $DO_MEDOC_2 ) {
#          Run the scipuf converter to generate medoc files
           $SCIPUF_DIR = $SCIPUF_DIR2;
           $SCIPUF_ARCHIVE_DIR = $SCIPUF_ARCHIVE_DIR2;
#           &do_scipuf( $WORK_DIR . "/" .$FILED2, 0, 0, $SCIPUF_DEST_LOC);
#        }
        if ( $DO_TILES ) {
           $SCIPUF_DIR = $SCIPUF_DIR2;
           $SCIPUF_ARCHIVE_DIR = $SCIPUF_ARCHIVE_DIR2;
           &do_scipuf( $WORK_DIR . "/" .$FILED21, 0, 0, $SCIPUF_DEST_LOC1);
           &do_scipuf( $WORK_DIR . "/" .$FILED22, 0, 0, $SCIPUF_DEST_LOC2);
           &do_scipuf( $WORK_DIR . "/" .$FILED23, 0, 0, $SCIPUF_DEST_LOC3);
           &do_scipuf( $WORK_DIR . "/" .$FILED24, 0, 0, $SCIPUF_DEST_LOC4);
           &do_scipuf( $WORK_DIR . "/" .$FILED25, 0, 0, $SCIPUF_DEST_LOC5);
           &do_scipuf( $WORK_DIR . "/" .$FILED26, 0, 0, $SCIPUF_DEST_LOC6);
        }
        if ( $DO_MEDOC_3 ) {
#          Run the scipuf converter to generate medoc files
           $SCIPUF_DIR = $SCIPUF_DIR3;
           $SCIPUF_ARCHIVE_DIR = $SCIPUF_ARCHIVE_DIR3;
           &do_scipuf( $WORK_DIR . "/" .$FILED3, 0, 0, $SCIPUF_DEST_LOC3);
        }
        if ( $DO_MEDOC_4 ) {
#          Run the scipuf converter to generate medoc files
           $SCIPUF_DIR = $SCIPUF_DIR4;
           $SCIPUF_ARCHIVE_DIR = $SCIPUF_ARCHIVE_DIR4;
           &do_scipuf( $WORK_DIR . "/" .$FILED4, 0, 0, $SCIPUF_DEST_LOC4);
        }
        if ( $DO_NAPS_3 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_naps( $WORK_DIR . "/" . $FILED3, $NAPS_DEST_DIR);
        }
        if ( $DO_STEREO ) {
#          Create the stereo vis5d files
           foreach $d (1..$NDOMAINS) {
              if ( $d == 1 ) { $FN = $FILED1 }
              if ( $d == 2 ) { $FN = $FILED2 }
              if ( $d == 3 ) { $FN = $FILED3 }
              if ( $d == 4 ) { $FN = $FILED4 }
              &do_stereo_vis( $WORK_DIR . "/" . $FN, 0, $STEREO_DEST_DIR);
           }
        }
# End if the file exists
        }
        if ( $HOURLY_OUTPUT_FLAG ) {
           ($POUT_SUFFIX) = increment_file_suffix($POUT_SUFFIX);
        }


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
###TEST    $start_size   = &get_file_size($filename);
    $start_size   = 0;
    $current_size = &get_file_size($filename);
    $j = 0;

    # Sleep while the file size has not changed
    while( $start_size == $current_size && $j < 72)
    {
	sleep($sleep);
        $j++;
	$current_size = &get_file_size($filename);
	&debug($DEBUG, "    Checking file $filename\n");
    }
    
    # File is removed when this cycle is finished. So, this program exit. 
    exit if($current_size < $start_size);

    # Sleep for an additional 20 seconds to make sure the output is finished writing
    sleep(60);

    $current_size = &get_file_size($filename);
    if ( $current_size > 0 ) {
    &debug($DEBUG, "About to process MM5 out file with size $current_size\n");
    return 0 ;
    } else {
    return 1 ;
    }
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
# Return the file name with the latest date since we only want to convert the last time period
#
# @args - 0 - Directory to look in
#
sub get_hourly_file_name
{
    local($dir) = $_[0];
    my $file;

    $file=`ls -1 $dir | tail -1`;

    return $file;
}



#
# Split this MM5 file into hourly files
#
# @args - 0 - directory
# @args - 1 - MM5 file
#
sub mm5name
{
    local($dir,$file,$filename) = @_;
    my ($fn,$bytes,$domain);
    my ($buf,$year,$month,$day,$hour);

    &debug($DEBUG, "    Find valid time for $file\n");
    chdir $dir;

      open(IN,"$file");

      seek(IN,64,0);        # move the file pointer position to after bhi(12,1)
      $bytes=read(IN,$buf,4);   # this is bhi(13,1)
      $domain=unpack "N",$buf;

      seek(IN,117684,0);
      $bytes=read(IN,$buf,24); # this is the 24-character date/time string
      close(IN);

      $buf=~ /^(\d+)\D+(\d+)\D+(\d+)\D+(\d+)\D+(\d+)/;
      $year=$1;
      $month=$2;
      $day=$3;
      $hour=$4;
      $min=$5;
 
# Round to nearest 5 minutes to account for non-zero seconds in the time-stamp
      $r5 = $min%5;
      $n5 = $min - $r5;
      if ($r5 > 2 ) {
       $min5 = $n5+5;
      } else {
       $min5 = $n5;
      }

      $filename = sprintf("%04d%02d%02d%02d%02d_MMOUTPUT_DOMAIN%s.%s",$year,$month,$day,$hour,$min5,$domain,$RANGE);

      system("cp $file $filename");
      chdir ($WORK_DIR);
      return ($filename);
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

        while($mm > 12)
        {
            $mm -= 12;
            $yy++;
        }
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
        $dd += $mon_days{$mm+0};
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
    
    close(CRITIC);

    chomp($time_max);

    if( ($time_max == 0) || (! $time_max) )
    {
	&debug($DEBUG, "Previous cycle failed and $THIS_CYCLE is not good for cold-start\n");
	exit(0);
    }

    elsif( $time_max == 1 )
    {
	&debug($DEBUG, "Previous cycle failed and $THIS_CYCLE is a cold-start\n");
	$FOUT_SUFFIX = "_000";
	$POUT_SUFFIX = "_001";
#       if ( $HOURLY_OUTPUT_FLAG ) { 
#          foreach $i (1..$FINAL_TIME_STEPS) {
#          ($POUT_SUFFIX) = increment_file_suffix($POUT_SUFFIX);
#          }
#       }
    }

    else
    {
	&debug($DEBUG, "The cycle $THIS_CYCLE is a normal cycle\n");
        $cycle_duration = $CYC_INT * 60.0;
##NOTSURE        $cycle_final_dur = $cycle_duration + $OUT_INT;
##NOTSURE        $cycle_final_dur = $cycle_duration + 60.0;
        $cycle_final_dur = $cycle_duration;
###3hr cycle
	$FOUT_SUFFIX = sprintf( "%003d", ($time_max - $cycle_final_dur) / $cycle_duration + 1 );
###6hr cycle
###	$FOUT_SUFFIX = sprintf( "%003d", ($time_max - 420) / 360 + 1 );
	$POUT_SUFFIX = "_".sprintf( "%003d", ($FOUT_SUFFIX + 1) );
	$FOUT_SUFFIX = "_".$FOUT_SUFFIX;
#       if ( $HOURLY_OUTPUT_FLAG ) { 
#          foreach $i (1..$FINAL_TIME_STEPS) {
#          ($POUT_SUFFIX) = increment_file_suffix($POUT_SUFFIX);
#          }
#       }
    }

    return ($FOUT_SUFFIX, $POUT_SUFFIX);
}

#
# Increment the suffix of the file for the next hour
#
sub increment_file_suffix
{
    $CURRENT_SUFFIX= @_[0];
    {
        $HOUR = substr( $CURRENT_SUFFIX, 1,3);
        $NEW_SUFFIX = "_".sprintf( "%003d", ($HOUR + 1) );
    }

    return ($NEW_SUFFIX);
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
#
# @args - 0 - directory
# @args - 1 - MM5 file
#
sub mm5split
{
    local($dir,$file) = @_;
    my ($fn,$bytes,$domain);
    my ($buf,$year,$month,$day,$hour);
    my $filename;

    &debug($DEBUG, "    Executing: $MM5SPLIT $file\n");
    chdir $dir;
    system("ln -sf $file fort.10");
    system("$MM5SPLIT");

    foreach $fn (<fort.*>) {

      @f=stat $fn;
      if($fn eq 'fort.10' || $f[7] < 120000) {
        unlink $fn;
        next;
      }

      open(IN,"$fn");

      seek(IN,64,0);        # move the file pointer position to after bhi(12,1)
      $bytes=read(IN,$buf,4);   # this is bhi(13,1)
      $domain=unpack "N",$buf;

      seek(IN,117684,0);
      $bytes=read(IN,$buf,24); # this is the 24-character date/time string
      close(IN);

      $buf=~ /^(\d+)\D+(\d+)\D+(\d+)\D+(\d+)\D+(\d+)/;
      $year=$1;
      $month=$2;
      $day=$3;
      $hour=$4;
      $min=$5;

# Round to nearest 5 minutes to account for non-zero seconds in the time-stamp
      $r5 = $min%5;
      $n5 = $min - $r5;
      if ($r5 > 2 ) {
       $min5 = $n5+5;
      } else {
       $min5 = $n5;
      }

      $filename = sprintf("%04d%02d%02d%02d%02d_MMOUTPUT_DOMAIN%s.%s",$year,$month,$day,$hour,$min5,$domain,$RANGE);

      rename($fn,$filename);
    }
    chdir "$WORK_DIR";
    return ($filename);
}


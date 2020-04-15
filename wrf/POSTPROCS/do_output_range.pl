#! /usr/bin/perl
use File::Basename;

#
# This script controls the processing of one output-time-step of RTFDDA data
# It is called from rtfdda_postproc.pl
# It assumes that it has a work directory, and that the RTFDDA output file is located there
#
# 1. Call each post-processor, selected by config file
#    RIP graphics
#    MEDOC (domains 3&4)
#    VERIF stats (final and fcst)
#    NAPS (output files for interactive naps)
#    MDV (output files for JVIZ/etc)
#    NAPS (default case -- at the end of the cycle)
#
##
## This is a range-specific version, several variables are "fixed" for
## the range case, rather than GMOD!
##
# Command line args required:
#  $RANGE  
#  $NODE_MEM (0 for MPP/non-ensemble runs)
#  $JOB_ID 
#  $THIS_CYCLE

# Command line args 
if ( $#ARGV >= 2 ) {
  $RUNDIR =  @ARGV[0];
  $JOB_ID  =  @ARGV[1];
  $THIS_CYCLE =  @ARGV[2];
  $WORK_DIR = @ARGV[3];
  $SUFFIX = @ARGV[4];
  if ( $SUFFIX eq "ZZ" ) {
    $SUFFIX = "";
  }
  $CYCLE_TAG   = @ARGV[5];
  $NDOMAINS = @ARGV[6];
  $RANGE = @ARGV[7];
  $lrange = $RANGE;$lrange =~ tr/A-Z/a-z/;
  print ( " $0  $RUNDIR  $JOB_ID $THIS_CYCLE $WORK_DIR $SUFFIX $CYCLE_TAG $NDOMAINS $RANGE\n");
} else {
  print ( " Usage:   $0  RUNDIR JOB_ID THIS_CYCLE WORK_DIR SUFFIX CYCLE_TAG NDOMAINS RANGE\n");
  exit (1);
}

chdir $WORK_DIR;
# code and run-time directories
$MM5HOME = "/raid/fddahome";
$POSTPROCS_DIR = "$MM5HOME/cycle_code/POSTPROCS";
$MM5SPLIT = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/splitv3.exe";
#
# Read these from the configuration file
#
require "/raid/GMODJOBS/$JOB_ID/flexinput.pl";
if ( -e  "/raid/GMODJOBS/$JOB_ID/postprocinput.pl" ) {
  require "/raid/GMODJOBS/$JOB_ID/postprocinput.pl";
} else {
   $USER = "ncaruser";
# Will MM5 generate a new file for each output time step (i.e. each hour)?
   $HOURLY_OUTPUT_FLAG = 0;
#
# Which post processors to run
   $DO_MDV_1 = 1 ;
   $DO_MDV_2 = 1 ;
   $DO_MDV_3 = 0 ;
   $DO_MDV_4 = 1 ;
   $DO_GEM_1 = 1 ;
   $DO_GEM_2 = 1 ;
   $DO_GEM_3 = 1 ;
   $DO_GEM_4 = 1 ;
   $DO_NAPS_3 = 1 ;
   $DO_NAPS_DEFAULT = 0 ;
   $DO_RIP = 1 ;
   $DO_PLOTS_SMALL = 0 ;
   $DO_SITES = 1 ;
   $DO_MEDOC_1 = 0 ;
   $DO_MEDOC_2 = 0 ;
   $DO_MEDOC_3 = 1 ;
   $DO_MEDOC_4 = 1 ;
   $DO_VERIF = 0 ;
   $DO_STEREO = 0 ;
   $DO_TILES = 0 ;
   $MDV_DEST_HOST = "4dwx\@atec-ingest";
   $MDV_DEST_ROOT = "/raid/mm5/raw";
   $DEST_SERVER = "4dwxdata\@fourdwx1";
   $KEY = "/home/fddasys/.ssh/rtfdda-atec";
   $DEST_SERVER_STEREO = "fddasys\@4dwx-stereo-atec";
}
# some WRF settings
if ( $IS_WRF ) {
   if ( $IS_COLD_START_SYSTEM ) {
#    For cold-start systems...
    $model_fin_dir = "WRFRUN";
    $model_pre_dir = "WRFRUN";
  } else {
#    For RTFDDA cyclcing...
    $model_fin_dir = "WRF_F";
    $model_pre_dir = "WRF_P";
  }
  $model_out_name = "wrfout_d0";
} else {
  $model_fin_dir = "MM5_F";
  $model_pre_dir = "MM5_P";
  $model_out_name = "MMOUT_DOMAIN";
}
if ( $CYCLE_TAG eq "final" ) {
 $MM5_DIR = $model_fin_dir;
} else {
 $MM5_DIR = $model_pre_dir;
}

# Copy the file to the working dir to get a snapshot "now"
      foreach $d (1..$NDOMAINS) {
	 $MM5_FILE = "${RUNDIR}/${THIS_CYCLE}/${MM5_DIR}/${model_out_name}$d${SUFFIX}";
	 if ( -e $MM5_FILE ) {
         if ( $HOURLY_OUTPUT_FLAG ) {
            # Name the latest hourly file with valid time
	    $MM5_FILE = "${RUNDIR}/${THIS_CYCLE}/${MM5_DIR}/${model_out_name}$d${SUFFIX}";
	    if ( $IS_WRF ) {
               $HOURLY_FILE = basename $MM5_FILE;
               system("cp $MM5_FILE $RUNDIR/postprocs");
               if ( $d == 1 ) { $OUT_FILED1 = basename $MM5_FILE; }
               if ( $d == 2 ) { $OUT_FILED2 = basename $MM5_FILE; }
               if ( $d == 3 ) { $OUT_FILED3 = basename $MM5_FILE; }
               if ( $d == 4 ) { $OUT_FILED4 = basename $MM5_FILE; }
            } else {
               &debug($DEBUG, "Get valid time from ${MM5_FILE}\n");
               $HOURLY_FILE = &mm5name($WORK_DIR, $MM5_FILE);
            }
         } 
         $WORK_FILE = "${model_out_name}$d";
         system("cp $MM5_FILE $WORK_FILE");
	 if ( $IS_WRF ) {
           $valid_time = &time_from_wrf_name($HOURLY_FILE);
           $valid_time_m1 = hh_advan_date($valid_time, -1);
	   $yr = substr( $valid_time_m1, 0, 4);
	   $mo = substr( $valid_time_m1, 4, 2);
	   $dy = substr( $valid_time_m1, 6, 2);
	   $hr = substr( $valid_time_m1, 8, 2);
	   $WRF_FILE_M1 = "wrfout_d0${d}_${yr}-${mo}-${dy}_${hr}:00:00";
           $WORK_FILE_M1 = "${model_out_name}$d-1";
           # Check in the "working directory" first...
	   $HOURLY_FILE_M1 = "${RUNDIR}/${THIS_CYCLE}/${MM5_DIR}/${WRF_FILE_M1}";
           if ( -e $HOURLY_FILE_M1 ) {
             system("cp $HOURLY_FILE_M1  $WORK_FILE_M1");
           } else {
	     $HOURLY_FILE_M1 = "${RUNDIR}/postprocs/${WRF_FILE_M1}";
             system("cp $HOURLY_FILE_M1  $WORK_FILE_M1");
           }
         }
         }
      }
      foreach $d (1..$NDOMAINS) {
         $MM5_FILE = "${model_out_name}$d";
	 if ( -e $MM5_FILE ) {
         if ( $HOURLY_OUTPUT_FLAG ) {
            # Name the latest hourly file with valid time
##            $MM5_FILE = "${RUNDIR}/${THIS_CYCLE}/${MM5_DIR}/MMOUT_DOMAIN$d${SUFFIX}";
	    if ( $IS_WRF ) {
               $HOURLY_FILE = basename $MM5_FILE;
            } else {
               &debug($DEBUG, "Get valid time from ${MM5_FILE}\n");
               $HOURLY_FILE = &mm5name($WORK_DIR, $MM5_FILE);
             }
         } else {
            # Splitting MM5 output into hourly files
            &debug($DEBUG, "Spliting ${MM5_FILE} into hourly files\n");
            $HOURLY_FILE = &mm5split($WORK_DIR, $MM5_FILE);

         }
	 if ( $IS_WRF ) {
	 } else  {
           $valid_time = substr( $HOURLY_FILE, 0, 12);
           $valid_time_m1 = hh_advan_date($valid_time, -1);
           $HOURLY_FILE_M1 = "$RUNDIR/postprocs/${valid_time_m1}00_MMOUTPUT_DOMAIN$d.$RANGE";
           $OUTPUT_FILE = "MMOUT." . $d;
           system("cp $HOURLY_FILE $POSTPROCS_SAV_DIR/postprocs");
           if ( -e $HOURLY_FILE_M1  && $SUFFIX ne "_000" ) {
             system("cat $HOURLY_FILE_M1 $HOURLY_FILE > $OUTPUT_FILE");
           } else {
             system("mv $HOURLY_FILE $OUTPUT_FILE");
           }
           system("rm [0-9]*MMOUTPUT_DOMAIN$d.$RANGE");
           system("mv $OUTPUT_FILE $HOURLY_FILE");
           system("rm MMOUT_DOMAIN$d");
         }

         if ( $d == 2 ) {$POST_FILE = $HOURLY_FILE}
         if ( $d == 1 ) { $FILED1 = $HOURLY_FILE; }
         if ( $d == 2 ) { $FILED2 = $HOURLY_FILE; }
         if ( $d == 3 ) { $FILED3 = $HOURLY_FILE; }
         if ( $d == 4 ) { $FILED4 = $HOURLY_FILE; }

         }
      }

#
# Subroutines for each post-processor
require "$POSTPROCS_DIR/do_naps.pl";
require "$POSTPROCS_DIR/do_mdv.pl";
require "$POSTPROCS_DIR/do_gempak.pl";
require "$POSTPROCS_DIR/do_scipuf.pl";
require "$POSTPROCS_DIR/do_sites.pl";
require "$POSTPROCS_DIR/do_plots+obs.pl";
require "$POSTPROCS_DIR/do_grid_plots.pl";
if ( -e "$POSTPROCS_DIR/conv2gif_$RANGE.pl" ) {
 require "$POSTPROCS_DIR/conv2gif_$RANGE.pl";
} else {
 require "$POSTPROCS_DIR/conv2gif.pl";
}
require "$POSTPROCS_DIR/conv2gif_grid.pl";
require "$POSTPROCS_DIR/conv2gif_obs.pl";
require "$POSTPROCS_DIR/veri_rtfdda3hcyc.pl";
require "$POSTPROCS_DIR/do_stereo_vis.pl";
require "$POSTPROCS_DIR/do_html.pl";
require "$POSTPROCS_DIR/do_plots_ncl.pl";

$IS_GEAPS=0;

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
# cycle control constants
$FINAL_TIME_STEPS = $CYC_INT*60.0 / $OUT_INT + $FIN_END + 1;
$PRELIM_TIME_STEPS = $FCST_LENGTH*60.0 / $OUT_INT ;
$NDOMAINS = $NUM_DOMS;
$MAX_ITER=120;
#
# Set an array of domain names -- set the length of the array to the
# actual number of domains for this case
@domains = ('1', '2', '3','4' );
$#domains = $NDOMAINS;	
# Domain to use to check for new output files
$CHECK_DOMAIN = 2;

# get environment vars 
$ENV{'MM5HOST'} = $MM5HOST;
$ENV{'MM5HOME'} = $MM5HOME;
$ENV{'RUNDIR'} = $RUNDIR;
$ENV{'DATADIR'} = $DATADIR;
$ENV{'DATA_DIR'} = $DATADIR;

$CSH_ARCHIVE = $MM5HOME.'/cycle_code/CSH_ARCHIVE';
$EXECUTABLE_ARCHIVE = $MM5HOME.'/cycle_code/EXECUTABLE_ARCHIVE';
$MustHaveDir = $EXECUTABLE_ARCHIVE."/MustHaveDir";
#
# Make the working directory
system("$MustHaveDir $WORK_DIR");

# Make some directories on atec-server, so the web page points to a real directory
#
$USER_DIR = "/www/htdocs/images/rtfdda";
$JOB_LOC = "/www/htdocs/images/rtfdda";

$JOB_DEST = "$DEST_SERVER:$JOB_LOC";
#
# Directory for NAPS files
#
if ( $DO_NAPS_3 ) {
   $NAPS_DIR = "$RUNDIR/naps${DOMAIN}";
   $NAPS_CYCLE_LOC = "$JOB_LOC/naps/metadata";
   if ( $IS_WRF ) {
     $NAPS_DEST_LOC = "$JOB_LOC/naps/wrf";
   } else {
     $NAPS_DEST_LOC = "$JOB_LOC/naps/mmoutput";
   }
   $NAPS_DEST_DIR3 = "$DEST_SERVER:$NAPS_DEST_LOC/";
   $NAPS_CYCLE_DIR = "$DEST_SERVER:$NAPS_CYCLE_LOC/";
}
if ( $DO_NAPS_4 ) {
   $NAPS_DIR = "$RUNDIR/naps${DOMAIN}";
   $NAPS_CYCLE_LOC = "$JOB_LOC/naps/metadata";
   if ( $IS_WRF ) {
     $NAPS_DEST_LOC = "$JOB_LOC/naps/wrf4";
   } else {
     $NAPS_DEST_LOC = "$JOB_LOC/naps/mmoutput4";
   }
   $NAPS_DEST_DIR4 = "$DEST_SERVER:$NAPS_DEST_LOC/";
   $NAPS_CYCLE_DIR = "$DEST_SERVER:$NAPS_CYCLE_LOC/";
}
if ( $DO_NAPS_2 ) {
   $NAPS_DIR = "$RUNDIR/naps${DOMAIN}";
   $NAPS_CYCLE_LOC = "$JOB_LOC/naps/metadata";
   if ( $IS_WRF ) {
     $NAPS_DEST_LOC = "$JOB_LOC/naps/wrf2";
   } else {
     $NAPS_DEST_LOC = "$JOB_LOC/naps/mmoutput2";
   } 
   $NAPS_DEST_DIR2 = "$DEST_SERVER:$NAPS_DEST_LOC/";
   $NAPS_CYCLE_DIR = "$DEST_SERVER:$NAPS_CYCLE_LOC/";
}
#
# Directory for NAPS default case files
#
if ( $DO_NAPS_DEFAULT_3 ) {
   $NAPS_DIR = "$RUNDIR/to_naps3";
   $NAPS_DEST_LOC = "$JOB_LOC/naps/default";
   $NAPS_DEST_DIR = "$DEST_SERVER:$NAPS_DEST_LOC/";
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
}
if ( $DO_MEDOC_3 ) {
   $SCIPUF_DIR3 = "$RUNDIR/medoc3";
   $SCIPUF_ARCHIVE_DIR3 = "$RUNDIR/medoc3_archive";
   $SCIPUF_DEST_LOC3 = "$JOB_LOC/medoc";
   $SCIPUF_DEST_DIR3 = "$DEST_SERVER:$SCIPUF_DEST_LOC3/";
}
if ( $DO_MEDOC_4 ) {
   $SCIPUF_DIR4 = "$RUNDIR/medoc4";
   $SCIPUF_ARCHIVE_DIR4 = "$RUNDIR/medoc4_archive";
   $SCIPUF_DEST_LOC4 = "$JOB_LOC/medoc";
   $SCIPUF_DEST_DIR4 = "$DEST_SERVER:$SCIPUF_DEST_LOC4/";
}
#
# Directory for RIP plots/files
#
if ( $DO_RIP ) {
   $PLOTS_DIR = "$WORK_DIR/plots";
   system("$MustHaveDir $PLOTS_DIR");
   $PLOT_DEST_LOC = "$JOB_LOC/gifs";
   $PLOT_DEST_DIR = "$DEST_SERVER:$PLOT_DEST_LOC/";
   $PLOT_SMALL_LOC = "$JOB_LOC/gifs_small";
   $PLOT_SMALL_DIR = "$DEST_SERVER:$PLOT_SMALL_LOC/";
}
#
# Directory for Stereo Vis files -- different destination host also
#
if ( $DO_STEREO ) {
   $STEREO_DIR = "$RUNDIR/stereo";
   $STEREO_DEST_LOC = "$JOB_LOC_STEREO";
   $STEREO_DEST_DIR = "$DEST_SERVER_STEREO:$STEREO_DEST_LOC/";
}
#
# Directory for tabular site fcsts
#
if ( $DO_SITES ) {
   $SITES_DIR = "$RUNDIR/sites";
   $SITES_ARCHIVE_DIR = "$RUNDIR/sites_archive";
   $SITES_DEST_LOC = "$JOB_LOC/fddasites/archive";
   $SITES_DEST_DIR = "$DEST_SERVER:$SITES_DEST_LOC/";
}
# 
# Directories for MDV files
if ( $DO_MDV_1 ) {
   $MDV_DIR = "$RUNDIR/naps${DOMAIN}";
   $MDV_DEST_LOC1 = "$MDV_DEST_ROOT/d1";
   $MDV_DEST_DIR1 = "$MDV_DEST_HOST:$MDV_DEST_LOC1/";
}
if ( $DO_MDV_2 ) {
   $MDV_DIR = "$RUNDIR/naps${DOMAIN}";
   $MDV_DEST_LOC2 = "$MDV_DEST_ROOT/d2";
   $MDV_DEST_DIR2 = "$MDV_DEST_HOST:$MDV_DEST_LOC2/";
}
if ( $DO_MDV_3 ) {
   $MDV_DIR = "$RUNDIR/naps${DOMAIN}";
   $MDV_DEST_LOC3 = "$MDV_DEST_ROOT/d3";
   $MDV_DEST_DIR3 = "$MDV_DEST_HOST:$MDV_DEST_LOC3/";
}
if ( $DO_MDV_4 ) {
   $MDV_DIR = "$RUNDIR/naps${DOMAIN}";
   $MDV_DEST_LOC4 = "$MDV_DEST_ROOT/d4";
   $MDV_DEST_DIR4 = "$MDV_DEST_HOST:$MDV_DEST_LOC4/";
}
# 
# Directories for GEMPAK files
if ( $DO_GEM_1 ) {
   $GEM_DIR = "$RUNDIR/gempak";
   $GEM_DEST_LOC = "/met_data0/gempak/model";
   $GEM_DEST_DIR = "ldm\@4dwx-metbox-dpg:$GEM_DEST_LOC/";
}
if ( $DO_GEM_2 ) {
   $GEM_DIR = "$RUNDIR/gempak";
   $GEM_DEST_LOC = "/met_data0/gempak/model";
   $GEM_DEST_DIR = "ldm\@4dwx-metbox-dpg:$GEM_DEST_LOC/";
}
if ( $DO_GEM_3 ) {
   $GEM_DIR = "$RUNDIR/gempak";
   $GEM_DEST_LOC = "/met_data0/gempak/model";
   $GEM_DEST_DIR = "ldm\@4dwx-metbox-dpg:$GEM_DEST_LOC/";
}
if ( $DO_GEM_4 ) {
   $GEM_DIR = "$RUNDIR/gempak";
   $GEM_DEST_LOC = "/met_data0/gempak/model";
   $GEM_DEST_DIR = "ldm\@4dwx-metbox-dpg:$GEM_DEST_LOC/";
}
#NCL test begins
if ( $DO_PLOTS_NCL ) {
   $PLOT_NCL_DIR = "$WORK_DIR/plots_ncl";
   system("$MustHaveDir $PLOT_NCL_DIR");
   $PLOT_NCL_DEST_LOC = "$JOB_LOC/gifs";
   $PLOT_NCL_DEST_DIR = "$DEST_SERVER:$PLOT_NCL_DEST_LOC/";
   $PLOT_NCL_SMALL_LOC = "$JOB_LOC/gifs_small";
   $PLOT_NCL_SMALL_DIR = "$DEST_SERVER:$PLOT_SMALL_LOC/";
}
#NCL test ends

#       Build the UTC date as yy mm dd hh for this cycle
$ttime = time - 0 *3600;
($sec,$mm,$hh,$dd,$mm,$yy,@_) = gmtime($ttime);

if ($yy<50){
  $yy+=2000;
} else {
  $yy+=1900;
}

#
# Critical time file
#
$CRITICAL_TIME_FILE = "$RUNDIR/critic.time";
open(CRITIC, $CRITICAL_TIME_FILE); $time_max = <CRITIC>; close(CRITIC);
chomp($time_max);
if ( $CYCLE_TAG eq "final" ) {
  $IS_FINAL = 1;
} else {
  $IS_FINAL = 0;
}

        if ( $DO_RIP ) {
#          Run the RIP script to generate graphics
           &do_plots( $WORK_DIR . "/" . $FILED2, $NDOMAINS, $THIS_CYCLE, $valid_time, $PLOT_DEST_DIR, $PLOT_SMALL_DIR);
        }
        if ( $DO_SITES ) {
#          Run the tabular site forecast script
           &do_sites( $WORK_DIR . "/" . $FILED2, $NDOMAINS, $CYCLE_TAG, $SITES_DEST_DIR);
        }
#        if ( $DO_MEDOC_2 ) {
#          Run the scipuf converter to generate medoc files
#           &do_scipuf( $WORK_DIR . "/" . $FILED2, 1, 1, $SCIPUF_DEST_LOC2);
#        }
        if ( $DO_MEDOC_3 ) {
#          Run the scipuf converter to generate medoc files
           $SCIPUF_DIR = $SCIPUF_DIR3;
           $SCIPUF_ARCHIVE_DIR = $SCIPUF_ARCHIVE_DIR3;
           &do_scipuf( $WORK_DIR . "/" . $FILED3, $IS_FINAL, 1, $SCIPUF_DEST_LOC3);
        }
        if ( $DO_MEDOC_4 ) {
#          Run the scipuf converter to generate medoc files
           $SCIPUF_DIR = $SCIPUF_DIR4;
           $SCIPUF_ARCHIVE_DIR = $SCIPUF_ARCHIVE_DIR4;
           &do_scipuf( $WORK_DIR . "/" . $FILED4, $IS_FINAL, 1, $SCIPUF_DEST_LOC4);
        }
        if ( $DO_NAPS_2 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_naps( $RUNDIR . "/postprocs/" . $OUT_FILED2, $NAPS_DEST_DIR2, $THIS_CYCLE, $NAPS_CYCLE_DIR);
        }
        if ( $DO_NAPS_3 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_naps( $RUNDIR . "/postprocs/" . $OUT_FILED3, $NAPS_DEST_DIR3, $THIS_CYCLE, $NAPS_CYCLE_DIR);
        }
        if ( $DO_NAPS_4 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_naps( $RUNDIR . "/postprocs/" . $OUT_FILED4, $NAPS_DEST_DIR4, $THIS_CYCLE, $NAPS_CYCLE_DIR);
        }
        if ( $DO_MDV_1 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_mdv( $WORK_DIR . "/" . $FILED1, $MDV_DEST_DIR1, $MDV_DEST_HOST, $MDV_DEST_LOC1);
        }
        if ( $DO_MDV_2 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_mdv( $WORK_DIR . "/" . $FILED2, $MDV_DEST_DIR2, $MDV_DEST_HOST, $MDV_DEST_LOC2);
        }
        if ( $DO_MDV_3 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_mdv( $WORK_DIR . "/" . $FILED3, $MDV_DEST_DIR3, $MDV_DEST_HOST, $MDV_DEST_LOC3);
        }
        if ( $DO_MDV_4 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_mdv( $WORK_DIR . "/" . $FILED4, $MDV_DEST_DIR4, $MDV_DEST_HOST, $MDV_DEST_LOC4);
        }
        if ( $DO_GEM_1 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_gempak( $WORK_DIR . "/" . $FILED1, $GEM_DEST_DIR, $valid_time, "1");
        }
        if ( $DO_GEM_2 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_gempak( $WORK_DIR . "/" . $FILED2, $GEM_DEST_DIR, $valid_time, "2");
        }
        if ( $DO_GEM_3 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_gempak( $WORK_DIR . "/" . $FILED3, $GEM_DEST_DIR, $valid_time, "3");
        }
        if ( $DO_GEM_4 ) {
#          Copy the file to the web server for NAPS-interactive
           &do_gempak( $WORK_DIR . "/" . $FILED4, $GEM_DEST_DIR, $valid_time, "4");
        }
#        if ( $DO_STEREO ) {
#          Create the stereo vis5d files
#           foreach $d (1..$NDOMAINS) {
#              if ( $d == 1 ) { $FN = $FILED1 }
#              if ( $d == 2 ) { $FN = $FILED2 }
#              if ( $d == 3 ) { $FN = $FILED3 }
#              if ( $d == 4 ) { $FN = $FILED4 }
#              &do_stereo_vis( $WORK_DIR . "/" . $FN, 1, $STEREO_DEST_DIR);
#           }
#        }
#NCL test begins
        if ( $DO_PLOTS_NCL ) {
#           Run the NCL plots to generate nice graphics
            &do_plots_ncl($WORK_DIR, $FILED2, $NDOMAINS, $THIS_CYCLE, $valid_time, $valid_time_m1, $PLOT_DEST_DIR, $PLOT_SMALL_DIR);
         }
#NCL test ends

system("rm -rf $WORK_DIR/plots");
system("rm -f $WORK_DIR/*MMOUT*");

exit(0);


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#               S U B R O U T I N E S                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

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

      $buf=~ /^(\d+)\D+(\d+)\D+(\d+)\D+(\d+)\D+(\d+)\D+(\d+)/;
      $year=$1;
      $month=$2;
      $day=$3;
      $hour=$4;
      $min=$5;
      $sec=$6;
 
# Round to nearest 5 minutes to account for non-zero seconds in the time-stamp
      $r5 = $min%5;
      $n5 = $min - $r5;
      if ($r5 > 2 ) {
       $min5 = $n5+5;
      } else {
       $min5 = $n5;
      }

      $filename="${year}${month}${day}${hour}${min5}_MMOUTPUT_DOMAIN${domain}.${RANGE}";
      $filename = sprintf("%04d%02d%02d%02d%02d_MMOUTPUT_DOMAIN%s.%s",$year,$month,$day,$hour,$min5,$domain,$RANGE);
      system("cp $file $filename");
      chdir ($WORK_DIR);
      return ($filename);
}

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
##    system("rsh node31 $POSTPROCS_DIR/split.pl $WORK_DIR");
    system("$POSTPROCS_DIR/split.pl $WORK_DIR");

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

      $buf=~ /^(\d+)\D+(\d+)\D+(\d+)\D+(\d+)\D+(\d+)\D+(\d+)/;
      $year=$1;
      $month=$2;
      $day=$3;
      $hour=$4;
      $min=$5;
      $sec=$6;
      if ( $sec > 0 ) {
        $min++;
      } 
# Round to nearest 5 minutes to account for non-zero seconds in the time-stamp
      $r5 = $min%5;
      $n5 = $min - $r5;
      if ($r5 > 2 ) {
       $min5 = $n5+5;
      } else {
       $min5 = $n5;
      }


      $filename="${year}${month}${day}${hour}${min5}_MMOUTPUT_DOMAIN${domain}.${RANGE}";
      $filename = sprintf("%04d%02d%02d%02d%02d_MMOUTPUT_DOMAIN%s.%s",$year,$month,$day,$hour,$min5,$domain,$RANGE);
      rename($fn,$filename);
    }
    chdir "$WORK_DIR";
    return ($filename);
}

#
# Determine the suffix of the file to look at
#
sub get_wrf_file_suffix
{
    open(CRITIC, $CRITICAL_TIME_FILE);

    $time_max = <CRITIC>;
    
    close(CRITIC);

    chomp($time_max);

# set this for now
    $time_max = 1 if ( $COLD_0012 == 1 ) ;

    if( ($time_max == 0) || (! $time_max) )
    {
	&debug($DEBUG, "Previous cycle failed and $THIS_CYCLE is not good for cold-start\n");
	exit(0);
    }

    elsif( $time_max == 1 )
    {
	&debug($DEBUG, "Previous cycle failed and $THIS_CYCLE is a cold-start\n");
        my $yr = substr($THIS_CYCLE,0,4);
        my $mo = substr($THIS_CYCLE,4,2);
        my $dy = substr($THIS_CYCLE,6,2);
        my $hr = substr($THIS_CYCLE,8,2);
        my $wrf_time = "${yr}-${mo}-${dy}_${hr}:00:00";
        $FOUT_SUFFIX = "_$wrf_time";
        $POUT_SUFFIX = "_$wrf_time";

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
# Increment the suffix of the wrf file for the next hour
#
sub increment_wrf_file_suffix
{
    ($CURRENT_SUFFIX,$nsecs) = @_;
    my $yr = substr($CURRENT_SUFFIX,1,4);
    my $mo = substr($CURRENT_SUFFIX,6,2);
    my $dy = substr($CURRENT_SUFFIX,9,2);
    my $hr = substr($CURRENT_SUFFIX,12,2);
    my $mm = substr($CURRENT_SUFFIX,15,2);
    my $ss = substr($CURRENT_SUFFIX,18,2);

    my %mo_names = qw/ 01 Jan 02 Feb 03 Mar 04 Apr 05 May 06 Jun 07 Jul 08 Aug 09 Sep 10 Oct 11 Nov 12 Dec /;
    my $mo_name = $mo_names{$mo};
    my $command = "date -u -d \"${hr}:${mm}:${ss} GMT $dy $mo_name $yr + $nsecs seconds\" > newfilesuffix\n";
    system "$command";

    open NEWSUF, "newfilesuffix";
    my($newday_name,$newmo_name,$newdy,$newhhmmss,$dum,$newyr)  = split /\s+/, (<NEWSUF>);
    $newdy = "0".$newdy while (length($newdy) < 2);
    my ($newhr,$newmm,$newss) = split /:/,$newhhmmss;
    my %mo_inds = qw/ Jan 01 Feb 02 Mar 03 Apr 04 May 05 Jun 06 Jul 07 Aug 08 Sep 09 Oct 10 Nov 11 Dec 12 /;
    my $newmo = $mo_inds{$newmo_name};
    my $wrf_time = "${newyr}-${newmo}-${newdy}_${newhr}:${newmm}:${newss}";
    $NEW_SUFFIX = "_".$wrf_time;

    return ($NEW_SUFFIX);
}
sub time_from_wrf_name {
    my $fname = $_[0];
    my $wrf_time_string = substr( $fname, 11);
    my $yr = substr($wrf_time_string,0,4);
    my $mo = substr($wrf_time_string,5,2);
    my $dy = substr($wrf_time_string,8,2);
    my $hr = substr($wrf_time_string,11,2);
    my $mm = substr($wrf_time_string,14,2);
    my $vtime = $yr.$mo.$dy.$hr.$mm;
    return ($vtime);
}

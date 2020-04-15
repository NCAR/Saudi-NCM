#! /usr/bin/perl
#------------------------------------------------------------------------------#
# Driver for WaveWatch model
#
#   cfdda_WaveWatch.pl files=D1/wrfout_d01_*-09-*_00* <year=1901 month=09 day=15 hour=00>
#
# - run WaveWatch from the WRF files matching pattern D1/wrfout_d01_*-09-*_00* 
# - compute statistics from the WaveWatch output files
# - rename the statistics file with the date stamp 1901-09-15_00
# - Plots statistics
#
#------------------------------------------------------------------------------#
# Copyright UCAR (c) 2010
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Laboratory (RAL),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA. 
#
# Francois Vandenberghe, vandenb@ucar.edu, May 2010.
#------------------------------------------------------------------------------#

#use strict;
require "stat.pl";
require "flush.pl";
require "ctime.pl";

use Time::Local;

#------------------------------------------------------------------------------#
# Local variables
#------------------------------------------------------------------------------#

my ($file_pattern,$path_to_files,$wrf_files);
my ($year,$day,$month,$hour);
my ($dom, $strlen, $n, $N,$f, $ff);
my ($domain);
my ($in, $out);
my ($command);
my (@MDL, @MDL1,@listf);
my ($VARS_IN,$VARS_OUT,$VARS);
my ($PLOTS_DIR);
my ($dummy1,$dummy2);
my ($ccyymmddhh,$ccyy,$mm,$dd,$hh);
my ($wrf_before);
our ($lat_antenna,$lon_antenna,$height_antenna);
our ($lat_target,$lon_target,$height_target_max);
#my ($this_cycle, $NUM_DOMS,$OUTPUT_DIR,$FLEXINPUT);

#------------------------------------------------------------------------------#
# Parameters
#------------------------------------------------------------------------------#
#
# WRF variables for wich statistics must be computed

our $NCL_SCRIPTS = "$ENV{MM5HOME}/cycle_code/POSTPROCS/ncl";

our $EXECUTABLE_ARCHIVE = "$ENV{MM5HOME}/cycle_code/EXECUTABLE_ARCHIVE";

# Debug (more prints out when DEBUG = 1, interactive only)

our $DEBUG = 1;

#------------------------------------------------------------------------------#
# Interactive mode: parse arguments
#------------------------------------------------------------------------------#

if ($#ARGV >= 0) {

for ($n = 0; $n <= $#ARGV; $n++) {

     if (index ($ARGV[$n],"=") < 0) {
         print "\n";
         print "ERROR: bad argument $ARGV[$n].\n";
         &help;
     }

     ($dummy1,$dummy2) = split('=',$ARGV[$n]);

# Expect keywords: files, year, month, day, hour

     if ($dummy1 eq 'files') {
         $file_pattern  = $dummy2 ;
     }

     if ($dummy1 eq 'year') {
         $ccyy = $dummy2 ;
     }
     if ($dummy1 eq 'month') {
         $mm = $dummy2 ;
     }
     if ($dummy1 eq 'day') {
         $dd = $dummy2 ;
     }
     if ($dummy1 eq 'hour') {
         $hh = $dummy2 ;
     }

}

if (! defined ($file_pattern)) {
    print "\n";
    print "ERROR: Need file pattern after keyword 'files='\n";
    &help;
}

#------------------------------------------------------------------------------#
# Or batch mode: environment variable flexinput must be defined
#------------------------------------------------------------------------------#

} else{

    if (! -e $ENV{FLEXINPUT})
    {
        print "\n";
        print "ERROR: Variable environment variable FLEXINPUT or file $FLEXINPUT does not exist.\n";
        &help;
    }

    $FLEXINPUT = $ENV{FLEXINPUT};

    print "\n";
    print "FLEXINPUT  = $ENV{FLEXINPUT}\n";

    # This defines the environment (GSJOBDIR, this_cycle, NUM_DOMS, MM5HOME)
    require "$FLEXINPUT";

    # extract time stamp for output file
    if (defined ($this_cycle))
    {
         print "this_cycle = $this_cycle\n";
    }else{
        print "\n";
        print "ERROR: variable this_cycle is undefined in file $FLEXINPUT.\n";
        &help;
    }

    $ccyy = substr($this_cycle,0,4);
    $mm = substr($this_cycle,4,2);
    $dd = substr($this_cycle,6,2);
    $hh = substr($this_cycle,8,2);

    # Number of domains to process
    if (defined ($NUM_DOMS))
    {
         print "NUM_DOMS   = $NUM_DOMS\n";
    }else{
        print "\n";
        print "ERROR: variable NUM_DOMS is undefined in file $FLEXINPUT.\n";
        &help;
    }

    # Read path to output directory from postprocinput.pl
    $POSTPROCINPUT = "$GSJOBDIR/postprocinput.pl";

    print "\n";
    print "POSTPROCINPUT  = $POSTPROCINPUT\n";

    if (! -e $POSTPROCINPUT)
    {
        print "\n";
        print "ERROR: Cannot find file $POSTPROCINPUT.\n";
        &help;
    }

    require $POSTPROCINPUT;

    # Redefine paths since MM5HOME have been reset in flexinput.pl
    $NCL_SCRIPTS = "$MM5HOME/cycle_code/POSTPROCS/ncl";
    $EXECUTABLE_ARCHIVE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE";
    $RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";

    print "WRF_OUTPUT_DIR = $WRF_OUTPUT_DIR\n";
    print "RIP_ROOT = $RIP_ROOT\n";
    print "EXECUTABLE_ARCHIVE = $EXECUTABLE_ARCHIVE\n";
    print "NCL_SCRIPTS = $NCL_SCRIPTS\n";
    print "JOB_LOC = $JOB_LOC\n";
#------------------------------------------------------------------------------#
# Run WaveWatch for each WRF output, smallest domain only
#------------------------------------------------------------------------------#

    for ($dom = 1; $dom <= $NUM_DOMS; $dom++){

        # Workdir for WaveWatch runs
        $RUNS_DIR = "$WRF_OUTPUT_DIR/WaveWatch/D${dom}/D${dom}_$ccyy$mm$dd$hh";

        # Workdir for statistics calculations
        $STATS_DIR = "$WRF_OUTPUT_DIR/WaveWatch_STATS/D${dom}_$ccyy$mm$dd$hh";

        # Workdir for graphics
        $PLOTS_DIR = "$WRF_OUTPUT_DIR/WaveWatch_PLOTS/D${dom}_$ccyy$mm$dd$hh";

        # Output directory
        $OUTPUT_DIR = "$JOB_LOC/WaveWatchRun";

        # Sleep and create files in the output directory 

        mkdir "$OUTPUT_DIR"     if (! -d "$OUTPUT_DIR");
        mkdir "$OUTPUT_DIR/png" if (! -d "$OUTPUT_DIR/png");
        mkdir "$OUTPUT_DIR/grb" if (! -d "$OUTPUT_DIR/grb");

 
#       for ($h = 0; $h <= 23; $h++){

#            if ($h <= 9) {
#                $hr = "0$h";
#            }else{
#                $hr = "$h";
#            }


             # png files
             mkdir "$OUTPUT_DIR/png/$hh" if (! -d "$OUTPUT_DIR/png/$hh");

             foreach $f ("vect_max","wind_avg","wind_std","wind_ros","wave_avg","wave_std","wave_ros")
             {
                $command = "cp -f $RIP_ROOT/not_yet_avail.gif $OUTPUT_DIR/png/$hh/d${dom}_$f.png";
                print "$command\n";
                system("$command");
             }

             # Text files
             mkdir "$OUTPUT_DIR/grb/$hh" if (! -d "$OUTPUT_DIR/grb/$hh");

             $command = "echo $hh > $OUTPUT_DIR/grb/$hh/gribfile_grid${dom}_${ccyy}${mm}${dd}${hh}.grb";
             print "$command\n";
             system("$command");

             sleep 300;

#        }


        # Clean up
        if ($DEBUG < 1) {
            $command = "rm -rf $RUNS_DIR $PLOTS_DIR $STATS_DIR";
            print "\n$command\n";
            system  ($command);
        }

 }


exit 0;

}
#------------------------------------------------------------------------------#
# Subroutines
#------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------
# Name: hh_advan_date
# Arguments: CCYYMMDDHH HOURS
# Return: CCYYMMDDHH
# Description: Advance a date string from a given amount of hours
#-------------------------------------------------------------------------------

sub hh_advan_date {
    use strict;

  my %mon_days = (1,31,2,28,3,31,4,30,5,31,6,30,7,31,8,31,9,30,10,31,11,30,12,31);
  (my $s_date, my $advan_hh) = @_ ;

  my $yy = substr($s_date,0,4);
  my $mm = substr($s_date,4,2);
  my $dd = substr($s_date,6,2);
  my $hh = substr($s_date,8,2);

  my $feb = 2;
  $mon_days{$feb} = 29 if ($yy%4 == 0 && ($yy%400 == 0 || $yy%100 != 0));

  $hh = $hh + $advan_hh;
  while($hh > 23) {
  $hh -= 24;
  $dd++;
  }
  while($dd > $mon_days{$mm+0}) {
  $dd = $dd - $mon_days{$mm+0};
  $mm++;
  while($mm > 12) {
  $mm -= 12;
  $yy++;
  }
  }

  while($hh < 0) {
  $hh += 24;
  $dd--;
  }
  if($dd < 1) {
  $mm--;
  $dd += $mon_days{$mm+0};
  while($mm < 1) {
  $mm += 12;
  $dd += $mon_days{$mm+0};
  $yy--;
  }
  }
  my $new_date = sprintf("%04d%02d%02d%02d",$yy,$mm,$dd,$hh);
}

#-------------------------------------------------------------------------------
# Sub. help.
#-------------------------------------------------------------------------------
sub help{

print "\n";
print "Usage: start_climops_stats.pl files=D1/wrfout_d01_\\*-09-\\*_00\\* \n";
print "                              <year=1901 month=09 day=15 hour=00>\n";
print "\n";
print "To postprocess all files matching pattern D1/wrfout_d01_*-09-*_00*\n";
print "And rename the statistics file with the date stamp 1901-09-15_00\n";
print "\n";
exit -1

}

1

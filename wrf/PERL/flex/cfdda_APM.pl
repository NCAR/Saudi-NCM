#! /usr/bin/perl
#------------------------------------------------------------------------------#
# Driver for APM model
#
#   cfdda_APM.pl files=D1/wrfout_d01_*-09-*_00* <year=1901 month=09 day=15 hour=00>
#
# - run APM from the WRF files matching pattern D1/wrfout_d01_*-09-*_00* 
# - compute statistics from the APM output files
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
my ($dom, $strlen, $i, $n, $N,$f, $ff, $fff);
my ($domain);
my ($in, $out);
my ($command);
my (@MDL, @MDL1,@listf);
my ($VARS_IN,$VARS_OUT,$VARS);
my ($PLOTS_DIR);
my ($dummy1,$dummy2);
my ($ccyymmddhh,$ccyy,$mm,$dd,$hh);
my ($wrf_before);
my ($source_id);
my (@frequencies_mhz);
my (@heights_antenna);
my (@heights_target_max);

our $missing_value = -999;

#our ($lat_antenna,$lon_antenna);
#our ($lat_target,$lon_target);

#@frequencies_mhz    = (3000,3000); # MHZ
#@heights_antenna    = (5,15);      # m
#@heights_target_max = (1000,1000); # m

#------------------------------------------------------------------------------#
# Parameters
#------------------------------------------------------------------------------#
#
# WRF variables for wich statistics must be computed

our $NCL_SCRIPTS = "$ENV{MM5HOME}/cycle_code/POSTPROCS/ncl";

our $EXECUTABLE_ARCHIVE = "$ENV{MM5HOME}/cycle_code/EXECUTABLE_ARCHIVE";

our $RIP_ROOT = "$ENV{MM5HOME}/cycle_code/CONSTANT_FILES/RIP4";

# IMAGE_DENSITY is used by the "convert" command to set dots-per-inch.  This
# has the effect of changing the geometric size of the resulting png or gif,
# along with increasing image resolution/clarity
# 120 is a decent default value giving image sizes of about 900x900 pixels.

our $IMAGE_DENSITY = 120;

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
# Add new field, compute stats and plot
#------------------------------------------------------------------------------#

#   &do_pre_stats ($file_pattern,$ccyy,$mm,$dd,$hh);

#   &do_stats ($file_pattern,$ccyy,$mm,$dd,$hh);

#   &do_rip_plot ($file_pattern,$ccyy,$mm,$dd,$hh);

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

    # Make sure the antenna/target coordinates have been defined

     if (! defined ($lat_antenna) || ! defined ($lon_antenna) ||
         ! defined ($lat_target)  || ! defined ($lon_target)) {
           $lat_antenna = $missing_value; 
           $lon_antenna = $missing_value;
           $lat_target  = $missing_value;
           $lon_target  = $missing_value;
     }

     if (! defined ($height1_antenna)){
         $height1_antenna = 5; # m 
     }else{
         if ($height1_antenna <= $missing_value){ 
             $height1_antenna = 5; # m 
         }
     }

     if (! defined ($height2_antenna)){
         $height2_antenna = 15; # m 
     }else{
         if ($height2_antenna <= $missing_value){ 
             $height2_antenna = 15; # m 
         }
     }

     if (! defined ($height_target_max)){
         $height_target_max = 1000; # m 
     }else{
         if ($height_target_max <= $missing_value){ 
             $height_target_max = 1000; # m 
         }
     }

     if (! defined ($frequency_mhz)){
         $frequency_mhz = 3000; # mHz
     }else{
         if ($frequency_mhz <= $missing_value){ 
             $frequency_mhz = 3000; # mHz 
         }
     }

     if (! defined ($dist_max_km)){
         $dist_max_km = 50; # km
     }else{
         if ($dist_max_km <= $missing_value){ 
             $dist_max_km = 50; # km
         }
     }

     @heights_antenna = ($height1_antenna,$height2_antenna);
     @frequencies_mhz = ($frequency_mhz,$frequency_mhz);
     @heights_target_max = ($height_target_max,$height_target_max);

#------------------------------------------------------------------------------#
# Run APM for each WRF output, smallest domain only
#------------------------------------------------------------------------------#

    for ($i = 0; $i <= 1; $i++){

    $source_id = $i+1;

    print 
"\n-------------------------------------------------------------------------\n";
    print "Set of plots $source_id\n";
    print "\n"; 
    print "height_antenna = $heights_antenna[$i]m\n";
    print "height_target_max = $heights_target_max[$i]m\n";
    print "frequency = $frequencies_mhz[$i]MHz\n";

    for ($dom = $NUM_DOMS; $dom <= $NUM_DOMS; $dom++){

        # Workdir for APM runs
        $RUNS_DIR = "$WRF_OUTPUT_DIR/APM/D${dom}/D${dom}_$ccyy$mm$dd$hh";

        # Workdir for statistics calculations
        $STATS_DIR = "$WRF_OUTPUT_DIR/APM_STATS/D${dom}_$ccyy$mm$dd$hh";

        # Workdir for graphics
        $PLOTS_DIR = "$WRF_OUTPUT_DIR/APM_PLOTS/D${dom}_$ccyy$mm$dd$hh";

        # Output directory
        $OUTPUT_DIR = "$JOB_LOC/Capricorne";

        # File pattern is hardwired for hourly stats
        $file_pattern = "$WRF_OUTPUT_DIR/WRF/D$dom/wrfout_d0${dom}_\*-\*-\*_${hh}:00:00.GRM_F";

        # Run the APM model

        ($status,$caption) = &do_run_apm ($file_pattern,$ccyy,$mm,$dd,$hh,$lat_antenna,$lon_antenna,$lat_target,$lon_target,$heights_antenna[$i],$heights_target_max[$i],$frequencies_mhz[$i],$dist_max_km,$RUNS_DIR,$POSTPROCINPUT);

        if ($status < 0) {
            print "\nERROR in do_run_apm, status = $status\n";
            exit -1
        }

        # Files have been renamed in do_pre_stats
        $file_pattern = "$RUNS_DIR/apmout_d0${dom}_\*-\*-\*_${hh}:00:00";

        # Compute and plot Hourly Stats
        $status = &do_stats ($file_pattern,$ccyy,$mm,$dd,$hh,$source_id,$caption,$STATS_DIR,$OUTPUT_DIR);
        if ($status < 0) {
            print "\nERROR in do_stats, status = $status\n";
            exit -1
        }

#       # Output statistics file to plot
#       $file_to_plot = "$STATS_DIR/avg_d0${dom}_${ccyy}-${mm}-${dd}_${hh}:00:00.nc";

        # Plot
#       $status = &do_rip_plot ($file_to_plot,$RIP_IMG,$WEB_IMG,$PLOTS_DIR,$OUTPUT_DIR);

#       if ($status < 0) {
#           print "\nERROR in do_rip_plot, status = $status\n";
#           exit -1
#       }

        # Clean up
        if ($DEBUG < 1) {
            $command = "rm -rf $PLOTS_DIR $STATS_DIR";
            print "\n$command\n";
            system  ($command);
        }

#       &ensemble_plots ($dom,"${ccyy}${mm}${dd}${hh}",$WEB_IMG,$PLOTS_DIR,$OUTPUT_DIR);
    }

  }
 }


exit 0;

#------------------------------------------------------------------------------#
# Subroutines
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Sub do_run_apm
# Convert WRF output into APM input files
#------------------------------------------------------------------------------#

sub do_run_apm
{

use strict;

my ($file_pattern,$year,$day,$month,$hour,$lat_antennai,$lon_antennai,$lat_targeti,$lon_targeti,$height_antenna,$height_target_max,$frequency_mhz,$dist_max_km,$stats_dir,$postprocinput);

my ($path_to_files,$wrf_files);
my ($dom, $strlen, $n, $N,$f, $ff);
my ($domain);
my ($in, $out);
my ($command);
my ($ccyymmddhh,$ccyy,$mm,$dd,$hh);
my ($wrf_before);
my (@MDL, @MDL1,@listf,@tmp);
my ($dist_max_km);
my ($lat_antenna,$lon_antenna,$lat_target,$lon_target);
my ($caption);

$file_pattern = shift; 
$year  = shift;
$month = shift;
$day   = shift;
$hour  = shift;
$lat_antennai = shift;
$lon_antennai = shift;
$lat_targeti  = shift;
$lon_targeti  = shift;
$height_antenna = shift;
$height_target_max = shift;
$frequency_mhz = shift;
$dist_max_km = shift;
$stats_dir = shift;
$postprocinput = shift;

our ($lat_antenna,$lon_antenna,$lat_target,$lon_target);
#------------------------------------------------------------------------------#
# Print arguments
#------------------------------------------------------------------------------#

print "\n";
print 
"---------------------------------------------------------------------";
print "\n";
print "do_run_apm ($file_pattern,$year,$day,$month,$hour,$lat_antenna,$lon_antennai,$lat_targeti,$lon_targeti,$height_antenna,$height_target_max,$frequency_mhz,$stats_dir,$postprocinput)\n";
print "\n";


#------------------------------------------------------------------------------#
# Load postprocinput file
#------------------------------------------------------------------------------#

#require $postprocinput;

print ("\n");
print "height_antenna = ${height_antenna}m\n";
print "height_target_max = ${height_target_max}m\n";
print "frequency = ${frequency_mhz}Mhz\n";

$caption = "Antenna height = ${height_antenna}m, Target max height = ${height_target_max}m, Frequency = ${frequency_mhz}MHz"; 

#------------------------------------------------------------------------------#
# Find path to WRF files
#------------------------------------------------------------------------------#

@tmp = split ('/',$file_pattern);

if ($#tmp >= 0){

    $path_to_files = $tmp[0];

    for ($n = 1; $n < $#tmp; $n++){
         $path_to_files = $path_to_files."/".$tmp[$n];
    }

    # Find WRF files
    $wrf_files = $tmp[$#tmp];
    undef @tmp;

    # Find WRF domain
    $domain = substr($wrf_files,9,1);

    if ($domain !~ /[1-9]/)
    { 
     print "\n";
     print "ERROR: bad argument: Cannot extract WRF domain in files=$file_pattern\n";
     &help;
    }

}else{
    $path_to_files = ".";
}

print "\nPath to WRF files = $path_to_files\n";
print "\nWRF files pattern = $wrf_files\n";

if (! -d $path_to_files){
    print "\n";
    print "ERROR: Cannot find directory $path_to_files\n";
    &help;
}

chdir $path_to_files;

#------------------------------------------------------------------------------#
# List all files whose names are matching the file name pattern 
#------------------------------------------------------------------------------#

print "\n";
print "Post-processing files with name ${file_pattern}\n";

@MDL1 = `ls $wrf_files`;

# Print if no input files were found
if ($#MDL1 < 0) {
    print "\nERROR: Cannot find any file with name $wrf_files in directory $path_to_files\n";
    exit -1;
}else{
     @MDL = sort @MDL1;
     print "\nFound files:\n @MDL\n";
}

# Find how many characters are needed
$strlen = length ("$#MDL"); 

#------------------------------------------------------------------------------#
# Move to the work dir
#------------------------------------------------------------------------------#

if (! -d $stats_dir){  
    system ("mkdir -p $stats_dir") 
}

if (! chdir $stats_dir){  
    print "ERROR cannot chdir to $stats_dir/D${dom}_${ccyy}${mm}${dd}${hh}\n";
    return -1;
}

print "\nWorkdir is $stats_dir\n\n";

#------------------------------------------------------------------------------#
# Link NCL scripts into local directory
#------------------------------------------------------------------------------#

for $f ("f_edh_analytic.ncl","f_edh_discrete.ncl","f_FillValue.ncl","f_refmod.ncl","f_refract.ncl","config_APM.ncl","WRFtoAPM.ncl","plot_config_APM.ncl","stats_APM.ncl")
 {

        if (! -e "$NCL_SCRIPTS/$f"){
            print "\n";
            print "ERROR cannot find script $NCL_SCRIPTS/$f\n";
            return -1;
         }
           
         unlink $f if (-e $f); 
         symlink ("$NCL_SCRIPTS/$f","$f");
}

#------------------------------------------------------------------------------#
# Loop over the list of files
#------------------------------------------------------------------------------#

$n = 1;
foreach $ff (@MDL) {

     chomp ($ff);

     print  "\n=============================================================\n";
     print "Process file $ff\n\n";

     #-------------------------------------------------------------------------#
     # Skip if the file does not exist
     #-------------------------------------------------------------------------#

     next if (! -e "$path_to_files/$ff");

     #-------------------------------------------------------------------------#
     # Get rid of the GRM suffixes
     #-------------------------------------------------------------------------#

     if ($ff =~ /GRM_F/) {
         $f = substr ($ff,0,length($ff)-6);
     }else{
         if ($ff =~ /GRM_P+FCST/) {
             $f = substr ($ff,0,length($ff)-11);
         }else{
             $f = $ff;
         }
     }

     #-------------------------------------------------------------------------#
     # 6.3 Bring the file
     #-------------------------------------------------------------------------#

     unlink $f if (-e $f);

     $command =  "ln -f  $path_to_files/$ff $f";  
     print  "$command\n" if ($DEBUG > 0);
     system ($command);

     next if (! -e $f);

     #-------------------------------------------------------------------------#
     # 6.4 Find the antenna and target coordinates if they haven't been provided
     #-------------------------------------------------------------------------#

     if ($lat_antennai <= $missing_value || $lon_antennai <= $missing_value ||
         $lat_targeti  <= $missing_value || $lon_targeti  <= $missing_value) {

         system ("rm -f config_apm.pl") if (-f "config_apm.pl");

         $command = "ncl \'file_in=\"$f\"\' \'dist_max_km=$dist_max_km\' \'file_apm=\"config_apm.pl\"\' config_APM.ncl"; 

         print  "\n-------------------------------------------------------------\n";
         print  "\n$command\n\n";
         system ($command);

        if (! -e "config_apm.pl"){
            print  "\nERROR in ${0}: Failed to find suitable antenna and target coordinates for APM from file $f\n\n";
            print  "Target and Antenna coordinates can be manually set in file $postprocinput\n\n";
            next ;
        }else{

        system ("cat ./config_apm.pl");
        require "config_apm.pl";

        print ("\n");
        print "lon_antenna = $lon_antenna\n";
        print "lat_antenna = $lat_antenna\n";
        print "lon_target = $lon_target\n";
        print "lat_target = $lat_target\n";
        }

     }

# DUCK
#       $lon_antenna = -75.00;
#       $lat_antenna =  36.10;
#       $lon_target  = -75.40;
#       $lat_target  =  36.10;

     #-------------------------------------------------------------------------#
     # 6.4 Convert WRF output into APM input file
     #-------------------------------------------------------------------------#

     system ("rm -f apm.in") if (-f "apm.in");

     $command = "ncl \'file_in=\"$f\"\' \'antenna=(/$lon_antenna,$lat_antenna,$height_antenna/)\' \'target=(/$lon_target,$lat_target,$height_target_max/)\' \'frequency=$frequency_mhz\' WRFtoAPM.ncl"; 

     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);

     if (! -e "apm.in"){
         print  "\nERROR in ${0}: APM convertion for file $f failed\n\n";
         next ;
     }

     #-------------------------------------------------------------------------#
     # 6.5 Run APM
     #-------------------------------------------------------------------------#

     system ("rm -f apm.out") if (-f "apm.out");

     $command = "$EXECUTABLE_ARCHIVE/apm.exe"; 

     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);

     if (! -e "apm.out"){
         print  "\nERROR in ${0}: APM run for file $f failed\n\n";
         if (-e "apm.err"){
             system ("cat apm.err");
         }
         next ;
     }

     
     #-------------------------------------------------------------------------#
     # 6.7 If runs fails, rename APM log time tag apmout_d0<DOM>_<CCYY-MM-DD_HH>:00:00
     #-------------------------------------------------------------------------#

     if (! -z "apm.err") # Expect error file to have 0 size
     {
         $a = $f;
         substr ($a,0,6) = "apmerr";
         $command = "mv -f apm.err $a"; 
         print  "\n$command\n\n";
         system ($command);

     #-------------------------------------------------------------------------#
     # 6.7 Rename APM output with time tag apmout_d0<DOM>_<CCYY-MM-DD_HH>:00:00
     #-------------------------------------------------------------------------#

     }else{   
         $a = $f;
         substr ($a,0,3) = "apm";
         $command = "mv -f apm.out $a"; 
         print  "\n$command\n\n";
         system ($command);
     
     #------------------------------------------------------------------------
     # 6.8 Save name of valid WRF file
     #------------------------------------------------------------------------

         $fff = $f;

     #-------------------------------------------------------------------------#
     # 6.9 Keep track of the number of APM file created
     #-------------------------------------------------------------------------#

         $n ++;
     }

}


$N = $n - 1;

print "\n-------------------------------------------------------------\n";
print "\nProcessed $N files with file name $file_pattern\n";

#-------------------------------------------------------------------------#
# Plot APM configuration from the last valid WRF file
#-------------------------------------------------------------------------#
 
if (-e $fff) {

     $command = "ncl \'file_in=\"$fff\"\' \'antenna=(/$lon_antenna,$lat_antenna,$height_antenna/)\' \'target=(/$lon_target,$lat_target,$height_target_max/)\' \'graph_name=\"plot_config_APM\"\' \'graph_type=\"ps\"\' plot_config_APM.ncl"; 

     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);

#-------------------------------------------------------------------------#
# Check for image
#-------------------------------------------------------------------------#

     if (! -e "plot_config_APM.ps") {

         print "\nWARNING in $0: plot of APM config was not generated\n";

     }else{

#-------------------------------------------------------------------------#
# Convert image into png and move to output directory
#-------------------------------------------------------------------------#

         $command = "convert -trim +repage -density $IMAGE_DENSITY plot_config_APM.ps plot_config_APM.png";
         print "\n$command\n";
         system("$command");

#-------------------------------------------------------------------------#
# Move image to output drectory
#-------------------------------------------------------------------------#

         $command = "cp -f plot_config_APM.png config_APM_${year}${month}$}{day}${hour}.png"; 
         print  "\n$command\n\n";
         system ($command);

    }
#-------------------------------------------------------------------------#
# Exit if APM did not run
#-------------------------------------------------------------------------#

}else{
         print "\nWARNING in $0: could not run APM from any of the WRF files\n";
}


#-------------------------------------------------------------------------------
# End
#-------------------------------------------------------------------------------

print "\n";
return ($N,$caption);

}


#------------------------------------------------------------------------------#
# Sub: do_stats
# Compute mean, max, min and stdv with NCO operators
#------------------------------------------------------------------------------#

sub do_stats
{

use strict;

my ($file_pattern,$year,$day,$month,$hour,$source_id,$caption,$stats_dir,$output_dir);
my ($path_to_files,$apm_files);
my ($dom, $strlen, $n, $N,$f, $ff);
my ($domain);
my ($in, $out);
my ($command);
my (@MDL, @MDL1,@listf,@tmp);

$file_pattern = shift; 
$year  = shift;
$month = shift;
$day   = shift;
$hour  = shift;
$source_id = shift;
$caption = shift;
$stats_dir = shift;
$output_dir = shift;

#------------------------------------------------------------------------------#
# Print arguments
#------------------------------------------------------------------------------#

print "\n";
print 
"---------------------------------------------------------------------";
print "\n";
print "do_stats ($file_pattern,$year,$month,$day,$hour,$source_id,$caption,$stats_dir,$output_dir)\n";
print "\n";


#------------------------------------------------------------------------------#
# Find path to WRF files
#------------------------------------------------------------------------------#

@tmp = split ('/',$file_pattern);

if ($#tmp >= 0){

    $path_to_files = $tmp[0];

    for ($n = 1; $n < $#tmp; $n++){
         $path_to_files = $path_to_files."/".$tmp[$n];
    }

    # Find APM files
    $apm_files = $tmp[$#tmp];
    undef @tmp;

    # Find WRF domain
    $domain = substr($apm_files,9,1);

    if ($domain !~ /[1-9]/)
    { 
     print "\n";
     print "ERROR: bad argument: Cannot extract WRF domain in files=$file_pattern\n";
     &help;
    }

}else{
    $path_to_files = ".";
}

print "\nPath to WRF files = $path_to_files\n";
print "\nWRF files pattern = $apm_files\n";

if (! -d $path_to_files){
    print "\n";
    print "ERROR: Cannot find directory $path_to_files\n";
    &help;
}

chdir $path_to_files;

#------------------------------------------------------------------------------#
# List all files whose names are matching the file name pattern 
#------------------------------------------------------------------------------#

print "\n";
print "Post-processing files with name ${file_pattern}\n";

@MDL1 = `ls $apm_files`;

# Print if no input files were found
if ($#MDL1 < 0) {
    print "\nERROR: Cannot find any file with name $apm_files in directory $path_to_files\n";
    exit -1;
}else{
     @MDL = sort @MDL1;
     print "\nFound files:\n @MDL\n";
}

# Find how many characters are needed
$strlen = length ("$#MDL"); 

#------------------------------------------------------------------------------#
# Link NCL scripts into local directory
#------------------------------------------------------------------------------#

for $f ("stats_APM","f_FillValue"){

        if (! -e "$NCL_SCRIPTS/$f.ncl"){
            print "\n";
            print "ERROR cannot find script $NCL_SCRIPTS/$f.ncl\n";
            return -1;
         }
           
         symlink ("$NCL_SCRIPTS/$f.ncl","$f.ncl");
}


#------------------------------------------------------------------------------#
# Run the NCL scripts
#------------------------------------------------------------------------------#

$command = "ncl \'pattern=\"$apm_files\"\' \'graph_type=\"ps\"\' \'caption=\"$caption\"\' stats_APM.ncl";

print  "\n-------------------------------------------------------------\n";
print  "\n$command\n\n";
system ($command);

$n = 0;
#------------------------------------------------------------------------------#
# Create output dir
#------------------------------------------------------------------------------#

    print  "\n-------------------------------------------------------------\n";

    if (! -d "$output_dir/txt") {
         $command = "mkdir -p $output_dir/txt";
         print "\n$command\n";
         system  ($command);
         sleep 2;
    }

    if (! -d "$output_dir/txt") {

        print "ERROR: Cannot find directory $output_dir/txt\n";

    }else{

#------------------------------------------------------------------------------#
# Move files to output dir
#------------------------------------------------------------------------------#

    @tmp = ("avg","min","max","std") ;

    foreach $f (@tmp){

        if (! -e "apm_${f}.txt") {
            print "Cannot find file apm_${f}.txt, skipping...\n";
            next;
         }

         # Copy to output dir
         $command = "cp -f apm_${f}.txt $output_dir/txt/apm_${f}${source_id}_d0${domain}_${year}-${month}-${day}_${hour}.txt";
         print  "\n$command\n";
         system ($command);
         $n ++;

    }
    undef @tmp;
 }
   
#------------------------------------------------------------------------------#
# Create output dir
#------------------------------------------------------------------------------#

    print  "\n-------------------------------------------------------------\n";

    if (! -d "$output_dir/png/${hour}") {
         $command = "mkdir -p $output_dir/png/${hour}";
         print "\n$command\n";
         system  ($command);
         sleep 2;
    }

    if (! -d "$output_dir/png/${hour}") {

        print "ERROR: Cannot find directory $output_dir/png/${hour}\n";

    }else{

#------------------------------------------------------------------------------#
# Move images to output dir
#------------------------------------------------------------------------------#

    @tmp = ("loss","pfac");

    foreach $f (@tmp){

         if (! -e "apm_${f}.ps") {

             print "Cannot find file apm_${f}.ps\n";
             $command = "cp $RIP_ROOT/not_yet_avail.png $output_dir/png/${hour}/antenna_${f}${source_id}.png";
             print "$command\n";
             system("$command");

         }else{

             # Convert into png
             $command = "convert -trim +repage -density $IMAGE_DENSITY apm_${f}.ps apm_${f}.png";
             print "\n$command\n";
             system("$command");

             # Copy
             $command = "cp -f apm_${f}.png $output_dir/png/${hour}/antenna_${f}${source_id}.png";
             print  "$command\n";
             system ($command);
             $n ++;
         }

    }

    undef @tmp;
  }

  #-----------------------------------------------------------------------------
  # Move the image of the APM configuration
  #-----------------------------------------------------------------------------

  if (-e "plot_config_APM.png"){
      $command = "cp -f plot_config_APM.png $output_dir/png/${hour}/antenna_config${source_id}.png";
      print  "\n$command\n\n";
      system ($command);
  }else{
      $command = "cp $RIP_ROOT/not_yet_avail.png $output_dir/png/${hour}/antenna_config${source_id}.png";
      print "$command\n";
      system("$command");
  }


#-------------------------------------------------------------------------------
# End
#-------------------------------------------------------------------------------

print "\n";
return $n;

}

#-------------------------------------------------------------------------------
# Sub plot_rip
#-------------------------------------------------------------------------------

sub do_rip_plot 
{

#use strict;

my ($hourly_file,$RIP_IMG,$WEB_IMG,$plots_dir,$output_dir) = @_;

my (@SEASONS,@parts);
my ($RIPDP_EXE,$RIP_EXE); #,$RIP_IMG,$WEB_IMG);
my ($wrf_domain,$wrf_date_string,$valid_time,$season);
my ($ccyy,$mm,$dd,$hh,$mn,$ss);
my ($range,$fn,$d);
my $nimages = 0;

print "\n";
print 
"---------------------------------------------------------------------";
print "\n";
print "do_rip_plot ($hourly_file,$RIP_IMG,$WEB_IMG,$plots_dir,$output_dir)\n";
print "\n";

#------------------------------------------------------------------------------#
# Definitions
#------------------------------------------------------------------------------#

# Use different contoure depending on the season
@SEASONS=( 'winter', 'winter', 'winter', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'winter');
@MONTHS=( 'Jan.', 'Feb.', 'Mar.', 'Apr.', 'May', 'Jun.', 'Jul.', 'Aug.', 'Sep.', 'Oct.', 'Nov.', 'Dec.');
@MONTHS_FR=( 'Janvier', 'Fevrier', 'Mars', 'Avril', 'Mai', 'Juin', 'Juillet', 'Aout', 'Septembre', 'Octobre', 'Novembre', 'Decembre');

# Environment
$ENV{'NCARG_ROOT'} = "$NCARG_ROOT";
$ENV{'NCARG_LIB'} = "$NCARG_LIB";
$ENV{'NCARG_RANGS'} = $NCARG_RANGS_DIR;
$ENV{'RIP_ROOT'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";

# Executables
$RIPDP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_wrf.exe";
$RIP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip.exe";

# Need the list of station for rip
if ( -e "$GSJOBDIR/postprocs/stationlist" ) {
    $ENV{'STATIONLIST'} = "$GSJOBDIR/postprocs/stationlist";
   } else {
    $ENV{'STATIONLIST'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4/stationlist";
   }

# Image format
#$RIP_IMG = "ps";
#$WEB_IMG = "png";

# RIP image format: (meta file or postscript)
if ( $RIP_IMG ne "ps" and $RIP_IMG ne "cgm" ) {
     $RIP_IMG = "cgm";
}

# Final image format: gif or png
if ( $WEB_IMG ne "gif" and $WEB_IMG ne "png" ) {
     $WEB_IMG = "png";
}

# Image titles default
$title_rangename = $RANGE;
$title_cycle = $valid_time;

#------------------------------------------------------------------------------#
# Move to the work dir
#------------------------------------------------------------------------------#

if (! -d $plots_dir){  
    system ("mkdir -p $plots_dir");
    sleep 2;
}

if (! chdir $plots_dir){  
    print "ERROR cannot chdir to $plots_dir\n";
    return -1;
}

print "\nWorkdir is $plots_dir\n\n";

#------------------------------------------------------------------------------#
# Calculations
#------------------------------------------------------------------------------#

# Clean up
system ("rm -rf ripscr")   if (-e "ripscr");
system ("rm -rf riprun")   if (-e "riprun");

system ("mkdir -p ripscr");
system ("mkdir -p riprun");

# Split the wrf file name to extract the time tag
# wrfout_d01_CCYY-MM-DD_HH:MN:SS.RANGE
@parts = split("d0",$hourly_file);
$wrf_domain = substr(@parts[1],0,1);
$wrf_date_string = substr(@parts[1],2,19);

# Reformat date string into CCYYMMDDHHMN
$ccyy = substr($wrf_date_string,0,4);
$mm   = substr($wrf_date_string,5,2);
$dd   = substr($wrf_date_string,8,2);
$hh   = substr($wrf_date_string,11,2);
$mn   = substr($wrf_date_string,14,2);
$ss   = substr($wrf_date_string,17,2);

$valid_time = "$ccyy$mm$dd$hh$mn";

# Grab the month and the season
$month = substr( $valid_time, 4, 2);
$season = @SEASONS[$month-1];

# Rename a few variables
$fn = $hourly_file;
$range = $RANGE;
$d = $wrf_domain;

# Plot main title
$title_rangename = "Hourly mean for";

# Plot time title
$title_cycle = "valid $MONTHS[$month*1-1] ${dd} at ${hh}z";
print "$title_cycle\n";

#------------------------------------------------------------------------------#
# Go to rip pre-processor work dir
#------------------------------------------------------------------------------#

if (! chdir "$plots_dir/ripscr")
{
    print "ERROR: Cannot chdir to directory $plots_dir/ripscr\n";
    return -1;
}


# Run rip preprocessor
$command = "$RIPDP_EXE Domain_$d all $fn >& $plots_dir/ripdp$d.log\n";
print "\n$command\n";
system("$command");

# Read the valid time of the file as extracted by rip pre-processor
open(XTIMES, "Domain_$d.xtimes") || die "ERROR: Cannot find file Domain_$d.xtimes";
$ntimes = <XTIMES>;
while ( $xtin = <XTIMES>) {
        $xt = $xtin;
}
close(XTIMES);
chomp($xt);

# Go to the rip work dir
if (! chdir "$plots_dir/riprun")
{
    print "ERROR: Cannot chdir to directory $plots_dir/riprun\n";
    return -1;
}

# Bring RIP namelists and prepare input
if ( -e "$GSJOBDIR/postprocs/Mdomain$d.$season.$range" ) {
     print "getting $GSJOBDIR/postprocs/Mdomain$d.$season.$range\n";
     system("cat $GSJOBDIR/postprocs/Mdomain$d.$season.$range | sed s/plttime/$xt/g > domain$d.1");
}else{
     print "\nERROR: Cannot find file $GSJOBDIR/postprocs/Mdomain$d.$season.$range\n";
     return -1;
}

system("cat domain$d.1 | sed s/rangename/'$title_rangename'/g > domain$d.2");
system("cat domain$d.2 | sed s/cycle/'$title_cycle'/g > domain$d.3");
$fcoffset = int($time_max / 60.);
system("cat domain$d.3 | sed s/OFFSET/$fcoffset/g > domain$d.4");
system("cat domain$d.4 | sed s/IMGFMT/$RIP_IMG/g > domain$d.5");

if ( -e  "$GSJOBDIR/postprocs/Xdomain.$season.$range" ) {
     system("cat domain$d.5 $GSJOBDIR/postprocs/Xdomain.$season.$range > domain$d.6");
} else {
     print "\nWARNING: Cannot find file $GSJOBDIR/postprocs/Xdomain.$season.$range\n";
}

if ( -e  "$GSJOBDIR/postprocs/Mdomain.sloc.$range" ) {
     system("cat domain$d.6 $GSJOBDIR/postprocs/Mdomain.sloc.$range > domain$d.7");
} else {
     print "\nWARNING: Cannot find file $GSJOBDIR/postprocs/Mdomain.sloc.$range\n";
}


if ( -e  "$GSJOBDIR/postprocs/Mdomain.spec.$range" ) {
     system("cat domain$d.7 $GSJOBDIR/postprocs/Mdomain.spec.$range > domain$d.8");
} else {
     print "\nWARNING: Cannot find file $GSJOBDIR/postprocs/Mdomain.spec.$range\n";
}

system("cat domain$d.8 > domain$d.in");

# Run rip4
$command = "$RIP_EXE ../ripscr/Domain_$d domain${d}.in  >& $plots_dir/rip$d.log";
print "\n$command\n";
system("$command");

# Convert RIP output into images
$field_file = "$GSJOBDIR/postprocs/image_fields.pl";

#print "Run conv_img domain$d.$RIP_IMG, $d, $valid_time, $RIP_IMG, $WEB_IMG\n";

$nimages = &conv_img( "domain$d.$RIP_IMG", $d, $valid_time, $RIP_IMG, $WEB_IMG, $field_file,$plots_dir, $output_dir);

return $nimages;

}

#------------------------------------------------------------------------------#
#
#------------------------------------------------------------------------------#

sub conv_img
{
my ($cgm_file,$domain, $valid_time, $from_img, $to_img, $field_file,$plots_dir,$output_dir) = @_;

print "\n";
print 
"---------------------------------------------------------------------";
print "\n";
print "\nconv_img ($cgm_file,$domain, $valid_time, $from_img, $to_img, $field_file,$plots_dir,$output_dir)\n";
print "\n";

# $from_img: 'cgm' or 'ps'
# $to_img: 'png' or 'gif'

# set environment variables

$ENV{'NCARG_ROOT'} = $NCARG_ROOT;
$ENV{'NCARG_LIB'} = $NCARG_LIB;
$RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";
$ENV{'RIP_ROOT'} = "$RIP_ROOT";


if (! chdir $plots_dir){  
    print "ERROR cannot chdir to $plots_dir\n";
    return -1;
}

system ("rm -rf gifs_gui") if (-e "gifs_ugui");
system ("mkdir -p gifs_ugui");

# Goto workdir
if (! chdir "$plots_dir/gifs_ugui")
{
    print "ERROR: Cannot chdir to directory $plots_dir/gifs_ugui\n";
    return -1;
}

print "\nWorkdir is $plots_dir/gifs_ugui\n\n";

# Extract hour from valid time

$valid_hour = substr ($valid_time,8,2);

# Create output directory
system ("mkdir -p $output_dir/$to_img/$valid_hour");

if (! -d "$output_dir/$to_img/$valid_hour")
{
    print "ERROR: Cannot create directory $output_dir/$to_img/$valid_hour\n";
    return -1;
}

# Bring the list of images
if ( -e "$field_file" ) {
    require "$field_file";
} elsif ( -e "$GSJOBDIR/postprocs/$field_file" ) {
    print "WARNING: Did not find file $field_file";
    print "         Will use file $GSJOBDIR/postprocs/image_fields.pl";
} else {
    print "ERROR: Cannot find file $GSJOBDIR/postprocs/$field_file";
    return -1;
}
  
# IMAGE_DENSITY is used by the "convert" command to set dots-per-inch.  This
# has the effect of changing the geometric size of the resulting png or gif,
# along with increasing image resolution/clarity
# 120 is a decent default value giving image sizes of about 900x900 pixels.
if ( $IMAGE_DENSITY <= 0 ) {
  $IMAGE_DENSITY = 120;
}
$rasfmt = "ps.color";

# if this is a cgm file from NCARGraphics, convert it to postscript with ctrans

$psfile = "file.ps";

if ( $from_img ne "ps") {

   if ( -e "../riprun/$cgm_file" ) {
     $command = "${NCARG_ROOT}/bin/ctrans -d $rasfmt -lscale 1.5  ../riprun/$cgm_file > $psfile";
     print "\n$command\n";
     system("$command");
   }else{
      print "\nERROR: Cannot find file ../riprun/$cgm_file\n";
      return -1;
   }

# only cgm or ps are options 
} else {

   if ( -e "../riprun/$cgm_file" ) {
      $command = "rm -f $psfile";
#     print "\n$command\n";
      system("$command");
      $command = "ln -s ../riprun/$cgm_file $psfile";
#     print "\n$command\n";
      system("$command");
   }else{
      print "\nERROR: Cannot find file ../riprun/$cgm_file\n";
      return -1;
   }

}

# now, extract the frames using psplit ("plot" is the output filename root)
   $command = "${NCARG_ROOT}/bin/psplit $psfile plot ";
   print "\n$command\n";
   system("$command");

# Now loop through the individual frames, and remane them to "nice" field-name images
# while converting to png or gif for web output

   print "\nImage conversion:\n";

$fnum = 0;
$ns = 0;
#  foreach $fld (@{ $img_fields[$domain-1]})
   foreach $fld (@fields)  {

     $fnum++;
     $imgfile = sprintf("plot%04d.ps", $fnum);
     if ( $to_img eq "png" ) {
       $newimg = "$output_dir/$to_img/$valid_hour" . "/d" . $domain . "_" . $fld . ".png";
     } else {
       $newimg = "$output_dir/$to_img/$valid_hour" . "/d" . $domain . "_" . $fld . ".gif";
     }

     if ( -e "$imgfile" ) {
#
#     "convert" is an ImageMagick tool which is assumed to be in the user's $PATH
#     it is usually in /usr/bin
      $command = "convert -trim +repage -density $IMAGE_DENSITY $imgfile $newimg";
      print "$command\n";
      system("$command");
      $ns++;

     }else{
      $command = "cp $RIP_ROOT/not_yet_avail.gif $newimg";
      print "$command\n";
      system("$command");
     }

   }

   print "\nConverted $ns of $fnum images valid at $valid_hour for domain $domain\n";

   return $fnum;
}
#------------------------------------------------------------------------------#
#
#------------------------------------------------------------------------------#

sub ensemble_plots
{
my ($domain, $valid_time, $to_img, $plots_dir, $output_dir) = @_;

print "\n";
print 
"---------------------------------------------------------------------";
print "\n";
print "\nensemble_plots ($domain, $valid_time, $to_img, $plots_dir,$output_dir)\n";
print "\n";

# $to_img: 'png' or 'gif'

# set environment variables

$ENV{'NCARG_ROOT'} = $NCARG_ROOT;
$ENV{'NCARG_LIB'} = $NCARG_LIB;
$RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";
$ENV{'RIP_ROOT'} = "$RIP_ROOT";

# Goto workdir
if (! chdir "$plots_dir/gifs_ugui")
{
    print "ERROR: Cannot chdir to directory $plots_dir/gifs_ugui\n";
    return -1;
}

print "\nWorkdir is $plots_dir/gifs_ugui\n\n";

# Extract hour from valid time

$valid_hour = substr ($valid_time,8,2);

# Create output directory
system ("mkdir -p $output_dir/$to_img/$valid_hour");

if (! -d "$output_dir/$to_img/$valid_hour")
{
    print "ERROR: Cannot create directory $output_dir/$to_img/$valid_hour\n";
    return -1;
}

# Copy the missing images
$ns = 0;
$fnum= 0;

foreach $fld ("T_mean_std", "DIR_mean_std", "SPD_mean_std", "Wind_stddev", "U_mean_std", "V_mean_std", "RH_mean_std", "PBL_HGT_mean_std", "RAINRATE_mean_std", "T_min", "T_max", "T_minmax", "SPD_max", "RAINRATE_max", "sfctempe_prob1", "sfctempe_prob2", "sfctempe_prob3", "sfctempe_prob4", "sfcspeed_prob1", "sfcspeed_prob2", "sfcspeed_prob3", "sfcspeed_prob4", "sfcspeed_prob5", "rainrate_prob1", "rainrate_prob2", "rainrate_prob3", "map_rose", "rose_wind_00", "rose_wind_01", "rose_wind_02", "rose_wind_03", "rose_wind_04", "sfctempe_histo_00", "sfcspeed_histo_00", "sfcdir_histo_00", "dewtemp_histo_00", "surfhum_histo_00", "pblheight_histo_00", "rainrate_histo_00", "sfctempe_histo_01", "sfcspeed_histo_01", "sfcdir_histo_01", "dewtemp_histo_01", "surfhum_histo_01", "pblheight_histo_01", "rainrate_histo_01", "sfctempe_histo_02", "sfcspeed_histo_02", "sfcdir_histo_02", "dewtemp_histo_02", "surfhum_histo_02", "pblheight_histo_02", "rainrate_histo_02", "sfctempe_histo_03", "sfcspeed_histo_03", "sfcdir_histo_03", "dewtemp_histo_03", "surfhum_histo_03", "pblheight_histo_03", "rainrate_histo_03", "sfctempe_histo_04", "sfcspeed_histo_04", "sfcdir_histo_04", "dewtemp_histo_04", "surfhum_histo_04", "pblheight_histo_04", "rainrate_histo_04"){

        if ($to_img eq "png" ) {
            $newimg = "$output_dir/$to_img/$valid_hour" . "/d" . $domain . "_" . $fld . ".png";
        } else {
            $newimg = "$output_dir/$to_img/$valid_hour" . "/d" . $domain . "_" . $fld . ".gif";
        }

        $command = "cp -f $RIP_ROOT/not_yet_avail.gif $newimg";
        print "$command\n";
        system("$command");
        $ns ++;
        $fnum ++;
}

   print "\nConverted $ns of $fnum ensemble images valid at $valid_hour for domain $domain\n";

   return $fnum;
}

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
print "Usage: cfdda_APM.pl files=D1/wrfout_d01_\\*-09-\\*_00\\* \n";
print "                          <year=1901 month=09 day=15 hour=00>\n";
print "\n";
print "To postprocess all files matching pattern D1/wrfout_d01_*-09-*_00*\n";
print "And rename the statistics file with the date stamp 1901-09-15_00\n";
print "\n";
exit -1

}

1

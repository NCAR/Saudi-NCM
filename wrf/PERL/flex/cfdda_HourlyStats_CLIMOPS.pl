#! /usr/bin/perl
#------------------------------------------------------------------------------#
# Driver for post-processing CLIMOPS WRF ouput files.
#
#   cfdda_HourlyStats_CLIMOPS.pl files=D1/wrfout_d01_*-09-*_00* <year=1901 month=09 day=15 hour=00>
#
# To postprocess all files matching pattern D1/wrfout_d01_*-09-*_00*
# and rename the statistics file with the date stamp 1901-09-15_00
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
my ($west_east,$south_north,$bottom_top);
#my ($this_cycle, $NUM_DOMS,$OUTPUT_DIR,$FLEXINPUT);

#------------------------------------------------------------------------------#
# Parameters
#------------------------------------------------------------------------------#
#
# WRF variables for wich statistics must be computed

our $VARS_IN = "ZNW,ZNU,PB,SH2O,SMOIS,TSLB,TH2,XLAND,PSFC,V10,U10,Q2,T2,UDROFF,SFROFF,VEGFRA,GRDFLX,ALBEDO,SNOWC,SNOWH,SNOW,CANWAT,GLW,SWDOWN,UST,LH,HFX,QFX,PBLH,SST,LU_INDEX,TMN,F,MAPFAC_M,HGT,RAINNC,RAINC,TSK,P,W,QRAIN,QCLOUD,QVAPOR,T,V,U,PHB,PH,MUB,XLAT,XLONG,XLAT_U,XLONG_U,XLAT_V,XLONG_V,P_TOP,XTIME,Times,QICE,QSNOW,QGRAUP";

# Addition (diagnostic fields added in routine do_pre_stats)
$VARS_IN = $VARS_IN.","."SLP,RAINRATE,EDH,DUCT_SFC,REFMOD,CEILING,VISI_HORIZ,VISI_VERTI,VISI_SLANT,CT2,CN2,GTG,REFMOD_DZ,TC,RH,HEIGHT_MSL,ITFADEF";

# Or WRF variables for wich statistics must not be computed 

# our $VARS_OUT = "RDX,RDY,XTIME,LANDMASK,VEGFRA,MAPFAC_M,MAPFAC_U,MAPFAC_V,MAPFAC_M,MAPFAC_M,MAPFAC_UX,MAPFAC_UY,MAPFAC_VX,MF_VX_INV,MAPFAC_VY,F,E,SINALPHA,COSALPHA,HGT,HGT_SHAD,MAX_MSTFX,MAX_MSTFY,XLAT,XLONG,XLAT_U,XLONG_U,XLAT_V,XLONG_V,ITIMESTEP,IVGTYP,ISLTYP,"P_TOP","XTIME","Times";

# Path to NCL scripts (interactive only)

our $NCL_SCRIPTS = "$ENV{MM5HOME}/cycle_code/POSTPROCS/ncl";

our $EXECUTABLE_ARCHIVE = "$ENV{MM5HOME}/cycle_code/EXECUTABLE_ARCHIVE";

our $GSJOBDIR = $ENV{GSJOBDIR};

# Debug (more prints out when DEBUG = 1, interactive only)

our $DEBUG = 1;

#------------------------------------------------------------------------------#
# Check input
#------------------------------------------------------------------------------#

print "\n$0 @ARGV\n";
print "\nPID: $$\n";

if (($VARS_IN && $VARS_OUT) || (! $VARS_IN && ! $VARS_OUT )) {
    print "\n";
    print "ERROR: Need to decide between VARS_IN and VARS_OUT\n";
    print "\n";
    print "See header of information\n";
    print "\n";
    exit -1;
    $VARS = " ";
}else{

if ($VARS_IN) {
    $VARS = "-v $VARS_IN";
}else{
    $VARS = "-x -v $VARS_OUT";
}
}

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

    &do_pre_stats ($file_pattern,$ccyy,$mm,$dd,$hh);

    &do_stats ($file_pattern,$ccyy,$mm,$dd,$hh);

    &do_rip_plot ($file_pattern,$ccyy,$mm,$dd,$hh);

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

    print "RIP_IMG = $RIP_IMG, WEB_IMG = $WEB_IMG JOB_LOC = $JOB_LOC\n";
    # Redefine paths since MM5HOME have been reset in flexinput.pl
    $NCL_SCRIPTS = "$MM5HOME/cycle_code/POSTPROCS/ncl";
    $EXECUTABLE_ARCHIVE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE";

#------------------------------------------------------------------------------#
# Compute stats
#------------------------------------------------------------------------------#

    for ($dom = 1; $dom <= $NUM_DOMS; $dom++){

        # Workdir for statistics calculations
        $STATS_DIR = "$WRF_OUTPUT_DIR/STATS/D${dom}_$ccyy$mm$dd$hh";

        # Workdir for graphics
        $PLOTS_DIR = "$WRF_OUTPUT_DIR/RIP/D${dom}_$ccyy$mm$dd$hh";

        # Output directory
        $OUTPUT_DIR = "$JOB_LOC/Statistics";

        # File pattern is hardwired for hourly stats
        $file_pattern = "$WRF_OUTPUT_DIR/WRF/D$dom/wrfout_d0${dom}_\*-\*-\*_${hh}:00:00\*";

        # Add new diagnostic fields to the files
        $status = &do_pre_stats ($file_pattern,$ccyy,$mm,$dd,$hh,$STATS_DIR);

        if ($status < 0) {
            print "\n";
            print "ERROR in do_pre_stats, status = $status\n";
            exit -1
        }

        # Files have been renamed in do_pre_stats
        $file_pattern = "$STATS_DIR/wrfout_d0${dom}_\*-\*-\*_${hh}:00:00";

        # Compute Hourly Stats
        $status = &do_stats ($file_pattern,$ccyy,$mm,$dd,$hh,$STATS_DIR,$OUTPUT_DIR);
        if ($status < 0) {
            print "\n";
            print "ERROR in do_stats, status = $status\n";
            exit -1
        }

        # Plot
        $file_to_plot = "$STATS_DIR/avg_d0${dom}_${ccyy}-${mm}-${dd}_${hh}:00:00.nc";
        $status = &do_rip_plot ($file_to_plot,$RIP_IMG,$WEB_IMG,$PLOTS_DIR,$OUTPUT_DIR);

        if ($status < 0) {
            print "\n";
            print "ERROR in do_rip_plot, status = $status\n";
            exit -1
        }

        # Clean up
        if ($DEBUG < 1) {
            $command = "rm -rf $PLOTS_DIR $STATS_DIR";
            print "\n$command\n";
            system  ($command);
        }

#20100903        &ensemble_plots ($dom,"${ccyy}${mm}${dd}${hh}",$WEB_IMG,$PLOTS_DIR,$OUTPUT_DIR);
    }


 }


exit 0;

#------------------------------------------------------------------------------#
# Subroutines
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Sub do_pre_stats
# Add diagnostic fields to the file to process.
#------------------------------------------------------------------------------#

sub do_pre_stats{

use strict;

#Badri Aug 2012
our ($NCARG_ROOT, $NCOBIN);
$ENV{'NCARG_ROOT'} = $NCARG_ROOT;
$ENV{'NCOBIN'} = $NCOBIN;

my ($file_pattern,$year,$day,$month,$hour,$stats_dir);
my ($path_to_files,$wrf_files);
my ($dom, $strlen, $n, $N,$f, $ff);
my ($domain);
my ($in, $out);
my ($command);
my ($ccyymmddhh,$ccyy,$mm,$dd,$hh);
my ($wrf_before);
my (@MDL, @MDL1,@listf,@tmp);

$file_pattern = shift; 
$year  = shift;
$month = shift;
$day   = shift;
$hour  = shift;
$stats_dir = shift;

#------------------------------------------------------------------------------#
# Print arguments
#------------------------------------------------------------------------------#

print "\n";
print 
"---------------------------------------------------------------------";
print "\n";
print "do_pre_stats ($file_pattern,$year,$month,$day,$hour,$stats_dir)\n";
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

print "\nPath and WRF files pattern = $file_pattern\n";
#system ("ls -al $file_pattern");
print "\nPath to WRF files = $path_to_files\n";
#system ("ls -al $path_to_files*");
print "\nWRF files pattern = $wrf_files\n";

if (! -e $path_to_files){
    print "\n";
    print "ERRORe: Cannot find directory $path_to_files\n";
#   &help;
}
if (! -d $path_to_files){
    print "\n";
    print "ERRORd: Cannot find directory $path_to_files\n";
#   &help;
}

if (! chdir $path_to_files){
    print "\n";
    print "ERROR: Unable to go to directory $path_to_files\n";
    &help;
}

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
    print "ERROR cannot chdir to $stats_dir\n";
    return -1;
}

print "\nWorkdir is $stats_dir\n\n";

#------------------------------------------------------------------------------#
# Link NCL scripts into local directory
#------------------------------------------------------------------------------#

for $f ("add_ceiling.ncl","add_duct_sfc.ncl","add_edh.ncl","add_gtg.ncl","add_rainrate.ncl","add_refmod_dz.ncl","add_htrhslp.ncl","add_times.ncl","add_vars_climops.ncl","add_visi_horiz.ncl","add_visi_slant.ncl","add_visi_verti.ncl","change_Times.ncl","print_wrf_dims.ncl","f_betacalc.ncl","f_duct_sfc_discrete.ncl","f_edh_analytic.ncl","f_edh_discrete.ncl","f_FillValue.ncl","f_refmod.ncl","f_refract.ncl")
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
# Bring the static parameter files
#------------------------------------------------------------------------------#

$f = "static_thresholds.dat";

if (! -e "$GSJOBDIR/static_thresholds.dat"){
    print "\n";
    print "ERROR cannot parameter file $GSJOBDIR/static_thresholds.dat\n";
    return -1;
}

#unlink $f if (-e $f); 
$command = "cp -f $GSJOBDIR/$f .";
print  "$command\n";
system ($command);

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
     # Bring the file locally
     #-------------------------------------------------------------------------#

     unlink $f if (-e $f);

     $command =  "ln -sf $path_to_files/$ff $f";  
#    $command =  "cp -f  $path_to_files/$ff $f";  
     print  "$command\n" if ($DEBUG > 0);
     system ($command);

     next if (! -e $f);

     #-------------------------------------------------------------------------#
     # Find the wrf dimensions (needs to do it only once)
     #-------------------------------------------------------------------------#

     if ($west_east <= 0 || $south_north <= 0 || $bottom_top <= 0){
        ($west_east,$south_north,$bottom_top) = &wrf_dims ($f);
     }

     #-------------------------------------------------------------------------#
     # Compute ITFA (GTG) index
     #-------------------------------------------------------------------------#

     if ($west_east > 0 && $south_north > 0 && $bottom_top > 0){

         mkdir  "./dirout" if (! -d "./dirout");

         unlink "./dirout/$f" if (-e "./dirout/$f");

         $command = "$EXECUTABLE_ARCHIVE/itfa.exe $domain $west_east $south_north $bottom_top 0 static_thresholds.dat . $f both dirout >& itfa.out"; 
         print  "\n-------------------------------------------------------------\n";

         print  "\n$command\n\n";
         system ($command);

         if (-e  "./dirout/$f") {
             unlink $f;
             $command = "mv -f ./dirout/$f .";
             print  "$command\n" if ($DEBUG > 0);
             system ($command);
         }

         rmdir "./dirout" if (-e "./dirout");

      }else{

         unlink "$f" if (-e "$f");
         if (! -e  "$f") {
             $command =  "cp -f  $path_to_files/$ff $f";  
             print    "$command\n" if ($DEBUG > 0);
             system ($command);
         }
         # Add the IFTA field with FillValue

      }

     #-------------------------------------------------------------------------#
     # Find file valid an hour earlier
     #-------------------------------------------------------------------------#

     $ccyymmddhh = substr($f,11,13);

     $ccyy = substr($ccyymmddhh,0,4);
     $mm = substr($ccyymmddhh,5,2);
     $dd = substr($ccyymmddhh,8,2);
     $hh = substr($ccyymmddhh,11,2);

     $ccyymmddhh = "$ccyy$mm$dd$hh";

     $ccyymmddhh = &hh_advan_date ($ccyymmddhh,-1);

     $ccyy = substr($ccyymmddhh,0,4);
     $mm = substr($ccyymmddhh,4,2);
     $dd = substr($ccyymmddhh,6,2);
     $hh = substr($ccyymmddhh,8,2);

     $wrf_before = $ff;

     substr($wrf_before,11,4) = $ccyy;
     substr($wrf_before,16,2) = $mm;
     substr($wrf_before,19,2) = $dd;
     substr($wrf_before,22,2) = $hh;

     #-------------------------------------------------------------------------#
     # 6.2 Compute and add rain rate
     #-------------------------------------------------------------------------#

     $wrf_before = "$path_to_files/$wrf_before";

     $command = "${NCARG_ROOT}/bin/ncl \'file_now=\"$f\"\' ";

     if (-e "$wrf_before") {
         $command = $command."\'file_before=\"$wrf_before\"\' 'time_inc_hour=1' add_rainrate.ncl";
     }else{
         print "\nWARNING: cannot find previous hour file $wrf_before\n";
         print "\nRaine rate will be filled with missing value in file: $f\n";

         $command = $command."add_rainrate.ncl";
     }

     print "\n-------------------------------------------------------------\n";
     print "\n$command\n\n";
     system  ($command);

     #-------------------------------------------------------------------------#
     # 6.3 Compute SLP, TC, RH, HEIGHT_MSL
     #-------------------------------------------------------------------------#

     $command = "${NCARG_ROOT}/bin/ncl \'file_in=\"$f\"\' add_htrhslp.ncl";

     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);

     #-------------------------------------------------------------------------#
     # 6.4 Compute modified refractivity and its vertical gradient
     #-------------------------------------------------------------------------#

#    $command = "ncl \'file_in=\"$f\"\' add_refmod_dz.ncl";

#    print  "\n-------------------------------------------------------------\n";
#    print  "\n$command\n\n";
#    system ($command);

     #-------------------------------------------------------------------------#
     # 6.5 Compute EDH
     #-------------------------------------------------------------------------#

     $command = "${NCARG_ROOT}/bin/ncl \'file_in=\"$f\"\' add_edh.ncl";

     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);

     #-------------------------------------------------------------------------#
     # 6.6 Compute surface duct layer height
     #-------------------------------------------------------------------------#

     $command = "${NCARG_ROOT}/bin/ncl \'file_in=\"$f\"\' add_duct_sfc.ncl";

     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);


     #-------------------------------------------------------------------------#
     # 6.7 Compute CEILING
     #-------------------------------------------------------------------------#

     $command = "${NCARG_ROOT}/bin/ncl \'file_in=\"$f\"\' add_ceiling.ncl";

     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);

     #-------------------------------------------------------------------------#
     # 6.8 Compute VISI_HORIZ at surface (flightlevel = 0)
     #-------------------------------------------------------------------------#

     $command = "${NCARG_ROOT}/bin/ncl \'file_in=\"$f\"\' \'flightlevel=0\' add_visi_horiz.ncl"; 

     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);

     #-------------------------------------------------------------------------#
     # 6.9 Compute VIZ_VERTI
     #-------------------------------------------------------------------------#

     $command = "${NCARG_ROOT}/bin/ncl \'file_in=\"$f\"\' \'flightlevel=15000\' add_visi_verti.ncl"; 

     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);

     #-------------------------------------------------------------------------#
     # 6.10 Compute VIZ_SLANT
     #-------------------------------------------------------------------------#

     $command = "${NCARG_ROOT}/bin/ncl \'file_in=\"$f\"\' \'flightlevel=21000\' add_visi_slant.ncl"; 

     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);

     #-------------------------------------------------------------------------#
     # 6.11 Compute CT2, CN2 REFMOD, REFMOD_DZ and GTG
     #-------------------------------------------------------------------------#

     $command = "$EXECUTABLE_ARCHIVE/wrf_ref_cn2.exe $f"; 
     print  "\n-------------------------------------------------------------\n";
     print  "\n$command\n\n";
     system ($command);

     #-------------------------------------------------------------------------#
     # 6.12 Compute ITFA
     #-------------------------------------------------------------------------#

#    mkdir "dirout";
#    $command = "$EXECUTABLE_ARCHIVE/itfa.exe 3 66 66 36 0 static_thresholds_arwrf_hightestfit3.dat . $f netcdf dirout"; 
#    print  "\n-------------------------------------------------------------\n";
#    print  "\n$command\n\n";
#    system ($command);


#    $command = "ncl \'file_in=\"$f\"\' add_gtg.ncl";

#    print  "\n-------------------------------------------------------------\n";
#    print  "\n$command\n\n";
#    system ($command);


     $n ++;
}


$N = $n - 1;

print "\n-------------------------------------------------------------\n";
print "\nProcessed $N files with file name $file_pattern\n";

#-------------------------------------------------------------------------------
# End
#-------------------------------------------------------------------------------

print "\n";
return $N;

}


#------------------------------------------------------------------------------#
# Sub: do_stats
# Compute mean, max, min and stdv with NCO operators
#------------------------------------------------------------------------------#

sub do_stats{

use strict;

#Badri Aug 2012
our ($NCARG_ROOT, $NCOBIN);
$ENV{'NCARG_ROOT'} = $NCARG_ROOT;
$ENV{'NCOBIN'} = $NCOBIN;

my ($file_pattern,$year,$day,$month,$hour,$stats_dir,$output_dir);
my ($path_to_files,$wrf_files);
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
$stats_dir = shift;
$output_dir = shift;

#------------------------------------------------------------------------------#
# Print arguments
#------------------------------------------------------------------------------#

print "\n";
print 
"---------------------------------------------------------------------";
print "\n";
print "do_stats ($file_pattern,$year,$month,$day,$hour,$stats_dir,$output_dir)\n";
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
    print "ERROR cannot chdir to $stats_dir\n";
    return -1;
}

print "\nWorkdir is $stats_dir\n\n";

#------------------------------------------------------------------------------#
# Link NCL scripts into local directory
#------------------------------------------------------------------------------#

for $f ("add_vars_climops","f_FillValue","change_Times"){

        if (! -e "$NCL_SCRIPTS/$f.ncl"){
            print "\n";
            print "ERROR cannot find script $NCL_SCRIPTS/$f.ncl\n";
            return -1;
         }
           
         symlink ("$NCL_SCRIPTS/$f.ncl","$f.ncl");
}

#------------------------------------------------------------------------------#
# Open an ASCII file to store the list of files used
#------------------------------------------------------------------------------#

$out = "list_files.txt";
if (-e $out) { unlink ($out); }

print "\nKeep list of WRF files in file $out\n\n" if ($DEBUG > 0);

open (OU, ">$out") || die "Cannot open file $out: $!" ;

#------------------------------------------------------------------------------#
# Loop over the list of files
#------------------------------------------------------------------------------#

$n = 1;
foreach $f (@MDL) {

     chomp ($f);

     #-------------------------------------------------------------------------#
     # Skip if the file does not exist
     #-------------------------------------------------------------------------#

     next if (! -e "$path_to_files/$f");

     #-------------------------------------------------------------------------#
     # Store the name of the file being processed in a list file
     #-------------------------------------------------------------------------#

     if (-e "$path_to_files/$f") {
         print OU "$path_to_files/$f\n";
     }

     #-------------------------------------------------------------------------#
     # Link file to generic name eg 000001.nc, 000002.nc for application of NCO
     #-------------------------------------------------------------------------#

     $ff = sprintf ("link_%${strlen}.${strlen}d",$n);
   
     unlink "$ff.nc" if (-e "$ff.nc");

     $command =  "ln -sf $path_to_files/$f $ff.nc";  
     print  "$command\n" if ($DEBUG > 0);
     system ($command);

     $n ++;
}


$N = $n - 1;
print "\nFound $N files with file name $file_pattern\n";

#------------------------------------------------------------------------------#
# Mean calculations
#------------------------------------------------------------------------------#

print  "\n-------------------------------------------------------------\n";

# Input and Output file names

$in  = sprintf ("link_%${strlen}.${strlen}d.nc",1);
$out = "avg.nc";

if (-e $out) { unlink ($out); }

# Apply NCO operators

print "\nCompute mean of $N files in $out\n";

$command = "$NCOBIN/bin/ncea -O $VARS -n $N,$strlen,1 $in -o $out";

print "\n$command\n";
system  ($command);

#------------------------------------------------------------------------------#
# Minima calculations
#------------------------------------------------------------------------------#

print  "\n-------------------------------------------------------------\n";

# Input and Output file names

$in  = sprintf ("link_%${strlen}.${strlen}d.nc",1);
$out = "min.nc";

if (-e $out) { unlink ($out); }

# Apply NCO operators

print "\nCompute minima of $N files in $out\n";

$command = "$NCOBIN/bin/ncea -O $VARS -y min -n $N,$strlen,1 $in -o $out";
print  "\n$command\n";
system "$command";

#------------------------------------------------------------------------------#
# Maxima calculations
#------------------------------------------------------------------------------#

print  "\n-------------------------------------------------------------\n";

# Input and Output file names

$in  = sprintf ("link_%${strlen}.${strlen}d.nc",1);
$out = "max.nc";

if (-e $out) { unlink ($out); }

# Compute the max with an nco operator

print "\nCompute maxima of $N files in $out\n";

$command = "$NCOBIN/bin/ncea -O $VARS -y max -n $N,$strlen,1 $in -o $out";
print  "\n$command\n";
system "$command";

# Delete intermediate files

unlink <link_*.nc>;

#------------------------------------------------------------------------------#
# Prepare for standard deviation calculations
#------------------------------------------------------------------------------#

print  "\n-------------------------------------------------------------\n";

# Get list of non-missing files

if (-e "list_files.txt") {
	open (LISTEF,"list_files.txt")|| die "Cannot open list_files.txt for reading: $!";
	while (<LISTEF>) {
		chomp ($_);
		push(@listf, $_);
	}
	close (LISTEF) || die "Cannot close list_files.txt: $!";

}else{
	die "Cannot find list_files.txt for reading: $!";
}

# Loop over the files to compute difference between each file and the mean

print "\nCompute differences between each of the $N files and the mean\n";

$n = 1;
foreach $f (@listf) {
	# Get rid of the \n character
	chomp ($f);
	
	# Define output files names
	$ff = sprintf ("SUB_%${strlen}.${strlen}d.nc",$n);

	# Process the difference between this file and the mean
        $command = "$NCOBIN/bin/ncbo -O $VARS --op_typ=sub avg.nc $f -o $ff";

        print "\n$command\n" if ($DEBUG > 0 || $n == 1);
	system ($command);
	
	# Increment loop
	$n ++;
	
}

# Number of files processed

$N = $n - 1;

#------------------------------------------------------------------------------#
# Standard deviation calculations
#------------------------------------------------------------------------------#

print  "\n-------------------------------------------------------------\n";

$in  = sprintf ("SUB_%${strlen}.${strlen}d.nc",1);
$out = "std.nc"; 

if (-e $out) { unlink ($out); }

# Call nco operator

print "\nCompute standard deviation of $N files in $out\n";

$command = "$NCOBIN/bin/ncea -O $VARS -y rmssdn -n $N,$strlen,1 $in -o $out";
print "\n$command\n";
system  ($command);

#------------------------------------------------------------------------------#
# Delete intermediate files
#------------------------------------------------------------------------------#

unlink <SUB_*.nc>;
unlink <*.tmp>;
unlink <list_files.txt>;

#------------------------------------------------------------------------------#
# Rename stats file with the requested date stamp
#------------------------------------------------------------------------------#

print  "\n-------------------------------------------------------------\n";
print "\nChanging time stamp\n";

#------------------------------------------------------------------------------#
# Change variable Times & time in stats file
#------------------------------------------------------------------------------#

@tmp = ("avg","min","max","std") ;

foreach $f (@tmp){

   if (! -e "$f.nc") {
       print "Cannot find file $f, skipping...\n";
       next;
   }

   $command = "${NCARG_ROOT}/bin/ncl \'file_in=\"$f.nc\"\'";

   if (defined ($year)) {
       $command = $command." \'year=\"$year\"\'";
   }
   if (defined ($month)) {
       $command = $command." \'month=\"$month\"\'";
   }
   if (defined ($day)) {
       $command = $command." \'month=\"$day\"\'";
   }
   if (defined ($hour)) {
       $command = $command." \'month=\"$hour\"\'";
   }

   $command = $command." $NCL_SCRIPTS/change_Times.ncl";
#  $command = $command." $NCL_SCRIPTS/add_vars_climops.ncl";


print  "\n-------------------------------------------------------------\n";
print  "\n$command\n\n";
system ($command);
last;
}

undef @tmp;

#------------------------------------------------------------------------------#
# Renaming files
#------------------------------------------------------------------------------#

if (defined ($domain) && defined ($year) && defined ($month) && defined ($day) && defined ($hour)) {

    if (! -d "$output_dir/nc") {
         $command = "mkdir -p $output_dir/nc";
         print "\n$command\n";
         system  ($command);
    }

    print  "\n-------------------------------------------------------------\n";

    @tmp = ("avg","min","max","std") ;

    foreach $f (@tmp){

        if (! -e "${f}.nc") {
            print "Cannot find file ${f}.nc, skipping...\n";
            next;
         }

         # Link to local file with time stamp
         $command = "ln -sf ${f}.nc ${f}_d0${domain}_${year}-${month}-${day}_${hour}:00:00.nc";
         print  "\n$command\n";
         system ($command);

         # Copy to output dir, drop minutes and seconds

         if (-d "$output_dir") {
             $command = "cp -f ${f}.nc $output_dir/nc/${f}_d0${domain}_${year}-${month}-${day}_${hour}.nc";
             print  "$command\n";
             system ($command);
         }else{
             print "ERROR: Cannot find directory $output_dir/nc\n";
         } 

    }

undef @tmp;

}

#-------------------------------------------------------------------------------
# End
#-------------------------------------------------------------------------------

print "\n";
return $N;

}

#-------------------------------------------------------------------------------
# Sub plot_rip
#-------------------------------------------------------------------------------

sub do_rip_plot {

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
    system ("mkdir -p $plots_dir") 
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
      $command = "cp $RIP_ROOT/not_yet_avail.pn $newimg";
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

        $command = "cp -f $RIP_ROOT/not_yet_avail.png $newimg";
        print "$command\n";
        system("$command");
        $ns ++;
        $fnum ++;
}

   print "\nConverted $ns of $fnum ensemble images valid at $valid_hour for domain $domain\n";

   return $fnum;
}

#------------------------------------------------------------------------------#
# Sub wrf_dims: Find the dimensions of a WRF output file
# Use script print_wrf_dims.ncl
# return west_east,south_north,bottom_top dimensions
#------------------------------------------------------------------------------#

sub wrf_dims
{
    use strict;
#Badri Aug 2012
our ($NCARG_ROOT, $NCOBIN);
$ENV{'NCARG_ROOT'} = $NCARG_ROOT;
$ENV{'NCOBIN'} = $NCOBIN;

    my ($file_in);
    my ($west_east,$south_north,$bottom_top);
    my ($command);
    my (@tmp);

    $file_in = shift;
    $west_east   = 0;
    $south_north = 0;
    $bottom_top  = 0;

    $command = "${NCARG_ROOT}/bin/ncl \'file_in=\"$file_in\"\' \'file_ou=\"wrf_dims.txt\"\' print_wrf_dims.ncl";

    print "\n-------------------------------------------------------------\n";
    print "\n$command\n\n";
    system  ($command);


    if (-e "wrf_dims.txt" && ! -z "wrf_dims.txt") {
        open (WRF_DIMS,"wrf_dims.txt")|| die "Cannot open wrf_dims.txt for reading: $!";
        while (<WRF_DIMS>) {
               chomp ($_);
               @tmp = split (' ');
        }
        close (WRF_DIMS) || die "Cannot close wrf_dims.txt: $!";

        if ($#tmp >= 2){ 
            $west_east   = $tmp[0];
            $south_north = $tmp[1];
            $bottom_top  = $tmp[2];
        }

        return ($west_east,$south_north,$bottom_top);
    }

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
print "Usage: cfdda_HourlyStats.pl files=D1/wrfout_d01_\\*-09-\\*_00\\* \n";
print "                              <year=1901 month=09 day=15 hour=00>\n";
print "\n";
print "To postprocess all files matching pattern D1/wrfout_d01_*-09-*_00*\n";
print "And rename the statistics file with the date stamp 1901-09-15_00\n";
print "\n";
exit -1

}

1

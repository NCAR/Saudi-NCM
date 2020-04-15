#! /usr/bin/perl
#------------------------------------------------------------------------------#
# Driver for generating native MEDOC files from GCAT WRF ouput files.
#
#   cfdda_MEDOC_GCAT.pl StartTime=2013080413 EndTime=2013080512 Domain=1 FLEXINPUT=pre_process_in_donotedit.pl
#
# To generate a single MEDOC files from WRF output file valid between
# StartTime and EndTime for the GCAT job defined in pre_process_in_donotedit.pl
#
#------------------------------------------------------------------------------#
# Copyright UCAR (c) 2010
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Laboratory (RAL),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA. 
#
# Francois Vandenberghe, vandenb@ucar.edu, August 2013.
#------------------------------------------------------------------------------#

use strict;
require "stat.pl";
require "flush.pl";
require "ctime.pl";

use Time::Local;

#------------------------------------------------------------------------------#
# Local variables
#------------------------------------------------------------------------------#

# temporary
my ($n,$dom);
my ($dummy1,$dummy2,$command,$status);
my ($f,$file_from,$file_to);
my (@tmp);

# Arguments/environment (torque)
my ($StartTime,$EndTime,$Domain,$FLEXINPUT);

# Paths built from FLEXINPUT
my ($POSTPROCINPUT);

#------------------------------------------------------------------------------#
# Global variables
#------------------------------------------------------------------------------#

# Define here the type of graphics (pdf,ps or png)
our $GRAPH_TYPE = "pdf";

# Paths read in FLEXINPUT
our ($WRF_OUTPUT_DIR,$GSJOBDIR,$MM5HOME,$NCARG_ROOT,$NCOBIN,$GSJOBID,$GSJOBTYPE,$outJobID);

# Variables read in FLEXINPUT
our ($CLEANDIR,$DEBUG,$RANGE);

# Path for medoc and images read from POSTPROCINPUT
our ($JOB_LOC);

# Other paths built from MM5HOME
#our ($EXECUTABLE_ARCHIVE,$CSH_ARCHIVE,$NCL_SCRIPTS);

our ($GMID);

#------------------------------------------------------------------------------#
# Check input
#------------------------------------------------------------------------------#

print "\n$0 @ARGV\n";
print "\nPID: $$\n\n";

#------------------------------------------------------------------------------#
# Interactive mode: parse arguments
#------------------------------------------------------------------------------#

if (! $ENV{PBS_JOBID}) {

 if ($#ARGV >= 0) {

   for ($n = 0; $n <= $#ARGV; $n++) {

        if (index ($ARGV[$n],"=") < 0) {
            print "\n";
            print "ERROR: bad argument $ARGV[$n].\n";
            &help;
        }

        ($dummy1,$dummy2) = split('=',$ARGV[$n]);

# Expect keywords: files, year, month, day, hour

        if ($dummy1 eq 'StartTime') {
            $StartTime = $dummy2 ;
        }
        if ($dummy1 eq 'EndTime') {
            $EndTime = $dummy2 ;
        }
        if ($dummy1 eq 'Domain') {
            $Domain = $dummy2 ;
        }
        if ($dummy1 eq 'FLEXINPUT') {
            $FLEXINPUT = $dummy2 ;
        }
   }

  }else{
       &help;
  }

  if (! defined ($StartTime)) {
      print "\n";
      print "ERROR: Need a starting time CCYYMMDDHH after keyword 'StartTime='\n";
      &help;
  }

  if (! defined ($EndTime)) {
      print "\n";
      print "ERROR: Need a ending time CCYYMMDDHH after keyword 'EndTime='\n";
      &help;
  }

  if (! defined ($Domain)) {
      print "\n";
      print "ERROR: Need a WRF domain id after keyword 'Domain='\n";
      &help;
  }

  if (! defined ($FLEXINPUT)) {
      print "\n";
      print "ERROR: Need a the path to file after keyword 'FLEXINPUT='\n";
      &help;
  }

#------------------------------------------------------------------------------#
# Or batch mode: environment variable flexinput must be defined
#------------------------------------------------------------------------------#

} else{

    if (! $ENV{StartTime})
    {
        print "\n";
        print "ERROR: variable StartTime is not defined.\n";
        &help;
    }

    $StartTime = $ENV{StartTime};

    if (! $ENV{EndTime})
    {
        print "\n";
        print "ERROR: variable EndTime is not defined.\n";
        &help;
    }

    $EndTime = $ENV{EndTime};

    if (! $ENV{Domain})
    {
        print "\n";
        print "ERROR: variable Domain is not defined.\n";
        &help;
    }

    $Domain = $ENV{Domain};

    if (! $ENV{FLEXINPUT})
    {
        print "\n";
        print "ERROR: Variable environment variable FLEXINPUT must be set\n";
        &help;
    }

    $FLEXINPUT = $ENV{FLEXINPUT};

 }

#------------------------------------------------------------------------------#
# Print-out and load environment from file FLEXINPUT
#------------------------------------------------------------------------------#

    print "StartTime  = $StartTime\n";

    print "EndTime    = $EndTime\n";

    print "Domain     = $Domain\n";

    print "FLEXINPUT  = $FLEXINPUT\n";

    if (! -e $FLEXINPUT)
    {
        print "\n";
        print "ERROR: File $FLEXINPUT does not exist.\n";
        &help;
    }

    # This defines the environment (GSJOBDIR, MM5HOME)
    print "requite $FLEXINPUT\n";
    require "$FLEXINPUT";
    print "\noutJobID = $outJobID\n";
#------------------------------------------------------------------------------#
# Load additional environment variables from file POSTPROCINPUT
#------------------------------------------------------------------------------#

    # Read path to output directory from postprocinput.pl
    $POSTPROCINPUT = "$GSJOBDIR/medocinput.pl";

    print "\nPOSTPROCINPUT  = $POSTPROCINPUT\n";

    if (! -e $POSTPROCINPUT)
    {
        print "\n";
        print "ERROR: Cannot find file $POSTPROCINPUT.\n";
        &help;
    }

    print "require $POSTPROCINPUT\n";
    require $POSTPROCINPUT;
    print "\noutJobID = $outJobID\n";

#------------------------------------------------------------------------------#
# Extract the GMID from GSJOBDIR (expect GSJOBDIR = <path>/GMID/Medoc)
#------------------------------------------------------------------------------#

    @tmp = split ('/',$GSJOBDIR); 

    if ($#tmp < 1){
        print "\n";
        print "ERROR WRF_OUTPUT_DIR is empty\n";
        exit -1
    }

    $GMID = $tmp[$#tmp-1]; 
    
#------------------------------------------------------------------------------#
# Create output directory whose path was read in POSTPROCINPUT
# and copy a few config files into the output directory
#------------------------------------------------------------------------------#

    $status = &makeDirOutput;

    if ($status < 0) {
        print "\n";
         print "ERROR makeDirOutput, status = $status\n";
         exit -1
     }

#------------------------------------------------------------------------------#
# Loop over domains
#------------------------------------------------------------------------------#

#   for ($dom = 1; $dom <= $NUM_DOMS; $dom++){
    $dom = $Domain;

#------------------------------------------------------------------------------#
# Generate MEDOC
#------------------------------------------------------------------------------#

        $status = 0;

        # Generate MEDOC files
        $status = &wrf2nativemedoc ($StartTime,$EndTime,$dom);#,$WRF_OUTPUT_DIR,$JOB_LOC);
        if ($status < 0) {
            print "\n";
            print "ERROR wrf2nativemedoc, status = $status\n";
            exit -1
        }

#------------------------------------------------------------------------------#
# Generate images from WRF 
#------------------------------------------------------------------------------#

        $status = 0;

        # Generate images from WRF output
        $status = &do_2d_wrf_graphics ($StartTime,$EndTime,$dom);#,$WRF_OUTPUT_DIR,$JOB_LOC);

        if ($status < 0) {
            print "\n";
            print "ERROR in do_2d_wrf_graphics, status = $status\n";
            exit -1;
        }

#------------------------------------------------------------------------------#
# Generate images from OBS 
#------------------------------------------------------------------------------#

        if ($GSJOBTYPE ne "forecast" && $GSJOBTYPE ne "Forecast" && $GSJOBTYPE ne "FORECAST"){  

            $status = 0;

            # Generate images from the observations
            $status = &do_2d_obs_graphics ($StartTime,$EndTime,$dom);#,$WRF_OUTPUT_DIR,$JOB_LOC);

            if ($status < 0) {
                print "\n";
                print "ERROR in do_2d_obs_graphics, status = $status\n";
                exit -1;
            }

        }

exit 0;

#------------------------------------------------------------------------------#
# Subroutines
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Sub wrf2nativemedoc
# Add diagnostic fields to the file to process.
#------------------------------------------------------------------------------#

sub wrf2nativemedoc
{

use strict;

#------------------------------------------------------------------------------#
# Global variables
#------------------------------------------------------------------------------#

our ($WRF_OUTPUT_DIR,$JOB_LOC);
our ($GSJOBDIR);

#------------------------------------------------------------------------------#
# Local variables
#------------------------------------------------------------------------------#

my ($path_to_files,$wrf_file,$medoc_dir,$fout);
my ($dom, $strlen, $n, $d, $f, $ff, $fff);
my ($command,$stats);
my ($ccyymmddhh,$ccyy,$mm,$dd,$hh,$mn,$ss);
my ($nwrf);
my (@MDL,@MDL1,@tmp);

my $output_dir_wrf = $WRF_OUTPUT_DIR;
my $output_dir_medoc = $JOB_LOC;

my $EXECUTABLE_ARCHIVE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE";
my $NCL_SCRIPTS = "$MM5HOME/cycle_code/POSTPROCS/ncl";


#------------------------------------------------------------------------------#
# Arguments
#------------------------------------------------------------------------------#

my $StartTime = shift; 
my $EndTime  = shift;
my $dom = shift;

#------------------------------------------------------------------------------#
# Print arguments
#------------------------------------------------------------------------------#

print "\n";
print 
"-----------------------------------------------------------------------------";
print "\n";
print "wrf2nativemedoc ($StartTime,$EndTime,$dom,$output_dir_wrf,$output_dir_medoc)\n";


#------------------------------------------------------------------------------#
# Find path to WRF files
#------------------------------------------------------------------------------#

$path_to_files = "$WRF_OUTPUT_DIR/WRF/D$dom";

if (! -e $path_to_files){
    print "\n";
    print "ERROR: Cannot find $path_to_files\n";
#   &help;
}

if (! -d $path_to_files){
    print "\n";
    print "ERROR: not a directory $path_to_files\n";
#   &help;
}

print "\n";
print "Converting into MEDOC format WRF files in directory $path_to_files\n";

$stats = 0;

#------------------------------------------------------------------------------#
# List all the WRF files that are needed for the conversion
#------------------------------------------------------------------------------#

$ccyymmddhh = $StartTime;
$n = 0;

while ($ccyymmddhh <= $EndTime){

       $ccyy = substr($ccyymmddhh,0,4);
       $mm = substr($ccyymmddhh,4,2);
       $dd = substr($ccyymmddhh,6,2);
       $hh = substr($ccyymmddhh,8,2);
       $mn = "00";
       $ss = "00";

       $wrf_file = "$path_to_files/wrfout_d0${dom}_${ccyy}-${mm}-${dd}_${hh}:${mn}:00.${RANGE}_F" ;

       if (! -e $wrf_file){
           print "\n";
           print "ERROR in wrf2nativemedoc: Cannot find file $wrf_file\n";
           return -1;
       }else{
           $MDL1[$n] = $wrf_file;
           $n++;
       }

       $ccyymmddhh = &hh_advan_date ($ccyymmddhh,+1);
}

# Print the list of files
if ($#MDL1 < 0) {
    print "\n";
    print "ERROR: Cannot find any WRF file for domain $dom valid between $StartTime and $EndTime\n in directory $path_to_files\n";
    return -1;
}else{
     @MDL = sort @MDL1;
     print "\n";
     print "Found $n files for domain $dom valid between $StartTime and $EndTime\n";
     foreach $f (@MDL){
        print "$f\n";
     }

}

#------------------------------------------------------------------------------#
# Move to the work dir
#------------------------------------------------------------------------------#

$medoc_dir = "$output_dir_wrf/MEDOC/D${dom}";

if (! -e $medoc_dir){  
    system ("rm -rf $medoc_dir") 
}

if (! -d $medoc_dir){  
    system ("mkdir -p $medoc_dir") 
}

if (! chdir $medoc_dir){  
    print "\n";
    print "ERROR: cannot chdir to $medoc_dir\n";
    return -1;
}

print "\n";
print "Workdir is $medoc_dir\n";
print "\n";

#------------------------------------------------------------------------------#
# Link executables
#------------------------------------------------------------------------------#

for $f ("MEDOCconverter.exe","latlon_wrf.exe")
{
        if (! -e "$EXECUTABLE_ARCHIVE/$f"){
            print "\n";
            print "ERROR: cannot find file $EXECUTABLE_ARCHIVE/$f\n";
            return -1;
         }
         unlink $f if (-e $f); 
         symlink ("$EXECUTABLE_ARCHIVE/$f","$f");
}

#------------------------------------------------------------------------------#
# Loop over the list of files
#------------------------------------------------------------------------------#

print "\n";
print
"-----------------------------------------------------------------------------";
print "\n";

$f = "infile_names";
system ("rm -f $f") if (-e $f); 

if (! open (IN,">$f")){
    print "\n";
    print "ERROR: Cannot open file $f for writing\n"; 
    return -1;
}

$n = 1;

foreach $fff (@MDL) {

     chomp ($fff);


     #-------------------------------------------------------------------------#
     # Check again if the file exists
     #-------------------------------------------------------------------------#

     if (! -e "$fff"){
         print "\n";
         print "ERROR: Cannot find file $fff\n";
         return -1;
     }

     #-------------------------------------------------------------------------#
     # Get rid of the GRM suffixes
     #-------------------------------------------------------------------------#

     if ($fff =~ /GRM_F/) {
         $ff = substr ($fff,0,length($fff)-6);
     }else{
         if ($fff =~ /GRM_P+FCST/) {
             $ff = substr ($fff,0,length($fff)-11);
         }else{
             $ff = $fff;
         }
     }

     #-------------------------------------------------------------------------#
     # Bring the file locally
     #-------------------------------------------------------------------------#

     @tmp = split ('/',$ff);
     $f = $tmp[$#tmp];

     system ("rm -f $f") if (-e $f);

     $command =  "ln -sf $fff $f";  
#    $command =  "cp -f  $fff $f";  
     print  "$command\n" if ($DEBUG > 1);
     system ($command);

     if (! -e $f){
         print "\n";
         print "ERROR: Cannot find file $f (linked to $fff)\n";
         return -1;
     }


     #-------------------------------------------------------------------------#
     # Store the name of the file
     #-------------------------------------------------------------------------#

     print IN "$f\n";

     #-------------------------------------------------------------------------#
     # Increment file counter
     #-------------------------------------------------------------------------#

     $n ++;
}

close (IN);

$nwrf = $n - 1;

print "\n";
print "Found $nwrf files for domain $dom valid between $StartTime end $EndTime\n";

#-------------------------------------------------------------------------------
# Find the wrf dimensions (needs to do it only once)
#-------------------------------------------------------------------------------

#if ($west_east <= 0 || $south_north <= 0 || $bottom_top <= 0){
#   ($west_east,$south_north,$bottom_top) = &wrf_dims ($f);
#}
#------------------------------------------------------------------------------#
# Generate the config file for the MEDOC converter
#------------------------------------------------------------------------------#

&medoc_cfg($MDL[0]);

#-------------------------------------------------------------------------------
# Convert into MEDOC format
#-------------------------------------------------------------------------------
print "\n";
print
"---------------------------------------------------------------------------\n";
print "MEDOC conversion:\n";
print "\n";


#$fout = "D${dom}.${StartTime}-${EndTime}.medoc";
$fout = "$GSJOBTYPE"."$outJobID"."_D${dom}_medoc.fmt";

system ("rm -f $fout") if (-e $fout);

$command = "./MEDOCconverter.exe $fout 0 $nwrf 1";
print  "$command\n";
system ($command);

if (! -e $fout){
    print "\n";
    print "ERROR: MEDOC file $medoc_dir/$fout was not created\n";
    $stats = -1;
    return $stats;
}else{
    print "\n";
    print "MEDOC file $medoc_dir/$fout was created\n";
    print "\n";
}

print "\n";
print
"---------------------------------------------------------------------------\n";
print "\n";

#-------------------------------------------------------------------------------
# Move MEDOC file to where is expected
#-------------------------------------------------------------------------------

$d = "$output_dir_medoc";

system ("mkdir -p $d");

if (! -e $d){
    print "\n";
    print "ERROR: Cannot create directory $d\n";
    $stats = -1;
    return $stats;
}else{

    $command = "gzip -f $fout";
    print  "\n$command\n";
    system ($command);

    $command = "cp -f $fout.gz $d/.";
    print  "\n$command\n";
    system ($command);
}

if (! -e "$d/$fout.gz" ){
    print "\n";
    print "ERROR: MEDOC file $d/$fout.gz was not created\n";
    $stats = -1;
    return $stats;
}else{
    print "\n";
    print "Output is file $d/$fout.gz\n";
 }

#-------------------------------------------------------------------------------
# Clean up
#-------------------------------------------------------------------------------

 if ($CLEANDIR >= 1) {
     $command = "rm -rf $medoc_dir";
     print "\n$command\n";
     system  ($command);
 }

#-------------------------------------------------------------------------------
# End
#-------------------------------------------------------------------------------

print "\n";
print
"---------------------------------------------------------------------------\n";
return $stats;

}


#------------------------------------------------------------------------------#
# Sub do_graphics
#------------------------------------------------------------------------------#

sub do_2d_wrf_graphics
{

use strict;

#------------------------------------------------------------------------------#
# Global variables needed in the environment
#------------------------------------------------------------------------------#
 our ($NCARG_ROOT, $NCOBIN);
 $ENV{'NCARG_ROOT'} = "$NCARG_ROOT";
 $ENV{'NCOBIN'}="$NCOBIN";

#------------------------------------------------------------------------------#
# Other global variables
#------------------------------------------------------------------------------#
 our ($WRF_OUTPUT_DIR,$JOB_LOC);
 our ($GRAPH_TYPE);

#------------------------------------------------------------------------------#
# Local variables
#------------------------------------------------------------------------------#
 my ($path_to_files,$wrf_file,$graph_dir);
 my ($nwrf);
 my ($dom, $strlen, $n, $ni, $f, $ff, $fff);
 my ($var2plot);
 my ($fin, $fout,$dout);
 my ($command,$stats);
 my ($ccyymmddhh,$ccyy,$mm,$dd,$hh,$mn,$ss);
 my (@MDL,@MDL1);
 my (@IMAGES);

 my $output_dir_wrf = $WRF_OUTPUT_DIR;
 my $output_dir_graph = $JOB_LOC;

 my $NCL_SCRIPTS = "$MM5HOME/cycle_code/POSTPROCS/ncl";

#------------------------------------------------------------------------------#
# Arguments
#------------------------------------------------------------------------------#

 my $StartTime = shift; 
 my $EndTime  = shift;
 my $dom = shift;

#------------------------------------------------------------------------------#
# Print arguments
#------------------------------------------------------------------------------#

print "\n";
print 
"---------------------------------------------------------------------------\n";
print "do_2d_wrf_graphics ($StartTime,$EndTime,$dom,$output_dir_wrf,$output_dir_graph)\n";

$stats = 0;

#------------------------------------------------------------------------------#
# Find path to WRF files
#------------------------------------------------------------------------------#

$path_to_files = "$WRF_OUTPUT_DIR/WRF/D$dom";

if (! -e $path_to_files){
    print "\n";
    print "ERROR: Cannot find $path_to_files\n";
    $stats = -1;
    return $stats;
}

if (! -d $path_to_files){
    print "\n";
    print "ERROR: not a directory $path_to_files\n";
    $stats = -1;
    return $stats;
}

print "\n";
print "Ploting WRF files in directory $path_to_files\n";

#------------------------------------------------------------------------------#
# List WRF files that are needed for the conversion
#------------------------------------------------------------------------------#

$ccyymmddhh = $StartTime;
$n = 0;

while ($ccyymmddhh <= $EndTime){

       $ccyy = substr($ccyymmddhh,0,4);
       $mm = substr($ccyymmddhh,4,2);
       $dd = substr($ccyymmddhh,6,2);
       $hh = substr($ccyymmddhh,8,2);
       $mn = "00";
       $ss = "00";

       $wrf_file = "$path_to_files/wrfout_d0${dom}_${ccyy}-${mm}-${dd}_${hh}:${mn}:00.${RANGE}_F" ;

       if (! -e $wrf_file){
           print "\n";
           print "ERROR: Cannot find file $wrf_file\n";
       }else{
           $MDL1[$n] = $wrf_file;
           $n++;
       }

       $ccyymmddhh = &hh_advan_date ($ccyymmddhh,+1);
}

# Print if no input files were found
if ($#MDL1 < 0) {
    print "\n";
    print "ERROR: Cannot find any WRF file for domain $dom valid between $StartTime and $EndTime\n in directory $path_to_files\n";
    $stats = -1;
    return $stats;
}else{
     @MDL = sort @MDL1;
     print "\n";
     print "Found $n files for domain $dom valid between $StartTime and $EndTime\nin directory $path_to_files\n";
     foreach $f (@MDL){
     print "$f\n";
     }

}

#------------------------------------------------------------------------------#
# Move to the work dir
#------------------------------------------------------------------------------#

$graph_dir = "$output_dir_wrf/GRAPHICS_WRF/D${dom}";

if (! -e $graph_dir){  
    system ("rm -rf $graph_dir") 
}

if (! -d $graph_dir){  
    system ("mkdir -p $graph_dir") 
}

if (! chdir $graph_dir){  
    print "\n";
    print "ERROR: cannot chdir to $graph_dir\n";
    $stats = -1;
    return $stats;
}

print "\n";
print "Workdir is $graph_dir\n";
print "\n";

#------------------------------------------------------------------------------#
# Link NCL scripts into local directory
#------------------------------------------------------------------------------#

for $f ("plot_wrf_pblheight.ncl","plot_wrf_wind_fixed_height.ncl")
 {
        if (! -e "$NCL_SCRIPTS/$f"){
            print "\n";
            print "ERROR: cannot find script $NCL_SCRIPTS/$f\n";
            return -1;
         }
         unlink $f if (-e $f); 
         symlink ("$NCL_SCRIPTS/$f","$f");
}

#-------------------------------------------------------------------------------
# Create output directory
#-------------------------------------------------------------------------------

$dout = "$output_dir_graph";

system ("mkdir -p $dout");

if (! -e $dout){
    print "\n";
    print "ERROR: Cannot create directory $dout\n";
    $stats = -1;
    return $stats;
}

#------------------------------------------------------------------------------#
# Loop over the list of files
#------------------------------------------------------------------------------#
print "\n";
print 
"---------------------------------------------------------------------------\n";

# Find how many characters are needed
$strlen = length ("$#MDL"); 

$n = 1;

foreach $ff (@MDL) {

     chomp ($ff);


     #-------------------------------------------------------------------------#
     # Check again if the file exists
     #-------------------------------------------------------------------------#

     if (! -e "$ff"){
         print "\n";
         print "ERROR: Cannot find file $ff\n";
         $stats = -1;
         return $stats;
     }

     #-------------------------------------------------------------------------#
     # Link to short names link_XXXX to apply NCO
     #-------------------------------------------------------------------------#

     $f = sprintf ("link_%${strlen}.${strlen}d",$n);
   
     unlink "$f.nc" if (-e "$ff.nc");

     $command =  "ln -sf $ff $f.nc";  
     print  "$command\n" if ($DEBUG > 1);
     system ($command);

     #-------------------------------------------------------------------------#
     # Increment file counter
     #-------------------------------------------------------------------------#

     $n ++;
}


$nwrf = $n - 1 ;

print "\n";
print "Found $nwrf files for domain $dom valid between $StartTime end $EndTime\n";
print "\n";
print 
"---------------------------------------------------------------------------\n";

#-------------------------------------------------------------------------------
# Cat WRF files along the time dimension
#-------------------------------------------------------------------------------

$fin  = sprintf ("link_%${strlen}.${strlen}d.nc",1);

$ccyy = substr($StartTime,0,4);
$mm   = substr($StartTime,4,2);
$dd   = substr($StartTime,6,2);
$hh   = substr($StartTime,8,2);

#$fout = "wrfout_d0${dom}_${ccyy}-${mm}-${dd}_${hh}";
$fout = "${GSJOBTYPE}${GMID}_wrf";

if (-e $fout) { unlink ($fout); }

# Apply NCO operators

print "\nCat $nwrf files into $fout\n";

$command = "$NCOBIN/bin/ncrcat -O -n $nwrf,$strlen,1 $fin $fout.nc";

print "\n$command\n";
system  ($command);

if (! -e "$fout.nc"){
    print "\n";
    print "ERROR: File $fout.nc was not generated\n"; 
    print "\n";
    $stats = -1;
    return $stats;
}

#-------------------------------------------------------------------------------
# Plot PBL height
#-------------------------------------------------------------------------------
print "\n";
print 
"---------------------------------------------------------------------------\n";
print "Plotting pblh from file $fout\n";

# Generate the PPBL height plot

$command = "${NCARG_ROOT}/bin/ncl \'wrf=\"$fout\"\' \'type=\"$GRAPH_TYPE\"\' plot_wrf_pblheight.ncl";

print "\n$command\n\n";
system  ($command);

$ni = 0;

if (! -e "${fout}_pblh.${GRAPH_TYPE}"){
    print "\n";
    print "ERROR: File ${fout}_pblh.${GRAPH_TYPE} was not generated\n"; 
    $stats = 0;
#}else{
#   $command = "mv -f ${fout}_pblh.${GRAPH_TYPE} $dout/.";
#   print  "$command\n";
#   system ($command);
}

#-------------------------------------------------------------------------------
# Plot wind, temperature and humidity at fixed height
#-------------------------------------------------------------------------------

foreach $var2plot ("wind","temp","humi")
{

  print "\n";
  print 
"---------------------------------------------------------------------------\n";
  print "Plotting $var2plot from file $fout\n";

  $command = "${NCARG_ROOT}/bin/ncl \'wrf=\"$fout\"\' \'var2plot=\"$var2plot\"\' \'type=\"$GRAPH_TYPE\"\' plot_wrf_wind_fixed_height.ncl";

  print "\n$command\n\n";
  system  ($command);

# Plots are made on several height levels, check the first ones at 2m and 10m

  if (! -e "${fout}_${var2plot}_0002m.${GRAPH_TYPE}" &&
      ! -e "${fout}_${var2plot}_0010m.${GRAPH_TYPE}"){
      print "\n";
      print "ERROR: Files ${fout}_${var2plot}_0002m.${GRAPH_TYPE} or ${fout}_${var2plot}_0010m.${GRAPH_TYPE} was not generated\n";
          print "\n";
          $stats = 0;
# }else{
#   $command = "mv -f ${fout}_${var2plot}_*.${GRAPH_TYPE} $dout/.";
#   print  "$command\n";
#   system ($command);
  }

}

#------------------------------------------------------------------------------#
# Make an archive of all images and move it to the output directory
#------------------------------------------------------------------------------#
print "\n";
print 
"---------------------------------------------------------------------------\n";
print "\n";

@IMAGES = `find . -name '${fout}_*.${GRAPH_TYPE}'`;

#$fout = "D${dom}.${StartTime}-${EndTime}.wrf";
$fout = "${GSJOBTYPE}${outJobID}_D${dom}_wrf";

if ($#IMAGES >= 0){

    chomp(@IMAGES);

    $command = "tar czf $fout.tar.gz @IMAGES";
    print "\n$command\n";
    system  ($command);

    if (! -e "$fout.tar.gz"){
        print "\nWARNING: file $fout.tar.gz was not generated\n";
        $stats = 0;
    }else{
        $command = "mv -f $fout.tar.gz $dout/.";
        print "\n$command\n";
        system  ($command);
    }

}

#-------------------------------------------------------------------------------
# Clean up
#-------------------------------------------------------------------------------

 if ($CLEANDIR >= 1) {
     $command = "rm -rf $graph_dir";
     print "\n$command\n";
     system  ($command);
 }

#-------------------------------------------------------------------------------
# End
#-------------------------------------------------------------------------------

print "\n";
print 
"---------------------------------------------------------------------------\n";
return $stats;

}

#------------------------------------------------------------------------------#
# Sub do_2d_obs_graphics
#------------------------------------------------------------------------------#

sub do_2d_obs_graphics
{

use strict;
#------------------------------------------------------------------------------#
# Global variables needed in the environment
#------------------------------------------------------------------------------#
 our ($NCARG_ROOT,$NCOBIN);
 $ENV{'NCARG_ROOT'} = "$NCARG_ROOT";
 $ENV{'NCOBIN'}="$NCOBIN";

#------------------------------------------------------------------------------#
# Other global variables
#------------------------------------------------------------------------------#
 our ($WRF_OUTPUT_DIR,$JOB_LOC);
 our ($GSJOBDIR);
 our ($GRAPH_TYPE);

#------------------------------------------------------------------------------#
# Local variables
#------------------------------------------------------------------------------#

 my ($path_to_files,$wrf_file,$fout,$graph_dir);
 my ($f,$g,$dout);
 my ($nwrf);
 my ($command,$stats);
 my ($ccyymmddhh,$ccyy,$mm,$dd,$hh,$mn,$ss);
 my (@MDL,@MDL1);
 my (@MDLTXT,@MDLSFC,@MDLUPR);
 my ($nsfc,$ntxt,$nupr);
 my ($geo_file);

 my $output_dir_wrf = $WRF_OUTPUT_DIR;
 my $output_dir_graph = $JOB_LOC;

 my $EXECUTABLE_ARCHIVE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE";
 my $CSH_ARCHIVE = "$MM5HOME/cycle_code/CSH_ARCHIVE";
 my $NCL_SCRIPTS = "$MM5HOME/cycle_code/POSTPROCS/ncl";

#------------------------------------------------------------------------------#
# Arguments
#------------------------------------------------------------------------------#

 my $StartTime = shift; 
 my $EndTime  = shift;
 my $dom = shift;

#------------------------------------------------------------------------------#
# Print arguments
#------------------------------------------------------------------------------#

print "\n";
print 
"---------------------------------------------------------------------------\n";
print "do_2d_obs_graphics ($StartTime,$EndTime,$dom,$output_dir_wrf,$output_dir_graph)\n";

$stats = 0;

#------------------------------------------------------------------------------#
# Find path to OBS files
#------------------------------------------------------------------------------#

$path_to_files = "$WRF_OUTPUT_DIR/OBS";

if (! -e $path_to_files){
    print "\n";
    print "ERROR: Cannot find $path_to_files\n";
    $stats = -1;
    return $stats;
}

if (! -d $path_to_files){
    print "\n";
    print "ERROR: not a directory $path_to_files\n";
    $stats = -1;
    return $stats;
}

print "\n";
print "Ploting OBS files for domain $dom in directory $path_to_files\n";


#------------------------------------------------------------------------------#
# List OBS files that are needed for the plot
#------------------------------------------------------------------------------#
     $ccyymmddhh = $StartTime;
     $n = 0;

     while ( $ccyymmddhh <= $EndTime){

            $ccyy = substr($ccyymmddhh,0,4);
            $mm = substr($ccyymmddhh,4,2);
            $dd = substr($ccyymmddhh,6,2);
            $hh = substr($ccyymmddhh,8,2);
            $mn = "00";
            $ss = "00";

            $wrf_file = "$path_to_files/qc_out_${ccyy}-${mm}-${dd}_${hh}:${mn}:00.${RANGE}_F" ;

            if (! -e $wrf_file){
                print "\n";
                print "WARNING: Cannot find file $wrf_file\n";
           }else{
                $MDL1[$n] = $wrf_file;
                $n++;
           }

           $ccyymmddhh = &hh_advan_date ($ccyymmddhh,+1);
     }

# Print if no input files were found
if ($#MDL1 < 0) {
    print "\n";
    print "ERROR: Cannot find any OBS file valid between $StartTime and $EndTime\n in directory $path_to_files\n";
    $stats = -1;
    return $stats;
}else{
     @MDL = sort @MDL1;
     print "\n";
     print "Found $n files valid between $StartTime and $EndTime\nin directory $path_to_files\n";
     foreach $f (@MDL){
        print "$f\n";
     }

}

#------------------------------------------------------------------------------#
# Move to the work dir
#------------------------------------------------------------------------------#

$graph_dir = "$output_dir_wrf/GRAPHICS_OBS/D${dom}";

if (! -e $graph_dir){  
    system ("rm -rf $graph_dir") 
}

if (! -d $graph_dir){  
    system ("mkdir -p $graph_dir") 
}

if (! chdir $graph_dir){  
    print "\n";
    print "ERROR: cannot chdir to $graph_dir\n";
    $stats = -1;
    return $stats;
}

print "\n";
print "Workdir is $graph_dir\n";
print "\n";

#------------------------------------------------------------------------------#
# Link geogrid file scripts into local directory
#------------------------------------------------------------------------------#

$geo_file = "geo_em.d0${dom}.nc";

if (! -e "$GSJOBDIR/wps/$geo_file")
  {
   print "\nERROR: Cannot find script $GSJOBDIR/wps/$geo_file\n\n";
   $stats = -1;
   return $stats;
  }else{
   system ("rm -f $geo_file") if (-f $geo_file);
   $command =  "ln -sf $GSJOBDIR/wps/$geo_file .";
   print  "$command\n";
   system ($command);
 }

#------------------------------------------------------------------------------#
# Link NCL scripts into local directory
#------------------------------------------------------------------------------#

foreach $f ("StationModel.ncl","RTFDDAUser.ncl","SfcStatsThin.ncl","UpperAirObs.ncl")
 {

        if (! -e "$NCL_SCRIPTS/$f"){
            print "\n";
            print "ERROR: cannot find script $NCL_SCRIPTS/$f\n";
            $stats = -1;
            return $stats;
         }
           
         unlink $f if (-e $f); 
         symlink ("$NCL_SCRIPTS/$f","$f");
}

#------------------------------------------------------------------------------#
# Link EXECUTABLES into local directory
#------------------------------------------------------------------------------#

print "\n";

foreach $f ("QCtoNC.exe","latlon_wrf.exe")
{
  if (! -e "$EXECUTABLE_ARCHIVE/$f")
  {
   print "\nERROR: Cannot find executable $EXECUTABLE_ARCHIVE/$f\n\n";
   $stats = -1;
   return $stats;
  }else{
   system ("rm -f $f") if (-f $f);
   $command = "ln -sf $EXECUTABLE_ARCHIVE/$f .";
   print  "$command\n";
   system ($command);
  }
}

#------------------------------------------------------------------------------#
# Link SHELL scripts into local directory
#------------------------------------------------------------------------------#

foreach $f ("RT_all.obs_trim-merge.USA")
{
  if (! -e "$CSH_ARCHIVE/Forecast/$f")
  {
   print "\nERROR: Cannot find script $CSH_ARCHIVE/Forecast/$f\n\n";
   $stats = -1;
   return $stats;
  }else{
   system ("rm -f $f") if (-f $f);
   $command = "ln -sf $CSH_ARCHIVE/Forecast/$f .";
   print  "$command\n";
   system ($command);
 }
}

#------------------------------------------------------------------------------#
# Create ouput directory
#------------------------------------------------------------------------------#

$dout = "$output_dir_graph";

system ("mkdir -p $dout");

if (! -e $dout){
    print "\n";
    print "ERROR: Cannot create directory $dout\n";
    $stats = -1;
    return $stats;
}

#------------------------------------------------------------------------------#
# Cat observation file valid within the requested time window in one file
#------------------------------------------------------------------------------#

$ccyymmddhh = $StartTime;

system ("rm -f hourly.obs") if (-f "hourly.obs");

$n = 1;

foreach $f (@MDL) {

     chomp ($f);

     #-------------------------------------------------------------------------#
     # Check again if the file exists
     #-------------------------------------------------------------------------#

     if (! -e "$f"){
         print "\n";
         print "ERROR: Cannot find file $f\n";
         $stats = -1;
         return $stats;
     }

     if (! -e $f)
     {
         if (! -e "$f")
         {
             print "\nWARNING: Cannot find file $f\n";
         }else{
             system ("cat $f >> hourly.obs");
         }
     }else{
             system ("cat $f >> hourly.obs");
     }

    $n ++;

}

if (! -e "hourly.obs" || -z "hourly.obs")
{
         print "\nERROR: Cannot find a valid file between $StartTime and $EndTime\n\n";
         $stats = -1;
         return $stats;
}

$nwrf = $n - 1;

print "\n";
print "Found $nwrf files for domain $dom valid between $StartTime end $EndTime\n";
print
"-----------------------------------------------------------------------------";
print "\n";

#------------------------------------------------------------------------------#
# Sort the data chronologically and trim onto the WRF domain
#------------------------------------------------------------------------------#

# Find the corner of the WRF domain

$command = "./latlon_wrf.exe geo_em.d0${dom}.nc -latlon latlon.txt";
print "\n$command\n";
system  ($command);

# Output is text file latlon.txt
if (! -e "latlon.txt")
{
 print "\nERROR: file $graph_dir/latlon.txt was not generated\n\n";
 $stats = -1;
 return $stats;
}

# Trim and sort the data (hourly file in time window ]hh-30,hh+30]
$command = "./RT_all.obs_trim-merge.USA hourly.obs 30 latlon.txt > /dev/null";
print "\n$command\n";
system  ($command);


#------------------------------------------------------------------------------#
# Convert obs file into NETCDF
#------------------------------------------------------------------------------#

print "\n";
print
"-----------------------------------------------------------------------------";
print "\n";

$ccyymmddhh = $StartTime;

while ($ccyymmddhh <= $EndTime)
{

 $f = "$ccyymmddhh.hourly.obs";

 if (! -e $f)
 {
     print "\nWARNING: Cannot find file $f\n";
  }else{
     $command = "./QCtoNC.exe $f ${ccyymmddhh}";
     print "\n$command\n\n";
     system  ($command);
  }

  $ccyymmddhh = &hh_advan_date( $ccyymmddhh, +1);

}

#------------------------------------------------------------------------------#
# Plot surface observations
#------------------------------------------------------------------------------#

$ccyymmddhh = $StartTime;
$nsfc = 0;
$ntxt = 0;

while ($ccyymmddhh <= $EndTime)
{

print "\n";
print
"-----------------------------------------------------------------------------";
print "\n";

 $ccyy = substr ($ccyymmddhh,0,4);
 $mm   = substr ($ccyymmddhh,4,2);
 $dd   = substr ($ccyymmddhh,6,2);
 $hh   = substr ($ccyymmddhh,8,2);

 $f = "$ccyymmddhh.hourly.obs_sgl";
 $g = "SFCplot";

 if (! -e "$f.nc")
 {
     print "\nWARNING: file $f.nc was not generated, skipping surface plot for $ccyymmddhh\n";
 }else{
     $command = "${NCARG_ROOT}/bin/ncl ./SfcStatsThin.ncl 'Range=\"$RANGE\"' Date=$ccyymmddhh Domain=$dom wrf_rotated=True 'graph_type=\"$GRAPH_TYPE\"'";
     print "\n$command\n";
     system  ($command);
 }

 # Rename txt output

 if (! -e "$f.txt")
 {
     print "\nWARNING: file $f.txt was not generated\n";
     $stats = 0;
 }else{
#    $command = "mv -f $f.txt $dout/qc_out_d0${dom}_${ccyy}-${mm}-${dd}_${hh}_sfc.txt";
#    $command = "mv -f $f.txt qc_out_d0${dom}_${ccyy}-${mm}-${dd}_${hh}_sfc.txt";
     $command = "mv -f ${f}.txt ${GSJOBTYPE}${outJobID}_D${dom}_obs_sfc_${ccyy}_${mm}_${dd}_${hh}.txt";
     print "\n$command\n";
     system  ($command);
     $MDLTXT[$ntxt] = "${GSJOBTYPE}${outJobID}_D${dom}_obs_sfc_${ccyy}_${mm}_${dd}_${hh}.txt";
     $ntxt ++;
 }

 # Rename graphic output


 if (! -e "$g.$GRAPH_TYPE")
 {
     print "\nWARNING: file $g.$GRAPH_TYPE was not generated\n";
     $stats = 0;
 }else{
#    $command = "mv -f $g.$GRAPH_TYPE qc_out_d0${dom}_${ccyy}-${mm}-${dd}_${hh}_sfc.$GRAPH_TYPE";
     $command = "mv -f ${g}.${GRAPH_TYPE} ${GSJOBTYPE}${outJobID}_D${dom}_obs_sfc_${ccyy}_${mm}_${dd}_${hh}.${GRAPH_TYPE}";
     print "\n$command\n";
     system  ($command);
     $MDLSFC[$nsfc] = "${GSJOBTYPE}${outJobID}_D${dom}_obs_sfc_${ccyy}_${mm}_${dd}_${hh}.${GRAPH_TYPE}";
     $nsfc ++;
 }

 # Go to next hour
  $ccyymmddhh = &hh_advan_date( $ccyymmddhh, +1);
}

#------------------------------------------------------------------------------#
# Catenate sfc images in one single file
#------------------------------------------------------------------------------#
print "\n";
print
"-----------------------------------------------------------------------------";
print "\n";

#$fout = "D${dom}.${StartTime}-${EndTime}.obs.sfc";
$fout = "${GSJOBTYPE}${outJobID}_D${dom}_obs_sfc";

if ($#MDLSFC < 0){
    print "\nWARNING: no surface plot was generated\n";
    $stats = 0;
}else{

    $command = "/usr/bin/gs -q -sPAPERSIZE=letter -dNOPAUSE -dBATCH -sDEVICE=${GRAPH_TYPE}write -sOutputFile=${fout}.${GRAPH_TYPE} @MDLSFC";

    print "\n$command\n";
    system ($command);

    if (! -e "$fout.$GRAPH_TYPE"){
        print "\nWARNING: file $fout.$GRAPH_TYPE was not generated\n";
        $stats = 0;
    }else{
        # Add catenated file to the list of file to be archieved
        $MDLSFC[$nsfc] = "${fout}.$GRAPH_TYPE";

        # Move catenated file to the output directory
#       $command = "mv -f $fout.$GRAPH_TYPE $dout/.";
#       print "\n$command\n";
#       system  ($command);
    }

}

#------------------------------------------------------------------------------#
# Catenate report text file in one single file
#------------------------------------------------------------------------------#

#$fout = "D${dom}.${StartTime}-${EndTime}.obs.sfc";
$fout = "${GSJOBTYPE}${outJobID}_D${dom}_obs_sfc";

if ($#MDLTXT < 0){
    print "\nWARNING: no surface report was generated\n";
    $stats = 0;
}else{

    if (-e "${fout}.txt"){
        system ("rm -f ${fout}.txt") 
    }

    $command = "cat @MDLTXT > ${fout}.txt";

    print "\n$command\n";
    system ($command);

    if (! -e "$fout.txt"){
        print "\nWARNING: file $fout.txt was not generated\n";
        $stats = 0;
    }else{
        # Add catenated file to the list of file to be archieved
        $MDLTXT[$ntxt] = "${fout}.txt";

        # Move catenated file to the output directory
#       $command = "mv -f $fout.txt $dout/.";
#       print "\n$command\n";
#       system  ($command);
    }

}


#------------------------------------------------------------------------------#
# Plot upper air and satellite observations
#------------------------------------------------------------------------------#

$ccyymmddhh = $StartTime;
$nupr = 0;

while ($ccyymmddhh <= $EndTime)
{

 print "\n";
 print
"-----------------------------------------------------------------------------";
 print "\n";

 $ccyy = substr ($ccyymmddhh,0,4);
 $mm   = substr ($ccyymmddhh,4,2);
 $dd   = substr ($ccyymmddhh,6,2);
 $hh   = substr ($ccyymmddhh,8,2);

 $f = "$ccyymmddhh.hourly.obs_mpl.nc";

 if (! -e $f)
 {
     print "\nWARNING: file $f was not generated, skipping upper plot for $ccyymmddhh\n";
 }else{
     # Generate graphics

     $command = "${NCARG_ROOT}/bin/ncl ./UpperAirObs.ncl 'Range=\"$RANGE\"' Date=$ccyymmddhh Domain=$dom wrf_rotated=True 'graph_type=\"$GRAPH_TYPE\"'";
     print "\n$command\n";
     system  ($command);
 }

 # Rename output

 $g = "RAOBplot";

 if (! -e "$g.$GRAPH_TYPE")
 {
     print "\nWARNING: file $g.$GRAPH_TYPE was not generated at time $ccyymmddhh\n";
 }else{
#    $command = "mv -f $g.$GRAPH_TYPE qc_out_d0${dom}_${ccyy}-${mm}-${dd}_${hh}_upr.$GRAPH_TYPE";
     $command = "mv -f ${g}.${GRAPH_TYPE} ${GSJOBTYPE}${outJobID}_D${dom}_obs_upr_${ccyy}_${mm}_${dd}_${hh}.${GRAPH_TYPE}";
     print "\n$command\n";
     system  ($command);
     $MDLUPR[$nupr] = "${GSJOBTYPE}${outJobID}_D${dom}_obs_upr_${ccyy}_${mm}_${dd}_${hh}.${GRAPH_TYPE}";
     $nupr ++;
 }

 # Go to next hour
 $ccyymmddhh = &hh_advan_date( $ccyymmddhh, +1);

}

#------------------------------------------------------------------------------#
# Catenate upr images in one single file
#------------------------------------------------------------------------------#

print "\n";
print
"-----------------------------------------------------------------------------";
print "\n";

#$fout = "D${dom}.${StartTime}-${EndTime}.obs.upr";
$fout = "${GSJOBTYPE}${outJobID}_D${dom}_obs_upr";

if ($#MDLUPR < 0){
    print "\nWARNING: no upr plot was generated\n";
    $stats = 0;
}else{

    $command = "/usr/bin/gs -q -sPAPERSIZE=letter -dNOPAUSE -dBATCH -sDEVICE=${GRAPH_TYPE}write -sOutputFile=${fout}.${GRAPH_TYPE} @MDLUPR";

    print "\n$command\n";
    system ($command);

    if (! -e "$fout.$GRAPH_TYPE"){
        print "\nWARNING: file $fout.$GRAPH_TYPE was not generated\n";
        $stats = 0;
    }else{
        # Add catenated file to the list of file to be archieved
        $MDLUPR[$nupr] = "${fout}.$GRAPH_TYPE";

        # Move catenated file to the output directory
#       $command = "mv -f $fout.$GRAPH_TYPE $dout/.";
#       print "\n$command\n";
#       system  ($command);
    }

}

#------------------------------------------------------------------------------#
# Make an archive of all hourly files and move it to the output directory
#------------------------------------------------------------------------------#

#$fout = "D${dom}.${StartTime}-${EndTime}.obs";
$fout = "${GSJOBTYPE}${outJobID}_D${dom}_obs";

if ($#MDLSFC >= 0 || $#MDLTXT >= 0 || $#MDLUPR >= 0){

    $command = "tar czf $fout.tar.gz @MDLSFC @MDLTXT @MDLUPR";
    print "\n$command\n";
    system  ($command);

    if (! -e "$fout.tar.gz"){
        print "\nWARNING: file $fout.tar.gz was not generated\n";
        $stats = 0;
    }else{
        $command = "mv -f $fout.tar.gz $dout/.";
        print "\n$command\n";
        system  ($command);
    }

}

#-------------------------------------------------------------------------------
# Clean up
#-------------------------------------------------------------------------------

 if ($CLEANDIR >= 1) {
     $command = "rm -rf $graph_dir";
     print "\n$command\n";
     system  ($command);
 }

#-------------------------------------------------------------------------------
# End
#-------------------------------------------------------------------------------

print "\n";
print 
"---------------------------------------------------------------------------\n";
return $stats;

}

#------------------------------------------------------------------------------#
# Sub medoc_cfg: Generate a input config file for MEDOC conversion
# Use subroutine wrf_dims and executable latlon_wrf.exe
#------------------------------------------------------------------------------#

sub medoc_cfg
{
    use strict;

    my ($file_in);
    my ($west_east,$south_north,$bottom_top);

    $file_in = shift;

    # Grab WRF dimensions

    ($west_east,$south_north,$bottom_top) = &wrf_dims ($file_in);

    if ($west_east <= 0 || $south_north <= 0 || $bottom_top <= 0){
        print "\nERROR: Cannot get dimensions of file $file_in\n\n";
        return -1;
    }

    # Generate the config file

    if (! open (CFG,">MEDOCconverter.cfg")){
        print "\nERROR: Cannot open MEDOCconverter.cfg for writing\n\n";
        return -1;
    }else{
        print CFG "   1    ix_ll (Starting WRF grid point in x-direction)\n";
        print CFG "   1    iy_ll (Starting WRF grid point in y-direction)\n";
        print CFG sprintf (" %3i    nx_SCIPUFF (ix_ll + nx_SCIPUFF <= nx_WRF+1)\n",$west_east+1);
        print CFG sprintf (" %3i    ny_SCIPUFF (iy_ll + ny_SCIPUFF <= ny_WRF+1)\n",$south_north+1);
        print CFG sprintf (" %3i    nz_SCIPUFF (        nz_SCIPUFF <= nz_WRF)\n",$bottom_top);
        print CFG "   0 0    output format (0: native in horizontal, 0: native in vertical)\n";
        close (CFG);
    }

    return 0;

}
#------------------------------------------------------------------------------#
# Sub wrf_dims: Find the dimensions of a WRF output file
# Use executable latlon_wrf.exe
# return west_east,south_north,bottom_top dimensions
#------------------------------------------------------------------------------#

sub wrf_dims
{
    use strict;

    our $EXECUTABLE_ARCHIVE;
    my ($file_in);
    my ($west_east,$south_north,$bottom_top);
    my ($l,$f,$command);
    my ($stats);
    my (@tmp,@dims);

    $file_in = shift;
    $west_east   = 0;
    $south_north = 0;
    $bottom_top  = 0;

    $stats = 0;

    $f = "$EXECUTABLE_ARCHIVE/latlon_wrf.exe";

    if (! -e $f){
         print "\nERROR: Cannot find executable $f\n\n";
         return ($west_east,$south_north,$bottom_top);
    }

    $command = "$f $file_in -dims wrf_dims.txt";
    print "\n-------------------------------------------------------------\n";
    print "\n$command\n\n";
    system  ($command);

    if (-e "wrf_dims.txt" && ! -z "wrf_dims.txt") {

        if (! open (WRF_DIMS,"wrf_dims.txt")){
            print "\nERROR: Cannot open wrf_dims.txt for reading:\n\n";
        }else{

            $l = 0;

            while (<WRF_DIMS>) {
                   chomp ($_);
                   @tmp = split ('=');
                   if ($#tmp >= 1){
                       $dims[$l] = $tmp[1];
                       $l++;
                   }
            }

            close (WRF_DIMS);

            if ($l >= 3){
                $west_east   = $dims[0];
                $south_north = $dims[1];
                $bottom_top  = $dims[2];
            }else{
                print "\nERROR: reading file wrf_dims.txt\n\n";
            }
        }

    }else{
         print "\nERROR: command $command failed\n\n";
    }

    return ($west_east,$south_north,$bottom_top);
}

#------------------------------------------------------------------------------#
# Sub wrf_dims_ncl: Find the dimensions of a WRF output file
# Use script print_wrf_dims.ncl
# return west_east,south_north,bottom_top dimensions
#------------------------------------------------------------------------------#

sub wrf_dims_ncl
{
    use strict;
# Environment
our $NCARG_ROOT;
$ENV{'NCARG_ROOT'} = "$NCARG_ROOT";

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

#------------------------------------------------------------------------------#
# Create output directory whose path was read in POSTPROCINPUT
# and copy a few config files into the output directory
#------------------------------------------------------------------------------#

sub makeDirOutput
{

 use strict;

 our ($GSJOBDIR,$JOB_LOC);

 my ($command);
 my ($file_from,$file_to);

 print "\n";
 print 
"---------------------------------------------------------------------------\n";
 print "\n";

    if (! -d $JOB_LOC){
        $command = "mkdir -p $JOB_LOC";
        print "\n$command\n";
        system  ($command);
    }

    if (! -d $JOB_LOC){
        print "\n";
        print "ERROR: Cannot create directory $JOB_LOC\n";
        return -1;
    }

    return 0;

    $file_from = "$GSJOBDIR/../${GSJOBTYPE}${outJobID}_D${Domain}_metadata.txt";
    $file_to   = "$JOB_LOC/${GSJOBTYPE}${outJobID}_D${Domain}_metadata.txt";
    if (! -e $file_to){
        if (-e $file_from){
             $command = "cp -f $file_from $file_to";
             print "\n$command\n";
             system ($command);
        }else{
             print "\nWARNING: Cannot find file $file_from\n";
             print   "WARNING: Cannot copy to file $file_to\n";
        }
    }else{
       print "\nFile $file_to already exists\n";
    }

    $file_from = "$GSJOBDIR/../job_configuration.xml";
    $file_to   = "$JOB_LOC/job_configuration.xml";
    if (! -e $file_to){
        if (-e $file_from){
             $command = "cp -f $file_from $file_to";
             print "\n$command\n";
             system ($command);
        }else{
             print "\nWARNING: Cannot find file $file_from\n";
             print   "WARNING: Cannot copy to file $file_to\n";
        }
    }else{
       print "\nFile $file_to already exists\n";
    }

    $file_from = "$GSJOBDIR/../WRF/namelists/wrf.nl.template";
    $file_to   = "$JOB_LOC/wrf.nl.template";
    if (! -e $file_to){
        if (-e $file_from){
             $command = "cp -f $file_from $file_to";
             print "\n$command\n";
             system ($command);
        }else{
             print "\nWARNING: Cannot find file $file_from\n";
             print   "WARNING: Cannot copy to file $file_to\n";
        }
    }else{
       print "\nFile $file_to already exists\n";
    }

    $file_from = "$GSJOBDIR/../WRF/namelists/wps.nl.template";
    $file_to   = "$JOB_LOC/wps.nl.template";
    if (! -e $file_to){
        if (-e $file_from){
             $command = "cp -f $file_from $file_to";
             print "\n$command\n";
             system ($command);
        }else{
             print "\nWARNING: Cannot find file $file_from\n";
             print   "WARNING: Cannot copy to file $file_to\n";
        }
    }else{
       print "\nFile $file_to already exists\n";
    }

    $file_from = "$GSJOBDIR/../WRF/wps/WRF_topo_config.pdf";
#   $file_to   = "$JOB_LOC/WRF_topo_config.pdf";
    $file_to   = "$JOB_LOC/${GSJOBTYPE}${outJobID}_config_wrf.pdf";
    if (! -e $file_to){
        if (-e $file_from){
             $command = "cp -f $file_from $file_to";
             print "\n$command\n";
             system ($command);
        }else{
             print "\nWARNING: Cannot find file $file_from\n";
             print   "WARNING: Cannot copy to file $file_to\n";
        }
    }else{
       print "\nFile $file_to already exists\n";
    }

    return 0;
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
print "Usage: cfdda_Medoc.pl StarTime=2013082019 EndTime=2013082119 Domain=3\n";
print "                      FLEXINPUT=GCWXXXX/WRF/Medoc/tmp/medoc/pre_process_in_donotedit.pl\n";
print "\n";
print "To generate MEDOC files and graphics between 2013082019 and 2013082119\n";
print :"for domain 3 of job GCWXXXX\n";
print "\n";
exit -1

}

1

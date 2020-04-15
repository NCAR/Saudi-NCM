#!/usr/bin/perl
#------------------------------------------------------------------------------#

use strict;

use Getopt::Std;
use XML::Simple;
use Time::Local;
use File::Basename;

require "stat.pl";
require "flush.pl";
require "ctime.pl";

#------------------------------------------------------------------------------#

# Pass through the environment
  my ($FLEXINPUT);

# Read in FLEXINPUT
  our ($MM5HOME,$EXECUTABLE_ARCHIVE,$PERL_ARCHIVE,$PERL_FLEX,$GSJOBDIR,$RUNDIR,$NCOBIN,$DEBUG,$RANGE,$NUM_DOMS,$CYC_INT,$this_cycle);

# Build from variables read in FLEXINPUT
  our ($SCIPUFF_EXE,$RESAMPLE_EXE,$MEDOC_EXE,$LATLON_EXE);

  my ($command,$line,$project);
  my ($path_to_files);
  my ($i,$n,$nf,$nh,$nwrf,$f,$ff);

  my (@fields,@MDL,@MDL1,@DOS);
  my (@PIDTMP,@ZMBPROC);
  my ($ccyymmddhh,,$ccyy,$mm,$dd,$hh,$mn,$ss);
  my ($dom,$number_chars);
  my ($wrf_file,$medoc_file,$geo_file);
  my ($wrf,$dos_in,$dos_out,$dos_out1,$dos_out_n,$dos_out_dn,$dos_missing);
  my ($TSTART,$TEND);
  my ($StartTime,$EndTime); 
  my ($xmin,$xmax,$ymin,$ymax);
  my (@tmp);
  
  # all purpose missing value flag
  my $missing_r = -9999.;

  # Name of the dosage field as it will appear in WRF file
  my $dosage_field = "DOSAGE";
  my $dosage_description = "SURFACE DOSAGE";
  my $dosage_units = "kg m-3 s";

  # This has to match _FillValue in program sfc_dosage_resample_nc.exe 
  my $dosage_missing = -9999.;

#------------------------------------------------------------------------------#

print "PID: $$\n";

print "\n$0 @ARGV\n";

$FLEXINPUT  =  $ENV{FLEXINPUT};

if (-e $FLEXINPUT)
{
  print "post_process_clean.pl: Using job configuration in $FLEXINPUT\n" if ($DEBUG);
}
else
{
 print "\nFile $FLEXINPUT is missing..... EXITING\n";
 exit(-1);
}

# This input file defines the configuration for the job
require $FLEXINPUT;

# This script contains the hh_advan_date and mm_advan_date subroutines
require $PERL_ARCHIVE.'/tools/advan_date.pl';

# This script contains the setEnvVars subroutine
require $PERL_FLEX.'/setEnvVars.pl';

# Set the environment variables
print "\nSetting env vars\n";
&setEnvVars();

# Construct paths
$LATLON_EXE   = "$EXECUTABLE_ARCHIVE/latlon_wrf.exe";
$MEDOC_EXE    = "$EXECUTABLE_ARCHIVE/MEDOCconverter.exe";
$SCIPUFF_EXE  = "$EXECUTABLE_ARCHIVE/runsci.exe";
$RESAMPLE_EXE = "$EXECUTABLE_ARCHIVE/sfc_dosage_resample_nc.exe";

#------------------------------------------------------------------------------#
# Open the log file in RUNDIR/this_cycle
#------------------------------------------------------------------------------#

print "\nRUNDIR = $RUNDIR\n";
print "\nthis_cycle = $this_cycle\n";

$path_to_files = "$RUNDIR/$this_cycle";

chdir $path_to_files;

print "\nWorkdir is $path_to_files\n";

#print "\nOpening the log file in $path_to_files/timing_post_process_scipuff\n";
#&makeRUNDIRLogFile("timing_post_process_scipuff");
#print FILEL "   Starting scipuff up at ", &ctime(time);
#print FILEL "   Ending scipuff up at ", &ctime(time);
#close(FILEL);

#------------------------------------------------------------------------------#
# Check the presence of the executables
#------------------------------------------------------------------------------#

foreach $f ($LATLON_EXE,$MEDOC_EXE,$SCIPUFF_EXE,$RESAMPLE_EXE){
     if (! -e $f){
         print "\n";
         print "ERROR in ${0}: Cannot find executable $f\n";
         exit -1;
     }
}

#------------------------------------------------------------------------------#
# Bring scipuff templates
#------------------------------------------------------------------------------#
print "\n";
print
"-----------------------------------------------------------------------------";
print "\n";
print "Setting scipuff input files:\n";
print "\n";


foreach $f ("scipuff.ini","scipuff.inp","scipuff.scn","scipuff.msc","landuse.dat"){
   $ff = "$GSJOBDIR/scipuff/$f";
   if (! -e $ff){
       print "\n";
       print "ERROR in ${0}: Cannot find file $ff\n";
       return -1;
   }
   $command = "cp -f $ff .";
   print "$command\n";
   system  ($command);

#  @tmp =  fileparse($f, qr/\.[^.]*/);
#  $command = "ln -s $ff GP$tmp[2]";
#  print "$command\n";
#  system  ($command);
}

#------------------------------------------------------------------------------#
# Read starting time and ending time in scipuff.inp
#------------------------------------------------------------------------------#
 
if (! open (INP,"scipuff.inp")){
    print "\n";
    print "ERROR in ${0}: Cannot open file scipuff.inp\n";
    `pwd`;
    system ("ls -al");
    return -1
}

$n = 0;
while ($line=<INP>){   

        if ($line =~ /TSTART/) {
            @fields = split(/\s+/,$line);
            $TSTART = sprintf ("%02i",$fields[3]); 
            $n ++;
        }

        if ($line =~ /TEND/ && $line !~ /TEND_HR/) {
            @fields = split(/\s+/,$line);
            $TEND = sprintf ("%02i",$fields[3]); 
            $n ++;
        }

}

close (INP);

if (! defined ($TSTART) || ! defined ($TEND)){ 
    print "ERROR in ${0}: Cannot open find variables TSTART and/or TEND in file scipuff.inp\n";
    exit -1
}

#------------------------------------------------------------------------------#
# Evaluate the SCIPUFF integration period (must be in the same day)
#------------------------------------------------------------------------------#

# this cycle actuall started CYC_INT hours earlier
$ccyymmddhh = &hh_advan_date ($this_cycle,-$CYC_INT);

$ccyy = substr($ccyymmddhh,0,4);
$mm   = substr($ccyymmddhh,4,2);
$dd   = substr($ccyymmddhh,6,2);
$hh   = substr($ccyymmddhh,8,2);
$mn   = "00";
$ss   = "00";

$StartTime = "${ccyy}${mm}${dd}${TSTART}";
$EndTime   = "${ccyy}${mm}${dd}${TEND}";

print "\n";
print "SCIPUFF start time is ${ccyy}/${mm}/${dd} ${TSTART}z\n";
print "SCIPUFF end time is   ${ccyy}/${mm}/${dd} ${TEND}z\n";

if ($EndTime <= $StartTime) {
    print "\nERROR in ${0}: start time must be previous to end time\n\n";
    exit -1;
}
#==============================================================================#
#                             Loop over WRF domains
#==============================================================================#

#------------------------------------------------------------------------------#
# Select the WRF domain 
#------------------------------------------------------------------------------#

# Highest resolution
foreach $dom (1..$NUM_DOMS)
{
print "\n";
print
"===========================================================================\n";
print "Processing domain $dom between ${TSTART}z and  ${TEND}z on ${ccyy}/${mm}/${dd}\n";
print "\n";

#------------------------------------------------------------------------------#
# Bring geogrid file
#------------------------------------------------------------------------------#

$geo_file  = sprintf ("geo_em.d%02d.nc",$dom);
$ff = "$GSJOBDIR/wps/$geo_file";

if (! -e $ff){
    print "\n";
    print "ERROR in ${0}: Cannot find file $ff\n";
    return -1;
}

print "\n";
$command = "cp -f $ff .";
print "$command\n";
system  ($command);


#------------------------------------------------------------------------------#
# Find the corners of the WRF domain
#------------------------------------------------------------------------------#

($xmin,$xmax,$ymin,$ymax) = &wrf_corners ($geo_file,$missing_r) ;

if ($xmin <= $missing_r || $xmax <= $missing_r ||
    $ymin <= $missing_r || $ymax <= $missing_r){
    print "ERROR: cannot determine the corners of the WRF domain\n";
    exit -1;
}
    

#------------------------------------------------------------------------------#
# Name of MEDOC file
#------------------------------------------------------------------------------#

#$medoc_file = "$GSJOBTYPE"."$GSJOBID"."_D${dom}_medoc.fmt";
# This is an internal file, keep an explicit name
$medoc_file = "D${dom}.${StartTime}-${EndTime}.medoc";

#------------------------------------------------------------------------------#
# Update scipuff.inp file with this cycle date
#------------------------------------------------------------------------------#

$command = "mv -f scipuff.inp scipuff.inp.tmp";
print  "$command\n";
system ($command);

if (! open (INP,"scipuff.inp.tmp")){
    print "\n";
    print "ERROR in ${0}: Cannot open for reading file scipuff.inp.tmp\n";
    exit -1;
}

if (! open (OUT,">scipuff.inp")){
    print "\n";
    print "ERROR in ${0}: Cannot open for writing file scipuff.inp\n";
    exit -1;
}

$n = 0;
while ($line=<INP>){   

        if ($line =~ /_YEAR_START_/) {
            $line =~ s/_YEAR_START_/$ccyy/;
        }
        if ($line =~ /_YEAR_END_/) {
            $line =~ s/_YEAR_END_/$ccyy/;
        }
        if ($line =~ /_MONTH_START_/) {
            $line =~ s/_MONTH_START_/$mm/;
        }
        if ($line =~ /_MONTH_END_/) {
            $line =~ s/_MONTH_END_/$mm/;
        }
        if ($line =~ /_DAY_START_/) {
            $line =~ s/_DAY_START_/$dd/;
        }
        if ($line =~ /_DAY_END/) {
            $line =~ s/_DAY_END_/$dd/;
        }
        if ($line =~ /_XMIN_/) {
            $line =~ s/_XMIN_/$xmin/;
        }
        if ($line =~ /_XMAX_/) {
            $line =~ s/_XMAX_/$xmax/;
        }
        if ($line =~ /_YMIN_/) {
            $line =~ s/_YMIN_/$ymin/;
        }
        if ($line =~ /_YMAX_/) {
            $line =~ s/_YMAX_/$ymax/;
        }
        print OUT $line;

}

close (INP);
close (OUT);

if (-e "scipuff.inp"){
    if (-e "scipuff.inp.tmp"){
        system ("rm -f scipuff.inp.tmp");
    }
}

#------------------------------------------------------------------------------#
# Update scipuff.msc file with this cycle date
#------------------------------------------------------------------------------#

$number_chars = sprintf "%03d",length($medoc_file);

$command = "mv -f scipuff.msc scipuff.msc.tmp";
print  "$command\n";
system ($command);

if (! open (INP,"scipuff.msc.tmp")){
    print "\n";
    print "ERROR in ${0}: Cannot open for reading file scipuff.msc.tmp\n";
    exit -1;
}

if (! open (OUT,">scipuff.msc")){
    print "\n";
    print "ERROR in ${0}: Cannot open for writing file scipuff.msc\n";
    exit -1;
}

$n = 0;
while ($line=<INP>){   

        if ($line =~  /_NUM_CHARS_/) {
            $line =~ s/_NUM_CHARS_/$number_chars/;
        }

        if ($line =~  /_MEDOC_FILE_/) {
            $line =~ s/_MEDOC_FILE_/$medoc_file/;
        }
        print OUT $line;

}

close (INP);
close (OUT);

if (-e "scipuff.msc"){
    if (-e "scipuff.msc.tmp"){
        system ("rm -f scipuff.msc.tmp"); 
    }
}

#------------------------------------------------------------------------------#
# List all WRF files needed for running scippuff for this domain
#------------------------------------------------------------------------------#

print "\n";
print
"-----------------------------------------------------------------------------";
print "\n";
print "Finding WRF files:\n";

$nf = 0;
$nh = 0;
$ccyymmddhh = $StartTime;

while ($ccyymmddhh <= $EndTime){

       $nh ++;
       $ccyy = substr($ccyymmddhh,0,4);
       $mm = substr($ccyymmddhh,4,2);
       $dd = substr($ccyymmddhh,6,2);
       $hh = substr($ccyymmddhh,8,2);
       $mn = "00";
       $ss = "00";

       # Look for the analysis first
       $wrf_file = sprintf ("wrfout_d%02d_${ccyy}-${mm}-${dd}_${hh}:${mn}:00.${RANGE}_F",$dom) ;

       if (! -e $wrf_file){

           print "\n";
           print "WARNING: Cannot find file $wrf_file\n";

           # then look for the forecast first
           $wrf_file = sprintf ("wrfout_d%02d_${ccyy}-${mm}-${dd}_${hh}:${mn}:00.${RANGE}_P+FCST",$dom) ;

           if (! -e $wrf_file){
               print "\n";
               print "ERROR: Cannot find file $wrf_file\n";
               $ccyymmddhh = &hh_advan_date ($ccyymmddhh,+1);
               next;
           }else{
               # Store the file name
               $MDL1[$nf] = $wrf_file;
               $nf ++;
           }

       }else{
           # Store the file name
           $MDL1[$nf] = $wrf_file;
           $nf ++;
       }

       $ccyymmddhh = &hh_advan_date ($ccyymmddhh,+1);
}

# Print the list of files
if ($#MDL1 < 0) {
    print "\n";
    print "ERROR: Cannot find a valid file between $StartTime to $EndTime on domain $dom\n"; 
    exit -1;
}
# Print the list of files

@MDL = sort @MDL1;
print "\n";
print "Found $nf files valid between $StartTime to $EndTime on domain ${dom}:\n"; 
print "\n";
foreach $f (@MDL){
   print "$path_to_files/$f\n";
}

#------------------------------------------------------------------------------#
# For all existing files, append dosage 2d variables filled of missing values
#------------------------------------------------------------------------------#
print "\n";
print
"-----------------------------------------------------------------------------";
print "\n";
print "Append $dosage_field ($dosage_description in $dosage_units) initialized with _FillValue = $dosage_missing";

foreach $f (@MDL){
        print "\n";
        print "$path_to_files/$f\n";
        &append_wrf_2dfield ($dosage_field,$dosage_description,$dosage_units,$dosage_missing,"$path_to_files/$f");
}

print "\n";
#------------------------------------------------------------------------------#
# Do not run scipuff if some WRF files are missing
#------------------------------------------------------------------------------#

if ($nf < $nh) {
    print "\n";
    print "ERROR: expected $nh files between $StartTime to $EndTime, found $nf\n"; 
    exit -1;
}

#------------------------------------------------------------------------------#
# Loop over the list of WRF files
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
    exit -1;
}

$n = 1;

foreach $f (@MDL) {

     chomp ($f);

     #-------------------------------------------------------------------------#
     # Check again if the file exists
     #-------------------------------------------------------------------------#

     if (! -e "$f"){
         print "\n";
         print "ERROR: Cannot find file $f\n";
         exit -1;
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

#------------------------------------------------------------------------------#
# Generate the config file for the MEDOC converter using the first WRF file
#------------------------------------------------------------------------------#

&medoc_cfg($MDL[0]);
#&medoc_cfg($geo_file);

if (! -e "MEDOCconverter.cfg"){
    print "ERROR: MEDOC config file MEDOCconverter.cfg was not generated\n";
    exit -1;
}


#-------------------------------------------------------------------------------
# Convert into MEDOC format
#-------------------------------------------------------------------------------
print "\n";
print
"---------------------------------------------------------------------------\n";
print "MEDOC conversion:\n";
print "\n";


system ("rm -f $medoc_file") if (-e $medoc_file);

$command = "$MEDOC_EXE $medoc_file 0 $nwrf 1";
print  "$command\n";
system ($command);

if (! -e $medoc_file){
    print "\n";
    print "ERROR: MEDOC file $path_to_files/$medoc_file was not created\n";
    exit -1;
}else{
    print "\n";
    print "MEDOC file $path_to_files/$medoc_file was created\n";
}

#-------------------------------------------------------------------------------
# Run  SCIPUFF
#-------------------------------------------------------------------------------
print "\n";
print
"---------------------------------------------------------------------------\n";
print "Running SCIPUFF on domain $dom between $StartTime and $EndTime\n";

$project = "scipuff";

$command ="time $SCIPUFF_EXE -I:scipuff.ini -P:${project}";
print "\n$command\n";
system($command);

$dos_in  = "${project}.dos";
#$dos_out = sprintf ("${project}_d%02d.dos",$dom);
$dos_out = sprintf ("GP.dos",$dom);

# Rename dosage file

if (! -e $dos_in){
    print "\n";
    print "ERROR: in $SCIPUFF_EXE, file $dos_in was not created\n";
    exit -1;
}else{
    $command ="mv -f $dos_in $dos_out";
    print "\n$command\n";
    system($command);
}

#-------------------------------------------------------------------------------
# Run surface dosage resampling onto WRF grid
#-------------------------------------------------------------------------------
print "\n";
print
"---------------------------------------------------------------------------\n";
print "Resampling SCIPUFF output on WRF grid $dom between $StartTime and $EndTime\n";
print "\n";


# Run resampling program
#$dos_out =  sprintf ("${project}_d%02d.dos",$dom);
# Take -log10 of dosage
# $command = "$RESAMPLE_EXE -dos $dos_out -geo $geo_file -log10";
# Use dosage as is
$command = "$RESAMPLE_EXE -dos $dos_out -geo $geo_file";
print "\n$command\n";
system($command);

# Expect at least 1 output

#$dos_out1 = sprintf ("${project}_d%02d.dos_01",$dom);
$dos_out1 = sprintf ("${dos_out}_01",$dom);

if (! -e $dos_out1){
    print "\n";
    print "ERROR: in resampling, Cannot find file $dos_out1\n";
    exit -1;
}

#-------------------------------------------------------------------------------
# Match WRF with dosage files and append dosage to WRF file
#-------------------------------------------------------------------------------
print "\n";
print
"---------------------------------------------------------------------------\n";
print "Appending SCIPUFF output on WRF grid $dom between $StartTime and $EndTime\n";

$n = 0;
$i = 0;

foreach $wrf (@MDL) {

     chomp ($wrf);

     #-------------------------------------------------------------------------#
     # Check again if the WRF file exists
     #-------------------------------------------------------------------------#

     if (! -e "$wrf"){
         print "\n";
         print "ERROR: Cannot find file $wrf\n";
         exit -1;
     }

     #-------------------------------------------------------------------------#
     # Check if the dosage file exists
     #-------------------------------------------------------------------------#

#    $dos_out = sprintf ("${project}_d%02d.dos_%02d",$dom,$n);
     $dos_out_n = sprintf ("${dos_out}_%02d",$n);
     $dos_missing = sprintf ("${project}_d%02d.dos_missing",$dom);

     # At release time, there's no (yet) a dosage field
     if (! -e "$dos_out_n"){
         print "\n";
         print "WARNING: Cannot find file $dos_out_n, will use missing value\n";

         # Create a single dosage file with all missing values
         $command = "$NCOBIN/bin/ncap2 -O -s \'$dosage_field=$dosage_field*0+$dosage_missing\' $dos_out1 $dos_missing";
         print "$command\n";
         system ($command);

         # If ncap2 failed, it leaves a *pidXXXX.ncap2.tmp file
         @PIDTMP = `find . -name '$dos_out1.pid*.ncap2.tmp' -print`;
         if ($#PIDTMP >= 0){
             chomp ($PIDTMP[0]);
             print "\nWARNING: detected file $PIDTMP[0]\n";

             # Check for zombies processes
             @ZMBPROC = `ps axo user,pid,ppid,command,s | grep -w Z\$`;
             if ($#ZMBPROC >= 0){
                 print "\nWARNING: detected zombie processes @ZMBPROC";
             }
         }

         # Append all missing value DOSAGE field to WRF file
         $command = "$NCOBIN/bin/ncks -A -v $dosage_field $dos_missing $wrf";
         print "$command\n";
         system ($command);


     }else{
         print "\n";
         print "INFO: Appending file $dos_out_n to $wrf\n";
         # Append the dosage field valid at this time to WRF file
         $command = "$NCOBIN/bin/ncks -A -v $dosage_field $dos_out_n $wrf";
         print "$command\n";
         system ($command);

         $dos_out_dn = sprintf ("d%02d_${dos_out}_%02d",$dom,$n);
         $command = "mv -f $dos_out_n $dos_out_dn";
         print "$command\n";
         system ($command);


         $i ++;
     }

     # If ncks failed, it leaves a *pidXXXX.ncks.tmp file
     @PIDTMP = `find . -name '$wrf.pid*.ncks.tmp' -print`;
     if ($#PIDTMP >= 0){
         chomp ($PIDTMP[0]);
         print "\nWARNING: detected file $PIDTMP[0]\n";

         # Check for zombies processes
         @ZMBPROC = `ps axo user,pid,ppid,command,s | grep -w Z\$`;
         if ($#ZMBPROC >= 0){
             print "\nWARNING: detected zombie processes @ZMBPROC";
         }
     }
     
     $n ++;

}

 print "\nINFO: Renaming scipuff output file:\n";
 $dos_out_dn = sprintf ("d%02d_${dos_out}",$dom);
 $command = "mv -f $dos_out $dos_out_dn";
 print "$command\n";
 system ($command);


print "\n";
print "Found $i dosage files valid between from $StartTime to $EndTime on domain $dom\n"; 
print
"---------------------------------------------------------------------------\n";

} # Loop over domains

#-------------------------------------------------------------------------------
# End
#-------------------------------------------------------------------------------
exit 0;

#-------------------------------------------------------------------------------
# Name: makeRUNDIRLogFile
# Arguments: name of the logfile
# Return: none
# Description: This method creates the 'timing' logfile in RUNDIR/this_cycle
#-------------------------------------------------------------------------------

sub makeRUNDIRLogFile
{
  my $fileName = $_[0];

  if(-e $RUNDIR."/".$this_cycle."/".$fileName)
  {
    system("rm $fileName");
  }
  select(FILEL);
  $| =1;
  open(FILEL, ">>$fileName") || do { warn "Can't open file $fileName: $!\n" };
  select(STDERR);
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

    our $LATLON_EXE;
    my ($file_in);
    my ($west_east,$south_north,$bottom_top);
    my ($l,$command);
    my ($stats);
    my (@tmp,@dims);

    $file_in = shift;
    $west_east   = 0;
    $south_north = 0;
    $bottom_top  = 0;

    $stats = 0;

    $command = "$LATLON_EXE $file_in -dims wrf_dims.txt";

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
1;
#------------------------------------------------------------------------------#
# Sub wrf_corners: Find the dimensions of a WRF output file
# Use executable latlon_wrf.exe
# return west_east,south_north,bottom_top dimensions
#------------------------------------------------------------------------------#

sub wrf_corners
{
    use strict;

    our $LATLON_EXE;
    my ($file_in,$missing_r);
    my ($west_east,$south_north,$bottom_top);
    my ($l,$command);
    my ($stats);
    my (@tmp,@dims);
    my ($lonmin,$lonmax,$latmin,$latmax);

    $file_in = shift;
    $missing_r = shift;
    $west_east   = 0;
    $south_north = 0;
    $bottom_top  = 0;

    $stats = 0;

    $command = "$LATLON_EXE $file_in -latlon_r latlon_r.txt";
    print "\n-------------------------------------------------------------\n";
    print "\n$command\n\n";
    system  ($command);

    if (-e "latlon_r.txt" && ! -z "latlon_r.txt") {

        if (! open (WRF_DIMS,"latlon_r.txt")){
            print "\nERROR: Cannot open wrf_dims.txt for reading:\n\n";
        }else{

        $latmin = $missing_r;
        $latmax = $missing_r;
        $lonmin = $missing_r;
        $lonmax = $missing_r;

#latlon_r.txt:
# latmin  latmax
# lonmin  lonmax 

            $l = 0;

            $line = <WRF_DIMS>;
            chomp ($line);
            @tmp = split (' ',$line);
            if ($#tmp >= 1){
                $latmin = $tmp[0];
                $latmax = $tmp[1];
            }else{
                print "\nERROR: reading line 1 of file latlon_r.txt\n\n";
            }

            $line = <WRF_DIMS>;
            chomp ($line);
            @tmp = split (' ',$line);
            if ($#tmp >= 1){
                $lonmin = $tmp[0];
                $lonmax = $tmp[1];
            }else{
                print "\nERROR: reading line 2 of file latlon_r.txt\n\n";
            }

        }

    }else{
         print "\nERROR: command $command failed\n\n";
    }

    return ($lonmin,$lonmax,$latmin,$latmax);
}
#------------------------------------------------------------------------------#
sub append_wrf_2dfield
{

  my ($field_name,$field_description,$field_units,$field_FillValue,$file_in) = @_;

  my ($command);

       # Create a new field in WRF file
       $command = "$NCOBIN/bin/ncap -h -O -s \"$field_name=T2\" $file_in $file_in";
       print  "$command\n";
       system ($command);

      # Reset the new field with missing value
      $command = "$NCOBIN/bin/ncap2 -O -s \'$field_name=$field_name*0+$field_FillValue\' $file_in $file_in";
       print  "$command\n";
       system ($command);

     # Append attribute to new field
     $command = "$NCOBIN/bin/ncatted -h -O".
     " -a FieldType,$field_name,o,c,104".
     " -a MemoryOrder,$field_name,o,c,\"XY \"".
     " -a description,$field_name,o,c,\"$field_description\"".
     " -a units,$field_name,o,c,\"$field_units\"".
     " -a stagger,$field_name,o,c,\" \"".
     " -a coordinates,$field_name,o,c,\"XLONG XLAT\"".
     " -a _FillValue,$field_name,o,f,$field_FillValue".
     " $file_in";

       print  "$command\n";
       system ($command);


 return (0);
}

1;

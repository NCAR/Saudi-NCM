#!/usr/bin/perl -w
#------------------------------------------------------------------------------#
#                        wrfv2_nml.pl
#
# From a WRF V2 or V3 namelist, create the minimal WRF V2 namelist needed
# to run WRFVAR V2.
#
# Beware: WRFVAR V2 requires 2 namelists: a regular WRF namelist 
# (namelist.input) for the model parameters and an additional namelist 
# (namelist.3dvar) for the assimilaiton parameters. This script does not 
# create the second namelist, but it generates the first namelist, 
# ie a regular WRF V2 namelist. This later needs only to contain limited
# inforation, namely the dimensions: e_we, e_sn, e_vert, dx, dy. 
# The parameter dyn_opt has been dropped from the V2 namelist in V3
# so it has to be added. Its value is hard-coded in this script (DYN_OPT)
# and set to 2 (ARW). 
#
# WRFVAR cannot handle nesting, therefore the dimensions of the domain that
# is processed should be first in the namelist entries.
#
# Output is file namelist.input<DD> where DD is the domain ID, eg: 01, 02, etc
#
# Usage:  wrfv2_nml.pl namelist=wrf.template domain=3;
# -----
# To create the minimal WRFVAR V2 namelist for domain "domain"
# from the WRF V2 or V3 namelist "wrf.template"
#
# Enter "wrfv2_nml.pl" for an on-line help.
#
#------------------------------------------------------------------------------#
# Copyright UCAR (c) 2008
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Program (RAP),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#
# Francois Vandenberghe, vandenb@ucar.edu, October 2008.
#------------------------------------------------------------------------------#
# dyn_opt was dropped from the V2 namelist in V3, so it has to be created
# For simplicity the value DYN_OPT hard coded below is always used, whatever
# if dyn_opt is present or not in the input namelist.

$DYN_OPT = 2;

#------------------------------------------------------------------------------#
# Parse arguments

if ($#ARGV < 0) {
    print "$0 @ARGV\n";
    print "\nERROR: missing arguments.\n";
    &help;
}

$filein = "missing";
$domain = 0;
$DEBUG =   "no";

for ($n = 0; $n <= $#ARGV; $n++) {

     if ($ARGV[$n] eq  "debug" ) { # debug option
         $DEBUG =   "-debug";
     }elsif ($ARGV[$n] eq  "help" ){
        &help;
     }

     if (index ($ARGV[$n],"=") < 0) {
         print "$0 $ARGV[$n]\n";
         print "\nERROR: bad argument.\n";
         &help;
     }

# Arguments are of the form KEY=value

     ($dummy1,$dummy2) = split('=',$ARGV[$n]);

# Expect namelist template file name after keyword nml=

     if ($dummy1 eq 'namelist') {

         $filein  = $dummy2 ;

         if (length($filein) <= 0) {
             print "$0 $ARGV[$n]\n";
             print "\nERROR: keyword must be specified after namelist=, eg 1, 2,...\n";
             &help;
          }

         if (! -e $filein) {
             print "$0 $ARGV[$n]\n";
             print "\nERROR: cannot find file $filein.\n";
             &help;
         }

     }

# Expect id of which domain to process after keyword dom=

     if ($dummy1 eq 'domain') {

         $domain  = $dummy2 ;

         if (length($domain) <= 0) {
             print "$0 $ARGV[$n]\n";
             print "\nERROR: keyword must be specified after domain=, eg 1, 2,...\n";
             &help;
          }
     }

}

#------------------------------------------------------------------------------#
# Open input/output files
#------------------------------------------------------------------------------#
# Output file name is "namelist.inputDD" where DD is the domain id: 01, 02, etc.

$fileout = sprintf ("namelist.input%2.2i",$domain);

if ($filein =~ $fileout) {
    print "$0 $ARGV[$n]\n";
    print "\nERROR: same input and output files: $filein\n\n";
    exit -1;
}

# Open input file

if (! open(IN,"$filein")) {
     print "\nCannot open for read input file: \"$filein\"\n\n";
     exit -1;
}

if (! open(OU,">$fileout")){
     print "\nCannot open for write output file: \"$fileout\"\n\n";
     exit -1;
}

#------------------------------------------------------------------------------#
# Read input file and write output file
#------------------------------------------------------------------------------#

print "\nCreating WRFVAR V2 namelist file for domain $domain\n"; 

while($aline=<IN>) {

#print "$aline";
 chomp ($aline);

# Need to write records names
 if($aline =~ "&") {

    $bline = $aline;
    print OU "$bline\n";

# dyn_opt was dropped in V3 namelist, force arbitrary to write to 2 (ARW)
    if($aline =~ "dynamics") {
       $bline = " dyn_opt = $DYN_OPT,";
       print OU "$bline\n";
     }

 }

# Need to write records ends

 if($aline =~ "/") {
    $bline = $aline;
    print OU "$bline\n";
 }

# Need to dimensions, nesting and grid size

 if($aline =~ "e_we" || $aline =~ "e_sn" || $aline =~ "e_vert" || $aline =~ "dx " || $aline =~ "dy  ") {

#   print "$aline\n";

# Find the record that corresponds to the requested domain

# Split data on the "=" sign

   ($lefth,$righth) = split ('=',$aline,2);

# Split the left hand side of the data on "," signs

   @doms_data = split (',',$righth);

# The last word is what we want

  $bline = "$lefth = $doms_data[$domain-1],";
 
  print "Add line $bline in file $fileout\n";

  print OU "$bline\n";

  }

  
}

#------------------------------------------------------------------------------#
# Close files
#------------------------------------------------------------------------------#

  
close (IN);
close (OU);

print "WRF V2 namelist file $fileout has been created\n\n"; 

exit 0;

#------------------------------------------------------------------------------#
# Subroutines
#------------------------------------------------------------------------------#
sub help {
         print ("\n\nUsage:\n");
         print ("-----\n");
         print ("\n wrfvarv2_nml.pl namelist=wrf.template domain=3\n");
         print ("\n  To create the minimal WRFVAR V2 namelist for domain \"domain\"\n");
         print ("  from a WRF V2 or V3 namelist \"wrf.template\"\n");
         print ("\n  Output if file namelist.input<DD> where DD is the domain ID, eg: 01, 02, etc.\n");
         print ("\n");

         exit 1;
}
#------------------------------------------------------------------------------#

1;


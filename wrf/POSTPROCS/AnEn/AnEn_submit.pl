#!/usr/bin/perl

####################################################
# Manual use:  You need to setenv FLEXINPUT 
# to the flexinput you desire and then source
# the env_vars.csh you want i.e. GWDPG/flexinput.pl
# and GWDPG/scripts/env_vars.csh
####################################################
use Time::Local;
use Getopt::Std;
getopts('c:');

$DEBUG = 1;
$FLEXINPUT = $ENV{FLEXINPUT};


if (-e $FLEXINPUT)
{
  print "AnEn_submit.pl: Using job configuration in $FLEXINPUT\n" if ($DEBUG);
}
else
{
  print "\nFile $FLEXINPUT is missing..... EXITING\n";
  exit(-1);
}

#Import job definition environment variables
require $FLEXINPUT;
require "$GSJOBDIR/postprocinput.pl";

#Variables needed from flexinput
#$GSJOBID
#$MM5HOME

print "JOBID $GSJOBID \n";


#Get single command line option = $this_cycle
#This will be passed via submitCycle or by hand
if($opt_c) 
{
  $this_cycle = $opt_c;
  print "Use cycle tag $this_cycle passed in from the command line\n";
} 
else 
{
  #$this_cycle is passed in via the temporary cshrc when used 
  #with submitCycleMM.csh
  #otherwise you need to pass it in from the command line

  #$this_cycle =  sprintf("%04d%02d%02d%02d",$yy,$mm+1,$dd,$hh);
}

if ( ! -e "$RUNDIR/${this_cycle}" ) 
{
   print "   \n The cycle ${this_cycle} does not exist \n. Something wrong. -- Exit.";
}

if ($this_cycle eq "" )
{
  $this_cycle = $ENV{this_cycle};
}


$AnEn                = "$MM5HOME/cycle_code/POSTPROCS/AnEn/AnEn_driver.py";
$AnEn_distrib        = "$MM5HOME/cycle_code/POSTPROCS/AnEn/AnEn_distrib.py";

#After the images are created we need to package up the 5 cycles with updated images
#and send them to a distrib area (if $DO_DISTRIB is true)
if (! defined($DISTRIB_ROOT)) {  # best if defined in postprocinput.pl
  $DISTRIB_ROOT = "/model/$ENV{LOGNAME}/distrib";
  }

if ($DO_DISTRIB and $DO_TAR_SUM_FOR_DISTRIB)
{
  print " $AnEn -c $this_cycle -r $GSJOBID -e $MM5HOME -d $DISTRIB_ROOT\n";
  system("$AnEn -c $this_cycle -r $GSJOBID -e $MM5HOME -d $DISTRIB_ROOT");
}
else
{
  print " $AnEn -c $this_cycle -r $GSJOBID -e $MM5HOME\n";
  system("$AnEn -c $this_cycle -r $GSJOBID -e $MM5HOME");
}


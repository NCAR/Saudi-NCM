#!/usr/bin/perl

###############################################################################
#
# Usage: ./coldstart_rtfdda3hcyc.pl FULL_PATH_INPUT_FILE
#
#    FULL_PATH_INPUT_FILE should be 'GSJOBDIR'/flexinput.pl.
#
#    This script uses the follwoing parameters in flexinput.pl:
#          $MODEL
#          $RUNDIR
#          $RESTART_ROOT
#    Please make sure that they are set corectly.
#
################################################################################

if ( !$ARGV[0] )
{
   print "Usage: ./coldstart_rtfdda3hcyc.pl FULL_PATH_INPUT_FILE\n";
   print "Command line argument not supplied.... exiting\n";
   exit(-1);
}

if ( -e $ARGV[0])
{
  require $ARGV[0];
}
else
{
  print "Can't find $ARGV[0], 'RUNDIR', 'RESTART_ROOT', etc. not set... exiting\n";
  exit(-1);
}

# Remove the critic.time file:
$critic_file = $RUNDIR."/critic.time";

print "Remove critic.time file : $critic_file\n";
system ("rm -f $critic_file");

if ($MODEL eq "MM5")
{
  $restart_files = $restart_files."/r-*";
  system ("rm -rf $restart_files");
}
else
{

# Rename the restart files

  chdir "$RUNDIR";

  foreach $dir (<*>) {
    next if ( ! -d $dir );
    $last_cycle = $dir if ( $dir =~ /\d{10}/ );
  }

  print "last cycle is $last_cycle\n";
  print "RESTART_PER_CORE $RESTART_PER_CORE\n";

  if ( -e "$RUNDIR/$last_cycle/WRF_F" ) {
     print "Entering $RUNDIR/$last_cycle/WRF_F\n";
     chdir "$RUNDIR/$last_cycle/WRF_F";
     if ( $RESTART_PER_CORE ) {
        foreach $r (<r-*>) {
          next if(-l $r);
          rename($r,"x$r");
        }
     } else {
        foreach $r (<wrfrst*>) {
          next if(-l $r);
          rename($r,"x$r");
        }
     }
  }
}

exit(0);

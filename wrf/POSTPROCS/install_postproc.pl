#!/usr/bin/perl
#
# Install the postprocessors for a range or GMOD system on a remote machine
#
if ( $#ARGV == 1 ) {
   $JOB_ID  =  @ARGV[0];
   $DEST_HOST =  @ARGV[1];
} else {
   print ( " Usage:   $0  JOB_ID DEST_HOST\n");
   exit (1);
}

$RANGE = substr($JOB_ID,2,5);

system("rsync -e ssh -avzC /data/fddahome_dev/cycle_code/POSTPROCS $DEST_HOST:/data/fddahome/cycle_code");
system("scp /data/fddahome_dev/cycle_code/CUSTOM/$JOB_ID/rtfdda_postproc.pl $DEST_HOST:/data/GMODJOBS/$JOB_ID");
system("scp /data/fddahome_dev/cycle_code/CUSTOM/$JOB_ID/postprocinput.pl $DEST_HOST:/data/GMODJOBS/$JOB_ID");
system("scp /data/fddahome_dev/cycle_code/CONSTANT_FILES/RIP/namelists/\*$RANGE\*  $DEST_HOST:/data/fddahome/cycle_code/CONSTANT_FILES/RIP/namelists");

exit;


#
# convert to grib and copy to date-stamped cycle-dir on web-server
#
# @args - 0 - hourly file
# @args - 1 - Destination string for scp command
#
sub do_grib
{
    my ($hourly_file,$dest) = @_;

   chomp $hourly_file;
   chdir $WORK_DIR;
   print "file is ${WORK_DIR}/${hourly_file}\n";
   if(-e ${hourly_file}) {
      $command = "$MM52GRIB -params params.MM5toGrib -mode ARCHIVE -f $hourly_file";
      system("$command");
      $grib_file = "${hourly_file}.grib";
      system("mv output.grib $grib_file); 
      system( "echo rsync {grib_file} $dest/.");
      system("rsync -e 'ssh -i $KEY' -avzC $grib_file $dest/.");
   } else {
    print "${hourly_file} does not exist!\n";
   }

   chdir "$RUNDIR/$THIS_CYCLE";
}
1;

#
# scp the hourly files to the web server for interactive-naps
#
# @args - 0 - hourly file
# @args - 1 - Destination string for scp command
#
sub do_naps
{
  if ( $this_domain == 3 ) {
   sleep 20;
  }
    my ($hourly_file,$dest,$cycle_tag,$meta_dest,$is_final) = @_;

   chomp $hourly_file;
   chdir $WORK_DIR;
   print "file is ${hourly_file}\n";
   if(-e ${hourly_file}) {
    if ( $IS_WRF ) {
      $file=basename $hourly_file;
      $name=substr($file,0,3);
      $hourly=substr($file,11,19);
      $meta_file = "./" . $hourly . ".txt";
      open(TXT,">$meta_file");
      print TXT "${cycle_tag}#${is_final}\n";
      close (TXT);
      if ( $name eq "wrf" ) {
        system( "echo rsync ${hourly_file} $dest/.");
        system("chmod g+w $meta_file ");
        system("chmod g+w $hourly_file ");
        system("rsync -e 'ssh -i $KEY' -avzC $meta_file $meta_dest/.");
        system("rsync -e 'ssh -i $KEY' -avzC $hourly_file $dest/.");
      }
    } else {
      @parts = split("/",$hourly_file);
      $hour_name = @parts[$#parts];
      $hourly = substr($hour_name,0,12);
      $meta_file = $hourly . ".txt";
      open(TXT,">$meta_file");
      print TXT "${cycle_tag}#${is_final}\n";
      close (TXT);
      system( "echo rsync {hourly_file} $dest/.");
      system("chmod g+w $meta_file ");
      system("chmod g+w $hourly_file ");
      system("rsync -e 'ssh -i $KEY' -avzC $meta_file $meta_dest/.");
      system("rsync -e 'ssh -i $KEY' -avzC $hourly_file $dest/.");
    }
   } else {
    print "${hourly_file} does not exist!\n";
   }

   chdir "$RUNDIR/$THIS_CYCLE";
}
1;

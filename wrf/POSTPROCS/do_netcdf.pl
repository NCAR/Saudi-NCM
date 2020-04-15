
sub do_netcdf
{
   my ($hourly_file,$distrib_dir,$this_cycle) = @_;
   print "\nin do_netcdf $hourly_file, $distrib_dir, $this_cycle\n\n";

   chomp $hourly_file;
   $work_dir = "$distrib_dir/netcdf/$this_cycle";
   system( "mkdir -p $work_dir" );

   chdir $work_dir;
   print "file is $hourly_file\n";
   if (-e ${hourly_file}) {
      print "cp $hourly_file $work_dir/$hourly_dest_file\n";
      $hourly_dest_file = substr($hourly_file, -30, -6);
      system( "cp $hourly_file $work_dir/$hourly_dest_file");
     if ( $DO_TAR_SUM_FOR_DISTRIB ) {
          $file_name = basename $hourly_file;
          print "sha256sum $file_name > ${file_name}.sum\n";
          system ( "sha256sum $file_name > ${file_name}.sum");
      }
   } else {
    print "$hourly_file does not exist!\n";
   }

   print "\nexiting do_netcdf $hourly_file, $distrib_dir, $this_cycle\n\n";
}
1;


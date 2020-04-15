#
# scp the hourly files to the web server for mdv conversion
#
# @args - 0 - hourly file
# @args - 1 - Destination string for scp command
#
sub do_mdv
{
  if ( $this_domain == 3 ) {
   sleep 5;
  }
    my ($hourly_file,$dest,$dest_server,$dest_dir) = @_;

   chomp $hourly_file;
   chdir $WORK_DIR;
   print "file is ${WORK_DIR}/${hourly_file}\n";
   if(-e ${hourly_file}) {
    system( "echo scp ${hourly_file} $dest/.");
    system( "scp ${hourly_file} $dest/.");
    split /\//, $hourly_file;
    $file_name = @_[ @_ - 1 ];
    system("ssh $dest_server LdataWriter -dir $dest_dir -dtype raw -info1 DPG -info2 ${file_name}");
   } else {
    print "${hourly_file} does not exist!\n";
   }

}
1;

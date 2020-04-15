
sub do_plots_tarsum
{
   my ($plots_dir, $this_cycle, $valid_t, $distrib_dir) = @_;
   print "\nin do_plots_tarsum $plots_dir, $this_cycle, $valid_t, $distrib_dir\n\n";

   $work_dir = "$distrib_dir/images/$this_cycle/gifs";
   system( "mkdir -p $work_dir" );

   chdir "$plots_dir/gifs_ugui";
   print "directory is $valid_t\n";
   if (-e $valid_t) {
   print "tarring and summing $valid_t\n";
   system("tar -czvf $valid_t.tgz $valid_t");
   system("sha256sum $valid_t.tgz > $valid_t.tgz.sum");
   system("cp $valid_t.tgz* $work_dir ");
#   system("rm $valid_t.tgz*");
   } else {
    print "directory $valid_t does not exist!\n";
   }

   print "\nexiting do_plots_tarsum $plots_dir, $this_cycle, $valid_t, $distrib_dir\n\n";
}
1;


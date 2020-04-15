sub MustHaveDir {
  use Cwd;

  my @dirs=@_;
  my ($path,$oldtop,$topdir);

  umask 022;
  foreach $path (@dirs) {
    if($path !~ /^\//) {
      $cwd=cwd;
      $path=$cwd . '/' . $path;
    }
    $oldtop='';

    if($path=~ /\+/) {
      die "'+' is not allowed in path names\n";
    }
    
    while($path=~ /^(\/[^\/]+)/) {
      $topdir=$oldtop . $1;
      mkdir $topdir, 0777 if(! -e $topdir);
      $path=~ s/^$1//;
      $oldtop=$topdir;
      last if(! $path);
    }
  }
}
1;

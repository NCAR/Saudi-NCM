#!/usr/bin/perl

#
# Usage: &makeSubDirs(FULLPATH_DIRECTORY)
#
# For making a directory in the current directory:
#   &makeSubDirs("./new_dir_path")
#

sub makeSubDirs
{
  (my $full_dir_path) = @_ ;

  if (!$full_dir_path)
  {
    print "\nExiting makeSubDirs($full_dir_path): full_dir_path is empty\n";
    return;
  }

  if (!$MustHaveDir)
  {
    $MustHaveDir = "/data/fddahome/cycle_code/EXECUTABLE_ARCHIVE/MustHaveDir";
  }

  @dirs = split ('/', $full_dir_path);

  $length = @dirs;
  $i = 0;
  $make_new_dir = "/";

  for($i = 0; $i < $length; $i++)
  {
    if ($dirs[$i])
    {
      if ($dirs[$i] eq '.' && $i==0)
      {
        $make_new_dir = "./"
      }
      else
      {
        $make_new_dir = $make_new_dir.$dirs[$i]."/";
        system ("$MustHaveDir $make_new_dir");
      }
    }
  }
}
1;

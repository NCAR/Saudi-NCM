#!/usr/bin/perl
use strict;
no strict 'refs';
use vars qw($opt_p $opt_D);
use Getopt::Std;
use XML::Simple;
use FileHandle;

getopts('p:d');

my $param;

if($opt_p) {
  $param = XMLin("$opt_p");
} else {
  print "$0 -p <XML param file> [-D]\n";
  print "  -D, optional, turns on debug mode\n";
  exit;
}

my $root_dir = $param->{inputDir};
my $out_dir  = $param->{outputDir};
my $EXECUTABLE_ARCHIVE = $param->{EXECUTABLE_ARCHIVE};
my $decoder = "$EXECUTABLE_ARCHIVE/gtsdecoder.exe";
my $CONSTANT_FILES = $param->{CONSTANT_FILES};
my $GTS_STTNID = "$CONSTANT_FILES/gts_sttnid_final";
my $GTS_STTNID_ICAO = "$CONSTANT_FILES/gts_sttnid_final.icao";

if (! -e "$decoder") {
   die "GTS decoder $decoder does not exist!\n";
}

if (! -e "$GTS_STTNID") {
   die "GTS station ID file $GTS_STTNID does not exist!\n";
}

if (! -e "$GTS_STTNID_ICAO") {
   die "GTS ICAO station ID file $GTS_STTNID_ICAO does not exist!\n";
}

system("mkdir -p $out_dir");

my $data;
my $dir;
my $status;
my $new_file;

foreach $data ( @{$param->{subscribedDir}} ) {
   $dir = "$root_dir/$data";
   $status = chdir "$dir";
   die "Failed to change directory to $dir\n" if (! $status );
   print "working in $dir\n";

   if (-s 'new.asc') {
      print "INFO: Decoding of $data begins...\n";
      system("rm -rf tmp") if ( -d 'tmp' );
      system("mkdir tmp");
      open(IN,'new.asc');
      while ($new_file=<IN>) {
         chomp($new_file);
         print "new file = $new_file\n";
         system("cat $new_file >> tmp/gts_data");
      }
      close(IN);
     
      chdir 'tmp';

      &rewrite($dir);

      system("ln -s $GTS_STTNID .");
      system("ln -s $GTS_STTNID_ICAO .");

      open(DECODE,"| $decoder");
      print DECODE `date --date "3 hours" +%Y%m%d%H`;
      close(DECODE);

      system("cat gts_out.7?? > one_big_gts");

      &split_file($dir);

      &concat();

      chdir '..';
      rename('new.asc','new.asc.old');
   } else {
      print "WARN: file new.asc does not exist. Decoding of $data skipped!\n";
   } 

}

exit;

sub rewrite {

  my $dir = $_[0];

  if (open(IN,'gts_data')) {
  } else {
    print "WARN: Cannot open required file $dir/tmp/gts_data. Skip decoding $data data!\n";
    return;
  }

  open(OUT,">gts_data.tmp");

  while (<IN>) {
    if (/####/) {
       /(\d+)/;
       $_ = $1."\n";
       if ($. > 1) {
          print OUT "\cM\cM\n";
          print OUT "\3\1\cM\cM\n";
       } else {
          print OUT "\1\cM\cM\n";
       }
     } else {
     }
     print OUT;
  }
  print OUT "\cM\cM\n";
  print OUT "\3\n";
  close(IN);
  close(OUT);

  system("mv gts_data.tmp gts_data");

  return;
}

sub split_file {

  my $dir = $_[0];

  if (open(IN,'one_big_gts')) {
  } else {
    print "WARN: Could not open required file $dir/tmp/gts_data.tmp. Skip decoding $dir data.\n";
    return;
  }

  my $previous="";
  my $new = 1;
  my $yyyymmddhh;
  my %fh;
  my $hh;
  my $keep;

  while (<IN>) {
    if ($new) {
       $yyyymmddhh=substr($_,326,10);
       print "$yyyymmddhh\n";
       $hh=substr($yyyymmddhh,-2);
       if ($hh > 23) {
          $keep=0;
       } else {
          $keep=1;
          if ($fh{$yyyymmddhh}) {
          } else {
             $fh{$yyyymmddhh} = FileHandle->new(">>${yyyymmddhh}.decoded");
          }
       }
    }
    print { $fh{$yyyymmddhh} } "$_" if($keep);

    $new = 0;

    $new = 1 if ($previous=~ /^-777777/);

    $previous = $_;
  }
  close(IN);

  return;
}

sub concat {

  my $decoded_file;
  my $gzfile;
  my $file;

  foreach $decoded_file (<*.decoded>) {
     $file = "ncep_" . $decoded_file;
     $gzfile = "ncep_" . $decoded_file . '.gz';
     if ( -s "$out_dir/$gzfile") {
        system("cp $out_dir/$gzfile .");
        system("gunzip $gzfile");
     }
     system("cat $decoded_file >> $file");
     system("gzip $file");
     system("mv $gzfile $out_dir/.$gzfile");
     system("mv $out_dir/.$gzfile $out_dir/$gzfile");
  }

  return;
}

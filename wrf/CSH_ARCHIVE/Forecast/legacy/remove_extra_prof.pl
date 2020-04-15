#!/usr/bin/perl
#--- obsproc.exe generates an output file, ob.ascii (also ob<nn>.ascii if
#--- obsproc.mpich is used), for use with WRFDA 3DVAR. In ob.ascii, if there
#--- is any profiler data with valid time beyond the intended analysis time,
#--- MPI version of da_wrfvar.exe will hang (Intel) or segfault (PGI).
#--- This program is designed to take these (normally just one) profilers out
#--- to avoid hanging or segfaulting. 
use strict;
no strict 'refs';
use vars qw($opt_f $opt_t);
use Getopt::Std;
getopts('f:t:');
use Time::Local;

my $infile;
my $timeMax;
my $prof_time;
my $write;

if ($opt_f) {
   $infile = $opt_f;
} else {
   die "Usage: $0 -f <input file> -t <max time in yyyymmddhh>\n";
}
if ($opt_t) {
   $timeMax = $opt_t;
} else {
   die "Usage: $0 -f <input file> -t <max time in yyyymmddhh>\n";
}

my $year_max = substr($timeMax,0,4);
my $month_max = substr($timeMax,4,2);
my $day_max = substr($timeMax,6,2);
my $hour_max = substr($timeMax,8,2);
my $minute_max = 0;
my $second_max = 0;
my $secsMax = timegm($second_max,$minute_max,$hour_max,$day_max,$month_max-1,$year_max-1900);
my $found_extra_profiler = 0;

open(IN,"$infile");

CHECK_EXTRA_PROFILER: {
  while (<IN>) {
    next unless (/[a-zA-Z#]/);
    if (/SYNOP\s+(\d+)-(\d+)-(\d+)_(\d+):(\d+):(\d+)\s+PROFILER/) {
      #print "  remove_extra_prof.pl   ------- $_ \n";
      $prof_time = timegm($6,$5,$4,$3,$2-1,$1-1900);
      if ($prof_time > $secsMax) {
        $found_extra_profiler = 1;
        last CHECK_EXTRA_PROFILER;
      }
    }
  }
}

if ($found_extra_profiler) {
  my $removed_count = 0;
  my $tmp_output = "${infile}.tmp";
  
  seek(IN, 0, 0);
  open(OUT,">${tmp_output}");
  
  while (<IN>) {
    $write = 1 if (/[a-zA-Z#]/);
    if (/SYNOP\s+(\d+)-(\d+)-(\d+)_(\d+):(\d+):(\d+)\s+PROFILER/) {
      #print "  remove_extra_prof.pl   ------- $_ \n";
      $prof_time = timegm($6,$5,$4,$3,$2-1,$1-1900);
      if ($prof_time > $secsMax) {
        print "      remove_extra_prof.pl: found extra profiler: $1 $2 $3 $4 $5 $6 $prof_time\n";
        $write = 0;
      }
    }
    #print "$write\n";
    if($write) {
      print OUT $_ if($write);
    }
    else {
      $removed_count++;
    }
  }
  close(OUT);
  
  if ($removed_count > 0) {
    rename $infile, "${infile}.org";
    rename $tmp_output, $infile;
    print "      remove_extra_prof.pl: removed $removed_count lines\n";
  }
}
close(IN);


exit;

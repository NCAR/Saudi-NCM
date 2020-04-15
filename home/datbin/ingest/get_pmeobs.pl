#!/usr/bin/perl -s

use strict;

my $user = "metpme";
my $host = "saudi-c2";
my $key = "/home/pmeop/.ssh/saudi-c2-metpme-obs.rsa";
my $remote_dir = "/raid/input_pme/observations";
my $local_dir = "/scratch/project/k1206/datainput_raw/pme_obs";
my $today_command;
my $yesterday_command;

my ($ss , $mn , $hh , $dd , $mm , $yy , $wday , $yday , $isdst );

# Julian day and year for current GMT time
       ($ss , $mn , $hh , $dd , $mm , $yy , $wday , $yday , $isdst ) = gmtime(time);

# PERL index begins at 0, increment month of one unit and set year as ccyyy
        $mm   = $mm + 1;    # Month 01, 02,.. 12
        $wday = $wday + 1;  # Week day 1 = Sunday
my        $ccyy = $yy + 1900; # Year ccyy

my        $year = $ccyy;
my        $jday = $yday+1;      # Julian day

my $today = sprintf("%04d%02d%02d%02d", $year, $mm, $dd, $hh);
my $yesterday = `~/datbin/advance_cymdh.pl $today -24`;
$today = substr($today, 0, 8);
$yesterday = substr($yesterday, 0, 8);
print "today = $today\n";
print "yesterday = $yesterday\n";

print "Start time is:  ";
system ("date");
print "\n\n";

$today_command = ("rsync -av -e '/usr/bin/ssh -i $key' $user\@$host:$remote_dir/*$today* $local_dir");
  print "$today_command\n";
  system ($today_command);

$yesterday_command = ("rsync -av -e '/usr/bin/ssh -i $key' $user\@$host:$remote_dir/*$yesterday* $local_dir");
  print "$yesterday_command\n";
  system ($yesterday_command);

print "End time is:  ";
system ("date");
print "\n\n";

exit;


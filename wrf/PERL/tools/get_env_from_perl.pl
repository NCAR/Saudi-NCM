#!/usr/bin/perl
#use Cwd;
use File::Basename;
 
$debug = 0;
#$debug = 1;

if($#ARGV == -1) {
  print STDERR  "Usage: $0 [perl_script_name] env_var_name\n";
  exit(10);
}

my $cur_dir = getcwd;

my $perl_script = $ENV{FLEXINPUT};
$env_name = shift(@ARGV);
if ( -f $env_name ) {
  $perl_script = $env_name;
  $env_name = shift(@ARGV);
}
my $perl_dir = dirname($perl_script);
my $perl_name = basename($perl_script);
if ($debug) {
  print STDERR "  perl_dir: $perl_dir, perl_name: $perl_name, env_name: $env_name\n";
}

chdir $perl_dir;

open (MY_LOG, ">/tmp/" . basename($0) . "." . $ENV{LOGNAME} . ".err") or die "Can't open log file: $!";

*STDERR = *MY_LOG;
select MY_LOG;
require "$perl_name" if ($perl_name ne "");
close MY_LOG;
select STDOUT;

chdir $cur_dir;

if ( $env_name ne "" ) {
  my $env_value = eval "return \$" . $env_name;
  #print STDERR "  env_value: $env_value\n" if ($debug);
  if ( $env_value eq "" ) {
    $env_value = eval "return \$ENV{\"" . $env_name . "\"}";
  }
  #print "  set environment variable $env_name to [$env_value]\n";
  print "$env_value\n";
}

exit(0);

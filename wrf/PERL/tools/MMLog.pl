#!/usr/bin/perl

use Log::Log4perl qw(:easy);
use Switch;
use POSIX;

Log::Log4perl->easy_init($INFO);

$PID = getppid;
my $process_name = `ps hp $PID o "%c"`;
$level   = shift(@ARGV);
$message = shift(@ARGV);
logit($level,$message);

sub logit 
{
  my ($level,$message)=@_;

  my $logger = get_logger();

  switch($level)
  {
    case "DEBUG"
    {
      $logger->debug("DEBUG ",$message," ",$process_name);
    }
    case "INFO"
    {
      $logger->info("INFO ",$message," ",$process_name);
    }
    case "WARN"
    {
      $logger->warn("WARN ",$message," ",$process_name);
    }
    case "ERROR"
    {
      $logger->error("ERROR ",$message," ",$process_name);
    }
    case "FATAL"
    {
      $logger->fatal("FATAL ",$message," ",$process_name);
    }
    else 
    {
      $logger->info("Unrecognized Level",$message," ",$process_name);
    }
  }
}


#!/usr/bin/perl
# The purpose of this script is for testing the WMO direct-access table by
# reading each record in and output each non-missing record.

$fn=shift;

open(IN,"$fn");

$len=100;
seek(IN,0,0);

while (read(IN,$line,$len) > 0) {

  seek(IN,0,1);

  next if($line=~ /^99999/);

  $line=~ s/ +$//;
# print substr($line,0,80),"\n";
  print "$line\n";
# print "$text\n";

}

close(IN);

exit;

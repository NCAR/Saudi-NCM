#!/usr/bin/perl -I/usr/lib/perl5/site_perl
#

#!/local/bin/perl -s
#
#  Get a file using ftp.
#
#   ftpget host source dest
#      gets file, logging in as anonymous with user@host password (default)
#   ftpget user@host source dest
#      gets file as user "user" and prompts for password
#
#  Written by Gene Spafford  <spaf@cs.purdue.edu>
#   Last update, 17 May 1994, D. Sundstrom  <sunds@lobby.ti.com>
#

use Net::FTP;

die "usage: ftpget [-ascii] <host> <source> 
    where <host> may be of the form user\@host
    remote user defaults to 'anonymous' if not specified
" unless ($#ARGV > 0 && $#ARGV < 4);

($Host, $Source, $file_to_get, $date) = @ARGV;
#print STDOUT "FILE = $file_to_get\n";

print STDOUT "HOST = $Host\n";
if ( $Host =~ /^ftp:\/\/(\S+)/ ){
    $bn = $1;
    print "$bn\n";
}

if ($Host =~ /^(\S+)@(\S+)$/) {   # user@host format?
    ($User, $Host) = ($1, $2);
#    print STDERR "Password to use: ";
#    system 'stty -echo';
#    chop($Pass = <STDIN>);
#    system 'stty echo';
} 

$pattern = "*." . $file_to_get;

die "Cannot transfer a directory.\n" if -d $Source;

$Pass = "fddasys@";
$User = "anonymous";
         $ftp = Net::FTP->new($Host, Timeout => 360, Passive=>true) || &fail ;
         $ftp->login($User,$Pass) ;
#           $ftp->pasv ;
$ftp->ls;
         $ftp->binary ;
         $ftp->cwd($Source);

foreach my $file ($ftp->ls($pattern)) {

      print "File Name: $file \n";
	if ($file =~ "00") {$hh= "00";}
	if ($file =~ "06") {$hh= "06";}
	if ($file =~ "12") {$hh= "12";}
	if ($file =~ "18") {$hh= "18";}

	$file1 = ${date} . ${hh} . "_" . ${file};

      if(! -e $file1) {$ftp->get($file) or warn $ftp->message; 
	system("mv $file $file1 &"); 
      }
}


sub fail {
    $ftp->quit;
    die "ftp error occurred\n";
}

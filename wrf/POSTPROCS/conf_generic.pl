#!/usr/bin/perl -w

# Generic version to make a plain "hosts" file --- not RTFDDA specific
# "hosts" and "conf" will be written in the current-working-directory

use strict;

################################################################
# MPP job controller: 
#   1). check node availability
#   2). find node range and number for a MPP job
#   3). prepare/clean the nodes in the range for mpp job to start
#
#   Developed and maintained by Yubao Liu  2001 - 2003
#   Modified by Al Bourgeois to add optional MPI scan, Nov 2003
#   Modified by Laurie Carson to make a generic non-range version
################################################################

# GLOBAL VARIABLES are designated by UPPERCASE.

#=============================================================================== 
my $MAIL_TO='carson@ucar.edu';     # Email list for sending notifications.

# Arrays and Hashes:
my @GOODNODES;                       # Array of good nodes by IP (ex: 10.1.1.12).
my %NNAME_IP;                        # Hash table of node names over node IP.
#-----------------------------------------------------------------------------

# END GLOBAL VARIABLES

my @bad_mpi_nodes;
my ($req_num,$node_start,$node_end);      # Required node number and range.
my ($aline, $arange, $agroup);
my ($r,$i,$cnt);                          # Loop counters.
my ($anode, $mynode);
my ($nname, $node_name, $noden);                  # Node IP and name.
my ($node, $ret);
my (@goodnodes_by_name);                  # Array of good nodes by node name (ex: node12).
my $num_bad_mpi_nodes;                    # Number of nodes detected to be in bad MPI state. 

my $cluster="testing";
my $gmpi = ".";
my $hostlist = "/etc/hosts";
my $upnodes = 0;
my $downnodes = 0;

my $outfile = "$gmpi/conf";
my $outfileh = "$gmpi/hosts";
$ENV{'GMPICONF'} =$outfile;
$ENV{'GMPICONF'} =$outfileh;

##
### Change this on different clusters...
##
$req_num = 14;
$node_start = 1;
$node_end = 31;

while ( ! $upnodes ) {
open (IN,"<$hostlist");
while (<IN>) {
      next if ($_ !~ /node/ && $_ !~ /Node/ && $_ !~ /NODE/);
      next if ($_ =~ /EthNode/) ;
      next if ($_ =~ /mnode/) ;
      $anode = $_;
      $mynode=0;
      for($i=$node_start; $i <= $node_end; $i++) {
       $mynode= 1 if ($anode =~ /node$i\n/ || $anode =~ /Node$i\n/ || $anode =~ /NODE$i\n/);
      }
      next if(! $mynode);
      chomp($anode);
# Exclude nodes here... ones that don't work, but still pass the ssh & gm_board_info test...
# 
#       next if ($_ =~ /node5$/ || $_ =~ /Node5$/ || $_ =~ /NODE5$/);
#       next if ($_ =~ /node8$/ || $_ =~ /Node8$/ || $_ =~ /NODE8$/);
        ($_,$nname)=split(/\t+/,$anode);
         $NNAME_IP{$_}=$nname;
         print $_, "++++";;
                ## now ssh to it, but timeout
                $node = $_;

                eval {
                        $ret = "";
                        local $SIG{ALRM} = sub { die "timeout happened sshing to $node\n" };
                        alarm 5;
                        $ret = `ssh $node ps 2> /dev/null`;
                        alarm 0;
                        if ($ret eq "") {
                                print "  $NNAME_IP{$node} -- ";
                                printf ("SSH FAILED\n");
                                $downnodes++;
                        }
                        else {
                                $node_name = $NNAME_IP{$node};
                                print "  $node_name -- ";
                                printf ("SSH SUCCEEDED\n");

                                ($noden = $node_name ) =~ tr/A-Z/a-z/;
                                $ret = "";
# Does the GM mapper run on the master node??? if not...
                              # $ret = (` ssh $noden /opt/gm/bin/gm_board_info | grep $noden`);
                                $ret = (` /opt/gm/bin/gm_board_info | grep $noden`);
                                if ( $ret ne "" ) {
                                   $upnodes++;
                                   push (@GOODNODES,$node);
                                   push (@goodnodes_by_name,$node_name);
                                } else {
                                   printf (" $noden  is not GM-mapped\n");
                                   $downnodes++;
                                }
                        }
                };
                if ($@ and $@ !~ /timeout happened sshing to/) {
                        printf ("SSH TIMED OUT\n");
                        $downnodes++;
                }
}
close IN;
print ("Number of up nodes: $upnodes\n");
print ("Number of down nodes: $downnodes\n");
sleep 120 if( ! $upnodes );
}


# Create MPI configuration file.
write_Conf_File($upnodes, $req_num);

1;

#*******************************************************************************
#                                SUBROUTINES                                   #
#*******************************************************************************
#
# Name:  write_Conf_File
#
# Purpose: Write the GMPI node configuration file using list of nodes deemed
#          to be good.
#
# INVOCATION:  write_Conf_File($outfile, $up_nodes, $req_num);
#
sub write_Conf_File
{
# Get input arguments.
    my ($up_nodes)   = shift;       # Number of good nodes.
    my ($req_num)    = shift;       # Number of nodes required to run job.

# Declare local variables.
    my $upprocs = $upnodes * 2;     # Number of processors available.
    my $subj;                       # Subject for email.

        open (OUT, ">$outfile");
        open (OUTH, ">$outfileh");
        print OUT "# .gmpi/conf file begin\n# first the number of nodes in the file\n";
        print OUT "$upprocs\n";
        print OUT "# the list of (node, port, board) that make the MPI World\n";

        # Write the config file
        $cnt = 0;
        foreach $node (@GOODNODES) {
           print OUT "$NNAME_IP{$node} 2\n$NNAME_IP{$node} 4\n";
           print OUTH "$NNAME_IP{$node}\n$NNAME_IP{$node}\n";
           $cnt++;
           last if ($cnt == $upnodes);
        }
        print OUT "# .gmpi/conf file end";
        close OUT;
        close OUTH;
      return 1;
}

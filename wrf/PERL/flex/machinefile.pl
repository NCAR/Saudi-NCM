#!/usr/bin/perl

#------------------------------------------------------------------------------#
#Parse command line args:
#------------------------------------------------------------------------------#

@NODES_AVAILABLE = (); 

&parseArgs;
&read_Nodes_File;
&check_Nodes;
&select_Nodes;
   
exit 0;
#-------------------------------------------------------------------------------
# Name: parseArgs
# Arguments: none
# Return: none
# Description: parses command line arguments
#-------------------------------------------------------------------------------

sub parseArgs
{
  if( !$ARGV[0] || $ARGV[0] eq "--")
  {
        &usage();
        exit(-1);
  }
  else
  {
#    print "Parsing command line args\n";
    $length = @ARGV;
    $i = 0;

    for($i = 0; $i < $length; $i++)
    {
      if ($ARGV[$i] eq "--")
      {
        break;
      }
      elsif ($ARGV[$i] eq "-i")
      {
        $nodes_available_file = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-m")
      {
        $max_cpus = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-n")
      {
        $cpus_per_node = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-o")
      {
        $machine_file = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-h")
      {
        &usage();
        exit(-1);
      }
      else
      {
        next;
      }
    }

    if (! -e $nodes_available_file || !$cpus_per_node || !$max_cpus)
    {
      &usage();
      exit(-1);
    }
  }

#  print "exiting parseArgs: nodes_available_file=$nodes_available_file, max_cpus=$max_cpus, cpus_per_node=$cpus_per_node, machine_file=$machine_file\n";
}

#-------------------------------------------------------------------------------
# Name: usage
# Arguments: none
# Return: none
# Description:
#   The usage subroutine "machinefile.pl -h"
#-------------------------------------------------------------------------------

sub usage
{
  print "\n";
  print "machinefile.pl <-h> -i nodes_available_file -m max_cpu -n cpus_per_node -o machinefile\n";
  print "\n";
  print "Where: \n";
  print " -i nodes_available_file: File with a pool of nodes available for the run\n";
  print " -o machinefile: Machine file needed by MPICH mpirun command.\n";
  print " -n max_cpus: Maximal number of CPUs to be used for this job\n";
  print "              This is variable NUM_PROC of flexinput.pl\n";
  print " -n cpus_per_node: The number of CPUs per node\n";
  print "                   This is variable PPN of flexinput.pl\n";
  print "\n";
  print "The content of a typical nodes_available file is:\n";
  print " node1\n";
  print " node2\n";
  print " node10\n";
  print " ...\n";
  print "There must be at least as much nodes available as NUM_PROC/PPN from flexinput.pl\n";
  print "\n";
}
#------------------------------------------------------------------------------#
# Name: read_Nodes_File
#
# Purpose: Open and read the file containing the list of nodes assigned for this
# installation.
#
# INVOCATION:  read_Nodes_File()
#   
# Global variables used:
#  GMODJOBS               # Path to file $GMODJOBS/config/nodes_available
#
#------------------------------------------------------------------------------#

sub read_Nodes_File
{

# Global variable (output)


# Declare local variables.

    my $retval;
    my @field;

    $retval = 0;

    print "Reading nodes available from file $nodes_available_file\n";

    if (! open(FLEX, $nodes_available_file)){
         print "Failed to open file $nodes_available_file\n";
         exit -1;
    }

    while (<FLEX>) {
      if (/^\/EOF/) {
          print "Hit end-of-file\n";
          last;
      }
      if (/^#/ || /^(\s)*$/) {
          next;                   # Skip blank lines and comment lines.
      } else {
          @field = split(' ');
          if ( $field[0] =~ /^#/ ) {
               next;
          } else {
               push (@NODES_AVAILABLE,"$field[0]");
               $retval ++;
          }

      }
    }
    close(FLEX);
#   print "nodes available =  @NODES_AVAILABLE\n";
    return $retval;
}
#------------------------------------------------------------------------------#
sub check_Nodes
{

# Declare local variables.
my $NODE1;
my ($ret, $n);
my $user_name = $ENV{USER};
my @NODES_BUSY_PROC;

@NODES_UP = ();
@NODES_BUSY = ();
@NODES_BUSY_PROC = ();
@NODES_UP_and_AVAIL = ();

foreach $NODE1 (@NODES_AVAILABLE){

#  print "   Node${NODE1}: \n";
   eval {
         $ret="";
         local $SIG{ALRM} = sub { die "timeout happened sshing to node$NODE1\n" };
         alarm 3;

         $ret=`ssh $NODE1 ps -ef |grep mpich |grep -v PID | grep -v sshd | grep -v tcsh |grep -v grep 2> /dev/null`;
         alarm 0;
         chomp ($ret);

   };

   if ($@ and $@ =~ /timeout happened sshing to/) {
       printf ("SSH FAILED -time out. May be it\'s down!!! \n");
   } else {
       push (@NODES_UP,"$NODE1");
       if ($ret ne "") {
               system("ssh $NODE1 snuff -l mpich ");
       } 
       $ret=`ssh $NODE1 ps -ef |grep mpich |grep -v PID | grep -v sshd | grep -v tcsh |grep -v grep 2> /dev/null`;
       if ($ret ne "") {
               push (@NODES_BUSY,"$NODE1");
               push (@NODES_BUSY_PROC,"$ret");
              printf ("BUSY node $NODE1\n");
       } else {
         push (@NODES_UP_and_AVAIL,"$NODE1");
       }
   }

}
#print "nodes available = @NODES_AVAILABLE\n";
#print "nodes up        = @NODES_UP\n";
if ($#NODES_BUSY >= 0) {
#print "nodes busy      = @NODES_BUSY\n";
  for ($n=0; $n<= $#NODES_BUSY; $n++){
      printf (" $NODES_BUSY[$n]: $NODES_BUSY_PROC[$n]\n");
  }
}

return ($#NODES_UP);
}
#------------------------------------------------------------------------------#
sub select_Nodes
{

my $NODE1;
my $np = 0;  # Processors to be used
my $nn = 0;  # Nodes to be used

if (! open(FLEX,">$machine_file")){
         print "Failed to open file $machine_file\n";
         exit -1;
}

#  List nodes available and up for the job
#foreach $NODE1 (@NODES_AVAILABLE){
foreach $NODE1 (@NODES_UP_and_AVAIL){

   $proc = 0;

   # Register each processor on that node
   while ($proc < $cpus_per_node)
   { 
          print FLEX "$NODE1\n";
          $proc ++;
          $np ++;
   }

   last if ($np >= $max_cpus);  # Limit to NUM_NODES

}

close (FLEX);

printf ("Output in file $machine_file\n");

return ($nn);

}
#------------------------------------------------------------------------------#
1;

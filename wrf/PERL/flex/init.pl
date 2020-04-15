#!/usr/bin/perl
use Time::gmtime 'gmctime';

$FLEXINPUT = $ENV{FLEXINPUT};

if (-e $FLEXINPUT)
{
  require $FLEXINPUT;
}
else
{
  print "$FLEXINPUT does not exit .... Exiting\n";
  exit (-1);
}

#Parse command line args:
$cycle_in   = "";
$queue_in   = "";
$account_in = "";
$start_in   = "";
$res_id_in  = "";
$cycle_int  = "";
$pre_proc_type = "";
$env_vars_file = "";
$check_rundir = 1;

&parseArgs();

# Check input variables
print "\nChecking whether all necessary parameters are set:\n";
&checkParams();

# This script contains the setEnvVars subroutine
require $PERL_FLEX.'/setEnvVars.pl';

# Set the environment variables
print "\nSetting env vars\n";
&setEnvVars();

#Write the envvars to the file
&writeEnvVarsFile();
print "Wrote cshrc file at ", gmctime();

# Check for necessary directories and create them if not there
print "\nChecking for necessary directories\n";
if ($check_rundir){
&checkRUNDIRDirs();
}

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
    print "$PERL_FLEX/init.pl: No command line arguments to parse\n";
  }
  else
  {
    print "$PERL_FLEX/init.pl: Parsing command line args\n";
    $length = @ARGV;
    $i = 0;

    for($i = 0; $i < $length; $i++)
    {
      if ($ARGV[$i] eq "--")
      {
        break;
      }
      elsif ($ARGV[$i] eq "-c")
      {
        $cycle_in = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-q")
      {
        $queue_in = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-a")
      {
        $account_in = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-s")
      {
        $start_in = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-r")
      {
        $res_id_in = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-i")
      {
	$cycle_int = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-t")
      {
        $pre_proc_type = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-f")
      {
        $env_vars_file = $ARGV[$i+1];
      }
      elsif ($ARGV[$i] eq "-no_rundir")
      {
       $check_rundir = 0;
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

    if (!$cycle_in && !$queue_in && !$res_id_in && !$account_in && !$start_in && !$env_vars_file)
    {
      &usage();
      exit(-1);
    }
  }

  print "exiting init.parseArgs: cycle_in=$cycle_in, queue_in=$queue_in, account_in=$account_in, start_in=$start_in, res_id_in=$res_id_in, cycle_int=$cycle_int, pre_proc_type=$pre_proc_type,no_rundir=$check_rundir\n";
}

#-------------------------------------------------------------------------------
# Name: checkParams
# Arguments: none
# Return: none
# Description: This subroutine checks whether all necessary job parameters are set
#              and writes out the file tmp/pre_process_in_donotedit.pl
#-------------------------------------------------------------------------------

sub checkParams
{
  print "\n***************** pre_process's input constants**************\n" if ($DEBUG);

  print "GSJOBDIR = $GSJOBDIR\n" if ($DEBUG);

  # open the $GSJOBDIR/tmp/pre_process_in_donotedit.pl
  if (! -e "$GSJOBDIR/tmp")
  {
    system ("mkdir $GSJOBDIR/tmp");
  }

  print "Writing parameters to $GSJOBDIR/tmp/pre_process_in_donotedit.pl";
  open(PRE_PROCESS_IN,">$GSJOBDIR/tmp/pre_process_in_donotedit.pl");

  print PRE_PROCESS_IN "#!/usr/bin/perl\n";
  print PRE_PROCESS_IN "\$DEBUG = $DEBUG;\n";
  print PRE_PROCESS_IN "\$GSJOBDIR = \"$GSJOBDIR\";\n";

  if (!$GSJOBID)
  {
    print "GSJOBID is not specified! EXITING\n";
    close(PRE_PROCESS_IN);
    exit(-1);
  }
  print "GSJOBID = $GSJOBID\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$GSJOBID = \"$GSJOBID\";\n";

  if ($cycle_in)
  {
    $this_cycle = $cycle_in;
  }
  if (!$this_cycle)
  {
    # Start the cycle at a later hour (offset is in hours)
    if (!$CYCLE_START_OFFSET) { $CYCLE_START_OFFSET=0; } 
    # Build the UTC date as yy mm dd hh for this cycle
    $ttime = time - $CYCLE_START_OFFSET * 3600;
    ($sec,$mm,$hh,$dd,$mo,$yy,@_) = gmtime($ttime);

    if ($yy<50)
    {
      $yy+=2000;
    }
    else
    {
      $yy+=1900;
    }

    $this_cycle = sprintf("%04d%02d%02d%02d",$yy,$mo+1,$dd,$hh);
  }
  print "this_cycle = $this_cycle\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$this_cycle = $this_cycle;\n";

  print PRE_PROCESS_IN "\$RT_CSHRC = \"$GSJOBDIR/tmp/$this_cycle/cshrc\";\n";

  if (!$MODEL)
  {
    print "MODEL is not specified! EXITING\n";
    close(PRE_PROCESS_IN);
    exit(-1);
  }
  print "MODEL = $MODEL\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$MODEL = \"$MODEL\";\n";

  if ($cycle_int)
  {
    $CYC_INT = $cycle_int;
  }
  print "CYC_INT = $cycle_int\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CYC_INT = $CYC_INT;\n";

  if ($pre_proc_type)
  {
    $InterpType = $pre_proc_type;
  }
  print "InterpType = $InterpType\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$InterpType = $InterpType;\n";

  if ($NODE != 1)
  {
    print "Resetting NODE to 1\n";
    $NODE=1;
  }
  print "NODE = $NODE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NODE = \"$NODE\";\n";

  if (! defined ($PPN) || $PPN <= 0)
  {
    print "PPN - the number of CPUs per node is $PPN! EXITING\n";
    close(PRE_PROCESS_IN);
    exit(-1);
  }
  print "PPN = $PPN\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$PPN = $PPN;\n";

  if (!$NUM_PROCS)
  {
    print "NUM_PROCS - the number of processors is not specified! EXITING\n";
    close(PRE_PROCESS_IN);
    exit(-1);
  }
  print "NUM_PROCS = $NUM_PROCS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NUM_PROCS = $NUM_PROCS;\n";

  if (!$NUM_NODES)
  {
    $NUM_NODES = $NUM_PROCS/$PPN;
  }
  print "NUM_NODES = $NUM_NODES\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NUM_NODES = $NUM_NODES;\n";

  if ( !$MPPJOB || ($MPPJOB ne "yes" && $MPPJOB ne "no"))
  {
    $MPPJOB = "yes";
  }
  print "MPPJOB = $MPPJOB\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$MPPJOB = \"$MPPJOB\";\n";

  if ( $MODEL eq "WRF" && $MPI_PRE_PROCESS!=0 && $MPI_PRE_PROCESS!=1 )
  {
    $MPI_PRE_PROCESS = 1;
  }
  print "MPI_PRE_PROCESS = $MPI_PRE_PROCESS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$MPI_PRE_PROCESS = \"$MPI_PRE_PROCESS\";\n";

  if (!$BATCH_SYSTEM)
  {
    $BATCH_SYSTEM = "PBS";
  }
  print "BATCH_SYSTEM = $BATCH_SYSTEM\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$BATCH_SYSTEM = $BATCH_SYSTEM;\n";

  if ($BATCH_SYSTEM eq "LSF")
  {
    $RESOURCE_LIST = "-x -n $NUM_PROCS -R 'span[ptile=${PPN}]' -a poe"; #:$PPN";
    if ($queue_in)
    {
      $QUEUE_TYPE = $queue_in;
    }
    if ( $MPI_WALLTIME > 0 )
    {
      $MPI_WALLTIME = $MPI_WALLTIME/60;
      $RESOURCE_LIST =  "$RESOURCE_LIST -W $MPI_WALLTIME";
    }

    if ( $MODEL eq "WRF" && $MPI_PRE_PROCESS == 1)
    {
      $RESOURCE_LIST_PRE= "-n $NUM_PROCS";
    }
    else
    {
      $RESOURCE_LIST_PRE= "-n 1";
    }

    if ( $PRE_PROC_WALLTIME > 0 )
    {
      $PRE_PROC_WALLTIME = $PRE_PROC_WALLTIME/60;
      $RESOURCE_LIST_PRE =  "$RESOURCE_LIST_PRE -W $PRE_PROC_WALLTIME";
    }

    $RESOURCE_LIST_PRE_P = "-n 1";
    if ( $PRE_PROC_WALLTIME > 0 )
    {
      $PRE_PROC_WALLTIME = $PRE_PROC_WALLTIME/60;
      $RESOURCE_LIST_PRE_P =  "$RESOURCE_LIST_PRE_P -W $PRE_PROC_WALLTIME";
    }

    $RESOURCE_LIST_POST= "-n 1";
    if ( $POST_PROC_WALLTIME > 0 )
    {
       $POST_PROC_WALLTIME =  $POST_PROC_WALLTIME/60;
       $RESOURCE_LIST_POST =  "$RESOURCE_LIST_POST -W $POST_PROC_WALLTIME";
    }
    $RESOURCE_LIST_VERIF= "-n 1";
    if ( $VERI_WALLTIME > 0 )
    {
       $VERI_WALLTIME =  $VERI_WALLTIME/60;
       $RESOURCE_LIST_VERIF =  "$RESOURCE_LIST_VERIF -W $VERI_WALLTIME";
    }
    $RESOURCE_LIST_CLEAN= "-n 1";
  }
  elsif ($BATCH_SYSTEM eq "PBS")
  {
    #$RESOURCE_LIST = "nodes=$NUM_NODES:ppn=$PPN";
    $RESOURCE_LIST = "select=$NUM_NODES:mpiprocs=$PPN:ncpus=$PPN,place=scatter:excl";
    if ($res_id_in)
    {
      $RES_NAME = $res_id_in;
    }
    if ( $RES_NAME )
    {
      $RESOURCE_LIST =  "$RESOURCE_LIST,advres=$RES_NAME";
    }
    if ( $MPI_WALLTIME > 0 )
    {
      $RESOURCE_LIST =  "$RESOURCE_LIST,walltime=$MPI_WALLTIME";
    }

    if ( $MODEL eq "WRF" && $MPI_PRE_PROCESS == 1)
    {
      #$RESOURCE_LIST_PRE= "nodes=$NUM_NODES:ppn=$PPN";
      $RESOURCE_LIST_PRE= "select=$NUM_NODES:mpiprocs=$PPN:ncpus=$PPN,place=scatter:excl";
    }
    else
    {
      #$RESOURCE_LIST_PRE= "nodes=1:ppn=$PPN";
      $RESOURCE_LIST_PRE= "select=$NUM_NODES:mpiprocs=$PPN:ncpus=$PPN,place=scatter:excl";
    }

    if ( $RES_NAME )
    {
      $RESOURCE_LIST_PRE =  "$RESOURCE_LIST_PRE,advres=$RES_NAME";
    }
    if ( $PRE_PROC_WALLTIME > 0 )
    {
      $RESOURCE_LIST_PRE =  "$RESOURCE_LIST_PRE,walltime=$PRE_PROC_WALLTIME";
    }

    $PRE_PROCESS_QUEUE = "";
    if ( $BIG_MEM_PRE_PROCESS && $MODEL eq "MM5")
    {
      if ( !$BIG_MEM_QUEUE_NAME )
      {
        $BIG_MEM_QUEUE_NAME = "bigmem";
      }
      $PRE_PROCESS_QUEUE ="-q $BIG_MEM_QUEUE_NAME";
    }

    if ( $BIG_MEM_PRE_PROCESS && $MODEL eq "WRF" && $MPI_PRE_PROCESS==0)
    {
      if ( !$BIG_MEM_QUEUE_NAME )
      {
        $BIG_MEM_QUEUE_NAME = "bigmem";
      }
      $PRE_PROCESS_QUEUE ="-q $BIG_MEM_QUEUE_NAME";
    }

    $RESOURCE_LIST_PRE_P = "nodes=1:ppn=$PPN";
    if ( $RES_NAME )
    {
      $RESOURCE_LIST_PRE_P =  "$RESOURCE_LIST_PRE_P,advres=$RES_NAME";
    }
    if ( $PRE_PROC_WALLTIME > 0 )
    {
      #$RESOURCE_LIST_PRE_P =  "$RESOURCE_LIST_PRE_P,walltime=$PRE_PROC_WALLTIME";
      $RESOURCE_LIST_PRE_P = "select=$NUM_NODES:ncpus=$PPN:mpiprocs=$PPN,place=scatter:excl,walltime=$PRE_PROC_WALLTIME";
    }

    $RESOURCE_LIST_POST= "nodes=1:ppn=$PPN";
    if ( $RES_NAME )
    {
      $RESOURCE_LIST_POST =  "$RESOURCE_LIST_POST,advres=$RES_NAME";
    }
    if ( $POST_PROC_WALLTIME > 0 )
    {
      #$RESOURCE_LIST_POST =  "$RESOURCE_LIST_POST,walltime=$POST_PROC_WALLTIME";
      $RESOURCE_LIST_POST= "select=1:ncpus=$PPN:mpiprocs=$PPN,place=scatter:excl,walltime=$POST_PROC_WALLTIME";
    }

    $POST_PROCESS_QUEUE = "";
    if ( $BIG_MEM_POST_PROCESS )
    {
      if ( !$BIG_MEM_QUEUE_NAME )
      {
        $BIG_MEM_QUEUE_NAME = "bigmem";
      }
      $PRE_PROCESS_QUEUE ="-q $BIG_MEM_QUEUE_NAME";
    }

    #$RESOURCE_LIST_VERIF= "nodes=1:ppn=$PPN";
    $RESOURCE_LIST_VERIF= "nodes=1:ppn=2";
    if ( $RES_NAME )
    {
       $RESOURCE_LIST_VERIF =  "$RESOURCE_LIST_VERIF,advres=$RES_NAME";
    }
    if ( $VERI_WALLTIME > 0 )
    {
       #$RESOURCE_LIST_VERIF =  "$RESOURCE_LIST_VERIF,walltime=$VERI_WALLTIME";
       $RESOURCE_LIST_VERIF= "select=1:ncpus=32:mpiprocs=32,place=scatter:excl,walltime=$VERI_WALLTIME";
    }


    #$RESOURCE_LIST_CLEAN= "nodes=1:ppn=$PPN,walltime=36000";
    $RESOURCE_LIST_CLEAN= "select=1:ncpus=$PPN:mpiprocs=$PPN,walltime=01:00:00";
    if ( $RES_NAME )
    {
      $RESOURCE_LIST_CLEAN =  "$RESOURCE_LIST_CLEAN,advres=$RES_NAME";
    }
  }
  print "RESOURCE_LIST = $RESOURCE_LIST\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RESOURCE_LIST = \"$RESOURCE_LIST\";\n";

  print "RESOURCE_LIST_PRE = $RESOURCE_LIST_PRE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RESOURCE_LIST_PRE = \"$RESOURCE_LIST_PRE\";\n";

  print "RESOURCE_LIST_PRE_P = $RESOURCE_LIST_PRE_P\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RESOURCE_LIST_PRE_P = \"$RESOURCE_LIST_PRE_P\";\n";

  print "PRE_PROCESS_QUEUE = $PRE_PROCESS_QUEUE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$PRE_PROCESS_QUEUE = \"$PRE_PROCESS_QUEUE\";\n";

  print "RESOURCE_LIST_POST = $RESOURCE_LIST_POST\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RESOURCE_LIST_POST = \"$RESOURCE_LIST_POST\";\n";

  print "POST_PROCESS_QUEUE = $POST_PROCESS_QUEUE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$POST_PROCESS_QUEUE = \"$POST_PROCESS_QUEUE\";\n";

  print "RESOURCE_LIST_VERIF = $RESOURCE_LIST_VERIF\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RESOURCE_LIST_VERIF = \"$RESOURCE_LIST_VERIF\";\n";

  print "RESOURCE_LIST_CLEAN = $RESOURCE_LIST_CLEAN\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RESOURCE_LIST_CLEAN = \"$RESOURCE_LIST_CLEAN\";\n";


  if ( $start_in)
  {
    $JOB_START_TIME = $start_in
  }

  if ($BATCH_SYSTEM eq "LSF")
  {
    if ( $JOB_START_TIME )
    {
      $JOB_START = "-b $JOB_START_TIME";
    }
  }
  elsif ($BATCH_SYSTEM eq "PBS")
  {
    if ( $JOB_START_TIME )
    {
      $JOB_START = "-a $JOB_START_TIME";
    }
  }

  print "JOB_START = $JOB_START\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$JOB_START = \"$JOB_START\";\n";

  $EMAIL = "";
  if ($BATCH_SYSTEM eq "LSF")
  {
    if ($EMAIL_JOBSTART || $EMAIL_JOBABORT || $EMAIL_JOBEND)
    {
      $EMAIL = "";
      $EMAIL = $EMAIL."-B " if ($EMAIL_JOBSTART);
      $EMAIL = $EMAIL."-N " if ($EMAIL_JOBEND || $EMAIL_JOBABORT);
      if ($EMAIL_RECIPIENT)
      {
        $EMAIL = $EMAIL." -u $EMAIL_RECIPIENT" ;
      }
    }
  }
  elsif ($BATCH_SYSTEM eq "PBS")
  {
    if ($EMAIL_JOBSTART || $EMAIL_JOBABORT || $EMAIL_JOBEND)
    {
      $EMAIL = "-m ";
      $EMAIL = $EMAIL."a" if ($EMAIL_JOBABORT);
      $EMAIL = $EMAIL."b" if ($EMAIL_JOBSTART);
      $EMAIL = $EMAIL."e" if ($EMAIL_JOBEND);
      if ($EMAIL_RECIPIENT)
      {
        $EMAIL = $EMAIL." -M $EMAIL_RECIPIENT";
      }
    }else{
      $EMAIL = "-m n"; # No e-mail at all
    }
  }
  print "EMAIL = $EMAIL\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$EMAIL = \"$EMAIL\";\n";

  $ACCOUNT = "";
  if ($account_in)
  {
    $ACCOUNT_KEY = $account_in;
  }
  if ($BATCH_SYSTEM eq "LSF")
  {
    if ($ACCOUNT_KEY)
    {
      $ACCOUNT ="-P $ACCOUNT_KEY";
    }
  }
  elsif ($BATCH_SYSTEM eq "PBS")
  {
    if ($ACCOUNT_KEY)
    {
      $ACCOUNT ="-A $ACCOUNT_KEY";
    }
  }
  print "ACCOUNT_KEY = $ACCOUNT_KEY\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$ACCOUNT_KEY = \"$ACCOUNT_KEY\";\n";

  if (!$RANGE)
  {
    $RANGE = "GRM";
  }
  print "RANGE = $RANGE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RANGE = \"$RANGE\";\n";

  if (!$NUM_DOMS)
  {
    print "NUM_DOMS - the number of domains is not specified! EXITING\n";
    close(PRE_PROCESS_IN);
    exit(-1);
  }
  print "NUM_DOMS = $NUM_DOMS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NUM_DOMS = $NUM_DOMS;\n";

  if (! defined ($NUM_DOMS) || $NUM_DOMS < 4)
  {
    $D4_start = 44640;
  }
  print "NUM_DOMS = $NUM_DOMS, so setting D4_start = $D4_start\n" if ($DEBUG);

  if ( !$normal)
  {
    $normal = 0;
  }
  print "normal = $normal\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$normal = $normal;\n";

  if (!$CYC_INT)
  {
    $CYC_INT = 1;
  }
  print "CYC_INT = $CYC_INT\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CYC_INT = $CYC_INT;\n";

  if (!$FIN_END)
  {
    $FIN_END = 0;
  }
  print "FIN_END = $FIN_END\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$FIN_END = $FIN_END;\n";

  if (!$OUT_INT)
  {
    $OUT_INT = 60;
  }
  print "OUT_INT = $OUT_INT\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$OUT_INT = $OUT_INT;\n";

  if (! defined ($COLD_START_FCST) || $COLD_START_FCST < 0)
  {
    $COLD_START_FCST = 15;
  }
  print "COLD_START_FCST = $COLD_START_FCST \n" if ($DEBUG);
  print PRE_PROCESS_IN "\$COLD_START_FCST = $COLD_START_FCST;\n";


  if ( ! defined ($FCST_LENGTH) || $FCST_LENGTH < 0)
  {
    $FCST_LENGTH = $CYC_INT;
  }

  if (!$COLD_0012)
  {
    $COLD_0012 = 0 ;
  }
  print "COLD_0012 = $COLD_0012\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$COLD_0012 = $COLD_0012;\n";

  if (! $WRFQC_BACK) {
     $WRFQC_BACK = 6;
  }
  print "WRFQC_BACK = $WRFQC_BACK\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$WRFQC_BACK = $WRFQC_BACK;\n";

  if (! $SKIP_WRFQC_P) {
     $SKIP_WRFQC_P = 0;
  }
  print "SKIP_WRFQC_P = $SKIP_WRFQC_P\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$SKIP_WRFQC_P = $SKIP_WRFQC_P;\n";

  if (@DOM_LENGTH) {
     $FCST_LENGTH = $DOM_LENGTH[0];
  } else {
     if ($FCST_LENGTH) {
        @DOM_LENGTH = ($FCST_LENGTH) x 5;
     } else {
        print "Error: Neither \@DOM_LENGTH nor \$FCST_LENGTH is defined!\n";
        exit;
     }
  } 

  print "FCST_LENGTH = $FCST_LENGTH \n" if ($DEBUG);
  print PRE_PROCESS_IN "\$FCST_LENGTH = $FCST_LENGTH;\n";

  if ( @DOM_LENGTH) {

    print "Array \@DOM_LENGTH overrides variable \$FCST_LENGTH!\n";

    if ( defined $DOM_LENGTH[0] ) {
       $D1_LENGTH = $DOM_LENGTH[0];
     }
     print "D1_LENGTH = $D1_LENGTH\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$D1_LENGTH = $D1_LENGTH;\n";
    if ( defined $DOM_LENGTH[1] ) {
     if ( $DOM_LENGTH[1] > $DOM_LENGTH[0] ) {
       print "D2_LENGTH reset to $DOM_LENGTH[0]\n" if ($DEBUG);
       $D2_LENGTH = $DOM_LENGTH[0];
     } else {
       $D2_LENGTH = $DOM_LENGTH[1];
     }
     print "D2_LENGTH = $D2_LENGTH\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$D2_LENGTH = $D2_LENGTH;\n";
    }
    if ( defined $DOM_LENGTH[2] ) {
     if ( $DOM_LENGTH[2] > $DOM_LENGTH[1] ) {
       print "D3_LENGTH reset to $DOM_LENGTH[1]\n" if ($DEBUG);
       $D3_LENGTH = $DOM_LENGTH[1];
     } else {
       $D3_LENGTH = $DOM_LENGTH[2];
     }
     print "D3_LENGTH = $D3_LENGTH\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$D3_LENGTH = $D3_LENGTH;\n";
    }
    if ( defined $DOM_LENGTH[3] ) {
     if ( $DOM_LENGTH[3] > $DOM_LENGTH[2] ) {
       print "D4_LENGTH reset to $DOM_LENGTH[2]\n" if ($DEBUG);
       $D4_LENGTH = $DOM_LENGTH[2];
     } else {
       $D4_LENGTH = $DOM_LENGTH[3];
     }
     print "D4_LENGTH = $D4_LENGTH\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$D4_LENGTH = $D4_LENGTH;\n";
    }
    if ( defined $DOM_LENGTH[4] ) {
     if ( $DOM_LENGTH[4] > $DOM_LENGTH[3] ) {
       print "D5_LENGTH reset to $DOM_LENGTH[3]\n" if ($DEBUG);
       $D5_LENGTH = $DOM_LENGTH[3];
     } else {
       $D5_LENGTH = $DOM_LENGTH[4];
     }
     print "D5_LENGTH = $D5_LENGTH\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$D5_LENGTH = $D5_LENGTH;\n";
    }
  }

  if (@COLD_LENGTH) {
  } else {
     if ($COLD_START_FCST) {
        @COLD_LENGTH = ($COLD_START_FCST) x 5;
     } else {
        print "ERROR: Neither \@COLD_LENGTH nor \$COLD_START_FCST is defined!\n";
        exit;
     }
  }

  if ( @COLD_LENGTH) {

    if ( defined $COLD_LENGTH[0] ) {
       $D1_COLD_LENGTH = $COLD_LENGTH[0];
     }
     print "D1_COLD_LENGTH = $D1_COLD_LENGTH\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$D1_COLD_LENGTH = $D1_COLD_LENGTH;\n";
    if ( defined $COLD_LENGTH[1] ) {
     if ( $COLD_LENGTH[1] > $COLD_LENGTH[0] ) {
       print "D2_COLD_LENGTH reset to $COLD_LENGTH[0]\n" if ($DEBUG);
       $D2_COLD_LENGTH = $COLD_LENGTH[0];
     } else {
       $D2_COLD_LENGTH = $COLD_LENGTH[1];
     }
     print "D2_COLD_LENGTH = $D2_COLD_LENGTH\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$D2_COLD_LENGTH = $D2_COLD_LENGTH;\n";
    }
    if ( defined $COLD_LENGTH[2] ) {
     if ( $COLD_LENGTH[2] > $COLD_LENGTH[1] ) {
       print "D3_COLD_LENGTH reset to $COLD_LENGTH[1]\n" if ($DEBUG);
       $D3_COLD_LENGTH = $COLD_LENGTH[1];
     } else {
       $D3_COLD_LENGTH = $COLD_LENGTH[2];
     }
     print "D3_COLD_LENGTH = $D3_COLD_LENGTH\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$D3_COLD_LENGTH = $D3_COLD_LENGTH;\n";
    }
    if ( defined $COLD_LENGTH[3] ) {
     if ( $COLD_LENGTH[3] > $COLD_LENGTH[2] ) {
       print "D4_COLD_LENGTH reset to $COLD_LENGTH[2]\n" if ($DEBUG);
       $D4_COLD_LENGTH = $COLD_LENGTH[2];
     } else {
       $D4_COLD_LENGTH = $COLD_LENGTH[3];
     }
     print "D4_COLD_LENGTH = $D4_COLD_LENGTH\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$D4_COLD_LENGTH = $D4_COLD_LENGTH;\n";
    }
    if ( defined $COLD_LENGTH[4] ) {
     if ( $COLD_LENGTH[4] > $COLD_LENGTH[3] ) {
       print "D5_COLD_LENGTH reset to $COLD_LENGTH[3]\n" if ($DEBUG);
       $D5_COLD_LENGTH = $COLD_LENGTH[3];
     } else {
       $D5_COLD_LENGTH = $COLD_LENGTH[4];
     }
     print "D5_COLD_LENGTH = $D5_COLD_LENGTH\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$D5_COLD_LENGTH = $D5_COLD_LENGTH;\n";
    }
  }

  if (!$D4_start)
  {
    $D4_start = 0;
  }
  print "D4_start = $D4_start\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$D4_start = $D4_start;\n";

  if ( @DOM_OUT_INT)
  {
    if ( defined $DOM_OUT_INT[0] ) {
      $D1_OUT_INT = $DOM_OUT_INT[0];
      print "D1_OUT_INT = $D1_OUT_INT\n" if ($DEBUG);
      print PRE_PROCESS_IN "\$D1_OUT_INT = $D1_OUT_INT;\n";
    }
    if ( defined $DOM_OUT_INT[1] ) {
      $D2_OUT_INT = $DOM_OUT_INT[1];
      print "D2_OUT_INT = $D2_OUT_INT\n" if ($DEBUG);
      print PRE_PROCESS_IN "\$D2_OUT_INT = $D2_OUT_INT;\n";
    }
    if ( defined $DOM_OUT_INT[2] ) {
      $D3_OUT_INT = $DOM_OUT_INT[2];
      print "D3_OUT_INT = $D3_OUT_INT\n" if ($DEBUG);
      print PRE_PROCESS_IN "\$D3_OUT_INT = $D3_OUT_INT;\n";
    }
    if ( defined $DOM_OUT_INT[3] ) {
      $D4_OUT_INT = $DOM_OUT_INT[3];
      print "D4_OUT_INT = $D4_OUT_INT\n" if ($DEBUG);
      print PRE_PROCESS_IN "\$D4_OUT_INT = $D4_OUT_INT;\n";
    }
    if ( defined $DOM_OUT_INT[4] ) {
      $D5_OUT_INT = $DOM_OUT_INT[4];
      print "D5_OUT_INT = $D5_OUT_INT\n" if ($DEBUG);
      print PRE_PROCESS_IN "\$D5_OUT_INT = $D5_OUT_INT;\n";
    }
  }

  if (defined $CYCLE_START_OFFSET) {
    print "CYCLE_START_OFFSET = $CYCLE_START_OFFSET\n" if ($DEBUG);
    print PRE_PROCESS_IN "\$CYCLE_START_OFFSET = $CYCLE_START_OFFSET;\n";
  }

  if (!$MM5HOME)
  {
    $MM5HOME = "/data/fddahome";
  }
  print "MM5HOME = $MM5HOME\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$MM5HOME = \"$MM5HOME\";\n";

  if (!$PERL_ARCHIVE)
  {
   $PERL_ARCHIVE = $MM5HOME.'/cycle_code/PERL';
  }
  print "PERL_ARCHIVE = $PERL_ARCHIVE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$PERL_ARCHIVE = \"$PERL_ARCHIVE\";\n";

  if (!$PERL_FLEX)
  {
   $PERL_FLEX = $PERL_ARCHIVE.'/flex';
  }
  print "PERL_FLEX = $PERL_FLEX\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$PERL_FLEX = \"$PERL_FLEX\";\n";

  if (!$DATADIR)
  {
    $DATADIR = "/data/input";
    if ( $BCS eq "NNRP" || $BCS eq "NNRP2" || $BCS eq "FNL" || $BCS eq "CFSR" ||  $BCS eq "CFSF" || $BCS eq "CFSV1" || $BCS eq "CFSV2")
    {
      $DATADIR = "/data/static";
    }
  }
  print "DATADIR = $DATADIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$DATADIR = \"$DATADIR\";\n";

  if (!$DATA_SST_DIR)
  {
    $DATA_SST_DIR = $DATADIR."/sst";
  }
  print "DATA_SST_DIR = $DATA_SST_DIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$DATA_SST_DIR = \"$DATA_SST_DIR\";\n";

  if (! defined ($USE_MODIS))
  {
    $USE_MODIS = 1;
  }
  print "USE_MODIS = $USE_MODIS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$USE_MODIS = \"$USE_MODIS\";\n";

  if (! defined ($USE_OI_DAILY_V2))
  {
    $USE_OI_DAILY_V2 = 0;
  }
  print "USE_OI_DAILY_V2 = $USE_OI_DAILY_V2\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$USE_OI_DAILY_V2 = \"$USE_OI_DAILY_V2\";\n";

  if (! defined ($USE_GLDAS))
  {
    $USE_GLDAS = 0;
  }
  print "USE_GLDAS = $USE_GLDAS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$USE_GLDAS = \"$USE_GLDAS\";\n";

  if (!$USE_GSL)
  {
    $USE_GSL = 0;
  }
  print "USE_GSL = $USE_GSL\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$USE_GSL = \"$USE_GSL\";\n";

  if (! defined ($DATA_LAND_DIR))
  {
    $DATA_LAND_DIR = $DATADIR."/GLDAS";
  }
  print "DATA_LAND_DIR = $DATA_LAND_DIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$DATA_LAND_DIR = \"$DATA_LAND_DIR\";\n";

#Badri Aug 2012
  if (!$NCOBIN)
  {
    $NCOBIN = "/opt/nco";
  }
  print "NCOBIN = $NCOBIN\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NCOBIN = \"$NCOBIN\";\n";

  if (! defined ($NETCDF_CONVERT)) {
    $NETCDF_CONVERT = 0;
  }
  print "NETCDF_CONVERT = $NETCDF_CONVERT\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NETCDF_CONVERT = \"$NETCDF_CONVERT\";\n";

#Needs to fix NETCDF var in moveMcycles.csh, CheckMfiles.csh and here.
  if (!$NETCDF)
  {
    $NETCDF = "/opt/netcdf";
  }
  print "NETCDF = $NETCDF\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NETCDF = \"$NETCDF\";\n";
#
  if (!$NCARG_ROOT)
  {
    $NCARG_ROOT = "/opt/ncl";
  }
  print "NCARG_ROOT = $NCARG_ROOT\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NCARG_ROOT = \"$NCARG_ROOT\";\n";

  if (!$NCARG_LIB)
  {
    $NCARG_LIB = $NCARG_ROOT."/lib";
  }
  print "NCARG_LIB = $NCARG_LIB\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NCARG_LIB = \"$NCARG_LIB\";\n";

  if (!$NCARG_RANGS_DIR)
  {
    $NCARG_RANGS_DIR = $NCARG_ROOT."/rangs";
  }
  print "NCARG_RANGS_DIR = $NCARG_RANGS_DIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NCARG_RANGS_DIR = \"$NCARG_RANGS_DIR\";\n";

  if (!$NCL_LIB)
  {
    $NCL_LIB = "/usr/share"; 
  }
  print "NCL_LIB = $NCL_LIB\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NCL_LIB = \"$NCL_LIB\";\n";

  if (!$MPICMD_BIN_DIR)
  {
    $MPICMD_BIN_DIR = "/opt/mpich/bin";
  }
  print "MPICMD_BIN_DIR = $MPICMD_BIN_DIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$MPICMD_BIN_DIR = \"$MPICMD_BIN_DIR\";\n";

  if (!$BCS)
  {
    $BCS  = "AVNFTP";
  }
  print "BCS = $BCS \n" if ($DEBUG);
  print PRE_PROCESS_IN "\$BCS = \"$BCS\";\n";

  if (!$DATA_ICBC_DIR)
  {
    if ( $BCS eq "ETA" )
    {
      $DATA_ICBC_DIR = $DATADIR."/eta";
    }

    if ( $BCS eq "AVNFTP" )
    {
      $DATA_ICBC_DIR = $DATADIR."/avnftp";
    }

    if ( $BCS eq "AVN" )
    {
      $DATA_ICBC_DIR = $DATADIR."/avn";
    }

    if ( $BCS eq "GFS004" )
    {
      $DATA_ICBC_DIR = $DATADIR."/GFS004";
    }

    if ( $BCS eq "NNRP" )
    {
      $DATA_ICBC_DIR = $DATADIR."/NNRP";
    }

    if ( $BCS eq "NNRP2" )
    {
      $DATA_ICBC_DIR = $DATADIR."/NNRP2";
    }

    if ( $BCS eq "FNL" )
    {
      $DATA_ICBC_DIR = $DATADIR."/FNL";
    }

    if ( $BCS eq "CFSR" )
    {
      $DATA_ICBC_DIR = $DATADIR."/CFSR";
    }

    if ( $BCS eq "CFSF" )
    {
      $DATA_ICBC_DIR = $DATADIR."/CFSF";
    }

    if ( $BCS eq "CFSV1" )
    {
      $DATA_ICBC_DIR = $DATADIR."/CFSV1";
    }

    if ( $BCS eq "CFSV2" )
    {
      $DATA_ICBC_DIR = $DATADIR."/CFSV2";
    }

    if (!$DATA_ICBC_DIR)
    {
      $DATA_ICBC_DIR  = $DATADIR."/avnftp";
    }
  }
  print "DATA_ICBC_DIR = $DATA_ICBC_DIR\n";
  print PRE_PROCESS_IN "\$DATA_ICBC_DIR = \"$DATA_ICBC_DIR\";\n";
  print "DATA_ICBC_DIR2 = $DATA_ICBC_DIR2\n";
  print PRE_PROCESS_IN "\$DATA_ICBC_DIR2 = \"$DATA_ICBC_DIR2\";\n";

  if (!$ICBC_NAME_TEMPLATE )
  {
    $ICBC_NAME_TEMPLATE = "CCYYMMDDHH_fh.FFFF_tl.press_gr.onedeg";
  }
  print "ICBC_NAME_TEMPLATE = $ICBC_NAME_TEMPLATE \n" if ($DEBUG);
  print PRE_PROCESS_IN "\$ICBC_NAME_TEMPLATE = \"$ICBC_NAME_TEMPLATE\";\n";
  print "ICBC_NAME_TEMPLATE2 = $ICBC_NAME_TEMPLATE2 \n" if ($DEBUG);
  print PRE_PROCESS_IN "\$ICBC_NAME_TEMPLATE2 = \"$ICBC_NAME_TEMPLATE2\";\n";

  if (!$ICBC_PREPROCESSOR)
  {
    if ( $BCS eq "ETA" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/ETA-preprocessor.pl';
    }

    if ( $BCS eq "AVNFTP" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/AVNFTP-preprocessor.pl';
    }

    if ( $BCS eq "AVN" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/AVN-preprocessor.pl';
    }

    if ( $BCS eq "GFS004" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/GFS004-preprocessor.pl';
    }

    if ( $BCS eq "NNRP" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/NNRP-preprocessor.pl';
    }

    if ( $BCS eq "NNRP2" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/NNRP2-preprocessor.pl';
    }

    if ( $BCS eq "FNL" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/FNL-preprocessor.pl';
    }

    if ( $BCS eq "CFSR" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/CFSR-preprocessor.pl';
    }

    if ( $BCS eq "CFSF" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/CFSF-preprocessor.pl';
    }

    if ( $BCS eq "CFSV1" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/CFSV1-preprocessor.pl';
    }

    if ( $BCS eq "CFSV2" )
    {
      $ICBC_PREPROCESSOR = $PERL_FLEX.'/ICBC/CFSV2-preprocessor.pl';
    }


    if (!$ICBC_PREPROCESSOR)
    {
      print "ICBC_PREPROCESSOR for $BCS cannot be determined! EXITING\n";
      close(PRE_PROCESS_IN);
      exit(-1);
    }
  }
  print "ICBC_PREPROCESSOR = $ICBC_PREPROCESSOR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$ICBC_PREPROCESSOR = \"$ICBC_PREPROCESSOR\";\n";

  if (!$PTOP)
  {
    if ($BCS eq 'AVN' || $BCS eq 'AVNFTP'|| $BCS eq 'GFS004' || $BCS eq 'NNRP' || $BCS eq 'NNRP2' || $BCS eq 'FNL' ||  $BCS eq 'CFSR' ||  $BCS eq 'CFSF' || $BCS eq 'CFSV1' ||  $BCS eq 'CFSV2')
    {
      $PTOP = 100;
    }
    elsif ($BCS eq 'ETA')
    {
      $PTOP = 50;
    }
  }
  print "PTOP = $PTOP\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$PTOP = $PTOP;\n";

  if (!$OBS_PROCESSOR)
  {
    $OBS_PROCESSOR = $PERL_FLEX.'/Observations/Obs-processor.pl'
  }
  print "OBS_PROCESSOR = $OBS_PROCESSOR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$OBS_PROCESSOR = \"$OBS_PROCESSOR\";\n";

  print "NO_WRFQC = $NO_WRFQC\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NO_WRFQC = $NO_WRFQC;\n";

  print "MADIS = $MADIS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$MADIS = $MADIS;\n";

  print "IAFSFC = $IAFSFC\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$IAFSFC = $IAFSFC;\n" if($IAFSFC);

  print "IAFUPR = $IAFUPR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$IAFUPR = $IAFUPR;\n" if($IAFUPR);

  print "RANGE_PROFILER = $RANGE_PROFILER\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RANGE_PROFILER = $RANGE_PROFILER;\n";

  print "SAMS = $SAMS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$SAMS = $SAMS;\n";

  print "DTE = $DTE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$DTE = $DTE;\n";

  print "GTS = $GTS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$GTS = $GTS;\n";

  print "WMO = $WMO\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$WMO = $WMO;\n";

  if ($PREPBUFR) {
  } else {
     $PREPBUFR = 0;
  }
  print "PREPBUFR = $PREPBUFR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$PREPBUFR = $PREPBUFR;\n";

  print "RAWS = $RAWS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RAWS = $RAWS;\n";

  print "OKMESO = $OKMESO\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$OKMESO = $OKMESO;\n";

  print "WVR = $WVR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$WVR = $WVR;\n";

  print "SAT = $SAT\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$SAT = $SAT;\n";

  print "CLASS = $CLASS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CLASS = $CLASS;\n";

  print "ACARS = $ACARS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$ACARS = $ACARS;\n";

  print "SATWINDS = $SATWINDS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$SATWINDS = $SATWINDS;\n";

  print "NPN_PROF = $NPN_PROF\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NPN_PROF = $NPN_PROF;\n";

  print "NIDSVAD = $NIDSVAD\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NIDSVAD = $NIDSVAD;\n";

  print "BLP_PROF = $BLP_PROF\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$BLP_PROF = $BLP_PROF;\n";

  print "DARPA_SODAR = $DARPA_SODAR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$DARPA_SODAR = $DARPA_SODAR;\n";

  print "DARPA_PWIDS = $DARPA_PWIDS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$DARPA_PWIDS = $DARPA_PWIDS;\n";

  print "DARPA_LIDARVAD = $DARPA_LIDARVAD\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$DARPA_LIDARVAD = $DARPA_LIDARVAD;\n";

  print "DARPA_DCNET = $DARPA_DCNET\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$DARPA_DCNET = $DARPA_DCNET;\n";

  print "QWND = $QWND\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$QWND = $QWND;\n";

  print "UAE_MICROSTEP = $UAE_MICROSTEP\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$UAE_MICROSTEP = $UAE_MICROSTEP;\n";

  print "SPECIAL = $SPECIAL\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$SPECIAL = $SPECIAL;\n";

  print "QCOUT = $QCOUT\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$QCOUT = $QCOUT;\n";

  print "TAMDAR = $TAMDAR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$TAMDAR = $TAMDAR;\n";

  print "ADP = $ADP\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$ADP = $ADP;\n";
  
  print "AFCCC = $AFCCC\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$AFCCC = $AFCCC;\n";

  if (defined($IAF_WORLD)) {
     print "IAF_WORLD = $IAF_WORLD\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$IAF_WORLD = $IAF_WORLD;\n";
  } else {
     print "IAF_WORLD = 0\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$IAF_WORLD = 0;\n";
  }

  if (defined($IAF_BUFR)) {
     print "IAF_BUFR = $IAF_BUFR\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$IAF_BUFR = $IAF_BUFR;\n";
  } else {
     print "IAF_BUFR = 0\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$IAF_BUFR = 0;\n";
  }

  if (defined($IAF)) {
     print "IAF = $IAF\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$IAF = $IAF;\n";
  } else {
     print "IAF = 0\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$IAF = 0;\n";
  }

  if (defined($AMV)) {
     print "AMV = $AMV\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$AMV = $AMV;\n";
  } else {
     print "AMV = 0\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$AMV = 0;\n";
  }

  if (defined($SPDB)) {
     print "SPDB = $SPDB\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$SPDB = $SPDB;\n";
  } else {
     print "SPDB = 0\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$SPDB = 0;\n";
  }

  if (defined($SAR_WIND)) {
     print "SAR_WIND = $SAR_WIND\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$SAR_WIND = $SAR_WIND;\n";
  } else {
     print "SAR_WIND = 0\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$SAR_WIND = 0;\n";
  }

  if ($PRELIM !~ /^0$/ && $PRELIM !~ /^1$/)
  {
    $PRELIM = 1;
  }
  print "PRELIM = $PRELIM \n" if ($DEBUG);
  print PRE_PROCESS_IN "\$PRELIM = $PRELIM;\n";

  if ($POSTPROCESS !~ /^0$/ && $POSTPROCESS !~ /^1$/)
  {
    $POSTPROCESS = 1;
  }
  print "POSTPROCESS = $POSTPROCESS \n" if ($DEBUG);
  print PRE_PROCESS_IN "\$POSTPROCESS = $POSTPROCESS;\n";

  if ($CONCAT_MMOUT !~ /^0$/ && $CONCAT_MMOUT !~ /^1$/)
  {
    $CONCAT_MMOUT = 1;
  }
  print "CONCAT_MMOUT = $CONCAT_MMOUT \n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CONCAT_MMOUT = $CONCAT_MMOUT;\n";

  if ($MM5FORVMET !~ /^0$/ && $MM5FORVMET !~ /^1$/)
  {
    $MM5FORVMET = 0;
  }
  print "MM5FORVMET = $MM5FORVMET\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$MM5FORVMET = $MM5FORVMET;\n";

  if ($VERI3HCYC !~ /^0$/ && $VERI3HCYC !~ /^1$/)
  {
    $VERI3HCYC = 0;
  }
  print "VERI3HCYC = $VERI3HCYC\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$VERI3HCYC = $VERI3HCYC;\n";

  if ($VERI_MC !~ /^0$/ && $VERI_MC !~ /^1$/)
  {
    $VERI_MC = 0;
  }
  print "VERI_MC = $VERI_MC\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$VERI_MC = $VERI_MC;\n";

  if ($GBC !~ /^0$/ && $GBC !~ /^1$/)
  {
    $GBC = 0;
  }
  print "GBC = $GBC\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$GBC = $GBC;\n";

  if ($AnEn !~ /^0$/ && $AnEn !~ /^1$/)
  {
    $AnEn = 0;
  }
  print "AnEn = $AnEn\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$AnEn = $AnEn;\n";

  if ($CLIMOCOMPACTRT !~ /^0$/ && $CLIMOCOMPACTRT !~ /^1$/)
  {
    $CLIMOCOMPACTRT = 0;
  }
  print "CLIMOCOMPACTRT = $CLIMOCOMPACTRT\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CLIMOCOMPACTRT = $CLIMOCOMPACTRT;\n";

  if ($NAPS !~ /^0$/ && $NAPS !~ /^1$/)
  {
    $NAPS = 0;
  }
  print "NAPS = $NAPS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$NAPS = $NAPS;\n";

  if (!$CLONEIN)
  {
    $CLONEIN = 0;
  }
  print "CLONEIN = $CLONEIN\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CLONEIN = $CLONEIN;\n";

  if (!$ETKF)
  {
    $ETKF = "no"
  }
  print "ETKF = $ETKF\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$ETKF = \"$ETKF\";\n";

  if (! defined ($SCIPUFF))
  {
    $SCIPUFF = 0;
  }
  print "SCIPUFF = $SCIPUFF\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$SCIPUFF = $SCIPUFF;\n";

  if (! defined ($CLEANDIR))
  {
    $CLEANDIR = 1;
  }
  print "CLEANDIR = $CLEANDIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CLEANDIR = $CLEANDIR;\n";

  print "re_run = $re_run\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$re_run = $re_run;\n";

  print "CASE_STUDY = $CASE_STUDY \n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CASE_STUDY = $CASE_STUDY;\n";

  # Checking environment variables
  if (!$MM5HOST)
  {
    $MM5HOST = $RANGE;
  }
  print "MM5HOST = $MM5HOST\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$MM5HOST = \"$MM5HOST\";\n";

  if (!$RUNDIR_ROOT)
  {
    $RUNDIR_ROOT =  "/data/cycles";
  }
  print "RUNDIR_ROOT = $RUNDIR_ROOT\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RUNDIR_ROOT = \"$RUNDIR_ROOT\";\n";

  if (!$RUNDIR)
  {
    $RUNDIR  = $RUNDIR_ROOT."/$GSJOBID/$NODE";
  }
  print "RUNDIR = $RUNDIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RUNDIR = \"$RUNDIR\";\n";

  if (!$DATA_DIR)
  {
    $DATA_DIR = $RUNDIR."/data";
  }
  print "DATA_DIR = $DATA_DIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$DATA_DIR = \"$DATA_DIR\";\n";

  $RESTART_ROOT = $RUNDIR;
  print "RESTART_ROOT = $RESTART_ROOT\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$RESTART_ROOT = \"$RESTART_ROOT\";\n";

  if ($RESTART_PER_CORE) {
     print "RESTART_PER_CORE = $RESTART_PER_CORE\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$RESTART_PER_CORE = $RESTART_PER_CORE;\n";
  } else {
     print "RESTART_PER_CORE = 0\n" if ($DEBUG);
     print PRE_PROCESS_IN "\$RESTART_PER_CORE = 0;\n";
  }

  $GEAP_ROOT = "/d1/$ENV{LOGNAME}/$GSJOBID";
  print "GEAP_ROOT = $GEAP_ROOT\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$GEAP_ROOT = \"$GEAP_ROOT\";\n";

  if (!$GEAPSTMP)
  {
    $GEAPSTMP = $GEAP_ROOT."/GEAPSTMP"."/".$this_cycle;
  }
  print "GEAPSTMP = $GEAPSTMP\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$GEAPSTMP = \"$GEAPSTMP\";\n";

  if ($CLEAN_GEAPSTMP !~ /^0$/ && $CLEAN_GEAPSTMP !~ /^1$/)
  {
    $CLEAN_GEAPSTMP = 1;
  }
  print "CLEAN_GEAPSTMP = $CLEAN_GEAPSTMP\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CLEAN_GEAPSTMP = \"$CLEAN_GEAPSTMP\";\n";

  if (!$GEAPSKEP)
  {
    $GEAPSKEP = $GEAP_ROOT."/GEAPSKEP"."/".$this_cycle;
  }
  print "GEAPSKEP = $GEAPSKEP\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$GEAPSKEP = \"$GEAPSKEP\";\n";

  if (!$POSTPROCS_TMP_DIR)
  {
    $POSTPROCS_TMP_DIR = $RUNDIR;
  }
  print "POSTPROCS_TMP_DIR = $POSTPROCS_TMP_DIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$POSTPROCS_TMP_DIR = \"$POSTPROCS_TMP_DIR\";\n";

  if (!$POSTPROCS_SAV_DIR)
  {
    $POSTPROCS_SAV_DIR = $RUNDIR;
  }
  print "POSTPROCS_SAV_DIR = $POSTPROCS_SAV_DIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$POSTPROCS_SAV_DIR = \"$POSTPROCS_SAV_DIR\";\n";

  if ($CLEAN_GEAPSKEP !~ /^0$/ && $CLEAN_GEAPKEP !~ /^1$/)
  {
    $CLEAN_GEAPSKEP = 1;
  }
  print "CLEAN_GEAPSKEP = $CLEAN_GEAPSKEP\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CLEAN_GEAPSKEP = \"$CLEAN_GEAPSKEP\";\n";

  if (!$CSH_ARCHIVE)
  {
    $CSH_ARCHIVE = $MM5HOME.'/cycle_code/CSH_ARCHIVE';
  }
  print "CSH_ARCHIVE = $CSH_ARCHIVE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CSH_ARCHIVE = \"$CSH_ARCHIVE\";\n";

  if (!$EXECUTABLE_ARCHIVE)
  {
    $EXECUTABLE_ARCHIVE = $MM5HOME.'/cycle_code/EXECUTABLE_ARCHIVE';
  }
  print "EXECUTABLE_ARCHIVE = $EXECUTABLE_ARCHIVE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$EXECUTABLE_ARCHIVE = \"$EXECUTABLE_ARCHIVE\";\n";

  if (!$PYTHON_ARCHIVE)
  {
    $PYTHON_ARCHIVE = $MM5HOME.'/cycle_code/PYTHON';
  }
  print "PYTHON_ARCHIVE = $PYTHON_ARCHIVE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$PYTHON_ARCHIVE = \"$PYTHON_ARCHIVE\";\n";

  if (!$PYTHONPATH)
  {
    $PYTHONPATH = '$PYTHON_ARCHIVE/modules';
  }
  print "PYTHONPATH = $PYTHONPATH\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$PYTHONPATH = \"$PYTHONPATH\";\n";

  if (!$GMT_BIN)
  {
    $GMT_BIN = '/p/work1/atec4dwx/opt/gmt-4.5.15/bin';
  }
  print "GMT_BIN = $GMT_BIN\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$GMT_BIN = \"$GMT_BIN\";\n";

  if (! defined ($LOCALHOST))
  {
    $LOCALHOST = $ENV{'HOST'};
  }
  print "LOCALHOST = $LOCALHOST\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$LOCALHOST = \"$LOCALHOST\";\n";

  if (! defined ($GSJOBTYPE))
  {
    $GSJOBTYPE = "climo";
  }
  print "GSJOBTYPE = $GSJOBTYPE\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$GSJOBTYPE = \"$GSJOBTYPE\";\n";

  if (! defined ($outJobID))
  {
    $outJobID = $GSJOBID;
  }
  print "outJobID = $outJobID\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$outJobID = \"$outJobID\";\n";


  if (!$MustHaveDir)
  {
    $MustHaveDir = $EXECUTABLE_ARCHIVE."/MustHaveDir";
  }
  print "MustHaveDir = $MustHaveDir\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$MustHaveDir = \"$MustHaveDir\";\n";

  if (!$CheckConfigFiles)
  {
    $CheckConfigFiles = $EXECUTABLE_ARCHIVE."/CheckConfigFiles";
  }
  print "CheckConfigFiles = $CheckConfigFiles\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CheckConfigFiles = \"$CheckConfigFiles\";\n";

  # 2009/09/28 addition for job status scripts:
  if (!$RTFDDA_LOG_DIR)
  {
    $RTFDDA_LOG_DIR = "/lustre/project/k1206/RTFDDA_LOG";
  }


  # 2007/05/18 addition begins: compatibility with GCAT code
  # in /cvs/projects/4dwx/gcat/SCRIPT_FILES
  if (!$CONFIGURE_FILES)
  {
    $CONFIGURE_FILES = $MM5HOME.'/cycle_code/CONFIG_FILES';
  }
  print "CONFIGURE_FILES = $CONFIGURE_FILES\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$CONFIGURE_FILES = \"$CONFIGURE_FILES\";\n";

  if (!$UTILS)
  {
    $UTILS = $MM5HOME.'/cycle_code/EXECUTABLE_ARCHIVE';
  }
  print "UTILS = $UTILS\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$UTILS = \"$UTILS\";\n";
  # 2007/05/18 addition ends

  if (!$WRF_OUTPUT_DIR)
  {
    $WRF_OUTPUT_DIR = "$RUNDIR_ROOT/${GSJOBID}_OUTPUT";
  }
  print "WRF_OUTPUT_DIR = $WRF_OUTPUT_DIR\n" if ($DEBUG);
  print PRE_PROCESS_IN "\$WRF_OUTPUT_DIR = \"$WRF_OUTPUT_DIR\";\n";

  # 2013/05/17 Added for RDA
  if (defined($RDA_NSSL_MOSAIC)) {
    print "RDA_NSSL_MOSAIC = $RDA_NSSL_MOSAIC\n" if ($DEBUG);
    print PRE_PROCESS_IN "\$RDA_NSSL_MOSAIC = \"$RDA_NSSL_MOSAIC\";\n";
  }
  if (defined($RDA_LEVEL_II)) {
    print "RDA_LEVEL_II = $RDA_LEVEL_II\n" if ($DEBUG);
    print PRE_PROCESS_IN "\$RDA_LEVEL_II = \"$RDA_LEVEL_II\";\n";
  }
  if (defined($RDA_HYBRID)) {
    print "RDA_HYBRID = $RDA_HYBRID\n" if ($DEBUG);
    print PRE_PROCESS_IN "\$RDA_HYBRID = \"$RDA_HYBRID\";\n";
  }
  if (defined($RDA_MOSIAC_MODEL_INPUT_DIR)) {
    print "RDA_MOSIAC_MODEL_INPUT_DIR = $RDA_MOSIAC_MODEL_INPUT_DIR\n" if ($DEBUG);
    print PRE_PROCESS_IN "\$RDA_MOSIAC_MODEL_INPUT_DIR = \"$RDA_MOSIAC_MODEL_INPUT_DIR\";\n";
  }
  if (defined($RDA_LEVELII_MODEL_INPUT_DIR)) {
    print "RDA_LEVELII_MODEL_INPUT_DIR = $RDA_LEVELII_MODEL_INPUT_DIR\n" if ($DEBUG);
    print PRE_PROCESS_IN "\$RDA_LEVELII_MODEL_INPUT_DIR = \"$RDA_LEVELII_MODEL_INPUT_DIR\";\n";
  }
  
  if ($CHEM) {
     print "WRF-Chem is turned on: CHEM = $CHEM\n" if($DEBUG);
     print PRE_PROCESS_IN "\$CHEM = $CHEM;\n";
  }

  if ($DUST_ONLY) {
     print "Dust-Only is turned on: DUST_ONLY = $DUST_ONLY\n" if($DEBUG);
     print PRE_PROCESS_IN "\$DUST_ONLY = $DUST_ONLY;\n";
  }

  close(PRE_PROCESS_IN);
}


#-------------------------------------------------------------------------------
# Name: writeEnvVarsFile
# Arguments: none
# Return: none
# Description: Writes envars file $GSJOBDIR/tmp/cshrc
#-------------------------------------------------------------------------------

sub writeEnvVarsFile
{
  print "Writing envars file $GSJOBDIR/tmp/cshrc\n";
  open(CSHRC,">$GSJOBDIR/tmp/cshrc");

  # Yes, pre_process_in_donotedit.pl is in $GSJOBDIR/tmp, but will be moved to
  # $GSJOBDIR/tmp/$this_cycle by qsub*.csh
  # Same for $GSJOBDIR/tmp/cshrc

  print CSHRC "setenv PRE_PROCESS_INPUT $GSJOBDIR/tmp/$this_cycle/pre_process_in_donotedit.pl\n";

  print CSHRC "setenv DEBUG $DEBUG\n";

  print CSHRC "setenv GSJOBDIR $GSJOBDIR\n";
  print CSHRC "setenv GSJOBID $GSJOBID\n";
  print CSHRC "setenv PERL_ARCHIVE $PERL_ARCHIVE\n";
  print CSHRC "setenv this_cycle $this_cycle\n";
  print CSHRC "setenv MODEL $MODEL\n";
  print CSHRC "setenv NUM_DOMS $NUM_DOMS\n";
  print CSHRC "setenv CYC_INT $CYC_INT\n";
  print CSHRC "setenv FIN_END $FIN_END\n";
  print CSHRC "setenv OUT_INT $OUT_INT\n";
  print CSHRC "setenv FCST_LENGTH $FCST_LENGTH\n";
  if ( defined $D1_OUT_INT ) {print CSHRC "setenv D1_OUT_INT $D1_OUT_INT\n";}
  if ( defined $D2_OUT_INT ) {print CSHRC "setenv D2_OUT_INT $D2_OUT_INT\n";}
  if ( defined $D3_OUT_INT ) {print CSHRC "setenv D3_OUT_INT $D3_OUT_INT\n";}
  if ( defined $D4_OUT_INT ) {print CSHRC "setenv D4_OUT_INT $D4_OUT_INT\n";}
  if ( defined $D5_OUT_INT ) {print CSHRC "setenv D5_OUT_INT $D5_OUT_INT\n";}
  if ( defined $D1_LENGTH ) {print CSHRC "setenv D1_LENGTH $D1_LENGTH\n";}
  if ( defined $D2_LENGTH ) {print CSHRC "setenv D2_LENGTH $D2_LENGTH\n";}
  if ( defined $D3_LENGTH ) {print CSHRC "setenv D3_LENGTH $D3_LENGTH\n";}
  if ( defined $D4_LENGTH ) {print CSHRC "setenv D4_LENGTH $D4_LENGTH\n";}
  if ( defined $D5_LENGTH ) {print CSHRC "setenv D5_LENGTH $D5_LENGTH\n";}
  if ( defined $D1_COLD_LENGTH ) {print CSHRC "setenv D1_COLD_LENGTH $D1_COLD_LENGTH\n";}
  if ( defined $D2_COLD_LENGTH ) {print CSHRC "setenv D2_COLD_LENGTH $D2_COLD_LENGTH\n";}
  if ( defined $D3_COLD_LENGTH ) {print CSHRC "setenv D3_COLD_LENGTH $D3_COLD_LENGTH\n";}
  if ( defined $D4_COLD_LENGTH ) {print CSHRC "setenv D4_COLD_LENGTH $D4_COLD_LENGTH\n";}
  if ( defined $D5_COLD_LENGTH ) {print CSHRC "setenv D5_COLD_LENGTH $D5_COLD_LENGTH\n";}
  if ( defined $CYCLE_START_OFFSET ) {print CSHRC "setenv CYCLE_START_OFFSET $CYCLE_START_OFFSET\n";}
  print CSHRC "setenv D4_start $D4_start\n";
  print CSHRC "setenv NODE $NODE\n";
  print CSHRC "setenv MPPJOB $MPPJOB\n";
  print CSHRC "setenv MPI_PRE_PROCESS $MPI_PRE_PROCESS \n";
  print CSHRC "setenv BCS $BCS\n";
  print CSHRC "setenv POSTPROCESS $POSTPROCESS\n";
  print CSHRC "setenv VERI3HCYC $VERI3HCYC\n";
  print CSHRC "setenv VERI_MC $VERI_MC\n";
  print CSHRC "setenv GBC $GBC\n";
  print CSHRC "setenv AnEn $AnEn\n";
  print CSHRC "setenv SCIPUFF $SCIPUFF\n";

  print CSHRC "setenv NUM_PROCS $NUM_PROCS\n";
  print CSHRC "setenv PPN $PPN\n";
  print CSHRC "setenv NUM_NODES $NUM_NODES\n";

  print CSHRC "setenv EMAIL \"$EMAIL\"\n";
  print CSHRC "setenv ACCOUNT \"$ACCOUNT\"\n";
  print CSHRC "setenv QUEUE_TYPE \"$QUEUE_TYPE\"\n";
  print CSHRC "setenv RESOURCE_LIST_PRE \"$RESOURCE_LIST_PRE\"\n";
  print CSHRC "setenv RESOURCE_LIST_PRE_P \"$RESOURCE_LIST_PRE_P\"\n";
  print CSHRC "setenv PRE_PROCESS_QUEUE \"$PRE_PROCESS_QUEUE\"\n";
  print CSHRC "setenv RESOURCE_LIST \"$RESOURCE_LIST\"\n";
  print CSHRC "setenv RESOURCE_LIST_POST \"$RESOURCE_LIST_POST\"\n";
  print CSHRC "setenv RESOURCE_LIST_VERIF \"$RESOURCE_LIST_VERIF\"\n";
  print CSHRC "setenv POST_PROCESS_QUEUE \"$POST_PROCESS_QUEUE\"\n";
  print CSHRC "setenv RESOURCE_LIST_CLEAN \"$RESOURCE_LIST_CLEAN\"\n";
  print CSHRC "setenv JOB_START \"$JOB_START\"\n";
  print CSHRC "setenv RES_NAME \"$RES_NAME\"\n";
  print CSHRC "setenv BATCH_SYSTEM \"$BATCH_SYSTEM\"\n";

  print CSHRC "setenv ETKF $ETKF\n";

  print CSHRC "setenv MM5HOST $RANGE\n" ;
  print CSHRC "setenv MM5HOME $MM5HOME\n";
  print CSHRC "setenv EXECUTABLE_ARCHIVE $EXECUTABLE_ARCHIVE\n";
  print CSHRC "setenv CSH_ARCHIVE $CSH_ARCHIVE\n";
  print CSHRC "setenv PYTHON_ARCHIVE $PYTHON_ARCHIVE\n";
  print CSHRC "setenv PYTHONPATH $PYTHONPATH\n";
  print CSHRC "setenv LOCALHOST $LOCALHOST\n";
  print CSHRC "setenv RUNDIR_ROOT $RUNDIR_ROOT\n";
  print CSHRC "setenv RUNDIR $RUNDIR\n";
  print CSHRC "setenv DATA_DIR $DATADIR\n";
  print CSHRC "setenv DATA_SST_DIR $DATA_SST_DIR\n";
  print CSHRC "setenv DATA_LAND_DIR $DATA_LAND_DIR\n";
  print CSHRC "setenv NCARG_ROOT $NCARG_ROOT\n";
  print CSHRC "setenv NCARG_LIB $NCARG_LIB\n";
  print CSHRC "setenv NCARG_RANGS_DIR $NCARG_RANGS_DIR\n";
  print CSHRC "setenv NCL_LIB $NCL_LIB\n";
  print CSHRC "setenv RESTART_ROOT $RESTART_ROOT\n";
  print CSHRC "setenv USE_MODIS $USE_MODIS\n";
  print CSHRC "setenv USE_OI_DAILY_V2 $USE_OI_DAILY_V2\n";
  print CSHRC "setenv USE_GLDAS $USE_GLDAS\n";
  print CSHRC "setenv USE_GSL $USE_GSL\n";
  print CSHRC "setenv GSJOBTYPE $GSJOBTYPE\n";
  print CSHRC "setenv outJobID $outJobID\n";
  print CSHRC "setenv GEAP_ROOT $GEAP_ROOT\n";
  print CSHRC "setenv GEAPSTMP $GEAPSTMP\n";
  print CSHRC "setenv GEAPSKEP $GEAPSKEP\n";
  print CSHRC "setenv CheckConfigFiles $CheckConfigFiles\n";
  print CSHRC "setenv MustHaveDir $MustHaveDir\n";
  print CSHRC "setenv UTILS $UTILS\n";
  print CSHRC "setenv CONFIGURE_FILES $CONFIGURE_FILES\n";
  print CSHRC "setenv MPICMD_BIN_DIR  $MPICMD_BIN_DIR\n";
  print CSHRC "setenv RTFDDA_LOG_DIR $RTFDDA_LOG_DIR\n";
  print CSHRC "setenv CLEAN_GEAPSTMP $CLEAN_GEAPSTMP\n";
  print CSHRC "setenv CLEAN_GEAPSKEP $CLEAN_GEAPSKEP\n";

#  $tempi = "";
#  foreach $NN (@nodes)
#  {
#    if ($NN eq $nodes[0])
#    {
#      $tempi = $NN;
#    }
#    else
#    {
#      $tempi = "$tempi+$NN";
#    }
#  }
#  print CSHRC "setenv NODE_LIST $tempi\n";

  # 2013/05/17 Added for RDA
  print CSHRC "setenv FLEXINPUT $FLEXINPUT\n";
  print CSHRC "setenv HYBRID $HYBRID\n" if (defined($HYBRID));
  print CSHRC "setenv RDA_HYBRID $RDA_HYBRID\n" if (defined($RDA_HYBRID));
  print CSHRC "setenv RDA_LEVEL_II $RDA_LEVEL_II\n" if (defined($RDA_LEVEL_II));
  print CSHRC "setenv RDA_NSSL_MOSAIC $RDA_NSSL_MOSAIC\n" if (defined($RDA_NSSL_MOSAIC));
  print CSHRC "setenv RDA_MOSIAC_MODEL_INPUT_DIR $RDA_MOSIAC_MODEL_INPUT_DIR\n" if (defined($RDA_MOSIAC_MODEL_INPUT_DIR));
  print CSHRC "setenv RDA_LEVELII_MODEL_INPUT_DIR $RDA_LEVELII_MODEL_INPUT_DIR\n" if (defined($RDA_LEVELII_MODEL_INPUT_DIR));
  print CSHRC "setenv NETCDF_CONVERT $NETCDF_CONVERT\n";
  print CSHRC "setenv NETCDF $NETCDF\n";
  print CSHRC "setenv CHEM $CHEM\n" if($CHEM);
  print CSHRC "setenv DUST_ONLY $DUST_ONLY\n" if($DUST_ONLY);
  if($RESTART_PER_CORE) {
    print CSHRC "setenv RESTART_PER_CORE 1\n";
  } else {
    print CSHRC "setenv RESTART_PER_CORE 0\n";
  }

  close(CSHRC);
}

#-------------------------------------------------------------------------------
# Name: checkRUNDIRDirs
# Arguments: none
# Return: none
# Description:
#   This method checks whether necessary directories are there.
#   If not there, it creates them.
#-------------------------------------------------------------------------------

sub checkRUNDIRDirs
{
  # Check for necessary directories
  if ( ! -e $RUNDIR )
  {
    system("$MustHaveDir $RUNDIR_ROOT/$GSJOBID");
    system("$MustHaveDir $RUNDIR_ROOT/$GSJOBID/$RANGE");
    chdir $RUNDIR_ROOT."/$GSJOBID";
    system("ln -s -f $RANGE $NODE");
  }

  if (-e "$RUNDIR/$this_cycle")
  {
    system("rm -r -f $RUNDIR/$this_cycle");
  }

  system("$MustHaveDir $RUNDIR/$this_cycle");
  system("$MustHaveDir $DATA_DIR");
  system("$MustHaveDir $DATA_DIR/gts");

  #  We also need these working directories
  if( $MPPJOB eq "yes")
  {
    system("$MustHaveDir $RUNDIR/restart_files");
    system("$MustHaveDir $RUNDIR/restart_files/zold");
  } # if MPPJOB

  system("$MustHaveDir $RUNDIR/data");
  system("$MustHaveDir $RUNDIR/data/gts");
  system("$MustHaveDir $RUNDIR/verify");
  system("$MustHaveDir $RUNDIR/veri_dat");
}

#-------------------------------------------------------------------------------
# Name: usage
# Arguments: none
# Return: none
# Description:
#   The usage subroutine "flexinput.pl -h"
#-------------------------------------------------------------------------------

sub usage
{
  print "init.pl <-crastiqfh>\n";
  print " where  \n";
  print "   -c cycle_time:       format is YYYYMMDDhh, will override 'this_cycle'\n";
  print "   -r reservation_id:   will override 'RES_NAME'\n";
  print "   -a project_account:  will override 'ACCOUNT_KEY'\n";
  print "   -s start_time:       when should the scheduler run the job, format is YYYYMMDDhhmm.ss,\n";
  print "                        will override 'JOB_START_TIME'\n";
  print "   -t pre_proc_type \n";
  print "   -i cyc_int:          will override 'CYC_INT' \n";
  print "   -q queue_name:       will override 'QUEUE_TYPE'\n";
  print "   -f env_vars_file\n";
  print "   -h help\n";
  print "All flags are optional.\n";
  print "\n";
}
1;

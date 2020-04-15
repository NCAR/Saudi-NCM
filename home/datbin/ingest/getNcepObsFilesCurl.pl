#! /usr/bin/perl
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2010 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2010/10/12 4:51:13 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 


#------------------------------------------------------------------------------#
# Name: getNcepObsFilesCurl.pl
# 
# Function: Uses ftp to pull obs files from the NCEP obs ftp site and write
#           them to specific local directories.
#
# Overview (from original script):
#
#  The script lists all the files available for the times defined above
#  and the observational platforms requested in variable SUBSCRIBED on the server,
#  examines it to see what it needs and then gets only the files its need 
#  (ie those that are not already been ftped or those who size has changed
#  since the previous ftp). It keeps an inventory of all files retrieved so 
#  that we don't get them redundantly. 
#
#  Data arrive arrive on the NCEP server on a continuous, asynchronous schedule,
#  and the current and previous hour's data are processed every 5 minutes. 
#  The data are segmented into hourly files, with the mesonet file for hour HH 
#  containing data for HH00 through HH59. In order to get regularly-scheduled 
#  hourly reports from the national data sources (METAR, SAO, and maritime) 
#  these files contain data starting 15 minutes before the hour and ending 
#  44 minutes after the hour (e.g., the 0000 file covers 2345 - 0044). 
#  The most complete data for a given hour is available a little after 2 hours 
#  following the file time. The user should also understand that the mesonet 
#  data isn't as timely as the national data sources. The lag time of these 
#  reports (lag = time available from FSL - observation time) ranges from about 
#  8 to 45 minutes, and can sometimes be longer.
#
#  See http://www-sdd.fsl.noaa.gov/MADIS/index.html for more details
#
# Original script written by: Francois Vandenberghe, vandenb@ucar.edu, April 2004.
#
# Modified to add procmap, ldata, param file usage, Nov 2009
#
#==============================================================================

#------------------------------------------------------------------------------#
# Needed Perl modules
#------------------------------------------------------------------------------#
use Time::Local;
use Getopt::Long;
use XML::Simple;
use Env;
use strict;

#--------------------------------------------------------------
# Needed RAL perl libraries
#--------------------------------------------------------------
use lib "$ENV{RAP_SHARED_LIB_DIR}/perl5/";
use lib "$ENV{RAP_LIB_DIR}/perl5/";
use Niwot;
use Toolsa;
use NNTUtils;

#--------------------------------------------------------------
# Hardwired defaults
#--------------------------------------------------------------
# ExitSuccess: return code from script on success
# ExitFailure: return code from script on failure
# NeededExes: list of required executables that need to be in the PATH
# LagHrsArr: array of "lag" hour offsets before start time to
#            check for new data on FTP site. Intentionally in
#            reverse order so DataMapper shows latest data
# DoRealTime: flag to do procmap reg, etc.
# RegInterval: Registration interval for procmap (secs)
# DefaultSleep: default sleep seconds if none specified
# MaxInventoryFileLines: max lines in inventory file
# MaxFtp: max number of files to FTP at one time

our $ExitSuccess=1;
our $ExitFailure=-1;
our @NeededExes=('LdataWriter', 'curl');
our @LagHrsArr=(2, 1, 0);
our $DoRealTime=1;
our $RegInterval=120;
our $DefaultSleep=120;
our $MaxInventoryFileLines=1000;
our $MaxFtp=24;

#--------------------------------------------------------------
# Message Logging
#--------------------------------------------------------------
#R Niwot::setMsgStderr();

#-------------------------------------------------------------
# Usage
#-------------------------------------------------------------
#
# Get the program basename.
#
our $prog;
($prog = $0) =~ s|.*/||;

#
# Setup usage statement
#
our $usage =
    "\nUsage: $prog -params <file>|-help|-print_params [-debug] [-test] [-runonce]\n" .
    "   [ -params ?]     - (required) name of the parameter  file to read.\n" .
    "   [ -print_params] - print a default param file to STDOUT then exit\n" . 
    "   [ -help]         - print usage\n" .
    "   [ -debug ]       - turn on debugging. Note that if this is on\n" .
    "                      temporary files and directories will not be\n" .
    "                      removed.\n" .
    "   [ -runonce ]     - run once and exit, ignores times in the param file\n" .
    "   [ -test ]        - test mode, do not actually retrieve any files\n" .
##    "   [ -verbose]    - turn on verbose debug message\n" . 
##    "   [ -start ?]    - start time. Default is current UTC time minus 1 hr\n" .
##    "                    If specified, format is CCYYMMDDHH\n"
##    "                    Not fully implemented at this time\n"
    "\n";

#----------------------------------------------------------------------
#                   Initialize command line
#----------------------------------------------------------------------
#
# Initialize command line arguments
#
our $paramFile          = "";
our $verbose            = 0;
our $debug              = 0;
our $test               = 0;
our $start              = 0;
our $help               = 0;
our $print_params       = 0;
our $run_once           = 0;
 
#----------------------------------------------------------------
#                  Process command line arguments
#----------------------------------------------------------------

our $result = &GetOptions("params=s" => \$paramFile,
			  "print_params" => \$print_params,
			  "help" => \$help,
			  "debug" => \$debug,
			  "verbose" => \$verbose,
			  "test" => \$test,
			  "runonce" => \$run_once,
			  "start=s" => \$start,
			  '<>', \&badArg);

if (($result == 0) || ( $help )) {
    print "$usage\n";
    exit $ExitFailure;
}

if ($print_params) {
    &printParams();
    exit $ExitFailure;
}

if ($paramFile !~ /\w/) {
    print ("*** ERROR: you must specify a param file to read\n");
    exit $ExitFailure;
}

#-------------------------------------------------------------
#            Set the start time -- not really implemented
#--------------------------------------------------------------

#if ($start) {
#    if ($start < 1000000000 || $start > 9999999999) {
#	print "ERROR: Invalid start time: $start\n";
#	&help;
#    }
#    $start_year=substr($start, 0, 4);
#    $start_mon=substr($start, 4, 2);
#    $start_day=substr($start, 6, 2);
#    $start_hour=substr($start, 8, 2);
#    $start_min=substr($start, 10, 2);
#    $start_sec=0;
#    $DoRealTime=0;
#}

#-------------------------------------------------------------
#            Read the parameter file and set various initial
#            variables
#--------------------------------------------------------------
our $params = XMLin($paramFile);
our $FtpHostDirVar;

# Print param file settings

if ($debug) {
    &printParamSettings();
}

if (defined($params->{ftpHostDir}) || ($params->{ftpHostDir} =~ /\w/)) {
    $FtpHostDirVar=$params->{ftpHostDir};
} else {
    $FtpHostDirVar="";
}
#R Niwot::postDebug("FtpHostDirVar: $FtpHostDirVar");

# Set initial variables

our ($doTimeTrigger, $data, @checktime_arr, $nchecktime_arr);
if (defined($params->{checkTime})) {
    $doTimeTrigger=1;
    $nchecktime_arr=0;
    foreach $data (@{$params->{checkTime}}) {
	$checktime_arr[$nchecktime_arr]=$data;
	$nchecktime_arr++;
    }
} else {
    $doTimeTrigger=0;
}

if ($run_once) {
    $doTimeTrigger=0;
}

# Intermediate file names

our $dir_list = "remote_dir_list.asc";
our $inv_file = "inventory.asc";
our $new = 'new.asc';

#-------------------------------------------------------------
#            Error checking before start main
#--------------------------------------------------------------

our $rapDataDir=$ENV{RAP_DATA_DIR};
#R if ($DoRealTime > 0) {
#R     my $is_ok;
#R     $is_ok=NNTUtils::checkExes(\@NeededExes, $debug);
#R     if ($is_ok < 1) {
#R        exit $ExitFailure;
#R     }
#R     NNTUtils::checkDidssEnvVars($debug);
#R }

#-------------------------------------------------------------
#            Realtime - do procmap registration, etc.
#--------------------------------------------------------------

# Set up signal handlers now, just before the main loop.

$SIG{'INT'} = $SIG{'QUIT'} = $SIG{'KILL'} = $SIG{'TERM'} = 'doSignalExit';

# Register with procmap

#R if ($DoRealTime > 0) {
#R     Toolsa::PMU_auto_init($prog, $params->{instance}, $RegInterval);
#R }

#-------------------------------------------------------------
#            Loop to get data and sleep
#--------------------------------------------------------------

# Initialize

our ($cmd, $NCEP_OBS_DATA, $icnt);
our (@sublist, @sublistwithpath, @subUrlList, $i, $j, $is_ok);
our ($found, $data_time, $last_index, $size, $file_size, $file_name);
our ($file, $file_sizes);
our ($dum1,$dum2,$dum3,$dum4,$dum5,$dum6,$dum7);
our $new_file;
our ($start_loop_utime, $end_loop_utime, $elapsed_loop_utime);
our $do_loop=1;

while ($do_loop > 0) {

    # Run once

    if (($run_once) || ($test)) {
	$do_loop = 0;
    }

    # Realtime -- procmap register

#R     if ($DoRealTime > 0) {
#R        Toolsa::PMU_auto_register("Starting loop...");
#R     }
#R     if ($debug) {
#R        Niwot::postDebug("Starting data retrieval loop");
#R     }

    # Check if we are at a trigger time (if this is the chosen
    # timing option)
    
#R    if ($doTimeTrigger > 0) {
#R       Niwot::postDebug("Go into waitForTriggerMin");
#R       $is_ok=NNTUtils::waitForTriggerMin($DefaultSleep, \@checktime_arr, $nchecktime_arr,
#R	                                    $DoRealTime, $test, $debug);
#R    }
    
#R    if ($DoRealTime > 0) {
#R       Toolsa::PMU_auto_register("Preparing for data retrieval");
#R    }
#R    if ($debug) {
#R       Niwot::postDebug("Start data retrieval...");
#R    }

    # Get the start time for data retrieval

    $start_loop_utime=NNTUtils::getNowAsUtime();

    # Setup date/time strings for data retrieval

    our ($lag, $str, @dataTimesArr);
    @dataTimesArr = ();
    foreach $lag (@LagHrsArr) {
	($is_ok, $str)=&genDateString($lag, $debug);
	push(@dataTimesArr, $str);
	if ($debug) {
	    Niwot::postDebug("Setup date/time strings for data retrieval: $str");
	}
    }

    if ($DoRealTime > 0 ) {
      Toolsa::PMU_auto_register("Setting up date/time strings for data retrieval" );
    }

    # Count the total number of new files brought on the disk
    # hold onto a filename and datatime to use with datamapper registration

    our $ftped_files = 0;
    our $hold_ftp_filename="";
    our $hold_data_time="";

    # Define file paths
    #   If the output directory path does not begin with '.' or '/'
    #   it is assumed to be relative to $RAP_DATA_DIR, so prepend
    #   local file path with $RAP_DATA_DIR in that case
    # Need to set MADIS_DATA explicitly

    our $outputDir = NNTUtils::expandPath($params->{outputDir});
    $NCEP_OBS_DATA=$outputDir;

    # Loop over the desired data times. This is done here so that
    # writing the _latest_data_info files makes sense and do not go
    # forward and backward in time. The _latest_data_info files are
    # the trigger for the decoder.

    our $data_time_counter=0;
    foreach $data_time (@dataTimesArr) {

	Niwot::postInfo("------------------------------------------------------");
	Niwot::postInfo("Retrieving data for data_time: $data_time");  

	# Loop over the subscribed platforms

	foreach $data (@{$params->{subscribedDir}}) {

	    Niwot::postInfo("------------------------------------------------------");
	    Niwot::postInfo("Retrieving data for $data");  

            if ( $DoRealTime > 0) {
              Toolsa::PMU_auto_register( "Retrieving data" );
            }

	    our $now=time;

	   ## handle special case for data dirs

	   #our $datanc;
	   #if ($data =~ /RSAS/) {
           #    $datanc = $data;
	   #} elsif ($data =~ /LDAD/) {
           #   $datanc = "${data}/netCDF";
	   #} else {
           #   $datanc = "${data}/netcdf";
	   #}

	   # Define file paths

            our $ftpUrl = "-O ftp:\/\/$params->{ftpHost}";
#	    our $ftpDataPath="$params->{FtpHostDirVar}/${datanc}";
	    our $ftpDataPath="${FtpHostDirVar}/${data}";
#	    our $localDataPath="${outputDir}/$params->{ftpHostDir}/${datanc}";
	    our $localDataPath="${outputDir}/${FtpHostDirVar}/${data}";
	    our $ftplock = "${localDataPath}/$now.lock";

	    # Create the local output directory

            if ($DoRealTime > 0) {
              Toolsa::PMU_auto_register( "Creating local output directory" );
            }

	    if (! -d "$localDataPath") {
		system("mkdir -p $localDataPath");
	    }

	    # Create an inventory file if it doesn't already exist
	    # if it does exist, cut down to a smaller size

            if ( $DoRealTime > 0) {
              Toolsa::PMU_auto_register( "Creating inventory file" );
            }

	    if (! -e "$localDataPath/$inv_file") {
		open INVENTORY, ">$localDataPath/$inv_file";
		close INVENTORY;
	    } else {
		$cmd="tail --lines=$MaxInventoryFileLines $localDataPath/$inv_file > $localDataPath/${inv_file}.tmp";
		system($cmd);
		if (-e "$localDataPath/${inv_file}.tmp") {
		    $cmd="mv $localDataPath/${inv_file}.tmp $localDataPath/$inv_file";
		    system($cmd);
		}
	    }
	
	    # Open lock file

            if ( $DoRealTime ) {
              Toolsa::PMU_auto_register( "Creating lock" );
            }
	    
	    open FTPLOCK, ">$ftplock";
	    exit $ExitFailure unless(flock (FTPLOCK, 2|4));  # Non-blocking lock

	    # Debug

	    if ($debug) {
		Niwot::postDebug("\tftp output will go to file $localDataPath/$dir_list");
		Niwot::postDebug("\tfiles inventory is in $localDataPath/$inv_file");
	    }

	    if (! (chdir $localDataPath)) {
		Niwot::postFatal("ERROR: Can't cd to $localDataPath: $!\n");
		exit $ExitFailure;
	    }
    
	    # Get a directory listing from the data source so we can decide
	    # which files to get. Only do this the first time through
	    # for this data type so do not hammer the server multiple times.
	    # We will reuse the list.

            if ($DoRealTime > 0) {
              Toolsa::PMU_auto_register( "Creating directory listing" );
            }

	    our $ftpstat=0;
	    if (($data_time_counter < 1) || (!-e $dir_list)) {
		# If doing passwordless retrieve, need to use different command
		if ((($params->{username} !~ /\w/) && ($params->{passwd} !~ /\w/)) || ($params->{username} =~ /anonymous/)) {
		    $cmd="curl --disable-epsv ftp\:\/\/$params->{ftpHost}\/${ftpDataPath}\/ \> $dir_list";
		} else {
		    $cmd="curl --ftp-ssl --disable-epsv -u $params->{username}:$params->{passwd} ftp\:\/\/$params->{ftpHost}\/${ftpDataPath}\/ \> $dir_list";
		}

		if ($debug) {
		    Niwot::postDebug("Running cmd: $cmd");
		}

		if ($test) {
		    Niwot::postInfo("Test mode: Would run command: $cmd");
		} else {
		    $ftpstat = system($cmd);
		}
	    }

	    # Main loop to determine files we need from the server
	    # For speed, it appears that a list of existing files is put into
	    # a hash: inv_list with sizes: inv_size.
	    # The original code captured the return value from the system command
	    # in ftpstat but note that that is not actually the return from the
	    # command called by the system() call, just the system() call itself
	    
	    our (@ftp_list, %inv_list, %inv_size);
	    undef @ftp_list;
	    undef %inv_list;
	    undef %inv_size;

            if ($DoRealTime > 0) {
              Toolsa::PMU_auto_register( "Figure out which files we need from server" );
            }

	    if ($ftpstat == 0) {

		# Read the inventory for files we already have

                if ($DoRealTime >0) {
                  Toolsa::PMU_auto_register( "Checking inventory file" );
                }

		$icnt = 0;
		if (open (INVENTORY, " < $inv_file")) {
		    while (<INVENTORY>) {
			chomp;
			$file =~ s#.*/##; 
			($file, $file_size) = split /\s+/;
			$file =~ s#.*/##; 
			if ($debug) {
			    Niwot::postDebug("Inventory says: File $file was copied ($file_size blocks)");
			}
			$inv_list{$file} = 1;
			$inv_size{"$ftpDataPath/$file"} = "$file_size";
			$icnt++;
		    }
		    close INVENTORY;
		}

		# Create a list of current files in the directory then loop though lines

                if ($DoRealTime >0 ) {
                  Toolsa::PMU_auto_register( "Creating list of files in directory" );
                }
	    
		if (open (LIST, "< $dir_list")) {

		    while (<LIST>) {

			# Skip entries which do not match the file pattern

			$found=0;
			if ($_ =~ /txt$/) {
			    $found=1;
			}

                       #$found=1; # RONG
			if ($found < 1) {
			    next;
			}

			# grab the name of the file if pattern is matched.

			our ($prv, $lnk, $uid, $gid, $month, $fd, $fh, $fmn, $ftime);
			chomp $_;
			($prv, $lnk, $uid, $gid, $size, $month, $fd, $ftime, $file) = split (/\s+/);
			($fh, $fmn) = split (/\:/, $ftime);

			# Decide if this file is from the data date
			# Maybe check filename pattern again???

			# See if we already have this in the inventory and if the size
			# is different

			if (exists($inv_list{"$file"})){
			    $file_size = $inv_size{"$ftpDataPath/$file"};
			   #next if ($size <= $file_size);
			    next if ($size == $file_size);
			} 

			push @ftp_list, $file;
		    
			if (! exists($inv_list{"$file"})){ 
			    Niwot::postInfo ("Get  $file new file   ($size blocks)");
			} elsif ($size >  $file_size) {
			    $file_size = $inv_size{"$ftpDataPath/$file"};
			    Niwot::postInfo ("Get  $file new size is $size blocks (was $file_size previously)");
			} else {
			    Niwot::postInfo ("Skip $ftpDataPath/$file");
			}
		    } #endwhile (LIST)
	
		    close LIST;
		
		} #endif (open(LIST)
	    } else {
		Niwot::postInfo("Could not get server directory listing, or no files available for $data");
	    } #endif ftpstat==0

	    # If we have a file list, get the files. Break up a large list into batches

	    if (defined(@ftp_list)) {

                open(NEW,">>$new");
                foreach $new_file (@ftp_list) {
                   print NEW "$new_file\n";
                }
                close(NEW);

                if ( $DoRealTime >0 ) {
                  Toolsa::PMU_auto_register("Getting files");
                }

		# Ftp a set number of files at a time.
                my $numList = $#ftp_list + 1;
                Niwot::postInfo("Number of files in ftp list = $numList" );

		for ($i=0; $i<=$#ftp_list; $i+=$MaxFtp) {

		    $last_index = $i+ $MaxFtp -1;
		    $last_index = $#ftp_list if (($i +$MaxFtp -1) > $#ftp_list);
		    @sublist = @ftp_list[$i .. $last_index];

		    for ($j=$i; $j<=$last_index; $j+=1) {
		      $sublistwithpath[$j-$i] = "$ftpDataPath/$ftp_list[$j]";
                      $subUrlList[$j-$i] = "$ftpUrl/$sublistwithpath[$j-$i]";
                        Niwot::postInfo("FILE: $sublistwithpath[$j-$i]" );
		    }
		    Niwot::postInfo ("Use FTP to pull @sublistwithpath");

		    # If doing passwordless retrieve, need to use different command
		    if ((($params->{username} !~ /\w/) && ($params->{passwd} !~ /\w/)) || ($params->{username} =~ /anonymous/)) {
			$cmd="curl --disable-epsv @subUrlList";
		    } else {
			$cmd="curl --ftp-ssl --disable-epsv -u $params->{username}:$params->{passwd} @subUrlList";
		    }
                    
		   # $cmd="ncftpget -d stdout -u $params->{username} -p $params->{passwd} -F -Z $params->{ftpHost} $localDataPath @sublistwithpath > /dev/null 2>&1";
		    Niwot::postInfo("Running cmd: $cmd");

		    if ($test) {
			Niwot::postInfo("Test mode: Would run command: $cmd");
		    } else {
			$ftpstat = system($cmd);
		    }

		    # Add files to inventory and unzip

                    if ( $DoRealTime > 0) {
                      Toolsa::PMU_auto_register( "Updating inventory and unzipping files" );
                    }

		    if ( $ftpstat == 0 ) {
			open INVENTORY, ">>$inv_file";
			foreach $file (@sublistwithpath) {
			   #$file_sizes = `ls -s --block-size=1 $NCEP_OBS_DATA/$file`;
			    $file_sizes = `ls -l $NCEP_OBS_DATA/$file`;
			    $_ = $file_sizes;
			    ($dum1, $dum2, $dum3, $dum4, $file_size, $dum5,
                             $dum6, $dum7, $file_name) = split /\s+/;
			    print INVENTORY "$file $file_size\n";
			    ($is_ok, $hold_ftp_filename)=genLdataInfoFromFilename($file, $debug);
			}
			close INVENTORY;
		    
			$cmd="gunzip -f @sublist";
			if ($test) {
			    Niwot::postInfo("Test mode: Would run command: $cmd");
			} else {
			#   system($cmd);
			}
		    } else {
			Niwot::postError("Error: Could not get @sublistwithpath");
			last;
		    }

		    undef @sublistwithpath;
		    undef @sublist;
                    undef @subUrlList;
		} #endfor MaxFtp
	    
		# write a latest_data_info file and register with DataMapper 
		# for this subscribedDir if actually got any files
		# Determine the data time to use: current? or conglomerate the
		# filename (YYYYMMDD_HHMM) to a YYYYMMDDHHMMSS ldata time?

		# Need to determine first if the file actually got written recently

		my $local_file="${localDataPath}/${hold_ftp_filename}";
		my($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks);
		our $did_retrieve_file=0;
		if (-f $local_file) {
		    ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks)=stat($local_file);
		    my $current_utime=NNTUtils::getNowAsUtime();
		    my $modified_secs_ago=$current_utime-$mtime;
		    if ($modified_secs_ago <= (60 * 15)) {
			$did_retrieve_file=1;
		    }
		}
		if ($debug) {
		    Niwot::postDebug("Flag for did update local file $local_file: $did_retrieve_file");
		}
		if ($did_retrieve_file > 0) {
		    if (($DoRealTime > 0) && (defined(@ftp_list))) {
			($is_ok, $hold_data_time)=&genLatestDataTime($hold_ftp_filename, $debug);
			$cmd="LdataWriter -dir $localDataPath -writer $prog -rpath $hold_ftp_filename -dtype netCDF -info2 $hold_ftp_filename -ltime $hold_data_time";
			if ($debug) {
			    Niwot::postDebug("Write _latest_data_info file, run cmd: $cmd");
			}
			
			system($cmd);

			Toolsa::PMU_auto_register("Writing latest data info files");
		    }
		
		    # update the counters

		    Niwot::postInfo("Got files: $#ftp_list+1, new inventory count: $#ftp_list+1+$icnt");

		    $ftped_files = $ftped_files + $#ftp_list+1;
		    undef @ftp_list;
		} else {
		    Niwot::postError("Did not update local file, $local_file");
		    undef @ftp_list;
		}
	    } else {
		Niwot::postInfo ("No new files available, current inventory count: $icnt");
	    } #endif defined(ftplist)
	
	    # Remove lock files

	    flock (FTPLOCK, 8); # Release lock
	    close FTPLOCK;

	    unlink($ftplock);

	} #endforeach (subscribedDirs)

	# write a latest_data_info file to trigger decoder if actually got any files
	# Determine the data time to use: current? or conglomerate the
	# filename (YYYYMMDD_HHMM) to a YYYYMMDDHHMMSS ldata time?
    
	if (($DoRealTime > 0) && ($ftped_files > 0)) {
	    ($is_ok, $hold_data_time)=&genLatestDataTime($data_time, $debug);
	    $cmd="LdataWriter -dir $params->{outputDir} -writer $prog -dtype netCDF -rpath $data_time -info2 $data_time -ltime $hold_data_time";
	    if ($debug) {
		Niwot::postDebug("Write _latest_data_info file to top dir, run cmd: $cmd");
	    }
	    system($cmd);

            Toolsa::PMU_auto_register("Writing latest data info file" );
	}

    } #endfor data_time

    # Remove any lock file that could have been left

    $cmd="find $NCEP_OBS_DATA -name \*.lock -exec rm \{\} \\;";
    system($cmd);

    # Get the end time for data retrieval

    $end_loop_utime=NNTUtils::getNowAsUtime();
    $elapsed_loop_utime=$end_loop_utime-$start_loop_utime;
    Niwot::postInfo("Elapsed time for data retrieval, $elapsed_loop_utime secs");

    # Exit if we are not in real time or run-once

    if (($run_once) || ($DoRealTime < 1)) {
	&doSignalExit;
    }

    # Sleep between checks for new data if this option was chosen
    # If no data found, sleep 1 minute so we go to the next minute

    if ($DoRealTime > 0) {
	if (($doTimeTrigger < 1) || ($ftped_files < 1)) {
	    Toolsa::PMU_auto_register("Sleeping between checks for new data...");
	    if ($debug) {
		Niwot::postDebug("Sleeping between checks for new data...");
	    }
	    if ($doTimeTrigger < 1) {
		sleep($params->{sleepCheck});
	    } else {
		sleep(60);
	    }
	}
    }

} #endwhile

&doSignalExit();

# ============================= SUBROUTINES =================================
#
# Subroutine: genDateString
#
# Usage:      ($return_val, $str) = genDateString($lag, $debug)
#
# Function:   Generate the date string to use for data retrieval
#
# Input:      $lag             lag hours offset
#             $dbg             debug flag 
#
# Output:     $return_val      1 on success, 0 on error
#             $str             date string 
# 
# Overview:
#

sub genDateString
{
  my ($lag, $dbg) = @_;

  # Local variables

  my($subname, $return_val, $str);
  my($sec, $min, $hour, $day, $mon, $year, $wday, $yday, $isdst);

  # Set defaults

  $subname="genDateString";
  $return_val=1;

  # Get the time. Force minutes to be zero so retrieve essentially
  # hourly files

  ($sec, $min, $hour, $day, $mon, $year, $wday, $yday, $isdst)=gmtime(time() - ($lag * 3600));
  $year=$year+1900;
  $mon=$mon+1;
  $min=0;

  $str = sprintf("%04d%02d%02d_%02d%02d", $year, $mon, $day, $hour, $min);
  
  return($return_val, $str);
}

#------------------------------------------------------------------------------------
# Subroutine: genLdataInfoFromFilename
#
# Usage:      ($return_val, $filestr) = genLdataInfoFromFilename($filename, $debug)
#
# Function:   Extract the filename from the full filename to use with DataMapper
#
# Input:      $filename        filename, as /dir/dir/dir/YYYYMMDD_HHMM.gz
#             $dbg             debug flag 
#
# Output:     $return_val      1 on success, 0 on error
#             $filestr         file string as YYYYMMDD_HHMM
# 
# Overview:
#

sub genLdataInfoFromFilename
{
  my ($filename, $dbg) = @_;

  # Local variables

  my($subname, $return_val);
  my($file, $ext, $ymd, $hms, $sec, $last_slash, $file_nodirs, $just_file);

  # Set defaults

  $subname="genLdataInfoFromFilename";
  $return_val=1;

  # Do the parsing

  $last_slash=rindex($filename, '/');
  $file_nodirs=substr($filename, $last_slash+1);

  ($just_file, $ext)=split(/\./, $file_nodirs);

  return($return_val, $just_file);
}


#------------------------------------------------------------------------------------
# Subroutine: genLatestDataTime
#
# Usage:      ($return_val, $timestr) = genLatestDataTime($in_timestr, $debug)
#
# Function:   Create a latest data time YYYYMMDDHHMMSS from the $in_timestr
#
# Input:      $in_timestr      YYYYMMDD_HHMM
#             $dbg             debug flag 
#
# Output:     $return_val      1 on success, 0 on error
#             $timestr         time string as YYYYMMDDHHMMSS
# 
# Overview:
#

sub genLatestDataTime
{
  my ($in_timestr, $dbg) = @_;

  # Local variables

  my($subname, $return_val);
  my($timestr);
  my($ymd, $hm, $sec);

  # Set defaults

  $subname="genLatestDataTime";
  $return_val=1;

  # Do the parsing
  # Expect YYYYMMDD_HHMM

  $ymd=substr($in_timestr, 0, 8);
  $hm=substr($in_timestr, 9, 4);
  $sec=0;

  $timestr=$ymd . $hm . "00";

  return($return_val, $timestr);
}

#---------------------------------------------------------------------------
# Subroutine printParamSettings
#
# Usage: printParamSettings
#
# Function: print parameter file settings
#
# Input:    none
#
# Output:   none
#
# Overview:
#

sub printParamSettings
{
 
  # Local variables

  my($subname);
  my($data);

  # Set defaults

  $subname="printParamSettings";

  # Print settings

  Niwot::postDebug( "Param file settings: $paramFile...");
  Niwot::postDebug( "\tinstance: $params->{instance}");
  Niwot::postDebug( "\toutputDir: $params->{outputDir}");
  foreach $data (@{$params->{subscribedDir}}) {
      Niwot::postDebug( "\tsubscribedDir: $data");
  }
  Niwot::postDebug( "\tftpHost: $params->{ftpHost}");
  Niwot::postDebug( "\tusername: $params->{username}");
  Niwot::postDebug( "\tpasswd: $params->{passwd}");
  Niwot::postDebug( "\tftpHostDir: $params->{ftpHostDir}");
  Niwot::postDebug( "\ttiming option, either checkTime or sleepCheck");
  if (defined($params->{checkTime})) {
      foreach $data (@{$params->{checkTime}}) {
	  Niwot::postDebug( "\t\tcheckTime: $data");
      }
  }
  if (defined($params->{sleepCheck})) {
      Niwot::postDebug( "\t\tsleepCheck: $params->{sleepCheck}");
  }

  # Done
}

#---------------------------------------------------------------------------
# Subroutine doSignalExit
#
# Usage: doSignalExit
#
# Function: Cleanup and exit
#
# Input:    none. All is handled by globals
#
# Output:   none
#
# Overview:
#

sub doSignalExit
{
 
  # Local variables

  my($subname);

  # Set defaults

  $subname="doSignalExit";

  # Unregister

  Toolsa::PMU_auto_unregister();

  # Done

  exit $ExitSuccess;

}

#---------------------------------------------------------------------------
# Subroutine printParams
#
# Usage: printParams
#
# Function: Print a basic param file to STDOUT
#
# Input:    none
#
# Output:   none
#
# Overview:
#

sub printParams
{
    my $today=`date -u`;
    print(STDOUT "\<\?xml version\=\"1.0\"\?\>\n");
    print(STDOUT "\<params\>\n");
    print(STDOUT "\<\!--\n");
    print(STDOUT "# This param file was generated on: $today");
    print(STDOUT "--\>\n");

    print(STDOUT "\<\!--\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "#                process instance                        #\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "--\>\n");
    print(STDOUT "\<instance\>test\<\/instance\>\n\n");

    print(STDOUT "\<\!--\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "#                NCEP_OBS - Output Directory             #\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "--\>\n");
    print(STDOUT "\<outputDir\>data\/ncep_obs-input\<\/outputDir\>\n\n");

    print(STDOUT "\<\!--\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "#                NCEP_OBS subscribed platforms           #\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "--\>\n");
    print(STDOUT "\<subscribedDir\>DC.sflnd/DS.metar\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sflnd/DS.sclim\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sflnd/DS.synop\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sflnd/DS.tafst\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sfmar/DS.dbuoy\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sfmar/DS.ships\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sfmar/DS.tideg\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sluan/DS.airep\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sluan/DS.airmet\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sluan/DS.amdar\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sluan/DS.pirep\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sluan/DS.recco\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.sluan/DS.sigmt\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.vsndn/DS.dropw\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.vsndn/DS.prflr\<\/subscribedDir\>\n");
    print(STDOUT "\<subscribedDir\>DC.vsndn/DS.raobf\<\/subscribedDir\>\n\n");
    print(STDOUT "\<subscribedDir\>DC.vsndn/DS.raobs\<\/subscribedDir\>\n\n");

    print(STDOUT "\<\!--\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "#             NCEP obs account username and password     #\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "--\>\n");
    print(STDOUT "\<ftpHost\>tgftp.nws.noaa.gov\<\/ftpHost\>\n");
    print(STDOUT "\<username\>anonymous\<\/username\>\n");
    print(STDOUT "\<passwd\>sheu\@ucar.edu\<\/passwd\>\n\n");

    print(STDOUT "\<\!--\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "#           NCEP obs top-level directory on ftp site     #\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "--\>\n");
    print(STDOUT "\<ftpHostDir\>\/SL.us008001\/DF.an\/\<\/ftpHostDir\>\n\n");

    print(STDOUT "\<\!--\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "#                Timing options for data polling         #\n");
    print(STDOUT "# Option A: specify a number of seconds to sleep between #\n");
    print(STDOUT "#           checks for data,                             #\n");
    print(STDOUT "#           e.g., <sleepCheck>300</sleepCheck>           #\n");
    print(STDOUT "# Option B: specify a list of times (minutes) during an  #\n");
    print(STDOUT "#           hour to check for data,                      #\n");
    print(STDOUT "#           e.g., <checkTime>10</checkTime>              #\n");
    print(STDOUT "#                 <checkTime>30</checkTime>              #\n");
    print(STDOUT "##########################################################\n");
    print(STDOUT "--\>\n");
    print(STDOUT "\<checkTime\>10\<\/checkTime\>\n");
    print(STDOUT "\<checkTime\>30\<\/checkTime\>\n");
    print(STDOUT "\<checkTime\>50\<\/checkTime\>\n");

    print(STDOUT "\<\/params\>\n");
}

#========================================= EOF =====================================

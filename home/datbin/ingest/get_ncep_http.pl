#! /usr/bin/perl -w
##---------------------------------------------------------------------------##
# CVS ID:     $Id: get_ncep_http.pl,v 1.1 2012/09/14 17:34:08 becky Exp $
# CVS SOURCE: $Source: /cvs/projects/saudi/rtf3h/pmeop/user/datbin/ingest/get_ncep_http.pl,v $
##---------------------------------------------------------------------------##
##                                                                           ##
##  Copyright UCAR (c) 2006-2007.                                            ##
##  University Corporation for Atmospheric Research (UCAR),                  ##
##  National Center for Atmospheric Research (NCAR),                         ##
##  Research Applications Laboratory (RAL),                                  ##
##  P.O. Box 3000, Boulder, Colorado, 80307-3000, USA.                       ##
##                                                                           ##
##---------------------------------------------------------------------------##
#==============================================================================
#
# SCRIPT:  get_ncep_http.pl
# Purpose: To retrieve NCEP model data sets from the National Weather
#          Service (NWS) or from the National Centers for Environment
#          Protection (NCEP).
#
# Notes:   Uses the XML configuration file
#          /home/<USER>/config/ncepIngestConfig.xml to get configuration
#          information for downloading each model type.
#
# Author:  Shane Swartz with assistance from Carl Drews' scripts for ingesting
#          GFS_3, GFS_4 and NAM_212 data sets
# Date:    October 30, 2006
#
# Change History:
#==============================================================================
# 20061030 : Shane Swartz : Created
#------------------------------------------------------------------------------
# 20070228 : Shane Swartz : Modified to add ability to download from the NCEP
#                           web site in addition to the NWS web site and added
#                           capability to specify an ingest date and time 
#                           rather than use current date and time.
#------------------------------------------------------------------------------
# 20061030 : Shane Swartz : Added version information and made it available
#                           via the -v or -version options.
#------------------------------------------------------------------------------
# 20070402 : Shane Swartz : Modified to use the PERL module IngestUtils.pm
#                           and fixed a bug for where to search for already
#                           downloaded copies of files for when input files
#                           are GRIB1 not GRIB2.
#------------------------------------------------------------------------------
#==============================================================================
#
# Set main package
#
package main;
#
# Get the needed PERL modules
#
   use strict;
   use LWP::Simple;
   use IO::File;
   use IO::Handle;
   use POSIX;
   use Time::Local;
   use File::Basename;
   use File::Temp qw(:mktemp);
   use Data::Dumper;
   use XML::Simple 2.14 qw(:strict);
   use Getopt::Long qw(:config no_ignore_case bundling_override);
   use lib "/home/pmeop/datbin/ingest";
   use IngestUtils qw(:DEFAULT :signalHandler decodeXMLEntry moveFile
                      putDirectory setCycle getFileViaWeb);
   use sigtrap qw(handler localSignalHandler INT TSTP STOP TERM QUIT ABRT);
#
# Global subroutines and variables used from the IngestUtils module
#=====================================================
# (From the :DEFAULT export tag)
#  &help
#  &putLogEntry
#  &runSystemCommand
#  &isInt
#  &isFloat
#  &isValidFileName
#  &isValidDirName
#  &showVersion
#  $SCRIPT
#  $USAGE
#  $DEBUG
#  $SUCCESS_EXIT_VALUE
#  $ERROR_EXIT_VALUE
#  %LOG_TYPES
#  $SCRIPT_VERSION
#  $SCRIPT_VERSION_DATE
#  $SCRIPT_CREATED_YEAR
#  $SCRIPT_LAST_MOD_YEAR
#-----------------------------------
# (From the :signalHandler export tag)
#  &localSignalHandler
#  $scriptCanBeStopped
#  $ctrlcAlready
#  $ctrlzAlready
#
# (From manual request)
#  &decodeXMLEntry
#  &moveFile
#  &putDirectory
#  &setCycle
#  &getFileViaWeb
#
#=====================================================
#
# Declare variables global to this package
#
   my $WGRIB_CMD;
   my $RM_CMD;
   my $MV_CMD;
   my $ERROR_EXIT_VALUE;
   my $SUCCESS_EXIT_VALUE;
   my $CONTINUE_DOWNLOAD_MSG;
   my $modelToUse;
   my $ingestDateTime;
   my $urlSiteAbbreviation;
   my @VALID_GRIB_TYPES;
   my @VALID_URL_PROTOCOLS;
   my $whoami;
#
# From XML Configuration file
#
   my $cycleIntervalTime;    # Measured in hours
   my $downloadOffsetTime;   # Measured in hours
   my $gribType;
   my $dataBeginHour;
   my $dataEndHour;
   my $dataHourInterval;
   my $localDirBasename;
   my $remoteFileNamePrefix;
   my $remoteFileNameMiddle;
   my $remoteFileNameSuffix;
   my $URLSite;
   my $URLProtocol;
   my $URLDirectoryPrefix;
   my $URLDirectoryMiddle;
   my $URLDirectorySuffix;
   my @validCycles;
#
# Declare local variables
#
   my $hour;
   my $mday;
   my $mon;
   my $year;
   my @hours;
   my $LOCAL_DIR;
   my $remoteDir;
   my $startTime;
   my $endTime;
   my $logMessage;
   my $LOCAL_DIR_OWNER_NAME;
   my $LOCAL_DIR_GROUP_NAME;
   my $cycle;
   my $cycleYearMonthDay;
   my $elapsedTime;
   my $remoteFileNamePrefixRegExp;
   my $remoteFileNameMiddleRegExp;
   my $remoteFileNameSuffixRegExp;
   my $remoteFileNameRegExp;
   my @availableRemoteFiles;
   my $numRemoteFilesAvailable;
   my $numRemoteFilesToDownload;
   my $doDownload;
   my $numRemoteFilesDownloaded;
   my $fileHour;
   my $remoteFile;
   my $maxLengthHourString;
   my $XMLConfigFile;
   my $refConfigInfo;
   my @gribFilesDownloaded;
   my @gribFilesConverted;
   my @gribFilesToMove;
   my $numGribFilesToMove;
   my $numFilesMoved; 
   my $downLoadDir;
   my $gribFile;
   my $returnValue;
   my $processedDir;
   my $numFilesConverted;
   my $gribFileBaseName;
   my @VALID_URL_SITES;
   my %DAYS_IN_MONTH;
   my $currentTime;
   my $validCycle;
   my $MIN_VALID_CYCLE;
   my $MAX_VALID_CYCLE;
#
# Declare subroutines
#
   sub getDataFiles;
   sub getInputs;
   sub help;
   sub getAvailableFiles;
   sub setHours;
   sub readXML;
   sub parseXML;
   sub verifyXMLValues;
#
# Set the environment variable "FTP_PASSIVE", which is used way down under
# the hood in Net::FTP.  If FTP_PASSIVE is set to non-zero then the passive
# mode is used, otherwise, the active mode is used.
# NOTE:  This is only needed if using the transfer mode <FTP> inside
# of the LWP getstore.
#
#   $ENV{'FTP_PASSIVE'} = 1;
#
# Set auto flushing of standard out and standard error
#
   STDOUT->autoflush(1);
   STDERR->autoflush(1);
#
# Initialize variables
#
# Version and version date from CVS
#
   $SCRIPT_VERSION      = q$Revision: 1.1 $;
   $SCRIPT_VERSION      =~ s/Revision: (\d+\.\d+)/$1/;
   $SCRIPT_VERSION      =~ s/\s+$//;

   $SCRIPT_VERSION_DATE = q$Date: 2012/09/14 17:34:08 $;
   $SCRIPT_VERSION_DATE =~ s/Date: (\d{4}\/\d{2}\/\d{2} \d{2}:\d{2}:\d{2})/$1/;
   $SCRIPT_VERSION_DATE =~ s/\s+$//;

   $SCRIPT_CREATED_YEAR  = '2006';
   $SCRIPT_VERSION_DATE  =~ /(\d{4})\/.*/;
   if ( defined($1) && length($1) > 0 ) {
      $SCRIPT_LAST_MOD_YEAR = $1;
   }
   else {
      if ( defined($SCRIPT_CREATED_YEAR) &&
           length($SCRIPT_CREATED_YEAR) > 0 ) {
         $SCRIPT_LAST_MOD_YEAR = $SCRIPT_CREATED_YEAR;
      }
      else {
         $SCRIPT_LAST_MOD_YEAR = (localtime(gmtime()))[5] + 1900;        
      }
   }
#
# Code variables
#
   $whoami = $ENV{'USER'}; 
   $startTime = time();
   $SCRIPT = basename($0);
   $DEBUG  = 0;
   $USAGE  = "$SCRIPT\n\t-h|help (Help)\n".
	     "\t-d|datetime [YYYYMMDDHH] (GMT Date and time ".
	     "\n\t\tto use instead of current date and time ".
	     "\n\t\tfor calculating data to download.  Minutes ".
	     "\n\t\tand seconds are set to zero.)\n".
             "\t-DEBUG|debug (Print debug information)\n".
	     "\t-m|model [Model Name to Use]\n".
	     "\t-v|version (Version information)";
#
   if ( defined($ENV{'HOME'}) ) {
      $XMLConfigFile = "$ENV{'HOME'}/datbin/ingest/ncepIngestConfig.xml";
   }
   else {
      $XMLConfigFile = "/home/${whoami}/datbin/ingest/ncepIngestConfig.xml";
   }

   $WGRIB_CMD     = "/opt/degrib/bin/degrib";
   $RM_CMD = '/bin/rm';
   $MV_CMD = '/bin/mv';

   @VALID_GRIB_TYPES    = ( 'grib1', 'grib2' );
   @VALID_URL_PROTOCOLS = ( 'http', 'https', 'ftp' );

   $CONTINUE_DOWNLOAD_MSG    = 'CONTINUE_DOWNLOAD';
   $doDownload               = 0;
   $numRemoteFilesDownloaded = 0;

   %DAYS_IN_MONTH = ( '01' => '31', '02' => '29', '03' => '31', '04' => '30',
                      '05' => '31', '06' => '30', '07' => '31', '08' => '31',
		      '09' => '30', '10' => '31', '11' => '30', '12' => '31');

   $LOCAL_DIR_OWNER_NAME   = $whoami;
   $LOCAL_DIR_GROUP_NAME   = $whoami;
   $SUCCESS_EXIT_VALUE     = 0;
   $ERROR_EXIT_VALUE       = 1;

   @VALID_URL_SITES = ('weather.noaa.gov', 'www.ftp.ncep.noaa.gov', 'tgftp.nws.noaa.gov');
#
   $MIN_VALID_CYCLE = 0;
   $MAX_VALID_CYCLE = 23;
#
# Variables for use with the localSignalHandler subroutine
#
   $scriptCanBeStopped = 0;     # Default value to not allow script to
                                # be stopped until the download is complete
				# and the last log entry has been written
   $ctrlcAlready       = 0;
   $ctrlzAlready       = 0;
#
# Get command line inputs
#
   getInputs;
#
# Read and parse the XML file
#
   $refConfigInfo = readXML($XMLConfigFile);
   parseXML($modelToUse, $refConfigInfo); 
#
# Output message stating that request to initiate data pull was received
#
   $logMessage = "Received request to initiate $URLProtocol pull for ".
                 "model <$modelToUse> from URL <$URLSite>";
   putLogEntry($LOG_TYPES{'info'}, \$logMessage);
#
# Determine that a valid URL Site was specified
#
   if ( scalar(grep(/^$URLSite$/, @VALID_URL_SITES)) != 1 ) {
      $logMessage = "Unknown URL site <$URLSite> - Not prepared to ".
                    "ingest data from there";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Verify that the array holding the validCycles has valid values
#
   foreach $validCycle (@validCycles) {
      if ( $validCycle !~ /^\d{2}$/ ||
           int("$validCycle") < $MIN_VALID_CYCLE ||
	   int("$validCycle") > $MAX_VALID_CYCLE ) {
         $logMessage = "Invalid value <$validCycle> for a validCycle ".
	               "in the XML configuration";
         putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
      }
   }
#
# Initialize variables that are dependent upon information
# in the XML configuration file
#
   $remoteFileNamePrefixRegExp = "$remoteFileNamePrefix";
   $remoteFileNamePrefixRegExp =~ s/\./\\\./g;
   $remoteFileNameMiddleRegExp = "$remoteFileNameMiddle";
   $remoteFileNameMiddleRegExp =~ s/\./\\\./g;
   $remoteFileNameSuffixRegExp = "$remoteFileNameSuffix";
   $remoteFileNameSuffixRegExp =~ s/\./\\\./g;

   if ( "$URLSite" eq "weather.noaa.gov" || "$URLSite" eq "tgftp.nws.noaa.gov" ) {
      $urlSiteAbbreviation = 'nws'; 
      $maxLengthHourString = 4;
   }
   elsif ( "$URLSite" eq "www.ftp.ncep.noaa.gov" ) {
      $urlSiteAbbreviation = 'ncep'; 
      $maxLengthHourString = 2;
   }
#
   if ( defined($ENV{'RAW_DATA_DIR'}) ) {
      $LOCAL_DIR = "$ENV{'RAW_DATA_DIR'}/$localDirBasename";
   }
   else {
      $LOCAL_DIR = "/d1/pmeop/datainput/$localDirBasename";
   }
#
# Initialize the hours array
#
   @hours = setHours($dataBeginHour, $dataEndHour, $dataHourInterval,
                     $maxLengthHourString, $urlSiteAbbreviation, $modelToUse);
#
# Get current date and time information using GMT
#
   $currentTime = time;
   if ( ! defined($ingestDateTime) ) {
      ($hour,$mday,$mon,$year) = (gmtime($currentTime))[2,3,4,5];
#
# Adjust values for the current date and time
#
      $mon  += 1;
      $year += 1900;
      $mon  = '0' . $mon  if ( $mon < 10 );
      $mday = '0' . $mday if ( $mday < 10 );
   }
   else {
      $ingestDateTime =~ /(\d{4})(\d{2})(\d{2})(\d{2})/;
#
# Year
#
      if ( defined($1) && int("$1") > 1900 ) {
         $year = $1;
      }
      else {
         $logMessage = "The specified ingest date and time <$ingestDateTime> ".
	               "did not provide a valid value for the year";
         putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
      }
#
# Month
#
      if ( defined($2) && int("$2") >= 1 && int("$2") <= 12 ) { 
            $mon = $2;
      }
      else {
         $logMessage = "The specified ingest date and time <$ingestDateTime> ".
	               "did not provide a valid value for the month";
         putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
      }
#
# Day in the month
#
      if ( defined($3) && int("$3") > 0 &&
           int("$3") <= int($DAYS_IN_MONTH{$mon})) {
         $mday = $3;
      }
      else {
         $logMessage = "The specified ingest date and time <$ingestDateTime> ".
	               "did not provide a valid value for the day in the month";
         putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
      }
#
# Hour
#
      if ( defined($4) && int("$4") >= 0 && int("$4") <= 23 ) {
         $hour = $4;
      }
      else {
         $logMessage = "The specified ingest date and time <$ingestDateTime> ".
	               "did not provide a valid value for the hour";
         putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
      }
   }
#
# Determine the cycle time based on the requested hour
# Cycle is forecast start time to access
#
   ($cycle, $cycleYearMonthDay) = setCycle(int("$year"), int("$mon"),
                                           int("$mday"), int("$hour"),
					   $downloadOffsetTime,
                                           $cycleIntervalTime,
					   $validCycles[0],
					   $currentTime, $ingestDateTime); 
   $cycle = '0' . $cycle if ( $cycle < 10 );
   if ( scalar(grep(/$cycle/, @validCycles)) == 1 ) {
      $logMessage = "Cycle <$cycle> and cycle date <$cycleYearMonthDay> ".
                    "will be used for the GMT date and time ".
		    "<${year}${mon}${mday}${hour}>";
      putLogEntry($LOG_TYPES{'info'}, \$logMessage);
   }
   else {
      $logMessage = "Invalid cycle time <$cycle> was calculated"; 
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Verify that the needed local directories exist.  If not, create them.
#
   putDirectory ($LOCAL_DIR, $LOCAL_DIR_OWNER_NAME, $LOCAL_DIR_GROUP_NAME);
   
   # $downLoadDir = "$LOCAL_DIR/${cycleYearMonthDay}";
   $downLoadDir = "$LOCAL_DIR";
   putDirectory ($downLoadDir, $LOCAL_DIR_OWNER_NAME,
                 $LOCAL_DIR_GROUP_NAME);
#
# Set the remote directory name for the HTTP search
# and the regular expression for the file names to
# attempt to download
#
   if ( "$urlSiteAbbreviation" eq 'nws' ) {
      $remoteDir = "${URLDirectoryPrefix}" . "${cycle}" . '/' . 
                   "$URLDirectoryMiddle" . "${cycleYearMonthDay}" .
                   '/' . "$URLDirectorySuffix";
      $remoteFileNameRegExp = "${remoteFileNamePrefixRegExp}\\d{4}".
                              "${remoteFileNameSuffixRegExp}";
   }
   elsif ( "$urlSiteAbbreviation" eq 'ncep' ) {
      $remoteDir = "${URLDirectoryPrefix}" . "${cycleYearMonthDay}";
      if ( $modelToUse =~ /^gfs[A34]$/ ) {
         $remoteDir = "${remoteDir}" . "${cycle}";
      }
      $remoteFileNameRegExp = "${remoteFileNamePrefixRegExp}${cycle}".
                              "${remoteFileNameMiddle}\\d{2,3}".
                              "${remoteFileNameSuffixRegExp}";
   }

   if ( $DEBUG ) {
      $logMessage = "remoteFileNamePrefixRegExp <$remoteFileNamePrefixRegExp>";
      putLogEntry($LOG_TYPES{'debug'}, \$logMessage);
      $logMessage = "remoteFileNameMiddleRegExp <$remoteFileNameMiddleRegExp>";
      putLogEntry($LOG_TYPES{'debug'}, \$logMessage);
      $logMessage = "remoteFileNameSuffixRegExp <$remoteFileNameSuffixRegExp>";
      putLogEntry($LOG_TYPES{'debug'}, \$logMessage);
      $logMessage = "remoteFileNameRegExp <$remoteFileNameRegExp>";
      putLogEntry($LOG_TYPES{'debug'}, \$logMessage);
   }
#
# Attempt to retrieve the new data
#
   $logMessage = "Attempting to retrieve data from the ".
                 "URL <$URLProtocol://$URLSite/$remoteDir>";
   putLogEntry($LOG_TYPES{'info'}, \$logMessage);
#
# Find the files available to download
#
   @availableRemoteFiles = getAvailableFiles("$URLSite/$remoteDir",
                                             "$LOCAL_DIR",
                                             $remoteFileNameRegExp);
 
   $numRemoteFilesAvailable  = scalar(@availableRemoteFiles);
   $numRemoteFilesToDownload = scalar(@hours);

   if ( $numRemoteFilesAvailable == 1 &&
        $availableRemoteFiles[0] eq "$CONTINUE_DOWNLOAD_MSG" ) {
      undef @availableRemoteFiles;
#
      if ( "$urlSiteAbbreviation" eq 'nws' ) {
         foreach $fileHour (@hours) {
            $remoteFile = "$remoteFileNamePrefix" . "$fileHour" .
   	                  "$remoteFileNameSuffix";
            push(@availableRemoteFiles, $remoteFile); 
         }
      }
      elsif ( "$urlSiteAbbreviation" eq 'NCEP' ) {
         foreach $fileHour (@hours) {
            $remoteFile = "$remoteFileNamePrefix" . "$cycle" . 
                          "$remoteFileNameMiddle" . "$fileHour" .
			  "$remoteFileNameSuffix";
            push(@availableRemoteFiles, $remoteFile); 
         }
      }
#
      $numRemoteFilesAvailable = 'Number Unknown';
      $doDownload = 1;
   }
   elsif ( $numRemoteFilesAvailable > 0 ) {
      $doDownload = 1;
   }
   else {
      $logMessage = "No files available to download";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
   }
#
# Download available files or attempt to download all files
#
   if ( $doDownload ) {
      $logMessage = "<$numRemoteFilesAvailable> files available to ".
                    "download out of <$numRemoteFilesToDownload> ".
		    "requested files";
      putLogEntry($LOG_TYPES{'info'}, \$logMessage);
      $logMessage = "Files will be downloaded to the local directory ".
                    "<$downLoadDir>";
      putLogEntry($LOG_TYPES{'info'}, \$logMessage);
      @gribFilesDownloaded = getDataFiles($urlSiteAbbreviation,
                                          $URLSite, $remoteDir,
                                          $cycle, $cycleYearMonthDay,
 					  $LOCAL_DIR,
					  $downLoadDir,
					  $remoteFileNamePrefix,
					  $remoteFileNameMiddle,
					  $remoteFileNameSuffix,
					  \@hours, \@availableRemoteFiles);
      $numRemoteFilesDownloaded = scalar(@gribFilesDownloaded);
      $logMessage = "Downloaded <$numRemoteFilesDownloaded> of ".
                    "<$numRemoteFilesToDownload> requested files";
      putLogEntry($LOG_TYPES{'info'}, \$logMessage);
   }
#
# Print how long that took
#
   $endTime     = time();
   $elapsedTime = $endTime - $startTime;
   $logMessage  = "$URLProtocol pull completed - ".
                  "Elapsed seconds <$elapsedTime>";
   putLogEntry($LOG_TYPES{'info'}, \$logMessage);
#
# Change the value for the variable "scriptCanBeStopped" to allow the
# script to be stopped
#
   $scriptCanBeStopped = 1;
#
# End of main program.  Exit with a successful exit value for UNIX/Linux
# shell interpretation.
#
   exit ($SUCCESS_EXIT_VALUE);
#
#=============================================================================#
#                                                                             #
########################## S U B R O U T I N E S ##############################
#                                                                             #
#=============================================================================#
#
# sub getDataFiles - Attempts to retrieve all of the remote files from the 
#                    remote site.
#
sub getDataFiles {
#
# Global variables used
#==========================
#
# %LOG_TYPES
# $WGRIB_CMD
# $RM_CMD
# $URLProtocol
# $DEBUG
#
#==========================
#
# Subroutines called
#==========================
#
#  putLogEntry
#  "is_success" from HTTP::Status, which is called by LWP::Simple 
#  "is_error" from HTTP::Status, which is called by LWP::Simple 
#  "status_message" from HTTP::Status, which is called by LWP::Simple
#  "head" from LWP::Simple
#
#==========================
#
# Declare local variables
#
   my $NUM_REQUIRED_ARGS;
   my $remoteSiteAbbreviation;
   my $remoteSite;
   my $remoteDir;
   my $refFileHours;
   my @fileHours;
   my $hour;
   my $remoteFile;
   my $localFile;
   my $cycleYearMonthDay;
   my $MAX_DOWNLOAD_ATTEMPTS;
   my $localFileModTime;
   my $remoteFileModTime;
   my $returnCode;
   my $localDir;
   my $downLoadDir;
   my $numDownLoadAttempts;
   my $downLoadCompleted;
   my $commandToRun;
   my $remoteURL;
   my $remoteFileNamePrefix;
   my $remoteFileNameMiddle;
   my $remoteFileNameSuffix;
   my $refAvailableRemoteFiles;
   my @availableRemoteFiles;
   my $cycle;
   my @gribFilesDownloaded;
   my $localGribFile;
#
# Initialize variables
#
   $NUM_REQUIRED_ARGS = 12;
   if ( scalar(@_) == $NUM_REQUIRED_ARGS ) {
      ($remoteSiteAbbreviation, $remoteSite, $remoteDir, $cycle,
       $cycleYearMonthDay, $localDir, $downLoadDir, $remoteFileNamePrefix,
       $remoteFileNameMiddle, $remoteFileNameSuffix, $refFileHours,
       $refAvailableRemoteFiles) = @_;
      $remoteURL = "$remoteSite/$remoteDir";
   }
   else {
      $logMessage = "Invalid number of arguments passed to subroutine ".
                    "<getDataFiles>";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
   $MAX_DOWNLOAD_ATTEMPTS = 5;
   undef (@gribFilesDownloaded);
#
# Verify that the variable for the refFileHours is actually a 
# reference and contains an array of values
#
   if ( ref($refFileHours) && ref($refFileHours) eq 'ARRAY' ) {
      @fileHours = @$refFileHours;
   }
   else {
      $logMessage = "Invalid argument for the variable refFileHours ".
                    "passed to the subroutine <getDataFiles>";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Verify that the variable for the refAvailableRemoteFiles is actually a 
# reference and contains an array of values
#
   if ( ref($refAvailableRemoteFiles) &&
        ref($refAvailableRemoteFiles) eq 'ARRAY' ) {
      @availableRemoteFiles = @$refAvailableRemoteFiles;
   }
   else {
      $logMessage = "Invalid argument for the variable ".
                    "refAvailableRemoteFiles passed to the ".
		    "subroutine <getDataFiles>";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Download the files
#
   foreach $hour (@fileHours) {
      if ( "$remoteSiteAbbreviation" eq 'nws' ) {
         $remoteFile = "$remoteFileNamePrefix" . "$hour" .
                       "$remoteFileNameSuffix";
      }
      elsif ( "$remoteSiteAbbreviation" eq 'ncep' ) {
         $remoteFile = "$remoteFileNamePrefix" . "$cycle" . 
                       "$remoteFileNameMiddle" . "$hour" .
                       "$remoteFileNameSuffix";
      }
#
# Verify that the remote file is available to download
#
      if ( scalar(grep(/$remoteFile/, @availableRemoteFiles)) == 0 ) {
         $logMessage = "Remote file <$remoteFile> not available to download";
	 putLogEntry($LOG_TYPES{'info'}, \$logMessage);
	 next;
      }
#
# Verify that the local download directory still exists
#
      if ( -d $downLoadDir ) {
         if ( "$gribType" eq 'grib1' ) {
           $localFile = "$downLoadDir/${cycleYearMonthDay}${cycle}" .
                        '_' . "$remoteFile" ;
           $localGribFile = "$localDir/${cycleYearMonthDay}${cycle}" .
                             '_' . "$remoteFile" ;
         } else {
           $localFile = "$downLoadDir/${cycleYearMonthDay}${cycle}" .
                        '_' . "$remoteFile" . ".$gribType";
           $localGribFile = "$localDir/${cycleYearMonthDay}${cycle}" .
                             '_' . "$remoteFile" . ".$gribType";
         }
      }
      else {
         $logMessage = "Local directory <$downLoadDir> for storing ".
                       "downloaded files no longer exists";
         putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
      }
#
# Print statement about attempting to download the file
#
      $logMessage = "Attempting to download file <$remoteFile>";
      putLogEntry($LOG_TYPES{'info'}, \$logMessage);
#
# Verify that the remote file exists by attempting to get the header
# information for the file
#
      $downLoadCompleted   = 0;
      $numDownLoadAttempts = 0;
      while ( ! $downLoadCompleted &&
         $numDownLoadAttempts < $MAX_DOWNLOAD_ATTEMPTS ) {
         undef $remoteFileModTime;
         $remoteFileModTime = ( head("$URLProtocol://$remoteURL/".
                                     "$remoteFile") )[2];
         $numDownLoadAttempts++;				      
         if ( defined($remoteFileModTime) ) {
	    $downLoadCompleted = 1;
            if ( $DEBUG ) {
               $logMessage = "Remote file <$remoteFile> has modification ".
                             "time <$remoteFileModTime>";
               putLogEntry($LOG_TYPES{'debug'}, \$logMessage);
            }
         }
	 else {
            $logMessage = "Download attempt number ".
                          "<$numDownLoadAttempts> ".
                          "failed for retrieving the header information ".
                          "for file <$remoteFile>";
            putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
         }
      }
      if ( ! $downLoadCompleted &&
           $numDownLoadAttempts >= $MAX_DOWNLOAD_ATTEMPTS ) {
         $logMessage = "Failed to get header information for file ".
                       "<$remoteFile> - Assuming that the file ".
                       "does not exist on the remote URL ".
                       "<$URLProtocol://$remoteURL> or the site is ".
                       "not currently accessible";
         putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
         next;
      }
#
# Get the modification time for the local file if it exists already
# and its GRIB file is not corrupt.  If it is corrupt then set the modification
# time to zero.  If the local file does not exist but the GRIB file exists
# then verify that the GRIB is not corrupt.  If it is not corrupt then do 
# not download the remote file.
#
      undef $localFileModTime;
      if ( -e $localFile ) {
         $localFileModTime = (stat($localFile))[9];
         if ( -e $localGribFile ) {
            if ( -e $WGRIB_CMD && -x $WGRIB_CMD ) {
	       $commandToRun = "$WGRIB_CMD -I -in $localGribFile";
	       if ( ! runSystemCommand(\$commandToRun) ) {
	          $logMessage = "Local GRIB file <$localGribFile> is possibly ".
                                "corrupt - Will download the remote file";
                  putLogEntry($LOG_TYPES{'info'}, \$logMessage);
                  $localFileModTime = 0;
               }
            }
            else {
	       $logMessage = "Program <$WGRIB_CMD> does not exist or ".
                             "is not executable to check status of file ".
                             "<$localGribFile>";
               putLogEntry($LOG_TYPES{'error'}, \$logMessage,
                           $ERROR_EXIT_VALUE);
            } 
         }
         else {
            $localFileModTime = 0;
         } 
      }
      elsif ( -e $localGribFile ) {
         if ( -e $WGRIB_CMD && -x $WGRIB_CMD ) {
            $commandToRun = "$WGRIB_CMD -I -in $localGribFile";
            if ( runSystemCommand(\$commandToRun) ) {
               $logMessage = "Local GRIB1 file <$localGribFile> exists and ".
	                     "is not corrupt - Will not download the remote file";
               putLogEntry($LOG_TYPES{'info'}, \$logMessage);
               $localFileModTime = timegm(gmtime(time));
	    }
	    else {
               $logMessage = "Local GRIB1 file <$localGribFile> is possibly ".
                             "corrupt - Will download the remote file";
               putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
               $localFileModTime = 0;
            }
         }
         else {
            $logMessage = "Program <$WGRIB_CMD> does not exist or ".
                          "is not executable to check status of file ".
                          "<$localGribFile>";
            putLogEntry($LOG_TYPES{'error'}, \$logMessage,
                        $ERROR_EXIT_VALUE);
         } 
      }
      else {
         $localFileModTime = 0;
      }
      if ( $DEBUG ) {
         if ( $localFileModTime == 0 ) {
            if ( ! -e  $localFile ) {
	       if ( ! -e $localGribFile ) {
                  $logMessage = "Local file <$localFile> and local GRIB1 ".
		                "file <$localGribFile> do not exist"
	       }
	       else {
                  $logMessage = "Local file <$localFile> does not exist ".
	                        "and local GRIB1 file <$localGribFile> ".
		                "exists but is corrupt"
	       }
            }
            elsif ( ! -e  $localGribFile ) {
               $logMessage = "Local GRIB1 file <$localGribFile> does not exist";
            }
            else {
               $logMessage = "Local GRIB1 file <$localGribFile> is corrupt";
            } 
         }
         else {
            $logMessage = "Local file <$localFile> has modification ".
                          "time <$localFileModTime>";
         } 
         putLogEntry($LOG_TYPES{'debug'}, \$logMessage);
      }
#
# Download the file 
#
      if ( $remoteFileModTime > $localFileModTime ) {
         $downLoadCompleted   = 0;
	 $numDownLoadAttempts = 0;
	 while ( ! $downLoadCompleted &&
	         $numDownLoadAttempts < $MAX_DOWNLOAD_ATTEMPTS ) {
            undef $returnCode; 
            $returnCode = getFileViaWeb("$URLProtocol://$remoteURL/$remoteFile",
	                                "$localFile");
            $numDownLoadAttempts++;
            if ( $DEBUG ) {
               $logMessage = "ReturnCode <$returnCode> for retrieving remote ".
                             "file <$remoteFile>";
               putLogEntry($LOG_TYPES{'debug'}, \$logMessage);
            }
            if ( $returnCode == 1 ) {
               if ( "$gribType" eq 'GRIB1' ) {
                  if ( -e $WGRIB_CMD && -x $WGRIB_CMD ) {
	          $commandToRun = "$WGRIB_CMD -I -in $localFile";
	             if ( runSystemCommand(\$commandToRun) ) {
	                $logMessage = "Successfully downloaded file ".
                                      "<$remoteFile>";
                        putLogEntry($LOG_TYPES{'info'}, \$logMessage);
                        push(@gribFilesDownloaded, $localFile);
	                $downLoadCompleted = 1;
                     }
	             else {
	                $logMessage = "Download attempt number ".
		                      "<$numDownLoadAttempts> ".
		                      "failed for file <$remoteFile> ".
		   	   	      "due to the downloaded file being corrupt";
                        putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
		        $commandToRun = "$RM_CMD -f $localFile";
		        if ( ! runSystemCommand(\$commandToRun) ) {
	                   $logMessage = "Failed to remove corrupt local file ".
                                         "<$localFile>";
                           putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
		        }
	             }
                  }
                  else {
	             $logMessage = "Program <$WGRIB_CMD> does not exist or ".
                                   "is not executable to check status of file ".
                                   "<$localGribFile>";
                     putLogEntry($LOG_TYPES{'error'}, \$logMessage,
                                 $ERROR_EXIT_VALUE);
                  }
               }
               else {
	          $logMessage = "Successfully downloaded file <$remoteFile>";
                  putLogEntry($LOG_TYPES{'info'}, \$logMessage);
                  push(@gribFilesDownloaded, $localFile);
	          $downLoadCompleted = 1;
               }
	    }
	    elsif ( $returnCode == 0 ) {
	       $logMessage = "Download attempt number <$numDownLoadAttempts> ".
	                     "failed for file <$remoteFile>";
               putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
	    }
         }
	 if ( ! $downLoadCompleted &&
	      $numDownLoadAttempts >= $MAX_DOWNLOAD_ATTEMPTS ) {
	    $logMessage = "Failed to downloaded file <$remoteFile>";
            putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
         }
      }
      else {
         $logMessage = "Remote file <$remoteFile> has not been ".
	               "updated since last download - Will not attempt ".
		       "to download it";
         putLogEntry($LOG_TYPES{'info'}, \$logMessage);
      }
   }
#
# Return the number of files downloaded
#
   return (@gribFilesDownloaded);
}
#
# Subroutine getInputs - Checks for command line inputs
#
sub getInputs {
#
# Global variables used
#---------------------------------------
#
#  $DEBUG
#  %LOG_TYPES
#  $modelToUse
#  $ERROR_EXIT_VALUE
#
#---------------------------------------
#
# Local variables
#
   my $numOptions;
   my $numArgs;
   my $arg;
   my $MIN_NUM_OPTIONS;
   my $MAX_NUM_OPTIONS;
   my $logMessage;
#
# Initialize variables
#
   $numOptions      = 0;
   $MIN_NUM_OPTIONS = 1;
   $MAX_NUM_OPTIONS = 3;
#
# Check the command line options
#
   foreach $arg ( @ARGV ) {
      $numOptions++ if ( substr($arg, 0, 1) eq '-' );
   }
#
# Verify correct number of concurrent options
#
   if ( $numOptions > $MAX_NUM_OPTIONS ||
        $numOptions < $MIN_NUM_OPTIONS ) {
	$logMessage = "Incorrect number of command line options.  Minimum ".
	              "number of options allowed <$MIN_NUM_OPTIONS> and ".
		      "Maximum number of options allowed <$MAX_NUM_OPTIONS>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      help;
   }
#
# Use the GetOptions method which allows specific type of requirements
# for the command line arguments
#
   if (! &GetOptions("h|help|?"     => \&help,
		     "d|datetime=i" => \$ingestDateTime,
                     "DEBUG|debug"  => \$DEBUG,
                     "m|model=s"    => \$modelToUse,
		     "v|version"    => \&showVersion) ) {
      $logMessage = "Invalid option or argument specified";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, 0);
   }
#
# Verify that a model was requested on the command line
#
   if ( ! defined($modelToUse) ) {
      $logMessage = "Model name to use not entered";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# If a download date and time was specified, verify, as best possible,
# that it is valid 
#
   if ( defined($ingestDateTime) ) {
      if ( $ingestDateTime !~ /^\d{10}$/ ) {
         $logMessage = "The specified ingest date and time <$ingestDateTime> ".
	               "is not the correct size or format.  Must be an ".
		       "integer ten characters in length [YYYYMMDDHH]";
         putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
      }
   }
}
#
# Subroutine getAvailableFiles - Parse an index.html file that gives the
#                                list of available files at the given URL
sub getAvailableFiles {
#
# Global variables used
#==============================
#
# %LOG_TYPES
# $ERROR_EXIT_VALUE
# $CONTINUE_DOWNLOAD_MSG
#
#==============================

my $remoteURL;
my $fhIndexHtmlFile;
my $NUM_REQUIRED_ARGS;
my $readLine;
my $remoteFileNameRegExp;
my $fileFound;
my @filesFound;
my $logMessage;
my $returnCode;
my $localDir;
my $commandToRun;
my $localIndexHtmlFile;
#
# Initialize variables
#
   $NUM_REQUIRED_ARGS = 3;
   if ( scalar(@_) == $NUM_REQUIRED_ARGS ) {
      ($remoteURL, $localDir, $remoteFileNameRegExp) = @_;
   }
   else {
      $logMessage = "Invalid number of arguments passed to subroutine ".
                    "<getAvailableFiles>";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
   $localIndexHtmlFile = "$localDir/tmp_remote_index.html";
   undef @filesFound;
#
# Remove the old localIndexHtmlFile if it exists
#
   if ( -e $localIndexHtmlFile ) {
      $commandToRun = "$RM_CMD $localIndexHtmlFile";
      if ( ! runSystemCommand(\$commandToRun) ) {
         $logMessage = "Failed to remove old temporary copy of ".
	               "remote index.html file <$localIndexHtmlFile> - ".
		       "Will attempt to overwrite it";
         putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      }
   }
#
# Attempt to get the index.html file showing the list of available files
#
   undef $returnCode; 
   $returnCode = getFileViaWeb("$URLProtocol://$remoteURL",
                               "$localIndexHtmlFile");
   if ( $returnCode == 1 ) {
      if ( $DEBUG ) {
         $logMessage = "Successfully downloaded the data stating what ".
	               "files are available to download from the URL ".
		       "<$URLProtocol://$remoteURL>";
         putLogEntry($LOG_TYPES{'debug'}, \$logMessage);
      }
#
# Open the file and parse it for the names of the available files
#
      $fhIndexHtmlFile = new IO::File;
      if ( $fhIndexHtmlFile->open("< $localIndexHtmlFile") ) {
         while ( ! $fhIndexHtmlFile->eof ) {
            chomp ($readLine = $fhIndexHtmlFile->getline);
	    if ( $readLine =~ /\<[aA] [hH][rR][eE][fF]=\"(${remoteFileNameRegExp})\"\>/ ) {
               if ( $DEBUG ) {
	          $logMessage = "Found remote available file <$1>";
	          putLogEntry($LOG_TYPES{'debug'}, \$logMessage);
               }
	       push(@filesFound, $1); 
	    }
         }
#
# Close the file and clean up
#
         $fhIndexHtmlFile->close;
         undef $fhIndexHtmlFile;
         $commandToRun = "$RM_CMD $localIndexHtmlFile";
         if ( ! runSystemCommand(\$commandToRun) ) {
            $logMessage = "Failed to remove temporary copy of ".
	                  "remote index.html file <$localIndexHtmlFile>";
            putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
         }
      }
      else {
         $logMessage = "Unable to open file handle to read the file ".
	               "<$localIndexHtmlFile>, which is a copy of the ".
		       "remote index.html file from the URL ".
		       "<$URLProtocol://$remoteURL> - Will still attempt ".
		       "to download all files";
         putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
	 push (@filesFound, "$CONTINUE_DOWNLOAD_MSG");
      }
   }
   elsif ( $returnCode == 0 ) {
      $logMessage = "Failed to download the data stating what files are ".
                    "available to download from the URL ".
		    "<$URLProtocol://$remoteURL>";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Return the array of files found
#
   return (@filesFound);
}
#
# Subroutine setHours - Fills the @hours array with values
#
sub setHours {
#
# Global variables used
#================================
#
# %LOG_TYPES
# $ERROR_EXIT_VALUE
#  
#================================
#
# Subroutines called
#================================
#
# putLogEntry
#
#================================
#
# Declare local variables
#
   my $NUM_REQUIRED_ARGS;
   my @hours;
   my $hourIndex;
   my $hourStart;
   my $hourEnd;
   my $hourInterval;
   my $numZerosToAdd;
   my $maxLengthHourString;
   my $hourString;
   my $maxEndHour;
   my $logMessage;
   my $zeroIndex;
   my $remoteSiteAbbreviation;
   my $modelType;
#
# Initialize variables
#
   $NUM_REQUIRED_ARGS = 6;
   if ( scalar(@_) == $NUM_REQUIRED_ARGS ) {
      ($hourStart, $hourEnd, $hourInterval, $maxLengthHourString,
       $remoteSiteAbbreviation, $modelType) = @_;
   }
   else {
      $logMessage = "Invalid number of arguments passed to subroutine ".
                    "<setHours>";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Determine the value for the maximum hour in the file name
#
   if ( "$remoteSiteAbbreviation" eq 'nws' ) {
      $maxEndHour = 10**$maxLengthHourString - 1;
   }
   elsif ( "$remoteSiteAbbreviation" eq 'ncep' ) {
      if ( $modelType =~ /^gfs[34]$/ ) {
         $maxEndHour = 10**($maxLengthHourString+1) - 1;
      }
      else {
         $maxEndHour = 10**$maxLengthHourString - 1;
      }
   }
#
# Verify input arguments allow for hours to be defined
#
   if ( $hourStart >= $hourEnd ) {
      $logMessage = "Start value for download file hours cannot be equal to ".
                    "or greater than the end value";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
   elsif ( $hourInterval >= $hourEnd ) {
      $logMessage = "Interval for download file hours cannot be equal to ".
                    "or greater than the end value";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
   elsif ( $hourEnd > $maxEndHour ) {
      $logMessage = "Invalid value for end hour <$hourEnd> to download - ".
                    "Maximum value allowed for file name <$maxEndHour>";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Fill the hours array
#
   for ($hourIndex=$hourStart; $hourIndex <= $hourEnd;
        $hourIndex+=$hourInterval ) {
      $numZerosToAdd = $maxLengthHourString - length($hourIndex);
      $hourString = $hourIndex;
      for ( $zeroIndex=0; $zeroIndex<$numZerosToAdd; $zeroIndex++ ) {
          $hourString = '0' . "$hourString";
      }
      push (@hours, $hourString);
   }
#
# Return the array of hours
#
   return (@hours);
}
#
# Subroutine readXML - Reads the XML configuration file
#
sub readXML {
#
# Global variables used
# ======================================
#
# %LOG_TYPES
# $ERROR_EXIT_VALUE
#
# ======================================
#
# Declare local variables
#
   my $XMLFile;
   my $refXMLConfig;
   my $logMessage;
   my $NUM_REQUIRED_ARGS;
#
# Initialize variables
#
   $NUM_REQUIRED_ARGS = 1;
   if ( scalar(@_) == $NUM_REQUIRED_ARGS ) {
      $XMLFile = $_[0];
   }
   else {
      $logMessage = "Invalid number of arguments passed to subroutine ".
                    "<readXML>";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Verify that the XML configuration file exists
#
   if ( ! -e $XMLFile ) {
      $logMessage = "XML configuration file <$XMLFile> does not exist";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Example use of XMLin
#  $refXMLConfig = XMLin($XMLFile, ForceArray => 1, KeyAttr => {});
#
# Force the reading of the XML file to use the value of the Name for the
# Model as a key
#
   eval {
      $refXMLConfig = XMLin($XMLFile, ForceArray => qw/Model/,
                            KeyAttr => {Model=>"+Name"},
			    SuppressEmpty => ''
			   );
   };
   if ($@) {
      chomp ($@);
      $logMessage = "Problem reading the XML configuration file <$XMLFile>: $@";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Verify that a reference to a HASH was returned from XMLin
#
   if ( ref($refXMLConfig) && ref($refXMLConfig) eq 'HASH' ) {
      return ($refXMLConfig);
   }
   else {
      $logMessage = "Invalid reference type returned from XMLin ".
                    "after reading the configuration file <$XMLFile>";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
}
#
# Subroutine parseXML - Parses the information read from the XML configuration
#                       file and sets the appropriate variables for the given
#                       model type to use
sub parseXML {
#
# Global variables used
# ======================================
#
# %LOG_TYPES
# $ERROR_EXIT_VALUE
# $DEBUG
#
# ======================================
#
# Declare local variables
#
   my $refXMLConfig;
   my %XMLConfig;
   my $logMessage;
   my $modelToUse;
   my $NUM_REQUIRED_ARGS;
   my $refTmpHash;
#
# Initialize variables
# 
   $NUM_REQUIRED_ARGS = 2;
   if ( scalar(@_) == $NUM_REQUIRED_ARGS ) {
      ($modelToUse, $refXMLConfig) = @_;
   }
   else {
      $logMessage = "Invalid number of arguments passed to subroutine ".
                    "<parseXML>";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Verify that the provided reference to the XMLConfig information is valid
#
   if ( ! ref($refXMLConfig) || ref($refXMLConfig) ne 'HASH' ) {
      $logMessage = "Invalid reference type provided to the subroutine ".
                    "<parseXML> for the XML configuration information";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Convert the XMLConfig reference to a HASH
#
   %XMLConfig = %$refXMLConfig;
#
# Determine if the requested model type is in the XML Configuration
#
   if ( ! defined($XMLConfig{'Model'}{"$modelToUse"}) ) {
      $logMessage = "Requested model type <$modelToUse> is not in the ".
                    "XML configuration";
      putLogEntry($LOG_TYPES{'error'}, \$logMessage, $ERROR_EXIT_VALUE);
   }
#
# Parse the XML configuration and set the variables based on 
# the requested model type
#
   $refTmpHash = $XMLConfig{'Model'}{"$modelToUse"};
#
# Variable: cycleIntervalTime
#
   $cycleIntervalTime = decodeXMLEntry($refTmpHash, 'CycleIntervalTime',
                                       'SCALAR', $modelToUse);
#
# Variable: downloadOffsetTime
#
   $downloadOffsetTime = decodeXMLEntry($refTmpHash, 'DownloadOffsetTime',
                                        'SCALAR', $modelToUse);
#
# Variable: gribType
# Convert the grib type to lower case
#
   $gribType = decodeXMLEntry($refTmpHash, 'GribType', 'SCALAR', $modelToUse);
   $gribType =~ s/\w/\l$&/g;
#
# Variable: dataBeginHour
#
   $dataBeginHour = decodeXMLEntry($refTmpHash, 'DataBeginHour', 'SCALAR',
                                   $modelToUse);
#
# Variable: dataEndHour
#
   $dataEndHour = decodeXMLEntry($refTmpHash, 'DataEndHour', 'SCALAR',
                                 $modelToUse);
#
# Variable: dataHourInterval
#
   $dataHourInterval = decodeXMLEntry($refTmpHash, 'DataHourInterval',
                                      'SCALAR', $modelToUse);
#
# Variable: localDirBasename
#
   $localDirBasename = decodeXMLEntry($refTmpHash, 'LocalDirBasename',
                                      'SCALAR', $modelToUse);
#
# Variable: remoteFileNamePrefix
#
   $remoteFileNamePrefix = decodeXMLEntry($refTmpHash, 'RemoteFileNamePrefix',
                                          'SCALAR', $modelToUse);
#
# Variable: remoteFileNameMiddle
#
   $remoteFileNameMiddle = decodeXMLEntry($refTmpHash, 'RemoteFileNameMiddle',
                                          'SCALAR', $modelToUse);
#
# Variable: remoteFileNameSuffix
#
   $remoteFileNameSuffix = decodeXMLEntry($refTmpHash, 'RemoteFileNameSuffix',
                                          'SCALAR', $modelToUse);
#
# Variable: URLSite
#
   $URLSite = decodeXMLEntry($refTmpHash, 'URL', 'SCALAR', $modelToUse);
#
# Variable: URLProtocol
#
   $URLProtocol = decodeXMLEntry($refTmpHash, 'URLProtocol', 'SCALAR',
                                 $modelToUse);
#
# Variable: URLDirectoryPrefix
#
   $URLDirectoryPrefix = decodeXMLEntry($refTmpHash, 'URLDirectoryPrefix',
                                        'SCALAR', $modelToUse);
#
# Variable: URLDirectoryMiddle
#
   $URLDirectoryMiddle = decodeXMLEntry($refTmpHash, 'URLDirectoryMiddle',
                                        'SCALAR', $modelToUse);
#
# Variable: URLDirectorySuffix
#
   $URLDirectorySuffix = decodeXMLEntry($refTmpHash, 'URLDirectorySuffix',
                                        'SCALAR', $modelToUse);
#
# Variable: validCycles
#
   @validCycles = decodeXMLEntry($refTmpHash, 'ValidCycle', 'ARRAY',
                                 $modelToUse);
}
#
# Subroutine verifyXMLValues - Verifies the values in the XML
#                              configuration file
sub verifyXMLValues {
#
# Global variables used
#============================
#
#  %LOG_TYPES
#  @VALID_URL_SITES
#  $cycleIntervalTime
#  $dataBeginHour
#  $dataEndHour
#  $dataHourInterval
#  $downloadOffsetTime
#  $gribType
#  $localDirBasename
#  $remoteFileNamePrefix
#  $remoteFileNameMiddle
#  $remoteFileNameSuffix
#  $URLProtocol
#  $URLSite
#  $URLDirectoryPrefix
#  $URLDirectoryMiddle
#  $URLDirectorySuffix
#  @validCycles
#  $modelToUse;
#  @VALID_GRIB_TYPES;
#
#============================
#
# Define local variables
#
   my $NUM_REQUIRED_ARGS;
   my $validCycle;
   my $errorFound;
   my $allValidCyclesCorrect;
   my %duplicateValidCycles;
   my @tmpValidCycles;
   my $numValidCycles;
   my $logMessage;
   my $index;
   my $dataHourIntervalCorrect;
   my $minDataBeginHour;
#
# Initialize variables
#
   $errorFound            = 0;
   $allValidCyclesCorrect = 1;
   $minDataBeginHour      = 0;
#
# Verify that the array holding the validCycles has valid values
# First sort the array containing the ValidCycles
#
   @tmpValidCycles = sort { $a<=>$b } (@validCycles);
   undef (@validCycles);
   foreach $validCycle (@tmpValidCycles) {
      push(@validCycles, $validCycle);
   }
#
   foreach $validCycle (@validCycles) {
      if ( $validCycle !~ /^\d{2}$/ || 
           int("$validCycle") < 0 ||
	   int("$validCycle") > 23 ) {
         $logMessage = "Invalid value <$validCycle> for a ValidCycle in ".
                       "the XML configuration for model <$modelToUse>";
         putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
         $errorFound = 1;
         $allValidCyclesCorrect = 0;
      }
      elsif ( scalar(grep(/^$validCycle$/, @validCycles)) > 1 ) {
         if ( ! defined($duplicateValidCycles{"$validCycle"}) ) {
            $logMessage = "Multiple entries of <$validCycle> for a ".
	                  "ValidCycle in the XML configuration for ".
			  "model <$modelToUse>";
            putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
            $errorFound = 1;
	    $duplicateValidCycles{"$validCycle"} = 1;
            $allValidCyclesCorrect = 0;
         }
      }
   }
   if ( $allValidCyclesCorrect ) {
      $numValidCycles = scalar(@validCycles);
      @tmpValidCycles = sort { $a<=>$b } (@validCycles);
      undef (@validCycles);
      foreach $validCycle (@tmpValidCycles) {
         push (@validCycles, $validCycle);
      }
   }
#
# Verify that the Cycle Interval Time is valid
#
   if ( isInt($cycleIntervalTime) ) {
      if ( 24 % int("$cycleIntervalTime") == 0 ) {
         if ( $cycleIntervalTime >= $downloadOffsetTime ) {
            if ( $allValidCyclesCorrect ) {
               for ($index=1; $index<$numValidCycles; $index++) {
                  if ( (int("$validCycles[$index]") - 
	                int("$validCycles[$index-1]")) !=
	               int("$cycleIntervalTime") ) {
                     $logMessage = "The CycleIntervalTime ".
		                   "<$cycleIntervalTime> does not match ".
				   "the difference between the time interval ".
				   "for the ValidCycles ".
				   "<$validCycles[$index-1]> and ".
		   	           "<$validCycles[$index]> specified in the ".
			           "XML configuration for model <$modelToUse>";
                     putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
                     $errorFound = 1;
                  }
               }
            }
            else {
               $logMessage = "Cannot verify the validity of the ".
	                     "CycleIntervalTime because the ValidCycles are ".
			     "not all correct in the XML configuration for ".
			     "model <$modelToUse>";
               putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
               $errorFound = 1;
            }
	 }
	 else {
	    $logMessage = "The CycleIntervalTime <$cycleIntervalTime> ".
	                  "cannot be less than the DownloadOffsetTime ".
			  "<$downloadOffsetTime> in the XML configuration ".
			  "for model <$modelToUse>";
            putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
            $errorFound = 1;
	 }
      }
      else {
         $logMessage = "The CycleIntervalTime <$cycleIntervalTime> is not a ".
	               "multiple of 24 hours in the XML configuration for ".
		       "model <$modelToUse>";
         putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
         $errorFound = 1;
      }
   }
   else {
      $logMessage = "Invalid format for the CycleIntervalTime ".
                    "<$cycleIntervalTime> specified in the XML configuration ".
		    "for model <$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the DataHourInterval is valid
#
   $dataHourIntervalCorrect = 1;
   if ( isInt($dataHourInterval) ) {
      if ( $dataHourInterval <=0 ) {
         $logMessage = "Invalid DataHourInterval ".
                       "<$dataHourInterval> specified in the ".
               	       "XML configuration for model <$modelToUse>";
         putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
         $errorFound = 1;
         $dataHourIntervalCorrect = 0;
      }
      elsif ( $dataHourInterval > $dataEndHour ) {
         $logMessage = "The value for DataHourInterval cannot ".
	               "be greater than the value for ".
		       "DataEndHour - Check the ".
		       "XML configuration for model <$modelToUse>";
         putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
         $errorFound = 1;
         $dataHourIntervalCorrect = 0;
      }
   }
   else {
      $logMessage = "Invalid format for the DataHourInterval ".
                    "<$dataHourInterval> specified in the ".
		    "XML configuration for model <$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the DataBeginHour and DataEndHour are valid
#
   if ( isInt($dataBeginHour) ) {
      if ( $dataBeginHour < $minDataBeginHour ) {
         $logMessage = "Invalid DataBeginHour ".
                       "<$dataBeginHour> specified in the ".
            	       "XML configuration for model <$modelToUse>";
         putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
         $errorFound = 1;
      }
      else {
         if ( isInt($dataEndHour) ) {
	    if ( $dataBeginHour > $dataEndHour ) {
               $logMessage = "The value for DataBeginHour cannot ".
	                     "be greater than the value for ".
			     "DataEndHour - Check the ".
		             "XML configuration for model <$modelToUse>";
               putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
               $errorFound = 1;
	    }
	    else {
	       if ( $dataBeginHour % $dataHourInterval != 0 ) {
                  $logMessage = "The DataBeginHour ".
		                "<$dataBeginHour> is not a multiple ".
				"of the DataIntervalHour <$dataHourInterval> ".
				"in the XML configuration for model ".
				"<$modelToUse>";
                  putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
                  $errorFound = 1;
	       }
	       if ( $dataEndHour % $dataHourInterval != 0 ) {
                  $logMessage = "The DataEndHour ".
		                "<$dataEndHour> is not a multiple ".
				"of the DataIntervalHour <$dataHourInterval> ".
				"in the XML configuration for model ".
				"<$modelToUse>";
                  putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
                  $errorFound = 1;
	       }
	    }
         }
	 else {
            $logMessage = "Invalid format for the DataEndHour ".
                          "<$dataEndHour> specified in the ".
		          "XML configuration for model <$modelToUse>";
            putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
            $errorFound = 1;
         }
      }
   }
   else {
      $logMessage = "Invalid format for the DataBeginHour ".
                    "<$dataBeginHour> specified in the ".
		    "XML configuration for model <$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the DownloadOffsetTime is valid
#
   if ( isInt($downloadOffsetTime) ) {
      if ( $downloadOffsetTime < 0 ) {
         $logMessage = "Invalid DownloadOffsetTime ".
                       "<$downloadOffsetTime> specified in the ".
            	       "XML configuration for model <$modelToUse>";
         putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
         $errorFound = 1;
      }
   }
   else {
      $logMessage = "Invalid format for the DownloadOffsetTime ".
                    "<$downloadOffsetTime> specified in the ".
		    "XML configuration for model <$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the GribType is valid
#
   if ( scalar(grep(/^$gribType$/, @VALID_GRIB_TYPES)) != 1 ) {
      $logMessage = "Invalid GribType <$gribType> specified in the ".
            	    "XML configuration for model <$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the LocalDirBasename is valid
#
   if ( ! isValidDirName($localDirBasename) ) {
      $logMessage = "Invalid LocalDirBasename <$localDirBasename> specified ".
                    "in the XML configuration for model <$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the RemoteFileNamePrefix is valid
#
   if ( ! isValidFileName($remoteFileNamePrefix) ) {
      $logMessage = "Invalid RemoteFileNamePrefix <$remoteFileNamePrefix> ".
                    "specified in the XML configuration for model ".
		    "<$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the RemoteFileNameMiddle is valid
#
   if ( ! isValidFileName($remoteFileNameMiddle) ) {
      $logMessage = "Invalid RemoteFileNameMiddle <$remoteFileNameMiddle> ".
                    "specified in the XML configuration for model ".
		    "<$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the RemoteFileNameSuffix is valid
#
   if ( ! isValidFileName($remoteFileNameSuffix) ) {
      $logMessage = "Invalid RemoteFileNameSuffix <$remoteFileNameSuffix> ".
                    "specified in the XML configuration for model ".
		    "<$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the URLProtocol is valid
#
   if ( scalar(grep(/^$URLProtocol$/, @VALID_URL_PROTOCOLS)) != 1 ) {
      $logMessage = "Invalid URLProtocol <$URLProtocol> specified in the ".
            	    "XML configuration for model <$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that a valid URL Site was specified
#
   if ( scalar(grep(/^$URLSite$/, @VALID_URL_SITES)) != 1 ) {
      $logMessage = "Unknown site <$URLSite> specified in the XML ".
                    "configuration for model <$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the URLDirectoryPrefix is valid
#
   if ( ! isValidDirName($URLDirectoryPrefix) ) {
      $logMessage = "Invalid URLDirectoryPrefix <$URLDirectoryPrefix> ".
                    "specified in the XML configuration for model ".
		    "<$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the URLDirectoryMiddle is valid
#
   if ( ! isValidDirName($URLDirectoryMiddle) ) {
      $logMessage = "Invalid URLDirectoryMiddle <$URLDirectoryMiddle> ".
                    "specified in the XML configuration for model ".
		    "<$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
# Verify that the URLDirectorySuffix is valid
#
   if ( ! isValidDirName($URLDirectorySuffix) ) {
      $logMessage = "Invalid URLDirectorySuffix <$URLDirectorySuffix> ".
                    "specified in the XML configuration for model ".
		    "<$modelToUse>";
      putLogEntry($LOG_TYPES{'warning'}, \$logMessage);
      $errorFound = 1;
   }
#
  ( $errorFound ) ? return 0 : return 1;
}
#
# Subroutine AUTOLOAD - Used to catch undefined subroutine calls
#
sub AUTOLOAD {
#
# Global variables used
#============================
#
#  $SCRIPT
#  $ERROR_EXIT_VALUE;
#  %LOG_TYPES;
#
#============================
#
# Define local variables
#
   my @args;
   my $logMessage;
   my $currentDateTime;
#
# Initialize variables
#
   @args = @_;
   $currentDateTime = strftime("%b %d %Y %H:%M:%S GMT", gmtime(time()));
#
   $logMessage = "The subroutine <$main::AUTOLOAD> does not exist";
#
   if ( scalar(@args) > 0 ) {
      $logMessage .= " - Ignoring arguments <@args> that were passed to ".
                     "the nonexistent subroutine";
   }
#
   STDERR->printf("%s: %23s: %-7s: %s - EXITING!\n",
                  $SCRIPT, $currentDateTime, 'ERROR', $logMessage);
   exit ($ERROR_EXIT_VALUE);
}

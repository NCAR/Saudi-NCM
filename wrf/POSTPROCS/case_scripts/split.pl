#! /usr/bin/perl

# code and run-time directories
$MM5HOME = "/data/fddahome";
$POSTPROCS_DIR = "$MM5HOME/cycle_code/POSTPROCS";
$MM5HOST = "GRM";
$RANGE = "GRM";

$CSH_ARCHIVE = $MM5HOME.'/cycle_code/CSH_ARCHIVE';
$EXECUTABLE_ARCHIVE = $MM5HOME.'/cycle_code/EXECUTABLE_ARCHIVE';
$MM5SPLIT = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/splitv3.exe";

$MM5_FILE = @ARGV[0];

# Splitting MM5 output into hourly files
print("Spliting ${MM5_FILE} into hourly files\n");
$HOURLY_FILE = &mm5split($WORK_DIR_STEP, $MM5_FILE);

exit;

#
# @args - 0 - directory
# @args - 1 - MM5 file
#
sub mm5split
{
    local($dir,$file) = @_;
    my ($fn,$bytes,$domain);
    my ($buf,$year,$month,$day,$hour);
    my $filename;

    print( "    Executing: $MM5SPLIT $file\n");
    system("ln -sf $file fort.10");
    system("$MM5SPLIT");

    foreach $fn (<fort.*>) {

      @f=stat $fn;
      if($fn eq 'fort.10' || $f[7] < 120000) {
        unlink $fn;
        next;
      }

      open(IN,"$fn");

      seek(IN,64,0);        # move the file pointer position to after bhi(12,1)
      $bytes=read(IN,$buf,4);   # this is bhi(13,1)
      $domain=unpack "N",$buf;

      seek(IN,117684,0);
      $bytes=read(IN,$buf,24); # this is the 24-character date/time string
      $buf=~ /^(\d+)\D+(\d+)\D+(\d+)\D+(\d+)\D+(\d+)\D+(\d+)/;

      $year=$1;
      if ( $year < 1950 ) {
       # skip another big header...
       seek(IN,235304,0);
       $bytes=read(IN,$buf,24); # this is the 24-character date/time string
       $buf=~ /^(\d+)\D+(\d+)\D+(\d+)\D+(\d+)\D+(\d+)\D+(\d+)/;
       $year=$1;
       $month=$2;
       $day=$3;
       $hour=$4;
       $min=$5;
       $sec=$6;
      } else {
      $month=$2;
      $day=$3;
      $hour=$4;
      $min=$5;
      $sec=$6;
      }
      if ( $sec > 0 ) {
        $min++;
      } 
# Round to nearest 5 minutes to account for non-zero seconds in the time-stamp
      $r5 = $min%5;
      $n5 = $min - $r5;
      if ($r5 > 2 ) {
       $min5 = $n5+5;
      } else {
       $min5 = $n5;
      }

      close(IN);

      $filename="${year}${month}${day}${hour}${min5}_MMOUTPUT_DOMAIN${domain}.${RANGE}";
      $filename = sprintf("%04d%02d%02d%02d%02d_MMOUTPUT_DOMAIN%s.%s",$year,$month,$day,$hour,$min5,$domain,$RANGE);
      rename($fn,$filename);
    }
    chdir "$WORK_DIR_STEP";
    return ($filename);
}


#!/usr/bin/perl
use File::Basename;

# merge ascii nexrad data from individual radars to one dataset for WRFVAR input
# (ascii nexrad data for individual radars are prepared by VDRAS called by launch_vdras.csh
# Mei Xu 10-05-2009
# each call process datasets in one hour for one domain 
#
$RANGE_NAME   = shift(@ARGV);
$D_ID         = shift(@ARGV);
$current_time = shift(@ARGV);

$RANGE_NAME = "ATC" if (! $RANGE_NAME) ;
$D_ID = "2" if (! $D_ID);
$current_time = `date +%Y%m%d%H` if(! $current_time);
$current_time = substr($current_time,0,10);
 
#
# Note: Keep this section for researcher (original code for developer).
# The directories will be overriden by *.job.pl.
# src_dir: directory for the configuration files
# out_dir: the output directory for merged file (LittleR format).
# [Section 1 start]
#
$src_dir = "/home/fddasys/radar_src/RT_DECODE_NEXRAD_LEVEL2/RT_MERGE_DATA";
$GSJOBDIR = "/raid/GMODJOBS/GM${RANGE_NAME}";
$data_dir = "/raid/fieldData/nexrad_level2/ascii";
$run_dir = "/raid/fieldData/nexrad_level2/Run";
$out_dir = "/raid/radar_ingest/nexrad_level2/LittleR/${RANGE_NAME}${D_ID}";
#$MustHaveDir = "/home/fddasys/bin/musthavedir";
# [Section 1 end]
#

#
# Override the job specific stuff
# - GSJOBDIR, run_dir, out_dir, src_dir
#
$_script_name = basename($0);
$_ops_sub_script_name = dirname($0) . "/" . basename($0, ".pl") . ".job.pl";
#print "  _ops_sub_script_name: $_ops_sub_script_name\n";
if ( -e $_ops_sub_script_name ) {
  print "  require $_ops_sub_script_name\n";
  require $_ops_sub_script_name;
}

system("mkdir -p $out_dir") if ( ! -e "$out_dir" );;
system("mkdir -p $run_dir") if ( ! -e "$run_dir" );;

chdir $run_dir;
print "$RANGE_NAME $D_ID  time = $current_time  output in $out_dir, data_dir: $data_dir \n";
$namelist = "$src_dir/radarlist.level2.${RANGE_NAME}${D_ID}";
system "rm namelist.level2" if ( -e namelist.level2);
system "cp $namelist namelist.level2";
open (IN, "namelist.level2");
@radarlist = ();
while($aline = <IN>) {
 $radar = substr($aline,0,3);
 @radarlist = (@radarlist, "$radar")
}
$nr = @radarlist;
print "process for $nr radars  @radarlist\n";

# process the previous nt hours
$nt = 1;
for($i = 60*($nt-1); $i >= 0;$i=$i-60) {
 $start_time = advance_time($current_time, -${i}*60) ;
 $this_date = substr($start_time,0,8);
 $hh = substr($start_time,8,2);
 $min = substr($start_time,10,2);
 $data_time = substr($start_time,8,6);
 print "process $data_time ${RANGE_NAME} ${D_ID} $date_dir $outfile\n" ;

 @allfiles = ();
 foreach $radar (@radarlist) {
  $datfile = "${data_dir}/K${radar}/${this_date}/${hh}0000.dat";
  print "${_script_name}: datfile: $datfile\n";
  @allfiles = (@allfiles,"$datfile") if ( -e $datfile);
 }
 $nf = @allfiles;
# write output
 if ( $nf > 0 ) {
# wrfdvar radar data header
  $outfile = "${out_dir}/${this_date}/${data_time}.dat";
  print "${_script_name}: outfile: $outfile\n";
  system("mkdir $out_dir/${this_date}") if ( ! -e "$out_dir/${this_date}" );
  open (ME, ">$outfile");
  print ME "TOTAL NUMBER = $nf\n";
  print ME "#-----------------#\n";
  print ME "\n"; 
  foreach $datfile (@allfiles) {
   system("cat $datfile >> $outfile ");
  }
 }
 else {
  print "${_script_name}: No output\n";
 }
}

sub advance_time {
   my ($s_date,$add_s)=@_;
   my ($yy,$mm,$dd,$hh,$mi,$ss);
   my @mdays=(31,31,28,31,30,31,30,31,31,30,31,30,31);
   my $time_new;
# if the advancement is more than 2 months, error may occur
   my $yy = substr($s_date,0,4);
   my $mm = substr($s_date,4,2);
   my $dd = substr($s_date,6,2);
   my $hh = substr($s_date,8,2);
   my $mi = substr($s_date,10,2);
   my $ss = substr($s_date,12,2);

  if($yy%400 == 0) { $mdays[2]=29; 
  } else { $mdays[2]=29 if(($yy%4 == 0) && ($yy%100 != 0)); }

	 $ss = $ss + $add_s;

     while($ss > 59) { $ss -= 60; $mi++; }
     while($mi > 59) { $mi -= 60; $hh++; }
     while($hh > 23) { $hh -= 24; $dd++; }
	 while($dd > $mdays[$mm+0]) { 
	   $dd = $dd - $mdays[$mm+0]; 
	   $mm++; 
       while($mm > 11) { $mm -= 12; $yy++; }
	 }
#
# need to use updated $mdays[$mm+0] if $yy is changed
# okay if $add_s is never > 2 months 
#
     while($ss < 0) { $ss += 60; $mi--; }
	 while($mi < 0) { $mi += 60; $hh--; }
	 while($hh < 0) { $hh += 24; $dd--; }
	 while($dd < 1) { 
       $mm--;
	   while($mm < 1) { $mm += 12; $yy--; }
	   $dd = $dd + $mdays[$mm+0];
     }

  $time_new=($yy*1000000+$mm*10000+$dd*100+$hh)*10000+$mi*100+$ss;
  return $time_new;
 }

sub advance_time_hh {
   my ($s_date,$add_h)=@_;
   my ($yy,$mm,$dd,$hh);
   my @mdays=(31,31,28,31,30,31,30,31,31,30,31,30,31);
   my $time_new;
# if the advancement is more than 2 months, error may occur
   my $yy = substr($s_date,0,4);
   my $mm = substr($s_date,4,2);
   my $dd = substr($s_date,6,2);
   my $hh = substr($s_date,8,2);

  if($yy%400 == 0) { $mdays[2]=29;
  } else { $mdays[2]=29 if(($yy%4 == 0) && ($yy%100 != 0)); }

	   $hh = $hh + $add_h;

       while($hh > 23) { $hh -= 24; $dd++; }
       while($dd > $mdays[$mm+0]) {
         $dd = $dd - $mdays[$mm+0];
         $mm++;
         while($mm > 11) { $mm -= 12; $yy++; }
    }
#
# need to use updated $mdays[$mm+0] if $yy is changed
# okay if $add_h is never > 2 months

     while($hh < 0) { $hh += 24; $dd--; }
     while($dd < 1) {
       $mm--;
	   while($mm < 1) { $mm += 12; $yy--; }
	   $dd = $dd + $mdays[$mm+0];
     }
    $time_new=$yy*1000000+$mm*10000+$dd*100+$hh;
    return $time_new;
}
1;

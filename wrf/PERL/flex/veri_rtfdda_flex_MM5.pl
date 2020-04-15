#!/usr/bin/perl

### This version processes upper-air data for any cycle, not just 02Z/14Z ###

use Getopt::Std;
getopts('c:');

print "PID: $$\n";

$FLEXINPUT  =  $ENV{FLEXINPUT};

if (-e $FLEXINPUT)
{
  print "verif_rtfdda_3hcyc_range_wrf.pl: Using job configuration in $FLEXINPUT\n" if ($DEBUG);
}
else
{
 print "\nFile $FLEXINPUT is missing..... EXITING\n";
 exit(-1);
}

# This input file defines the configuration for the job
require $FLEXINPUT;

# The following variables are passed in from $FLEXINPUT:
# $PERL_ARCHIVE
# $CSH_ARCHIVE
# $EXECUTABLE_ARCHIVE
# $CYC_INT
# $FCST_LENGTH

require  $PERL_ARCHIVE."/Dirs.pm";
require  $PERL_ARCHIVE."/TimeUtil.pm";

require "ctime.pl";

###  This script used to be called with the follwoing command line args
#$this_cycle =  $ARGV[0];
#$RANGE =  $ARGV[1];
#$RANGE = "GRM" if (! $RANGE);
#$EXPNO =  $ARGV[2];
#$EXPNO = "" if (! $EXPNO);
#$GMID  =  $ARGV[3];
#$GMID  = "GW$RANGE" if (! $GMID);
###
## --- not sure what $EXPNO is?

$THIS_CYCLE = $this_cycle ;
$GMID=$GSJOBID;

# $RUNDIR     = "/data/cycles/$JOB_ID/$NODE_MEM"; - defined in $FLEXINPUT
if ( ! -e $RUNDIR )
{
   print ( " RUNDIR does not exist!  $RUNDIR \n");
   exit (1);
}

if ( -e  $GSJOBDIR."/postprocinput.pl" )
{
   require $GSJOBDIR."/postprocinput.pl";
} else {
   print ( " postprocinput.pl does not exist!  $GSJOBDIR \n");
   exit (1);
}

#
# ENVIRONMENT#
$ISLINUX="yes";
$OBSDIR=$RUNDIR;
$MMOUTDIR=$RUNDIR;
$use_saved_qc_obsfile=0;
$VERI_DIR  = "$RUNDIR/verify";  ######
$SAVEDIR = "$RUNDIR";

$SAVE_DIR_SFC="$SAVEDIR/veri_dat/sfc";
$SAVE_DIR_UPR="$SAVEDIR/veri_dat/upr";

$UPR_STATS_TMP="$VERI_DIR/.upr";

$range = $RANGE;$range =~ tr/A-Z/a-z/;
$RANGE_tag =$RANGE;

$WebServer=$DEST_SERVER;
$REMOTE_DIR_SFC="$JOB_LOC/veri_dat/sfc";  ######
$REMOTE_DIR_UPR="$JOB_LOC/veri_dat/upr";  ######
#$web_images_rel = "/images/$range/rtfdda/veri_images";  ######
$web_images_rel = "/images/$range/rtfdda_gmod/veri_images";  ######
$web_images_dir = "$JOB_LOC/veri_images";  ######

if($NUM_DOMS) {
  $NDOM=$NUM_DOMS;
} else {
  $NDOM=5;
}
$QC_CUT = 3;

%PROD=("realtime","yes",  "plot","yes",
       "3dvar","no",      "fdda","no");

print "+++++++++++++ Start FDDA verification stats ++++++++++++++\n";

&MustHaveDir("$SAVE_DIR_SFC/final","$SAVE_DIR_SFC/prelim","$SAVE_DIR_SFC/fcst",
	     "$SAVE_DIR_SFC/3dvar","$SAVE_DIR_SFC/fdda", "$VERI_DIR");

chdir $VERI_DIR;


#       Build the UTC date as yy mm dd hh
$ttime = time;

($sec,$mm,$hh,$dd,$mm,$yy,@_) = gmtime($ttime);
$yy += 1900;

$wait_time = 5400;

if($opt_c) {
  $this_cycle = $opt_c;
  print "Use cycle tag $this_cycle passed in from the command line\n";
#} else {
#  $this_cycle =  sprintf("%04d%02d%02d%02d",$yy,$mm+1,$dd,$hh);
}

print "        Cycle = $this_cycle  at ", &ctime(time);

if ( ! -e "$RUNDIR/${this_cycle}" ) {
   print "   \n The cycle ${this_cycle} does not exist \n. Something wrong. -- Exit.";
#   exit (1);
}


$date=$this_cycle;

print "Cycling interval is $CYC_INT\n";

$numOldCycles = 24/$CYC_INT;

foreach $i (1..$numOldCycles) {
  $hrsRetro = $i*$CYC_INT;
  push(@old_cycles,date_retro($date,$hrsRetro));
}

##### sfc stats/plot first

$ifsfc="yes";
if ($ifsfc eq 'yes') {

print "\ Start sfc stats generation \n";

($file_final,$file_fcst)=save_sfc(\@old_cycles);

#
# In the working directory, create a directory using the current cycle name
# to hold the pictures.
#

mkdir $date, 0755 if( ! -d "$date");

foreach $domain (1..$NDOM) {

# $domain=2;
  @stats_file=stats($domain);
 if($PROD{"plot"} eq "yes") {
  foreach $var ('t','rh','q','ws','wd','slp','psfc') {
    &stats_plot($var,@stats_file);
  }
 }

}

if($PROD{"plot"} eq "yes") {
#
# Create reomte directory on $WebServer and remote copy gifs to $WebServer
#
  print "rsync -e 'ssh -i $KEY' -avzC $date $DEST_SERVER:$JOB_LOC/veri_images/sfc/.\n";
  system("rsync -e 'ssh -i $KEY' -avzC $date $DEST_SERVER:$JOB_LOC/veri_images/sfc/.");
# Create webpage and remote copy to $WebServer
  &html_create;
}

# Remove stats ASCII file

  unlink <stats_*>;

# Remove the picture directory after it's remotely copied to $WebServer

  unlink <$date/*>;
  rmdir $date;

#
# Cannot do the following anymore!!
# Remove folders that are more than 7 days old on $WebServer
#
#if($PROD{"plot"} eq "yes" ) {
#@dirs=`ssh $WebUser\@$WebServer find $web_images_dir/sfc/. -type d`;

#foreach $dir (@dirs) {
#  chomp $dir;
#  $dir=~ /(\d+)$/;
#  $tag=$1;
#  $flag_delete=old_enough($date,$tag);
#  if($flag_delete == 1) {
#    print "Removing $dir on $WebServer!\n";
#    `ssh $WebUser\@$WebServer rm -rf $dir`;
#  }
#}
#}
} # ifsfc end

##### upper-air validation

$ifupr = "yes";

if ($ifupr eq 'yes') {
print "\ Start upr stats generation \n";

# If only the 02Z/14Z cycles need to be process, uncomment the next if block.
# First need to check if the "FINAL" analysis contains time periods
# corresponding to 00 or 12 UT. If it does, then proceed.

$cycle_hour=substr($date,8,2);
$earliest=$cycle_hour-3;
$latest=$cycle_hour-1;
#if((($earliest <= 12) && ($latest >= 12)) ||
#   (($earliest <= 0) && ($latest >= 0))) {

  &MustHaveDir("$SAVE_DIR_UPR/final","$SAVE_DIR_UPR/fcst","$UPR_STATS_TMP");

# &MustHaveDir("$SAVE_DIR_UPR/prelim","$SAVE_DIR_UPR/3dvar","$SAVE_DIR_UPR/fdda");

  @upr_pairs_files=save_upr($date,\@old_cycles);
  print "upper-air pairs files : @upr_pairs_files\n";

  foreach $domain (1..$NDOM) {
#   $domain=2;
    $i=0;
    foreach $file (@upr_pairs_files) {
       $stats_upr_file[$i]=stats_upr($file,$domain);
       $i++;
    }

   if($PROD{"plot"} eq "yes") {
    foreach $fn (@stats_upr_file) {
      $html_table=html_table_upr($fn);
      $html_table=~ /^\d+_(\d+)_d(\d+)/;
      $upr_dir=$1;
      $d=$2;
      $html_doc="${upr_dir}_d${d}.html";
##    $msg=`ssh $WebUser\@$WebServer find $web_images_dir/upr -name $upr_dir -print`;
##    system("ssh $WebUser\@$WebServer mkdir $web_images_dir/upr/$upr_dir") if(length($msg) == 0);
##    system("ssh $WebUser\@$WebServer touch $web_images_dir/upr/$upr_dir/$html_doc");
##    system("scp $html_table $WebUser\@$WebServer:$web_images_dir/upr/$upr_dir/.");
##    system("ssh $WebUser\@$WebServer 'cat $web_images_dir/upr/$upr_dir/$html_table >> $web_images_dir/upr/$upr_dir/$html_doc'");
      &MustHaveDir("$UPR_STATS_TMP/$upr_dir");
      system("touch $UPR_STATS_TMP/$upr_dir/$html_doc");
      system("cat $html_table >> $UPR_STATS_TMP/$upr_dir/$html_doc");
      system("rsync -e 'ssh -i $KEY' -avzC $UPR_STATS_TMP/$upr_dir $DEST_SERVER:$JOB_LOC/veri_images/upr/.");

      unlink ("$html_table");
    }
   }
  }  ## end foreach domain
  unlink <*veri_upr_stats*>;
#
# Cannont do the following anymore!!
# Remove folders that are more than 7 days old on $WebServer
#
#if($PROD{"plot"} eq "yes") {
# @dirs=`ssh $WebUser\@$WebServer find $web_images_dir/upr/. -type d`;

# foreach $dir (@dirs) {
#   chomp $dir;
#   $dir=~ /(\d+)$/;
#   $tag=$1;
#   $flag_delete=old_enough($date,$tag);
#   if($flag_delete == 1) {
#     print "Removing $dir on $WebServer!\n";
#     `ssh $WebUser\@$WebServer rm -rf $dir`;
#   }
# }
#}
#}  #if earliest end
}   # ifupr end

exit;


#
#
#
sub save_sfc {
  my ($r_old_cycles)=@_[0];
  my $cycle;
  my $cycOldest;
  my $d;
  my $fn;
  my @files;

 if($use_saved_qc_obsfile==0) {
  unlink 'obs.dat' if(-e 'obs.dat');
  system("rm -rf qc_out_*");
  foreach $cycle (sort @$r_old_cycles) {
    system("ln -sf $OBSDIR/$cycle/RAP_RTFDDA/qc_out* .");
  }
  system("ln -sf $OBSDIR/$date/RAP_RTFDDA/qc_out* .");
  system("cat qc_out_* > qc_out.dat");
 } elsif($use_saved_qc_obsfile ==1) {
# yliu
  unlink 'qc_out.dat' if(-e 'qc_out.dat');
  for($i=0;$i<25;$i++) {
   $dh=date_retro($date,$i);
   $qc_file="qc_out_".
            substr($dh,0,4)."-".substr($dh,4,2)."-".substr($dh,6,2)."_".
            substr($dh,8,2).":00:00.0000";
   system("cat $OBSDIR/$qc_file >> qc_out.dat");
  }
# yliu
 }
  unlink 'surface_obs.cleanup' if(-e 'surface_obs.cleanup');
  system("$CSH_ARCHIVE/Forecast/RT_all.obs_cleanup.pl -f qc_out.dat -l $PERL_ARCHIVE");
  system("$EXECUTABLE_ARCHIVE/v_rewrite_obs.exe surface_obs.cleanup");
  system("sort -k1 fort.31 > obs.dat");

### Forecast!

if($PROD{"realtime"} eq "yes") {   #--------
### Forecast!
 if($MMOUTDIR eq $RUNDIR) {
  $cycOldest = pop @$r_old_cycles;
  print "Cycle 24 hours ago: $cycOldest\n";
  $new_dir="$RUNDIR/$cycOldest";
  push (@$r_old_cycles,$cycOldest);
 } else {
  $new_dir=$MMOUTDIR;
 }
  if(-e $new_dir) {
    foreach $d (1..$NDOM) {
#     system("touch pairs_cat$d");
#     foreach $fn (<$new_dir/MM5_P/MMOUT_DOMAIN${d}_*>) {
        $fn="$new_dir/${cycOldest}_MMOUTPUT_DOMAIN${d}.${RANGE}_P+FCST";
        print "\n process $fn \n";
        system("$EXECUTABLE_ARCHIVE/v_mm5_sfc.exe $fn") if(-e $fn);
        system("dd bs=56 conv=swab if=pairs_domain$d of=pairs_swap$d");
#       system("cat pairs_swap$d >> pairs_cat$d");
#     }
    }
  }
#foreach $d (1..$NDOM) {
# ## for linux: swap
# if($ISLINUX eq 'yes') {
#  system("dd bs=56 conv=swab if=pairs_domain$d of=pairs_swap$d");
# } else {
# ## for SGI
#  system("mv pairs_domain$d pairs_swap$d");
# }
#}
  system("$CSH_ARCHIVE/verif/v_merge_pairs.pl pairs_swap*");
  system("mv pairs_swap1.out pairs_swap");
# rename("pairs_cat$d",'pairs_swap');

  $fn= $cycOldest . "_veri_dat_${RANGE_tag}_FCST";
##if($PROD{"plot"} eq "yes") {   #--------
## system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC pairs_swap $WebUser\@$WebServer:$REMOTE_DIR_SFC/fcst/$fn");
##}
  system("mv pairs_swap $SAVE_DIR_SFC/fcst/$fn");

  system("rm -rf pairs_*");
  unshift(@files,"$SAVE_DIR_SFC/fcst/$fn");

  print "   Finish fcst_sfc of $cycOldest cycle at ", &ctime(time);
}   #--------

### Preliminary!

#if($PROD{"realtime"} eq "yes") {   #--------
#  $new_dir="$RUNDIR/$$r_old_cycles[0]";
# print "$new_dir\n";
#  if(-e $new_dir) {
#    foreach $d (1..$NDOM) {
#      $new_fn=$new_dir . '/' . $$r_old_cycles[0] . '_MMOUTPUT_DOMAIN' . $d .
#              ".${RANGE}_P+FCST";
#      system("$EXECUTABLE_ARCHIVE/v_mm5_sfc.exe $new_fn -p") if(-e $new_fn);
#    }
#  }

# if ($NDOM == 4 && -s 'pairs_domain4') {
#  system("$EXECUTABLE_ARCHIVE/v_merge_pairs.exe pairs_domain1 pairs_domain2 pairs_domain3 pairs_domain4");
# } elsif ($NDOM == 3) {
#  system("$EXECUTABLE_ARCHIVE/v_merge_pairs.exe pairs_domain1 pairs_domain2 pairs_domain3");
# }

#foreach $d (1..$NDOM) {
#  ## for linux: swap
#  if($ISLINUX eq 'yes') {
#   system("dd bs=56 conv=swab if=pairs_domain$d of=pairs_swap$d");
#  } else {
#  ## for SGI
#   system("mv pairs_domain$d pairs_swap$d");
#  }
#}

#  system("$CSH_ARCHIVE/verif/v_merge_pairs.pl pairs_swap1 pairs_swap2 pairs_swap3 pairs_swap4");
#  system("mv pairs_swap1.out pairs_swap");

#  $fn=$$r_old_cycles[0] . "_veri_dat_${RANGE_tag}_P";
#  system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC pairs_swap $WebUser\@$WebServer:$REMOTE_DIR_SFC/prelim/$fn");
#  system("mv pairs_swap $SAVE_DIR_SFC/prelim/$fn");

#  system("rm -rf pairs_*");
#  $files[1]="$SAVE_DIR_SFC/prelim/$fn";
#  print "   Finish prelim-fdda_sfc of $date cycle  at ", &ctime(time);

### Final!

   foreach $d (1..$NDOM) {
#   system("touch pairs_cat$d");
#   foreach $fn (<$RUNDIR/$date/MM5_F/MMOUT_DOMAIN$d_*>) {
      if($MMOUTDIR eq $RUNDIR) {
        $fn="$RUNDIR/$date/${date}_MMOUTPUT_DOMAIN${d}.${RANGE}_F";
      } else {
        $fn="$MMOUTDIR/${date}_MMOUTPUT_DOMAIN${d}.${RANGE}_F";
      }
        print "\n process $fn \n";
      system("$EXECUTABLE_ARCHIVE/v_mm5_sfc.exe $fn") if( -e $fn );
      system("dd bs=56 conv=swab if=pairs_domain$d of=pairs_swap$d");
#     system("cat pairs_swap$d >> pairs_cat$d");
#   }
   }

#foreach $d (1..$NDOM) {
#  ## for linux: swap
#  if($ISLINUX eq 'yes') {
#   system("dd bs=56 conv=swab if=pairs_domain$d of=pairs_swap$d");
#  } else {
#  ## for SGI
#   system("mv pairs_domain$d pairs_swap$d");
#  }
#}

  system("$CSH_ARCHIVE/verif/v_merge_pairs.pl pairs_swap*");
  system("mv pairs_swap1.out pairs_swap");
# rename("pairs_cat$d",'pairs_swap');

  $fn=$date . "_veri_dat_${RANGE_tag}_F";
##system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC pairs_swap $WebUser\@$WebServer:$REMOTE_DIR_SFC/final/$fn");
  system("mv pairs_swap $SAVE_DIR_SFC/final/$fn");

  system("rm -rf pairs_*");
  print "   Finish Final-fdda_sfc of $date cycle  at ", &ctime(time);
  unshift(@files,"$SAVE_DIR_SFC/final/$fn");

  return @files;
}

#
#
#
#
sub stats {

  my $d=$_[0];
  my $file;
  my $count;
  my $bytes;
  my $outf;
  my @f;
  my $year;
  my $nbytes=56;
  my $missing=-8888;
  my ($tag,$type);
  my ($yy,$mm,$dd,$hh);
  my ($n,$i);
  my ($date,$date_min);

  $nf=-1;

# foreach $file ($file_final,$file_prelim,$file_fcst) {
  foreach $file ($file_final,$file_fcst) {
    if( -e "$file") {
      $nf += 1;
      $file=~ /(\d+)_veri_dat/;
      $tag=$1;
      $file=~/([A-Z]*)$/;
      $type=$1;
      $outf='stats_' . $tag . '_d' . $d . '_' . $type;
      open(IN,"$file");
      open(OUT,"> $outf");
      $output[$nf]=$outf;
      @f=stat "$file";
      $file_size=$f[7];
      $total_records=$file_size/$nbytes;

      for($i=0; $i < 24; $i++) {
         $sum_t_b[$i]=0;
         $sum_t_r[$i]=0;
         $sum_t_m[$i]=0;
         $no_t[$i]=0;

         $sum_rh_b[$i]=0;
         $sum_rh_r[$i]=0;
         $sum_rh_m[$i]=0;
         $no_rh[$i]=0;

         $sum_ws_b[$i]=0;
         $sum_ws_r[$i]=0;
         $sum_ws_m[$i]=0;
         $no_ws[$i]=0;

         $sum_wd_b[$i]=0;
         $sum_wd_r[$i]=0;
         $sum_wd_m[$i]=0;
         $no_wd[$i]=0;

         $sum_slp_b[$i]=0;
         $sum_slp_r[$i]=0;
         $sum_slp_m[$i]=0;
         $no_slp[$i]=0;

         $sum_psfc_b[$i]=0;
         $sum_psfc_r[$i]=0;
         $sum_psfc_m[$i]=0;
         $no_psfc[$i]=0;

         $sum_q_b[$i]=0;
         $sum_q_r[$i]=0;
         $sum_q_m[$i]=0;
         $no_q[$i]=0;

      }

      $count=0;
      for($n=0; $n < $total_records; $n++) {
         seek(IN,0,1);
         $bytes=read(IN,$buf,$nbytes);
         ($year,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
          $psfc_m,$psfc_o,$psfc_qc,
          $slp_m,$slp_o,$slp_qc,
          $ter_m,$ter_o,
          $t2_m,$t2_o,$t2_qc,
          $q_m,$q_o,$q_qc,
          $ws_m,$ws_o,$ws_qc,
          $wd_m,$wd_o,$wd_qc)=unpack("s6a4s20",$buf);

          next if($domain_id != $d);

          $count++;

          $month=int($monthday/100);
          $day=$monthday-$month*100;

          $hour=int($hourmin/100);
          $minute=$hourmin-$hour*100;

	  $this_date=$year*100000000+$month*1000000+$day*10000+$hour*100+$minute;
	  $date=nearest_hour($this_date);

          $date_min=date_retro($date,1) if($count == 1);

          $index=indexcal($date,$date_min);
          if($count == 1) {
            $index_max=$index;
          }
          $index_max=$index if($index > $index_max);

          if(($t2_m > $missing) && ($t2_o > $missing) && ($t2_qc  > $QC_CUT)) {
            $t2_m *= 0.01;
            $t2_o *= 0.01;
            $diff_t=$t2_m-$t2_o;
            $sum_t_b[$index] += $diff_t;
            $sum_t_r[$index] += $diff_t**2;
            $sum_t_m[$index] += abs($diff_t);
            $no_t[$index] += 1;
          }

#yliu -- take very weak winds (<0.1 m/s) out of count of wind directions
         if(($ws_m > 100.) && ($ws_o > 100.) && ($ws_qc  > $QC_CUT) &&
             ($wd_m > $missing) && ($wd_o > $missing) && ($wd_qc  > $QC_CUT)
             && ($wd_o < 361)) {
#yliu    if(($wd_m > $missing) && ($wd_o > $missing) && ($wd_qc  > $QC_CUT)) {
            $diff_wd=$wd_m-$wd_o;

            $diff_wd += 360 if($diff_wd < -180);
            $diff_wd -= 360 if($diff_wd > 180);

            $sum_wd_b[$index] += $diff_wd;
            $sum_wd_r[$index] += $diff_wd**2;
            $sum_wd_m[$index] += abs($diff_wd);
            $no_wd[$index] += 1;
          }

          if(($ws_m > $missing) && ($ws_o > $missing) && ($ws_qc  > $QC_CUT)) {
            $ws_m *= 0.01;
            $ws_o *= 0.01;
            $diff_ws=$ws_m-$ws_o;
            $sum_ws_b[$index] += $diff_ws;
            $sum_ws_r[$index] += $diff_ws**2;
            $sum_ws_m[$index] += abs($diff_ws);
            $no_ws[$index] += 1;
          }

          if(($slp_m > $missing) && ($slp_o > $missing) && ($slp_qc  > $QC_CUT)) {
            $slp_m *= 0.1;
            $slp_o *= 0.1;
            $diff_slp=$slp_m-$slp_o;
            $sum_slp_b[$index] += $diff_slp;
            $sum_slp_r[$index] += $diff_slp**2;
            $sum_slp_m[$index] += abs($diff_slp);
            $no_slp[$index] += 1;
          }

          if(($psfc_m > $missing) && ($psfc_o > $missing) && ($psfc_qc  > $QC_CUT)) {
            $psfc_m *= 0.1;
            $psfc_o *= 0.1;
            $diff_psfc=$psfc_m-$psfc_o;
            $sum_psfc_b[$index] += $diff_psfc;
            $sum_psfc_r[$index] += $diff_psfc**2;
            $sum_psfc_m[$index] += abs($diff_psfc);
            $no_psfc[$index] += 1;
          }

          if(($q_m > $missing) && ($q_o > $missing) && ($q_qc  > $QC_CUT)) {
            $q_m *= 0.01;
            $q_o *= 0.01;
            $diff_q=$q_m-$q_o;
            $sum_q_b[$index] += $diff_q;
            $sum_q_r[$index] += $diff_q**2;
            $sum_q_m[$index] += abs($diff_q);
            $no_q[$index] += 1;
          }

          if(($psfc_m > $missing) && ($psfc_o > $missing) &&
             ($t2_m > $missing)   && ($t2_o > $missing) &&
             ($q_m > $missing)    && ($q_o > $missing) &&
             ($t2_qc  > $QC_CUT) && ($psfc_qc  > $QC_CUT) && ($q_qc  > $QC_CUT)) {

            $rh_m=rh_from_q($psfc_m,$t2_m,$q_m);
            $rh_o=rh_from_q($psfc_o,$t2_o,$q_o);

            $diff_rh=$rh_m-$rh_o;
            $sum_rh_b[$index] += $diff_rh;
            $sum_rh_r[$index] += $diff_rh**2;
            $sum_rh_m[$index] += abs($diff_rh);
            $no_rh[$index] += 1;
          }

      }

      close(IN);

      for($i=1; $i <= $index_max; $i++) {
         if($no_t[$i] > 0) {
           $t_bias[$i]=$sum_t_b[$i]/$no_t[$i];
           $t_rmse[$i]=sqrt($sum_t_r[$i]/$no_t[$i]);
           $t_mae[$i]=$sum_t_m[$i]/$no_t[$i];
         } else {
           $t_bias[$i]=-99;
           $t_rmse[$i]=-99;
           $t_mae[$i]=-99;
         }

         if($no_rh[$i] > 0) {
           $rh_bias[$i]=$sum_rh_b[$i]/$no_rh[$i];
           $rh_rmse[$i]=sqrt($sum_rh_r[$i]/$no_rh[$i]);
           $rh_mae[$i]=$sum_rh_m[$i]/$no_rh[$i];
         } else {
           $rh_bias[$i]=-99;
           $rh_rmse[$i]=-99;
           $rh_mae[$i]=-99;
         }

         if($no_ws[$i] > 0) {
           $ws_bias[$i]=$sum_ws_b[$i]/$no_ws[$i];
           $ws_rmse[$i]=sqrt($sum_ws_r[$i]/$no_ws[$i]);
           $ws_mae[$i]=$sum_ws_m[$i]/$no_ws[$i];
         } else {
           $ws_bias[$i]=-99;
           $ws_rmse[$i]=-99;
           $ws_mae[$i]=-99;
         }

         if($no_wd[$i] > 0) {
           $wd_bias[$i]=$sum_wd_b[$i]/$no_wd[$i];
           $wd_rmse[$i]=sqrt($sum_wd_r[$i]/$no_wd[$i]);
           $wd_mae[$i]=$sum_wd_m[$i]/$no_wd[$i];
         } else {
           $wd_bias[$i]=-99;
           $wd_rmse[$i]=-99;
           $wd_mae[$i]=-99;
         }

         if($no_slp[$i] > 0) {
           $slp_bias[$i]=$sum_slp_b[$i]/$no_slp[$i];
           $slp_rmse[$i]=sqrt($sum_slp_r[$i]/$no_slp[$i]);
           $slp_mae[$i]=$sum_slp_m[$i]/$no_slp[$i];
         } else {
           $slp_bias[$i]=-99;
           $slp_rmse[$i]=-99;
           $slp_mae[$i]=-99;
         }

         if($no_psfc[$i] > 0) {
           $psfc_bias[$i]=$sum_psfc_b[$i]/$no_psfc[$i];
           $psfc_rmse[$i]=sqrt($sum_psfc_r[$i]/$no_psfc[$i]);
           $psfc_mae[$i]=$sum_psfc_m[$i]/$no_psfc[$i];
         } else {
           $psfc_bias[$i]=-99;
           $psfc_rmse[$i]=-99;
           $psfc_mae[$i]=-99;
         }

         if($no_q[$i] > 0) {
           $q_bias[$i]=$sum_q_b[$i]/$no_q[$i];
           $q_rmse[$i]=sqrt($sum_q_r[$i]/$no_q[$i]);
           $q_mae[$i]=$sum_q_m[$i]/$no_q[$i];
         } else {
           $q_bias[$i]=-99;
           $q_rmse[$i]=-99;
           $q_mae[$i]=-99;
         }

      }

      printf OUT "%10d\n",$date_min;
      for($i=1; $i <= $index_max; $i++) {
         printf OUT "%2d%7.1f%7.1f%7.1f%5d%7.1f%7.1f%7.1f%5d%7.1f%7.1f%7.1f%5d%7.1f%7.1f%7.1f%5d%7.1f%7.1f%7.1f%5d%7.1f%7.1f%7.1f%5d%7.1f%7.1f%7.1f%5d\n",
         $i,$t_bias[$i],$t_rmse[$i],$t_mae[$i],$no_t[$i],
            $rh_bias[$i],$rh_rmse[$i],$rh_mae[$i],$no_rh[$i],
            $ws_bias[$i],$ws_rmse[$i],$ws_mae[$i],$no_ws[$i],
            $wd_bias[$i],$wd_rmse[$i],$wd_mae[$i],$no_wd[$i],
            $slp_bias[$i],$slp_rmse[$i],$slp_mae[$i],$no_slp[$i],
            $psfc_bias[$i],$psfc_rmse[$i],$psfc_mae[$i],$no_psfc[$i],
            $q_bias[$i],$q_rmse[$i],$q_mae[$i],$no_q[$i];
      }

     close(OUT);
    }

  }

  return @output;

}

#
#
#
#
#

sub rh_from_q {

  my ($p,$t,$q)=@_;
  my ($es,$qs,$rh);

  $q *= 0.001;

  $es=10**(-2937.4/($t+273.15)-4.9283*log($t+273.15)/log(10)+23.5518);
  $qs=0.622*$es/($p-$es);
  $rh=$q/$qs*100;
  $rh=0 if($rh < 0);
  $rh=100 if($rh > 100);

  return $rh;
}

#
#
#
#
#

sub indexcal {

  my ($date,$date_min)=@_;
  my ($yy_now,$mm_now,$dd_now,$hh_now,$yy_min,$mm_min,$dd_min,$hh_min);
  my ($secs_total,$secs_min);
  my $index;

  $yy_now=int($date/1000000);
  $mm_now=int(($date%1000000)/10000);
  $dd_now=int(($date%10000)/100);
  $hh_now=$date%100;

  $yy_min=int($date_min/1000000);
  $mm_min=int(($date_min%1000000)/10000);
  $dd_min=int(($date_min%10000)/100);
  $hh_min=$date_min%100;

  $secs_total=date2secs($yy_now,$mm_now,$dd_now,$hh_now,0,0,0);
  $secs_min=date2secs($yy_min,$mm_min,$dd_min,$hh_min,0,0,0);

  $index=int($secs_total-$secs_min)/3600;

  return $index;

}

#
#
#
#
#

sub stats_plot {

  my ($var,@files)=@_;
  my ($domain,$title);
  my $file;
  my ($time_tag,$time_stamp,$time_min,$time_max);
  my @f;
  my $n,$ind;
  my $outf;
  my $count;
  my $n_elements;

  print "plotting surface stats...\n";

  if($var eq 't') {
    $fadd=0;
    $b_RANGE='-5/5';
    $rm_RANGE='0/5';
    $y_ref=-1.25;
  } elsif($var eq 'rh') {
    $fadd=4;
    $b_RANGE='-20/20';
    $rm_RANGE='0/40';
    $y_ref=-10;
  } elsif($var eq 'ws') {
    $fadd=8;
    $b_RANGE='-5/5';
    $rm_RANGE='0/5';
    $y_ref=-1.25
  } elsif($var eq 'wd') {
    $fadd=12;
    $b_RANGE='-50/50';
    $rm_RANGE='0/100';
    $y_ref=-25;
  } elsif($var eq 'slp') {
    $fadd=16;
    $b_RANGE='-10/10';
    $rm_RANGE='0/20';
    $y_ref=-5;
  } elsif($var eq 'psfc') {
    $fadd=20;
    $b_RANGE='-10/10';
    $rm_RANGE='0/20';
    $y_ref=-5;
  } elsif($var eq 'q') {
    $fadd=24;
    $b_RANGE='-2/2';
    $rm_RANGE='0/2';
    $y_ref=-0.5
  } else {
    die "Wrong variable, pick from 't', 'rh', 'q','ws', 'wd', 'slp', and 'psfc'";
  }

#
# BIAS loop
#
  $n_elements=scalar @files;
  $n=0;
  foreach $file (@files) {

    $file=~ /_d(\d+)/;
    $domain=$1;

    $n++;
    if($file=~ /_F$/) {
      $file=~ /(\d+)/;
      $time_tag=$1;
      $time_stamp_f=$time_tag;
      $time_min=date_retro($time_tag,24);
      $time_max=date_retro($time_tag,1);
      $color='/0/255/0';
    } elsif($file=~ /_P$/) {
      $file=~ /(\d+)/;
      $time_tag=$1;
      $time_stamp_p=$time_tag;
      $time_min=date_retro($time_tag,10);
      $time_max=advance_h($time_tag,2);
      $color='/0/0/255';
    } else {                             # forecast
      $file=~ /(\d+)/;
      $time_tag=$1;
      $time_stamp_fcst=$time_tag;
      $time_min=advance_h($time_tag,0);
      $time_max=advance_h($time_tag,23);
      $color='/255/0/0';
    }

    $start_time=`head -1 $file`;
    $beg=indexcal($start_time,$time_min);

    open(STATS,"$file");

    $outf="${date}/${time_stamp_f}_${var}_d${domain}.ps";
    $title="Domain $domain";

    if($n == 1) {

      if($var eq 't') {
        $yaxis='"T  BIAS (K)"';
        $ytick='f0.5a1';
      } elsif($var eq 'rh') {
        $yaxis='"RH  BIAS (%)"';
        $ytick='f2a10';
      } elsif($var eq 'ws') {
        $yaxis='"SPD  BIAS (m s@+-1@+)"';
        $ytick='f0.5a1';
      } elsif($var eq 'wd') {
        $yaxis='"DIR  BIAS (\312)"';
        $ytick='f50a10';
      } elsif($var eq 'slp') {
        $yaxis='"SLP  BIAS (hPa)"';
        $ytick='f1a2';
      } elsif($var eq 'psfc') {
        $yaxis='"PSFC  BIAS (hPa)"';
        $ytick='f1a2';
      } else {
        $yaxis='"Q  BIAS (g kg@+-3@+)"';
        $ytick='f0.1a0.5';
      }

      open(PIPE,"| psxy -JX7.32/2 -R0/24.4/$b_RANGE -Ba1:.\"$title\":/$ytick:$yaxis: -M -W1.5p$color -X1 -Y7.5 -K > $outf");
    } else {
      open(PIPE,"| psxy -JX -R -W1.5p$color -O -K >> $outf");
    }

#   open(PIPE1,"| pstext -JX -R -N -O -K >> $outf") if($n == $n_elements);

#   $b_RANGE=~ /(\-*\d+)\/(\d+)/;
    $b_RANGE=~ /([0-9\-\.]+)\/([0-9\.]+)/;
    $y_no=($2-$1)*0.05+$1;

    $count=0;
    while ($record=<STATS>) {
      next if($. == 1);
      @f=split " ",$record;

      $ind=$f[0]+$beg;

      if($f[1+$fadd] != -99) {
        print PIPE "$ind $f[1+$fadd]\n";
#       print PIPE1 "$ind $y_no 12 0 0 MC $f[4+$fadd]\n" if($n == $n_elements);
        $count++;
      } else {
        print PIPE '>'," $ind $f[1+$fadd]\n" if($count > 0);
      }

    }

    close(STATS);
    close(PIPE);
    open(PIPE,"| psxy -JX -R -Wta -O -K >> $outf");
    print PIPE "0 0\n 24.4 0";
    close(PIPE);
#   close(PIPE1) if($n == $n_elements);

  }

#
# RMSE loop
#
  $n=0;
  foreach $file (@files) {
    $n++;
    if($file=~ /_F$/) {
      $color='/0/255/0';
    } elsif($file=~ /_P$/) {
      $color='/0/0/255';
    } else {                             # forecast
      $color='/255/0/0';
    }

    $start_time=`head -1 $file`;
    $beg=indexcal($start_time,$time_min);

    open(STATS,"$file");

    if($n == 1) {

      if($var eq 't') {
        $yaxis='"T  RMSE (K)"';
        $ytick='f0.5a1';
      } elsif($var eq 'rh') {
        $yaxis='"RH  RMSE (%)"';
        $ytick='f2a10';
      } elsif($var eq 'ws') {
        $yaxis='"SPD  RMSE (m s@+-1@+)"';
        $ytick='f0.5a1';
      } elsif($var eq 'wd') {
        $yaxis='"DIR  RMSE (\312)"';
        $ytick='f50a10';
      } elsif($var eq 'slp') {
        $yaxis='"SLP  RMSE (hPa)"';
        $ytick='f1a2';
      } elsif($var eq 'psfc') {
        $yaxis='"PSFC  RMSE (hPa)"';
        $ytick='f1a2';
      } else {
        $yaxis='"Q  RMSE (g kg@+-3@+)"';
        $ytick='f0.1a0.5';
      }

      open(PIPE,"| psxy -JX7.32/2 -R0/24.4/$rm_RANGE -Ba1/$ytick:$yaxis: -M -W1.5p$color -Y-2.5 -K -O >> $outf");
    } else {
      open(PIPE,"| psxy -JX -R -W1.5p$color -O -K >> $outf");
    }

    $count=0;

    while ($record=<STATS>) {
      next if($. == 1);
      @f=split " ",$record;

      $ind=$f[0]+$beg;

      if($f[2+$fadd] != -99) {
        print PIPE "$ind $f[2+$fadd]\n";
        $count++;
      } else {
        print PIPE '>'," $ind $f[2+$fadd]\n" if($count > 0);
      }

    }

    close(STATS);
    close(PIPE);

  }
#
# MAE loop
#

  $n=0;
  foreach $file (@files) {
    $n++;
    if($file=~ /_F$/) {
      $color='/0/255/0';
    } elsif($file=~ /_P$/) {
      $color='/0/0/255';
    } else {                             # forecast
      $color='/255/0/0';
    }

    $start_time=`head -1 $file`;
    $beg=indexcal($start_time,$time_min);

    open(STATS,"$file");

    if($n == 1) {

      if($var eq 't') {
        $yaxis='"T  MAE (K)"';
        $ytick='f0.5a1';
      } elsif($var eq 'rh') {
        $yaxis='"RH  MAE (%)"';
        $ytick='f2a10';
      } elsif($var eq 'ws') {
        $yaxis='"SPD  MAE (m s@+-1@+)"';
        $ytick='f0.5a1';
      } elsif($var eq 'wd') {
        $yaxis='"DIR  MAE (\312)"';
        $ytick='f50a10';
      } elsif($var eq 'slp') {
        $yaxis='"SLP  MAE (hPa)"';
        $ytick='f1a2';
      } elsif($var eq 'psfc') {
        $yaxis='"PSFC  MAE (hPa)"';
        $ytick='f1a2';
      } else {
        $yaxis='"Q  MAE (g kg@+-3@+)"';
        $ytick='f0.1a0.5';
      }

      open(PIPE,"| psxy -JX7.32/2 -R0/24.4/$rm_RANGE -Ba1:HOUR:/$ytick:$yaxis: -M -W1.5p$color -Y-2.5 -K -O >> $outf");
    } else {
      open(PIPE,"| psxy -JX -R -W1.5p$color -O -K >> $outf");
    }

    $count=0;

    while ($record=<STATS>) {
      next if($. == 1);
      @f=split " ",$record;

      $ind=$f[0]+$beg;

      if($f[3+$fadd] != -99) {
        print PIPE "$ind $f[3+$fadd]\n";
        $count++;
      } else {
        print PIPE '>'," $ind $f[3+$fadd]\n" if($count > 0);
      }

    }

    close(STATS);
    close(PIPE);

  }


# Label the reference date/time tag

  open(PIPE,"| pstext -JX -R -N -O -K >> $outf");
  print PIPE "0 $y_ref 12 0 0 MC $time_min";
  close(PIPE);

# Annotate

  open(PIPE,"| psxy -JX7/0.25 -R0/24.4/0/10 -W1.5p/255/0/0 -Y-1 -O -K >> $outf");
  print PIPE "0 5 \n 7.2 5";
  close(PIPE);
# open(PIPE,"| psxy -JX -R -W1.5p/0/0/255 -O -K >> $outf");
# print PIPE "8 5 \n 15.2 5";
# close(PIPE);
  open(PIPE,"| psxy -JX -R -W1.5p/0/255/0 -O -K >> $outf");
  print PIPE "16 5 \n 23.2 5";
  close(PIPE);

  open(PIPE,"| pstext -JX -R -N -O -K >> $outf");
  print PIPE "3.6 0 12 0 0 MC $time_stamp_fcst FCST\n";
  close(PIPE);

# open(PIPE,"| pstext -JX -R -N -O -K >> $outf");
# print PIPE "11.6 0 12 0 0 MC $time_stamp_p PRLM\n";
# close(PIPE);

  open(PIPE,"| pstext -JX -R -N -O >> $outf");
  print PIPE "19.6 0 12 0 0 MC $time_stamp_f FINAL\n";
  close(PIPE);

  $out_gif=$outf;
  $out_gif=~ s/ps$/gif/;

  system("convert -crop 0x0 $outf $out_gif");

  unlink $outf;

}

#
#
#

sub html_create {

my $domain;
my $fn;

foreach $domain (1..$NDOM) {

  $fn="d$domain.html";
  open(OUT,">$fn");

  print OUT "
<HTML>
<HEAD>
<TITLE>RT FDDA Surface Verification Plots Domain $domain</TITLE></HEAD>
<BODY>
";

  if($domain == 5) {
    print OUT "
<H4><P>Caution: There is only one station, which is close to grid boundary, in this domain!</P></H4>
";
  }

  print OUT "
<H2><P>2 m Temperature</P>
<IMG SRC=\"$web_images_rel/sfc/$date/${date}_t_d$domain.gif\">
<P>Mixing Ratio</P>
<IMG SRC=\"$web_images_rel/sfc/$date/${date}_q_d$domain.gif\">
<P>Relative Humidity </P>
<IMG SRC=\"$web_images_rel/sfc/$date/${date}_rh_d$domain.gif\">
<P>10 m Wind Speed</P>
<IMG SRC=\"$web_images_rel/sfc/$date/${date}_ws_d$domain.gif\">
<P>10 m Wind Direction</P>
<IMG SRC=\"$web_images_rel/sfc/$date/${date}_wd_d$domain.gif\">
<P>Sea Level Pressure</P>
<IMG SRC=\"$web_images_rel/sfc/$date/${date}_slp_d$domain.gif\">
<P>Surface Pressure</P>
<IMG SRC=\"$web_images_rel/sfc/$date/${date}_psfc_d$domain.gif\"></H2>
</BODY>
</HTML>
";
  close(OUT);
  system("rsync -e 'ssh -i $KEY' -avzC $fn $DEST_SERVER:$JOB_LOC/veri_images/sfc/$date/.");
  unlink "$fn";
}

}

#
#
#
#
#

sub old_enough {

  my ($date,$tag)=@_;
  my ($yy,$mm,$dd,$hh);
  my ($sec0,$sec1,$sec_diff,$flag);

  $yy=substr($date,0,4);
  $mm=substr($date,4,2);
  $dd=substr($date,6,2);
  $hh=substr($date,8,2);

  $sec0=date2secs($yy,$mm,$dd,$hh,0,0,0);

  $yy=substr($tag,0,4);
  $mm=substr($tag,4,2);
  $dd=substr($tag,6,2);
  $hh=substr($tag,8,2);

  $sec1=date2secs($yy,$mm,$dd,$hh,0,0,0);

  $sec_diff=$sec0-$sec1;

  if($sec_diff >= 86400*7) {
    $flag=1;
  } else {
    $flag=0;
  }

  return $flag;

}

#
#
#
sub save_upr {

  my ($date,$r_old_cycles)=@_;
  my $cycOldest;
  my $d;
  my $fn;
  my @files;
  my $count;
  my $f;
  my @split_files;

  unlink 'soundings_obs.cleanup' if(-e 'soundings_obs.cleanup');
  system("$CSH_ARCHIVE/Forecast/RT_all.obs_cleanup.pl -S -f qc_out.dat -l $PERL_ARCHIVE");
  system("$EXECUTABLE_ARCHIVE/v_rewrite_snd.exe soundings_obs.cleanup");
# The above system call generates a new soundings ASCII file, fort.61.

### Final!
# if(substr($date,-2) eq '00' || substr($date,-2) eq '12') {
    foreach $d (1..$NDOM) {
#     system("touch snd_pairs_domain${d}_cat");
#     $count=0;
#     foreach $fn (<$RUNDIR/$date/MM5_F/MMOUT_DOMAIN${d}_*>) {
#       $count++;
#       next if($count != 6);
      if($MMOUTDIR eq $RUNDIR) {
        $fn="$RUNDIR/$date/${date}_MMOUTPUT_DOMAIN${d}.${RANGE}_F";
      } else {
        $fn="$MMOUTDIR/${date}_MMOUTPUT_DOMAIN${d}.${RANGE}_F";
      }
        $output_date=date_retro($date,2);
        system("ln -sf $fn fort.71");
        system("$EXECUTABLE_ARCHIVE/v_snd_pairs.exe");
#       system("cat snd_pairs_domain${d} >> snd_pairs_domain${d}_cat");
#     }
    }

    system("$EXECUTABLE_ARCHIVE/v_merge_snd_pairs.exe snd_pairs_domain*");
    system("$EXECUTABLE_ARCHIVE/v_read_snd_pairs.exe snd_pairs_domain1");
#   The above system call generates a merged soundings ASCII file, fort.81.
    $fn="${date}_${output_date}_veri_dat_upr_${RANGE_tag}_F";
##  system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC fort.81 $WebUser\@$WebServer:$REMOTE_DIR_UPR/final/$fn");
    system("mv fort.81 $SAVE_DIR_UPR/final/$fn");
    system("rm -rf snd_pairs_domain*");

    if(-s "$SAVE_DIR_UPR/final/$fn") {
      push(@files,"$SAVE_DIR_UPR/final/$fn");
    }

    print "   Finish Final-fdda_upr of $date cycle at ",&ctime(time);
# }  # only for 00 and 12Z cycles

### Forecast!

 if($MMOUTDIR eq $RUNDIR) {
  $cycOldest = pop (@$r_old_cycles);
  $new_dir="$RUNDIR/$cycOldest";
  push (@$r_old_cycles,$cycOldest);
 } else {
  $new_dir=$MMOUTDIR;
 }
  if(-e $new_dir) {
    foreach $d (1..$NDOM) {
#      system("touch snd_pairs_domain${d}_cat");
#      $count=0;
#      foreach $fn (<$new_dir/MM5_P/MMOUT_DOMAIN${d}_*>) {
#        $count++;
         $fn="$new_dir/${cycOldest}_MMOUTPUT_DOMAIN${d}.${RANGE}_P+FCST";
#        $output_date=advance_h($date_m24,$count);
#        next if(substr($output_date,-2) != '00' &&
#                substr($output_date,-2) != '12');
#        print "output_date = $output_date\n";
         system("ln -sf $fn fort.71");
         system("$EXECUTABLE_ARCHIVE/v_snd_pairs.exe");
#        system("cat snd_pairs_domain${d} >> snd_pairs_domain${d}_cat");
#      }
    }
  }

  system("$EXECUTABLE_ARCHIVE/v_merge_snd_pairs.exe snd_pairs_domain*");
  system("$EXECUTABLE_ARCHIVE/v_read_snd_pairs.exe snd_pairs_domain1");
  $fn=$cycOldest . "_veri_dat_upr_${RANGE_tag}_FCST";
  @split_files=split_upr('fort.81',$cycOldest);
  unlink 'fort.81';
  foreach $f (@split_files) {
##  system("rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC $f $WebUser\@$WebServer:$REMOTE_DIR_UPR/fcst/.");
    system("mv $f $SAVE_DIR_UPR/fcst/.");
  }
  system("rm -rf snd_pairs_domain*");
  system("rm -rf fort.*");

  foreach $f (@split_files) {
    if(-s "$SAVE_DIR_UPR/fcst/$f") {
      push(@files,"$SAVE_DIR_UPR/fcst/$f");
    }
  }

  print "   Finish forecast-fdda_upr of $cycOldest  at ",&ctime(time);

  return @files;

}

sub stats_upr {

#
# This subroutine deals with nested data structure as outlined below:
#
# %snds = ( 'snd' => [
#                      { 'date' => ,
#                        'lat'  => ,
#                        'lon'  => ,
#                        'id'   => ,
#                        'level'=> [
#                                    { 'p'     => ,
#                                      'tm'    => ,
#                                      'to'    => ,
#                                      'qc_t'  => ,
#                                       .
#                                       .
#                                       .
#
#                                    },
#
#                                    { 'p'     => ,
#                                      'tm'    => ,
#                                      'to'    => ,
#                                      'qc_t'  => ,
#                                       .
#                                       .
#                                       .
#
#                                    },
#                                    .
#                                    .
#                                    .
#                                  ]
#                      }
#                    ],
#
#                    [
#                      { 'date' =>,
#                          .
#                          .
#                          .
#                      },
#
#                      .
#                      .
#                      .
#                    ],
#                    .
#                    .
#                    .
#         )
#
#
#

  my ($file,$d)=@_;
  my %snds;
  my ($date,$lat,$lon,$id);
  my $line;
  my $nsnd,$l_index;   ## indices for soundings, levels
  my $missing=-8888;
  my (@sum_t,@sum_ta,@sum_tr,@no_t,@bias_t,@rmse_t,@mae_t);
  my (@sum_q,@sum_qa,@sum_qr,@no_q,@bias_q,@rmse_q,@mae_q);
  my (@sum_rh,@sum_rha,@sum_rhr,@no_rh,@bias_rh,@rmse_rh,@mae_rh);
  my (@sum_ws,@sum_wsa,@sum_wsr,@no_ws,@bias_ws,@rmse_ws,@mae_ws);
  my (@sum_wd,@sum_wda,@sum_wdr,@no_wd,@bias_wd,@rmse_wd,@mae_wd);

# foreach $file ($file_final,$file_prelim,$file_fcst) {
    if( -e "$file") {
      $file=~ /(\d+\w+)$/;
      $outf=$1;
      $outf=~ s/upr/upr_stats_d$d/;
      $outf=~ s/_dat//;
      open(IN,"$file");
      open(OUT,"> $outf");

      %snds=();

      while ($line=<IN>) {
        chomp $line;
        $nsnd=int(($.-1)/41);
        $l_index=($.-1)%41;
        if($l_index == 0) {
          ($date,$lat,$lon,$id)=split " ",$line;
           $snds{snd}[$nsnd]{date}=$date;
           $snds{snd}[$nsnd]{lat}=$lat;
           $snds{snd}[$nsnd]{lon}=$lon;
           $snds{snd}[$nsnd]{id}=$id;
        } else {
          ($p,$tm,$to,$qc_t,$qm,$qo,$qc_q,$rhm,$rho,$qc_rh,$wsm,$wso,$qc_ws,
           $wdm,$wdo,$qc_wd)=split " ",$line;
           $snds{snd}[$nsnd]{level}[$l_index]{p}=$p;
           $snds{snd}[$nsnd]{level}[$l_index]{tm}=$tm;
           $snds{snd}[$nsnd]{level}[$l_index]{to}=$to;
           $snds{snd}[$nsnd]{level}[$l_index]{qc_t}=$qc_t;
           $snds{snd}[$nsnd]{level}[$l_index]{qm}=$qm;
           $snds{snd}[$nsnd]{level}[$l_index]{qo}=$qo;
           $snds{snd}[$nsnd]{level}[$l_index]{qc_q}=$qc_q;
           $snds{snd}[$nsnd]{level}[$l_index]{rhm}=$rhm;
           $snds{snd}[$nsnd]{level}[$l_index]{rho}=$rho;
           $snds{snd}[$nsnd]{level}[$l_index]{qc_rh}=$qc_rh;
           $snds{snd}[$nsnd]{level}[$l_index]{wsm}=$wsm;
           $snds{snd}[$nsnd]{level}[$l_index]{wso}=$wso;
           $snds{snd}[$nsnd]{level}[$l_index]{qc_ws}=$qc_ws;
           $snds{snd}[$nsnd]{level}[$l_index]{wdm}=$wdm;
           $snds{snd}[$nsnd]{level}[$l_index]{wdo}=$wdo;
           $snds{snd}[$nsnd]{level}[$l_index]{qc_wd}=$qc_wd;
        }
      }

      close(IN);

      for $l (1..$l_index) {

          $sum_t[$l]=0;
          $sum_ta[$l]=0;
          $sum_tr[$l]=0;
          $no_t[$l]=0;

           $sum_q[$l]=0;
          $sum_qa[$l]=0;
          $sum_qr[$l]=0;
          $no_q[$l]=0;

          $sum_rh[$l]=0;
          $sum_rha[$l]=0;
          $sum_rhr[$l]=0;
          $no_rh[$l]=0;

          $sum_ws[$l]=0;
          $sum_wsa[$l]=0;
          $sum_wsr[$l]=0;
          $no_ws[$l]=0;

          $sum_wd[$l]=0;
          $sum_wda[$l]=0;
          $sum_wdr[$l]=0;
          $no_wd[$l]=0;

      }

      for $i (0..$nsnd) {

      next if($snds{snd}[$i]{id} != $d);

        for $l (1..$l_index) {
          if(($snds{snd}[$i]{level}[$l]{tm} > $missing) &&
             ($snds{snd}[$i]{level}[$l]{to} > $missing) &&
             ($snds{snd}[$i]{level}[$l]{qc_t}  > $QC_CUT)) {
            $diff=$snds{snd}[$i]{level}[$l]{tm}-$snds{snd}[$i]{level}[$l]{to};
            $sum_t[$l] += $diff;
            $sum_ta[$l] += abs($diff);
            $sum_tr[$l] += $diff**2;
            $no_t[$l] += 1;
          }
          if(($snds{snd}[$i]{level}[$l]{qm} > $missing) &&
             ($snds{snd}[$i]{level}[$l]{qo} > $missing) &&
             ($snds{snd}[$i]{level}[$l]{qc_q}  > $QC_CUT)) {
            $diff=$snds{snd}[$i]{level}[$l]{qm}-$snds{snd}[$i]{level}[$l]{qo};
            $sum_q[$l] += $diff;
            $sum_qa[$l] += abs($diff);
            $sum_qr[$l] += $diff**2;
            $no_q[$l] += 1;
          }
          if(($snds{snd}[$i]{level}[$l]{rhm} > $missing) &&
             ($snds{snd}[$i]{level}[$l]{rho} > $missing) &&
             ($snds{snd}[$i]{level}[$l]{qc_rh}  > $QC_CUT)) {
            $diff=$snds{snd}[$i]{level}[$l]{rhm}-$snds{snd}[$i]{level}[$l]{rho};            $sum_rh[$l] += $diff;
            $sum_rha[$l] += abs($diff);
            $sum_rhr[$l] += $diff**2;
            $no_rh[$l] += 1;
          }
          if(($snds{snd}[$i]{level}[$l]{wsm} > $missing) &&
             ($snds{snd}[$i]{level}[$l]{wso} > $missing) &&
             ($snds{snd}[$i]{level}[$l]{qc_ws}  > $QC_CUT)) {
            $diff=$snds{snd}[$i]{level}[$l]{wsm}-$snds{snd}[$i]{level}[$l]{wso};            $sum_ws[$l] += $diff;
            $sum_wsa[$l] += abs($diff);
            $sum_wsr[$l] += $diff**2;
            $no_ws[$l] += 1;
          }
          if(($snds{snd}[$i]{level}[$l]{wdm} > $missing) &&
             ($snds{snd}[$i]{level}[$l]{wdo} > $missing) &&
             ($snds{snd}[$i]{level}[$l]{qc_wd}  > $QC_CUT)) {
            $diff=$snds{snd}[$i]{level}[$l]{wdm}-$snds{snd}[$i]{level}[$l]{wdo};            $diff -= 360 if($diff > 180);
            $diff += 360 if($diff < -180);
            $sum_wd[$l] += $diff;
            $sum_wda[$l] += abs($diff);
            $sum_wdr[$l] += $diff**2;
            $no_wd[$l] += 1;
          }
        }
      }

#    stats for each level

     for $l (1..$l_index) {

         if($no_t[$l] > 0) {
           $bias_t[$l]=$sum_t[$l]/$no_t[$l];
           $mae_t[$l]=$sum_ta[$l]/$no_t[$l];
           $rmse_t[$l]=sqrt($sum_tr[$l]/$no_t[$l]);
         } else {
           $bias_t[$l]=-99;
           $rmse_t[$l]=-99;
           $mae_t[$l]=-99;
         }

         if($no_q[$l] > 0) {
           $bias_q[$l]=$sum_q[$l]/$no_q[$l];
           $mae_q[$l]=$sum_qa[$l]/$no_q[$l];
           $rmse_q[$l]=sqrt($sum_qr[$l]/$no_q[$l]);
         } else {
           $bias_q[$l]=-99;
           $rmse_q[$l]=-99;
           $mae_q[$l]=-99;
         }

         if($no_rh[$l] > 0) {
           $bias_rh[$l]=$sum_rh[$l]/$no_rh[$l];
           $mae_rh[$l]=$sum_rha[$l]/$no_rh[$l];
           $rmse_rh[$l]=sqrt($sum_rhr[$l]/$no_rh[$l]);
         } else {
           $bias_rh[$l]=-99;
           $rmse_rh[$l]=-99;
           $mae_rh[$l]=-99;
         }

         if($no_ws[$l] > 0) {
           $bias_ws[$l]=$sum_ws[$l]/$no_ws[$l];
           $mae_ws[$l]=$sum_wsa[$l]/$no_ws[$l];
           $rmse_ws[$l]=sqrt($sum_wsr[$l]/$no_ws[$l]);
         } else {
           $bias_ws[$l]=-99;
           $rmse_ws[$l]=-99;
           $mae_ws[$l]=-99;
         }

         if($no_wd[$l] > 0) {
           $bias_wd[$l]=$sum_wd[$l]/$no_wd[$l];
           $mae_wd[$l]=$sum_wda[$l]/$no_wd[$l];
           $rmse_wd[$l]=sqrt($sum_wdr[$l]/$no_wd[$l]);
         } else {
           $bias_wd[$l]=-99;
           $rmse_wd[$l]=-99;
           $mae_wd[$l]=-99;
         }

         if(($snds{snd}[0]{level}[$l]{p} == 1000) ||
            ($snds{snd}[0]{level}[$l]{p} == 925) ||
            ($snds{snd}[0]{level}[$l]{p} == 850) ||
            ($snds{snd}[0]{level}[$l]{p} == 700) ||
            ($snds{snd}[0]{level}[$l]{p} == 500) ||
            ($snds{snd}[0]{level}[$l]{p} == 400) ||
            ($snds{snd}[0]{level}[$l]{p} == 300) ||
            ($snds{snd}[0]{level}[$l]{p} == 250) ||
            ($snds{snd}[0]{level}[$l]{p} == 200) ||
            ($snds{snd}[0]{level}[$l]{p} == 150) ||
            ($snds{snd}[0]{level}[$l]{p} == 100)) {

           printf OUT "%4d%6.1f%6.1f%6.1f%4d%6.1f%6.1f%6.1f%4d%6.1f%6.1f%6.1f%4d%6.1f%6.1f%6.1f%4d%7.1f%6.1f%6.1f%4d\n",
                  $snds{snd}[0]{level}[$l]{p},
                  $bias_t[$l],$rmse_t[$l],$mae_t[$l],$no_t[$l],
                  $bias_q[$l],$rmse_q[$l],$mae_q[$l],$no_q[$l],
                  $bias_rh[$l],$rmse_rh[$l],$mae_rh[$l],$no_rh[$l],
                  $bias_ws[$l],$rmse_ws[$l],$mae_ws[$l],$no_ws[$l],
                  $bias_wd[$l],$rmse_wd[$l],$mae_wd[$l],$no_wd[$l];
         }
     }

     close(OUT);

    }
# }

  return $outf;
}
#
#
#
sub html_table_upr {

  my $file=$_[0];
  my $x,$y,$i,$n;
  my @row,@table;
  my $time_tag,$cycle,$domain,$type,$type_ext,$color;
  my $htmlf;

# foreach $file (@files) {

  $file=~ /(\d+)_(\d+)_veri_upr_stats_d(\d+)_${RANGE_tag}_(\w+)/;
  $cycle=$1;
  $snd_time=$2;
  $domain=$3;
  $type=$4;
  $htmlf="${cycle}_${snd_time}_d${domain}.html";

  open(OUT,">$htmlf");
  print OUT "<HTML>\n";
  print OUT "<BODY>\n";


  if($type eq 'F') {
    $type_ext='FINAL';
    $color='Green';
  } elsif($type eq 'P') {
    $type_ext='PRELIMINARY';
    $color='Blue';
  } else {
    $type_ext='FORECAST';
    $color='Red';
  }

  open(IN,"$file");

  while (<IN>) {
    $y=$.;
    chomp;
    @row=split;
    $x=scalar @row;
    foreach $i (0..$x-1) {
      $table[$y-1][$i]=$row[$i];
    }
  }

  close(IN);

# print OUT "Content-type: text/html\n\n";
  print OUT "<H4>Date/Time: $snd_time Cycle: $cycle <FONT COLOR=\'$color\'>$type_ext</FONT> Domain $domain</H4>\n";
  print OUT "<TABLE BORDER WIDTH=70% CELLSPACING=0 CELLPADDING=5 COLS=16>\n";
  print OUT "<TR>\n";
  print OUT "<TH ROWSPAN=2>Pressure (hPa)</TH>\n";
  print OUT "<TH COLSPAN=4>Temperature (K)</TH>\n";
  print OUT "<TH COLSPAN=4>Mixing Ratio (g/kg)</TH>\n";
  print OUT "<TH COLSPAN=4>Relative Humidity (%)</TH>\n";
  print OUT "<TH COLSPAN=4>Wind Speed (m/s)</TH>\n";
  print OUT "<TH COLSPAN=4>Wind Direction (deg)</TH>\n";
  print OUT "</TR>\n";

  print OUT "<TR>\n";
  foreach $i (1..5) {
    print OUT "<TH>Bias</TH>\n";
    print OUT "<TH>RMSE</TH>\n";
    print OUT "<TH>MAE</TH>\n";
    print OUT "<TH>Count</TH>\n";
  }
  print OUT "</TR>\n";

  foreach $i (0..$y-1) {
    print OUT "<TR ALIGN=CENTER>\n";
    foreach $n (0..$x-1) {
      print OUT "<TD>$table[$i][$n]</TD>\n";
    }
    print OUT "</TR>\n";
  }

  print OUT "</TABLE>\n";

# }

  print OUT "</BODY></HTML>\n";

  close(OUT);

  return $htmlf;
}
#
#
#
sub split_upr {

  my ($file_orig,$cycle_tag)=@_;
  my @files;
  my $date;
  my @f;

  open(IN,"$file_orig");

  while (<IN>) {

    if($.%41 == 1) {
      ($date,@f)=split;
      $fn="${cycle_tag}_${date}_veri_dat_upr_${RANGE_tag}_FCST";
      if(! -e $fn) {
        open(OUT,">$fn");
        push(@files,$fn);
      } else {
        open(OUT,">>$fn");
      }
      print OUT $_;
    } elsif($.%41 == 0) {
      print OUT $_;
      close(OUT);
    } else {
      print OUT $_;
    }

  }

  close(IN);
  return @files;
}

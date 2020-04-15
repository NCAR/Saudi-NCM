#!/usr/bin/perl
use strict;
no strict 'refs';
use vars qw($opt_c $opt_j $opt_d $opt_b);
use Getopt::Std;

my ($wrf_sfc_example,$wrf_upr_example,$wrf_sfc_suffix,$wrf_upr_suffix);
my $ref_suffix;
my $config_file;
my ($lstation,$llevel,$lfcst);
my ($sid,$lat,$lon);
my ($level,$fcst,$weight);
my (@levels,@l_weight,@leads,@f_weight);
my ($wrf,$ref);
my ($fcst_index,$t_bias_wrf,$t_rmse_wrf,$t_mae_wrf,$no_t_wrf,
    $rh_bias_wrf,$rh_rmse_wrf,$rh_mae_wrf,$no_rh_wrf,
    $ws_bias_wrf,$ws_rmse_wrf,$ws_mae_wrf,$no_ws_wrf,
    $wd_bias_wrf,$wd_rmse_wrf,$wd_mae_wrf,$no_wd_wrf,
    $slp_bias_wrf,$slp_rmse_wrf,$slp_mae_wrf,$no_slp_wrf,
    $psfc_bias_wrf,$psfc_rmse_wrf,$psfc_mae_wrf,$no_psfc_wrf,
    $q_bias_wrf,$q_rmse_wrf,$q_mae_wrf,$no_q_wrf,
    $gh_bias_wrf,$gh_rmse_wrf,$gh_mae_wrf,$no_gh_wrf);

my ($fcst_index,$t_bias_ref,$t_rmse_ref,$t_mae_ref,$no_t_ref,
    $rh_bias_ref,$rh_rmse_ref,$rh_mae_ref,$no_rh_ref,
    $ws_bias_ref,$ws_rmse_ref,$ws_mae_ref,$no_ws_ref,
    $wd_bias_ref,$wd_rmse_ref,$wd_mae_ref,$no_wd_ref,
    $slp_bias_ref,$slp_rmse_ref,$slp_mae_ref,$no_slp_ref,
    $psfc_bias_ref,$psfc_rmse_ref,$psfc_mae_ref,$no_psfc_ref,
    $q_bias_ref,$q_rmse_ref,$q_mae_ref,$no_q_ref,
    $gh_bias_ref,$gh_rmse_ref,$gh_mae_ref,$no_gh_ref);

my ($ifcst,$ilevel);
my ($wrf_file,$ref_file);
my $prs;
my $vmissing = -99;
my $ss = 0;
my $total_weight = 0;
my ($s,$n);

our $GSJOBDIR;
my $days;
our $MM5HOME;
my ($begin,$end);
our $RUNDIR;
my ($WRF_VERI_DAT,$REF_VERI_DAT);
my $GO_DIR;
my ($file,$cycle,$valid);
my $hour;
our $EXECUTABLE_ARCHIVE;
our $DEBUG;
our $DEST_SERVER;
our $JOB_LOC;
our $KEY;
my $count;
my @counts;
my $no_go_index_ukmo = 1; # customized for IAF's UKMO
my $no_go_index_dwd = 1;  # customized for IAF's DWD
our $GMT_BIN;

getopts('c:j:d:b:');

if ($opt_c) {
   $config_file = $opt_c;
} else {
   print "CRITICAL: Usage: $0 -c <config_file> -j <GSJOBDIR> -d <stats period in days> [-b <begin time in YYYYMMDDHH>]\n";
   exit;
}

if ($opt_j) {
   $GSJOBDIR = $opt_j;
   $ENV{GSJOBDIR} = $GSJOBDIR;
   $GSJOBDIR =~ /(\w+)$/;
   $ENV{GSJOBID} = $1;
} else {
   print "CRITICAL: Usage: $0 -c <config_file> -j <GSJOBDIR> -d <stats period in days> [-b <begin time in YYYYMMDDHH>]\n";
   exit;
}

if ($opt_d) {
   $days = $opt_d;
} else {
   print "CRITICAL: Usage: $0 -c <config_file> -j <GSJOBDIR> -d <stats period in days> [-b <begin time in YYYYMMDDHH>]\n";
   exit;
}

### GMT bin path

if (! $GMT_BIN) {
   $GMT_BIN = '/opt/gmt/bin';
}

$ENV{PATH} = "${GMT_BIN}:$ENV{PATH}";

###

if (-e "$GSJOBDIR/scripts/env_vars.csh") {
   $MM5HOME=`grep 'setenv MM5HOME' $GSJOBDIR/scripts/env_vars.csh | awk '{print \$3}'| sed s/\\"//g`;
   $MM5HOME =~ s/\$\{LOGNAME\}/$ENV{LOGNAME}/ if ($MM5HOME =~ /LOGNAME/);
   $MM5HOME =~ s/\$\{HOME\}/\/home\/$ENV{LOGNAME}/ if ($MM5HOME =~ /HOME/);
   chomp($MM5HOME);
   $ENV{MM5HOME} = $MM5HOME;
} else {
   print "CRITICAL: $GSJOBDIR/scripts/env_vars.csh does not exist!\n";
   exit;
}

if (-e "$GSJOBDIR/flexinput.pl") {
   require "$GSJOBDIR/flexinput.pl";
} else {
   print "CRITICAL: $GSJOBDIR/flexinput.pl does not exist!\n";
   exit;
}

if (-e "$GSJOBDIR/postprocinput.pl") {
   require "$GSJOBDIR/postprocinput.pl";
} else {
   print "CRITICAL: $GSJOBDIR/postprocinput.pl does not exist!\n";
   exit;
}

if ($opt_b) {
   $begin = $opt_b;
   $end = advance_h($begin,24*$days);
} else {
   $end = `date --date="1 day ago" +%Y%m%d`;
   chomp($end);
   $end .= '00';
   $begin = advance_h($end,-24*$days);
}

print "IFNO: The following 5 directories need to be defined and existing:\n";
print "INFO: GSJOBDIR = $GSJOBDIR\n";
print "INFO: RUNDIR = $RUNDIR\n";
print "INFO: MM5HOME = $MM5HOME\n";
print "INFO: EXECUTABLE_ARCHIVE = $EXECUTABLE_ARCHIVE\n";
print "INFO: JOB_LOC = $JOB_LOC\n";

my $date =`date +%Y-%m-%d`;
chomp($date);

my $WRF_VERI_DAT="$RUNDIR/veri_dat";
my $REF_VERI_DAT="$RUNDIR/veri_dat_interm";
my $GO_DIR = "$RUNDIR/go_index";

system("mkdir -p $GO_DIR");

chdir "$GO_DIR";

### REF SURFACE STATS

open(REF_SFC_LIST,">ref_sfc.list");

foreach $file (<$REF_VERI_DAT/sfc/*>) {
  $file =~ /(\d{10})/;
  $cycle = $1;
  if ($cycle >= $begin && $cycle < $end && -s $file) {
     print REF_SFC_LIST "$file\n";
  }
}
close(REF_SFC_LIST);

system("$EXECUTABLE_ARCHIVE/stats_calc.exe -conf $config_file -list ref_sfc.list -type sfc -domain 1 -model ref");

### REF UPPER-AIR STATS

open(REF_UPR_LIST,">ref_upr.list");

foreach $file (<$REF_VERI_DAT/upr/*>) {
  $file =~ /(\d{10})_(\d{10})/;
  $valid = $2;
  my $valid_hour = substr($valid,8,2);
  next if($valid_hour eq '06' || $valid_hour eq '18');
  if ($valid >= $begin && $valid < $end && -s $file) {
     print REF_UPR_LIST "$file\n";
  }
}

system("$EXECUTABLE_ARCHIVE/stats_calc.exe -conf $config_file -list ref_upr.list -type upr -domain 1 -model ref");

### WRF SUFACE STATS

open(REF_SFC_LIST,"ref_sfc.list");
open(WRF_SFC_LIST,">wrf_sfc.list");

$wrf_sfc_example = `ls -1 $WRF_VERI_DAT/sfc/fcst | tail -1`;
chomp($wrf_sfc_example);
$wrf_sfc_example =~ /veri_dat_(\S+)$/;
$wrf_sfc_suffix = $1;

while (<REF_SFC_LIST>) {
  s/${REF_VERI_DAT}\/sfc/${WRF_VERI_DAT}\/sfc\/fcst/;
  $_ =~ /([a-zA-Z0-9]*)$/;
  $ref_suffix = $1;
  s/$ref_suffix/$wrf_sfc_suffix/;
  print WRF_SFC_LIST $_;
}
close(WRF_SFC_LIST);

system("$EXECUTABLE_ARCHIVE/stats_calc.exe -conf $config_file -list wrf_sfc.list -type sfc -domain 1 -model wrf");

### WRF UPPER-AIR STATS

open(REF_UPR_LIST,"ref_upr.list");
open(WRF_UPR_LIST,">wrf_upr.list");

$wrf_upr_example = `ls -1 $WRF_VERI_DAT/upr/fcst | tail -1`;
chomp($wrf_upr_example);
$wrf_upr_example =~ /veri_dat_upr_(\S+)$/;
$wrf_upr_suffix = $1;

while (<REF_UPR_LIST>) {
  s/${REF_VERI_DAT}\/upr/${WRF_VERI_DAT}\/upr\/fcst/;
  $_ =~ /([a-zA-Z0-9]*)$/;
  $ref_suffix = $1;
  s/$ref_suffix/$wrf_upr_suffix/;
  print WRF_UPR_LIST $_;
}
close(WRF_UPR_LIST);

system("$EXECUTABLE_ARCHIVE/stats_calc.exe -conf $config_file -list wrf_upr.list -type upr -domain 1 -model wrf");

### END OF STATS CALCULATION

### BEGIN GO-INDEX CALCULATION

open(CONFIG,"$config_file");

while (<CONFIG>) {
   chomp;

   next if(length == 0);

   if (/STATIONS/) {
      $lstation = 1;
      $llevel = 0;
      $lfcst = 0;
      next;
   } elsif (/LEVELS/) {
      $lstation = 0;
      $llevel = 1;
      $lfcst = 0;
      next;
   } elsif (/FCST/) {
      $lstation = 0;
      $llevel = 0;
      $lfcst = 1;
      next;
   } elsif (/UPR/) {
      $lstation = 0;
      $llevel = 0;
      $lfcst = 0;
      next;
   } elsif (/BIN/) {
      $lstation = 0;
      $llevel = 0;
      $lfcst = 0;
      next;
   }

   if($lstation) {
     ($sid,$lat,$lon) = split;
   } elsif($llevel) {
     ($level,$weight) = split;
     push(@levels,$level);
     push(@l_weight,$weight);
   } elsif($lfcst) {
     ($fcst,$weight) = split;
     push(@leads,$fcst);
     push(@f_weight,$weight);
   }
}

close(CONFIG);

print "@levels\n";
print "@l_weight\n";
print "@leads\n";
print "@f_weight\n";

open(SFC_WRF,"sfc_stats_wrf.txt");
open(SFC_REF,"sfc_stats_ref.txt");

while (($wrf=<SFC_WRF>) && ($ref=<SFC_REF>)) {
   ($fcst_index,$t_bias_wrf,$t_rmse_wrf,$t_mae_wrf,$no_t_wrf,
    $rh_bias_wrf,$rh_rmse_wrf,$rh_mae_wrf,$no_rh_wrf,
    $ws_bias_wrf,$ws_rmse_wrf,$ws_mae_wrf,$no_ws_wrf,
    $wd_bias_wrf,$wd_rmse_wrf,$wd_mae_wrf,$no_wd_wrf,
    $slp_bias_wrf,$slp_rmse_wrf,$slp_mae_wrf,$no_slp_wrf,
    $psfc_bias_wrf,$psfc_rmse_wrf,$psfc_mae_wrf,$no_psfc_wrf,
    $q_bias_wrf,$q_rmse_wrf,$q_mae_wrf,$no_q_wrf) = split " ",$wrf;
    
   ($fcst_index,$t_bias_ref,$t_rmse_ref,$t_mae_ref,$no_t_ref,
    $rh_bias_ref,$rh_rmse_ref,$rh_mae_ref,$no_rh_ref,
    $ws_bias_ref,$ws_rmse_ref,$ws_mae_ref,$no_ws_ref,
    $wd_bias_ref,$wd_rmse_ref,$wd_mae_ref,$no_wd_ref,
    $slp_bias_ref,$slp_rmse_ref,$slp_mae_ref,$no_slp_ref,
    $psfc_bias_ref,$psfc_rmse_ref,$psfc_mae_ref,$no_psfc_ref,
    $q_bias_ref,$q_rmse_ref,$q_mae_ref,$no_q_ref) = split " ",$ref;

   if ($t_rmse_wrf != $vmissing && $t_rmse_ref != $vmissing) {
     #$ss += (1 - $t_rmse_wrf**2/$t_rmse_ref**2)*
      $ss += (1 - $t_rmse_wrf**2/(0.5*($t_rmse_wrf**2+$t_rmse_ref**2)))*
             $f_weight[$fcst_index-1]*$l_weight[0];
      $total_weight += $f_weight[$fcst_index-1]*$l_weight[0];
   } 
    
   if ($ws_rmse_wrf != $vmissing && $ws_rmse_ref != $vmissing) {
     #$ss += (1 - $ws_rmse_wrf**2/$ws_rmse_ref**2)*
      $ss += (1 - $ws_rmse_wrf**2/(0.5*($ws_rmse_wrf**2+$ws_rmse_ref**2)))*
             $f_weight[$fcst_index-1]*$l_weight[0];
      $total_weight += $f_weight[$fcst_index-1]*$l_weight[0];
   } 
    
   if ($wd_rmse_wrf != $vmissing && $wd_rmse_ref != $vmissing) {
     #$ss += (1 - $wd_rmse_wrf**2/$wd_rmse_ref**2)*
      $ss += (1 - $wd_rmse_wrf**2/(0.5*($wd_rmse_wrf**2+$wd_rmse_ref**2)))*
             $f_weight[$fcst_index-1]*$l_weight[0];
      $total_weight += $f_weight[$fcst_index-1]*$l_weight[0];
   } 

   if ($rh_rmse_wrf != $vmissing && $rh_rmse_ref != $vmissing) {
     #$ss += (1 - $rh_rmse_wrf**2/$rh_rmse_ref**2)*
      $ss += (1 - $rh_rmse_wrf**2/(0.5*($rh_rmse_wrf**2+$rh_rmse_ref**2)))*
             $f_weight[$fcst_index-1]*$l_weight[0];
      $total_weight += $f_weight[$fcst_index-1]*$l_weight[0];

      $no_go_index_ukmo = 0; # customized for IAF's UKMO
   } 
}

close(SFC_WRF);
close(SFC_REF);

$ifcst = 0;
foreach $fcst (@leads) {

  $ifcst += 1;

  $wrf_file = sprintf("upr_stats_wrf_%02dhrs.txt",$fcst);
  $ref_file = sprintf("upr_stats_ref_%02dhrs.txt",$fcst);

  open(UPR_WRF,"$wrf_file");
  open(UPR_REF,"$ref_file");

  $ilevel = 0;
  while (($wrf=<UPR_WRF>) && ($ref=<UPR_REF>)) {

     $ilevel += 1;

     ($prs,$t_bias_wrf,$t_rmse_wrf,$t_mae_wrf,$no_t_wrf,
      $rh_bias_wrf,$rh_rmse_wrf,$rh_mae_wrf,$no_rh_wrf,
      $ws_bias_wrf,$ws_rmse_wrf,$ws_mae_wrf,$no_ws_wrf,
      $wd_bias_wrf,$wd_rmse_wrf,$wd_mae_wrf,$no_wd_wrf,
      $gh_bias_wrf,$gh_rmse_wrf,$gh_mae_wrf,$no_gh_wrf) = split " ",$wrf;

     ($prs,$t_bias_ref,$t_rmse_ref,$t_mae_ref,$no_t_ref,
      $rh_bias_ref,$rh_rmse_ref,$rh_mae_ref,$no_rh_ref,
      $ws_bias_ref,$ws_rmse_ref,$ws_mae_ref,$no_ws_ref,
      $wd_bias_ref,$wd_rmse_ref,$wd_mae_ref,$no_wd_ref,
      $gh_bias_ref,$gh_rmse_ref,$gh_mae_ref,$no_gh_ref) = split " ",$ref;

     if ($t_rmse_wrf != $vmissing && $t_rmse_ref != $vmissing) {
       #$ss += (1 - $t_rmse_wrf**2/$t_rmse_ref**2)*
        $ss += (1 - $t_rmse_wrf**2/(0.5*($t_rmse_wrf**2+$t_rmse_ref**2)))*
               $f_weight[$ifcst-1]*$l_weight[$ilevel-1];
        $total_weight += $f_weight[$ifcst-1]*$l_weight[$ilevel-1];
     }
      
     if ($rh_rmse_wrf != $vmissing && $rh_rmse_ref != $vmissing) {
       #$ss += (1 - $rh_rmse_wrf**2/$rh_rmse_ref**2)*
        $ss += (1 - $rh_rmse_wrf**2/(0.5*($rh_rmse_wrf**2+$rh_rmse_ref**2)))*
               $f_weight[$ifcst-1]*$l_weight[$ilevel-1];
        $total_weight += $f_weight[$ifcst-1]*$l_weight[$ilevel-1];
     }
      
     if ($ws_rmse_wrf != $vmissing && $ws_rmse_ref != $vmissing) {
       #$ss += (1 - $ws_rmse_wrf**2/$ws_rmse_ref**2)*
        $ss += (1 - $ws_rmse_wrf**2/(0.5*($ws_rmse_wrf**2+$ws_rmse_ref**2)))*
               $f_weight[$ifcst-1]*$l_weight[$ilevel-1];
        $total_weight += $f_weight[$ifcst-1]*$l_weight[$ilevel-1];
     }
      
     if ($wd_rmse_wrf != $vmissing && $wd_rmse_ref != $vmissing) {
       #$ss += (1 - $wd_rmse_wrf**2/$wd_rmse_ref**2)*
        $ss += (1 - $wd_rmse_wrf**2/(0.5*($wd_rmse_wrf**2+$wd_rmse_ref**2)))*
               $f_weight[$ifcst-1]*$l_weight[$ilevel-1];
        $total_weight += $f_weight[$ifcst-1]*$l_weight[$ilevel-1];
     }
      
     if ($gh_rmse_wrf != $vmissing && $gh_rmse_ref != $vmissing) {
       #$ss += (1 - $gh_rmse_wrf**2/$gh_rmse_ref**2)*
        $ss += (1 - $gh_rmse_wrf**2/(0.5*($gh_rmse_wrf**2+$gh_rmse_ref**2)))*
               $f_weight[$ifcst-1]*$l_weight[$ilevel-1];
        $total_weight += $f_weight[$ifcst-1]*$l_weight[$ilevel-1];

        $no_go_index_dwd = 0; # customized for IAF's DWD
     }
      
  }

  close(UPR_WRF);
  close(UPR_REF);

}

if ($no_go_index_ukmo || $no_go_index_dwd) { # customized for IAF's UKMO and DWD
   $n = $vmissing;
} else {
   $s = $ss/$total_weight;
   $n = sqrt(1/(1-$s));
}

open(GO_INDEX,">>go_index.txt");

print GO_INDEX "${date}T $n\n";

close(GO_INDEX);

# list date entries in descending order

system("sort -r go_index.txt > go_index.tmp");
system("mv go_index.tmp go_index.txt");

my $date_beg=`date --date="30 days ago" +%Y-%m-%d`;
chomp($date_beg);

# plot monthly go-index time series

system("gmtset TIME_FORMAT_PRIMARY abbreviated CHAR_ENCODING ISOLatin1+");
system("gmtset PLOT_DATE_FORMAT o-dd");

system("psxy go_index.txt -JX5/2 -R${date_beg}T/${date}T/0.5/1.5 -Bf1da1U:Date:/f0.1a0.5:GO-Index:WSen -W1p/255/0/0 -X1.5 -Y6 -K > go_index.ps");
system("/bin/echo -e '${date_beg}T 1\n${date}T 1' | psxy -JX -R -W1pt -O >> go_index.ps");
system("convert -trim +repage -flatten -density 288 -geometry 25% go_index.ps go_index.gif");

#

$date_beg=`date --date="a year ago" +%Y-%m-%d`;
chomp($date_beg);

# plot yearly go-index time series

system("gmtset PLOT_DATE_FORMAT o");

system("psxy go_index.txt -JX5/2 -R${date_beg}T/${date}T/0.5/1.5 -Bpa3Of1o:Date:/f0.1a0.5:GO-Index:WSen -Bsf1u/ -W1p/255/0/0 -X1.5 -Y6 -K > go_index_year.ps");
system("/bin/echo -e '${date_beg}T 1\n${date}T 1' | psxy -JX -R -W1pt -O >> go_index_year.ps");
system("convert -trim +repage -flatten -density 288 -geometry 25% go_index_year.ps go_index_year.gif");

&GI_components();

&GI_log();

## mv/copy/rsync to web produdt directory

if ($DEST_SERVER =~ /localhost/) {
   system("mkdir -p $JOB_LOC/go_index");
   system("mv go_index*.gif $JOB_LOC/go_index/.");
   system("cp go_index.txt $JOB_LOC/go_index/.");
   system("mv *.html $JOB_LOC/go_index/.");
   system("mv *.json $JOB_LOC/go_index/.");
} else {
   system("rsync -e 'ssh -i $KEY' -avzC go_index.gif $DEST_SERVER:$JOB_LOC/go_index/.");
   system("rsync -e 'ssh -i $KEY' -avzC go_index_year.gif $DEST_SERVER:$JOB_LOC/go_index/.");
   system("rsync -e 'ssh -i $KEY' -avzC go_index.txt $DEST_SERVER:$JOB_LOC/go_index/.");
   system("rsync -e 'ssh -i $KEY' -avzC GI_components_${date}.html $DEST_SERVER:$JOB_LOC/go_index/.");
   system("rsync -e 'ssh -i $KEY' -avzC GI_log_${date}.json $DEST_SERVER:$JOB_LOC/go_index/.");
}

if ($DEBUG >= 10) {
  print "Keeping temporary files!\n";
} else {
  print "Cleaning up temporary files...\n";
  system("rm -f *.list");
  system("rm -f *stats*.txt *.ps");
}

exit;

sub advance_h {

  my %mon_days = (1,31,2,28,3,31,4,30,5,31,6,30,7,31,8,31,9,30,10,31,11,30,12,31);
  (my $s_date, my $advan_hh) = @_ ;

  my $yy = substr($s_date,0,4);
  my $mm = substr($s_date,4,2);
  my $dd = substr($s_date,6,2);
  my $hh = substr($s_date,8,2);

  my $feb = 2;
  $mon_days{$feb} = 29 if ($yy%4 == 0 && ($yy%400 == 0 || $yy%100 != 0));

  print "hh: $hh - advan_hh: $advan_hh\n";
  $hh = $hh + $advan_hh;
  while($hh > 23) {
  $hh -= 24;
  $dd++;
  }
  while($dd > $mon_days{$mm+0}) {
  $dd = $dd - $mon_days{$mm+0};

  $mm++;

  while($mm > 12) {
  $mm -= 12;
  $yy++;
  }
  }

  while($hh < 0) {
  $hh += 24;
  $dd--;
  }
  if($dd < 1) {
  $mm--;
  while($mm < 1) {
  $mm += 12;
  $yy--;
  }
  $dd += $mon_days{$mm+0};
  }

  my $new_date = sprintf("%04d%02d%02d%02d",$yy,$mm,$dd,$hh);

  return $new_date;
}
#
#
#
sub GI_components {

  my @fields;
  my (@t_ref,@rh_ref,@ws_ref,@wd_ref,@psfc_ref,@ht_ref);
  my (@t_wrf,@rh_wrf,@ws_wrf,@wd_wrf,@psfc_wrf,@ht_wrf);
  my (@t_ss,@rh_ss,@ws_ss,@wd_ss,@psfc_ss,@ht_ss);
  my @prs;
  my ($i,$l);
  my $fcst;
  my $color;

  open(SFC_REF,'sfc_stats_ref.txt');
  open(SFC_WRF,'sfc_stats_wrf.txt');

  my $rec = 0;
  while (<SFC_REF>) {

    @fields=split;

   #$t_ref[$rec]    = r2($fields[2]);
   #$rh_ref[$rec]   = r2($fields[6]);
   #$ws_ref[$rec]   = r2($fields[10]);
   #$wd_ref[$rec]   = r2($fields[14]);
   #$psfc_ref[$rec] = r2($fields[22]);
    $t_ref[$rec]    = $fields[2];
    $rh_ref[$rec]   = $fields[6];
    $ws_ref[$rec]   = $fields[10];
    $wd_ref[$rec]   = $fields[14];
    $psfc_ref[$rec] = $fields[22];

    $rec++;
  }
  close(SFC_REF);

  $rec = 0;
  while (<SFC_WRF>) {

    @fields=split;

   #$t_wrf[$rec]    = r2($fields[2]);
   #$rh_wrf[$rec]   = r2($fields[6]);
   #$ws_wrf[$rec]   = r2($fields[10]);
   #$wd_wrf[$rec]   = r2($fields[14]);
   #$psfc_wrf[$rec] = r2($fields[22]);
    $t_wrf[$rec]    = $fields[2];
    $rh_wrf[$rec]   = $fields[6];
    $ws_wrf[$rec]   = $fields[10];
    $wd_wrf[$rec]   = $fields[14];
    $psfc_wrf[$rec] = $fields[22];

    $rec++;
  }
  close(SFC_WRF);

  foreach $i (0..$#leads) {

     $t_ss[$i]    = ss_calc($t_ref[$i],$t_wrf[$i]);
     $rh_ss[$i]   = ss_calc($rh_ref[$i],$rh_wrf[$i]);
     $ws_ss[$i]   = ss_calc($ws_ref[$i],$ws_wrf[$i]);
     $wd_ss[$i]   = ss_calc($wd_ref[$i],$wd_wrf[$i]);
     $psfc_ss[$i] = ss_calc($psfc_ref[$i],$psfc_wrf[$i]);

  }

  open(TABLES,">GI_components_${date}.html");

  print TABLES "<HTML><HEAD>\n
<TITLE>GO-Index Components Table -- ${date}</TITLE>\n
</HEAD>\n\n
<BODY>\n";

  print TABLES "<H4>GO-Index Surface Components -- ${date}:</H4>\n";

  print TABLES "<TABLE BORDER WIDTH=70% CELLSPACING=0 CELLPADDING=5 COLS=16 ID=\"sfc_n/a\">\n";
  print TABLES "<TR>\n";
  print TABLES "<TH ROWSPAN=2>FCST LEAD (hrs)</TH>\n";
  print TABLES "<TH COLSPAN=3>Temperature (K)</TH>\n";
  print TABLES "<TH COLSPAN=3>Relative Humidity (%)</TH>\n";
  print TABLES "<TH COLSPAN=3>Wind Speed (m/s)</TH>\n";
  print TABLES "<TH COLSPAN=3>Wind Direction (deg)</TH>\n";
  print TABLES "<TH COLSPAN=3>Surce Pressure (hPa)</TH>\n";
  print TABLES "</TR>\n";

  print TABLES "<TR>\n";
  foreach $i (0..4) {
#   print TABLES "<TH>r<SUP>2</SUP><SUB>wrf</SUB></TH>\n";
#   print TABLES "<TH>r<SUP>2</SUP><SUB>ref</SUB></TH>\n";
    print TABLES "<TH>r<SUB>wrf</SUB></TH>\n";
    print TABLES "<TH>r<SUB>ref</SUB></TH>\n";
    print TABLES "<TH>ss</TH>\n";
  }
  print TABLES "</TR>\n";

  foreach $i (0..$#leads) {
    print TABLES "<TR align='CENTER'>\n";
    print TABLES "<TD>$leads[$i]</TD>\n";
    print TABLES "<TD>$t_wrf[$i]</TD>\n";
    print TABLES "<TD>$t_ref[$i]</TD>\n";
   #$color = pick_color($t_ss[$i]);
   #printf TABLES "<TD bgcolor=\"$color\">%.4f</TD>\n",$t_ss[$i];
    printf TABLES "<TD>%.4f</TD>\n",$t_ss[$i];
    print TABLES "<TD>$rh_wrf[$i]</TD>\n";
    print TABLES "<TD>$rh_ref[$i]</TD>\n";
   #$color = pick_color($rh_ss[$i]);
   #printf TABLES "<TD bgcolor=\"$color\">%.4f</TD>\n",$rh_ss[$i];
    printf TABLES "<TD>%.4f</TD>\n",$rh_ss[$i];
    print TABLES "<TD>$ws_wrf[$i]</TD>\n";
    print TABLES "<TD>$ws_ref[$i]</TD>\n";
   #$color = pick_color($ws_ss[$i]);
   #printf TABLES "<TD bgcolor=\"$color\">%.4f</TD>\n",$ws_ss[$i];
    printf TABLES "<TD>%.4f</TD>\n",$ws_ss[$i];
    print TABLES "<TD>$wd_wrf[$i]</TD>\n";
    print TABLES "<TD>$wd_ref[$i]</TD>\n";
   #$color = pick_color($wd_ss[$i]);
   #printf TABLES "<TD bgcolor=\"$color\">%.4f</TD>\n",$wd_ss[$i];
    printf TABLES "<TD>%.4f</TD>\n",$wd_ss[$i];
    print TABLES "<TD>$psfc_wrf[$i]</TD>\n";
    print TABLES "<TD>$psfc_ref[$i]</TD>\n";
   #$color = pick_color($psfc_ss[$i]);
   #printf TABLES "<TD bgcolor=\"$color\">%.4f</TD>\n",$psfc_ss[$i];
    printf TABLES "<TD>%.4f</TD>\n",$psfc_ss[$i];
    print TABLES "</TR>\n";
  }

  print TABLES "</TABLE>\n";
  print TABLES "<BR>\n";

  foreach $fcst (@leads) {

    $fcst = sprintf "%02d", $fcst;

    open(UPR_REF,"upr_stats_ref_${fcst}hrs.txt");

    $l = 0;
    while (<UPR_REF>) {
       @fields = split;

       $prs[$l] = $fields[0];

      #$t_ref[$l]  = r2($fields[2]);
      #$rh_ref[$l] = r2($fields[6]);
      #$ws_ref[$l] = r2($fields[10]);
      #$wd_ref[$l] = r2($fields[14]);
      #$ht_ref[$l] = r2($fields[18]);
       $t_ref[$l]  = $fields[2];
       $rh_ref[$l] = $fields[6];
       $ws_ref[$l] = $fields[10];
       $wd_ref[$l] = $fields[14];
       $ht_ref[$l] = $fields[18];

       $l++;
    }
    close(UPR_REF);

    open(UPR_WRF,"upr_stats_wrf_${fcst}hrs.txt");

    $l = 0;
    while (<UPR_WRF>) {
       @fields = split;

       $prs[$l] = $fields[0];

      #$t_wrf[$l]  = r2($fields[2]);
      #$rh_wrf[$l] = r2($fields[6]);
      #$ws_wrf[$l] = r2($fields[10]);
      #$wd_wrf[$l] = r2($fields[14]);
      #$ht_wrf[$l] = r2($fields[18]);
       $t_wrf[$l]  = $fields[2];
       $rh_wrf[$l] = $fields[6];
       $ws_wrf[$l] = $fields[10];
       $wd_wrf[$l] = $fields[14];
       $ht_wrf[$l] = $fields[18];

       $l++;
    }
    close(UPR_WRF);

    foreach $l (0..$#prs) {

       $t_ss[$l]  = ss_calc($t_ref[$l],$t_wrf[$l]);
       $rh_ss[$l] = ss_calc($rh_ref[$l],$rh_wrf[$l]);
       $ws_ss[$l] = ss_calc($ws_ref[$l],$ws_wrf[$l]);
       $wd_ss[$l] = ss_calc($wd_ref[$l],$wd_wrf[$l]);
       $ht_ss[$l] = ss_calc($ht_ref[$l],$ht_wrf[$l]);

    }
    
    print TABLES "<HR size=\"3\" />\n";

    print TABLES "<H4>GO-Index Upper-air Components at ${fcst}-hr Forecast Time -- ${date}:</H4>\n";
    print TABLES "<TABLE BORDER WIDTH=70% CELLSPACING=0 CELLPADDING=5 COLS=16 ID=\"upr_${fcst}hrs\">\n";
    print TABLES "<TR>\n";
    print TABLES "<TH ROWSPAN=2>Level (hPa)</TH>\n";
    print TABLES "<TH COLSPAN=3>Temperature (K)</TH>\n";
    print TABLES "<TH COLSPAN=3>Relative Humidity (%)</TH>\n";
    print TABLES "<TH COLSPAN=3>Wind Speed (m/s)</TH>\n";
    print TABLES "<TH COLSPAN=3>Wind Direction (deg)</TH>\n";
    print TABLES "<TH COLSPAN=3>Height (m)</TH>\n";
    print TABLES "</TR>\n";
     
    print TABLES "<TR>\n";
    foreach $i (0..4) {
#     print TABLES "<TH>r<SUP>2</SUP><SUB>wrf</SUB></TH>\n";
#     print TABLES "<TH>r<SUP>2</SUP><SUB>ref</SUB></TH>\n";
      print TABLES "<TH>r<SUB>wrf</SUB></TH>\n";
      print TABLES "<TH>r<SUB>ref</SUB></TH>\n";
      print TABLES "<TH>ss</TH>\n";
    }
    print TABLES "</TR>\n";

    foreach $l (0..$#prs) {

       next if($prs[$l] == 2001);

       print TABLES "<TR align='CENTER'>\n";
       print TABLES "<TD>$prs[$l]</TD>\n";
       print TABLES "<TD>$t_wrf[$l]</TD>\n";
       print TABLES "<TD>$t_ref[$l]</TD>\n";
      #$color = pick_color($t_ss[$l]);
      #printf TABLES "<TD bgcolor=\"$color\">%.4f</TD>\n",$t_ss[$l];
       printf TABLES "<TD>%.4f</TD>\n",$t_ss[$l];
       print TABLES "<TD>$rh_wrf[$l]</TD>\n";
       print TABLES "<TD>$rh_ref[$l]</TD>\n";
      #$color = pick_color($rh_ss[$l]);
      #printf TABLES "<TD bgcolor=\"$color\">%.4f</TD>\n",$rh_ss[$l];
       printf TABLES "<TD>%.4f</TD>\n",$rh_ss[$l];
       print TABLES "<TD>$ws_wrf[$l]</TD>\n";
       print TABLES "<TD>$ws_ref[$l]</TD>\n";
      #$color = pick_color($ws_ss[$l]);
      #printf TABLES "<TD bgcolor=\"$color\">%.4f</TD>\n",$ws_ss[$l];
       printf TABLES "<TD>%.4f</TD>\n",$ws_ss[$l];
       print TABLES "<TD>$wd_wrf[$l]</TD>\n";
       print TABLES "<TD>$wd_ref[$l]</TD>\n";
      #$color = pick_color($wd_ss[$l]);
      #printf TABLES "<TD bgcolor=\"$color\">%.4f</TD>\n",$wd_ss[$l];
       printf TABLES "<TD>%.4f</TD>\n",$wd_ss[$l];
       print TABLES "<TD>$ht_wrf[$l]</TD>\n";
       print TABLES "<TD>$ht_ref[$l]</TD>\n";
      #$color = pick_color($ht_ss[$l]);
      #printf TABLES "<TD bgcolor=\"$color\">%.4f</TD>\n",$ht_ss[$l];
       printf TABLES "<TD>%.4f</TD>\n",$ht_ss[$l];

    }

    print TABLES "</TABLE>\n";
    print TABLES "<BR>\n";

  }

  print TABLES "</BODY></HTML>\n";

  close(TABLES);

  return;

}
#
#
#
sub r2 {

  my $val = $_[0];
  my $rmse2;

  if ($val != $vmissing) {
     $rmse2 = $val**2;
  } else {
     $rmse2 = $vmissing;
  }

  return $rmse2;

}
#
#
#
sub ss_calc {

  my ($ref,$wrf) = @_;
  my $ss;

  if ($ref != $vmissing && $wrf != $vmissing) {
     if ($ref == 0) {
        $ss = $vmissing;
     } else {
       #$ss = 1 - $wrf/(0.5*($ref+$wrf));
        $ss = 1 - $wrf**2/(0.5*($ref**2+$wrf**2));
     }
  } else {
     $ss = $vmissing;
  }

  return $ss;

}
#
#
#
sub pick_color {

  my $val = $_[0];
  my $color;

  if ($val > 0) {
     $color = 'green';
  } elsif ($val < 0) {
     if ($val == $vmissing) {
        $color = 'gray';
     } else {
        $color = 'red';
     }
  } else {
     $color = 'yellow';
  }

  return $color;

}
#
# Begin GO-Index log file
#
sub GI_log {

  system("paste sfc_table.txt upr_table.txt > log_tmp.txt");
  my $wcOut = `wc -l log_tmp.txt`;
  chomp($wcOut);
  my ($lines,$dummy) = split " ",$wcOut;
  open(LOG_TEXT,'log_tmp.txt');
  open(LOG,">GI_log_${date}.json");
  print LOG "{";
  print LOG "header:[\"Station\", \"Latitude\", \"Longitude\", \"# Surface obs: 0-h fcst\", \"# Surface obs: 12-h fcst\", \"# Surface obs: 24-h fcst\", \"# Surface obs: 48-h fcst\",\"# Soundings: 0-h fcst\", \"# Soundings: 12-h fcst\", \"# Soundings: 24-h fcst\", \"# Soundings: 48-h fcst\"], body:[";
  while (<LOG_TEXT>) {
    chomp;
    ($sid,$lat,$lon,@counts) = split;
    print LOG "[$sid,$lat,$lon";
    foreach $count (@counts) {
      print LOG ",$count";
    }
    if ($. < $lines) {
       print LOG "],";
    } else {
       print LOG "]";
    }
  }
  print LOG "]";
  print LOG "}";
  close(LOG_TEXT);
  close(LOG);

  return;

}

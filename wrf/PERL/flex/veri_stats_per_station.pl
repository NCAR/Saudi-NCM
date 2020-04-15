#!/usr/bin/perl
use strict;
no strict 'refs';
use vars qw($opt_c $opt_j $opt_d $opt_b);
use Getopt::Std;

my $GSJOBDIR;
our $WRF_PAIRS_DIR;
our $REF_PAIRS_DIR;
our $MONTHLY_DIR;
our $NUM_DOMS;
our $EXECUTABLE_ARCHIVE;
our %plot_range;
our $DEBUG;
our $DEST_SERVER;
our $JOB_LOC;
our $KEY;
our $HISTOGRAM;
our $GMT_BIN;

my ($wrf_sfc_example,$ref_upr_example,$wrf_sfc_suffix,$wrf_upr_suffix);
my $ref_suffix;
my $config_file;
my @uprPlotHours;
my $VERI_LENGTH;
my @mdays = (31,31,28,31,30,31,30,31,31,30,31,30,31);
my ($lstation,$llevel,$lfcst);
my ($sid,$lat,$lon);
my ($level,$fcst,$weight);
my (@levels,@l_weight,@leads,@f_weight);
my ($wrf,$ref);
my ($wrfUprStats,$wrfUprStatsNew,$refUprStats,$refUprStatsNew);
my ($wrfFreq,$wrfFreqNew,$refFreq,$refFreqNew);
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
my $n;
my $domain;

my $YYYYMM;
my ($year,$month);
my $days;
my ($begin,$end);
my ($file,$cycle,$valid);
my $hour;
my ($ps,$gif);
my %sname;
my @stations;
my $istation;

getopts('c:j:d:b:');

if ($opt_c) {
   $config_file = $opt_c;
} else {
   die "Usage: $0 -c <config_file> -j <GSJOBDIR> [-d <stats period in days>] [-b <begin time in YYYYMMDDHH>]\n";
}

if ($opt_j) {
   $GSJOBDIR = $opt_j;
   $ENV{GSJOBDIR} = $GSJOBDIR;
   $GSJOBDIR =~ /(\w+)$/;
   $ENV{GSJOBID} = $1;
} else {
   die "Usage: $0 -c <config_file> -j <GSJOBDIR> [-d <stats period in days>] [-b <begin time in YYYYMMDDHH>]\n"; }

###

if (-e "$GSJOBDIR/flexinput.pl") {
   require "$GSJOBDIR/flexinput.pl";
} else {
   die "$GSJOBDIR/flexinput.pl does not exist!\n";
}

if (-e "$GSJOBDIR/statsinput.pl") {
   require "$GSJOBDIR/statsinput.pl";
} else {
   die "$GSJOBDIR/statsinput.pl does not exist!\n";
}

if (-e "$GSJOBDIR/postprocinput.pl") {
   require "$GSJOBDIR/postprocinput.pl";
} else {
   die "$GSJOBDIR/postprocinput.pl does not exist!\n";
}

### GMT bin path

if (! $GMT_BIN) {
   $GMT_BIN = '/opt/gmt/bin';
}

$ENV{PATH} = "${GMT_BIN}:$ENV{PATH}";

if ($opt_b && $opt_d) {
   $begin = $opt_b;
   $days = $opt_d;
   $end = advance_h($begin,24*$days);
   $YYYYMM = substr($begin,0,6);
   $year = substr($YYYYMM,0,4);
   $month = substr($YYYYMM,-2);
} else {
   $YYYYMM = `date --date="1 day ago" +%Y%m`;
   chomp($YYYYMM);
   $year = substr($YYYYMM,0,4);
   $month = substr($YYYYMM,-2);

   if ($year%100 == 0) {
      if ($year%400 == 0) {
         $mdays[2] = 29;
      }
   } elsif ($year%4 == 0) {
      $mdays[2] = 29;
   }

   $days = $mdays[$month];
   $begin = sprintf "%4d%02d0100",$year,$month;
   $end = sprintf "%4d%02d%2d23",$year,$month,$days;

   print "begin = $begin; end = $end\n";
}

print "GSJOBDIR = $GSJOBDIR\n";
print "WRF_PAIRS_DIR = $WRF_PAIRS_DIR\n";
print "REF_PAIRS_DIR = $REF_PAIRS_DIR\n";
print "EXECUTABLE_ARCHIVE = $EXECUTABLE_ARCHIVE\n";

$MONTHLY_DIR .= '_per_station';

system("mkdir -p $MONTHLY_DIR/$YYYYMM");

chdir "$MONTHLY_DIR/$YYYYMM";

&parse_config();

open(WRF_SFC_LIST,">wrf_sfc.list");

print "WRF_PAIRS_DIR = $WRF_PAIRS_DIR\n";

# REF surface pairs file list:

open(REF_SFC_LIST,">ref_sfc.list");

foreach $file (<$REF_PAIRS_DIR/sfc/*>) {
  $file =~ /(\d{10})/;
  $cycle = $1;
  if ($cycle >= $begin && $cycle < $end && -s $file) {
     print REF_SFC_LIST "$file\n";
  }
}
close(REF_SFC_LIST);

# REF upper-air pairs file list:

open(REF_UPR_LIST,">ref_upr.list");

foreach $file (<$REF_PAIRS_DIR/upr/*>) {
  $file =~ /(\d{10})_(\d{10})/;
  $valid = $2;
# my $valid_hour = substr($valid,8,2);
# next if($valid_hour eq '06' || $valid_hour eq '18');
  if ($valid >= $begin && $valid < $end && -s $file) {
     print REF_UPR_LIST "$file\n";
  }
}
close(REF_UPR_LIST);

# WRF surface pairs file list:

open(REF_SFC_LIST,"ref_sfc.list");
open(WRF_SFC_LIST,">wrf_sfc.list");

$wrf_sfc_example = `ls -1 $WRF_PAIRS_DIR/sfc/fcst | tail -1`;
chomp($wrf_sfc_example);
$wrf_sfc_example =~ /veri_dat_(\S+)$/;
$wrf_sfc_suffix = $1;

while (<REF_SFC_LIST>) {
  s/${REF_PAIRS_DIR}\/sfc/${WRF_PAIRS_DIR}\/sfc\/fcst/;
  $_ =~ /([a-zA-Z0-9]*)$/;
  $ref_suffix = $1;
  s/$ref_suffix/$wrf_sfc_suffix/;
  print WRF_SFC_LIST $_;
}
close(WRF_SFC_LIST);

# WRF upper-air pairs file list:

$ref_upr_example = `ls -1 $REF_PAIRS_DIR/upr | tail -1`;
chomp($ref_upr_example);
$ref_upr_example =~ /veri_dat_upr_(\S+)$/;
$ref_suffix = $1;

open(WRF_UPR_LIST,">wrf_upr.list");

foreach $file (<$WRF_PAIRS_DIR/upr/fcst/*>) {
  $file =~ /(\d{10})_(\d{10})/;
  $cycle = $1;
  $valid = $2;
  my $cycle_hour = substr($cycle,8,2);
  if ($cycle_hour eq '06' || $cycle_hour eq '18') {
     if ($valid >= $begin && $valid < $end) {
        print WRF_UPR_LIST "$file\n";
     }
  } else {
     my $ref = $file;
     $ref =~ s/${WRF_PAIRS_DIR}\/upr\/fcst/${REF_PAIRS_DIR}\/upr/;
     $file =~ /veri_dat_upr_(\S+)$/;
     $wrf_upr_suffix = $1;
     $ref =~ s/$wrf_upr_suffix/$ref_suffix/;
     if (-s "$ref") {
        if ($valid >= $begin && $valid < $end) {
           print WRF_UPR_LIST "$file\n";
        }
     }
  }
}
close(WRF_UPR_LIST);

#open(REF_UPR_LIST,"ref_upr.list");
#open(WRF_UPR_LIST,">wrf_upr.list");

#$wrf_upr_example = `ls -1 $WRF_PAIRS_DIR/upr/fcst | tail -1`;
#chomp($wrf_upr_example);
#$wrf_upr_example =~ /veri_dat_upr_(\S+)$/;
#$wrf_upr_suffix = $1;

#while (<REF_UPR_LIST>) {
#  s/${REF_PAIRS_DIR}\/upr/${WRF_PAIRS_DIR}\/upr\/fcst/;
#  $_ =~ /([a-zA-Z0-9]*)$/;
#  $ref_suffix = $1;
#  s/$ref_suffix/$wrf_upr_suffix/;
#  print WRF_UPR_LIST $_;
#}
#close(WRF_UPR_LIST);

# loop through each individual station

foreach $istation (0..$#stations) {

   $domain = create_config($stations[$istation]);

   print "station = $stations[$istation]; domain = $domain\n";

# stats and plots

  ### WRF SUFACE STATS

  system("$EXECUTABLE_ARCHIVE/stats_calc.exe -conf config_file.$stations[$istation] -list wrf_sfc.list -type sfc -domain $domain -model wrf");
  system("mv sfc_stats_wrf.txt sfc_stats_wrf_${stations[$istation]}.txt");

  ### WRF UPPER-AIR STATS

 #system("$EXECUTABLE_ARCHIVE/stats_calc.exe -conf config_file.$stations[$istation] -list wrf_upr.list -type upr -domain $domain -model wrf");
  system("/raid2/sheu/src/GO-INDEX/stats_calc.exe -conf config_file.$stations[$istation] -list wrf_upr.list -type upr -domain $domain -model wrf");

  foreach $wrfUprStats (<upr_stats_wrf_??hrs.txt>) {
     $wrfUprStatsNew = $wrfUprStats;
     $wrfUprStatsNew =~ s/wrf_/wrf_${stations[$istation]}_/;
     system("mv $wrfUprStats $wrfUprStatsNew");
    #if ($domain > 1) {
    #   $refUprStatsNew = $wrfUprStatsNew;
    #   $refUprStatsNew =~ s/wrf/ref/;
    #   system("cp $wrfUprStatsNew $refUprStatsNew");
    #}
  }

  foreach $wrfFreq (<*_freq_*_*_wrf.txt>) {
     $wrfFreqNew = $wrfFreq;
     $wrfFreqNew =~ s/wrf\./wrf_${stations[$istation]}\./;
     system("mv $wrfFreq $wrfFreqNew");
  }
  ### REF SURFACE STATS

  system("$EXECUTABLE_ARCHIVE/stats_calc.exe -conf config_file.$stations[$istation] -list ref_sfc.list -type sfc -domain 1 -model ref");
  system("mv sfc_stats_ref.txt sfc_stats_ref_${stations[$istation]}.txt");

  ### REF UPPER-AIR STATS

  system("$EXECUTABLE_ARCHIVE/stats_calc.exe -conf config_file.$stations[$istation] -list ref_upr.list -type upr -domain 1 -model ref");

  foreach $refUprStats (<upr_stats_ref_??hrs.txt>) {
     $refUprStatsNew = $refUprStats;
     $refUprStatsNew =~ s/ref_/ref_${stations[$istation]}_/;
     system("mv $refUprStats $refUprStatsNew");
  }

  foreach $refFreq (<*_freq_*_*_ref.txt>) {
     $refFreqNew = $refFreq;
     $refFreqNew =~ s/ref\./ref_${stations[$istation]}\./;
     system("mv $refFreq $refFreqNew");
  }

  ### END OF STATS CALCULATION

  ### BEGIN STATS PLOTTING

  &sfc_plot($domain,$stations[$istation]);

  &upr_plot($domain,$stations[$istation]);

  &histogram($domain,$stations[$istation]);

# convert ps to gif

foreach $ps (<*.ps>) {
  $gif = $ps;
  $gif =~ s/ps$/gif/;
  system("convert -density 56 -trim +repage -flatten $ps $gif");
}

# clean up if $DEBUG is low

if ($DEBUG < 10) {
   system("rm -f *.txt *.ps");
}

#system("mkdir $stations[$istation]");
#sstem("mv *.gif $stations[$istation]/.");

} # end of individual station loop

## mv/copy/rsync to web produdt directory

if ($DEST_SERVER =~ /localhost/) {
   system("mkdir -p $JOB_LOC/monthly_stats/$YYYYMM");
   system("mv *.gif $JOB_LOC/monthly_stats/$YYYYMM/.");
} else {
   chdir "$MONTHLY_DIR";
   system("rsync -e 'ssh -i $KEY' -avzC $YYYYMM $DEST_SERVER:$JOB_LOC/monthly_stats/.");
}

system("rm -rf $MONTHLY_DIR/$YYYYMM");

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
sub sfc_plot {

  my ($domain,$station) = @_;
  my @fields;
  my $missing = -99;
  my $fn;
  my ($lead,$l);
  my $max_count;
  my ($ymax,$ytick);

  open(WRF,"sfc_stats_wrf_${station}.txt");
  open(REF,"sfc_stats_ref_${station}.txt") if (-e "sfc_stats_ref_${station}.txt");

  my @wrf_t_bs = ();
  my @wrf_t_rs = ();
  my @wrf_t_ms = ();
  my @wrf_t_ns = ();
  my @wrf_rh_bs = ();
  my @wrf_rh_rs = ();
  my @wrf_rh_ms = ();
  my @wrf_rh_ns = ();
  my @wrf_ws_bs = ();
  my @wrf_ws_rs = ();
  my @wrf_ws_ms = ();
  my @wrf_ws_ns = ();
  my @wrf_wd_bs = ();
  my @wrf_wd_rs = ();
  my @wrf_wd_ms = ();
  my @wrf_wd_ns = ();
  my @wrf_ps_bs = ();
  my @wrf_ps_rs = ();
  my @wrf_ps_ms = ();
  my @wrf_ps_ns = ();

  my @ref_t_bs = ();
  my @ref_t_rs = ();
  my @ref_t_ms = ();
  my @ref_t_ns = ();
  my @ref_rh_bs = ();
  my @ref_rh_rs = ();
  my @ref_rh_ms = ();
  my @ref_rh_ns = ();
  my @ref_ws_bs = ();
  my @ref_ws_rs = ();
  my @ref_ws_ms = ();
  my @ref_ws_ns = ();
  my @ref_wd_bs = ();
  my @ref_wd_rs = ();
  my @ref_wd_ms = ();
  my @ref_wd_ns = ();
  my @ref_ps_bs = ();
  my @ref_ps_rs = ();
  my @ref_ps_ms = ();
  my @ref_ps_ns = ();

  while (<WRF>) {

    @fields = split;
    push(@leads,$fields[0]-1);

    push(@wrf_t_bs,$fields[1]);
    push(@wrf_t_rs,$fields[2]);
    push(@wrf_t_ms,$fields[3]);
    push(@wrf_t_ns,$fields[4]);

    push(@wrf_rh_bs,$fields[5]);
    push(@wrf_rh_rs,$fields[6]);
    push(@wrf_rh_ms,$fields[7]);
    push(@wrf_rh_ns,$fields[8]);

    push(@wrf_ws_bs,$fields[9]);
    push(@wrf_ws_rs,$fields[10]);
    push(@wrf_ws_ms,$fields[11]);
    push(@wrf_ws_ns,$fields[12]);

    push(@wrf_wd_bs,$fields[13]);
    push(@wrf_wd_rs,$fields[14]);
    push(@wrf_wd_ms,$fields[15]);
    push(@wrf_wd_ns,$fields[16]);

    push(@wrf_ps_bs,$fields[21]);
    push(@wrf_ps_rs,$fields[22]);
    push(@wrf_ps_ms,$fields[23]);
    push(@wrf_ps_ns,$fields[24]);

  }
  close(WRF);

  while (<REF>) {

    @fields = split;

    push(@ref_t_bs,$fields[1]);
    push(@ref_t_rs,$fields[2]);
    push(@ref_t_ms,$fields[3]);
    push(@ref_t_ns,$fields[4]);

    push(@ref_rh_bs,$fields[5]);
    push(@ref_rh_rs,$fields[6]);
    push(@ref_rh_ms,$fields[7]);
    push(@ref_rh_ns,$fields[8]);

    push(@ref_ws_bs,$fields[9]);
    push(@ref_ws_rs,$fields[10]);
    push(@ref_ws_ms,$fields[11]);
    push(@ref_ws_ns,$fields[12]);

    push(@ref_wd_bs,$fields[13]);
    push(@ref_wd_rs,$fields[14]);
    push(@ref_wd_ms,$fields[15]);
    push(@ref_wd_ns,$fields[16]);

    push(@ref_ps_bs,$fields[21]);
    push(@ref_ps_rs,$fields[22]);
    push(@ref_ps_ms,$fields[23]);
    push(@ref_ps_ns,$fields[24]);

  }
  close(REF);

# T BIAS:

  $fn = "sfc_plots_t_${station}_${YYYYMM}.ps";

  open(GMT,"| pstext -JX5/0.5 -R0/100/-10/10 -X1.5 -Y9.5 -K > $fn");
  print GMT "0 0 14 0 5 ML Month $month/$year\n";
  print GMT "25 0 14 0 5 ML Domain $domain\n";
  print GMT "50 0 14 0 5 ML Station: ${station} $sname{$station}";
  close(GMT);

  open(GMT,"| psxy -JX5/2 -R0/$VERI_LENGTH/$plot_range{t}{bias} -Bf1a6/f1a5:\"T BIAS (K)\":WSen -W1.5p/0/255/0 -Y-2 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_t_bs[$lead]\n" if ($wrf_t_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -K -O >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_t_bs[$lead]\n" if ($ref_t_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -Wta -K -O >> $fn");
  print GMT "0 0 \n $VERI_LENGTH 0";
  close(GMT);

# T RMSE:

  open(GMT,"| psxy -JX5/2 -R0/$VERI_LENGTH/$plot_range{t}{rmse} -Bf1a6/f1a1:\"T RMSE (K)\":WSen -W1.5p/0/255/0 -Y-2.5 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_t_rs[$lead]\n" if($wrf_t_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_t_rs[$lead]\n" if($ref_t_ns[$lead] > 0);
  }
  close(GMT);

# T MAE:

  open(GMT,"| psxy -JX5/2 -R -Bf1a6:\"FCST LEAD TIME (hrs)\":/f1a1:\"T MAE (K)\":WSen -W1.5p/0/255/0 -Y-2.5 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_t_ms[$lead]\n" if($wrf_t_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_t_ms[$lead]\n" if($ref_t_ns[$lead] > 0);
  }
  close(GMT);

# plot counts

  $max_count = 0;
  foreach $lead (@leads) {
     $max_count = $wrf_t_ns[$lead] if ($wrf_t_ns[$lead] > $max_count);
  }

  ($ymax,$ytick) = annotate_property($max_count);

  open(GMT,"| psxy -JX5/0.5 -R0/${VERI_LENGTH}/0/$ymax -Bf1a6:Hour:/${ytick}:Counts:WS -Sb0.5u -Ggray -Y-1.25 -O -K >> $fn");
  foreach $l (0..$#leads) {
     print GMT "$leads[$l] $wrf_t_ns[$l]\n";
  }
  close(GMT);

  # Annotate:

  open(GMT,"| psxy -JX5/1 -R0/100/-10/10 -W1.5p/0/255/0 -Y-1.5 -O -K >> $fn");
  print GMT "0 0 \n 20 0";
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  print GMT "50 0 \n 70 0";
  close(GMT);

  open(GMT,"| pstext -JX -R -O -K >> $fn");
  print GMT "25 0 12 0 5 ML WRF";
  close(GMT);

  open(GMT,"| pstext -JX -R -O >> $fn");
  print GMT "75 0 12 0 5 ML REF";
  close(GMT);

###

# RH BIAS:

  $fn = "sfc_plots_rh_${station}_${YYYYMM}.ps";

  open(GMT,"| pstext -JX5/0.5 -R0/100/-10/10 -X1.5 -Y9.5 -K > $fn");
  print GMT "0 0 14 0 5 ML Month $month/$year\n";
  print GMT "25 0 14 0 5 ML Domain $domain\n";
  print GMT "50 0 14 0 5 ML Station: ${station} $sname{$station}";
  close(GMT);

  open(GMT,"| psxy -JX5/2 -R0/$VERI_LENGTH/$plot_range{rh}{bias} -Bf1a6/f1a5:\"RH BIAS (%)\":WSen -W1.5p/0/255/0 -Y-2 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_rh_bs[$lead]\n" if ($wrf_rh_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -K -O >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_rh_bs[$lead]\n" if ($ref_rh_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -Wta -K -O >> $fn");
  print GMT "0 0 \n $VERI_LENGTH 0";
  close(GMT);

# RH RMSE:

  open(GMT,"| psxy -JX -R0/$VERI_LENGTH/$plot_range{rh}{rmse} -Bf1a6/f1a5:\"MAE RMSE (%)\": -W1.5p/0/255/0 -Y-2.5 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_rh_rs[$lead]\n" if($wrf_rh_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_rh_rs[$lead]\n" if($ref_rh_ns[$lead] > 0);
  }
  close(GMT);

# RH MAE:

  open(GMT,"| psxy -JX -R -Bf1a6:\"FCST LEAD TIME (hrs)\":/f1a5:\"RH MAE (%)\":WSen -W1.5p/0/255/0 -Y-2.5 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_rh_ms[$lead]\n" if($wrf_rh_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_rh_ms[$lead]\n" if($ref_rh_ns[$lead] > 0);
  }
  close(GMT);

# plot counts

  $max_count = 0;
  foreach $lead (@leads) {
     $max_count = $wrf_rh_ns[$lead] if ($wrf_rh_ns[$lead] > $max_count);
  }

  ($ymax,$ytick) = annotate_property($max_count);

  open(GMT,"| psxy -JX5/0.5 -R0/${VERI_LENGTH}/0/$ymax -Bf1a6:Hour:/${ytick}:Counts:WS -Sb0.5u -Ggray -Y-1.25 -O -K >> $fn");
  foreach $l (0..$#leads) {
     print GMT "$leads[$l] $wrf_rh_ns[$l]\n";
  }
  close(GMT);

  # Annotate:

  open(GMT,"| psxy -JX5/1 -R0/100/-10/10 -W1.5p/0/255/0 -Y-1.5 -O -K >> $fn");
  print GMT "0 0 \n 20 0";
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  print GMT "50 0 \n 70 0";
  close(GMT);

  open(GMT,"| pstext -JX -R -O -K >> $fn");
  print GMT "25 0 12 0 5 ML WRF";
  close(GMT);

  open(GMT,"| pstext -JX -R -O >> $fn");
  print GMT "75 0 12 0 5 ML REF";
  close(GMT);

###

# WS BIAS:

  $fn = "sfc_plots_ws_${station}_${YYYYMM}.ps";

  open(GMT,"| pstext -JX5/0.5 -R0/100/-10/10 -X1.5 -Y9.5 -K > $fn");
  print GMT "0 0 14 0 5 ML Month $month/$year\n";
  print GMT "25 0 14 0 5 ML Domain $domain\n";
  print GMT "50 0 14 0 5 ML Station: ${station} $sname{$station}";
  close(GMT);

  open(GMT,"| psxy -JX5/2 -R0/$VERI_LENGTH/$plot_range{ws}{bias} -Bf1a6/f1a5:\"WSPD BIAS (m s\@+-1\@+)\":WSen -W1.5p/0/255/0 -Y-2 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_ws_bs[$lead]\n" if ($wrf_ws_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -K -O >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_ws_bs[$lead]\n" if ($ref_ws_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -Wta -K -O >> $fn");
  print GMT "0 0 \n $VERI_LENGTH 0";
  close(GMT);

# WS RMSE:

  open(GMT,"| psxy -JX5/2 -R0/$VERI_LENGTH/$plot_range{ws}{rmse} -Bf1a6/f1a1:\"WSPD RMSE (m s\@+-1\@+)\": -W1.5p/0/255/0 -Y-2.5 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_ws_rs[$lead]\n" if($wrf_ws_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_ws_rs[$lead]\n" if($ref_ws_ns[$lead] > 0);
  }

# WS MAE:

  open(GMT,"| psxy -JX -R -Bf1a6:\"FCST LEAD TIME (hrs)\":/f1a1:\"WSPD MAE (m s\@+-1\@+)\":WSen -W1.5p/0/255/0 -Y-2.5 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_ws_ms[$lead]\n" if($wrf_ws_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_ws_ms[$lead]\n" if($ref_ws_ns[$lead] > 0);
  }
  close(GMT);

# plot counts

  $max_count = 0;
  foreach $lead (@leads) {
     $max_count = $wrf_ws_ns[$lead] if ($wrf_ws_ns[$lead] > $max_count);
  }

  ($ymax,$ytick) = annotate_property($max_count);

  open(GMT,"| psxy -JX5/0.5 -R0/${VERI_LENGTH}/0/$ymax -Bf1a6:Hour:/${ytick}:Counts:WS -Sb0.5u -Ggray -Y-1.25 -O -K >> $fn");
  foreach $l (0..$#leads) {
     print GMT "$leads[$l] $wrf_ws_ns[$l]\n";
  }
  close(GMT);

  # Annotate:

  open(GMT,"| psxy -JX5/1 -R0/100/-10/10 -W1.5p/0/255/0 -Y-1.5 -O -K >> $fn");
  print GMT "0 0 \n 20 0";
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  print GMT "50 0 \n 70 0";
  close(GMT);

  open(GMT,"| pstext -JX -R -O -K >> $fn");
  print GMT "25 0 12 0 5 ML WRF";
  close(GMT);

  open(GMT,"| pstext -JX -R -O >> $fn");
  print GMT "75 0 12 0 5 ML REF";
  close(GMT);

###

# WD BIAS:

  $fn = "sfc_plots_wd_${station}_${YYYYMM}.ps";

  open(GMT,"| pstext -JX5/0.5 -R0/100/-10/10 -X1.5 -Y9.5 -K > $fn");
  print GMT "0 0 14 0 5 ML Month $month/$year\n";
  print GMT "25 0 14 0 5 ML Domain $domain\n";
  print GMT "50 0 14 0 5 ML Station: ${station} $sname{$station}";
  close(GMT);

  open(GMT,"| psxy -JX5/2 -R0/$VERI_LENGTH/$plot_range{wd}{bias} -Bf1a6/f1a10:\"WDIR BIAS (\272)\":WSen -W1.5p/0/255/0 -Y-2 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_wd_bs[$lead]\n" if ($wrf_wd_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -K -O >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_wd_bs[$lead]\n" if ($ref_wd_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -Wta -K -O >> $fn");
  print GMT "0 0 \n $VERI_LENGTH 0";
  close(GMT);

# WD RMSE:

  open(GMT,"| psxy -JX5/2 -R0/$VERI_LENGTH/$plot_range{wd}{rmse} -Bf1a6/f10a50:\"WDIR RMSE (\272)\":WSen -W1.5p/0/255/0 -Y-2.5 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_wd_rs[$lead]\n" if($wrf_wd_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_wd_rs[$lead]\n" if($ref_wd_ns[$lead] > 0);
  }
  close(GMT);

# WD MAE:

  open(GMT,"| psxy -JX -R0/$VERI_LENGTH/$plot_range{wd}{rmse} -Bf1a6:\"FCST LEAD TIME (hrs)\":/f10a50:\"WDIR MAE (\272)\":WSen -W1.5p/0/255/0 -Y-2.5 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_wd_ms[$lead]\n" if($wrf_wd_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_wd_ms[$lead]\n" if($ref_wd_ns[$lead] > 0);
  }
  close(GMT);

# plot counts

  $max_count = 0;
  foreach $lead (@leads) {
     $max_count = $wrf_wd_ns[$lead] if ($wrf_wd_ns[$lead] > $max_count);
  }

  ($ymax,$ytick) = annotate_property($max_count);

  open(GMT,"| psxy -JX5/0.5 -R0/${VERI_LENGTH}/0/$ymax -Bf1a6:Hour:/${ytick}:Counts:WS -Sb0.5u -Ggray -Y-1.25 -O -K >> $fn");
  foreach $l (0..$#leads) {
     print GMT "$leads[$l] $wrf_wd_ns[$l]\n";
  }
  close(GMT);

  # Annotate:

  open(GMT,"| psxy -JX5/1 -R0/100/-10/10 -W1.5p/0/255/0 -Y-1.5 -O -K >> $fn");
  print GMT "0 0 \n 20 0";
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  print GMT "50 0 \n 70 0";
  close(GMT);

  open(GMT,"| pstext -JX -R -O -K >> $fn");
  print GMT "25 0 12 0 5 ML WRF";
  close(GMT);

  open(GMT,"| pstext -JX -R -O >> $fn");
  print GMT "75 0 12 0 5 ML REF";
  close(GMT);

###

# PS BIAS:

  $fn = "sfc_plots_ps_${station}_${YYYYMM}.ps";

  open(GMT,"| pstext -JX5/0.5 -R0/100/-10/10 -X1.5 -Y9.5 -K > $fn");
  print GMT "0 0 14 0 5 ML Month $month/$year\n";
  print GMT "25 0 14 0 5 ML Domain $domain\n";
  print GMT "50 0 14 0 5 ML Station: ${station} $sname{$station}";
  close(GMT);

  open(GMT,"| psxy -JX5/2 -R0/$VERI_LENGTH/$plot_range{psfc}{bias} -Bf1a6/f1a5:\"PSFC BIAS (hPa)\":WSen -W1.5p/0/255/0 -Y-2 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_ps_bs[$lead]\n" if ($wrf_ps_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -K -O >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_ps_bs[$lead]\n" if ($ref_ps_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -Wta -K -O >> $fn");
  print GMT "0 0 \n $VERI_LENGTH 0";
  close(GMT);

# PS RMSE:

  open(GMT,"| psxy -JX5/2 -R0/$VERI_LENGTH/$plot_range{psfc}{rmse} -Bf1a6/f1a5:\"PSFC RMSE (hPa)\":WSen -W1.5p/0/255/0 -Y-2.5 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_ps_rs[$lead]\n" if($wrf_ps_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_ps_rs[$lead]\n" if($ref_ps_ns[$lead] > 0);
  }
  close(GMT);

# PS MAE:

  open(GMT,"| psxy -JX5/2 -R0/$VERI_LENGTH/$plot_range{psfc}{rmse} -Bf1a6:\"FCST LEAD TIME (hrs)\":/f1a5:\"PSFC MAE (hPa)\":WSen -W1.5p/0/255/0 -Y-2.5 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $wrf_ps_ms[$lead]\n" if($wrf_ps_ns[$lead] > 0);
  }
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  foreach $lead (@leads) {
     print GMT "$lead $ref_ps_ms[$lead]\n" if($ref_ps_ns[$lead] > 0);
  }
  close(GMT);

# plot counts

  $max_count = 0;
  foreach $lead (@leads) {
     $max_count = $wrf_ps_ns[$lead] if ($wrf_ps_ns[$lead] > $max_count);
  }

  ($ymax,$ytick) = annotate_property($max_count);

  open(GMT,"| psxy -JX5/0.5 -R0/${VERI_LENGTH}/0/$ymax -Bf1a6:Hour:/${ytick}:Counts:WS -Sb0.5u -Ggray -Y-1.25 -O -K >> $fn");
  foreach $l (0..$#leads) {
     print GMT "$leads[$l] $wrf_ps_ns[$l]\n";
  }
  close(GMT);

  # Annotate:

  open(GMT,"| psxy -JX5/1 -R0/100/-10/10 -W1.5p/0/255/0 -Y-1.5 -O -K >> $fn");
  print GMT "0 0 \n 20 0";
  close(GMT);

  open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
  print GMT "50 0 \n 70 0";
  close(GMT);

  open(GMT,"| pstext -JX -R -O -K >> $fn");
  print GMT "25 0 12 0 5 ML WRF";
  close(GMT);

  open(GMT,"| pstext -JX -R -O >> $fn");
  print GMT "75 0 12 0 5 ML REF";
  close(GMT);

  undef @leads;
  undef @wrf_t_bs;
  undef @wrf_t_rs;
  undef @wrf_t_ms;
  undef @wrf_t_ns;
  undef @wrf_rh_bs;
  undef @wrf_rh_rs;
  undef @wrf_rh_ms;
  undef @wrf_rh_ns;
  undef @wrf_ws_bs;
  undef @wrf_ws_rs;
  undef @wrf_ws_ms;
  undef @wrf_ws_ns;
  undef @wrf_wd_bs;
  undef @wrf_wd_rs;
  undef @wrf_wd_ms;
  undef @wrf_wd_ns;
  undef @wrf_ps_bs;
  undef @wrf_ps_rs;
  undef @wrf_ps_ms;
  undef @wrf_ps_ns;

  undef @ref_t_bs;
  undef @ref_t_rs;
  undef @ref_t_ms;
  undef @ref_t_ns;
  undef @ref_rh_bs;
  undef @ref_rh_rs;
  undef @ref_rh_ms;
  undef @ref_rh_ns;
  undef @ref_ws_bs;
  undef @ref_ws_rs;
  undef @ref_ws_ms;
  undef @ref_ws_ns;
  undef @ref_wd_bs;
  undef @ref_wd_rs;
  undef @ref_wd_ms;
  undef @ref_wd_ns;
  undef @ref_ps_bs;
  undef @ref_ps_rs;
  undef @ref_ps_ms;
  undef @ref_ps_ns;

  return;

}
#
#
#
sub upr_plot {

  my ($domain,$station) = @_;
  my (@wrf_t_bs,@wrf_t_rs,@wrf_t_ms,@wrf_t_ns);
  my (@wrf_rh_bs,@wrf_rh_rs,@wrf_rh_ms,@wrf_rh_ns);
  my (@wrf_ws_bs,@wrf_ws_rs,@wrf_ws_ms,@wrf_ws_ns);
  my (@wrf_wd_bs,@wrf_wd_rs,@wrf_wd_ms,@wrf_wd_ns);
  my (@wrf_gh_bs,@wrf_gh_rs,@wrf_gh_ms,@wrf_gh_ns);

  my (@ref_t_bs,@ref_t_rs,@ref_t_ms,@ref_t_ns);
  my (@ref_rh_bs,@ref_rh_rs,@ref_rh_ms,@ref_rh_ns);
  my (@ref_ws_bs,@ref_ws_rs,@ref_ws_ms,@ref_ws_ns);
  my (@ref_wd_bs,@ref_wd_rs,@ref_wd_ms,@ref_wd_ns);
  my (@ref_gh_bs,@ref_gh_rs,@ref_gh_ms,@ref_gh_ns);

  my @leads;
  my $lead;
  my ($wrf,$ref);
  my @levels;
  my $level;
  my $l;
  my (@wrf_stats,@ref_stats);
  my $fn;
  my $max_count;

  foreach $lead (@uprPlotHours) {
     $lead = sprintf "%02d",$lead;
     push(@leads,$lead);
  }

  foreach $lead (@leads) {

     print "lead = $lead\n";
     next if (! -e "upr_stats_wrf_${station}_${lead}hrs.txt" ||
              ! -e "upr_stats_ref_${station}_${lead}hrs.txt");

     open(WRF,"upr_stats_wrf_${station}_${lead}hrs.txt");
     open(REF,"upr_stats_ref_${station}_${lead}hrs.txt");

     @levels = ();
     @wrf_t_bs = ();
     @wrf_t_rs = ();
     @wrf_t_ms = ();
     @wrf_t_ns = ();
     @wrf_rh_bs = ();
     @wrf_rh_rs = ();
     @wrf_rh_ms = ();
     @wrf_rh_ns = ();
     @wrf_ws_bs = ();
     @wrf_ws_rs = ();
     @wrf_ws_ms = ();
     @wrf_ws_ns = ();
     @wrf_wd_bs = ();
     @wrf_wd_rs = ();
     @wrf_wd_ms = ();
     @wrf_wd_ns = ();
     @wrf_gh_bs = ();
     @wrf_gh_rs = ();
     @wrf_gh_ms = ();
     @wrf_gh_ns = ();

     @ref_t_bs = ();
     @ref_t_rs = ();
     @ref_t_ms = ();
     @ref_t_ns = ();
     @ref_rh_bs = ();
     @ref_rh_rs = ();
     @ref_rh_ms = ();
     @ref_rh_ns = ();
     @ref_ws_bs = ();
     @ref_ws_rs = ();
     @ref_ws_ms = ();
     @ref_ws_ns = ();
     @ref_wd_bs = ();
     @ref_wd_rs = ();
     @ref_wd_ms = ();
     @ref_wd_ns = ();
     @ref_gh_bs = ();
     @ref_gh_rs = ();
     @ref_gh_ms = ();
     @ref_gh_ns = ();

     while (($wrf=<WRF>) && ($ref=<REF>)) {
        @wrf_stats = split " ",$wrf;
        @ref_stats = split " ",$ref;

        push(@levels,$wrf_stats[0]);
        push(@wrf_t_bs,$wrf_stats[1]);
        push(@wrf_t_rs,$wrf_stats[2]);
        push(@wrf_t_ms,$wrf_stats[3]);
        push(@wrf_t_ns,$wrf_stats[4]);
        push(@wrf_rh_bs,$wrf_stats[5]);
        push(@wrf_rh_rs,$wrf_stats[6]);
        push(@wrf_rh_ms,$wrf_stats[7]);
        push(@wrf_rh_ns,$wrf_stats[8]);
        push(@wrf_ws_bs,$wrf_stats[9]);
        push(@wrf_ws_rs,$wrf_stats[10]);
        push(@wrf_ws_ms,$wrf_stats[11]);
        push(@wrf_ws_ns,$wrf_stats[12]);
        push(@wrf_wd_bs,$wrf_stats[13]);
        push(@wrf_wd_rs,$wrf_stats[14]);
        push(@wrf_wd_ms,$wrf_stats[15]);
        push(@wrf_wd_ns,$wrf_stats[16]);
        push(@wrf_gh_bs,$wrf_stats[17]);
        push(@wrf_gh_rs,$wrf_stats[18]);
        push(@wrf_gh_ms,$wrf_stats[19]);
        push(@wrf_gh_ns,$wrf_stats[20]);
   
        push(@ref_t_bs,$ref_stats[1]);
        push(@ref_t_rs,$ref_stats[2]);
        push(@ref_t_ms,$ref_stats[3]);
        push(@ref_t_ns,$ref_stats[4]);
        push(@ref_rh_bs,$ref_stats[5]);
        push(@ref_rh_rs,$ref_stats[6]);
        push(@ref_rh_ms,$ref_stats[7]);
        push(@ref_rh_ns,$ref_stats[8]);
        push(@ref_ws_bs,$ref_stats[9]);
        push(@ref_ws_rs,$ref_stats[10]);
        push(@ref_ws_ms,$ref_stats[11]);
        push(@ref_ws_ns,$ref_stats[12]);
        push(@ref_wd_bs,$ref_stats[13]);
        push(@ref_wd_rs,$ref_stats[14]);
        push(@ref_wd_ms,$ref_stats[15]);
        push(@ref_wd_ns,$ref_stats[16]);
        push(@ref_gh_bs,$ref_stats[17]);
        push(@ref_gh_rs,$ref_stats[18]);
        push(@ref_gh_ms,$ref_stats[19]);
        push(@ref_gh_ns,$ref_stats[20]);

        ### T:

        $fn = "upr_plots_t_${station}_${lead}hrs_${YYYYMM}.ps";

        open(GMT,"| pstext -JX6.7/0.5 -R0/100/-10/10 -X1 -Y7 -K > $fn");
        print GMT "0 0 14 0 5 ML Month $month/$year\n";
        print GMT "25 0 14 0 5 ML Domain $domain\n";
        print GMT "50 0 14 0 5 ML Station: ${station} $sname{$station}";
        close(GMT);

        # T BIAS:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{t}{bias}/100/1000 -Bf1a5:\"T BIAS (K)\":/a100g100:\"Pressure (hPa)\":WSen -W1.5p/0/255/0 -Y-3 -O -K >> $fn"); 

        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_t_bs[$l] $level\n" if ($level < 1050 && $wrf_t_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -Wta -O -K >> $fn");
        print GMT "0 100\n0 1000";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_t_bs[$l] $level\n" if ($level < 1050 && $ref_t_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1pt5_3:2 -O -K >> $fn");
        print OUT "0 100\n0 1000";
        close(GMT);
     
        # T RMSE:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{t}{rmse}/100/1000 -Bf1a5:\"T RMSE (K)\":/a100g100:\"Pressure (hPa)\":wSen -W1.5p/0/255/0 -X2.35 -O -K >> $fn");
     
        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_t_rs[$l] $level\n" if ($level < 1050 && $wrf_t_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_t_rs[$l] $level\n" if ($level < 1050 && $ref_t_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        # T MAE:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{t}{rmse}/100/1000 -Bf1a5:\"T MAE (K)\":/a100g100:\"Pressure (hPa)\":wSen -W1.5p/0/255/0 -X2.35 -O -K >> $fn");
     
        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_t_ms[$l] $level\n" if ($level < 1050 && $wrf_t_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_t_ms[$l] $level\n" if ($level < 1050 && $ref_t_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        $max_count = 0;
        foreach $l (0..$#levels) {
           $max_count = $wrf_t_ns[$l] if ($wrf_t_ns[$l] > $max_count);
        }

        # Annotate

        open(GMT,"| pstext -JX6.7/1 -R0/100/-10/10 -X-4.7 -Y-1.5 -O -K >> $fn");
        print GMT "0 0 12 0 5 ML ${lead}-hour FCST";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/0/255/0 -O -K >> $fn");
        print GMT "20 0 \n 30 0";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
        print GMT "50 0 \n 60 0";
        close(GMT);

        open(GMT,"| pstext -JX -R -O -K >> $fn");
        print GMT "32 0 12 0 5 ML WRF";
        close(GMT);

        open(GMT,"| pstext -JX -R -O -K >> $fn");
        print GMT "62 0 12 0 5 ML REF";
        close(GMT);

        open(GMT,"| pstext -JX -R -O >> $fn");
        print GMT "70 0 12 0 5 ML $max_count soundings";
        close(GMT);

        ### RH:

        $fn = "upr_plots_rh_${station}_${lead}hrs_${YYYYMM}.ps";

        open(GMT,"| pstext -JX6.7/0.5 -R0/100/-10/10 -X1 -Y7 -K > $fn");
        print GMT "0 0 14 0 5 ML Month $month/$year\n";
        print GMT "25 0 14 0 5 ML Domain $domain\n";
        print GMT "50 0 14 0 5 ML Station: ${station} $sname{$station}";
        close(GMT);

        # RH BIAS:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{rh}{bias}/100/1000 -Bf1a5:\"RH BIAS (%)\":/a100g100:\"Pressure (hPa)\":WSen -W1.5p/0/255/0 -Y-3 -O -K >> $fn"); 

        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_rh_bs[$l] $level\n" if ($level < 1050 && $wrf_rh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -Wta -O -K >> $fn");
        print GMT "0 100\n0 1000";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_rh_bs[$l] $level\n" if ($level < 1050 && $ref_rh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1pt5_3:2 -O -K >> $fn");
        print OUT "0 100\n0 1000";
        close(GMT);
     
        # RH RMSE:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{rh}{rmse}/100/1000 -Bf1a5:\"RH RMSE (%)\":/a100g100:\"Pressure (hPa)\":wSen -W1.5p/0/255/0 -X2.35 -O -K >> $fn");
     
        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_rh_rs[$l] $level\n" if ($level < 1050 && $wrf_rh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_rh_rs[$l] $level\n" if ($level < 1050 && $ref_rh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        # RH MAE:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{rh}{rmse}/100/1000 -Bf1a5:\"RH MAE (%)\":/a100g100:\"Pressure (hPa)\":wSen -W1.5p/0/255/0 -X2.35 -O -K >> $fn");
     
        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_rh_ms[$l] $level\n" if ($level < 1050 && $wrf_rh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_rh_ms[$l] $level\n" if ($level < 1050 && $ref_rh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        $max_count = 0;
        foreach $l (0..$#levels) {
           $max_count = $wrf_rh_ns[$l] if ($wrf_rh_ns[$l] > $max_count);
        }

        # Annotate

        open(GMT,"| pstext -JX6.7/1 -R0/100/-10/10 -X-4.7 -Y-1.5 -O -K >> $fn");
        print GMT "0 0 12 0 5 ML ${lead}-hour FCST";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/0/255/0 -O -K >> $fn");
        print GMT "20 0 \n 30 0";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
        print GMT "50 0 \n 60 0";
        close(GMT);

        open(GMT,"| pstext -JX -R -O -K >> $fn");
        print GMT "32 0 12 0 5 ML WRF";
        close(GMT);

        open(GMT,"| pstext -JX -R -O -K >> $fn");
        print GMT "62 0 12 0 5 ML REF";
        close(GMT);

        open(GMT,"| pstext -JX -R -O >> $fn");
        print GMT "70 0 12 0 5 ML $max_count soundings";
        close(GMT);

        ### WS:

        $fn = "upr_plots_ws_${station}_${lead}hrs_${YYYYMM}.ps";

        open(GMT,"| pstext -JX6.7/0.5 -R0/100/-10/10 -X1 -Y7 -K > $fn");
        print GMT "0 0 14 0 5 ML Month $month/$year\n";
        print GMT "25 0 14 0 5 ML Domain $domain\n";
        print GMT "50 0 14 0 5 ML Station: ${station} $sname{$station}";
        close(GMT);

        # WS BIAS:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{ws}{bias}/100/1000 -Bf1a5:\"WSPD BIAS (m s\@+-1\@+)\":/a100g100:\"Pressure (hPa)\":WSen -W1.5p/0/255/0 -Y-3 -O -K >> $fn"); 

        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_ws_bs[$l] $level\n" if ($level < 1050 && $wrf_ws_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -Wta -O -K >> $fn");
        print GMT "0 100\n0 1000";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_ws_bs[$l] $level\n" if ($level < 1050 && $ref_ws_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1pt5_3:2 -O -K >> $fn");
        print OUT "0 100\n0 1000";
        close(GMT);
     
        # WS RMSE:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{ws}{rmse}/100/1000 -Bf1a5:\"WSPD RMSE (m s\@+-1\@+)\":/a100g100:\"Pressure (hPa)\":wSen -W1.5p/0/255/0 -X2.35 -O -K >> $fn");
     
        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_ws_rs[$l] $level\n" if ($level < 1050 && $wrf_ws_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_ws_rs[$l] $level\n" if ($level < 1050 && $ref_ws_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        # WS MAE:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{ws}{rmse}/100/1000 -Bf1a5:\"WSPD MAE (m s\@+-1\@+)\":/a100g100:\"Pressure (hPa)\":wSen -W1.5p/0/255/0 -X2.35 -O -K >> $fn");
     
        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_ws_ms[$l] $level\n" if ($level < 1050 && $wrf_ws_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_ws_ms[$l] $level\n" if ($level < 1050 && $ref_ws_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        $max_count = 0;
        foreach $l (0..$#levels) {
           $max_count = $wrf_ws_ns[$l] if ($wrf_ws_ns[$l] > $max_count);
        }

        # Annotate

        open(GMT,"| pstext -JX6.7/1 -R0/100/-10/10 -X-4.7 -Y-1.5 -O -K >> $fn");
        print GMT "0 0 12 0 5 ML ${lead}-hour FCST";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/0/255/0 -O -K >> $fn");
        print GMT "20 0 \n 30 0";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
        print GMT "50 0 \n 60 0";
        close(GMT);

        open(GMT,"| pstext -JX -R -O -K >> $fn");
        print GMT "32 0 12 0 5 ML WRF";
        close(GMT);

        open(GMT,"| pstext -JX -R -O -K >> $fn");
        print GMT "62 0 12 0 5 ML REF";
        close(GMT);

        open(GMT,"| pstext -JX -R -O >> $fn");
        print GMT "70 0 12 0 5 ML $max_count soundings";
        close(GMT);

        ### WD:

        $fn = "upr_plots_wd_${station}_${lead}hrs_${YYYYMM}.ps";

        open(GMT,"| pstext -JX6.7/0.5 -R0/100/-10/10 -X1 -Y7 -K > $fn");
        print GMT "0 0 14 0 5 ML Month $month/$year\n";
        print GMT "25 0 14 0 5 ML Domain $domain\n";
        print GMT "50 0 14 0 5 ML Station: ${station} $sname{$station}";
        close(GMT);

        # WD BIAS:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{wd}{bias}/100/1000 -Bf5a10:\"WDIR BIAS (\272)\":/a100g100:\"Pressure (hPa)\":WSen -W1.5p/0/255/0 -Y-3 -O -K >> $fn"); 

        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_wd_bs[$l] $level\n" if ($level < 1050 && $wrf_wd_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -Wta -O -K >> $fn");
        print GMT "0 100\n0 1000";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_wd_bs[$l] $level\n" if ($level < 1050 && $ref_wd_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1pt5_3:2 -O -K >> $fn");
        print OUT "0 100\n0 1000";
        close(GMT);
     
        # WD RMSE:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{wd}{rmse}/100/1000 -Bf10a50:\"WDIR RMSE (\272)\":/a100g100:\"Pressure (hPa)\":wSen -W1.5p/0/255/0 -X2.35 -O -K >> $fn");
     
        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_wd_rs[$l] $level\n" if ($level < 1050 && $wrf_wd_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_wd_rs[$l] $level\n" if ($level < 1050 && $ref_wd_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        # WD MAE:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{wd}{rmse}/100/1000 -Bf10a50:\"WDIR MAE (\272)\":/a100g100:\"Pressure (hPa)\":wSen -W1.5p/0/255/0 -X2.35 -O -K >> $fn");
     
        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_wd_ms[$l] $level\n" if ($level < 1050 && $wrf_wd_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_wd_ms[$l] $level\n" if ($level < 1050 && $ref_wd_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        $max_count = 0;
        foreach $l (0..$#levels) {
           $max_count = $wrf_wd_ns[$l] if ($wrf_wd_ns[$l] > $max_count);
        }

        # Annotate

        open(GMT,"| pstext -JX6.7/1 -R0/100/-10/10 -X-4.7 -Y-1.5 -O -K >> $fn");
        print GMT "0 0 12 0 5 ML ${lead}-hour FCST";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/0/255/0 -O -K >> $fn");
        print GMT "20 0 \n 30 0";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
        print GMT "50 0 \n 60 0";
        close(GMT);

        open(GMT,"| pstext -JX -R -O -K >> $fn");
        print GMT "32 0 12 0 5 ML WRF";
        close(GMT);

        open(GMT,"| pstext -JX -R -O -K >> $fn");
        print GMT "62 0 12 0 5 ML REF";
        close(GMT);

        open(GMT,"| pstext -JX -R -O >> $fn");
        print GMT "70 0 12 0 5 ML $max_count soundings";
        close(GMT);

        # GH:

        $fn = "upr_plots_gh_${station}_${lead}hrs_${YYYYMM}.ps";

        open(GMT,"| pstext -JX6.7/0.5 -R0/100/-10/10 -X1 -Y7 -K > $fn");
        print GMT "0 0 14 0 5 ML Month $month/$year\n";
        print GMT "25 0 14 0 5 ML Domain $domain\n";
        print GMT "50 0 14 0 5 ML Station: ${station} $sname{$station}";
        close(GMT);

        # GH BIAS:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{gh}{bias}/100/1000 -Bf10a10:\"HGT BIAS (m)\":/a100g100:\"Pressure (hPa)\":WSen -W1.5p/0/255/0 -Y-3 -O -K >> $fn"); 

        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_gh_bs[$l] $level\n" if ($level < 1050 && $wrf_gh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -Wta -O -K >> $fn");
        print GMT "0 100\n0 1000";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_gh_bs[$l] $level\n" if ($level < 1050 && $ref_gh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1pt5_3:2 -O -K >> $fn");
        print OUT "0 100\n0 1000";
        close(GMT);
     
        # GH RMSE:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{gh}{rmse}/100/1000 -Bf10a10:\"HGT RMSE (m)\":/a100g100:\"Pressure (hPa)\":wSen -W1.5p/0/255/0 -X2.35 -O -K >> $fn");
     
        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_gh_rs[$l] $level\n" if ($level < 1050 && $wrf_gh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_gh_rs[$l] $level\n" if ($level < 1050 && $ref_gh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        # GH MAE:

        open(GMT,"| psxy -JX2/-3p0.1 -R$plot_range{gh}{rmse}/100/1000 -Bf10a10:\"HGT MAE (m)\":/a100g100:\"Pressure (hPa)\":wSen -W1.5p/0/255/0 -X2.35 -O -K >> $fn");
     
        $l = 0;
        foreach $level (@levels) {
           print GMT "$wrf_gh_ms[$l] $level\n" if ($level < 1050 && $wrf_gh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");

        $l = 0;
        foreach $level (@levels) {
           print GMT "$ref_gh_ms[$l] $level\n" if ($level < 1050 && $ref_gh_ns[$l] > 0);
           $l++;
        }
        close(GMT);

        $max_count = 0;
        foreach $l (0..$#levels) {
           $max_count = $wrf_gh_ns[$l] if ($wrf_gh_ns[$l] > $max_count);
        }

        # Annotate

        open(GMT,"| pstext -JX6.7/1 -R0/100/-10/10 -X-4.7 -Y-1.5 -O -K >> $fn");
        print GMT "0 0 12 0 5 ML ${lead}-hour FCST";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/0/255/0 -O -K >> $fn");
        print GMT "20 0 \n 30 0";
        close(GMT);

        open(GMT,"| psxy -JX -R -W1.5p/255/0/0 -O -K >> $fn");
        print GMT "50 0 \n 60 0";
        close(GMT);

        open(GMT,"| pstext -JX -R -O -K >> $fn");
        print GMT "32 0 12 0 5 ML WRF";
        close(GMT);

        open(GMT,"| pstext -JX -R -O -K >> $fn");
        print GMT "62 0 12 0 5 ML REF";
        close(GMT);

        open(GMT,"| pstext -JX -R -O >> $fn");
        print GMT "70 0 12 0 5 ML $max_count soundings";
        close(GMT);

     } # end while <WRF> and <REF>
     close(WRF);
     close(REF);

  } # end $lead loop

  return;

}
#
#
#
sub parse_config {

  my ($lstation,$llevel,$lfcst,$luprhrs);
  my ($fcst,$weight);
  my @hrs;
  my $sid;

  open(CONFIG,"$config_file");

  while (<CONFIG>) {

    chomp;

    next if(length == 0);

    if (/STATIONS/) {
       $lstation = 1;
       $llevel = 0;
       $lfcst = 0;
       $luprhrs = 0;
       next;
    } elsif (/LEVELS/) {
       $lstation = 0;
       $llevel = 1;
       $lfcst = 0;
       $luprhrs = 0;
       next;
    } elsif (/FCST/) {
       $lstation = 0;
       $llevel = 0;
       $lfcst = 1;
       $luprhrs = 0;
       next;
    } elsif (/UPR/) {
       $lstation = 0;
       $llevel = 0;
       $lfcst = 0;
       $luprhrs = 1;
       next;
    } elsif (/BIN/) {
       $lstation = 0;
       $llevel = 0;
       $lfcst = 0;
       $luprhrs = 0;
       next;
    }

    if ($lfcst) {
       ($fcst,$weight) = split;
       push(@hrs,$fcst);
    } elsif ($luprhrs) {
       push(@uprPlotHours,$_);
    }

    if ($lstation) {
       /(\S+)\s+(\S+)\s+(\S+)\s+\"(.*)\"/;
       $sid = $1;
       push(@stations,$sid);
       $sname{$sid} = $4;
    }

  }

  close(CONFIG);

  $VERI_LENGTH = pop(@hrs);

  return;

}
#
# 
#
sub histogram {

  my ($domain,$station) = @_;
  my ($freq,$ref_freq);
  my $minmax;
  my ($min_x,$max_x,$min_y,$max_y);
  my $y_ub;
  my ($var,$level,$lead,$model);
  my $x_range;
  my $dx;
  my %x_title = ('t' => 'T error (K)',
                 'rh' => 'RH error (%)',
                 'ws' => 'Wind Speed Error (m s@+-1@+)',
                 'wd' => 'Wind Direction Error (deg)',
                 'gh' => 'Height Error (m)');
  my %xtick = ('t' => '1',
               'rh' => '10',
               'ws' => '1',
               'wd' => '10',
               'gh' => '10');

  my %xticka = ('t' => '1',
                'rh' => '10',
                'ws' => '1',
                'wd' => '20',
                'gh' => '10');

  my %width = ('t' => '0.25',
               'rh' => '2.5',
               'ws' => '0.25',
               'wd' => '2.5',
               'gh' => '2.5');


  my ($ps,$gif);
  my ($x,$y);

  foreach $freq (<*freq*wrf_${station}.txt>) {

     next if($freq =~ /psfc/);

     $minmax = `minmax -C $freq`;
     ($min_x,$max_x,$min_y,$max_y) = split " ",$minmax;
     next if($min_y == 0 && $max_y == 0);

     $freq =~ /(\w+)_freq_(\w+)_(\d+)hr_(\w+)_(\d+)\.txt/;

     $var = $1;
     $level = $2;
     $lead = $3;
     $model = uc($4);

     $ref_freq = $freq;
     $ref_freq =~ s/wrf/ref/;

     $ps = $freq;
     $ps =~ s/txt$/ps/;
     $ps =~ s/_wrf//;
     
     $gif = $ps;
     $gif =~ s/ps$/gif/;

     if (-s "$ref_freq") {
        $minmax = `cat $freq $ref_freq | minmax -C`;
     } else {
        $minmax = `minmax -C $freq`;
     }

     chomp($minmax);
     ($min_x,$max_x,$min_y,$max_y) = split " ",$minmax;
     $y_ub = (int($max_y/10)+1)*10;

     $min_x -= 0.5*$xtick{$var};
     $max_x += 0.5*$xtick{$var};

     $x_range="$min_x/$max_x";

     next if($max_y == 0);

     if (-s "$ref_freq") {

        $dx = 0.5*$width{$var};

        open(GMT,"| psxy -JX5/2 -R$x_range/0/$y_ub \\
          -Bf$xtick{$var}a$xticka{$var}:\"$x_title{$var}\":/f1a5:\"Frequency (%)\":WSen \\
          -Sb${width{$var}}u -G0/0/255 -W1p -X1.5 -Y6 -K > $ps");

        open(WRFIN,"$freq");
        while (<WRFIN>) {
           ($x,$y)=split;
           $x -= $dx;
           print GMT "$x $y\n";
        }
        close(WRFIN);
        close(GMT); 

        print "plot $ref_freq: psxy -JX -R -Sb${width{$var}}u -G255/0/0 -W1p -O -K >> $ps\n";
        open(GMT,"| psxy -JX -R -Sb${width{$var}}u -G255/0/0 -W1p -O -K >> $ps");
        open(REFIN,"$ref_freq");
        while (<REFIN>) {
           ($x,$y)=split;
           $x += $dx;
           print GMT "$x $y\n";
        }
        close(REFIN);
        close(GMT);

     } else {
        system("psxy $freq -JX5/2 -R$x_range/0/$y_ub \\
          -Bf$xtick{$var}a$xticka{$var}:\"$x_title{$var}\":/f1a5:\"Frequency (%)\":WSen \\
          -Sb${width{$var}}u -G0/0/255 -W1p -X1.5 -Y6 -K > $ps");
     }

     system("echo 0 0 14 0 5 ML D${domain} ${lead}-hr FCST Station: ${station} $sname{$station} | pstext -JX5/0.25 -R0/10/-10/10 -Y2 -O -K >> $ps");

     system("echo -e '2.5 0\n2.5 5' | psxy -JX5/0.25 -R0/100/0/10 -Sb5u -G0/0/255 -W1p -Y-2.65 -O -K >> $ps");

     system("echo -e '17.5 0\n17.5 5' | psxy -JX -R -Sb5u -G255/0/0 -W1p -O -K >> $ps");

     system("echo 6 0 12 0 5 BL WRF | pstext -JX -R -O -K >> $ps");
     system("echo 21 0 12 0 5 BL REF | pstext -JX -R -O -K >> $ps");

     system("convert -trim +repage -flatten $ps $gif");
     system("rm -f $ps");

  }
     
  return;

}
#
#
#
sub create_config {

  my $station = $_[0];
  my ($lstation,$llevel,$lfcst,$luprhrs);
  my ($fcst,$weight);
  my @hrs;
  my ($sid,$lat,$lon);
  my ($domain,$d,$d02);
  my $std_out;
  my $outside;
  my @dummy;

  system("ln -sf $GSJOBDIR/wps/geo_em*.nc .");

  open(CONFIG,"$config_file");
  open(NEWCONFIG,">config_file.$station");

  while (<CONFIG>) {

    chomp;

    if(length == 0) {
      print NEWCONFIG "$_\n";
      next;
    }

    if (/STATIONS/) {
       $lstation = 1;
       $llevel = 0;
       $lfcst = 0;
       $luprhrs = 0;
       print NEWCONFIG "$_\n";
       next;
    } elsif (/LEVELS/) {
       $lstation = 0;
       $llevel = 1;
       $lfcst = 0;
       $luprhrs = 0;
       print NEWCONFIG "$_\n";
       next;
    } elsif (/FCST/) {
       $lstation = 0;
       $llevel = 0;
       $lfcst = 1;
       $luprhrs = 0;
       print NEWCONFIG "$_\n";
       next;
    } elsif (/UPR/) {
       $lstation = 0;
       $llevel = 0;
       $lfcst = 0;
       $luprhrs = 1;
       print NEWCONFIG "$_\n";
       next;
    } elsif (/BIN/) {
       $lstation = 0;
       $llevel = 0;
       $lfcst = 0;
       $luprhrs = 0;
       print NEWCONFIG "$_\n";
       next;
    }

    if ($lstation) {
       /(\S+)\s+(\S+)\s+(\S+)\s+\"(.*)\"/;
       $sid = $1;
       $lat = $2;
       $lon = $3;

       if ($sid eq $station) {
          $domain = 0;
          for($d=3;$d>=1;$d--) {
             $d02 = sprintf "%02d",$d;
             $std_out = `$EXECUTABLE_ARCHIVE/WRF_lltoxy.exe -f geo_em.d${d02}.nc -lat $lat -lon $lon -useGeoFile`;
             ($outside,@dummy) = split " ",$std_out;
             if ($outside) {
             } else {
                $domain = $d;
                last;
             }
          }
          print NEWCONFIG "$_\n";
       }
    } else {
       print NEWCONFIG "$_\n";
    }

  }

  close(CONFIG);
  close(NEWCONFIG);

  return $domain;

}
#
#
#
sub annotate_property {

  my $max_count = $_[0];
  my ($ymax,$ytick);
  my ($tenth,$half);

  if ($max_count <= 20) {
     $ymax  = 20;
     $ytick = 'f1a10';
  } elsif ($max_count <= 50) {
     $ymax  = 50;
     $ytick = 'f5a25';
  } elsif ($max_count <= 100) {
     $ymax  = 100;
     $ytick = 'f10a50';
  } elsif ($max_count <= 500) {
     $ymax  = 500;
     $ytick = 'f100a250';
  } elsif ($max_count <= 1000) {
     $ymax  = 1000;
     $ytick = 'f100a500';
  } else {
     $ymax  = (int($max_count/1000)+1)*1000;
     $tenth = $ymax/10;
     $half  = $ymax/2;
     $ytick = "f${tenth}a${half}";
  }

  return ($ymax,$ytick);
}

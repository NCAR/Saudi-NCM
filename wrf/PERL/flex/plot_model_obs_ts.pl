#!/usr/bin/perl
use Getopt::Std;
use FileHandle;

getopts('c:d:j:l:R:');

our $WRF_PAIRS_DIR;
our $REF_PAIRS_DIR;
our $RUNDIR;
our $JOB_LOC;
our $DEST_SERVER;
our $VERI_LENGTH;
our $TS_ARCHIVE_DIR;
our $EXECUTABLE_ARCHIVE;
our $GMT_BIN;

my $VERI_LIST;

my %GRID;
my $GSJOBDIR;

my $changeDays;

if ($opt_c) {
   $VERI_LIST = $opt_c;
} else {
   die "Usage: $0 -c <config_file> -j <GSJOBDIR> -d <number of days in calculation> [-l <lag in hours>]\n";
}

if ($opt_d) {
   $days = $opt_d;
} else {
   die "Usage: $0 -c <config_file> -j <GSJOBDIR> -d <number of days in calculation> [-l <lag in hours>]\n";
}

if ($opt_j) {
   $GSJOBDIR = $opt_j;
   $ENV{GSJOBDIR} = $GSJOBDIR;
   $GSJOBDIR =~ /(\w+)$/;
   $ENV{GSJOBID} = $1;
} else {
   die "Usage: $0 -c <config_file> -j <GSJOBDIR> -d <number of days in calculation> [-l <lag in hours>]\n";
}

###

if (-e "$GSJOBDIR/scripts/env_vars.csh") {
   $MM5HOME=`grep 'setenv MM5HOME' $GSJOBDIR/scripts/env_vars.csh | awk '{print \$3}'| sed s/\\"//g`;
   $MM5HOME =~ s/\$\{LOGNAME\}/$ENV{LOGNAME}/ if ($MM5HOME =~ /LOGNAME/);
   $MM5HOME =~ s/\$\{HOME\}/\/home\/$ENV{LOGNAME}/ if ($MM5HOME =~ /HOME/);
   chomp($MM5HOME);
   $ENV{MM5HOME} = $MM5HOME;

   $PERL_ARCHIVE=`grep 'setenv PERL_ARCHIVE' $GSJOBDIR/scripts/env_vars.csh | awk '{print \$3}'| sed s/\\"//g`;
   $PERL_ARCHIVE =~ s/\$\{MM5HOME\}/${MM5HOME}/ if ($PERL_ARCHIVE =~ /\{MM5HOME\}/);
   $PERL_ARCHIVE =~ s/\$MM5HOME/${MM5HOME}/ if ($PERL_ARCHIVE =~ /\$MM5HOME/);
   chomp($PERL_ARCHIVE);
   $ENV{PERL_ARCHIVE} = $PERL_ARCHIVE;
} else {
   die "$GSJOBDIR/scripts/env_vars.csh does not exist!\n";
}

if (-e "$PERL_ARCHIVE/TimeUtil.pm") {
   require "$PERL_ARCHIVE/TimeUtil.pm";
} else {
   die "$PERL_ARCHIVE/TimeUtil.pm does not exist!\n";
}

if (-e "$GSJOBDIR/statsinput.pl") {
   require "$GSJOBDIR/statsinput.pl";
} else {
   die "$GSJOBDIR/statsinput.pl does not exist!\n";
}

if (-e "$GSJOBDIR/flexinput.pl") {
   require "$GSJOBDIR/flexinput.pl";
} else {
   die "$GSJOBDIR/flexinput.pl does not exist!\n";
}

if (-e "$GSJOBDIR/postprocinput.pl") {
   require "$GSJOBDIR/postprocinput.pl";
} else {
   die "$GSJOBDIR/postprocinput.pl does not exist!\n";
}

if (-e "$GSJOBDIR/verifyinput.pl") {
   require "$GSJOBDIR/verifyinput.pl";
} else {
   die "$GSJOBDIR/verifyinput.pl does not exist!\n";
}

if ($opt_R) {
   $RUNDIR = $opt_R;
}

### GMT bin path

if (! $GMT_BIN) {
   $GMT_BIN = '/opt/gmt/bin';
}

$ENV{PATH} = "${GMT_BIN}:$ENV{PATH}";

print "MM5HOME = $MM5HOME\n";
print "EXECUTABLE_ARCHIVE = $EXECUTABLE_ARCHIVE\n";
print "PERL_ARCHIVE = $PERL_ARCHIVE\n";
print "WRF_PAIRS_DIR $WRF_PAIRS_DIR\n";
print "REF_PAIRS_DIR $REF_PAIRS_DIR\n";
print "RUNDIR = $RUNDIR\n";
print "JOB_LOC = $JOB_LOC\n";

my $OBS_TS_DIR = "$RUNDIR/obs_ts";

if (! defined($TS_ARCHIVE_DIR)) {
   die "Please define \$TS_ARCHIVE_DIR in $GSJOBDIR/verifyinput.pl!\n This directory is required for the purpose of archiving time-series plots.\n";
}

my %stations;
my %slat;
my %slon;
my %sname;
my %levels;

my $missing=-8888;

my $fcst_length = int($VERI_LENGTH/12)*12; # so the extra 6-hour is not considered

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=gmtime(time-$opt_l*3600-$fcst_length*3600);
$latestCycle = sprintf "%4d%02d%02d%02d",$year+1900,$mon+1,$mday,$hour;

$changeDays = check_archive($latestCycle);

if ($changeDays) {
   print "overriding days to $changeDays\n";
   $days = $changeDays;
}

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=gmtime(time-$opt_l*3600-($days-0.5)*86400-$fcst_length*3600);
$oldestCycle = sprintf "%4d%02d%02d%02d",$year+1900,$mon+1,$mday,$hour;

print "latestCycle = $latestCycle ; oldestCycle = $oldestCycle\n";

system("mkdir -p $OBS_TS_DIR/$latestCycle");

$total_cycles = $days*2;

chdir "$OBS_TS_DIR";

&parse_stations();

for ($i=0; $i <= $days*2; $i++) {
    $cycle = advance_h($oldestCycle,$i*12); # since GFS only on 12-hr interval
    print "cycle = $cycle\n";

    $dateTimeString = substr($cycle,0,4) . '-' . substr($cycle,4,2) . '-' .
                      substr($cycle,6,2) . 'T' . substr($cycle,8,2) . ':00:00';

    $nbytes = 56;

    $WRF_SFC = "$WRF_PAIRS_DIR/sfc/fcst/${cycle}_veri_dat_GRM_P+FCST";
    $REF_SFC = `ls $REF_PAIRS_DIR/sfc/${cycle}_veri_dat_*`;
    chomp($REF_SFC);

    $REF_SFC =~ /_([A-Z0-9]+)$/;
    $REF_MODEL = $1 if($1);

    foreach $key (keys %stations) {
     #print "$key\n";
      $index_array_wrf{$key} = [];
      $index_array_obs{$key} = [];
    }

    open(WRF,"$WRF_SFC");

    seek(WRF,0,1);
    $bytes=read(WRF,$buf,$nbytes);

    ($year1,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
              $psfc_m,$psfc_o,$psfc_qc,
              $slp_m,$slp_o,$slp_qc,
              $ter_m,$ter_o,
              $t2_m,$t2_o,$t2_qc,
              $q_m,$q_o,$q_qc,
              $ws_m,$ws_o,$ws_qc,
              $wd_m,$wd_o,$wd_qc)=unpack("s6a4s20a8",$buf);

    seek(WRF,0,1);
    $bytes=read(WRF,$buf,$nbytes);

    ($year2,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
              $psfc_m,$psfc_o,$psfc_qc,
              $slp_m,$slp_o,$slp_qc,
              $ter_m,$ter_o,
              $t2_m,$t2_o,$t2_qc,
              $q_m,$q_o,$q_qc,
              $ws_m,$ws_o,$ws_qc,
              $wd_m,$wd_o,$wd_qc)=unpack("s6a4s20a8",$buf);

    if (abs($year2-$year1) > 1) {
       $nbytes = 64;
       $lstid = 1;
    } else {
       $lstid = 0;
    }

#
# wrf surface
#
    seek(WRF,0,0);
    while (read(WRF,$buf,$nbytes) > 0) {

       seek(IN,0,1);

       if ($lstid) {
          ($year,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
          $psfc_m,$psfc_o,$psfc_qc,
          $slp_m,$slp_o,$slp_qc,
          $ter_m,$ter_o,
          $t2_m,$t2_o,$t2_qc,
          $q_m,$q_o,$q_qc,
          $ws_m,$ws_o,$ws_qc,
          $wd_m,$wd_o,$wd_qc,$st_id)=unpack("s6a4s20a8",$buf);
       } else {
          ($year,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
          $psfc_m,$psfc_o,$psfc_qc,
          $slp_m,$slp_o,$slp_qc,
          $ter_m,$ter_o,
          $t2_m,$t2_o,$t2_qc,
          $q_m,$q_o,$q_qc,
          $ws_m,$ws_o,$ws_qc,
          $wd_m,$wd_o,$wd_qc)=unpack("s6a4s20",$buf);
       }

       $output_wrf = 0;
       $output_obs = 0;

       $rlat = sprintf "%.2f", $lat*0.01;
       $rlon = sprintf "%.2f", $lon*0.01;

       $latlon = "$rlat$rlon";

       if ($lstid) {
          $st_id =~ s/\s+$//;
          if (defined($stations{$st_id})) {
             $output_wrf = 1;
             $output_obs = 1;
          }
       } else {
          if(defined($stations{$latlon})) {
            $output_wrf = 1;
            $output_obs = 1;
            $st_id = $stations{$latlon};
          } else {
A:          foreach $y (-1,1) {
            foreach $x (-1,1) {
              $rlat = $lat*0.01+$y*0.01;
              $rlon = $lon*0.01+$x*0.01;
              $rlat = sprintf "%.2f",$rlat;
              $rlon = sprintf "%.2f",$rlon;
              $latlon = "$rlat$rlon";
              if(defined($stations{$latlon})) {
                $output_wrf = 1;
                $output_obs = 1;
                $st_id = $stations{$latlon};
                last A;
              }
            }
            }
          }
       }

       $rt2_m = $t2_m*0.01 if ($t2_m > $missing);
       $rt2_o = $t2_o*0.01 if ($t2_o > $missing);
       $rws_m = $ws_m*0.01 if ($ws_m > $missing);
       $rws_o = $ws_o*0.01 if ($ws_o > $missing);
       $rpsfc_m = $psfc_m*0.1 if ($psfc_m > $missing);
       $rpsfc_o = $psfc_o*0.1 if ($psfc_o > $missing);

       if ($t2_m > $missing && $q_m > $missing && $psfc_m > $missing) {
          $rh_m = rh_from_q($psfc_m,$t2_m,$q_m);
       } else {
          $rh_m = $missing;
       }

       if ($t2_o > $missing && $q_o > $missing && $psfc_o > $missing) {
          $rh_o = rh_from_q($psfc_o,$t2_o,$q_o);
       } else {
          $rh_o = $missing;
       }

       $this_date = sprintf "%4d%04d%04d",$year,$monthday,$hourmin;
       $date = nearest_hour($this_date);
       $index = indexcal($date,$cycle);

       foreach $i (@{$index_array_wrf{$st_id}}) {
         if ($i == $index) {
            $output_wrf = 0;
            last;
         }
       }

       foreach $i (@{$index_array_obs{$st_id}}) {
         if ($i == $index) {
            $output_obs = 0;
            last;
         }
       }

       $output_wrf = 0 if($index%3 != 0);  # output only every 3-hr lead time
       $output_obs = 0 if($index%3 != 0);  # output only every 3-hr lead time

       ### is this the right domain solution for wrf?

       $output_wrf = 0 if ($domain_id != $GRID{$st_id});

       ###

       if ($output_wrf) {
          $lead = sprintf "%02d",$index;
          open(WRF_OUT,">>$latestCycle/wrf_sfc_${lead}hr_${st_id}.txt");
          print WRF_OUT "${dateTimeString} $rt2_m $rh_m $rws_m $wd_m $rpsfc_m\n";
          close(WRF_OUT);
          push @{ $index_array_wrf{$st_id} }, $index;
       }

       if ($output_obs) {
          $lead = sprintf "%02d",$index;
          open(OBS_OUT,">>$latestCycle/obs_sfc_${lead}hr_${st_id}.txt");
          print OBS_OUT "${dateTimeString} $rt2_o $rh_o $rws_o $wd_o $rpsfc_o\n";
          close(OBS_OUT);
          push @{ $index_array_obs{$st_id} }, $index;
       }
    }

    close(WRF);
#
# reference model surface
#
    foreach $key (keys %stations) {
      $index_array{$key} = [];
    }

    open(REF,"$REF_SFC");

    seek(REF,0,0);

    while (read(REF,$buf,$nbytes) > 0) {

       seek(IN,0,1);

       if ($lstid) {
          ($year,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
          $psfc_m,$psfc_o,$psfc_qc,
          $slp_m,$slp_o,$slp_qc,
          $ter_m,$ter_o, $t2_m,$t2_o,$t2_qc,
          $q_m,$q_o,$q_qc,
          $ws_m,$ws_o,$ws_qc,
          $wd_m,$wd_o,$wd_qc,$st_id)=unpack("s6a4s20a8",$buf);
       } else {
          ($year,$monthday,$hourmin,$lat,$lon,$domain_id,$platform,
          $psfc_m,$psfc_o,$psfc_qc,
          $slp_m,$slp_o,$slp_qc,
          $ter_m,$ter_o,
          $t2_m,$t2_o,$t2_qc,
          $q_m,$q_o,$q_qc,
          $ws_m,$ws_o,$ws_qc,
          $wd_m,$wd_o,$wd_qc)=unpack("s6a4s20",$buf);
       }

       $output = 0;

       $rlat = sprintf "%.2f", $lat*0.01;
       $rlon = sprintf "%.2f", $lon*0.01;

       $latlon = "$rlat$rlon";

       if ($lstid) {
          $st_id =~ s/\s+$//;
          if(defined($stations{$st_id})) {
            $output = 1;
          }
       } else {
          if(defined($stations{$latlon})) {
            $output = 1;
            $st_id = $stations{$latlon};
          } else {
B:          foreach $y (-1,1) {
            foreach $x (-1,1) {
              $rlat = $lat*0.01+$y*0.01;
              $rlon = $lon*0.01+$x*0.01;
              $rlat = sprintf "%.2f",$rlat;
              $rlon = sprintf "%.2f",$rlon;
              $latlon = "$rlat$rlon";
              if(defined($stations{$latlon})) {
                $output = 1;
                $st_id = $stations{$latlon};
                last B;
              }
            }
            }
          }
       }

       $rt2_m = $t2_m*0.01 if ($t2_m > $missing);
       $rws_m = $ws_m*0.01 if ($ws_m > $missing);
       $rpsfc_m = $psfc_m*0.1 if ($psfc_m > $missing);

       if ($t2_m > $missing && $q_m > $missing && $psfc_m > $missing) {
          $rh_m = rh_from_q($psfc_m,$t2_m,$q_m);
       } else {
          $rh_m = $missing;
       }

       $this_date = sprintf "%4d%04d%04d",$year,$monthday,$hourmin;
       $date = nearest_hour($this_date);
       $index = indexcal($date,$cycle);

       foreach $i (@{$index_array{$st_id}}) {
         if ($i == $index) {
            $output = 0;
            last;
         }
       }

       $output = 0 if($index%3 != 0);  # output only every 3-hr lead time

       if ($output) {
          $lead = sprintf "%02d",$index;
          open(REF_OUT,">>$latestCycle/ref_sfc_${lead}hr_${st_id}.txt");
          print REF_OUT "${dateTimeString} $rt2_m $rh_m $rws_m $wd_m $rpsfc_m\n";
          close(REF_OUT);
          push @{ $index_array{$st_id} }, $index;
       }
    }

    close(REF);

    $WRF_UPR_DIR = "$WRF_PAIRS_DIR/upr/fcst";
    $REF_UPR_DIR = "$REF_PAIRS_DIR/upr";
#
# wrf soundings
#
    foreach $file (<$WRF_UPR_DIR/${cycle}_*>) {
       if (-s "$file") {
          print "processing file : $file\n";
          $file=~ /\d+_(\d+)_veri_dat_upr/;
          $valid_time = $1;
          $valid_hour = substr($valid_time,-2);
          if ($valid_hour == 11 || $valid_hour == 23 || $valid_hour == 35 ||
              $valid_hour == 47 || $valid_hour == 59 || $valid_hour == 71) {
              $valid_time = advance_h($valid_time,1);
          }

          foreach $key (keys %stations) {
            $cycle_array_wrf{$key} = [];
            $cycle_array_obs{$key} = [];
          }

          open(IN,"$file");

          while ($line=<IN>) {
            chomp $line;
            $l_index=($.-1)%41;
            if($l_index == 0) {
              @fields = split " ", $line;
              if ($#fields == 6) {
                 ($date,$st_id,$lat,$lon,$elevm,$elev,$id) = split " ",$line;
                 $lstid = 1;
              } elsif ($#fields == 5) {
                 ($date,$st_id,$lat,$lon,$elev,$id)=split " ",$line;
                 $lstid = 1;
              } else {
                 ($date,$lat,$lon,$id)=split " ",$line;
                 $lstid = 0;
              }

              $output_wrf = 0;
              $output_obs = 0;

              $latlon = "$lat$lon";

              if ($lstid) {
                 if(defined($stations{$st_id})) {
                   $output_wrf = 1;
                   $output_obs = 1;
                 }
              } else {
                 if(defined($stations{$latlon}) && $id == 1) {
                   $output_wrf = 1;
                   $output_obs = 1;
                   $st_id = $stations{$latlon};
                 } else {
C:                 foreach $y (-1,1) {
                   foreach $x (-1,1) {
                     $rlat = $lat+$y*0.01;
                     $rlon = $lon+$x*0.01;
                     $rlat = sprintf "%.2f",$rlat;
                     $rlon = sprintf "%.2f",$rlon;
                     $latlon = "$rlat$rlon";
                     if(defined($stations{$latlon}) && $id == 1) {
                       $output_wrf = 1;
                       $output_obs = 1;
                       $st_id = $stations{$latlon};
                       last C;
                     }
                   }
                   }
                 }
              }

              $index = indexcal($valid_time,$cycle);

              foreach $i (@{$cycle_array_wrf{$st_id}}) {
                if ($i == $cycle) {
                   $output_wrf = 0;
                   last;
                }
              }

              foreach $i (@{$cycle_array_obs{$st_id}}) {
                if ($i == $cycle) {
                   $output_obs = 0;
                   last;
                }
              }

              $output_wrf = 0 if($index%12 != 0); # only every 12-hr lead time
              $output_obs = 0 if($index%12 != 0); # only every 12-hr lead time

              ### is this the right domain solution for wrf?

              $output_wrf = 0 if ($id != $GRID{$st_id});

              ###

              if ($output_wrf) {
                 $lead = sprintf "%02d",$index;
                 foreach $key (keys %levels) {
                    next if($key == 2001);
                    $fhw{$key} = FileHandle->new(">>$latestCycle/wrf_${key}mb_${lead}hr_${st_id}.txt");
                 }
                 push @{ $cycle_array_wrf{$st_id} }, $cycle;
              }

              if ($output_obs) {
                 $lead = sprintf "%02d",$index;
                 foreach $key (keys %levels) {
                    next if($key == 2001);
                    $fho{$key} = FileHandle->new(">>$latestCycle/obs_${key}mb_${lead}hr_${st_id}.txt");
                 }
                 push @{ $cycle_array_obs{$st_id} }, $cycle;
              }

            } else {
              ($p,$tm,$to,$qc_t,$qm,$qo,$qc_q,$rhm,$rho,$qc_rh,$wsm,$wso,$qc_ws,
               $wdm,$wdo,$qc_wd,$ghm,$gho,$qc_gh)=split " ",$line;

               if ($rhm == $missing || $tm == $missing) {
                  $tdm = $missing;
               } else {
                  $tdm = td_from_rh($rhm,$tm)  # rhm in %, $t in C
               }

               if ($rho == $missing || $to == $missing) {
                  $tdo = $missing;
               } else {
                  $tdo = td_from_rh($rho,$to)  # rhm in %, $t in C
               }

               if ($tm == $missing) {
                  $tmc = $missing;
               } else {
                  $tmc = $tm-273.15;
               }

               if ($to == $missing) {
                  $toc = $missing;
               } else {
                  $toc = $to-273.15;
               }

               if ($output_wrf) {
                  chop($p);
                  if(defined($levels{$p})) {
                    print { $fhw{$p} } "${dateTimeString} $tmc $rhm $wsm $wdm $ghm\n";
                  }
               }

               if ($output_obs) {
                  chop($p);
                  if(defined($levels{$p})) {
                    print { $fho{$p} } "${dateTimeString} $toc $rho $wso $wdo $gho\n";
                  }
               }

            }
          }

          close(IN);
       } 
    }
#
# ref soundings
#
    foreach $file (<$REF_UPR_DIR/${cycle}_*>) {
       if (-s "$file") {
          $file=~ /\d+_(\d+)_veri_dat_upr/;
          $valid_time = $1;

          foreach $key (keys %stations) {
            $cycle_array{$key} = [];
          }

          open(IN,"$file"); 
          while ($line=<IN>) {
            chomp $line;
            $l_index=($.-1)%41;
            if($l_index == 0) {
              @fields = split " ", $line;
              if ($#fields == 6) {
                 ($date,$st_id,$lat,$lon,$elevm,$elev,$id) = split " ",$line;
                 $lstid = 1;
              } elsif ($#fields == 5) {
                 ($date,$st_id,$lat,$lon,$elev,$id)=split " ",$line;
                 $lstid = 1;
              } else {
                 ($date,$lat,$lon,$id)=split " ",$line;
                 $lstid = 0;
              }

              $output = 0;

              $latlon = "$lat$lon";

              if ($lstid) {
                 if(defined($stations{$st_id})) {
                   $output = 1;
                 }
              } else {
                 if(defined($stations{$latlon}) && $id == 1) {
                   $output = 1;
                   $st_id = $stations{$latlon};
                 } else {
D:                 foreach $y (-1,1) {
                   foreach $x (-1,1) {
                     $rlat = $lat*+$y*0.01;
                     $rlon = $lon*+$x*0.01;
                     $rlat = sprintf "%.2f",$rlat;
                     $rlon = sprintf "%.2f",$rlon;
                     $latlon = "$rlat$rlon";
                     if(defined($stations{$latlon}) && $id == 1) {
                       $output = 1;
                       $st_id = $stations{$latlon};
                       last D;
                     }
                   }
                   }
                 }
              }

              $index = indexcal($valid_time,$cycle);

              foreach $i (@{$cycle_array{$st_id}}) {
                if ($i == $cycle) {
                   $output = 0;
                   last;
                }
              }

              $output = 0 if($index%12 != 0); # only every 12-hr lead time

              if ($output) {
                 $lead = sprintf "%02d",$index;
                 foreach $key (keys %levels) {
                    next if($key == 2001);
                    $fhr{$key} = FileHandle->new(">>$latestCycle/ref_${key}mb_${lead}hr_${st_id}.txt");
                 }
                 push @{ $cycle_array{$st_id} }, $cycle;
              }

            } else {
              ($p,$tm,$to,$qc_t,$qm,$qo,$qc_q,$rhm,$rho,$qc_rh,$wsm,$wso,$qc_ws,
               $wdm,$wdo,$qc_wd,$ghm,$gho,$qc_gh)=split " ",$line;

               if ($rhm == $missing || $tm == $missing) {
                  $tdm = $missing;
               } else {
                  $tdm = td_from_rh($rhm,$tm)  # rhm in %, $t in K
               }

               if ($tm == $missing) {
                  $tmc = $missing;
               } else {
                  $tmc = $tm-273.15;
               }

               if ($output) {
                  chop($p);
                  if(defined($levels{$p})) {
                    print { $fhr{$p} } "${dateTimeString} $tmc $rhm $wsm $wdm $ghm\n";
                  }
               }
            }
          }

          close(IN);
       } 
    }

}

chdir "$latestCycle";
#
# plot time series
#
foreach $key (keys %levels) {

  if ($key == 2001) {
     $level = 'sfc';
  } else {
     $level = "${key}mb";
  }

  &plot_ts($level);

}

system("rm -rf *.ps");

if (defined($DEBUG) && $DEBUG < 10) {
   system("rm -f *.txt");
}

chdir "..";

if ($changeDays) {  ## need to archive
   system("mkdir -p $TS_ARCHIVE_DIR");
   print  "tar cf $TS_ARCHIVE_DIR/$latestCycle.tar $latestCycle\n";
   system("tar cf $TS_ARCHIVE_DIR/$latestCycle.tar $latestCycle");
}

if ($DEST_SERVER =~ /localhost/) {
   system("mkdir -p $JOB_LOC/obs_ts");
   system("mv $latestCycle $JOB_LOC/obs_ts/.");
} else {
   system("rsync -e 'ssh -i $KEY' -avzC $cycle $DEST_SERVER:$JOB_LOC/obs_ts/.");
}

exit;
#
#
#
sub parse_stations {

  my ($lstation,$llevel,$lfcst);
  my ($sid,$lat,$lon,$name);
  my ($lat2,$lon2);
  my $key;
  my ($domain,$d,$d02);
  my $std_out;
  my $outside,@dummy;

  system("ln -sf $GSJOBDIR/wps/geo_em*.nc .");

  open(STATION,"$VERI_LIST");

  while (<STATION>) {

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
      /(\S+)\s+(\S+)\s+(\S+)\s+\"(.*)\"/;
      $sid = $1;
      $lat = $2;
      $lon = $3;
      $name = $4;

      $lat2 = sprintf "%.2f",$lat;
      $lon2 = sprintf "%.2f",$lon;
      $key = "$lat2$lon2";
     #$key = $sid;
      $stations{$key} = $sid;
      $stations{$sid} = 1;
      $slat{$sid} = $lat2;
      $slon{$sid} = $lon2;
      $sname{$sid} = $name;

      $domain = 0;
      for($d=3;$d>=1;$d--) {
         $d02 = sprintf "%02d",$d;
         $std_out = `$EXECUTABLE_ARCHIVE/WRF_lltoxy.exe -f geo_em.d${d02}.nc -lat $lat -lon $lon -useGeoFile`;
         ($outside,@dummy) = split " ",$std_out;
         if ($outside) {
            next;
         } else {
            $domain = $d;
            last;
         }
      }

      $GRID{$sid} = $domain;
    }

    if($llevel) {
      ($level,$weight) = split;
      $levels{$level} = 1;
    }
  }

  return;
}
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
sub rh_from_q {

  my ($p,$t,$q)=@_;
  my ($es,$qs,$rh);

  $p *= 0.1;
  $t *= 0.01;
  $q *= 0.00001;

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
sub td_from_rh {

  my ($rh,$t) = @_;
  my ($x,$xlv,$td);
  my $eps = 1.0e-4;
  my $xr=461.51;

  $rh=$rh*0.01;
  $x=-1*log($rh+$eps);
  $xlv=(2.5-0.002274*($t-273.15))*1000000;
  $td=($t)/($xr*$x*($t)/$xlv+1);
  $td -= 273.15;

  return $td;

}
#
#
#
sub plot_ts {

  my $level = $_[0];

  my $WRF;
  my $REF;
  my $OBS;
  my $fileID;

  my $missing = -8888;
  my ($st_id,$lat,$lon,$name);

  my ($t_range,$ws_range,$ht_range);

  my $wrf_count;

  %ht_tick  = ('950mb' => 'f10a50', '900mb' => 'f10a50', '850mb' => 'f10a50',
               '700mb' => 'f10a50', '500mb' => 'f10a50', '400mb' => 'f10a50',
               '300mb' => 'f10a50', '200mb' => 'f10a50', '100mb' => 'f10a50');

  $oldestTime = substr($oldestCycle,0,4) . '-' . 
                substr($oldestCycle,4,2) . '-' .
                substr($oldestCycle,6,2) . 'T' .
                substr($oldestCycle,8,2) . ':00:00';
#
  $latestTime = substr($latestCycle,0,4) . '-' . 
                substr($latestCycle,4,2) . '-' .
                substr($latestCycle,6,2) . 'T' .
                substr($latestCycle,8,2) . ':00:00';

  $x_range = "$oldestTime/$latestTime";

  system("gmtset PLOT_DATE_FORMAT mm/dd/yy");

  foreach $WRF (<wrf_${level}*.txt>) {

    $REF = $WRF;
    $OBS = $WRF;

    $REF =~ s/wrf/ref/;
    $OBS =~ s/wrf/obs/;

    $WRF =~ /wrf_(\S+)\.txt/;
    $fileID = $1;

    $fileID =~ /(\w+)_\w+_(\d+)/;

   #$level = $1;
    $st_id = $2;
    $lat = $slat{$st_id};
    $lon = $slon{$st_id};
    $name = $sname{$st_id};

    $t_range = minmax($WRF,$REF,$OBS,1,10);
    $ws_range = minmax($WRF,$REF,$OBS,3,10);
    $ht_range = minmax($WRF,$REF,$OBS,5,50);
#
#   OBS
#
    open(T_PLOT,"| psxy -JX5/2 -R$x_range/$t_range -W2p/0/255/0 -Bpa24hf12h:\"Cycle Time\":/f1a5:\"T (\272C)\":WSen -Bsa4D/ -X1.5 -Y5 -K > t_ts_${fileID}.ps");
    open(T_PLOT2,"| psxy -JX5/2 -R$x_range/$t_range -W2p/0/255/0 -Sc0.05i -O -K >> t_ts_${fileID}.ps");
    open(RH_PLOT,"| psxy -JX5/2 -R$x_range/0/100 -W2p/0/255/0 -Bpa24hf12h:\"Cycle Time\":/f5a20:\"RH (%)\":WSen -Bsa4D/ -X1.5 -Y5 -K > rh_ts_${fileID}.ps");
    open(RH_PLOT2,"| psxy -JX5/2 -R$x_range/0/100 -W2p/0/255/0 -Sc0.05i -O -K >> rh_ts_${fileID}.ps");
    open(WS_PLOT,"| psxy -JX5/2 -R$x_range/$ws_range -W2p/0/255/0 -Bpa24hf12h:\"Cycle Time\":/f1a5:\"Wind Speed (ms\@+-1\@+)\":WSen -Bsa4D/ -X1.5 -Y5 -K > ws_ts_${fileID}.ps");
    open(WS_PLOT2,"| psxy -JX5/2 -R$x_range/$ws_range -W2p/0/255/0 -Sc0.05i -O -K >> ws_ts_${fileID}.ps");
    open(WD_PLOT,"| psxy -JX5/2 -R$x_range/120/480 -W2p/0/255/0 -Bpa24hf12h:\"Cycle Time\":/f10a60:\"Wind Direction (\272)\":WSen -Bsa4D/ -X1.5 -Y5 -K > wd_ts_${fileID}.ps");
    open(WD_PLOT2,"| psxy -JX5/2 -R$x_range/120/480 -W2p/0/255/0 -Sc0.05i -O -K >> wd_ts_${fileID}.ps");
    open(HT_PLOT,"| psxy -JX5/2 -R$x_range/$ht_range -W2p/0/255/0 -Bpa24hf12h:\"Cycle Time\":/$ht_tick{$level}:\"Height (m)\":WSen -Bsa4D/ -X1.5 -Y5 -K > ght_ts_${fileID}.ps") if($level ne 'sfc');
    open(HT_PLOT2,"| psxy -JX5/2 -R$x_range/$ht_range -W2p/0/255/0 -Sc0.05i -O -K >> ght_ts_${fileID}.ps") if($level ne 'sfc');

    open(OBS,"$OBS");
    while (<OBS>) {
      chomp;
      ($x,$t,$rh,$ws,$wd,$psfc_ght) = split;
      print T_PLOT "$x $t\n" if($t != $missing);
      print T_PLOT2 "$x $t\n" if($t != $missing);
      print RH_PLOT "$x $rh\n" if($rh != $missing);
      print RH_PLOT2 "$x $rh\n" if($rh != $missing);
      print WS_PLOT "$x $ws\n" if($ws != $missing);
      print WS_PLOT2 "$x $ws\n" if($ws != $missing);
      if ($wd != $missing) {
         $wd += 360 if ($wd < 120);
         print WD_PLOT "$x $wd\n";
         print WD_PLOT2 "$x $wd\n";
      }
      print HT_PLOT "$x $psfc_ght\n" if($psfc_ght != $missing && $level ne 'sfc');
      print HT_PLOT2 "$x $psfc_ght\n" if($psfc_ght != $missing && $level ne 'sfc');
    }
    close(OBS);
    close(T_PLOT);
    close(T_PLOT2);
    close(RH_PLOT);
    close(RH_PLOT2);
    close(WS_PLOT);
    close(WS_PLOT2);
    close(WD_PLOT);
    close(WD_PLOT2);
    close(HT_PLOT) if($level ne 'sfc');
    close(HT_PLOT2) if($level ne 'sfc');
#
#   WRF
#
    open(T_PLOT,"| psxy -JX -R$x_range/$t_range -W2p/0/0/255 -O -K >> t_ts_${fileID}.ps");
    open(T_PLOT2,"| psxy -JX -R$x_range/$t_range -W2p/0/0/255 -Sc0.05i -O -K >> t_ts_${fileID}.ps");
    open(RH_PLOT,"| psxy -JX -R$x_range/0/100 -W2p/0/0/255 -O -K >> rh_ts_${fileID}.ps");
    open(RH_PLOT2,"| psxy -JX -R$x_range/0/100 -W2p/0/0/255 -Sc0.05i -O -K >> rh_ts_${fileID}.ps");
    open(WS_PLOT,"| psxy -JX -R$x_range/$ws_range -W2p/0/0/255 -O -K >> ws_ts_${fileID}.ps");
    open(WS_PLOT2,"| psxy -JX -R$x_range/$ws_range -W2p/0/0/255 -Sc0.05i -O -K >> ws_ts_${fileID}.ps");
    open(WD_PLOT,"| psxy -JX -R$x_range/120/480 -W2p/0/0/255 -O -K >> wd_ts_${fileID}.ps");
    open(WD_PLOT2,"| psxy -JX -R$x_range/120/480 -W2p/0/0/255 -Sc0.05i -O -K >> wd_ts_${fileID}.ps");
    open(HT_PLOT,"| psxy -JX -R$x_range/$ht_range -W2p/0/0/255 -O -K >> ght_ts_${fileID}.ps") if($level ne 'sfc');
    open(HT_PLOT2,"| psxy -JX -R$x_range/$ht_range -W2p/0/0/255 -Sc0.05i -O -K >> ght_ts_${fileID}.ps") if($level ne 'sfc');

    $wrf_count = 0; # if this remains 0 thru out entire time, wrf station must be below ground for this pressure level
    open(WRF,"$WRF");
      while (<WRF>) {
      chomp;
      ($x,$t,$rh,$ws,$wd,$psfc_ght) = split;
      if ($t != $missing) {
         print T_PLOT "$x $t\n";
         print T_PLOT2 "$x $t\n";
         $wrf_count++;
      }
      print RH_PLOT "$x $rh\n" if($rh != $missing);
      print RH_PLOT2 "$x $rh\n" if($rh != $missing);
      print WS_PLOT "$x $ws\n" if($ws != $missing);
      print WS_PLOT2 "$x $ws\n" if($ws != $missing);
      if ($wd != $missing) {
         $wd += 360 if ($wd < 120);
         print WD_PLOT "$x $wd\n";
         print WD_PLOT2 "$x $wd\n";
      }
      print HT_PLOT "$x $psfc_ght\n" if($psfc_ght != $missing && $level ne 'sfc');
      print HT_PLOT2 "$x $psfc_ght\n" if($psfc_ght != $missing && $level ne 'sfc');
    }
    close(WRF);
    close(T_PLOT);
    close(T_PLOT2);
    close(RH_PLOT);
    close(RH_PLOT2);
    close(WS_PLOT);
    close(WS_PLOT2);
    close(WD_PLOT);
    close(WD_PLOT2);
    close(HT_PLOT) if($level ne 'sfc');
    close(HT_PLOT2) if($level ne 'sfc');

    if ($wrf_count == 0) {
       system("rm -f *${fileID}.ps");
       next;
    }
#
#   REF
#

    if (-e "$REF") {

    open(T_PLOT,"| psxy -JX -R$x_range/$t_range -W2p/255/0/0 -O -K >> t_ts_${fileID}.ps");
    open(T_PLOT2,"| psxy -JX -R$x_range/$t_range -W2p/255/0/0 -Sc0.05i -O -K >> t_ts_${fileID}.ps");
    open(RH_PLOT,"| psxy -JX -R$x_range/0/100 -W2p/255/0/0 -O -K >> rh_ts_${fileID}.ps");
    open(RH_PLOT2,"| psxy -JX -R$x_range/0/100 -W2p/255/0/0 -Sc0.05i -O -K >> rh_ts_${fileID}.ps");
    open(WS_PLOT,"| psxy -JX -R$x_range/$ws_range -W2p/255/0/0 -O -K >> ws_ts_${fileID}.ps");
    open(WS_PLOT2,"| psxy -JX -R$x_range/$ws_range -W2p/255/0/0 -Sc0.05i -O -K >> ws_ts_${fileID}.ps");
    open(WD_PLOT,"| psxy -JX -R$x_range/120/480 -W2p/255/0/0 -O -K >> wd_ts_${fileID}.ps");
    open(WD_PLOT2,"| psxy -JX -R$x_range/120/480 -W2p/255/0/0 -Sc0.05i -O -K >> wd_ts_${fileID}.ps");
    open(HT_PLOT,"| psxy -JX -R$x_range/$ht_range -W2p/255/0/0 -O -K >> ght_ts_${fileID}.ps") if($level ne 'sfc');
    open(HT_PLOT2,"| psxy -JX -R$x_range/$ht_range -W2p/255/0/0 -Sc0.05i -O -K >> ght_ts_${fileID}.ps") if($level ne 'sfc');

    open(REF,"$REF");
    while (<REF>) {
      chomp;
      ($x,$t,$rh,$ws,$wd,$psfc_ght) = split;
      print T_PLOT "$x $t\n" if ($t != $missing);
      print T_PLOT2 "$x $t\n" if ($t != $missing);
      print RH_PLOT "$x $rh\n" if($rh != $missing);
      print RH_PLOT2 "$x $rh\n" if($rh != $missing);
      print WS_PLOT "$x $ws\n" if($ws != $missing);
      print WS_PLOT2 "$x $ws\n" if($ws != $missing);
      if ($wd != $missing) {
         $wd += 360 if ($wd < 120);
         print WD_PLOT "$x $wd\n";
         print WD_PLOT2 "$x $wd\n";
      }
      print HT_PLOT "$x $psfc_ght\n" if($psfc_ght != $missing && $level ne 'sfc');
      print HT_PLOT2 "$x $psfc_ght\n" if($psfc_ght != $missing && $level ne 'sfc');
    }
    close(REF);
    close(T_PLOT);
    close(T_PLOT2);
    close(RH_PLOT);
    close(RH_PLOT2);
    close(WS_PLOT);
    close(WS_PLOT2);
    close(WD_PLOT);
    close(WD_PLOT2);
    close(HT_PLOT) if($level ne 'sfc');
    close(HT_PLOT2) if($level ne 'sfc');

    }  # end if( -e "$REF")
#
#   legend
#
    open(T_PLOT,"| pstext -JX5/0.75 -R0/10/-10/10 -N -Y-1.5 -O -K >> t_ts_${fileID}.ps");
    print T_PLOT "0 -3 12 0 5 ML Lat: $lat\n";
    print T_PLOT "1.75 -3 12 0 5 ML Lon: $lon\n";
    print T_PLOT "3.5 -3 12 0 5 ML Station: $st_id $name\n";
    close(T_PLOT);

    open(RH_PLOT,"| pstext -JX5/0.75 -R0/10/-10/10 -N -Y-1.5 -O -K >> rh_ts_${fileID}.ps");
    print RH_PLOT "0 -3 12 0 5 ML Lat: $lat\n";
    print RH_PLOT "1.75 -3 12 0 5 ML Lon: $lon\n";
    print RH_PLOT "3.5 -3 12 0 5 ML Station: $st_id $name\n";
    close(RH_PLOT);

    open(WS_PLOT,"| pstext -JX5/0.75 -R0/10/-10/10 -N -Y-1.5 -O -K >> ws_ts_${fileID}.ps");
    print WS_PLOT "0 -3 12 0 5 ML Lat: $lat\n";
    print WS_PLOT "1.75 -3 12 0 5 ML Lon: $lon\n";
    print WS_PLOT "3.5 -3 12 0 5 ML Station: $st_id $name\n";
    close(WS_PLOT);

    open(WD_PLOT,"| pstext -JX5/0.75 -R0/10/-10/10 -N -Y-1.5 -O -K >> wd_ts_${fileID}.ps");
    print WD_PLOT "0 -3 12 0 5 ML Lat: $lat\n";
    print WD_PLOT "1.75 -3 12 0 5 ML Lon: $lon\n";
    print WD_PLOT "3.5 -3 12 0 5 ML Station: $st_id $name\n";
    close(WD_PLOT);

    if ($level ne 'sfc') {
    open(HT_PLOT,"| pstext -JX5/0.75 -R0/10/-10/10 -N -Y-1.5 -O -K >> ght_ts_${fileID}.ps");
    print HT_PLOT "0 -3 12 0 5 ML Lat: $lat\n";
    print HT_PLOT "1.75 -3 12 0 5 ML Lon: $lon\n";
    print HT_PLOT "3.5 -3 12 0 5 ML Station: $st_id $name\n";
    close(HT_PLOT);
    }

    open(T_PLOT,"| psxy -JX -R -W2p/0/255/0 -O -K >> t_ts_${fileID}.ps");
    print T_PLOT "5 3\n5.5 3";
    close(T_PLOT);

    open(RH_PLOT,"| psxy -JX -R -W2p/0/255/0 -O -K >> rh_ts_${fileID}.ps");
    print RH_PLOT "5 3\n5.5 3";
    close(RH_PLOT);

    open(WS_PLOT,"| psxy -JX -R -W2p/0/255/0 -O -K >> ws_ts_${fileID}.ps");
    print WS_PLOT "5 3\n5.5 3";
    close(WS_PLOT);

    open(WD_PLOT,"| psxy -JX -R -W2p/0/255/0 -O -K >> wd_ts_${fileID}.ps");
    print WD_PLOT "5 3\n5.5 3";
    close(WD_PLOT);

    if ($level ne 'sfc') {
    open(HT_PLOT,"| psxy -JX -R -W2p/0/255/0 -O -K >> ght_ts_${fileID}.ps");
    print HT_PLOT "5 3\n5.5 3";
    close(HT_PLOT);
    }

    open(T_PLOT,"| pstext -JX -R -O -K >> t_ts_${fileID}.ps");
    print T_PLOT "5.6 3 12 -3 5 ML OBS";
    close(T_PLOT);

    open(RH_PLOT,"| pstext -JX -R -O -K >> rh_ts_${fileID}.ps");
    print RH_PLOT "5.6 3 12 0 5 ML OBS";
    close(RH_PLOT);

    open(WS_PLOT,"| pstext -JX -R -O -K >> ws_ts_${fileID}.ps");
    print WS_PLOT "5.6 3 12 0 5 ML OBS";
    close(WS_PLOT);

    open(WD_PLOT,"| pstext -JX -R -O -K >> wd_ts_${fileID}.ps");
    print WD_PLOT "5.6 3 12 0 5 ML OBS";
    close(WD_PLOT);

    if ($level ne 'sfc') {
    open(HT_PLOT,"| pstext -JX -R -O -K >> ght_ts_${fileID}.ps");
    print HT_PLOT "5.6 3 12 0 5 ML OBS";
    close(HT_PLOT);
    }

    open(T_PLOT,"| psxy -JX -R -W2p/0/0/255 -O -K >> t_ts_${fileID}.ps");
    print T_PLOT "7 3\n7.5 3";
    close(T_PLOT);

    open(RH_PLOT,"| psxy -JX -R -W2p/0/0/255 -O -K >> rh_ts_${fileID}.ps");
    print RH_PLOT "7 3\n7.5 3";
    close(RH_PLOT);

    open(WS_PLOT,"| psxy -JX -R -W2p/0/0/255 -O -K >> ws_ts_${fileID}.ps");
    print WS_PLOT "7 3\n7.5 3";
    close(WS_PLOT);

    open(WD_PLOT,"| psxy -JX -R -W2p/0/0/255 -O -K >> wd_ts_${fileID}.ps");
    print WD_PLOT "7 3\n7.5 3";
    close(WD_PLOT);

    if ($level ne 'sfc') {
    open(HT_PLOT,"| psxy -JX -R -W2p/0/0/255 -O -K >> ght_ts_${fileID}.ps");
    print HT_PLOT "7 3\n7.5 3";
    close(HT_PLOT);
    }

    open(T_PLOT,"| pstext -JX -R -O -K >> t_ts_${fileID}.ps");
    print T_PLOT "7.6 3 12 0 5 ML WRF D$GRID{$st_id}";
    close(T_PLOT);

    open(RH_PLOT,"| pstext -JX -R -O -K >> rh_ts_${fileID}.ps");
    print RH_PLOT "7.6 3 12 0 5 ML WRF D$GRID{$st_id}";
    close(RH_PLOT);

    open(WS_PLOT,"| pstext -JX -R -O -K >> ws_ts_${fileID}.ps");
    print WS_PLOT "7.6 3 12 0 5 ML WRF D$GRID{$st_id}";
    close(WS_PLOT);

    open(WD_PLOT,"| pstext -JX -R -O -K >> wd_ts_${fileID}.ps");
    print WD_PLOT "7.6 3 12 0 5 ML WRF D$GRID{$st_id}";
    close(WD_PLOT);

    if ($level ne 'sfc') {
    open(HT_PLOT,"| pstext -JX -R -O -K >> ght_ts_${fileID}.ps");
    print HT_PLOT "7.6 3 12 0 5 ML WRF D$GRID{$st_id}";
    close(HT_PLOT);
    }

    open(T_PLOT,"| psxy -JX -R -W2p/255/0/0 -O -K >> t_ts_${fileID}.ps");
    print T_PLOT "9 3\n9.5 3";
    close(T_PLOT);

    open(RH_PLOT,"| psxy -JX -R -W2p/255/0/0 -O -K >> rh_ts_${fileID}.ps");
    print RH_PLOT "9 3\n9.5 3";
    close(RH_PLOT);

    open(WS_PLOT,"| psxy -JX -R -W2p/255/0/0 -O -K >> ws_ts_${fileID}.ps");
    print WS_PLOT "9 3\n9.5 3";
    close(WS_PLOT);

    open(WD_PLOT,"| psxy -JX -R -W2p/255/0/0 -O -K >> wd_ts_${fileID}.ps");
    print WD_PLOT "9 3\n9.5 3";
    close(WD_PLOT);

    if ($level ne 'sfc') {
    open(HT_PLOT,"| psxy -JX -R -W2p/255/0/0 -O -K >> ght_ts_${fileID}.ps");
    print HT_PLOT "9 3\n9.5 3";
    close(HT_PLOT);
    }

    open(T_PLOT,"| pstext -JX -R -N -O >> t_ts_${fileID}.ps");
    print T_PLOT "9.6 3 12 0 5 ML $REF_MODEL";
    close(T_PLOT);

    open(RH_PLOT,"| pstext -JX -R -N -O >> rh_ts_${fileID}.ps");
    print RH_PLOT "9.6 3 12 0 5 ML $REF_MODEL";
    close(RH_PLOT);

    open(WS_PLOT,"| pstext -JX -R -N -O >> ws_ts_${fileID}.ps");
    print WS_PLOT "9.6 3 12 0 5 ML $REF_MODEL";
    close(WS_PLOT);

    open(WD_PLOT,"| pstext -JX -R -N -O >> wd_ts_${fileID}.ps");
    print WD_PLOT "9.6 3 12 0 5 ML $REF_MODEL";
    close(WD_PLOT);

    if ($level ne 'sfc') {
    open(HT_PLOT,"| pstext -JX -R -N -O >> ght_ts_${fileID}.ps");
    print HT_PLOT "9.6 3 12 0 5 ML $REF_MODEL";
    close(HT_PLOT);
    }

    &fix_wd_label("wd_ts_${fileID}.ps");

    foreach $ps (<*${fileID}.ps>) {
       $gif = $ps;
       $gif =~ s/\.ps$/\.gif/;
       system("convert -trim +repage -flatten $ps $gif");
       system("rm -f $ps");
    }
  }

  return;

}
#
#
#
sub minmax {

  my ($WRF,$REF,$OBS,$nf,$bin) = @_;

  my $min = 999999;
  my $max = -999999;
  my $missing = -8888;
  my @f;

  open(W,"$WRF");
  while (<W>) {
    @f = split;
    next if($f[$nf] == $missing);
    if ($f[$nf] < $min ) {
       $min = $f[$nf];
    }
    if ($f[$nf] > $max) {
       $max = $f[$nf];
    }
  }
  close(W);

  open(R,"$REF");
  while (<R>) {
    @f = split;
    next if($f[$nf] == $missing);
    if ($f[$nf] < $min ) {
       $min = $f[$nf];
    }
    if ($f[$nf] > $max) {
       $max = $f[$nf];
    }
  }
  close(R);

  open(O,"$OBS");
  while (<O>) {
    @f = split;
    next if($f[$nf] == $missing);
    if ($f[$nf] < $min ) {
       $min = $f[$nf];
    }
    if ($f[$nf] > $max) {
       $max = $f[$nf];
    }
  }
  close(O);

  my $lb = (int($min/$bin)-1)*$bin;
  my $ub = (int($max/$bin)+1)*$bin;

  if ($nf == 3 || $nf == 5) {  # ws, height
     $lb = 0 if ($lb < 0);
  }

  my $r = "$lb/$ub";

  return $r;

}
#
#
#
sub check_archive {

  my ($cycle) = $_[0];

  my @mdays=(31,31,28,31,30,31,30,31,31,30,31,30,31);
  my ($year,$month,$day,$hour);
  my $mid_month;

  $year=        substr($cycle,0,4);
  $month=       substr($cycle,4,2);
  $day=         substr($cycle,6,2);
  $hour=        substr($cycle,8,2);

  $new_days;

  if($year%4 == 0) {
    if($year%100 == 0) {
      $mdays[2]=29 if($year%400 == 0);
    } else {
      $mdays[2]=29;
    }
  }

  if ($mdays[$month] == 28) { 
     $mid_month = 14;
  } else {
     $mid_month = 15;
  }

  if ($day == $mid_month && $hour == 12) {
     $new_days = $mid_month;
  } elsif ($day == $mdays[$month] && $hour == 12) {
     $new_days = $mdays[$month] - $mid_month;
  } else {
     $new_days = 0;
  }

  return $new_days;

}
#
#
#
sub fix_wd_label {

  my $file = $_[0];

  open(IN,"$file");
  open(OUT,">wd.tmp");

  while (<IN>) {
    s/\(420\)/\(60\)/ if(/\(420\)/);
    s/\(480\)/\(120\)/ if(/\(480\)/);
    print OUT $_;
  }

  close(IN);
  close(OUT);

  system("mv wd.tmp $file");

}

#!/usr/bin/perl
use Getopt::Std;
getopts('c:j:l:');

our $WRF_PAIRS_DIR;
our $REF_PAIRS_DIR;
our $RUNDIR;
our $JOB_LOC;
our $DEST_SERVER;
our $VERI_LENGTH;
our $EXECUTABLE_ARCHIVE;
our $GMT_BIN;

my $VERI_LIST;

my %GRID;
my $GSJOBDIR;

if ($opt_c) {
   $VERI_LIST = $opt_c;
} else {
   die "Usage: $0 -c <config_file> -j <GSJOBDIR> [-l <lag in hours>\n";
}

if ($opt_j) {
   $GSJOBDIR = $opt_j;
   $ENV{GSJOBDIR} = $GSJOBDIR;
   $GSJOBDIR =~ /(\w+)$/;
   $ENV{GSJOBID} = $1;
} else {
   die "Usage: $0 -c <config_file> -j <GSJOBDIR> [-l <lag in hours>\n";
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

print "MM5HOME = $MM5HOME\n";
print "EXECUTABLE_ARCHIVE = $EXECUTABLE_ARCHIVE\n";
print "PERL_ARCHIVE = $PERL_ARCHIVE\n";
print "WRF_PAIRS_DIR $WRF_PAIRS_DIR\n";
print "REF_PAIRS_DIR $REF_PAIRS_DIR\n";
print "RUNDIR = $RUNDIR\n";
print "JOB_LOC = $JOB_LOC\n";

my $OBS_CYCLE_DIR = "$RUNDIR/obs_cycle";

my %stations;
my %slat;
my %slon;
my %sname;

my $missing=-8888;
my $nbytes = 56;

$VERI_LENGTH = int($VERI_LENGTH/12)*12; # so the extra 6-hour is not considered

if (! defined($opt_l)) {
   $opt_l = 0;
}

### GMT bin path

if (! $GMT_BIN) {
   $GMT_BIN = '/opt/gmt/bin';
}

$ENV{PATH} = "${GMT_BIN}:$ENV{PATH}";

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=gmtime(time-$opt_l*3600-$VERI_LENGTH*3600);

$cycle = sprintf "%4d%02d%02d%02d",$year+1900,$mon+1,$mday,$hour;

system("mkdir -p $OBS_CYCLE_DIR/sfc/$cycle");
system("mkdir -p $OBS_CYCLE_DIR/snd/$cycle");

$WRF_SFC = "$WRF_PAIRS_DIR/sfc/fcst/${cycle}_veri_dat_GRM_P+FCST";
$REF_SFC = `ls $REF_PAIRS_DIR/sfc/${cycle}_veri_dat_*`;
chomp($REF_SFC);
if ($REF_SFC) {
   $REF_SFC =~ /_([A-Z0-9]+)$/;
   $REF_MODEL = $1;
} else {
   die "REF pairs file $REF_PAIRS_DIR/sfc/${cycle}_veri_dat_\* does not exist!\n";
}

chdir "$OBS_CYCLE_DIR";

&parse_stations();

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
A:      foreach $y (-1,1) {
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

   # any station can have multiple model-obs pairs around top of the hour;
   # in this case, only the first occurrence is used

  #if ($lstid) {
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
  #} else {
  #   foreach $i (@{$index_array{$latlon}}) {
  #     if ($i == $index) {
  #        $output = 0;
  #        last;
  #     }
  #   }
  #}

   ### is this the right domain solution for wrf?

   $output_wrf = 0 if ($domain_id != $GRID{$st_id});

   ###

   if ($output_wrf) {
      open(WRF_OUT,">>sfc/$cycle/wrf_${cycle}_${st_id}.txt");
      print WRF_OUT "$index $rt2_m $rh_m $rws_m $wd_m $rpsfc_m\n";
      close(WRF_OUT);
     #if ($lstid) {
      push @{ $index_array_wrf{$st_id} },$index;
     #} else {
     #   push @{ $index_array{$latlon} }, $index;
     #}
   }

   if ($output_obs) {
      open(OBS_OUT,">>sfc/$cycle/obs_${cycle}_${st_id}.txt");
      print OBS_OUT "$index $rt2_o $rh_o $rws_o $wd_o $rpsfc_o\n";
      close(OBS_OUT);
      push @{ $index_array_obs{$st_id} },$index;
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
B:      foreach $y (-1,1) {
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

   if ($output) {
      open(REF_OUT,">>sfc/$cycle/ref_${cycle}_${st_id}.txt");
      print REF_OUT "$index $rt2_m $rh_m $rws_m $wd_m $rpsfc_m\n";
      close(REF_OUT);
     #if ($lstid) {
         push @{ $index_array{$st_id} }, $index;
     #} else {
     #   push @{ $index_array{$latlon} }, $index;
     #}
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
               $st_id = $stattions{$latlon};
             } else {
C:             foreach $y (-1,1) {
               foreach $x (-1,1) {
                 $rlat = $lat+$y*0.01;
                 $rlon = $lon+$x*0.01;
                 $rlat = sprintf "%.2f",$rlat;
                 $rlon = sprintf "%.2f",$rlon;
                 $latlon = "$rlat$rlon";
                 if(defined($stations{$latlon}) && $id == 1) {
                   $output = 1;
                   $st_id = $stattions{$latlon};
                   last C;
                 }
               }
               }
             }
          }

          ### is this the right domain solution for wrf?

          $output = 0 if ($id != $GRID{$st_id});

          ###

          if ($output) {
             open(WOUT,">snd/$cycle/wrf_${cycle}_${valid_time}_${st_id}.snd");
             print WOUT "$date $st_id $lat $lon\n";
             open(OOUT,">snd/$cycle/obs_${cycle}_${valid_time}_${st_id}.snd");
             print OOUT "$date $st_id $lat $lon\n";
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

           if ($output) {
              printf WOUT "%12.4f%12.4f%12.4f%12.4f%12.4f\n",$p,$tmc,$tdm,$wsm,$wdm if($p >= 100);
              printf OOUT "%12.4f%12.4f%12.4f%12.4f%12.4f\n",$p,$toc,$tdo,$wso,$wdo if($p >= 100);
           }
        }
      }

      close(IN);
      close(WOUT);
      close(OOUT);
   } 
}
#
# ref soundings
#
foreach $file (<$REF_UPR_DIR/${cycle}_*>) {
   if (-s "$file") {
      $file=~ /\d+_(\d+)_veri_dat_upr/;
      $valid_time = $1;

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
             if(defined($stations{$latlon})) {
               $output = 1;
               $st_id = $stations{$latlon};
             } else {
D:             foreach $y (-1,1) {
               foreach $x (-1,1) {
                 $rlat = $lat*+$y*0.01;
                 $rlon = $lon*+$x*0.01;
                 $rlat = sprintf "%.2f",$rlat;
                 $rlon = sprintf "%.2f",$rlon;
                 $latlon = "$rlat$rlon";
                 if(defined($stations{$latlon})) {
                   $output = 1;
                   $st_id = $stations{$latlon};
                   last D;
                 }
               }
               }
             }
          }

          if ($output) {
             open(ROUT,">snd/$cycle/ref_${cycle}_${valid_time}_${st_id}.snd");
             print ROUT "$date $st_id $lat $lon\n";
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
              printf ROUT "%12.4f%12.4f%12.4f%12.4f%12.4f\n",$p,$tmc,$tdm,$wsm,$wdm if($p >= 100);
           }
        }
      }

      close(IN);
      close(ROUT);
   } 
}

#
# plot sfc time series
#
chdir "$OBS_CYCLE_DIR/sfc/$cycle";
foreach $wrf_sfc (<wrf_*.txt>) {
  print "$wrf_sfc\n";
  &plot_sfc($wrf_sfc);
}

if (defined($DEBUG) && $DEBUG < 10) {
   system("rm -f *.txt");
}

chdir "..";

if ($DEST_SERVER =~ /localhost/) {
   system("mkdir -p $JOB_LOC/obs_cycle/sfc");
   system("mv $cycle $JOB_LOC/obs_cycle/sfc/.");
} else {
   system("rsync -e 'ssh -i $KEY' -avzC $cycle $DEST_SERVER:$JOB_LOC/obs_cycle/sfc/.");
}

#
# plot skewT
#
chdir "$OBS_CYCLE_DIR/snd/$cycle";
foreach $obs_snd (<obs_*.snd>) {
  print "$obs_snd\n";
  &plot_skewt($obs_snd);
}

chdir "..";
if ($DEST_SERVER =~ /localhost/) {
   system("mkdir -p $JOB_LOC/obs_cycle/snd");
   system("mv $cycle $JOB_LOC/obs_cycle/snd/.");
} else {
   system("rsync -e 'ssh -i $KEY' -avzC $cycle $DEST_SERVER:$JOB_LOC/obs_cycle/snd/.");
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
      $name =~ s/\s+$//;

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
sub plot_sfc {

  my $WRF = $_[0];
  my $REF;
  my $OBS;
  my $fileID;

  $REF = $WRF;
  $OBS = $WRF;

  $REF =~ s/wrf/ref/;
  $OBS =~ s/wrf/obs/;

  $WRF =~ /wrf_(\S+)\.txt/;
  $fileID = $1;

  $fileID =~ /(\d+)_(\d+)/;

  my $basetime = $1;
  my $st_id = $2;
  my $lat = $slat{$st_id};
  my $lon = $slon{$st_id};
  my $name = $sname{$st_id};

  my $missing = -8888;

  print "sfc plotting for $WRF\n";
  print "$basetime $lat $lon\n";
#
# OBS
#
  open(T_PLOT,"| psxy -JX5/2 -R0/48/-5/40 -W2p/0/255/0 -Bf1a6:\"Forecast Lead Time (HR)\":/f1a5:\"T (K)\":WSen -X1.5 -Y5 -K > t_cycle_${fileID}.ps");
  open(T_PLOT2,"| psxy -JX5/2 -R0/48/-5/40 -W2p/0/255/0 -Sc0.05i -O -K >> t_cycle_${fileID}.ps");
  open(RH_PLOT,"| psxy -JX5/2 -R0/48/0/100 -W2p/0/255/0 -Bf1a6:\"Forecast Lead Time (HR)\":/f5a20:\"RH (%)\":WSen -X1.5 -Y5 -K > rh_cycle_${fileID}.ps");
  open(RH_PLOT2,"| psxy -JX5/2 -R0/48/0/100 -W2p/0/255/0 -Sc0.05i -O -K >> rh_cycle_${fileID}.ps");
  open(WS_PLOT,"| psxy -JX5/2 -R0/48/0/20 -W2p/0/255/0 -Bf1a6:\"Forecast Lead Time\":/f1a5:\"Wind Speed (ms\@+-1\@+)\":WSen -X1.5 -Y5 -K > ws_cycle_${fileID}.ps");
  open(WS_PLOT2,"| psxy -JX5/2 -R0/48/0/20 -W2p/0/255/0 -Sc0.05i -O -K >> ws_cycle_${fileID}.ps");
  open(WD_PLOT,"| psxy -JX5/2 -R0/48/120/480 -W2p/0/255/0 -Bf1a6:\"Forecast Lead Time (HR)\":/f10a60:\"Wind Direction (DEG)\":WSen -X1.5 -Y5 -K > wd_cycle_${fileID}.ps");
  open(WD_PLOT2,"| psxy -JX5/2 -R0/48/120/480 -W2p/0/255/0 -Sc0.05i -O -K >> wd_cycle_${fileID}.ps");

  open(OBS,"$OBS");
  while (<OBS>) {
    chomp;
    ($x,$t,$rh,$ws,$wd,$psfc) = split;
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
#
# WRF
#
  open(T_PLOT,"| psxy -JX -R0/48/-5/40 -W2p/0/0/255 -O -K >> t_cycle_${fileID}.ps");
  open(T_PLOT2,"| psxy -JX -R0/48/-5/40 -W2p/0/0/255 -Sc0.05i -O -K >> t_cycle_${fileID}.ps");
  open(RH_PLOT,"| psxy -JX -R0/48/0/100 -W2p/0/0/255 -O -K >> rh_cycle_${fileID}.ps");
  open(RH_PLOT2,"| psxy -JX -R0/48/0/100 -W2p/0/0/255 -Sc0.05i -O -K >> rh_cycle_${fileID}.ps");
  open(WS_PLOT,"| psxy -JX -R0/48/0/20 -W2p/0/0/255 -O -K >> ws_cycle_${fileID}.ps");
  open(WS_PLOT2,"| psxy -JX -R0/48/0/20 -W2p/0/0/255 -Sc0.05i -O -K >> ws_cycle_${fileID}.ps");
  open(WD_PLOT,"| psxy -JX -R0/48/120/480 -W2p/0/0/255 -O -K >> wd_cycle_${fileID}.ps");
  open(WD_PLOT2,"| psxy -JX -R0/48/120/480 -W2p/0/0/255 -Sc0.05i -O -K >> wd_cycle_${fileID}.ps");

  open(WRF,"$WRF");
    while (<WRF>) {
    chomp;
    ($x,$t,$rh,$ws,$wd,$psfc) = split;
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
#
# REF
#
  open(T_PLOT,"| psxy -JX -R0/48/-5/40 -W2p/255/0/0 -O -K >> t_cycle_${fileID}.ps");
  open(T_PLOT2,"| psxy -JX -R0/48/-5/40 -W2p/255/0/0 -Sc0.05i -O -K >> t_cycle_${fileID}.ps");
  open(RH_PLOT,"| psxy -JX -R0/48/0/100 -W2p/255/0/0 -O -K >> rh_cycle_${fileID}.ps");
  open(RH_PLOT2,"| psxy -JX -R0/48/0/100 -W2p/255/0/0 -Sc0.05i -O -K >> rh_cycle_${fileID}.ps");
  open(WS_PLOT,"| psxy -JX -R0/48/0/20 -W2p/255/0/0 -O -K >> ws_cycle_${fileID}.ps");
  open(WS_PLOT2,"| psxy -JX -R0/48/0/20 -W2p/255/0/0 -Sc0.05i -O -K >> ws_cycle_${fileID}.ps");
  open(WD_PLOT,"| psxy -JX -R0/48/120/480 -W2p/255/0/0 -O -K >> wd_cycle_${fileID}.ps");
  open(WD_PLOT2,"| psxy -JX -R0/48/120/480 -W2p/255/0/0 -Sc0.05i -O -K >> wd_cycle_${fileID}.ps");

  open(REF,"$REF");
  while (<REF>) {
    chomp;
    ($x,$t,$rh,$ws,$wd,$psfc) = split;
    print T_PLOT "$x $t\n";
    print T_PLOT2 "$x $t\n";
    print RH_PLOT "$x $rh\n" if($rh != $missing);
    print RH_PLOT2 "$x $rh\n" if($rh != $missing);
    print WS_PLOT "$x $ws\n" if($ws != $missing);
    print WS_PLOT2 "$x $ws\n" if($ws != $missing);
    if ($wd != $missing) {
       $wd += 360 if ($wd < 120);
       print WD_PLOT "$x $wd\n";
       print WD_PLOT2 "$x $wd\n";
    }
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
#
# legend
#
  open(T_PLOT,"| pstext -JX5/0.75 -R0/10/-10/10 -N -Y-1.25 -O -K >> t_cycle_${fileID}.ps");
  print T_PLOT "0 3 12 0 5 MC $basetime";
  close(T_PLOT);

  open(RH_PLOT,"| pstext -JX5/0.75 -R0/10/-10/10 -N -Y-1.25 -O -K >> rh_cycle_${fileID}.ps");
  print RH_PLOT "0 3 12 0 5 MC $basetime";
  close(RH_PLOT);

  open(WS_PLOT,"| pstext -JX5/0.75 -R0/10/-10/10 -N -Y-1.25 -O -K >> ws_cycle_${fileID}.ps");
  print WS_PLOT "0 3 12 0 5 MC $basetime";
  close(WS_PLOT);

  open(WD_PLOT,"| pstext -JX5/0.75 -R0/10/-10/10 -N -Y-1.25 -O -K >> wd_cycle_${fileID}.ps");
  print WD_PLOT "0 3 12 0 5 MC $basetime";
  close(WD_PLOT);

  open(T_PLOT,"| pstext -JX -R -O -K >> t_cycle_${fileID}.ps");
  print T_PLOT "0 -3 12 0 5 ML Lat: $lat\n";
  print T_PLOT "1.75 -3 12 0 5 ML Lon: $lon\n";
  print T_PLOT "3.5 -3 12 0 5 ML Station: $st_id  $name\n";
  close(T_PLOT);

  open(RH_PLOT,"| pstext -JX -R -O -K >> rh_cycle_${fileID}.ps");
  print RH_PLOT "0 -3 12 0 5 ML Lat: $lat\n";
  print RH_PLOT "1.75 -3 12 0 5 ML Lon: $lon\n";
  print RH_PLOT "3.5 -3 12 0 5 ML Station: $st_id  $name\n";
  close(RH_PLOT);

  open(WS_PLOT,"| pstext -JX -R -O -K >> ws_cycle_${fileID}.ps");
  print WS_PLOT "0 -3 12 0 5 ML Lat: $lat\n";
  print WS_PLOT "1.75 -3 12 0 5 ML Lon: $lon\n";
  print WS_PLOT "3.5 -3 12 0 5 ML Station: $st_id  $name\n";
  close(WS_PLOT);

  open(WD_PLOT,"| pstext -JX -R -O -K >> wd_cycle_${fileID}.ps");
  print WD_PLOT "0 -3 12 0 5 ML Lat: $lat\n";
  print WD_PLOT "1.75 -3 12 0 5 ML Lon: $lon\n";
  print WD_PLOT "3.5 -3 12 0 5 ML Station: $st_id  $name\n";
  close(WD_PLOT);

  open(T_PLOT,"| psxy -JX -R -W2p/0/255/0 -O -K >> t_cycle_${fileID}.ps");
  print T_PLOT "5 3\n5.5 3";
  close(T_PLOT);

  open(RH_PLOT,"| psxy -JX -R -W2p/0/255/0 -O -K >> rh_cycle_${fileID}.ps");
  print RH_PLOT "5 3\n5.5 3";
  close(RH_PLOT);

  open(WS_PLOT,"| psxy -JX -R -W2p/0/255/0 -O -K >> ws_cycle_${fileID}.ps");
  print WS_PLOT "5 3\n5.5 3";
  close(WS_PLOT);

  open(WD_PLOT,"| psxy -JX -R -W2p/0/255/0 -O -K >> wd_cycle_${fileID}.ps");
  print WD_PLOT "5 3\n5.5 3";
  close(WD_PLOT);

  open(T_PLOT,"| pstext -JX -R -O -K >> t_cycle_${fileID}.ps");
  print T_PLOT "5.6 3 12 0 5 ML OBS";
  close(T_PLOT);

  open(RH_PLOT,"| pstext -JX -R -O -K >> rh_cycle_${fileID}.ps");
  print RH_PLOT "5.6 3 12 0 5 ML OBS";
  close(RH_PLOT);

  open(WS_PLOT,"| pstext -JX -R -O -K >> ws_cycle_${fileID}.ps");
  print WS_PLOT "5.6 3 12 0 5 ML OBS";
  close(WS_PLOT);

  open(WD_PLOT,"| pstext -JX -R -O -K >> wd_cycle_${fileID}.ps");
  print WD_PLOT "5.6 3 12 0 5 ML OBS";
  close(WD_PLOT);

  open(T_PLOT,"| psxy -JX -R -W2p/0/0/255 -O -K >> t_cycle_${fileID}.ps");
  print T_PLOT "7 3\n7.5 3";
  close(T_PLOT);

  open(RH_PLOT,"| psxy -JX -R -W2p/0/0/255 -O -K >> rh_cycle_${fileID}.ps");
  print RH_PLOT "7 3\n7.5 3";
  close(RH_PLOT);

  open(WS_PLOT,"| psxy -JX -R -W2p/0/0/255 -O -K >> ws_cycle_${fileID}.ps");
  print WS_PLOT "7 3\n7.5 3";
  close(WS_PLOT);

  open(WD_PLOT,"| psxy -JX -R -W2p/0/0/255 -O -K >> wd_cycle_${fileID}.ps");
  print WD_PLOT "7 3\n7.5 3";
  close(WD_PLOT);

  open(T_PLOT,"| pstext -JX -R -O -K >> t_cycle_${fileID}.ps");
  print T_PLOT "7.6 3 12 0 5 ML WRF D$GRID{$st_id}";
  close(T_PLOT);

  open(RH_PLOT,"| pstext -JX -R -O -K >> rh_cycle_${fileID}.ps");
  print RH_PLOT "7.6 3 12 0 5 ML WRF D$GRID{$st_id}";
  close(RH_PLOT);

  open(WS_PLOT,"| pstext -JX -R -O -K >> ws_cycle_${fileID}.ps");
  print WS_PLOT "7.6 3 12 0 5 ML WRF D$GRID{$st_id}";
  close(WS_PLOT);

  open(WD_PLOT,"| pstext -JX -R -O -K >> wd_cycle_${fileID}.ps");
  print WD_PLOT "7.6 3 12 0 5 ML WRF D$GRID{$st_id}";
  close(WD_PLOT);

  open(T_PLOT,"| psxy -JX -R -W2p/255/0/0 -O -K >> t_cycle_${fileID}.ps");
  print T_PLOT "9 3\n9.5 3";
  close(T_PLOT);

  open(RH_PLOT,"| psxy -JX -R -W2p/255/0/0 -O -K >> rh_cycle_${fileID}.ps");
  print RH_PLOT "9 3\n9.5 3";
  close(RH_PLOT);

  open(WS_PLOT,"| psxy -JX -R -W2p/255/0/0 -O -K >> ws_cycle_${fileID}.ps");
  print WS_PLOT "9 3\n9.5 3";
  close(WS_PLOT);

  open(WD_PLOT,"| psxy -JX -R -W2p/255/0/0 -O -K >> wd_cycle_${fileID}.ps");
  print WD_PLOT "9 3\n9.5 3";
  close(WD_PLOT);

  open(T_PLOT,"| pstext -JX -R -N -O >> t_cycle_${fileID}.ps");
  print T_PLOT "9.6 3 12 0 5 ML $REF_MODEL";
  close(T_PLOT);

  open(RH_PLOT,"| pstext -JX -R -N -O >> rh_cycle_${fileID}.ps");
  print RH_PLOT "9.6 3 12 0 5 ML $REF_MODEL";
  close(RH_PLOT);

  open(WS_PLOT,"| pstext -JX -R -N -O >> ws_cycle_${fileID}.ps");
  print WS_PLOT "9.6 3 12 0 5 ML $REF_MODEL";
  close(WS_PLOT);

  open(WD_PLOT,"| pstext -JX -R -N -O >> wd_cycle_${fileID}.ps");
  print WD_PLOT "9.6 3 12 0 5 ML $REF_MODEL";
  close(WD_PLOT);

  &fix_wd_label("wd_cycle_${fileID}.ps");

  foreach $ps (<*.ps>) {
     $gif = $ps;
     $gif =~ s/\.ps$/\.gif/;
     system("convert -trim +repage -flatten $ps $gif");
     unlink "$ps";
  }

  return;

}
#
#
#
sub plot_skewt {

  my $raob = $_[0];
  my $wrf = $raob;
  my $ref = $raob;

  my $fileID;

  my $i;

  $wrf =~ s/obs/wrf/;
  $ref =~ s/obs/ref/;

  $raob =~ /obs_(\S+)\.snd/;
  $fileID = $1;

  my @xb=(-19,27.1,27.1,18.6,18.6,-19,-19);
#@yb=(-.9346217,-.9346217,9.,17.53,44.061,44.061,-.9346217);
  my @yb=(-.9346216,-.9346216,9.,17.53,44.06,44.06,-.9346216);
  my @plv=(100,200,300,400,500,600,700,800,900,1000,1050);
  my @pln=(
     [-19,-19,-19,-19,-19,-19,-19,-19,-19,-19,-19],
     [18.6,18.6,18.6,18.6,22.83,26.306,27.1,27.1,27.1,27.1,27.1],
     );
  my @tp=(
    [1050,1050,1050,1050,1050,1050,1050,1050,855,625,459,337,247,181,132],
    [730,580,500,430,342,251,185,135,100,100,100,100,100,100,100],
    );
  my @rat=(20.,12.,8.,5.,3.,2.,1.,0.4);
  my @lrat=('20','12',' 8',' 5',' 3',' 2',' 1','.4');
  my $pi=atan2(1,1)*4;
  my $bad_value=99999;
  my $missing = -8888;
  my ($k,$t,$td,$ws,$wd,$p,$ts,$y1,$y2,$tk,$x,$y,$xo);
  my ($date,$st_id,$lat,$lon,$name);
#
  open(OUT,"| psxy -JX6/6 -R-19/27.11/-0.9346217/44.061 -X1.5 -Y2 -K > skt_${fileID}.ps");
  print OUT "$xb[0] $yb[0]\n $xb[1] $yb[1]\n";
  close(OUT);
  open(OUT,"| psxy -JX -R -O -K >> skt_${fileID}.ps");
  for($i=1;$i<=6;$i++) {
    print OUT "$xb[$i] $yb[$i]\n";
  }
  close(OUT);
#
###
sub fy {
  $yy=132.182-44.061*log($_[0])/log(10);
  return $yy;
}
###
sub fx {
  $xx=0.54*$_[0]+0.90692*$_[1];
  return $xx;
}
###
sub tmr {
  $x=log($_[0]*$_[1]/(622+$_[0]))/log(10);
  $y=10**(.0498646455*$x+2.4082965)-7.07475+38.9114*
     ((10.**( .0915*$x ) - 1.2035 )**2 );
  return $y;
}
###
sub os {
  $x=$_[0]*((1000/$_[1])**0.286)/(exp(-2.6518986*&w($_[0],$_[1])/$_[0]));
  return $x;
}
###
sub tsa {
  $a=$_[0];
  $tq=253.16;
  $d=120;
  for($i=1;$i<=12;$i++) {
    $d=$d/2;
    $x=$a*exp(-2.6518986*&w($tq,$_[1])/$tq)-$tq*((1000/$_[1])**0.286);
    if(abs($x) >= 0.01) {
      if($d*$x > 0) {
        $tq += $d;
      } elsif($d*$x < 0) {
        $tq -= $d;
      }
    }
  }
  return $tq;
}
###
sub w {
  $x= &esat($_[0]);
  $y= 621.97*$x/($_[1]-$x);
  $y=0 if($_[0] > 999);
  return $y;
}
###
sub esat {
  $tc=$_[0]-273.16;
  $x=6.1078*exp((17.2693882*$tc)/($tc+237.3));
  return $x;
}
###
$abz=273.16;
#
# Draw pressure lines!
#
for($k=0;$k<11;$k++) {
  $y1= &fy($plv[$k]);
  open(OUT,"| psxy -JX -R -O -K >> skt_${fileID}.ps");
  if(($k != 0) && ($k != 10)) {
     print OUT "$pln[0][$k] $y1\n$pln[1][$k] $y1";
  }
  close(OUT);
  open(OUT,"| pstext -JX -R -N -O -K >> skt_${fileID}.ps");
  print OUT "-19.2 $y1 10 0 4 7 $plv[$k]";
  close(OUT);
}
#
# Draw temperature lines!
#
$t=40;
for($i=0;$i<15;$i++) {
  $y1= &fy($tp[0][$i]);
  $y2= &fy($tp[1][$i]);
  $x1= &fx($t,$y1);
  $x2= &fx($t,$y2);
  open(OUT,"| psxy -JX -R -O -K >> skt_${fileID}.ps");
  print OUT "$x1 $y1\n$x2 $y2";
  close(OUT);
  $x2 += 0.4;
  $y2 += 0.441;
# if($t == 20) {
#   $t -= 10;
#   next;
# }
  open(OUT,"| pstext -JX -R -N -O -K >> skt_${fileID}.ps");
  print OUT "$x2 $y2 10 47 4 5 $t";
  close(OUT);
  $t -= 10;
}
#
# Tick marks at 500 mb!
#
$y1=13.2627;
$y2=13.75;
$t=-52;
for($i=1;$i<=31;$i++) {
  $t += 2;
  next if(($t%10) == 0);
  $x1= &fx($t,$y1);
  $x2= &fx($t,$y2);
  open(OUT,"| psxy -JX -R -O -K >> skt_${fileID}.ps");
  print OUT "$x1 $y1\n$x2 $y2";
  close(OUT);
}
#
# Draw mixing ratio lines
#
$y1= &fy(1050);
$y2= &fy(700);
$yy=$y2+0.4;
for($i=0;$i<8;$i++) {
  $x1= &fx(&tmr($rat[$i],1050)-$abz,$y1);
  $x2= &fx(&tmr($rat[$i],700)-$abz,$y2);
  open(OUT,"| psxy -JX -R -W2to -O -K >> skt_${fileID}.ps");
  print OUT "$x1 $y1\n$x2 $y2";
  close(OUT);
  open(OUT,"| pstext -JX -R -O -K >> skt_${fileID}.ps");
  print OUT "$x2 $yy 10 0 4 2 $rat[$i]";
  close(OUT);
}
#
# Draw saturated adiabats!
#
$ts=32;
for (1..7) {
  $p=1060;
  $tk=$ts+$abz;
  $aos= &os($tk,1000);
  open(OUT,"| psxy -JX -R -W2/0/255/0t20_5:10 -O -K >> skt_${fileID}.ps");
  open(OUT,"| psxy -JX -R -W2t20_5:10 -O -K >> skt_${fileID}.ps");
  for ($j=1; $j <= 86; $j++) {
    $p -= 10;
    $asta= &tsa($aos,$p)-$abz;
    $sy= &fy($p);
    $sx= &fx($asta,$sy);
    print OUT "$sx $sy\n";
  }
  close(OUT);
  open(OUT,"| pstext -JX -R -O -K >> skt_${fileID}.ps");
  $sy += 0.4;
  print OUT "$sx $sy 10 0 4 2 $ts";
  close(OUT);
  $ts -= 4;
}
#
# Draw dry adiabats!
#
$t=51;
for($i=1; $i <= 162; $i++) {
  $y45[$i]=66.67*(5.7625544-log($t+$abz));
  $t -= 1;
}
#
$t=450;
$td=52;
for (1..20) {
  $t -= 10;
  $k=0;
  $yd=66.67*(log($t)-5.7625544);
  open(OUT,"| psxy -JX -R -Wta -O -K >> skt_${fileID}.ps");
  for($j=1; $j <= 162; $j++) {
    $ypd=$y45[$j]+$yd;
    $tx=$td-$j;
    last if($ypd > 44.061);
    next if($ypd < -.9346217);
    $xpd= &fx($tx,$ypd);
    last if($xpd < -19);
    next if($xpd > 27.1);
    next if(($xpd > 18.6) && ($t > 350));
    $k += 1;
    $sx[$k]=$xpd;
    $sy[$k]=$ypd;
    print OUT "$xpd $ypd\n";
  }
  close(OUT);
#
  $x=$sx[$k-3];
  $y=$sy[$k-3];
  $x=-17.95 if($x < -15);
  $y=42.9 if($y > 40);
  open(OUT,"| pstext -JX -R -O -K >> skt_${fileID}.ps");
  print OUT "$x $y 10 0 4 6 $t";
  close(OUT);
}
$xo=39;
open(OUT,"| psxy -JX8/8 -R-8.4346217/51.561/-8.4346217/51.561 -X-1 -Y-1 -O -K >> skt_${fileID}.ps");
print OUT "$xo -.9346216\n$xo 44.06";
close(OUT);
open(OUT7,"| psxy -JX6/6 -R-19/27.11/-0.9346217/44.061 -W4/0/255/0 -X1 -Y1 -O -K >> skt_${fileID}.ps");
open(OUT8,"| psxy -JX6/6 -R-19/27.11/-0.9346217/44.061 -m -W4/0/255/0t30_15:10 -O -K >> skt_${fileID}.ps");
#
open(IN0,"$raob");
while (<IN0>) {
  chomp;
  @field=split;
  if($. == 1) {
    $date=$field[0];
    $st_id=$field[1];
    $lat=$field[2];
    $lon=$field[3];
    $name=$sname{$st_id};
    next;
  }
  $p=$field[0];
  next if($p < 100);
  $t=$field[1];
  $td=$field[2];
  $ws=$field[3]*1.9438; # convert m/s to knots
  $wd=$field[4]*$pi/180.;
  $y= &fy($p);
  $x= &fx($t,$y);
  $xd= &fx($td,$y);
  if($t < $bad_value && $t > $missing) {
    print OUT7 "$x $y\n";
  }
  if($td < $bad_value && $td > $missing) {
    print OUT8 "$xd $y\n";
  }
#  } else {
#    print OUT8 "> 99999 $y\n";
#  }
  next if(($field[3] == $bad_value ) || ($field[4] == $bad_value));
  next if(($field[3] == $missing ) || ($field[4] == $missing));
  $wd_new=2.5*$pi-$wd;
  $wd_new -= 2*$pi if($wd_new >= 2*$pi);
  $x1=$xo+2*cos($wd_new);
  $y1= $y+2*sin($wd_new);
  open(OUTX,"| psxy -JX8/8 -R-8.4346217/51.561/-8.4346217/51.561 -W2/0/255/0 -N -O -K >> skt_${fileID}.ps");
  print OUTX "$xo $y\n $x1 $y1";
  close(OUTX);
  $n_fl=int(($ws+2.5)/50);  # number of flag tails
  $n_bt=int(($ws+2.5-$n_fl*50)/10); # number of big tails
  $n_r=($ws+2.5)%10;
  if($n_r >= 5) {
    $n_st=1;  # There is a small tail!
  } else {
    $n_st=0;
  }
#
# Plot flag tail(s), if any!
#
  $ratio=1;
  if($n_fl > 0) {
    for($i=0; $i < $n_fl; $i++) {
       $wd_new_new=$wd_new-5/12*$pi;
       $wd_new_new += 2*$pi if($wd_new_new < 0);
       $x2=$xo+2*$ratio*cos($wd_new);
       $y2=$y+2*$ratio*sin($wd_new);
       $x3=$x2+1.25*cos($wd_new_new);
       $y3=$y2+1.25*sin($wd_new_new);
       $ratio -= 0.25;
       $x4=$xo+2*$ratio*cos($wd_new);
       $y4=$y+2*$ratio*sin($wd_new);
       open(OUTY,"| psxy -JX -R -W2/0/255/0 -L -G0/255/0 -N -O -K >> skt_${fileID}.ps");
       print OUTY "$x2 $y2\n$x3 $y3\n$x4 $y4";
       close(OUTY);
    }
  }
#
# Plot big tail(s), if any!
  if($n_bt > 0) {
    for($i=0; $i < $n_bt; $i++) {
       $wd_new_new=$wd_new-5/12*$pi;
       $wd_new_new += 2*$pi if($wd_new_new < 0);
       $x2=$xo+2*$ratio*cos($wd_new);
       $y2=$y+2*$ratio*sin($wd_new);
       $x3=$x2+1.25*cos($wd_new_new);
       $y3=$y2+1.25*sin($wd_new_new);
       open(OUTZ,"| psxy -JX -R -W2/0/255/0 -N -O -K >> skt_${fileID}.ps");
       print OUTZ "$x2 $y2\n$x3 $y3";
       close(OUTZ);
       $ratio -= 0.15;
    }
  }
#
# Plot small tail, if any!
#
  if($n_st == 1) {
    $wd_new_new=$wd_new-5/12*$pi;
    $wd_new_new += 2*$pi if($wd_new_new < 0);
    $x2=$xo+2*$ratio*cos($wd_new);
    $y2=$y+2*$ratio*sin($wd_new);
    $x3=$x2+0.625*cos($wd_new_new);
    $y3=$y2+0.625*sin($wd_new_new);
    open(OUTA,"| psxy -JX -R -W2/0/255/0 -N -O -K >> skt_${fileID}.ps");
    print OUTA "$x2 $y2\n$x3 $y3";
    close(OUTA);
  }
}
close(IN0);
close(OUT7);
close(OUT8);
#
# Plot model pseudo-sounding!
#
if(-e "$wrf") {
  open(IN1,"$wrf");
$xm=42;
open(OUT,"| psxy -JX8/8 -R-8.4346217/51.561/-8.4346217/51.561 -X-1 -Y-1 -O -K >> skt_${fileID}.ps");
print OUT "$xm -.9346216\n$xm 44.06";
close(OUT);
  open(OUT7,"| psxy -JX6/6 -R-19/27.11/-0.9346217/44.061 -W4/0/0/255 -X1 -Y1 -O -K >> skt_${fileID}.ps");
  open(OUT8,"| psxy -JX6/6 -R-19/27.11/-0.9346217/44.061 -W4/0/0/255t30_15:10 -O -K >> skt_${fileID}.ps");
#
  while (<IN1>) {
    next if($. == 1);
    chomp;
    @field=split;
    $p=$field[0];
    next if($p < 100);
    $t=$field[1];
    $td=$field[2];
    $ws=$field[3]*1.9438;
    $wd=$field[4]*$pi/180.;
    $y= &fy($p);
    $x= &fx($t,$y);
    $xd= &fx($td,$y);
    if($t < $bad_value && $t > $missing) {
      print OUT7 "$x $y\n";
    }
    if($td < $bad_value && $td > $missing) {
      print OUT8 "$xd $y\n";
    }
#    } else {
#      print OUT8 "> 99999 $y\n";
#    }
    next if(($field[3] == $bad_value ) || ($field[4] == $bad_value));
    next if(($field[3] == $missing ) || ($field[4] == $missing));
    $wd_new=2.5*$pi-$wd;
    $wd_new -= 2*$pi if($wd_new >= 2*$pi);
    $x1=$xm+2*cos($wd_new);
    $y1= $y+2*sin($wd_new);
    open(OUTX,"| psxy -JX8/8 -R-8.4346217/51.561/-8.4346217/51.561 -W2/0/0/255 -N -O -K >> skt_${fileID}.ps");
    print OUTX "$xm $y\n $x1 $y1";
    close(OUTX);
    $n_fl=int(($ws+2.5)/50);  # number of flag tails
    $n_bt=int(($ws+2.5-$n_fl*50)/10); # number of big tails
    $n_r=($ws+2.5)%10;
    if($n_r >= 5) {
      $n_st=1;  # There is a small tail!
    } else {
      $n_st=0;
    }
#
# Plot flag tail(s), if any!                                             
#
    $ratio=1;                                              
    if($n_fl > 0) {   
      for($i=0; $i < $n_fl; $i++) {
         $wd_new_new=$wd_new-5/12*$pi;  
         $wd_new_new += 2*$pi if($wd_new_new < 0);
         $x2=$xm+2*$ratio*cos($wd_new);
         $y2=$y+2*$ratio*sin($wd_new);
         $x3=$x2+1.25*cos($wd_new_new);
         $y3=$y2+1.25*sin($wd_new_new);
         $ratio -= 0.25;
         $x4=$xm+2*$ratio*cos($wd_new);
         $y4=$y+2*$ratio*sin($wd_new);
         open(OUTY,"| psxy -JX -R -W2/0/0/255 -L -G0/0/255 -N -O -K >> skt_${fileID}.ps");
         print OUTY "$x2 $y2\n$x3 $y3\n$x4 $y4";
         close(OUTY);
      }
    }
#
# Plot big tail(s), if any!
    if($n_bt > 0) {
      for($i=0; $i < $n_bt; $i++) {
         $wd_new_new=$wd_new-5/12*$pi;
         $wd_new_new += 2*$pi if($wd_new_new < 0);                     
         $x2=$xm+2*$ratio*cos($wd_new);                                
         $y2=$y+2*$ratio*sin($wd_new);
         $x3=$x2+1.25*cos($wd_new_new);
         $y3=$y2+1.25*sin($wd_new_new);
         open(OUTZ,"| psxy -JX -R -W2/0/0/255 -N -O -K >> skt_${fileID}.ps");
         print OUTZ "$x2 $y2\n$x3 $y3";
         close(OUTZ);
         $ratio -= 0.15;     
      }            
    }
#
# Plot small tail, if any!
#
    if($n_st == 1) {
      $wd_new_new=$wd_new-5/12*$pi;
      $wd_new_new += 2*$pi if($wd_new_new < 0);
      $x2=$xm+2*$ratio*cos($wd_new);
      $y2=$y+2*$ratio*sin($wd_new);
      $x3=$x2+0.625*cos($wd_new_new);
      $y3=$y2+0.625*sin($wd_new_new);
      open(OUTA,"| psxy -JX -R -W2/0/0/255 -N -O -K >> skt_${fileID}.ps");
      print OUTA "$x2 $y2\n$x3 $y3";
      close(OUTA);
    }
  }
}
close(IN1);
close(OUT7);
close(OUT8);
#
# Plot reference model pseudo-sounding!
#
if(-e $ref) {
  open(IN2,"$ref");
$xr=45;
open(OUT,"| psxy -JX8/8 -R-8.4346217/51.561/-8.4346217/51.561 -X-1 -Y-1 -O -K >> skt_${fileID}.ps");
print OUT "$xr -.9346216\n$xr 44.06";
close(OUT);
  open(OUT7,"| psxy -JX6/6 -R-19/27.11/-0.9346217/44.061 -W4/255/0/0 -X1 -Y1 -O -K >> skt_${fileID}.ps");
  open(OUT8,"| psxy -JX6/6 -R-19/27.11/-0.9346217/44.061 -W4/255/0/0t30_15:10 -O -K >> skt_${fileID}.ps");
#
  while (<IN2>) {
    next if($. == 1);
    chomp;
    @field=split;
    $p=$field[0];
    next if($p < 100);
    $t=$field[1];
    $td=$field[2];
    $ws=$field[3]*1.9438;
    $wd=$field[4]*$pi/180.;
    $y= &fy($p);
    $x= &fx($t,$y);
    $xd= &fx($td,$y);
    if($t < $bad_value && $t > $missing) {
      print OUT7 "$x $y\n";
    }
    if($td < $bad_value && $td > $missing) {
      print OUT8 "$xd $y\n";
    }
#    } else {
#      print OUT8 "> 99999 $y\n";
#    }
    next if(($field[3] == $bad_value ) || ($field[4] == $bad_value));
    next if(($field[3] == $missing ) || ($field[4] == $missing));
    $wd_new=2.5*$pi-$wd;
    $wd_new -= 2*$pi if($wd_new >= 2*$pi);
    $x1=$xr+2*cos($wd_new);
    $y1= $y+2*sin($wd_new);
    open(OUTX,"| psxy -JX8/8 -R-8.4346217/51.561/-8.4346217/51.561 -W2/255/0/0 -N -O -K >> skt_${fileID}.ps");
    print OUTX "$xr $y\n $x1 $y1";
    close(OUTX);
    $n_fl=int(($ws+2.5)/50);  # number of flag tails
    $n_bt=int(($ws+2.5-$n_fl*50)/10); # number of big tails
    $n_r=($ws+2.5)%10;
    if($n_r >= 5) {
      $n_st=1;  # There is a small tail!
    } else {
      $n_st=0;
    }
#
# Plot flag tail(s), if any!                                             
#
    $ratio=1;                                              
    if($n_fl > 0) {   
      for($i=0; $i < $n_fl; $i++) {
         $wd_new_new=$wd_new-5/12*$pi;  
         $wd_new_new += 2*$pi if($wd_new_new < 0);
         $x2=$xr+2*$ratio*cos($wd_new);
         $y2=$y+2*$ratio*sin($wd_new);
         $x3=$x2+1.25*cos($wd_new_new);
         $y3=$y2+1.25*sin($wd_new_new);
         $ratio -= 0.25;
         $x4=$xr+2*$ratio*cos($wd_new);
         $y4=$y+2*$ratio*sin($wd_new);
         open(OUTY,"| psxy -JX -R -W2/255/0/0 -L -G255/0/0 -N -O -K >> skt_${fileID}.ps");
         print OUTY "$x2 $y2\n$x3 $y3\n$x4 $y4";
         close(OUTY);
      }
    }
#
# Plot big tail(s), if any!
    if($n_bt > 0) {
      for($i=0; $i < $n_bt; $i++) {
         $wd_new_new=$wd_new-5/12*$pi;
         $wd_new_new += 2*$pi if($wd_new_new < 0);                     
         $x2=$xr+2*$ratio*cos($wd_new);                                
         $y2=$y+2*$ratio*sin($wd_new);
         $x3=$x2+1.25*cos($wd_new_new);
         $y3=$y2+1.25*sin($wd_new_new);
         open(OUTZ,"| psxy -JX -R -W2/255/0/0 -N -O -K >> skt_${fileID}.ps");
         print OUTZ "$x2 $y2\n$x3 $y3";
         close(OUTZ);
         $ratio -= 0.15;     
      }            
    }
#
# Plot small tail, if any!
#
    if($n_st == 1) {
      $wd_new_new=$wd_new-5/12*$pi;
      $wd_new_new += 2*$pi if($wd_new_new < 0);
      $x2=$xr+2*$ratio*cos($wd_new);
      $y2=$y+2*$ratio*sin($wd_new);
      $x3=$x2+0.625*cos($wd_new_new);
      $y3=$y2+0.625*sin($wd_new_new);
      open(OUTA,"| psxy -JX -R -W2/255/0/0 -N -O -K >> skt_${fileID}.ps");
      print OUTA "$x2 $y2\n$x3 $y3";
      close(OUTA);
    }
  }
}
close(IN2);
close(OUT7);
close(OUT8);
open(TEXT,"| pstext -JX8/0.5 -R0/12/0/10 -X-0.75 -Y-0.75 -O -K >> skt_${fileID}.ps");
print TEXT "1 7.5 12 0 4 5 Date: $date\n";
close(TEXT);
#
  open(LEG,"| psxy -JX -R -W4/0/255/0 -O -K >> skt_${fileID}.ps");
  print LEG "7.0 7.5\n7.25 7.5";
  close(LEG);
  open(TEXT,"| pstext -JX -R -O -K >> skt_${fileID}.ps");
  print TEXT "7.35 7.5 12 0 4 5 RAOB\n";
  close(TEXT);
  open(LEG,"| psxy -JX -R -W4/0/0/255 -O -K >> skt_${fileID}.ps");
  print LEG "8.25 7.5\n8.5 7.5";
  close(LEG);
  open(TEXT,"| pstext -JX -R -O -K >> skt_${fileID}.ps");
  print TEXT "8.6 7.5 12 0 4 5 WRF D$GRID{$st_id}\n";
  close(TEXT);
  open(LEG,"| psxy -JX -R -W4/255/0/0 -O -K >> skt_${fileID}.ps");
  print LEG "9.5 7.5\n9.75 7.5";
  close(LEG);
  open(TEXT,"| pstext -JX -R -O -K >> skt_${fileID}.ps");
  print TEXT "9.85 7.5 12 0 4 5 $REF_MODEL\n";
  close(TEXT);
  open(TEXT,"| pstext -JX -R -O >> skt_${fileID}.ps");
  print TEXT "1 2.5 12 0 4 5 Lat: $lat  Lon: $lon  Station: $st_id  $name\n";
  close(TEXT);

  foreach $ps (<*.ps>) {
     $gif = $ps;
     $gif =~ s/\.ps$/\.gif/;
     system("convert -trim +repage -flatten $ps $gif");
     unlink "$ps";
  }

  return;
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

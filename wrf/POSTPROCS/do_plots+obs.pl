
sub do_plots
{
my ($hourly_file,$domain,$cycle,$valid_time,$dest,$dest_small,$PROC_OBS) = @_;
my $dd;
my $valid_time_short;

print "\nin do_plots $hourly_file,$domain, $cycle, $valid_time, $dest, $dest_small, $PROC_OBS\n\n";
##
## Special version to do EPG plots based on WSMR Domain2...
##
#$DO_EPG = 0;
## Special version to do UAE D3 plots based on UAE Domain2...
##$DO_UAED3 = 0;
## Special version to do MAGEN north/south plots based on Domain3
#$DO_MAGEN4 = 1;

$ENV{'NCARG_ROOT'} = "$NCARG_ROOT";
$ENV{'NCARG_LIB'} = "$NCARG_LIB";
$ENV{'NCARG_RANGS'} = $NCARG_RANGS_DIR;

$RAP_RTFDDA = "$RUNDIR/$cycle/RAP_RTFDDA";

if (! @SEASONS) {  # best if defined in postprocinput.pl
   @SEASONS=( 'winter', 'winter', 'winter', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'winter', 'winter');
}

if ( $IS_WRF || $IS_RIP4 ) {
   if ( $IS_WRF ) {
      $RIPDP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_wrf.exe";
      $RIPDP_OBS = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_wrf.exe";
   } else { 
      $RIPDP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_mm5.exe";
      $RIPDP_OBS = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_obs.exe";
   }
   $RIP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip.exe";
   $ENV{'RIP_ROOT'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";
   $RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";
   if ( -e "$GSJOBDIR/postprocs/stationlist" ) {
     $ENV{'STATIONLIST'} = "$GSJOBDIR/postprocs/stationlist";
   } else {
     $ENV{'STATIONLIST'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4/stationlist";
   }
} else {
   $RIPDP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_new.exe";
   $RIPDP_OBS = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_obs.exe";
   $RIP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip_new.exe";
   $ENV{'RIP_ROOT'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
   $RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
   if ( -e "$GSJOBDIR/postprocs/stationlist" ) {
     $ENV{'STATIONLIST'} = "$GSJOBDIR/postprocs/stationlist";
   } else {
     $ENV{'STATIONLIST'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/stationlist";
   }
}
$ENV{'HIRESMAP'} = "$RIP_ROOT/${RANGE}_map.ascii";
$ENV{'RANGEMAP'} = "$RIP_ROOT/${RANGE}_map.ascii";
$ENV{'stationg3'} = "$RIP_ROOT/stationg3";
$ENV{'stationg4'} = "$RIP_ROOT/stationg4";
#
$RIP_OBS = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip_obs.exe";

if ( $RIP_IMG ne "ps" and $RIP_IMG ne "cgm" ) {
   $RIP_IMG = "cgm";
}
if ( $WEB_IMG ne "gif" and $WEB_IMG ne "png" ) {
   $WEB_IMG = "gif";
}

# Add IC/BC source to model plots

if ($BCS eq 'AVNFTP') {
   $BCS = 'GFS';
}

# Add cycle start offset to model plots

if (defined $CYCLE_START_OFFSET) {
   $SOFFSET = sprintf "%.0f",$CYCLE_START_OFFSET*60;
}

#$fcoffset = int($time_max / 60.); # Wrong! Now calculated in do_output_gmod.pl
if ($lead_time <= 0) {
   $lead_time_pos = 0;
   $lead_time_neg = 0;
} else {
   $lead_time_pos = ${lead_time};
   $lead_time_neg = $lead_time * (-1);
}

system("mkdir -p $PLOTS_DIR");
system("mkdir -p $PLOTS_DIR/ripscr");
system("mkdir -p $PLOTS_DIR/ripscr_obs");
system("mkdir -p $PLOTS_DIR/riprun");
system("mkdir -p $PLOTS_DIR/riprun_obs");
chdir $PLOTS_DIR;

system("scrub 1/6 $PLOTS_DIR/ripscr");
system("scrub -d 1/6 $PLOTS_DIR/gifs_ugui");
system("scrub -d 1/6 $PLOTS_DIR/gifs_small");
system("rm -f $PLOTS_DIR/riprun/*");
system("rm -f $PLOTS_DIR/riprun_obs/*");
system("rm -f $PLOTS_DIR/ripscr_obs/*");

print "WRF $IS_WRF \n";
if ( $IS_WRF ) {
   @parts = split("d0",$hourly_file);
   $filen = @parts[0];
   $wrf_date_string = substr(@parts[1],2);
} else {
   @parts = split("DOMAIN",$hourly_file);
   $filen = @parts[0];
#   $range = substr( @parts[1], 2, 10);
}
$range = $RANGE;
if ( $IS_WRF ) {
   $month = substr( $valid_time, 4, 2);
} else {
   $month = substr( $valid_time, 4, 2);
}
$season = @SEASONS[$month-1];

  #foreach $d ( 1..$ndom) {

     $d = $domain;

     if ( $IS_WRF ) {
        $fn = $filen . "d0" . $d;
        $fn_f0 = $filen . "d0" . $d . "_0";
        $fn_m1 = $filen . "d0" . $d . "-1";
     } else {
        $fn = $filen . "DOMAIN" . $d . "." . $range;
     }

     if ( -e $fn ) {
       print "fn  $fn\n";
       chdir $PLOTS_DIR;
       chdir "ripscr";

       if ( $IS_WRF ) {
         $command00 = "$RIPDP_EXE Domain_$d all $fn_f0 > $PLOTS_DIR/ripdp${d}_f0.log 2>&1\n";
         $command0 = "$RIPDP_EXE Domain_$d all $fn_m1 > $PLOTS_DIR/ripdp${d}_m1.log 2>&1\n";
         $command = "$RIPDP_EXE Domain_$d all $fn > $PLOTS_DIR/ripdp$d.log 2>&1\n";
#     die "in do_plots $command\n";
       } else {
         $command = "$RIPDP_EXE Domain_$d $fn > $PLOTS_DIR/ripdp$d.log 2>&1\n";
       }
      #print "lead_time in do_plots passed in from do_output_gmod.pl = $lead_time\n";
       if ( (-e $fn_f0) && ($lead_time > 1) ) {
          print "$command00\n";
          system("$command00");
          open(XTIMES, "Domain_$d.xtimes");
          while (<XTIMES>) {
            chomp;
            $xtf0 = $_;
           #print "xtf0 here = $xtf0\n";
          }
          close(XTIMES);
       }
      #print "xtf0 final = $xtf0\n";
       print "$command0\n";
       system("$command0");
       print "$command\n";
       system("$command");

       open(XTIMES, "Domain_$d.xtimes");
       $ntimes = <XTIMES>; 
       while ( $xtin = <XTIMES>) {
          $xt = $xtin;
       }
       close(XTIMES);
       chomp($xt);
       $newfile = sprintf( "%s/ripscr/Domain_%s.xtimes", $PLOTS_DIR, $d);

       if ( $xt >= 1.0 ) {
         $xtprev = $xt - 1;
         if ( $IS_WRF ) {
         $xtout = sprintf("%010.5f\n",$xtprev);
         $xtoute = sprintf("%010.5f",$xtprev);
         } else {
         $xtout = sprintf("%09.5f\n",$xtprev);
         $xtoute = sprintf("%09.5f",$xtprev);
         }
         $checkfile = sprintf("%s/ripscr/Domain_1_%s_tmk",$PLOTS_DIR,$xtoute);
         if ( -e "$checkfile" ) {
           open(OUTFILE, ">$newfile") || do {
              warn "Cannot open output file: $newfile\n$!\n";
           };

           if ( (-e $fn_f0) && ($lead_time > 1) ) {
              print OUTFILE "3\n";
              print OUTFILE "$xtf0\n";
           }  else {
              print OUTFILE "2\n";
           }
           print OUTFILE "$xtout";
           print OUTFILE "$xt";
           close(OUTFILE);
         }
       }

       chdir $PLOTS_DIR;
       chdir "riprun";

       if ( $d == 1 ) {
         $xt_d1 = $xt
       }
       if ( $xt != $xt_d1 ) {
#      If this domain doesn't run as long as D1...
          system("rm -f domain$d.cgm");
       } else {
          if ( -e "$GSJOBDIR/postprocs/Mdomain$d.$season.$range" ) {
            print "getting $GSJOBDIR/postprocs/Mdomain$d.$season.$range\n";
            system("cp $GSJOBDIR/postprocs/Mdomain$d.$season.$range domain$d.1");
          } else {
            print "getting $RIP_ROOT/namelists/Mdomain$d.$season.$range\n";
            system("cp $RIP_ROOT/namelists/Mdomain$d.$season.$range domain$d.1");
          }
          open(PLT_TABLE,"domain$d.1");
          open(PLT_TABLE_NEW,">domain$d.in");
          while (<PLT_TABLE>) {
                s/plttime/$xt/g;
                s/rangename/$range/g;
                s/_BCS_/$BCS/;
                s/_SOFFSET_/$SOFFSET/;
                s/cycle/$cycle/g;
                s/OFFSET/$fcoffset/g;
                s/IMGFMT/$RIP_IMG/g;
                s/HOURSBACK/$lead_time_neg/g;
                s/FCLEADHRS/${lead_time_pos}h/g;
                print PLT_TABLE_NEW;
          } 
          close(PLT_TABLE);
          close(PLT_TABLE_NEW);

          if ( -e  "$GSJOBDIR/postprocs/Mdomain$d.sloc.$range" ) {
             system("cat $GSJOBDIR/postprocs/Mdomain$d.sloc.$range >> domain$d.in");
          } else {
             system("cat $RIP_ROOT/namelists/Mdomain$d.sloc.$range >> domain$d.in");
          }

          system("$RIP_EXE -f ../ripscr/Domain_$d domain${d}.in  > $PLOTS_DIR/rip$d.log 2>&1");

##        For EPG
          if ( $d == 2 && $DO_EPG == 1 ) {
             system("cat $RIP_ROOT/namelists/Mdomain4.$season.$range | sed s/plttime/$xt/g > domain4.1");
             system("cat domain4.1 | sed s/rangename/$range/g > domain4.2");
             system("cat domain4.2 | sed s/cycle/$cycle/g > domain4.3");
             if ( -e  "$GSJOBDIR/sloc4.in" ) {
                system("cat domain4.3 $GSJOBDIR/sloc4.in > domain4.in");
             } else {
                system("cat domain4.3 $RIP_ROOT/namelists/Mdomain4.sloc.$range > domain4.in");
             }


             system("$RIP_EXE -f ../ripscr/Domain_$d domain4  > $PLOTS_DIR/rip4.log 2>&1");
##        End for EPG
          }

##        For UAED3
          if ( $d == 2 && $DO_UAED3 == 1 ) {
             if ( -e "$GSJOBDIR/postprocs/Mdomain3.$season.$range" ) {
               system("cat $GSJOBDIR/postprocs/Mdomain3.$season.$range | sed s/plttime/$xt/g > domain3.1");
             } else {
               system("cat $RIP_ROOT/namelists/Mdomain3.$season.$range | sed s/plttime/$xt/g > domain3.1");
             }
             system("cat domain3.1 | sed s/rangename/$range/g > domain3.2");
             system("cat domain3.2 | sed s/cycle/$cycle/g > domain3.3");
             if ( -e  "$GSJOBDIR/postprocs/Mdomain3.sloc.$range" ) {
                system("cat domain3.3 $GSJOBDIR/postprocs/Mdomain3.sloc.$range > domain3.in");
             } else {
                system("cat domain3.3 $RIP_ROOT/namelists/Mdomain3.sloc.$range > domain3.in");
             }


             system("$RIP_EXE -f ../ripscr/Domain_$d domain3  > $PLOTS_DIR/rip3.log 2>&1");
##           End for UAED3
          }
##        For MAGEN
          if ( $d == $NUM_DOMS && $DO_MAGEN4 == 1 ) {
## North D3
             if ( -e "$GSJOBDIR/postprocs/Mdomain4.$season.$range" ) {
               system("cp $GSJOBDIR/postprocs/Mdomain4.$season.$range domain4.1");
             } else {
               system("cp $RIP_ROOT/namelists/Mdomain4.$season.$range domain4.1");
             }
             open(PLT_TABLE,"domain4.1");
             open(PLT_TABLE_NEW,">domain4.in");
             while (<PLT_TABLE>) {
                   s/plttime/$xt/g;
                   s/rangename/$range/g;
                   s/_BCS_/$BCS/;
                   s/_SOFFSET_/$SOFFSET/;
                   s/cycle/$cycle/g;
                   s/OFFSET/$fcoffset/g;
                   s/IMGFMT/$RIP_IMG/g;
                   s/HOURSBACK/$lead_time_neg/g;
                   s/FCLEADHRS/${lead_time}h/g;
                   print PLT_TABLE_NEW;
             } 
             close(PLT_TABLE);
             close(PLT_TABLE_NEW);

             if ( -e  "$GSJOBDIR/postprocs/Mdomain4.sloc.$range" ) {
                system("cat $GSJOBDIR/postprocs/Mdomain4.sloc.$range >> domain4.in");
             } else {
                system("cat $RIP_ROOT/namelists/Mdomain4.sloc.$range >> domain4.in");
             }

             system("$RIP_EXE -f ../ripscr/Domain_$d domain4  > $PLOTS_DIR/rip4.log 2>&1");
## South D3  
             if ( -e "$GSJOBDIR/postprocs/Mdomain5.$season.$range" ) {
               system("cp $GSJOBDIR/postprocs/Mdomain5.$season.$range domain5.1");
             } else {
               system("cp $RIP_ROOT/namelists/Mdomain5.$season.$range  domain5.1");
             }
             open(PLT_TABLE,"domain5.1");
             open(PLT_TABLE_NEW,">domain5.in");
             while (<PLT_TABLE>) {
                   s/plttime/$xt/g;
                   s/rangename/$range/g;
                   s/_BCS_/$BCS/;
                   s/_SOFFSET_/$SOFFSET/;
                   s/cycle/$cycle/g;
                   s/OFFSET/$fcoffset/g;
                   s/IMGFMT/$RIP_IMG/g;
                   s/HOURSBACK/$lead_time_neg/g;
                   s/FCLEADHRS/${lead_time}h/g;
                   print PLT_TABLE_NEW;
             } 
             close(PLT_TABLE);
             close(PLT_TABLE_NEW);

             if ( -e  "$GSJOBDIR/postprocs/Mdomain5.sloc.$range" ) {
                system("cat $GSJOBDIR/postprocs/Mdomain5.sloc.$range >> domain5.in");
             } else {
                system("cat $RIP_ROOT/namelists/Mdomain5.sloc.$range >> domain5.in");
             }

             system("$RIP_EXE -f ../ripscr/Domain_$d domain5  > $PLOTS_DIR/rip5.log 2>&1");
          } ## End MAGEN

       }
       $field_file = "$GSJOBDIR/postprocs/image_fields.pl";
       &conv_img( "domain$d.$RIP_IMG", $d, $valid_time, $RIP_IMG, $WEB_IMG, $field_file);
       print "ran conv_img domain$d.$RIP_IMG, $d, $valid_time, $RIP_IMG, $WEB_IMG\n";

##     For EPG
       if ( $d == 2 && $DO_EPG == 1 ) {
         &conv_img( "domain4.$RIP_IMG", 4, $valid_time, $RIP_IMG, $WEB_IMG, $field_file);
       }

##     For UAED3
       if ( $d == 2 && $DO_UAED3 == 1 ) {
         &conv_img( "domain3.$RIP_IMG", 3, $valid_time, $RIP_IMG, $WEB_IMG, $field_file);
       }

##     For MAGEN
       if ( $d == $NUM_DOMS && $DO_MAGEN4 == 1 ) {
         &conv_img( "domain4.$RIP_IMG", 4, $valid_time, $RIP_IMG, $WEB_IMG, $field_file);
         &conv_img( "domain5.$RIP_IMG", 5, $valid_time, $RIP_IMG, $WEB_IMG, $field_file);
       }

       if ( $IS_WRF ) {
#         Rong's addition 2
          chdir $PLOTS_DIR;

          $valid_m1 = &hh_advan_date( $valid_time, -1);
          $date_time = dtstring($valid_time);
          $date_time_m1 = dtstring($valid_m1);

          print "date_time = ${date_time}; date_time_m1 = ${date_time_m1}\n";

          if ($PROC_OBS) {

          if (-s "$RAP_RTFDDA/qc_out_${date_time_m1}:00:00.${RANGE}_F" &&
              -s "$RAP_RTFDDA/qc_out_${date_time}:00:00.${RANGE}_F") {

             system("cat $RAP_RTFDDA/qc_out_${date_time_m1}:00:00.${RANGE}_F $RAP_RTFDDA/qc_out_${date_time}:00:00.${RANGE}_F > hourly.obs");

          } elsif (-s "$RAP_RTFDDA/qc_out_${date_time_m1}:00:00.${RANGE}_F" &&
                 ! -s "$RAP_RTFDDA/qc_out_${date_time}:00:00.${RANGE}_F") {

             system("cp $RAP_RTFDDA/qc_out_${date_time_m1}:00:00.${RANGE}_F hourly.obs");

          } elsif (-s "$RAP_RTFDDA/qc_out_${date_time}:00:00.${RANGE}_F" &&
                   ! -s "$RAP_RTFDDA/qc_out_${date_time_m1}:00:00.${RANGE}_F") {

             system("cp $RAP_RTFDDA/qc_out_${date_time}:00:00.${RANGE}_F hourly.obs");

          } else {
             print "No obs plot for $valid_time; use $RIP_ROOT/not_yet_avail.gif\n";
             &conv2gif_obs( "domain$d.cgm", $d, $valid_time);
          }

          if (-s "hourly.obs") {

            $pwd = `pwd`;
            print "current directory = $pwd";

            system("cp $RUNDIR/$cycle/WRFQC_F/latlon.txt .") if (! -e 'latlon.txt');
            system("$CSH_ARCHIVE/Forecast/RT_all.obs_trim-merge.USA hourly.obs 30 latlon.txt > /dev/null");
            unlink 'hourly.obs';
          }

          } # end if ($PROC_OBS)

          print "valid_time = $valid_time\n";

          $valid_time_short = substr($valid_time,0,10);

          if (-s "${valid_time_short}.hourly.obs") {
            system("$EXECUTABLE_ARCHIVE/QCtoNC.exe ${valid_time_short}.hourly.obs");
           #system("rm -f *.hourly.obs") if($DEL_OBS);
            system("ln -sf $POSTPROCS_DIR/NCL/StationModel.ncl .");
            system("ln -sf $POSTPROCS_DIR/NCL/RTFDDAUser.ncl .");

            my $dd = sprintf("%02d",$d);

            system("ln -sf $GSJOBDIR/wps/geo_em.d$dd.nc .");

            system("ncl $POSTPROCS_DIR/NCL/SfcStatsThin.ncl 'Range=\"$RANGE\"' Date=$valid_time_short Domain=$d");

            system("ncl $POSTPROCS_DIR/NCL/UpperAirObs.ncl 'Range=\"$RANGE\"' Date=$valid_time_short Domain=$d");

            system("ncl $POSTPROCS_DIR/NCL/UpperAirObsSat.ncl 'Range=\"$RANGE\"' Date=$valid_time_short Domain=$d");

            system("ls -l");

            &conv2gif_obs( "domain$d.cgm", $d, $valid_time);
## For MAGEN
            if ( $d == 3 && $DO_MAGEN4 == 1 )   {
            &conv2gif_obs( "domain$d.cgm", 4, $valid_time);
            &conv2gif_obs( "domain$d.cgm", 5, $valid_time);
            }

            system("rm -f SFCplot.eps RAOBplot.ps UPPplot.ps");
          }
       } else {  # MM5 obs plot
#      End Rong's addition 2

#    Now do the observation plots --- using a custom version of RIP
          chdir $PLOTS_DIR;
          chdir "ripscr_obs";
          system("rm MMOUT.in");
          system("ln -s $fn MMOUT.in");
          system("$RIPDP_OBS Domain_$d MMOUT.in > $PLOTS_DIR/ripdp_obs$d.log 2>&1");

          chdir $PLOTS_DIR;
          $valid_m1 = &hh_advan_date( $valid_time, -1);

          chdir "riprun_obs";
          system("rm -f indata");
          $obs_f1 = $WORK_DIR . "/" . $valid_m1 . "_qc_obs_for_assimilation_s";
          $obs_f2 = $WORK_DIR . "/" . substr($valid_time,0,10) . "_qc_obs_for_assimilation_s";
          system("cat $obs_f1 $obs_f2 > indata");

          if ( -e  "$GSJOBDIR/postprocs/Mdomain3.sloc.$range" ) {
             system("cat $GSJOBDIR/postprocs/Mdomain$d.obs.$range | sed s/plttime/$xt/g > domain$d.1");
          } else {
            system("cat $RIP_ROOT/namelists/Mdomain$d.obs.$range | sed s/plttime/$xt/g > domain$d.1");
          }
          system("cat domain$d.1 | sed s/rangename/$range/g > domain$d.2");
          system("cat domain$d.2 | sed s/cycle/$cycle/g > domain$d.in");

          system("$RIP_OBS -f ../ripscr_obs/Domain_$d domain$d  > $PLOTS_DIR/rip_obs$d.log 2>&1");
          &conv2gif_obs( "domain$d.cgm", $d, $valid_time);

##        For EPG
          if ( $d == 2 && $DO_EPG == 1 ) {
             chdir $PLOTS_DIR;
             chdir "riprun_obs";
             system("cat $RIP_ROOT/namelists/Mdomain4.obs.$range | sed s/plttime/$xt/g > domain4.1");
             system("cat domain4.1 | sed s/rangename/$range/g > domain4.2");
             system("cat domain4.2 | sed s/cycle/$cycle/g > domain4.in");

             system("$RIP_OBS -f ../ripscr_obs/Domain_$d domain4  > $PLOTS_DIR/rip_obs4.log 2>&1");
             &conv2gif_obs( "domain4.cgm", 4, $valid_time);
##          End for EPG
          }
### For UAED3
#   if ( $d == 2 && $DO_UAED3 == 1 ) {
#     chdir $PLOTS_DIR;
#     chdir "riprun_obs";
#    system("cat $RIP_ROOT/namelists/Mdomain3.obs.$range | sed s/plttime/$xt/g > domain3.1");
#     system("cat domain3.1 | sed s/rangename/$range/g > domain3.2");
#     system("cat domain3.2 | sed s/cycle/$cycle/g > domain3.in");
#
#     system("$RIP_OBS -f ../ripscr_obs/Domain_$d domain3  > $PLOTS_DIR/rip_obs3.log 2>&1");
#     &conv2gif_obs( "domain3.cgm", 3, $valid_time);
###   End for UAED3
#     }
       } # end obs plotting, uncommented out by Rong

       chdir $PLOTS_DIR;
       chdir "gifs_ugui";

#     system ("ls -al $valid_time");

#     print "\nssh $RELAY_USER \"rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC $PLOTS_DIR/gifs_ugui/$valid_time $dest\"\n";
#     system("ssh $RELAY_USER \"rsync -e 'ssh -i /home/fddasys/.ssh/rtfdda' -avzC $PLOTS_DIR/gifs_ugui/$valid_time $dest\"");
# don't need this since we can ssh-fddasys-to-int2 now - for now -

       # Changing to a direct rsync instead of via ssh which is outdated 1-25-18
       system("chmod g+w $valid_time ");
       system("chmod g+w $valid_time/*.gif ");
       #print "\nrsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_time $dest\n";
       #system("rsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_time $dest");
       print "\nrsync  -avzC $PLOTS_DIR/gifs_ugui/$valid_time $JOB_LOC/gifs\n";
       system("rsync -avzC $PLOTS_DIR/gifs_ugui/$valid_time $JOB_LOC/gifs");


       if ( $DO_CYCLES_IMAGES ) {

         system( "mkdir -p $cycle" );
         system( "mkdir -p $cycle/gifs" );
         system("chmod g+w $cycle ");
         system("chmod g+w $cycle/gifs ");
         $dest_dir = "$JOB_LOC/cycles";
         system("rsync -avzC $cycle $dest_dir");
         $dest_dir = "$JOB_LOC/cycles/$cycle/gifs";
         #$dest_cycles = "$DEST_SERVER:$dest_dir";
         $dest_cycles = "$dest_dir";
         system("rsync -avzC $valid_time $dest_cycles");

       }
## For EPG
       if ( $DO_EPG == 1 ) {
         system("chmod g+w $valid_time ");
         system("chmod g+w $valid_time/*.gif ");
         system("rsync -e 'ssh -i $KEY' -avzC $valid_time carson\@atec-server:/d1/www/htdocs/images/epg/rtfdda/gifs");
       }
       chdir $PLOTS_DIR;
       if ( $DO_PLOTS_SMALL ) {
         $mkdir_command = "mkdir $JOB_LOC/gifs_small/$valid_time";
         system( "ssh 4dwx\@atec-server $mkdir_command" );
         system("scp -pr gifs_small/$valid_time/d$d\* $dest_small/$valid_time" );
         $scandir_command = "/www/cgi-bin/model/gmod/pda_gif_lister range=\"$USER/$JOB_ID\"";
         system( "ssh 4dwx\@atec-server $scandir_command" );
       }
     } else {
       print " WRF_FILE $WRF_FILE \n";
       print " error finding fn  $fn \n";

     }

  #} take out domain loop

print "\nexiting do_plots $hourly_file,$domain, $cycle, $valid_time, $dest, $dest_small, $PROC_OBS\n\n";

}
1;

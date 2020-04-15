
sub do_gbc
{
use Time::Local;

my ($hourly_file, $ndom, $cycle, $valid_t, $dest, $dest_small) = @_;
my $dd;
my $valid_t_short;

print "\nin do_gbc $hourly_file, $ndom, $cycle, $valid_t, $dest, $dest_small\n\n";

$ENV{'NCARG_ROOT'} = "$NCARG_ROOT";
$ENV{'NCARG_LIB'} = "$NCARG_LIB";
$ENV{'NCARG_RANGS'} = $NCARG_RANGS_DIR;
$FLEXINPUT = $ENV{FLEXINPUT};
#Import job definition environment variables
require $FLEXINPUT;

$CORRECT_FORECAST            = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/CorrectFcst";
$pre_analysis_config         = "$MM5HOME/cycle_code/POSTPROCS/gbc/gbc_config_final.plist";
$forecast_config             = "$MM5HOME/cycle_code/POSTPROCS/gbc/gbc_config_fcst.plist";
$correct_pre_analysis_config = "$MM5HOME/cycle_code/POSTPROCS/gbc/cf_config_final.plist";
$correct_forecast_config     = "$MM5HOME/cycle_code/POSTPROCS/gbc/cf_config_fcst.plist";
$bias_plot_T2                = "$MM5HOME/cycle_code/POSTPROCS/gbc/bias_T2_Q2.ncl";
$bias_plot_Wind              = "$MM5HOME/cycle_code/POSTPROCS/gbc/bias_barb.ncl";
$bias_plot_U10V10            = "$MM5HOME/cycle_code/POSTPROCS/gbc/bias_U10_V10.ncl";


$RAP_RTFDDA = "$RUNDIR/$cycle/RAP_RTFDDA";

system("$MustHaveDir $RUNDIR/merged_gbc");
system("cp $hourly_file $RUNDIR/merged_gbc");


@work_values = split('/',$WORK_DIR);
$time_step = $work_values[-1];
$logfile = "$RUNDIR/merged_gbc/out_$time_step.log";
open(OUTFILE, ">$logfile") || do {
            warn "Cannot open output file: $logfile\n$!\n";
         };
print OUTFILE "do_gbc: $ndom $cycle $valid_t $dest $dest_small\n";

#$hourly_file="/raid2/fdda-atec/cycles/GWDPG/DPG/postprocs/wrfout_d03_2010-01-19_15:00:00";
$WRF_FILE = basename($hourly_file);
$valid_t = substr($WRF_FILE, 11, 19);
print OUTFILE "do_gbc: $RUNDIR $hourly_file $WRF_FILE \n";
print OUTFILE "do_gbc: new valid_t $valid_t \n";

$hourly_file = "$RUNDIR/merged_gbc/$time_step/$WRF_FILE";
####################################################
# Correct each forecast hour as it becomes available
# with the bias
####################################################
$this_cycle = $cycle;
$cycle = substr($this_cycle,8,2);
$day   = substr($this_cycle,6,2);
$month = substr($this_cycle,4,2);
$year  = substr($this_cycle,0,4);

#getting the current cycle and 24+cycle time in GBC form
$current_cycle = $year."-".$month."-".$day."-".$cycle;
print OUTFILE "$cycle, $day, $month, $year%100 \n";
$test_date = timelocal(0,0,$cycle,$day,$month-1,$year%100);
$test_date_24 = $test_date - (24+$CYC_INT)*60*60;   #subtract 24+$CYC_INT hours worth of seconds
($year, $month, $day, $cycle) = (localtime($test_date_24))[5,4,3,2];
system("cp $RUNDIR/$test_date_24/bias/bias_warning $RUNDIR/web/status");

#Correct Forecast needs to use the same cycle (i.e. 17Z) of bias values
#48 hours in the past is the most recent bias calculation for this cycle that are done
$test_date_48 = $test_date - 48*60*60;   #subtract 48 hours worth of seconds
($bias_year, $bias_month, $bias_day, $bias_cycle) = (localtime($test_date_48))[5,4,3,2];
$bias_date = ($bias_year+1900)."-".($bias_month+1)."-".$bias_day."-".$bias_cycle;

#GBC starts with the fcst not the analysis
if ($CYC_INT == 6) {
  $bias_step = $time_step - $CYC_INT - 1; #6 hour cyclcing does an extra time step right now
} else {
  $bias_step = $time_step - $CYC_INT;
}

#$bias_date_compact = ($bias_year+1900).$bias_month.$bias_day.$bias_cycle;
$bias_date_compact = sprintf("%4d%02d%02d%02d", ($bias_year+1900), ($bias_month+1), $bias_day, $bias_cycle );

# Correct the current forecast output
print OUTFILE "\n$CORRECT_FORECAST $correct_forecast_config $current_cycle $bias_step $bias_date $RUNDIR $RANGE\n\n";
$cf_results = `$CORRECT_FORECAST $correct_forecast_config $current_cycle $bias_step $bias_date $RUNDIR $RANGE 2>&1`;

######### End correct forecast #####################

#if ($bias_step > 119 ) {
#  $bias_step = $bias_step - 120 
#}
#if ($bias_step > 95 ) {
#  $bias_step = $bias_step - 96
#}
#if ($bias_step > 71 ) {
#  $bias_step = $bias_step - 72
#}
#if ($bias_step > 47 ) {
#  $bias_step = $bias_step - 48
#}
#if ($bias_step > 23 ) {
#  $bias_step = $bias_step - 24
#}

#We only have 24 hours of bias data so for > 24 hours of forecast
#we reuse the bias 
#This should do integer division and subtract any multiple of 24 needed
$div = int($bias_step/24);
$bias_step = $bias_step - ($div*24);

if ($bias_step >= 0) {
   $bias_step_signed = sprintf "+%02d",$bias_step;
} else {
   $bias_step_signed = sprintf "-%02d",abs($bias_step);
}

$bias_file = "$RUNDIR/$bias_date_compact/bias/bias_d03_".$bias_date_compact.$bias_step_signed.".nc";
$corrected_file = "$RUNDIR/postprocs/corrected/$WRF_FILE";
$merged_file = "$RUNDIR/merged_gbc/$WRF_FILE";
print OUTFILE "Corrected file $corrected_file  Merged file $merged_file Bias file $bias_file\n";


#replace raw results with bias corrected data - $merged_file is the result
print OUTFILE "/opt/nco/bin/ncks -A -C -v T2,Q2,U10,V10 $corrected_file $merged_file\n";
$merge_results = `/opt/nco/bin/ncks -A -C -v T2,Q2,U10,V10 $corrected_file $merged_file 2>&1`;
#$merge_results = system("/opt/nco/bin/ncks -A -C -v T2,Q2,U10,V10 $corrected_file $merged_file");
#add attributes to the netcdf file
$attadd_results  = system("/opt/nco/bin/ncatted -a bias_corrected,T2,c,c,'TRUE' $merged_file");
$attadd_results  = system("/opt/nco/bin/ncatted -a bias_corrected,Q2,c,c,'TRUE' $merged_file");
$attadd_results  = system("/opt/nco/bin/ncatted -a bias_corrected,U10,c,c,'TRUE' $merged_file");
$attadd_results  = system("/opt/nco/bin/ncatted -a bias_corrected,U10,c,c,'TRUE' $merged_file");

#add PSFC to corrected_file so RH2M can be calculated
$psfc_results = `/opt/nco/bin/ncks -A -C -v PSFC $merged_file $corrected_file 2>&1`;

print OUTFILE "Merge results $merge_results\n";
###### End of correcting, merging and adding attributes, we should now have a file ready for image creation ######

#Make directory similar to one in postprocs
$GBC_WORK_DIR = "$RUNDIR/merged_gbc/$time_step";
print OUTFILE "GBC_WORK_DIR $GBC_WORK_DIR\n";
system("$MustHaveDir $GBC_WORK_DIR");
system("rm $GBC_WORK_DIR/wrfout_d03");
system("ln -s $merged_file $GBC_WORK_DIR/wrfout_d03");

if (! defined(@SEASONS)) {  # best if defined in postprocinput.pl
   @SEASONS=( 'winter', 'winter', 'winter', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'summer', 'winter', 'winter');
}

if ( $IS_WRF || $IS_RIP4 ) {
   if ( $IS_WRF ) {
      $RIPDP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_wrf.exe";
      $RIPDP_OBS = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_wrf.exe";
   } 
   else {
      print OUTFILE "Doesn't work on non WRF models\n";
   }

   $RIP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip.exe";
   $ENV{'STATIONLIST'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4/stationlist";
   $ENV{'RIP_ROOT'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";
   $RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP4";
} else {
   $RIPDP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_new.exe";
   $RIPDP_OBS = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/ripdp_obs.exe";
   $RIP_EXE = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/rip_new.exe";
   $ENV{'STATIONLIST'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP/stationlist";
   $ENV{'RIP_ROOT'} = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
   $RIP_ROOT = "$MM5HOME/cycle_code/CONSTANT_FILES/RIP";
}
$ENV{'HIRESMAP'} = "$RIP_ROOT/${RANGE}_map.ascii";
$ENV{'RANGEMAP'} = "$RIP_ROOT/${RANGE}_map.ascii";
$ENV{'stationg3'} = "$RIP_ROOT/stationg3";
$ENV{'stationg4'} = "$RIP_ROOT/stationg4";
#

$PLOTS_DIR_SAVE = "$PLOTS_DIR";
$PLOTS_DIR = "$GBC_WORK_DIR/plots";
if( -e $PLOTS_DIR )
{
  system( "rm -rf $PLOTS_DIR");
}
system("mkdir -p $PLOTS_DIR");
system("mkdir -p $PLOTS_DIR/ripscr");
system("mkdir -p $PLOTS_DIR/riprun");
chdir $PLOTS_DIR;

system("scrub 1/6 $PLOTS_DIR/ripscr");
system("scrub -d 1/6 $PLOTS_DIR/gifs_ugui");
system("scrub -d 1/6 $PLOTS_DIR/gifs_small");
system("rm -f $PLOTS_DIR/riprun/*");

print "WRF $IS_WRF \n";
if ( $IS_WRF ) {
   @parts = split("d0",$hourly_file);
   $filen = @parts[0];
   $wrf_date_string = substr(@parts[1],2);
   print OUTFILE "$hourly_file $filen $wrf_date_string \n";
} else {
   print OUTFILE "Doesn't work on non WRF models\n";
}

$range = $RANGE;
if ( $IS_WRF ) {
   #example valid_t should be 2010-01-26_03:00:00 
   $year  = substr( $valid_t, 0, 4);
   $month = substr( $valid_t, 5, 2);
   $day   = substr( $valid_t, 8, 2);
   $hour  = substr( $valid_t, 11, 2); 
   $minute = "00"; 
   $valid_t_compact = $year.$month.$day.$hour.$minute;
} else {
   print OUTFILE "Doesn't work on non WRF models\n";
   print "Doesn't work on non WRF models\n";
}
$season = @SEASONS[$month-1];

   foreach $d ( 3..3) {

     if ( $IS_WRF ) {
        $fn = $filen . "d0" . $d;
        $fn_m1 = $filen . "d0" . $d . "-1";
        print OUTFILE "$fn $fn_m1\n";
     } else {
        print OUTFILE "Doesn't work on non WRF models\n";
        print "Doesn't work on non WRF models\n";
     }

     if ( -e $fn ) {
       print "fn  $fn\n";
       chdir $PLOTS_DIR;
       chdir "ripscr";

       if ( $IS_WRF ) {
         $command0 = "$RIPDP_EXE Domain_$d all $fn_m1 > $PLOTS_DIR/ripdp$d1.log 2>&1\n";
         $command = "$RIPDP_EXE Domain_$d all $fn > $PLOTS_DIR/ripdp$d.log 2>&1\n";
#     die "in do_plots $command\n";
       } else {
         print OUTFILE "Doesn't work on non WRF models\n";
         print "Doesn't work on non WRF models\n";
       }
       print OUTFILE "$command0\n";
       system("$command0");
       print OUTFILE "$command\n";
       system("$command");

       open(XTIMES, "Domain_$d.xtimes");
       $ntimes = <XTIMES>; 
       while ( $xtin = <XTIMES>) {
          $xt = $xtin;
          print OUTFILE "endless loop? $xt $xtin\n";
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
         open(OUTFILE2, ">$newfile") || do {
            warn "Cannot open output file: $newfile\n$!\n";
         };
         print OUTFILE2 "2\n";
         print OUTFILE2 "$xtout";
         print OUTFILE2 "$xt";
         close(OUTFILE2);
         }
       }

       chdir $PLOTS_DIR;
       chdir "riprun";

       if ( $d == 3 ) {
         $xt_d1 = $xt
       }
       print OUTFILE "XT: $xt $xt_d1 \n";
       if ( $xt != $xt_d1 ) {
#      If this domain doesn't run as long as D1...
          system("rm -f domain$d.cgm");
          print OUTFILE "$xt =  $xt_d1 \n";
       } else {
          if ( -e "$GSJOBDIR/postprocs/Mdomain$d.$season.GBC" ) {
            print OUTFILE "getting $GSJOBDIR/postprocs/Mdomain$d.$season.GBC\n";
            system("cat $GSJOBDIR/postprocs/Mdomain$d.$season.GBC | sed s/plttime/$xt/g > domain$d.1");
          } else {
            print OUTFILE "getting $RIP_ROOT/namelists/Mdomain$d.$season.GBC\n";
            system("cat $RIP_ROOT/namelists/Mdomain$d.$season.GBC | sed s/plttime/$xt/g > domain$d.1");
          }
          system("cat domain$d.1 | sed s/rangename/$range/g > domain$d.2");
          system("cat domain$d.2 | sed s/cycle/$this_cycle/g > domain$d.3");
          $fcoffset = int($time_max / 60.);
           system("cat domain$d.3 | sed s/OFFSET/$fcoffset/g > domain$d.4");
           system("cat domain$d.4 | sed s/IMGFMT/$RIP_IMG/g > domain$d.in");
           #system("cp domain$d.in $RUNDIR/merged_gbc/");

          system("$RIP_EXE -f ../ripscr/Domain_$d domain${d}.in  > $PLOTS_DIR/rip$d.log 2>&1");
          print OUTFILE "$RIP_EXE -f ../ripscr/Domain_$d domain${d}.in  > $PLOTS_DIR/rip$d.log 2>&1 \n";

       }
       $field_file = "$GSJOBDIR/postprocs/image_fields_gbc.pl";

       &conv_img( "domain$d.$RIP_IMG", $d, $valid_t_compact, $RIP_IMG, $WEB_IMG, $field_file );
       print OUTFILE "ran conv_img domain$d.$RIP_IMG, $d, $valid_t_compact $RIP_IMG $WEB_IMG $field_file\n";

       chdir $PLOTS_DIR;
     #####################################################################################
     # Do actual Bias plots                                                              #
     #####################################################################################
       ($diag_year, $diag_month, $diag_day, $diag_cycle) = (localtime($test_date_48))[5,4,3,2];
       $diag_date = sprintf("%4d%02d%02d%02d" , ($diag_year+1900), ($diag_month+1), $diag_day, $diag_cycle );
       $diag_file = "diagnostic_d03_".$diag_date.$bias_step_signed.".nc";
       print OUTFILE "\nStarting bias plots \n";
       print OUTFILE "ncl \'file_uncorr=\"$RUNDIR/postprocs/$WRF_FILE\"\' \'file_corr=\"$RUNDIR/postprocs/corrected/$WRF_FILE\"\' \'file_bias=\"$bias_file\"\' \'loc=\"$RUNDIR/$diag_date/diagnostics/$diag_file\"\' \'cycle=\"$this_cycle\"\' $bias_plot_T2 \n";
       $result = system("ncl \'file_uncorr=\"$RUNDIR/postprocs/$WRF_FILE\"\' \'file_corr=\"$RUNDIR/postprocs/corrected/$WRF_FILE\"\' \'file_bias=\"$bias_file\"\' \'loc=\"$RUNDIR/$diag_date/diagnostics/$diag_file\"\' \'cycle=\"$this_cycle\"\' $bias_plot_T2 ");
       print OUTFILE "ncl \'file_uncorr=\"$RUNDIR/postprocs/$WRF_FILE\"\' \'file_corr=\"$RUNDIR/postprocs/corrected/$WRF_FILE\"\' \'file_bias=\"$bias_file\"\' \'loc=\"$RUNDIR/$diag_date/diagnostics/$diag_file\"\' \'cycle=\"$this_cycle\"\' $bias_plot_Wind \n";
       $result = system("ncl \'file_uncorr=\"$RUNDIR/postprocs/$WRF_FILE\"\' \'file_corr=\"$RUNDIR/postprocs/corrected/$WRF_FILE\"\' \'file_bias=\"$bias_file\"\' \'loc=\"$RUNDIR/$diag_date/diagnostics/$diag_file\"\' \'cycle=\"$this_cycle\"\' $bias_plot_Wind ");
       print OUTFILE "ncl \'file_uncorr=\"$RUNDIR/postprocs/$WRF_FILE\"\' \'file_corr=\"$RUNDIR/postprocs/corrected/$WRF_FILE\"\' \'file_bias=\"$bias_file\"\' \'loc=\"$RUNDIR/$diag_date/diagnostics/$diag_file\"\' \'cycle=\"$this_cycle\"\' $bias_plot_U10V10 \n";
       $result = system("ncl \'file_uncorr=\"$RUNDIR/postprocs/$WRF_FILE\"\' \'file_corr=\"$RUNDIR/postprocs/corrected/$WRF_FILE\"\' \'file_bias=\"$bias_file\"\' \'loc=\"$RUNDIR/$diag_date/diagnostics/$diag_file\"\' \'cycle=\"$this_cycle\"\' $bias_plot_U10V10 ");
       #mv the files to the correct ugui directory
       system ("mv bias_T2_Q2-0.png $PLOTS_DIR/gifs_ugui/$valid_t_compact/d3_T2_bias.png");
       system ("mv bias_T2_Q2-1.png $PLOTS_DIR/gifs_ugui/$valid_t_compact/d3_Q2_bias.png");
       system ("mv bias_U10V10.png $PLOTS_DIR/gifs_ugui/$valid_t_compact/d3_U10V10_bias.png");
       system ("mv bias_U10_V10-0.png $PLOTS_DIR/gifs_ugui/$valid_t_compact/d3_U10_bias.png");
       system ("mv bias_U10_V10-1.png $PLOTS_DIR/gifs_ugui/$valid_t_compact/d3_V10_bias.png");
     #####################################################################################

       chdir "gifs_ugui";

       system("chmod g+w $valid_t_compact ");
       system("chmod g+w $valid_t_compact/*.gif ");
       print "\nrsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_t_compact $dest\n";
       print OUTFILE "\nrsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_t_compact $dest\n";
       system("rsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_t_compact $dest");

       chdir $PLOTS_DIR;
       if ( $DO_CYCLES_IMAGES ) 
       {

         #Add corrected gifs to the gifs area
         system( "mkdir -p $this_cycle" );
         system( "mkdir -p $this_cycle/gifs" );
         system("chmod g+w $this_cycle ");
         system("chmod g+w $this_cycle/gifs ");
         $dest_dir = "$DEST_SERVER:$JOB_LOC/cycles";
         system("rsync -e 'ssh -i $KEY' -avzC $this_cycle $dest_dir");

         $dest_dir = "$JOB_LOC/cycles/$this_cycle/gifs";
         $dest_cycles = "$DEST_SERVER:$dest_dir";

         print "\nrsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_t_compact $dest_cycles\n";
         print OUTFILE "\nrsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_t_compact $dest_cycles\n";
         system("rsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_t_compact $dest_cycles");
       }
       if ( $DO_DISTRIB ) 
       {
# Add corrected Netcdf files
         $work_dir = "$DISTRIB_DIR/corrected/$this_cycle";
         system( "mkdir -p $work_dir" );
         chdir "$work_dir";
         print "Copying $WRF_FILE to $work_dir\n\n";
         system( "cp $RUNDIR/postprocs/corrected/$WRF_FILE $work_dir");
         if ( $DO_TAR_SUM_FOR_DISTRIB ) {
          print "sha256sum $WRF_FILE > ${WRF_FILE}.sum\n";
          system( "sha256sum $WRF_FILE > ${WRF_FILE}.sum");
# Copy gbc/bias images to where other images got made in preparation for tar/sum
         print "\nrsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_t_compact/* $PLOTS_DIR_SAVE/gifs_ugui/$valid_t_compact\n";
         print OUTFILE "\nrsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_t_compact/* $PLOTS_DIR_SAVE/gifs_ugui/$valid_t_compact\n";
         system("rsync -e 'ssh -i $KEY' -avzC $PLOTS_DIR/gifs_ugui/$valid_t_compact/* $PLOTS_DIR_SAVE/gifs_ugui/$valid_t_compact");
          }
       }

       if ( $DO_PLOTS_SMALL ) {
         $mkdir_command = "mkdir $JOB_LOC/gifs_small/$valid_t";
         system( "ssh 4dwx\@atec-server $mkdir_command" );
         system("scp -pr gifs_small/$valid_t/d$d\* $dest_small/$valid_t" );
         $scandir_command = "/www/cgi-bin/model/gmod/pda_gif_lister range=\"$USER/$JOB_ID\"";
         system( "ssh 4dwx\@atec-server $scandir_command" );
       }
     } else {
       print " WRF_FILE $WRF_FILE \n";
       print " error finding fn  $fn \n";

     }
   }

# restore $PLOTS_DIR to what it was, for tar/sum routine to find it
$PLOTS_DIR = "$PLOTS_DIR_SAVE";
print OUTFILE "\nrestoring PLOTS_DIR to $PLOTS_DIR\n";

#remove temporary softlink - fix in next release
#system("rm $RUNDIR/$cycle/$WRF_FILE.DPG_P+FCST");
print OUTFILE "$merge_results\n";
print OUTFILE "WORK DIR $WORK_DIR\n";
close(OUTFILE);

print "\nexiting do_gbc $hourly_file, $ndom, $cycle, $valid_t, $dest, $dest_small\n\n";
}
1;

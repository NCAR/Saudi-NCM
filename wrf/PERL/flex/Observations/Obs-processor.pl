#!/usr/bin/perl
use Time::gmtime;

$RDA_PROCESSOR = $PERL_FLEX.'/rda/rda_processor.pm';

#
#---------------------------------------------------------------------
#       Collect and decode OBS upto most recent time
#---------------------------------------------------------------------
#
sub get_and_decode_OBS
{
  (my $stage, my $s_date, my $e_date, my $this_cycle) = @_ ;
  (my $yy) = substr($this_cycle,0,4);
  $y_beg = timegm(0,0,0,1,0,$yy);
  $jday = int(($cycle_time - $y_beg)/24/60/60) + 1;
  my $obs_date = &hh_advan_date($this_cycle,-24);

  if ($GTS)
  {
    # This one just makes links for gts data and might need to go
    # to the gts part
    system("$CSH_ARCHIVE/Forecast/RT_get_obs_WMO_csh $e_date");
  }

  if ( $AFCCC)
  {
    print FILEL "      starting $stage get_afccc.csh at ", gmctime();
    system("$CSH_ARCHIVE/Forecast/RT_get_obs_AFCCC_decoded.csh $this_cycle $DATADIR");
    print FILEL "      ending   $stage  get_afccc.csh at ", gmctime();
  }

  if ( $ADP )
  {
   # Because ADP observations are historical, they can be decoded once at
   # the C-stage, but the time period should cover the forecast (+1hr)
   # as well. This is automatically done when this routine get_and_decode_OBS
   # is called with argument s_date=f_fdda_start, e_date=p_fdda_end and
   # one hour is added to p_fdda_end.

     if ( $stage eq "C-stage")
     {

      #For the forecast with ADP, we need to decode until one hour after e_date
      $e_date1 = &hh_advan_date($e_date,1);

      # ADP fetching has been moved here from pre_process_F.pl
      print FILEL "      starting $stage RT_get_obs_ADP_SFC.csh at ",gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_get_obs_ADP_SFC.csh $this_cycle $s_date $e_date1 $DATADIR");
      print FILEL "      starting $stage RT_get_obs_UPA_SFC.csh at ",gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_get_obs_ADP_UPA.csh $this_cycle $s_date $e_date1 $DATADIR");

      #  Decode ADP data
      print FILEL "      starting $stage decode_ADP_rtfdda.csh at ", gmctime();
      $systat = system("$CSH_ARCHIVE/Forecast/RT_S_decode_ADP_rtfdda.csh $this_cycle $s_date $e_date1");

      # This can be commented, unless we want to know when ADP obs are missing.
      if ($systat != 0) {
        print ("Unable to decode ADP Obs data for cycle $this_cycle, stop here!\n");
        exit (1)
      }
      print FILEL "      ending   $stage decode_ADP_rtfdda.csh at ", gmctime();
    }
  }

  if ($RANGE_PROFILER)
  {
    print FILEL "      starting $stage decode_prof_new_rtfdda.csh at ", gmctime();
    $obs_date_prev = &hh_advan_date($this_cycle,-24);
    $e_date1 = &hh_advan_date($e_date,1);
    system("$CSH_ARCHIVE/Forecast/RT_S_decode_prof_new_rtfdda.csh $stage $this_cycle $obs_date_prev $s_date $e_date1");
    print FILEL "      ending   $stage  RT_S_decode_prof_new_rtfdda.csh at ", gmctime();
  }

  if ($SAMS)
  {
    print FILEL "      starting $stage decode_sams_rtfdda.csh at ", gmctime();
    $obs_date = &hh_advan_date($this_cycle,-24);
    system("$CSH_ARCHIVE/Forecast/RT_S_decode_sams_rtfdda.csh $this_cycle $obs_date");
    $obs_date = &hh_advan_date($this_cycle,0);
    system("$CSH_ARCHIVE/Forecast/RT_S_decode_sams_rtfdda.csh $this_cycle $obs_date");
    print FILEL "      ending   $stage  decode_sams_rtfdda.csh at ", gmctime();
  }

  if ($DTE)
  {
    print FILEL "      starting $stage decode_dte_rtfdda.csh at ", gmctime();
    $obs_date = &hh_advan_date($this_cycle,0);
    system("$CSH_ARCHIVE/Forecast/RT_S_decode_dte_rtfdda.csh $this_cycle $obs_date");
    print FILEL "      starting $stage decode_dte_rtfdda.csh at ", gmctime();
  }

  $obs_date = $s_date;
  my $adv_hh = 1;

  #Decode hourly GTS, RAWS, SATWINS, CLASS SND, ACARS, MADIS, NPN_PROF,.....
  while ($obs_date <= $e_date )
  {
    if ($GTS)
    {
      print FILEL "      starting $stage decode_gts_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_gts_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  decode_gts_rtfdda.csh at ", gmctime(); ;
    }

    if ($RAWS)
    {
      print FILEL "      starting $stage  rd_raws_fdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_raws_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  rd_raws_fdda.csh at ", gmctime();
    }

    if ($OKMESO)
    {
      print FILEL "      starting $stage  RT_S_rd_okmeso_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_okmeso_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  RT_S_rd_okmeso_rtfdda.csh at ", gmctime();
    }

    if ($WVR)
    {
      print FILEL "      starting $stage  RT_S_rd_wvr_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_wvr_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  RT_S_rd_wvr_rtfdda.csh at ", gmctime();
    }

    if ($SAT)
    {
      print FILEL "      starting $stage  decode_sat_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_sat_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  decode_sat_rtfdda.csh at ", gmctime();
    }

    if ($CLASS)
    {
      print FILEL "      starting $stage  decode_class_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_class_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  decode_class_rtfdda.csh at ", gmctime();
    }

    if ($ACARS)
    {
      print FILEL "      starting $stage  decode_acars_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_acars_rtfdda.csh $this_cycle $jday $obs_date");
      print FILEL "      ending   $stage  decode_acars_rtfdda.csh at ", gmctime();
    }

    if ($SATWINDS)
    {
      print FILEL "      starting $stage  decode_satwinds_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_satwinds_rtfdda.csh $this_cycle $obs_date $jday");
      print FILEL "      ending   $stage  decode_satwinds_rtfdda.csh at ", gmctime();
    }

    if ($NIDSVAD)
    {
      print FILEL "      starting $stage  decode_nidsvad_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_nidsvad_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  decode_nidsvad_rtfdda.csh at ", gmctime();
    }

    $jdayo = $jday;
    $jdayo=$jday + 1 if(substr($obs_date,6,2) gt substr($this_cycle,6,2));
    $jdayo=$jday - 1 if(substr($obs_date,6,2) lt substr($this_cycle,6,2));

    if ($BLP_PROF)
    {
      print FILEL "      starting $stage decode_profblp_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_profblp_rtfdda.csh $this_cycle $jdayo $obs_date");
      print FILEL "      ending   $stage  decode_profblp_rtfdda.csh at ", gmctime();
    }

    if ($NPN_PROF)
    {
      print FILEL "      starting $stage decode_profnpn_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_profnpn_rtfdda.csh $this_cycle $jdayo $obs_date");
      print FILEL "      ending   $stage  decode_profnpn_rtfdda.csh at ", gmctime();
    }

    if ($DARPA_SODAR)
    {
      print FILEL "      starting $stage DARPA sodar at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_sodar_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage DARPA sodar at ", gmctime();
    }

    if ($DARPA_PWIDS)
    {
      print FILEL "      starting $stage DARPA pwids at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_pwids_rtfdda.csh $this_cycle $obs_date 5");
      print FILEL "      ending   $stage DARPA pwids at ", gmctime();
    }

    if ($DARPA_LIDARVAD)
    {
      print FILEL "      starting $stage DARPA lidarvad at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_lidar_rtfdda.csh $this_cycle $obs_date 30");
      print FILEL "      ending   $stage DARPA lidarvad at ", gmctime();
    }

    if ($DARPA_DCNET)
    {
      print FILEL "      starting $stage DARPA dcnet at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_dcnet_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage DARPA dcnet at ", gmctime();
    }

    if ($MADIS)
    {
      print FILEL "      starting $stage decode_madis.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_madis.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  decode_madis.csh at ", gmctime();
    }

    if ($IAFSFC)
    {
      print FILEL "      starting $stage RT_S_rd_iaf_sfc.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_iaf_sfc.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  RT_S_rd_iaf_sfc.csh at ", gmctime();
    }

    if ($IAFUPR)
    {
      print FILEL "      starting $stage RT_S_rd_iaf_upr.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_iaf_upr.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  RT_S_rd_iaf_upr.csh at ", gmctime();
    }

    if ($WMO)
    {
       print FILEL "      starting $stage decode_wmo.csh at ", gmctime();
       system("$CSH_ARCHIVE/Forecast/RT_S_rd_wmo.csh $this_cycle $obs_date");
       print FILEL "      ending   $stage  decode_wmo.csh at ", gmctime();
    }

    if ($QWND)
    {
      print FILEL "      starting $stage decode_qwnd.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_qwnd.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  decode_qwnd.csh at ", gmctime();
    }

    if ($UAE_MICROSTEP)
    {
      print FILEL "      starting $stage uae-microstep and uae-gts at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_microstep_rtfdda.csh $this_cycle $obs_date");
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_gts_uae_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage uae-microstep and uae-gts at ", gmctime();
    }

    if ($SPECIAL)
    {
      print FILEL "      starting $stage decode_special.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_special.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  decode_special.csh at ", gmctime();
    }

    if ($QCOUT)
    {
      print FILEL "      starting $stage decode_qcout.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_qcout.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  decode_qcout.csh at ", gmctime();
    }

    if ($TAMDAR)
    {
      print FILEL "      starting $stage decode_tamdar.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_rd_tamdar.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage  decode_tamdar.csh at ", gmctime();
    }

    if ($IAF_WORLD)
    {
      print FILEL "      starting $stage RT_S_decode_iafworld_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_iafworld_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage RT_S_decode_iafworld_rtfdda.csh at ", gmctime(); ;
    }

    if ($IAF_BUFR)
    {
      print FILEL "      starting $stage RT_S_decode_iafbufr_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_iafbufr_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage RT_S_decode_iafbufr_rtfdda.csh at ", gmctime(); ;
    }

    if ($IAF)
    {
      print FILEL "      starting $stage RT_S_decode_iaf_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_iaf_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage RT_S_decode_iaf_rtfdda.csh at ", gmctime(); ;
    }

    if ($SPDB)
    {
      print FILEL "      starting $stage RT_S_decode_spdb_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_spdb_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage RT_S_decode_spdb_rtfdda.csh at ", gmctime(); ;
    }

    if ($AMV)
    {
      print FILEL "      starting $stage RT_S_decode_amv_rtfdda.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_amv_rtfdda.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage RT_S_decode_amv_rtfdda.csh at ", gmctime(); ;
    }

    if ($SAR_WIND)
    {
      print FILEL "      starting $stage decode_sar_wind.csh at ", gmctime();
      system("$CSH_ARCHIVE/Forecast/RT_S_decode_sar_wind.csh $this_cycle $obs_date");
      print FILEL "      ending   $stage decode_sar_wind.csh at ", gmctime();
    }

    $obs_date = &hh_advan_date($obs_date,$adv_hh);
   }

   if ($stage eq "C-stage") {
     if (-e $RDA_PROCESSOR) {
       require $RDA_PROCESSOR;

       # For 14Z: s_date=09Z, e_date=14Z, p_fdda_start=10Z, p_fdda_start=13Z
       #  because of an hour offset
       # Use f_fdda_start, p_fdda_start as start_time and end_time
       &get_and_decode_Radar_data($f_fdda_start, $p_fdda_start, $this_cycle, $last_cycle);
       #&get_and_decode_Radar_data($s_date, $e_date, $this_cycle, $last_cycle);
     }
     else {
       print " INFO **** RDA script [$RDA_PROCESSOR] does NOT exist!!!\n";
     }
   }

}
1;

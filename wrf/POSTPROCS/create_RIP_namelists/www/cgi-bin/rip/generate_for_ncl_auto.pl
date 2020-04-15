#!/usr/bin/perl -wT --

use strict;
use CGI; # Lincold Stein's CGI.pm perl module - http://stein.cshl.org/WWW/CGI/
require "./ReadWriteDB.pm";

my $web_input = new CGI; # gets form input fields into one CGI object

# open web page for writing to screen
print $web_input->header();

# Universal Parameters
my $Range = $web_input->param('Range');
my $new_range = $web_input->param('new_range');
if ($new_range ne '') {
  $Range = $new_range;
}
my $wind_barb = $web_input->param('wind_barb');
my $N_doms = $web_input->param('N_doms');
my $underground = $web_input->param('underground');
my $color_bar = $web_input->param('color_bar');
my $wind_brb_freq = $web_input->param('wind_brb_freq');
my $maj_tic_freq = $web_input->param('maj_tic_freq');
my $mnr_tic_freq = $web_input->param('mnr_tic_freq');
my $dup_sounding = $web_input->param('dup_sounding');

print $web_input->start_html(-title=>"$Range input for NCL");

print <<DONE;
***Universal Parameters for $Range***
<br>Wind (kts/mps); No. Doms; Hide fields under terrain; Color bar location;Wind Barb Freq.;Dupl. Snding;MajTick;MnrTick
<br>$wind_barb;$N_doms;$underground;$color_bar;$wind_brb_freq;$dup_sounding;$maj_tic_freq;$mnr_tic_freq
<br>***Plot Specific Parameters***
<br>Plot Name;Sha Int;Ctr Int ;Dsh Int ;Sha Max Smr; Sha Min Smr; Sha Max Wtr; Sha Min Wtr; Clr Tbl; Hghlt Line
DONE

# T2mWnd - MSLP/Temp(C)/Wind
my $T2mWnd_name = $web_input->param('T2mWnd_name');
my $T2mWnd_ctr_int = $web_input->param('T2mWnd_ctr_int');
my $T2mWnd_sha_int = $web_input->param('T2mWnd_sha_int');
my $T2mWnd_rng_smr = $web_input->param('T2mWnd_rng_smr');
my $T2mWnd_rng_wtr = $web_input->param('T2mWnd_rng_wtr');
my $T2mWnd_color   = $web_input->param('T2mWnd_color');
  if ( $T2mWnd_name ne "") {
print <<DONE;
<br>$T2mWnd_name;$T2mWnd_sha_int;$T2mWnd_ctr_int;100,100,100,100;$T2mWnd_rng_smr;$T2mWnd_rng_wtr;$T2mWnd_color;999
DONE
  }

# TF2mWnd - MSLP/Temp(F)/Wind
my $TF2mWnd_name = $web_input->param('TF2mWnd_name');
my $TF2mWnd_ctr_int = $web_input->param('TF2mWnd_ctr_int');
my $TF2mWnd_sha_int = $web_input->param('TF2mWnd_sha_int');
my $TF2mWnd_rng_smr = $web_input->param('TF2mWnd_rng_smr');
my $TF2mWnd_rng_wtr = $web_input->param('TF2mWnd_rng_wtr');
my $TF2mWnd_color   = $web_input->param('TF2mWnd_color');
  if ( $TF2mWnd_name ne "") {
print <<DONE;
<br>$TF2mWnd_name;$TF2mWnd_sha_int;$TF2mWnd_ctr_int;100,100,100,100;$TF2mWnd_rng_smr;$TF2mWnd_rng_wtr;$TF2mWnd_color;999
DONE
  }

# RHsfcWnd  - RH/Wind
my $RHsfcWnd_name = $web_input->param('RHsfcWnd_name');
my $RHsfcWnd_color   = $web_input->param('RHsfcWnd_color');
  if ( $RHsfcWnd_name ne "") {
print <<DONE;
<br>$RHsfcWnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHsfcWnd_color;999
DONE
  }

# Td2mWnd  - 2m Td(C)/Wind
my $Td2mWnd_sha_int = $web_input->param('Td2mWnd_sha_int');
my $Td2mWnd_name = $web_input->param('Td2mWnd_name');
my $Td2mWnd_rng_smr = $web_input->param('Td2mWnd_rng_smr');
my $Td2mWnd_rng_wtr = $web_input->param('Td2mWnd_rng_wtr');
my $Td2mWnd_color   = $web_input->param('Td2mWnd_color');
  if ( $Td2mWnd_name ne "") {
print <<DONE;
<br>$Td2mWnd_name;$Td2mWnd_sha_int;999,999,999,999;999,999,999,999;$Td2mWnd_rng_smr;$Td2mWnd_rng_wtr;$Td2mWnd_color;999
DONE
  }

# TdF2mWnd  - 2m Td(F)/Wind
my $TdF2mWnd_sha_int = $web_input->param('TdF2mWnd_sha_int');
my $TdF2mWnd_name = $web_input->param('TdF2mWnd_name');
my $TdF2mWnd_rng_smr = $web_input->param('TdF2mWnd_rng_smr');
my $TdF2mWnd_rng_wtr = $web_input->param('TdF2mWnd_rng_wtr');
my $TdF2mWnd_color   = $web_input->param('TdF2mWnd_color');
  if ( $TdF2mWnd_name ne "") {
print <<DONE;
<br>$TdF2mWnd_name;$TdF2mWnd_sha_int;999,999,999,999;999,999,999,999;$TdF2mWnd_rng_smr;$TdF2mWnd_rng_wtr;$TdF2mWnd_color;999
DONE
  }

# QvWnd  - 2m Qv/Wind
my $QvWnd_name = $web_input->param('QvWnd_name');
my $QvWnd_color   = $web_input->param('QvWnd_color');
  if ( $QvWnd_name ne "") {
print <<DONE;
<br>$QvWnd_name;2,2,1,1;999,999,999,999;999,999,999,999;30;2;20;2;$QvWnd_color;999
DONE
  }

# ThEsfcWnd - Theta-E/Wind
my $ThEsfcWnd_name = $web_input->param('ThEsfcWnd_name');
my $ThEsfcWnd_rng_smr = $web_input->param('ThEsfcWnd_rng_smr');
my $ThEsfcWnd_rng_wtr = $web_input->param('ThEsfcWnd_rng_wtr');
my $ThEsfcWnd_color   = $web_input->param('ThEsfcWnd_color');
  if ( $ThEsfcWnd_name ne "") {
print <<DONE;
<br>$ThEsfcWnd_name;5,5,2.5,2.5;999,999,999,999;999,999,999,999;$ThEsfcWnd_rng_smr;$ThEsfcWnd_rng_wtr;$ThEsfcWnd_color;999
DONE
  }

# Strm10m - Surface Streamlines
my $Strm10m_name = $web_input->param('Strm10m_name');
my $Strm10m_color   = $web_input->param('Strm10m_color');
  if ( $Strm10m_name ne "") {
print <<DONE;
<br>$Strm10m_name;200,200,200,200;999,999,999,999;999,999,999,999;4000;0;4000;0;$Strm10m_color;999
DONE
  }

# sfcstrm - Surface Wind Speed/Streamlines
my $sfcstrm_name = $web_input->param('sfcstrm_name');
my $sfcstrm_color= $web_input->param('sfcstrm_color');
  if ( ($sfcstrm_name ne "") && ($wind_barb =="5mps") ) {
print <<DONE;
<br>$sfcstrm_name;5,5,5,5;999,999,999,999;999,999,999,999;35;5;35;5;$sfcstrm_color;999
DONE
  }
  if ( ($sfcstrm_name ne "") && ($wind_barb =="10kts") ) {
print <<DONE;
<br>$sfcstrm_name;10,10,10,10;999,999,999,999;999,999,999,999;70;10;70;10;$sfcstrm_color;999
DONE
  }

# WSPD10MAX - Surface Max Wind Gust
my $WSPD10MAX_name = $web_input->param('WSPD10MAX_name');
my $WSPD10MAX_color= $web_input->param('WSPD10MAX_color');
  if ($WSPD10MAX_name ne "") {
print <<DONE;
<br>$WSPD10MAX_name;2,2,2,2;999,999,999,999;999,999,999,999;40;0;40;0;$WSPD10MAX_color;999
DONE
  }

# Pre1hr - 1 Hour Precip
my $Pre1hr_name = $web_input->param('Pre1hr_name');
my $Pre1hr_color= $web_input->param('Pre1hr_color');
  if ( $Pre1hr_name ne "") {
print <<DONE;
<br>$Pre1hr_name;0.5,0.5,0.5,0.5;999,999,999,999;999,999,999,999;102.5;0.1;102.5;0.1;$Pre1hr_color;999
DONE
  }

# PreAcc - Accumulated Precip
my $PreAcc_name = $web_input->param('PreAcc_name');
my $PreAcc_color= $web_input->param('PreAcc_color');
  if ( $PreAcc_name ne "") {
print <<DONE;
<br>$PreAcc_name;0.5,0.5,0.5,0.5;999,999,999,999;999,999,999,999;409.7;0.1;409.7;0.1;$PreAcc_color;999
DONE
  }

# Pre1hrZ - MSLP/Thickness/Wind/Precip (NVL)
my $Pre1hrZ_name = $web_input->param('Pre1hrZ_name');
my $Pre1hrZ_color= $web_input->param('Pre1hrZ_color');
my $Pre1hrZ_ctr_int = $web_input->param('Pre1hrZ_ctr_int');
my $Pre1hrZ_dsh_int = $web_input->param('Pre1hrZ_dsh_int');
my $Pre1hrZ_ctr_hghlt = $web_input->param('Pre1hrZ_ctr_hghlt');
  if ( $Pre1hrZ_name ne "") {
    if ( $Pre1hrZ_dsh_int == "6,3,1,0.5") {
print <<DONE;
<br>Pre1hrZ;0.5,0.5,0.5,0.5;$Pre1hrZ_ctr_int;$Pre1hrZ_dsh_int;102.5;0.1;102.5;0.1;$Pre1hrZ_color;$Pre1hrZ_ctr_hghlt
DONE
    }
    if ( $Pre1hrZ_dsh_int == "3,2,1,0.5") {
print <<DONE;
<br>Pre1hrZ2;0.5,0.5,0.5,0.5;$Pre1hrZ_ctr_int;$Pre1hrZ_dsh_int;102.5;0.1;102.5;0.1;$Pre1hrZ_color;$Pre1hrZ_ctr_hghlt
DONE
    }
  }

# Snow1hr - 1 Hour Snowfall Accumulation
my $Snow1hr_name = $web_input->param('Snow1hr_name');
my $Snow1hr_color= $web_input->param('Snow1hr_color');
  if ( $Snow1hr_name ne "") {
print <<DONE;
<br>$Snow1hr_name;0.5,0.5,0.5,0.5;999,999,999,999;999,999,999,999;0.512;0.001;0.512;0.001;$Snow1hr_color;999
DONE
  }

# SnowAcc - Accumulated Snowfall in forecast
my $SnowAcc_name = $web_input->param('SnowAcc_name');
my $SnowAcc_color= $web_input->param('SnowAcc_color');
  if ( $SnowAcc_name ne "") {
print <<DONE;
<br>$SnowAcc_name;0.5,0.5,0.5,0.5;999,999,999,999;999,999,999,999;2.048;0.001;2.048;0.001;$SnowAcc_color;999
DONE
  }

# radar - Radar Reflectivity
my $radar_name  = $web_input->param('radar_name');
my $radar_color = $web_input->param('radar_color');
  if ( $radar_name ne "") {
print <<DONE;
<br>$radar_name;5,5,5,5;999,999,999,999;999,999,999,999;75;5;75;5;$radar_color;999
DONE
  }

# pcpwtr - Precipitable Water
my $pcpwtr_name  = $web_input->param('pcpwtr_name');
my $pcpwtr_color = $web_input->param('pcpwtr_color');
  if ( $pcpwtr_name ne "") {
print <<DONE;
<br>$pcpwtr_name;4,4,4,4;999,999,999,999;999,999,999,999;76;4;76;4;$pcpwtr_color;999
DONE
  }

# pcpwv - Precipitable Water Vapor
my $pcpwv_name  = $web_input->param('pcpwv_name');
my $pcpwv_color = $web_input->param('pcpwv_color');
  if ( $pcpwv_name ne "") {
print <<DONE;
<br>$pcpwv_name;4,4,4,4;999,999,999,999;999,999,999,999;76;4;76;4;$pcpwv_color;999
DONE
  }

# cape - CAPE
my $cape_name  = $web_input->param('cape_name');
my $cape_color = $web_input->param('cape_color');
  if ( $cape_name ne "") {
print <<DONE;
<br>$cape_name;200,200,200,200;999,999,999,999;999,999,999,999;5000;0;3000;0;$cape_color;999
DONE
  }

# cin - CIN
my $cin_name  = $web_input->param('cin_name');
my $cin_color = $web_input->param('cin_color');
  if ( $cin_name ne "") {
print <<DONE;
<br>$cin_name;50,50,50,50;999,999,999,999;999,999,999,999;0;-600;0;-600;$cin_color;999
DONE
  }

# pblhgt - PBL Height
my $pblhgt_name    = $web_input->param('pblhgt_name');
my $pblhgt_color   = $web_input->param('pblhgt_color');
  if ( $pblhgt_name ne "") {
print <<DONE;
<br>$pblhgt_name;250,250,250,250;999,999,999,999;999,999,999,999;4000;0;4000;0;$pblhgt_color;999
DONE
  }

# frzlvlhgt - Freezing Level Height above ground level
my $frzlvlhgt_name    = $web_input->param('frzlvlhgt_name');
my $frzlvlhgt_color   = $web_input->param('frzlvlhgt_color');
  if ( $frzlvlhgt_name ne "") {
print <<DONE;
<br>$frzlvlhgt_name;200,200,200,200;999,999,999,999;999,999,999,999;5000;0;5000;0;$frzlvlhgt_color;999
DONE
  }

# frzlvlhgtAMSL - Freezing Level Height above mean sea level
my $frzlvlhgtAMSL_name    = $web_input->param('frzlvlhgtAMSL_name');
my $frzlvlhgtAMSL_color   = $web_input->param('frzlvlhgtAMSL_color');
  if ( $frzlvlhgtAMSL_name ne "") {
print <<DONE;
<br>$frzlvlhgtAMSL_name;200,200,200,200;999,999,999,999;999,999,999,999;5000;0;5000;0;$frzlvlhgtAMSL_color;999
DONE
  }

# sfcvis - Surface Visibility
my $sfcvis_name = $web_input->param('sfcvis_name');
my $sfcvis_sha_max = $web_input->param('sfcvis_sha_max');
my $sfcvis_color   = $web_input->param('sfcvis_color');
  if ( $sfcvis_name ne "") {
    if ( $sfcvis_sha_max == "30") {
print <<DONE;
<br>$sfcvis_name;1,1,1,1;999,999,999,999;999,999,999,999;$sfcvis_sha_max;0.25;$sfcvis_sha_max;0.25;VisCBCust2;999
DONE
    }
    if ( $sfcvis_sha_max ne "30") {
print <<DONE;
<br>$sfcvis_name;1,1,1,1;999,999,999,999;999,999,999,999;$sfcvis_sha_max;0.25;$sfcvis_sha_max;0.25;$sfcvis_color;999
DONE
    }
  }

# ms500h - MSLP/Thickness
my $ms500h_name = $web_input->param('ms500h_name');
my $ms500h_ctr_int = $web_input->param('ms500h_ctr_int');
my $ms500h_ctr_hghlt = $web_input->param('ms500h_ctr_hghlt');
  if ( $ms500h_name ne "") {
print <<DONE;
<br>$ms500h_name;200,200,200,200;4,3,2,2;$ms500h_ctr_int;3000;0;3000;0;Terrain;$ms500h_ctr_hghlt
DONE
  }

# Z925MSLP - MSLP/925 mb height (ATC)
my $Z925MSLP_name = $web_input->param('Z925MSLP_name');
  if ( $Z925MSLP_name ne "") {
print <<DONE;
<br>$Z925MSLP_name;200,200,200,200;4,3,2,2;30,30,7.5,7.5;3000;0;3000;0;Terrain;999
DONE
  }

# Get T/Height/Wind Color Scale
my $TXXXWnd_color = $web_input->param('TXXXWnd_color');

# T975Wnd - 975 hPa T/Height/Wind
my $T975Wnd_name = $web_input->param('T975Wnd_name');
  if ( $T975Wnd_name ne "") {
print <<DONE;
<br>$T975Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;48;-16;40;-24;$TXXXWnd_color;999
DONE
  }

# T950Wnd - 950 hPa T/Height/Wind
my $T950Wnd_name = $web_input->param('T950Wnd_name');
  if ( $T950Wnd_name ne "") {
print <<DONE;
<br>$T950Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;48;-16;40;-24;$TXXXWnd_color;999
DONE
  }

# T925Wnd - 925 hPa T/Height/Wind
my $T925Wnd_name = $web_input->param('T925Wnd_name');
  if ( $T925Wnd_name ne "") {
print <<DONE;
<br>$T925Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;40;-24;24;-40;$TXXXWnd_color;999
DONE
  }

# T900Wnd - 900 hPa T/Height/Wind
my $T900Wnd_name = $web_input->param('T900Wnd_name');
  if ( $T900Wnd_name ne "") {
print <<DONE;
<br>$T900Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;40;-24;24;-40;$TXXXWnd_color;999
DONE
  }

# T850Wnd - 850 hPa T/Height/Wind
my $T850Wnd_name = $web_input->param('T850Wnd_name');
my $T850Wnd_max_smr = $web_input->param('T850Wnd_max_smr');
my $T850Wnd_max_wtr = $web_input->param('T850Wnd_max_wtr');
my $T850Wnd_min_smr = $web_input->param('T850Wnd_min_smr');
my $T850Wnd_min_wtr = $web_input->param('T850Wnd_min_wtr');
  if ( $T850Wnd_name ne "") {
print <<DONE;
<br>$T850Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;$T850Wnd_max_smr;$T850Wnd_min_smr;$T850Wnd_max_wtr;$T850Wnd_min_wtr;$TXXXWnd_color;999
DONE
  }

# T800Wnd - 800 hPa T/Height/Wind
my $T800Wnd_name = $web_input->param('T800Wnd_name');
my $T800Wnd_max_smr = $web_input->param('T800Wnd_max_smr');
my $T800Wnd_max_wtr = $web_input->param('T800Wnd_max_wtr');
my $T800Wnd_min_smr = $web_input->param('T800Wnd_min_smr');
my $T800Wnd_min_wtr = $web_input->param('T800Wnd_min_wtr');
  if ( $T800Wnd_name ne "") {
print <<DONE;
<br>$T800Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;$T800Wnd_max_smr;$T800Wnd_min_smr;$T800Wnd_max_wtr;$T800Wnd_min_wtr;$TXXXWnd_color;999
DONE
  }

# T750Wnd - 750 hPa T/Height/Wind
my $T750Wnd_name = $web_input->param('T750Wnd_name');
my $T750Wnd_max_smr = $web_input->param('T750Wnd_max_smr');
my $T750Wnd_max_wtr = $web_input->param('T750Wnd_max_wtr');
my $T750Wnd_min_smr = $web_input->param('T750Wnd_min_smr');
my $T750Wnd_min_wtr = $web_input->param('T750Wnd_min_wtr');
  if ( $T750Wnd_name ne "") {
print <<DONE;
<br>$T750Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;$T750Wnd_max_smr;$T750Wnd_min_smr;$T750Wnd_max_wtr;$T750Wnd_min_wtr;$TXXXWnd_color;999
DONE
  }

# T700Wnd - 700 hPa T/Height/Wind
my $T700Wnd_name = $web_input->param('T700Wnd_name');
my $T700Wnd_max_smr = $web_input->param('T700Wnd_max_smr');
my $T700Wnd_max_wtr = $web_input->param('T700Wnd_max_wtr');
my $T700Wnd_min_smr = $web_input->param('T700Wnd_min_smr');
my $T700Wnd_min_wtr = $web_input->param('T700Wnd_min_wtr');
  if ( $T700Wnd_name ne "") {
print <<DONE;
<br>$T700Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;$T700Wnd_max_smr;$T700Wnd_min_smr;$T700Wnd_max_wtr;$T700Wnd_min_wtr;$TXXXWnd_color;999
DONE
  }

# T600Wnd - 600 hPa T/Height/Wind
my $T600Wnd_name = $web_input->param('T600Wnd_name');
  if ( $T600Wnd_name ne "") {
print <<DONE;
<br>$T600Wnd_name;4,3,2,2;60,60,60,60;999,999,999,999;16;-40;8;-48;$TXXXWnd_color;999
DONE
  }

# T500Wnd - 500 hPa T/Height/Wind
my $T500Wnd_name = $web_input->param('T500Wnd_name');
  if ( $T500Wnd_name ne "") {
print <<DONE;
<br>$T500Wnd_name;4,3,2,2;60,60,60,60;999,999,999,999;8;-48;0;-56;$TXXXWnd_color;999
DONE
  }

# T400Wnd - 400 hPa T/Height/Wind
my $T400Wnd_name = $web_input->param('T400Wnd_name');
  if ( $T400Wnd_name ne "") {
print <<DONE;
<br>$T400Wnd_name;4,3,2,2;120,120,120,120;999,999,999,999;-16;-72;-16;-72;$TXXXWnd_color;999
DONE
  }

# T300Wnd - 300 hPa T/Height/Wind
my $T300Wnd_name = $web_input->param('T300Wnd_name');
  if ( $T300Wnd_name ne "") {
print <<DONE;
<br>$T300Wnd_name;4,3,2,2;120,120,120,120;999,999,999,999;-16;-72;-16;-72;$TXXXWnd_color;999
DONE
  }

# T250Wnd - 250 hPa T/Height/Wind
my $T250Wnd_name = $web_input->param('T250Wnd_name');
  if ( $T250Wnd_name ne "") {
print <<DONE;
<br>$T250Wnd_name;4,3,2,2;120,120,120,120;999,999,999,999;-16;-72;-16;-72;$TXXXWnd_color;999
DONE
  }

# T200Wnd - 200 hPa T/Height/Wind
my $T200Wnd_name = $web_input->param('T200Wnd_name');
  if ( $T200Wnd_name ne "") {
print <<DONE;
<br>$T200Wnd_name;4,3,2,2;120,120,120,120;999,999,999,999;-16;-72;-16;-72;$TXXXWnd_color;999
DONE
  }

# Get RH/Wind Color Scale
my $RHXXXWnd_color = $web_input->param('RHXXXWnd_color');

# RH975Wnd - 975 hPa RH/Wind
my $RH975Wnd_name = $web_input->param('RH975Wnd_name');
  if ( $RH975Wnd_name ne "") {
print <<DONE;
<br>$RH975Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH950Wnd - 950 hPa RH/Wind
my $RH950Wnd_name = $web_input->param('RH950Wnd_name');
  if ( $RH950Wnd_name ne "") {
print <<DONE;
<br>$RH950Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH925Wnd - 925 hPa RH/Wind
my $RH925Wnd_name = $web_input->param('RH925Wnd_name');
  if ( $RH925Wnd_name ne "") {
print <<DONE;
<br>$RH925Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH900Wnd - 900 hPa RH/Wind
my $RH900Wnd_name = $web_input->param('RH900Wnd_name');
  if ( $RH900Wnd_name ne "") {
print <<DONE;
<br>$RH900Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH850Wnd - 850 hPa RH/Wind
my $RH850Wnd_name = $web_input->param('RH850Wnd_name');
  if ( $RH850Wnd_name ne "") {
print <<DONE;
<br>$RH850Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH800Wnd - 800 hPa RH/Wind
my $RH800Wnd_name = $web_input->param('RH800Wnd_name');
  if ( $RH800Wnd_name ne "") {
print <<DONE;
<br>$RH800Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH750Wnd - 750 hPa RH/Wind
my $RH750Wnd_name = $web_input->param('RH750Wnd_name');
  if ( $RH750Wnd_name ne "") {
print <<DONE;
<br>$RH750Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH700Wnd - 700 hPa RH/Wind
my $RH700Wnd_name = $web_input->param('RH700Wnd_name');
  if ( $RH700Wnd_name ne "") {
print <<DONE;
<br>$RH700Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH500Wnd - 500 hPa RH/Wind
my $RH500Wnd_name = $web_input->param('RH500Wnd_name');
  if ( $RH500Wnd_name ne "") {
print <<DONE;
<br>$RH500Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH400Wnd - 400 hPa RH/Wind
my $RH400Wnd_name = $web_input->param('RH400Wnd_name');
  if ( $RH400Wnd_name ne "") {
print <<DONE;
<br>$RH400Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH300Wnd - 300 hPa RH/Wind
my $RH300Wnd_name = $web_input->param('RH300Wnd_name');
  if ( $RH300Wnd_name ne "") {
print <<DONE;
<br>$RH300Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH250Wnd - 250 hPa RH/Wind
my $RH250Wnd_name = $web_input->param('RH250Wnd_name');
  if ( $RH250Wnd_name ne "") {
print <<DONE;
<br>$RH250Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# RH200Wnd - 200 hPa RH/Wind
my $RH200Wnd_name = $web_input->param('RH200Wnd_name');
  if ( $RH200Wnd_name ne "") {
print <<DONE;
<br>$RH200Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color;999
DONE
  }

# VV600m - 600 m Vert. Vel. and Terrain
my $VV600m_name    = $web_input->param('VV600m_name');
my $VV600m_ctr_int = $web_input->param('VV600m_ctr_int');
my $VV600m_color   = $web_input->param('VV600m_color');
  if ( $VV600m_name ne "") {
print <<DONE;
<br>$VV600m_name;200,200,200,200;$VV600m_ctr_int;999,999,999,999;3000;0;3000;0;$VV600m_color;999
DONE
  }

# Omeg700RH - 700 hPa Omega, RH and Height
my $Omeg700RH_name  = $web_input->param('Omeg700RH_name');
my $Omeg700RH_ctr_int  = $web_input->param('Omeg700RH_ctr_int');
my $Omeg700RH_color = $web_input->param('Omeg700RH_color');
  if ( $Omeg700RH_name ne "") {
print <<DONE;
<br>$Omeg700RH_name;10,10,10,10;30,30,30,30;$Omeg700RH_ctr_int;90;10;90;10;$Omeg700RH_color;999
DONE
  }

# Vor500Z - 500 hPa Vorticity and Height
my $Vor500Z_name  = $web_input->param('Vor500Z_name');
my $Vor500Z_color = $web_input->param('Vor500Z_color');
  if ( $Vor500Z_name ne "") {
print <<DONE;
<br>$Vor500Z_name;5,5,5,5;60,60,60,60;999,999,999,999;30;-30;30;-30;$Vor500Z_color;999
DONE
  }





# Get Potential Vorticity Color Scale
my $pvorXXXZ_color = $web_input->param('pvorXXXZ_color');

# pvor700Z - 700 hPa Potential Vorticity and Height
my $pvor700Z_name = $web_input->param('pvor700Z_name');
  if ( $pvor700Z_name ne "" ) {
print <<DONE;
<br>$pvor700Z_name;0.5,0.5,0.5,0.5;60,60,60,60;999,999,999,999;3;-3;3;-3;$pvorXXXZ_color;999
DONE
  }

# pvor500Z - 500 hPa Potential Vorticity and Height
my $pvor500Z_name = $web_input->param('pvor500Z_name');
  if ( $pvor500Z_name ne "" ) {
print <<DONE;
<br>$pvor500Z_name;0.5,0.5,0.5,0.5;60,60,60,60;999,999,999,999;3;-3;3;-3;$pvorXXXZ_color;999
DONE
  }




# Get Jet Stream Color Scale
my $WndXXXZ_color = $web_input->param('WndXXXZ_color');

# Wnd300Z - 300 hPa Wind Speed and Height
my $Wnd300Z_name = $web_input->param('Wnd300Z_name');
  if ( ($Wnd300Z_name ne "") && ($wind_barb =="5mps") ) { 
print <<DONE;
<br>$Wnd300Z_name;10,10,10,10;120,120,120,120;999,999,999,999;90;30;90;30;$WndXXXZ_color;999
DONE
  }
  if ( ($Wnd300Z_name ne "") && ($wind_barb =="10kts") ) {
print <<DONE;
<br>$Wnd300Z_name;20,20,20,20;120,120,120,120;999,999,999,999;190;70;190;70;$WndXXXZ_color;999
DONE
  }

# Wnd250Z - 250 hPa Wind Speed and Height
my $Wnd250Z_name = $web_input->param('Wnd250Z_name');
  if ( ($Wnd250Z_name ne "") && ($wind_barb =="5mps") ) {
print <<DONE;
<br>$Wnd250Z_name;10,10,10,10;120,120,120,120;999,999,999,999;90;30;90;30;$WndXXXZ_color;999
DONE
  }
  if ( ($Wnd250Z_name ne "") && ($wind_barb =="10kts") ) {
print <<DONE;
<br>$Wnd250Z_name;20,20,20,20;120,120,120,120;999,999,999,999;190;70;190;70;$WndXXXZ_color;999
DONE
  }

# Wnd200Z - 200 hPa Wind Speed and Height
my $Wnd200Z_name = $web_input->param('Wnd200Z_name');
  if ( ($Wnd200Z_name ne "") && ($wind_barb =="5mps") ) {
print <<DONE;
<br>$Wnd200Z_name;10,10,10,10;120,120,120,120;999,999,999,999;90;30;90;30;$WndXXXZ_color;999
DONE
  }
  if ( ($Wnd200Z_name ne "") && ($wind_barb =="10kts") ) {
print <<DONE;
<br>$Wnd200Z_name;20,20,20,20;120,120,120,120;999,999,999,999;190;70;190;70;$WndXXXZ_color;999
DONE
  }

# Wnd100Z - 100 hPa Wind Speed and Height
my $Wnd100Z_name = $web_input->param('Wnd100Z_name');
  if ( ($Wnd100Z_name ne "") && ($wind_barb =="5mps") ) {
print <<DONE;
<br>$Wnd100Z_name;10,10,10,10;120,120,120,120;999,999,999,999;90;30;90;30;$WndXXXZ_color;999
DONE
  }
  if ( ($Wnd100Z_name ne "") && ($wind_barb =="10kts") ) {
print <<DONE;
<br>$Wnd100Z_name;20,20,20,20;120,120,120,120;999,999,999,999;190;70;190;70;$WndXXXZ_color;999
DONE
  }

# Div250Z - 300 hPa Wind Speed and Height
my $Div250Z_name  = $web_input->param('Div250Z_name');
my $Div250Z_color = $web_input->param('Div250Z_color');
  if ( $Div250Z_name ne "") {
print <<DONE;
<br>$Div250Z_name;2.5,2.5,2.5,2.5;120,120,120,120;999,999,999,999;15;-15;15;-15;$Div250Z_color;999
DONE
  }

# Get isentropic RH/P/Wind Color Scale
my $isenXXXPRH_color = $web_input->param('isenXXXPRH_color');

# isen280PRH - 280 K RH, P and wind barb
my $isen280PRH_name = $web_input->param('isen280PRH_name');
  if ($isen280PRH_name ne "") {
print <<DONE;
<br>$isen280PRH_name;10,10,10,10;40,20,10,10;999,999,999,999;90;10;90;10;$isenXXXPRH_color;999
DONE
  }

# isen285PRH - 285 K RH, P and wind barb
my $isen285PRH_name = $web_input->param('isen285PRH_name');
  if ($isen285PRH_name ne "") {
print <<DONE;
<br>$isen285PRH_name;10,10,10,10;40,20,10,10;999,999,999,999;90;10;90;10;$isenXXXPRH_color;999
DONE
  }

# isen290PRH - 290 K RH, P and wind barb
my $isen290PRH_name = $web_input->param('isen290PRH_name');
  if ($isen290PRH_name ne "") {
print <<DONE;
<br>$isen290PRH_name;10,10,10,10;40,20,10,10;999,999,999,999;90;10;90;10;$isenXXXPRH_color;999
DONE
  }

# isen295PRH - 295 K RH, P and wind barb
my $isen295PRH_name = $web_input->param('isen295PRH_name');
  if ($isen295PRH_name ne "") {
print <<DONE;
<br>$isen295PRH_name;10,10,10,10;40,20,10,10;999,999,999,999;90;10;90;10;$isenXXXPRH_color;999
DONE
  }

# isen300PRH - 300 K RH, P and wind barb
my $isen300PRH_name = $web_input->param('isen300PRH_name');
  if ($isen300PRH_name ne "") {
print <<DONE;
<br>$isen300PRH_name;10,10,10,10;40,20,10,10;999,999,999,999;90;10;90;10;$isenXXXPRH_color;999
DONE
  }

# isen305PRH - 305 K RH, P and wind barb
my $isen305PRH_name = $web_input->param('isen305PRH_name');
  if ($isen305PRH_name ne "") {
print <<DONE;
<br>$isen305PRH_name;10,10,10,10;40,20,10,10;999,999,999,999;90;10;90;10;$isenXXXPRH_color;999
DONE
  }

# isen310PRH - 310 K RH, P and wind barb
my $isen310PRH_name = $web_input->param('isen310PRH_name');
  if ($isen310PRH_name ne "") {
print <<DONE;
<br>$isen310PRH_name;10,10,10,10;40,20,10,10;999,999,999,999;90;10;90;10;$isenXXXPRH_color;999
DONE
  }

# IRsat - Cloud Top T
my $IRsat_name  = $web_input->param('IRsat_name');
my $IRsat_color = $web_input->param('IRsat_color');
  if ( $IRsat_name ne "") {
print <<DONE;
<br>$IRsat_name;5,5,5,5;999,999,999,999;999,999,999,999;30;-70;30;-70;$IRsat_color;999
DONE
  }

# slhflux - Surface Lat. Heat Flux
my $slhflux_name  = $web_input->param('slhflux_name');
my $slhflux_color = $web_input->param('slhflux_color');
  if ( $slhflux_name ne "") { 
print <<DONE;
<br>$slhflux_name;50,50,50,50;999,999,999,999;999,999,999,999;600;-150;600;-150;$slhflux_color;999
DONE
  }

# sshflux - Surface Sens. Heat Flux
my $sshflux_name  = $web_input->param('sshflux_name');
my $sshflux_color = $web_input->param('sshflux_color');
  if ( $sshflux_name ne "") {
print <<DONE;
<br>$sshflux_name;50,50,50,50;999,999,999,999;999,999,999,999;600;-150;600;-150;$sshflux_color;999
DONE
  }

# SkinT - Skin Temperature
my $SkinT_name  = $web_input->param('SkinT_name');
my $SkinT_color = $web_input->param('SkinT_color');
my $SkinT_rng_smr = $web_input->param('SkinT_rng_smr');
my $SkinT_rng_wtr = $web_input->param('SkinT_rng_wtr');
  if ( $SkinT_name ne "") {
print <<DONE;
<br>$SkinT_name;3,3,2,2;999,999,999,999;999,999,999,999;$SkinT_rng_smr;$SkinT_rng_wtr;$SkinT_color;999
DONE
  }

# soilt1 - Soil Temperature
my $soilt1_name  = $web_input->param('soilt1_name');
my $soilt1_color = $web_input->param('soilt1_color');
  if ( $soilt1_name ne "") {
print <<DONE;
<br>$soilt1_name;3,3,2,2;999,999,999,999;999,999,999,999;308;248;308;248;$soilt1_color;999
DONE
  }

# soilm1 - Soil Moisture
my $soilm1_name  = $web_input->param('soilm1_name');
my $soilm1_color = $web_input->param('soilm1_color');
  if ( $soilm1_name ne "") {
print <<DONE;
<br>$soilm1_name;0.05,0.05,0.05,0.05;999,999,999,999;999,999,999,999;0.45;0.05;0.45;0.05;$soilm1_color;999
DONE
  }

# SWDOWN - Downwelling Shortwave Solar Radiation Flux
my $SWDOWN_name  = $web_input->param('SWDOWN_name');
my $SWDOWN_color = $web_input->param('SWDOWN_color');
  if ( $SWDOWN_name ne "") {
print <<DONE;
<br>$SWDOWN_name;50,50,50,50;999,999,999,999;999,999,999,999;1400;0;1400;0;$SWDOWN_color;999
DONE
  }

# Always print out "Maps" plot parameters
print <<DONE;
<br>landuse;1,1,1,1;999,999,999,999;999,999,999,999;27,1,27,1;LandUse;999
<br>LandUseTbl;1,1,1,1;999,999,999,999;999,999,999,999;999,999,999,999;MMM;999
<br>WRFgrids;200,200,200,200;999,999,999,999;999,999,999,999;4000;0;4000;0;Grayscale;999
DONE

# Get cross section endpoints
my $xs1_d1_name = $web_input->param('xs1_d1_name');
my $xs1_d1_lat_lf = $web_input->param('xs1_d1_lat_lf');
my $xs1_d1_lon_lf = $web_input->param('xs1_d1_lon_lf');
my $xs1_d1_lat_rt = $web_input->param('xs1_d1_lat_rt');
my $xs1_d1_lon_rt = $web_input->param('xs1_d1_lon_rt');
my $xs2_d1_name = $web_input->param('xs2_d1_name');
my $xs2_d1_lat_lf = $web_input->param('xs2_d1_lat_lf');
my $xs2_d1_lon_lf = $web_input->param('xs2_d1_lon_lf');
my $xs2_d1_lat_rt = $web_input->param('xs2_d1_lat_rt');
my $xs2_d1_lon_rt = $web_input->param('xs2_d1_lon_rt');

my $xs1_d2_name = $web_input->param('xs1_d2_name');
my $xs1_d2_lat_lf = $web_input->param('xs1_d2_lat_lf');
my $xs1_d2_lon_lf = $web_input->param('xs1_d2_lon_lf');
my $xs1_d2_lat_rt = $web_input->param('xs1_d2_lat_rt');
my $xs1_d2_lon_rt = $web_input->param('xs1_d2_lon_rt');
my $xs2_d2_name = $web_input->param('xs2_d2_name');
my $xs2_d2_lat_lf = $web_input->param('xs2_d2_lat_lf');
my $xs2_d2_lon_lf = $web_input->param('xs2_d2_lon_lf');
my $xs2_d2_lat_rt = $web_input->param('xs2_d2_lat_rt');
my $xs2_d2_lon_rt = $web_input->param('xs2_d2_lon_rt');

my $xs1_d3_name = $web_input->param('xs1_d3_name');
my $xs1_d3_lat_lf = $web_input->param('xs1_d3_lat_lf');
my $xs1_d3_lon_lf = $web_input->param('xs1_d3_lon_lf');
my $xs1_d3_lat_rt = $web_input->param('xs1_d3_lat_rt');
my $xs1_d3_lon_rt = $web_input->param('xs1_d3_lon_rt');
my $xs2_d3_name = $web_input->param('xs2_d3_name');
my $xs2_d3_lat_lf = $web_input->param('xs2_d3_lat_lf');
my $xs2_d3_lon_lf = $web_input->param('xs2_d3_lon_lf');
my $xs2_d3_lat_rt = $web_input->param('xs2_d3_lat_rt');
my $xs2_d3_lon_rt = $web_input->param('xs2_d3_lon_rt');

my $xs1_d4_name = $web_input->param('xs1_d4_name');
my $xs1_d4_lat_lf = $web_input->param('xs1_d4_lat_lf');
my $xs1_d4_lon_lf = $web_input->param('xs1_d4_lon_lf');
my $xs1_d4_lat_rt = $web_input->param('xs1_d4_lat_rt');
my $xs1_d4_lon_rt = $web_input->param('xs1_d4_lon_rt');
my $xs2_d4_name = $web_input->param('xs2_d4_name');
my $xs2_d4_lat_lf = $web_input->param('xs2_d4_lat_lf');
my $xs2_d4_lon_lf = $web_input->param('xs2_d4_lon_lf');
my $xs2_d4_lat_rt = $web_input->param('xs2_d4_lat_rt');
my $xs2_d4_lon_rt = $web_input->param('xs2_d4_lon_rt');

# xsloc - Cross section location(s)
  if ( $xs1_d1_name ne "") {
print <<DONE;
<br>xsloc;200,200,200,200;999,999,999,999;999,999,999,999;4000;0;4000;0;Grayscale;999
DONE

# xsthw - Cross section of Theta and Winds
my $xsthw_name  = $web_input->param('xsthw_name');
my $xsthw_color = $web_input->param('xsthw_color');
  if ( $xsthw_name ne "") {
print <<DONE;
<br>xsthw;5,5,5,5;999,999,999,999;999,999,999,999;400;250;400;250;$xsthw_color;999
DONE
  }

# xsrhw - Cross section of Theta and Winds
my $xsrhw_name  = $web_input->param('xsrhw_name');
my $xsrhw_color = $web_input->param('xsrhw_color');
  if ( $xsrhw_name ne "") {
print <<DONE;
<br>xsrhw;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$xsrhw_color;999
DONE
  }

# xswWnd - Cross section of Theta and Winds
my $xswWnd_name  = $web_input->param('xswWnd_name');
my $xswWnd_ctr_int  = $web_input->param('xswWnd_ctr_int');
my $xswWnd_color = $web_input->param('xswWnd_color');
  if ( $xswWnd_name ne "") {
print <<DONE;
<br>xswWnd;$xswWnd_ctr_int;4,4,4,4;4,4,4,4;50;-50;50;-50;$xswWnd_color;999
DONE
  }

# xsdBZ - Cross section of Reflectivity
my $xsdBZ_name  = $web_input->param('xsdBZ_name');
my $xsdBZ_color = $web_input->param('xsdBZ_color');
  if ( $xsdBZ_name ne "") {
print <<DONE;
<br>$xsdBZ_name;5,5,5,5;999,999,999,999;999,999,999,999;75;5;75;5;$xsdBZ_color;999
DONE
  }

# xsPcp - Cross section of Precipitation Mixing Ratio
my $xsPcp_name  = $web_input->param('xsPcp_name');
my $xsPcp_color = $web_input->param('xsPcp_color');
  if ( $xsPcp_name ne "") {
print <<DONE;
<br>$xsPcp_name;0.05,0.05,0.05,0.05;5,10,20,40;999,999,999,999;0.5;0.05;0.5;0.05;$xsPcp_color;999
DONE
  }

# xsrhcv - Cross section of RH, Theta and Circulation Vectors
my $xsrhcv_name  = $web_input->param('xsrhcv_name');
my $xsrhcv_color = $web_input->param('xsrhcv_color');
  if ( $xsrhcv_name ne "") {
print <<DONE;
<br>xsrhcv;999,999,10,10;999,999,2,2;999,999,999,999;90;10;90;10;$xsrhcv_color;999
DONE
  }

# xsrhthgm - Cross section of RH, Theta-E and Geostrophic Momentum
my $xsrhthgm_name  = $web_input->param('xsrhthgm_name');
my $xsrhthgm_color = $web_input->param('xsrhthgm_color');
  if ( $xsrhthgm_name ne "") {
print <<DONE;
<br>xsrhthgm;10,10,10,10;2,2,2,2;20,20,20,20;90;10;90;10;$xsrhthgm_color;999
DONE
  }

# cust_plts - Add custom plots
my $cust_plts_name = $web_input->param('cust_plts_name');
  if ( $cust_plts_name ne "") {
print <<DONE;
<br>$cust_plts_name;999,999,999,999;999,999,999,999;999,999,999,999;999;999;999;999;MMM;999
DONE
  }

# Read in Sounding locations
my $snd1_d1_name = $web_input->param('snd1_d1_name');
my $snd2_d1_name = $web_input->param('snd2_d1_name');
my $snd3_d1_name = $web_input->param('snd3_d1_name');
my $snd4_d1_name = $web_input->param('snd4_d1_name');
my $snd5_d1_name = $web_input->param('snd5_d1_name');
my $snd1_d1_loc = $web_input->param('snd1_d1_loc');
my $snd2_d1_loc = $web_input->param('snd2_d1_loc');
my $snd3_d1_loc = $web_input->param('snd3_d1_loc');
my $snd4_d1_loc = $web_input->param('snd4_d1_loc');
my $snd5_d1_loc = $web_input->param('snd5_d1_loc');

my $snd1_d2_name = $web_input->param('snd1_d2_name');
my $snd2_d2_name = $web_input->param('snd2_d2_name');
my $snd3_d2_name = $web_input->param('snd3_d2_name');
my $snd4_d2_name = $web_input->param('snd4_d2_name');
my $snd5_d2_name = $web_input->param('snd5_d2_name');
my $snd1_d2_loc = $web_input->param('snd1_d2_loc');
my $snd2_d2_loc = $web_input->param('snd2_d2_loc');
my $snd3_d2_loc = $web_input->param('snd3_d2_loc');
my $snd4_d2_loc = $web_input->param('snd4_d2_loc');
my $snd5_d2_loc = $web_input->param('snd5_d2_loc');

my $snd1_d3_name = $web_input->param('snd1_d3_name');
my $snd2_d3_name = $web_input->param('snd2_d3_name');
my $snd3_d3_name = $web_input->param('snd3_d3_name');
my $snd4_d3_name = $web_input->param('snd4_d3_name');
my $snd5_d3_name = $web_input->param('snd5_d3_name');
my $snd6_d3_name = $web_input->param('snd6_d3_name');
my $snd7_d3_name = $web_input->param('snd7_d3_name');
my $snd8_d3_name = $web_input->param('snd8_d3_name');
my $snd9_d3_name = $web_input->param('snd9_d3_name');
my $snd10_d3_name = $web_input->param('snd10_d3_name');
my $snd1_d3_loc = $web_input->param('snd1_d3_loc');
my $snd2_d3_loc = $web_input->param('snd2_d3_loc');
my $snd3_d3_loc = $web_input->param('snd3_d3_loc');
my $snd4_d3_loc = $web_input->param('snd4_d3_loc');
my $snd5_d3_loc = $web_input->param('snd5_d3_loc');
my $snd6_d3_loc = $web_input->param('snd6_d3_loc');
my $snd7_d3_loc = $web_input->param('snd7_d3_loc');
my $snd8_d3_loc = $web_input->param('snd8_d3_loc');
my $snd9_d3_loc = $web_input->param('snd9_d3_loc');
my $snd10_d3_loc = $web_input->param('snd10_d3_loc');

my $snd1_d4_name = $web_input->param('snd1_d4_name');
my $snd2_d4_name = $web_input->param('snd2_d4_name');
my $snd3_d4_name = $web_input->param('snd3_d4_name');
my $snd4_d4_name = $web_input->param('snd4_d4_name');
my $snd5_d4_name = $web_input->param('snd5_d4_name');
my $snd6_d4_name = $web_input->param('snd6_d4_name');
my $snd7_d4_name = $web_input->param('snd7_d4_name');
my $snd8_d4_name = $web_input->param('snd8_d4_name');
my $snd9_d4_name = $web_input->param('snd9_d4_name');
my $snd10_d4_name = $web_input->param('snd10_d4_name');
my $snd1_d4_loc = $web_input->param('snd1_d4_loc');
my $snd2_d4_loc = $web_input->param('snd2_d4_loc');
my $snd3_d4_loc = $web_input->param('snd3_d4_loc');
my $snd4_d4_loc = $web_input->param('snd4_d4_loc');
my $snd5_d4_loc = $web_input->param('snd5_d4_loc');
my $snd6_d4_loc = $web_input->param('snd6_d4_loc');
my $snd7_d4_loc = $web_input->param('snd7_d4_loc');
my $snd8_d4_loc = $web_input->param('snd8_d4_loc');
my $snd9_d4_loc = $web_input->param('snd9_d4_loc');
my $snd10_d4_loc = $web_input->param('snd10_d4_loc');

# Output Sounding locations
print <<DONE;
<br>***Sounding Parameters***
<br>Domain; Sounding Locations
DONE

  if ( $snd1_d1_name ne "") {
print <<DONE;
<br>1;$snd1_d1_loc
DONE
  }
  if ( $snd2_d1_name ne "") {
print <<DONE;
:$snd2_d1_loc
DONE
  }
  if ( $snd3_d1_name ne "") { 
print <<DONE;
:$snd3_d1_loc
DONE
  }
  if ( $snd4_d1_name ne "") { 
print <<DONE;
:$snd4_d1_loc
DONE
  }
  if ( $snd5_d1_name ne "") {
print <<DONE;
:$snd5_d1_loc
DONE
  }

  if ( $snd1_d2_name ne "") {
print <<DONE;
<br>2;$snd1_d2_loc
DONE
  }
  if ( $snd2_d2_name ne "") { 
print <<DONE;
:$snd2_d2_loc
DONE
  }
  if ( $snd3_d2_name ne "") {
print <<DONE;
:$snd3_d2_loc
DONE
  }
  if ( $snd4_d2_name ne "") {
print <<DONE;
:$snd4_d2_loc
DONE
  }
  if ( $snd5_d2_name ne "") {
print <<DONE;
:$snd5_d2_loc
DONE
  }

  if ( $snd1_d3_name ne "") {
print <<DONE;
<br>3;$snd1_d3_loc
DONE
  }
  if ( $snd2_d3_name ne "") {
print <<DONE;
:$snd2_d3_loc
DONE
  }
  if ( $snd3_d3_name ne "") {
print <<DONE;
:$snd3_d3_loc
DONE
  }
  if ( $snd4_d3_name ne "") {
print <<DONE;
:$snd4_d3_loc
DONE
  }
  if ( $snd5_d3_name ne "") {
print <<DONE;
:$snd5_d3_loc
DONE
  }
  if ( $snd6_d3_name ne "") {
print <<DONE;
:$snd6_d3_loc
DONE
  }
  if ( $snd7_d3_name ne "") {
print <<DONE;
:$snd7_d3_loc
DONE
  }
  if ( $snd8_d3_name ne "") {
print <<DONE;
:$snd8_d3_loc
DONE
  }
  if ( $snd9_d3_name ne "") {
print <<DONE;
:$snd9_d3_loc
DONE
  }
  if ( $snd10_d3_name ne "") {
print <<DONE;
:$snd10_d3_loc
DONE
  }

  if ( $snd1_d4_name ne "") {
print <<DONE;
<br>4;$snd1_d4_loc
DONE
  }
  if ( $snd2_d4_name ne "") {
print <<DONE;
:$snd2_d4_loc
DONE
  }
  if ( $snd3_d4_name ne "") {
print <<DONE;
:$snd3_d4_loc
DONE
  }
  if ( $snd4_d4_name ne "") {
print <<DONE;
:$snd4_d4_loc
DONE
  }
  if ( $snd5_d4_name ne "") {
print <<DONE;
:$snd5_d4_loc
DONE
  }
  if ( $snd6_d4_name ne "") {
print <<DONE;
:$snd6_d4_loc
DONE
  }
  if ( $snd7_d4_name ne "") {
print <<DONE;
:$snd7_d4_loc
DONE
  }
  if ( $snd8_d4_name ne "") {
print <<DONE;
:$snd8_d4_loc
DONE
  }
  if ( $snd9_d4_name ne "") {
print <<DONE;
:$snd9_d4_loc
DONE
  }
  if ( $snd10_d4_name ne "") {
print <<DONE;
:$snd10_d4_loc
DONE
  }


# Output XS endpoints
print <<DONE;
<br>***Cross Section Parameters***
<br>Domain; Cross Section End Points
DONE

  if ( $xs1_d1_name ne "") {
print <<DONE;
<br>1;$xs1_d1_lat_lf,$xs1_d1_lon_lf>$xs1_d1_lat_rt,$xs1_d1_lon_rt
DONE
  }
  if ( $xs2_d1_name ne "") {
print <<DONE;
:$xs2_d1_lat_lf,$xs2_d1_lon_lf>$xs2_d1_lat_rt,$xs2_d1_lon_rt
DONE
  }

  if ( $xs1_d2_name ne "") {
print <<DONE;
<br>2;$xs1_d2_lat_lf,$xs1_d2_lon_lf>$xs1_d2_lat_rt,$xs1_d2_lon_rt
DONE
  }
  if ( $xs2_d2_name ne "") {
print <<DONE;
:$xs2_d2_lat_lf,$xs2_d2_lon_lf>$xs2_d2_lat_rt,$xs2_d2_lon_rt
DONE
  }

  if ( $xs1_d3_name ne "") {
print <<DONE;
<br>2;$xs1_d3_lat_lf,$xs1_d3_lon_lf>$xs1_d3_lat_rt,$xs1_d3_lon_rt
DONE
  }
  if ( $xs2_d3_name ne "") {
print <<DONE;
:$xs2_d3_lat_lf,$xs2_d3_lon_lf>$xs2_d3_lat_rt,$xs2_d3_lon_rt
DONE
  }

  if ( $xs1_d4_name ne "") {
print <<DONE;
<br>2;$xs1_d4_lat_lf,$xs1_d4_lon_lf>$xs1_d4_lat_rt,$xs1_d4_lon_rt
DONE
  }
  if ( $xs2_d4_name ne "") {
print <<DONE;
:$xs2_d4_lat_lf,$xs2_d4_lon_lf>$xs2_d4_lat_rt,$xs2_d4_lon_rt
DONE
  }
 }

# Get overlay info
my $range_overlay_d1 = $web_input->param('range_overlay_d1');
my $range_overlay_d2 = $web_input->param('range_overlay_d2');
my $range_overlay_d3 = $web_input->param('range_overlay_d3');
my $range_overlay_d4 = $web_input->param('range_overlay_d4');
my $county_overlay_d1 = $web_input->param('county_overlay_d1');
my $county_overlay_d2 = $web_input->param('county_overlay_d2');
my $county_overlay_d3 = $web_input->param('county_overlay_d3');
my $county_overlay_d4 = $web_input->param('county_overlay_d4');
my $poli_bdry_d3 = $web_input->param('poli_bdry_d3');
my $poli_bdry_d4 = $web_input->param('poli_bdry_d4');
my $sta1_mark_name = $web_input->param('sta1_mark_name');
my $sta2_mark_name = $web_input->param('sta2_mark_name');
my $sta3_mark_name = $web_input->param('sta3_mark_name');
my $sta4_mark_name = $web_input->param('sta4_mark_name');
my $sta5_mark_name = $web_input->param('sta5_mark_name');
my $sta6_mark_name = $web_input->param('sta6_mark_name');
my $sta7_mark_name = $web_input->param('sta7_mark_name');
my $sta8_mark_name = $web_input->param('sta8_mark_name');
my $sta9_mark_name = $web_input->param('sta9_mark_name');
my $sta10_mark_name = $web_input->param('sta10_mark_name');
my $sta11_mark_name = $web_input->param('sta11_mark_name');
my $sta12_mark_name = $web_input->param('sta12_mark_name');
my $sta13_mark_name = $web_input->param('sta13_mark_name');
my $sta14_mark_name = $web_input->param('sta14_mark_name');
my $sta15_mark_name = $web_input->param('sta15_mark_name');
my $sta16_mark_name = $web_input->param('sta16_mark_name');
my $sta17_mark_name = $web_input->param('sta17_mark_name');
my $sta18_mark_name = $web_input->param('sta18_mark_name');
my $sta19_mark_name = $web_input->param('sta19_mark_name');
my $sta20_mark_name = $web_input->param('sta20_mark_name');
my $sta1_mark_loc = $web_input->param('sta1_mark_loc');
my $sta2_mark_loc = $web_input->param('sta2_mark_loc');
my $sta3_mark_loc = $web_input->param('sta3_mark_loc');
my $sta4_mark_loc = $web_input->param('sta4_mark_loc');
my $sta5_mark_loc = $web_input->param('sta5_mark_loc');
my $sta6_mark_loc = $web_input->param('sta6_mark_loc');
my $sta7_mark_loc = $web_input->param('sta7_mark_loc');
my $sta8_mark_loc = $web_input->param('sta8_mark_loc');
my $sta9_mark_loc = $web_input->param('sta9_mark_loc');
my $sta10_mark_loc = $web_input->param('sta10_mark_loc');
my $sta11_mark_loc = $web_input->param('sta11_mark_loc');
my $sta12_mark_loc = $web_input->param('sta12_mark_loc');
my $sta13_mark_loc = $web_input->param('sta13_mark_loc');
my $sta14_mark_loc = $web_input->param('sta14_mark_loc');
my $sta15_mark_loc = $web_input->param('sta15_mark_loc');
my $sta16_mark_loc = $web_input->param('sta16_mark_loc');
my $sta17_mark_loc = $web_input->param('sta17_mark_loc');
my $sta18_mark_loc = $web_input->param('sta18_mark_loc');
my $sta19_mark_loc = $web_input->param('sta19_mark_loc');
my $sta20_mark_loc = $web_input->param('sta20_mark_loc');
my $sta1_mark_d1 = $web_input->param('sta1_mark_d1');
my $sta1_mark_d2 = $web_input->param('sta1_mark_d2');
my $sta1_mark_d3 = $web_input->param('sta1_mark_d3');
my $sta1_mark_d4 = $web_input->param('sta1_mark_d4');
my $sta2_mark_d1 = $web_input->param('sta2_mark_d1');
my $sta2_mark_d2 = $web_input->param('sta2_mark_d2');
my $sta2_mark_d3 = $web_input->param('sta2_mark_d3');
my $sta2_mark_d4 = $web_input->param('sta2_mark_d4');
my $sta3_mark_d1 = $web_input->param('sta3_mark_d1');
my $sta3_mark_d2 = $web_input->param('sta3_mark_d2');
my $sta3_mark_d3 = $web_input->param('sta3_mark_d3');
my $sta3_mark_d4 = $web_input->param('sta3_mark_d4');
my $sta4_mark_d1 = $web_input->param('sta4_mark_d1');
my $sta4_mark_d2 = $web_input->param('sta4_mark_d2');
my $sta4_mark_d3 = $web_input->param('sta4_mark_d3');
my $sta4_mark_d4 = $web_input->param('sta4_mark_d4');
my $sta5_mark_d1 = $web_input->param('sta5_mark_d1');
my $sta5_mark_d2 = $web_input->param('sta5_mark_d2');
my $sta5_mark_d3 = $web_input->param('sta5_mark_d3');
my $sta5_mark_d4 = $web_input->param('sta5_mark_d4');
my $sta6_mark_d1 = $web_input->param('sta6_mark_d1');
my $sta6_mark_d2 = $web_input->param('sta6_mark_d2');
my $sta6_mark_d3 = $web_input->param('sta6_mark_d3');
my $sta6_mark_d4 = $web_input->param('sta6_mark_d4');
my $sta7_mark_d1 = $web_input->param('sta7_mark_d1');
my $sta7_mark_d2 = $web_input->param('sta7_mark_d2');
my $sta7_mark_d3 = $web_input->param('sta7_mark_d3');
my $sta7_mark_d4 = $web_input->param('sta7_mark_d4');
my $sta8_mark_d1 = $web_input->param('sta8_mark_d1');
my $sta8_mark_d2 = $web_input->param('sta8_mark_d2');
my $sta8_mark_d3 = $web_input->param('sta8_mark_d3');
my $sta8_mark_d4 = $web_input->param('sta8_mark_d4');
my $sta9_mark_d1 = $web_input->param('sta9_mark_d1');
my $sta9_mark_d2 = $web_input->param('sta9_mark_d2');
my $sta9_mark_d3 = $web_input->param('sta9_mark_d3');
my $sta9_mark_d4 = $web_input->param('sta9_mark_d4');
my $sta10_mark_d1 = $web_input->param('sta10_mark_d1');
my $sta10_mark_d2 = $web_input->param('sta10_mark_d2');
my $sta10_mark_d3 = $web_input->param('sta10_mark_d3');
my $sta10_mark_d4 = $web_input->param('sta10_mark_d4');
my $sta11_mark_d1 = $web_input->param('sta11_mark_d1');
my $sta11_mark_d2 = $web_input->param('sta11_mark_d2');
my $sta11_mark_d3 = $web_input->param('sta11_mark_d3');
my $sta11_mark_d4 = $web_input->param('sta11_mark_d4');
my $sta12_mark_d1 = $web_input->param('sta12_mark_d1');
my $sta12_mark_d2 = $web_input->param('sta12_mark_d2');
my $sta12_mark_d3 = $web_input->param('sta12_mark_d3');
my $sta12_mark_d4 = $web_input->param('sta12_mark_d4');
my $sta13_mark_d1 = $web_input->param('sta13_mark_d1');
my $sta13_mark_d2 = $web_input->param('sta13_mark_d2');
my $sta13_mark_d3 = $web_input->param('sta13_mark_d3');
my $sta13_mark_d4 = $web_input->param('sta13_mark_d4');
my $sta14_mark_d1 = $web_input->param('sta14_mark_d1');
my $sta14_mark_d2 = $web_input->param('sta14_mark_d2');
my $sta14_mark_d3 = $web_input->param('sta14_mark_d3');
my $sta14_mark_d4 = $web_input->param('sta14_mark_d4');
my $sta15_mark_d1 = $web_input->param('sta15_mark_d1');
my $sta15_mark_d2 = $web_input->param('sta15_mark_d2');
my $sta15_mark_d3 = $web_input->param('sta15_mark_d3');
my $sta15_mark_d4 = $web_input->param('sta15_mark_d4');
my $sta16_mark_d1 = $web_input->param('sta16_mark_d1');
my $sta16_mark_d2 = $web_input->param('sta16_mark_d2');
my $sta16_mark_d3 = $web_input->param('sta16_mark_d3');
my $sta16_mark_d4 = $web_input->param('sta16_mark_d4');
my $sta17_mark_d1 = $web_input->param('sta17_mark_d1');
my $sta17_mark_d2 = $web_input->param('sta17_mark_d2');
my $sta17_mark_d3 = $web_input->param('sta17_mark_d3');
my $sta17_mark_d4 = $web_input->param('sta17_mark_d4');
my $sta18_mark_d1 = $web_input->param('sta18_mark_d1');
my $sta18_mark_d2 = $web_input->param('sta18_mark_d2');
my $sta18_mark_d3 = $web_input->param('sta18_mark_d3');
my $sta18_mark_d4 = $web_input->param('sta18_mark_d4');
my $sta19_mark_d1 = $web_input->param('sta19_mark_d1');
my $sta19_mark_d2 = $web_input->param('sta19_mark_d2');
my $sta19_mark_d3 = $web_input->param('sta19_mark_d3');
my $sta19_mark_d4 = $web_input->param('sta19_mark_d4');
my $sta20_mark_d1 = $web_input->param('sta20_mark_d1');
my $sta20_mark_d2 = $web_input->param('sta20_mark_d2');
my $sta20_mark_d3 = $web_input->param('sta20_mark_d3');
my $sta20_mark_d4 = $web_input->param('sta20_mark_d4');

# Print Overlay information
print <<DONE;
<br>***Overlays***
<br>Range Outline Domains:
DONE

  if ( $range_overlay_d1 ne "") {
print <<DONE;
$range_overlay_d1,
DONE
  }
  if ( $range_overlay_d2 ne "") {
print <<DONE;
$range_overlay_d2,
DONE
  }
  if ( $range_overlay_d3 ne "") {
print <<DONE;
$range_overlay_d3,
DONE
  }
  if ( $range_overlay_d4 ne "") {
print <<DONE;
$range_overlay_d4
DONE
  }

print <<DONE;
<br>County Boundary Domains:
DONE

  if ( $county_overlay_d1 ne "") {
print <<DONE;
$county_overlay_d1,
DONE
  }
  if ( $county_overlay_d2 ne "") {
print <<DONE;
$county_overlay_d2,
DONE
  }
  if ( $county_overlay_d3 ne "") {
print <<DONE;
$county_overlay_d3,
DONE
  }
  if ( $county_overlay_d4 ne "") {
print <<DONE;
$county_overlay_d4
DONE
  }

print <<DONE;
<br>Political boundaries:
DONE
  if ( $poli_bdry_d3 ne "") {
print <<DONE;
$poli_bdry_d3,
DONE
  }
  if ( $poli_bdry_d4 ne "") {
print <<DONE;
$poli_bdry_d4
DONE
  }

print <<DONE;
<br>
DONE

  if ( $sta1_mark_name ne "" && $sta1_mark_loc ne "" ) {
print <<DONE;
$sta1_mark_name;$sta1_mark_loc;
DONE
    if ( $sta1_mark_d1 ne "") {
print <<DONE;
$sta1_mark_d1,
DONE
    }
    if ( $sta1_mark_d2 ne "") {
print <<DONE;
$sta1_mark_d2,
DONE
    }
    if ( $sta1_mark_d3 ne "") {
print <<DONE;
$sta1_mark_d3,
DONE
    }
    if ( $sta1_mark_d4 ne "") {
print <<DONE;
$sta1_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta2_mark_name ne "" && $sta2_mark_loc ne "" ) {
print <<DONE;
$sta2_mark_name;$sta2_mark_loc;
DONE
    if ( $sta2_mark_d1 ne "") {
print <<DONE;
$sta2_mark_d1,
DONE
    }
    if ( $sta2_mark_d2 ne "") {
print <<DONE;
$sta2_mark_d2,
DONE
    }
    if ( $sta2_mark_d3 ne "") {
print <<DONE;
$sta2_mark_d3,
DONE
    }
    if ( $sta2_mark_d4 ne "") {
print <<DONE;
$sta2_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta3_mark_name ne "" && $sta3_mark_loc ne "" ) {
print <<DONE;
$sta3_mark_name;$sta3_mark_loc;
DONE
    if ( $sta3_mark_d1 ne "") {
print <<DONE;
$sta3_mark_d1,
DONE
    }
    if ( $sta3_mark_d2 ne "") {
print <<DONE;
$sta3_mark_d2,
DONE
    }
    if ( $sta3_mark_d3 ne "") {
print <<DONE;
$sta3_mark_d3,
DONE
    }
    if ( $sta3_mark_d4 ne "") {
print <<DONE;
$sta3_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta4_mark_name ne "" && $sta4_mark_loc ne "" ) {
print <<DONE;
$sta4_mark_name;$sta4_mark_loc;
DONE
    if ( $sta4_mark_d1 ne "") {
print <<DONE;
$sta4_mark_d1,
DONE
    }
    if ( $sta4_mark_d2 ne "") {
print <<DONE;
$sta4_mark_d2,
DONE
    }
    if ( $sta4_mark_d3 ne "") {
print <<DONE;
$sta4_mark_d3,
DONE
    }
    if ( $sta4_mark_d4 ne "") {
print <<DONE;
$sta4_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta5_mark_name ne "" && $sta5_mark_loc ne "" ) {
print <<DONE;
$sta5_mark_name;$sta5_mark_loc;
DONE
    if ( $sta5_mark_d1 ne "") {
print <<DONE;
$sta5_mark_d1,
DONE
    }
    if ( $sta5_mark_d2 ne "") {
print <<DONE;
$sta5_mark_d2,
DONE
    }
    if ( $sta5_mark_d3 ne "") {
print <<DONE;
$sta5_mark_d3,
DONE
    }
    if ( $sta5_mark_d4 ne "") {
print <<DONE;
$sta5_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta6_mark_name ne "" && $sta6_mark_loc ne "" ) {
print <<DONE;
$sta6_mark_name;$sta6_mark_loc;
DONE
    if ( $sta6_mark_d1 ne "") {
print <<DONE;
$sta6_mark_d1,
DONE
    }
    if ( $sta6_mark_d2 ne "") {
print <<DONE;
$sta6_mark_d2,
DONE
    }
    if ( $sta6_mark_d3 ne "") {
print <<DONE;
$sta6_mark_d3,
DONE
    }
    if ( $sta6_mark_d4 ne "") {
print <<DONE;
$sta6_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta7_mark_name ne "" && $sta7_mark_loc ne "" ) {
print <<DONE;
$sta7_mark_name;$sta7_mark_loc;
DONE
    if ( $sta7_mark_d1 ne "") {
print <<DONE;
$sta7_mark_d1,
DONE
    }
    if ( $sta7_mark_d2 ne "") {
print <<DONE;
$sta7_mark_d2,
DONE
    }
    if ( $sta7_mark_d3 ne "") {
print <<DONE;
$sta7_mark_d3,
DONE
    }
    if ( $sta7_mark_d4 ne "") {
print <<DONE;
$sta7_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta8_mark_name ne "" && $sta8_mark_loc ne "" ) {
print <<DONE;
$sta8_mark_name;$sta8_mark_loc;
DONE
    if ( $sta8_mark_d1 ne "") {
print <<DONE;
$sta8_mark_d1,
DONE
    }
    if ( $sta8_mark_d2 ne "") {
print <<DONE;
$sta8_mark_d2,
DONE
    }
    if ( $sta8_mark_d3 ne "") {
print <<DONE;
$sta8_mark_d3,
DONE
    }
    if ( $sta8_mark_d4 ne "") {
print <<DONE;
$sta8_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta9_mark_name ne "" && $sta9_mark_loc ne "" ) {
print <<DONE;
$sta9_mark_name;$sta9_mark_loc;
DONE
    if ( $sta9_mark_d1 ne "") {
print <<DONE;
$sta9_mark_d1,
DONE
    }
    if ( $sta9_mark_d2 ne "") {
print <<DONE;
$sta9_mark_d2,
DONE
    }
    if ( $sta9_mark_d3 ne "") {
print <<DONE;
$sta9_mark_d3,
DONE
    }
    if ( $sta9_mark_d4 ne "") {
print <<DONE;
$sta9_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta10_mark_name ne "" && $sta10_mark_loc ne "" ) {
print <<DONE;
$sta10_mark_name;$sta10_mark_loc;
DONE
    if ( $sta10_mark_d1 ne "") {
print <<DONE;
$sta10_mark_d1,
DONE
    }
    if ( $sta10_mark_d2 ne "") {
print <<DONE;
$sta10_mark_d2,
DONE
    }
    if ( $sta10_mark_d3 ne "") {
print <<DONE;
$sta10_mark_d3,
DONE
    }
    if ( $sta10_mark_d4 ne "") {
print <<DONE;
$sta10_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta11_mark_name ne "" && $sta11_mark_loc ne "" ) {
print <<DONE;
$sta11_mark_name;$sta11_mark_loc;
DONE
    if ( $sta11_mark_d1 ne "") {
print <<DONE;
$sta11_mark_d1,
DONE
    }
    if ( $sta11_mark_d2 ne "") {
print <<DONE;
$sta11_mark_d2,
DONE
    }
    if ( $sta11_mark_d3 ne "") {
print <<DONE;
$sta11_mark_d3,
DONE
    }
    if ( $sta11_mark_d4 ne "") {
print <<DONE;
$sta11_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta12_mark_name ne "" && $sta12_mark_loc ne "" ) {
print <<DONE;
$sta12_mark_name;$sta12_mark_loc;
DONE
    if ( $sta12_mark_d1 ne "") {
print <<DONE;
$sta12_mark_d1,
DONE
    }
    if ( $sta12_mark_d2 ne "") {
print <<DONE;
$sta12_mark_d2,
DONE
    }
    if ( $sta12_mark_d3 ne "") {
print <<DONE;
$sta12_mark_d3,
DONE
    }
    if ( $sta12_mark_d4 ne "") {
print <<DONE;
$sta12_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta13_mark_name ne "" && $sta13_mark_loc ne "" ) {
print <<DONE;
$sta13_mark_name;$sta13_mark_loc;
DONE
    if ( $sta13_mark_d1 ne "") {
print <<DONE;
$sta13_mark_d1,
DONE
    }
    if ( $sta13_mark_d2 ne "") {
print <<DONE;
$sta13_mark_d2,
DONE
    }
    if ( $sta13_mark_d3 ne "") {
print <<DONE;
$sta13_mark_d3,
DONE
    }
    if ( $sta13_mark_d4 ne "") {
print <<DONE;
$sta13_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta14_mark_name ne "" && $sta14_mark_loc ne "" ) {
print <<DONE;
$sta14_mark_name;$sta14_mark_loc;
DONE
    if ( $sta14_mark_d1 ne "") {
print <<DONE;
$sta14_mark_d1,
DONE
    }
    if ( $sta14_mark_d2 ne "") {
print <<DONE;
$sta14_mark_d2,
DONE
    }
    if ( $sta14_mark_d3 ne "") {
print <<DONE;
$sta14_mark_d3,
DONE
    }
    if ( $sta14_mark_d4 ne "") {
print <<DONE;
$sta14_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta15_mark_name ne "" && $sta15_mark_loc ne "" ) {
print <<DONE;
$sta15_mark_name;$sta15_mark_loc;
DONE
    if ( $sta15_mark_d1 ne "") {
print <<DONE;
$sta15_mark_d1,
DONE
    }
    if ( $sta15_mark_d2 ne "") {
print <<DONE;
$sta15_mark_d2,
DONE
    }
    if ( $sta15_mark_d3 ne "") {
print <<DONE;
$sta15_mark_d3,
DONE
    }
    if ( $sta15_mark_d4 ne "") {
print <<DONE;
$sta15_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta16_mark_name ne "" && $sta16_mark_loc ne "" ) {
print <<DONE;
$sta16_mark_name;$sta16_mark_loc;
DONE
    if ( $sta16_mark_d1 ne "") {
print <<DONE;
$sta16_mark_d1,
DONE
    }
    if ( $sta16_mark_d2 ne "") {
print <<DONE;
$sta16_mark_d2,
DONE
    }
    if ( $sta16_mark_d3 ne "") {
print <<DONE;
$sta16_mark_d3,
DONE
    }
    if ( $sta16_mark_d4 ne "") {
print <<DONE;
$sta16_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta17_mark_name ne "" && $sta17_mark_loc ne "" ) {
print <<DONE;
$sta17_mark_name;$sta17_mark_loc;
DONE
    if ( $sta17_mark_d1 ne "") {
print <<DONE;
$sta17_mark_d1,
DONE
    }
    if ( $sta17_mark_d2 ne "") {
print <<DONE;
$sta17_mark_d2,
DONE
    }
    if ( $sta17_mark_d3 ne "") {
print <<DONE;
$sta17_mark_d3,
DONE
    }
    if ( $sta17_mark_d4 ne "") {
print <<DONE;
$sta17_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta18_mark_name ne "" && $sta18_mark_loc ne "" ) {
print <<DONE;
$sta18_mark_name;$sta18_mark_loc;
DONE
    if ( $sta18_mark_d1 ne "") {
print <<DONE;
$sta18_mark_d1,
DONE
    }
    if ( $sta18_mark_d2 ne "") {
print <<DONE;
$sta18_mark_d2,
DONE
    }
    if ( $sta18_mark_d3 ne "") {
print <<DONE;
$sta18_mark_d3,
DONE
    }
    if ( $sta18_mark_d4 ne "") {
print <<DONE;
$sta18_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta19_mark_name ne "" && $sta19_mark_loc ne "" ) {
print <<DONE;
$sta19_mark_name;$sta19_mark_loc;
DONE
    if ( $sta19_mark_d1 ne "") {
print <<DONE;
$sta19_mark_d1,
DONE
    }
    if ( $sta19_mark_d2 ne "") {
print <<DONE;
$sta19_mark_d2,
DONE
    }
    if ( $sta19_mark_d3 ne "") {
print <<DONE;
$sta19_mark_d3,
DONE
    }
    if ( $sta19_mark_d4 ne "") {
print <<DONE;
$sta19_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

  if ( $sta20_mark_name ne "" && $sta20_mark_loc ne "" ) {
print <<DONE;
$sta20_mark_name;$sta20_mark_loc;
DONE
    if ( $sta20_mark_d1 ne "") {
print <<DONE;
$sta20_mark_d1,
DONE
    }
    if ( $sta20_mark_d2 ne "") {
print <<DONE;
$sta20_mark_d2,
DONE
    }
    if ( $sta20_mark_d3 ne "") {
print <<DONE;
$sta20_mark_d3,
DONE
    }
    if ( $sta20_mark_d4 ne "") {
print <<DONE;
$sta20_mark_d4
DONE
    }
print <<DONE;
<br>
DONE
  }

# Get terrain min and max info
my $terr_sha_max_d1 = $web_input->param('terr_sha_max_d1');
my $terr_sha_min_d1 = $web_input->param('terr_sha_min_d1');
my $terr_sha_max_d2 = $web_input->param('terr_sha_max_d2');
my $terr_sha_min_d2 = $web_input->param('terr_sha_min_d2');
my $terr_sha_max_d3 = $web_input->param('terr_sha_max_d3');
my $terr_sha_min_d3 = $web_input->param('terr_sha_min_d3');
my $terr_sha_max_d4 = $web_input->param('terr_sha_max_d4');
my $terr_sha_min_d4 = $web_input->param('terr_sha_min_d4');

print <<DONE;
***Terrain Heights***
<br>Domain; Min; Max
<br>1;$terr_sha_min_d1;$terr_sha_max_d1
<br>2;$terr_sha_min_d2;$terr_sha_max_d2
<br>3;$terr_sha_min_d3;$terr_sha_max_d3
<br>4;$terr_sha_min_d4;$terr_sha_max_d4
DONE

print <<DONE;
<br>***END OF INPUT***
DONE

#print $web_input->end_html();

# now, write data to GCBM_file
my(@names) = $web_input->param;
my(@values,$value,$name,$save);
my($key) = "$Range";
$key =~ s/ /+/g;
#$key=~s/([^a-zA-Z0-9_\-.])/uc sprintf("%%%02x",ord($1))/eg;
#print "<br>key = $key\n";
foreach $name (@names)
{
#print "<br>name = $name\n";
  if ($name eq "Range") {
#      print "<br>in $name eq Range loop<br>";
      if ($new_range ne '') {
      $value = $new_range;
      } else {
      $value = $Range;
      }
  } else {
#      print "<br>not in $name eq Range loop<br>";
    @values = $web_input->param($name);
    $value = join(',',@values);
    }
#print "<br>value = $value\n";
  $value=~s/([^a-zA-Z0-9_\-.])/uc sprintf("%%%02x",ord($1))/eg;
  $save = "$save$name=$value&";
  }
chop($save);
#print "<p>save is $save\n";
ReadWriteDB::WriteData($key,$save);

print $web_input->end_html();

exit;


#!/usr/bin/perl -wT --

use strict;
use CGI; # Lincold Stein's CGI.pm perl module - http://stein.cshl.org/WWW/CGI/

my $web_input = new CGI; # gets form input fields into one CGI object

# open web page for writing to screen
print $web_input->header();

# Universal Parameters
my $Range = $web_input->param('Range');
my $wind_barb = $web_input->param('wind_barb');
my $N_doms = $web_input->param('N_doms');

print $web_input->start_html(-title=>"$Range input for NCL");

print <<DONE;
***Universal Parameters for $Range***
<br>Wind (kts/mps); No. Doms
<br>$wind_barb;$N_doms
<br>***Plot Specific Parameters***
<br>Plot Name;Sha Int;Ctr Int1;Ctr Int2;Sha Max Smr; Sha Min Smr; Sha Max Wtr; Sha Min Wtr; Clr Tbl
DONE

# T2mWnd - MSLP/Temp/Wind
my $T2mWnd_name = $web_input->param('T2mWnd_name');
my $T2mWnd_ctr_int = $web_input->param('T2mWnd_ctr_int');
my $T2mWnd_sha_int = $web_input->param('T2mWnd_sha_int');
my $T2mWnd_rng_smr = $web_input->param('T2mWnd_rng_smr');
my $T2mWnd_rng_wtr = $web_input->param('T2mWnd_rng_wtr');
my $T2mWnd_color   = $web_input->param('T2mWnd_color');
  if ( $T2mWnd_name ne "") {
print <<DONE;
<br>$T2mWnd_name;$T2mWnd_sha_int;$T2mWnd_ctr_int;999,999,999,999;$T2mWnd_rng_smr;$T2mWnd_rng_wtr;$T2mWnd_color
DONE
  }

# RHsfcWnd  - RH/Wind
my $RHsfcWnd_name = $web_input->param('RHsfcWnd_name');
my $RHsfcWnd_color   = $web_input->param('RHsfcWnd_color');
  if ( $RHsfcWnd_name ne "") {
print <<DONE;
<br>$RHsfcWnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHsfcWnd_color
DONE
  }

# Td2mWnd  - 2m Td/Wind
my $Td2mWnd_name = $web_input->param('Td2mWnd_name');
my $Td2mWnd_rng_smr = $web_input->param('Td2mWnd_rng_smr');
my $Td2mWnd_rng_wtr = $web_input->param('Td2mWnd_rng_wtr');
my $Td2mWnd_color   = $web_input->param('Td2mWnd_color');
  if ( $Td2mWnd_name ne "") {
print <<DONE;
<br>$Td2mWnd_name;4,4,2,2;999,999,999,999;999,999,999,999;$Td2mWnd_rng_smr;$Td2mWnd_rng_wtr;$Td2mWnd_color
DONE
  }

# Qv2mWnd  - 2m Qv/Wind
my $Qv2mWnd_name = $web_input->param('Qv2mWnd_name');
my $Qv2mWnd_color   = $web_input->param('Qv2mWnd_color');
  if ( $Qv2mWnd_name ne "") {
print <<DONE;
<br>$Qv2mWnd_name;2,2,1,1;999,999,999,999;999,999,999,999;30;2;20;2;$Qv2mWnd_color
DONE
  }

# ThEsfcWnd - Theta-E/Wind
my $ThEsfcWnd_name = $web_input->param('ThEsfcWnd_name');
my $ThEsfcWnd_rng_smr = $web_input->param('ThEsfcWnd_rng_smr');
my $ThEsfcWnd_rng_wtr = $web_input->param('ThEsfcWnd_rng_wtr');
my $ThEsfcWnd_color   = $web_input->param('ThEsfcWnd_color');
  if ( $ThEsfcWnd_name ne "") {
print <<DONE;
<br>$ThEsfcWnd_name;5,5,2.5,2.5;999,999,999,999;999,999,999,999;$ThEsfcWnd_rng_smr;$ThEsfcWnd_rng_wtr;$ThEsfcWnd_color
DONE
  }

# Strm10m - Surface Streamlines
my $Strm10m_name = $web_input->param('Strm10m_name');
my $Strm10m_sha_int = $web_input->param('Strm10m_sha_int');
my $Strm10m_sha_max = $web_input->param('Strm10m_sha_max');
my $Strm10m_sha_min = $web_input->param('Strm10m_sha_min');
my $Strm10m_color   = $web_input->param('Strm10m_color');
  if ( $Strm10m_name ne "") {
print <<DONE;
<br>$Strm10m_name;$Strm10m_sha_int,$Strm10m_sha_int,$Strm10m_sha_int,$Strm10m_sha_int;999,999,999,999;999,999,999,999;$Strm10m_sha_max;$Strm10m_sha_min;$Strm10m_sha_max;$Strm10m_sha_min;$Strm10m_color
DONE
  }

# sfcstrm - Surface Wind Speed/Streamlines
my $sfcstrm_name = $web_input->param('sfcstrm_name');
my $sfcstrm_color= $web_input->param('sfcstrm_color');
  if ( ($sfcstrm_name ne "") && ($wind_barb =="5mps") ) {
print <<DONE;
<br>$sfcstrm_name;5,5,5,5;999,999,999,999;999,999,999,999;35;5;35;5;$sfcstrm_color
DONE
  }
  if ( ($sfcstrm_name ne "") && ($wind_barb =="10kts") ) {
print <<DONE;
<br>$sfcstrm_name;10,10,10,10;999,999,999,999;999,999,999,999;70;10;70;10;$sfcstrm_color
DONE
  }

# Pre1hr - 1 Hour Precip
my $Pre1hr_name = $web_input->param('Pre1hr_name');
my $Pre1hr_color= $web_input->param('Pre1hr_color');
  if ( $Pre1hr_name ne "") {
print <<DONE;
<br>$Pre1hr_name;0.5,0.5,0.5,0.5;999,999,999,999;999,999,999,999;35;0.1;35;0.1;$Pre1hr_color
DONE
  }

# Pre1hrZ - MSLP/Thickness/Wind/Precip (NVL)
my $Pre1hrZ_name = $web_input->param('Pre1hrZ_name');
my $Pre1hrZ_color= $web_input->param('Pre1hrZ_color');
my $Pre1hrZ_ctr_int = $web_input->param('Pre1hrZ_ctr_int');
my $Pre1hrZ_dsh_int = $web_input->param('Pre1hrZ_dsh_int');
  if ( $Pre1hrZ_dsh_int == "6,3,1,0.5") {
print <<DONE;
<br>Pre1hrZ;0.5,0.5,0.5,0.5;$Pre1hrZ_ctr_int;$Pre1hrZ_dsh_int;35;0.1;35;0.1;$Pre1hrZ_color
DONE
  }
  if ( $Pre1hrZ_dsh_int == "3,2,1,0.5") {
print <<DONE;
<br>Pre1hrZ2;0.5,0.5,0.5,0.5;$Pre1hrZ_ctr_int;$Pre1hrZ_dsh_int;35;0.1;35;0.1;$Pre1hrZ_color
DONE
  }

# radar - Radar Reflectivity
my $radar_name  = $web_input->param('radar_name');
my $radar_color = $web_input->param('radar_color');
  if ( $radar_name ne "") {
print <<DONE;
<br>$radar_name;5,5,5,5;999,999,999,999;999,999,999,999;75;5;75;5;$radar_color
DONE
  }

# cape - CAPE
my $cape_name  = $web_input->param('cape_name');
my $cape_color = $web_input->param('cape_color');
  if ( $cape_name ne "") {
print <<DONE;
<br>$cape_name;200,200,200,200;999,999,999,999;999,999,999,999;5000;0;3000;0;$cape_color
DONE
  }

# cin - CIN
my $cin_name  = $web_input->param('cin_name');
my $cin_color = $web_input->param('cin_color');
  if ( $cin_name ne "") {
print <<DONE;
<br>$cin_name;50,50,50,50;999,999,999,999;999,999,999,999;0;-600;0;-600;$cin_color
DONE
  }

# pblhgt - PBL Height
my $pblhgt_name    = $web_input->param('pblhgt_name');
my $pblhgt_sha_max = $web_input->param('pblhgt_sha_max');
my $pblhgt_color   = $web_input->param('pblhgt_color');
  if ( $pblhgt_name ne "") {
print <<DONE;
<br>$pblhgt_name;250,250,250,250;999,999,999,999;999,999,999,999;$pblhgt_sha_max;0;$pblhgt_sha_max;0;$pblhgt_color
DONE
  }

# sfcvis - Surface Visibility
my $sfcvis_name = $web_input->param('sfcvis_name');
my $sfcvis_sha_int = $web_input->param('sfcvis_sha_int');
my $sfcvis_sha_max = $web_input->param('sfcvis_sha_max');
my $sfcvis_color   = $web_input->param('sfcvis_color');
  if ( $sfcvis_name ne "") {
    if ( $sfcvis_sha_max == "30") {
print <<DONE;
<br>$sfcvis_name;$sfcvis_sha_int,$sfcvis_sha_int,$sfcvis_sha_int,$sfcvis_sha_int;999,999,999,999;999,999,999,999;$sfcvis_sha_max;$sfcvis_sha_int;$sfcvis_sha_max;$sfcvis_sha_int;VisCBCust2
DONE
    }
    if ( $sfcvis_sha_max ne "30") {
print <<DONE;
<br>$sfcvis_name;$sfcvis_sha_int,$sfcvis_sha_int,$sfcvis_sha_int,$sfcvis_sha_int;999,999,999,999;999,999,999,999;$sfcvis_sha_max;$sfcvis_sha_int;$sfcvis_sha_max;$sfcvis_sha_int;$sfcvis_color
DONE
    }
  }

# snowcv - Snow Cover (CRTC)
my $snowcv_name  = $web_input->param('snowcv_name');
my $snowcv_color = $web_input->param('snowcv_color');
  if ( $snowcv_name ne "") {
print <<DONE;
<br>$snowcv_name;100,100,100,100;20,20,20,20;999,999,999,999;3500;0;3500;0;$snowcv_color
DONE
  }

# ms500h - MSLP/Thickness
my $ms500h_name = $web_input->param('ms500h_name');
my $ms500h_ctr_int = $web_input->param('ms500h_ctr_int');
  if ( $ms500h_name ne "") {
print <<DONE;
<br>$ms500h_name;200,200,200,200;4,3,2,2;$ms500h_ctr_int;3000;0;3000;0;Grayscale
DONE
  }

# Z925MSLP - MSLP/925 mb height (ATC)
my $Z925MSLP_name = $web_input->param('Z925MSLP_name');
  if ( $Z925MSLP_name ne "") {
print <<DONE;
<br>$Z925MSLP_name;200,200,200,200;4,3,2,2;30,30,7.5,7.5;3000;0;3000;0;Grayscale
DONE
  }

# Get T/Height/Wind Color Scale
my $TXXXWnd_color = $web_input->param('TXXXWnd_color');

# T975Wnd - 975 hPa T/Height/Wind
my $T975Wnd_name = $web_input->param('T975Wnd_name');
  if ( $T975Wnd_name ne "") {
print <<DONE;
<br>$T975Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;48;-16;40;-24;$TXXXWnd_color
DONE
  }

# T950Wnd - 950 hPa T/Height/Wind
my $T950Wnd_name = $web_input->param('T950Wnd_name');
  if ( $T950Wnd_name ne "") {
print <<DONE;
<br>$T950Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;48;-16;40;-24;$TXXXWnd_color
DONE
  }

# T925Wnd - 925 hPa T/Height/Wind
my $T925Wnd_name = $web_input->param('T925Wnd_name');
  if ( $T925Wnd_name ne "") {
print <<DONE;
<br>$T925Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;40;-24;24;-40;$TXXXWnd_color
DONE
  }

# T850Wnd - 850 hPa T/Height/Wind
my $T850Wnd_name = $web_input->param('T850Wnd_name');
my $T850Wnd_rng_smr = $web_input->param('T850Wnd_rng_smr');
my $T850Wnd_rng_wtr = $web_input->param('T850Wnd_rng_wtr');
  if ( $T850Wnd_name ne "") {
print <<DONE;
<br>$T850Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;$T850Wnd_rng_smr;$T850Wnd_rng_wtr;$TXXXWnd_color
DONE
  }

# T700Wnd - 700 hPa T/Height/Wind
my $T700Wnd_name = $web_input->param('T700Wnd_name');
my $T700Wnd_rng_smr = $web_input->param('T700Wnd_rng_smr');
my $T700Wnd_rng_wtr = $web_input->param('T700Wnd_rng_wtr');
  if ( $T700Wnd_name ne "") {
print <<DONE;
<br>$T700Wnd_name;4,3,2,2;30,30,30,30;999,999,999,999;$T700Wnd_rng_smr;$T700Wnd_rng_wtr;$TXXXWnd_color
DONE
  }

# T600Wnd - 600 hPa T/Height/Wind
my $T600Wnd_name = $web_input->param('T600Wnd_name');
  if ( $T600Wnd_name ne "") {
print <<DONE;
<br>$T600Wnd_name;4,3,2,2;60,60,60,60;999,999,999,999;16;-40;8;-48;$TXXXWnd_color
DONE
  }

# T500Wnd - 500 hPa T/Height/Wind
my $T500Wnd_name = $web_input->param('T500Wnd_name');
  if ( $T500Wnd_name ne "") {
print <<DONE;
<br>$T500Wnd_name;4,3,2,2;60,60,60,60;999,999,999,999;8;-48;0;-56;$TXXXWnd_color
DONE
  }

# T200Wnd - 200 hPa T/Height/Wind
my $T200Wnd_name = $web_input->param('T200Wnd_name');
  if ( $T200Wnd_name ne "") {
print <<DONE;
<br>$T200Wnd_name;4,3,2,2;120,120,120,120;999,999,999,999;-16;-72;-16;-72;$TXXXWnd_color
DONE
  }

# Get RH/Wind Color Scale
my $RHXXXWnd_color = $web_input->param('RHXXXWnd_color');

# RH975Wnd - 975 hPa RH/Wind
my $RH975Wnd_name = $web_input->param('RH975Wnd_name');
  if ( $RH975Wnd_name ne "") {
print <<DONE;
<br>$RH975Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color
DONE
  }

# RH950Wnd - 950 hPa RH/Wind
my $RH950Wnd_name = $web_input->param('RH950Wnd_name');
  if ( $RH950Wnd_name ne "") {
print <<DONE;
<br>$RH950Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color
DONE
  }

# RH925Wnd - 925 hPa RH/Wind
my $RH925Wnd_name = $web_input->param('RH925Wnd_name');
  if ( $RH925Wnd_name ne "") {
print <<DONE;
<br>$RH925Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color
DONE
  }

# RH850Wnd - 850 hPa RH/Wind
my $RH850Wnd_name = $web_input->param('RH850Wnd_name');
  if ( $RH850Wnd_name ne "") {
print <<DONE;
<br>$RH850Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color
DONE
  }

# RH700Wnd - 700 hPa RH/Wind
my $RH700Wnd_name = $web_input->param('RH700Wnd_name');
  if ( $RH700Wnd_name ne "") {
print <<DONE;
<br>$RH700Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color
DONE
  }

# RH500Wnd - 500 hPa RH/Wind
my $RH500Wnd_name = $web_input->param('RH500Wnd_name');
  if ( $RH500Wnd_name ne "") {
print <<DONE;
<br>$RH500Wnd_name;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$RHXXXWnd_color
DONE
  }

# VV600m - 600 m Vert. Vel. and Terrain
my $VV600m_name    = $web_input->param('VV600m_name');
my $VV600m_sha_int = $web_input->param('VV600m_sha_int');
my $VV600m_color   = $web_input->param('VV600m_color');
  if ( $VV600m_name ne "") {
print <<DONE;
<br>$VV600m_name;$VV600m_sha_int,$VV600m_sha_int,$VV600m_sha_int,$VV600m_sha_int;2,10,25,25;999,999,999,999;3000;0;3000;0;$VV600m_color
DONE
  }

# Omeg700RH - 700 hPa Omega, RH and Height
my $Omeg700RH_name  = $web_input->param('Omeg700RH_name');
my $Omeg700RH_color = $web_input->param('Omeg700RH_color');
  if ( $Omeg700RH_name ne "") {
print <<DONE;
<br>$Omeg700RH_name;10,10,10,10;30,30,30,30;2.500,5,10,20;90;10;90;10;$Omeg700RH_color
DONE
  }

# Vor500Z - 500 hPa Vorticity and Height
my $Vor500Z_name  = $web_input->param('Vor500Z_name');
my $Vor500Z_color = $web_input->param('Vor500Z_color');
  if ( $Vor500Z_name ne "") {
print <<DONE;
<br>$Vor500Z_name;5,5,5,5;60,60,60,60;999,999,999,999;30;-30;30;-30;$Vor500Z_color
DONE
  }

# Get Jet Stream Color Scale
my $WndXXXZ_color = $web_input->param('WndXXXZ_color');

# Wnd300Z - 300 hPa Wind Speed and Height
my $Wnd300Z_name = $web_input->param('Wnd300Z_name');
my $wind_barb = $web_input->param('wind_barb');
  if ( ($Wnd300Z_name ne "") && ($wind_barb =="5mps") ) { 
print <<DONE;
<br>$Wnd300Z_name;10,10,10,10;120,120,120,120;999,999,999,999;90;30;90;30;$WndXXXZ_color
DONE
  }
  if ( ($Wnd300Z_name ne "") && ($wind_barb =="10kts") ) {
print <<DONE;
<br>$Wnd300Z_name;20,20,20,20;120,120,120,120;999,999,999,999;190;70;190;70;$WndXXXZ_color
DONE
  }

# Wnd250Z - 250 hPa Wind Speed and Height
my $Wnd250Z_name = $web_input->param('Wnd250Z_name');
my $wind_barb = $web_input->param('wind_barb');
  if ( ($Wnd250Z_name ne "") && ($wind_barb =="5mps") ) {
print <<DONE;
<br>$Wnd250Z_name;10,10,10,10;120,120,120,120;999,999,999,999;90;30;90;30;$WndXXXZ_color
DONE
  }
  if ( ($Wnd250Z_name ne "") && ($wind_barb =="10kts") ) {
print <<DONE;
<br>$Wnd250Z_name;20,20,20,20;120,120,120,120;999,999,999,999;190;70;190;70;$WndXXXZ_color
DONE
  }

# Wnd200Z - 200 hPa Wind Speed and Height
my $Wnd200Z_name = $web_input->param('Wnd200Z_name');
my $wind_barb = $web_input->param('wind_barb');
  if ( ($Wnd200Z_name ne "") && ($wind_barb =="5mps") ) {
print <<DONE;
<br>$Wnd200Z_name;10,10,10,10;120,120,120,120;999,999,999,999;90;30;90;30;$WndXXXZ_color
DONE
  }
  if ( ($Wnd200Z_name ne "") && ($wind_barb =="10kts") ) {
print <<DONE;
<br>$Wnd200Z_name;20,20,20,20;120,120,120,120;999,999,999,999;190;70;190;70;$WndXXXZ_color
DONE
  }

# Wnd100Z - 100 hPa Wind Speed and Height
my $Wnd100Z_name = $web_input->param('Wnd100Z_name');
my $wind_barb = $web_input->param('wind_barb');
  if ( ($Wnd100Z_name ne "") && ($wind_barb =="5mps") ) {
print <<DONE;
<br>$Wnd100Z_name;10,10,10,10;120,120,120,120;999,999,999,999;90;30;90;30;$WndXXXZ_color
DONE
  }
  if ( ($Wnd100Z_name ne "") && ($wind_barb =="10kts") ) {
print <<DONE;
<br>$Wnd100Z_name;20,20,20,20;120,120,120,120;999,999,999,999;190;70;190;70;$WndXXXZ_color
DONE
  }

# Div250Z - 300 hPa Wind Speed and Height
my $Div250Z_name  = $web_input->param('Div250Z_name');
my $Div250Z_color = $web_input->param('Div250Z_color');
  if ( $Div250Z_name ne "") {
print <<DONE;
<br>$Div250Z_name;2.5,5,5,5;120,120,120,120;999,999,999,999;15;-15;15;-15;$Div250Z_color
DONE
  }

# cust_plts - Add custom plots
my $cust_plts_name = $web_input->param('cust_plts_name');
  if ( $cust_plts_name ne "") {
print <<DONE;
<br>$cust_plts_name;999,999,999,999;999,999,999,999;999,999,999,999;999;999;999;999;MMM
DONE
  }

# IRsat - Cloud Top T
my $IRsat_name  = $web_input->param('IRsat_name');
my $IRsat_color = $web_input->param('IRsat_color');
  if ( $IRsat_name ne "") {
print <<DONE;
<br>$IRsat_name;5,5,5,5;999,999,999,999;999,999,999,999;30;-70;30;-70;$IRsat_color
DONE
  }

# slhflux - Surface Lat. Heat Flux
my $slhflux_name  = $web_input->param('slhflux_name');
my $slhflux_color = $web_input->param('slhflux_color');
  if ( $slhflux_name ne "") { 
print <<DONE;
<br>$slhflux_name;50,50,50,50;999,999,999,999;999,999,999,999;600;-150;600;-150;$slhflux_color
DONE
  }

# sshflux - Surface Sens. Heat Flux
my $sshflux_name  = $web_input->param('sshflux_name');
my $sshflux_color = $web_input->param('sshflux_color');
  if ( $sshflux_name ne "") {
print <<DONE;
<br>$sshflux_name;50,50,50,50;999,999,999,999;999,999,999,999;600;-150;600;-150;$sshflux_color
DONE
  }

# soilt1 - Soil Temperature
my $soilt1_name  = $web_input->param('soilt1_name');
my $soilt1_color = $web_input->param('soilt1_color');
  if ( $soilt1_name ne "") {
print <<DONE;
<br>$soilt1_name;3,3,2,2;999,999,999,999;999,999,999,999;308;248;308;248;$soilt1_color
DONE
  }

# soilm1 - Soil Moisture
my $soilm1_name  = $web_input->param('soilm1_name');
my $soilm1_color = $web_input->param('soilm1_color');
  if ( $soilm1_name ne "") {
print <<DONE;
<br>$soilm1_name;0.05,0.05,0.05,0.05;999,999,999,999;999,999,999,999;0.45;0.05;0.45;0.05;$soilm1_color
DONE
  }

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
<br>xsloc;250,250,250,250;999,999,999,999;999,999,999,999;4000;0;4000;0;Grayscale
DONE

# xsthw - Cross section of Theta and Winds
my $xsthw_name  = $web_input->param('xsthw_name');
my $xsthw_color = $web_input->param('xsthw_color');
  if ( $xsthw_name ne "") {
print <<DONE;
<br>xsthw;5,5,5,5;999,999,999,999;999,999,999,999;400;250;400;250;$xsthw_color
DONE
  }

# xsrhw - Cross section of Theta and Winds
my $xsrhw_name  = $web_input->param('xsrhw_name');
my $xsrhw_color = $web_input->param('xsrhw_color');
  if ( $xsrhw_name ne "") {
print <<DONE;
<br>xsrhw;10,10,10,10;999,999,999,999;999,999,999,999;90;10;90;10;$xsrhw_color
DONE
  }

# xswWnd - Cross section of Theta and Winds
my $xswWnd_name  = $web_input->param('xswWnd_name');
my $xswWnd_color = $web_input->param('xswWnd_color');
  if ( $xswWnd_name ne "") {
print <<DONE;
<br>xswWnd;5,5,20,20;4,4,4,4;999,999,999,999;50;-50;50;-50;$xswWnd_color
DONE
  }

# xsrhcv - Cross section of Theta and Winds
my $xsrhcv_name  = $web_input->param('xsrhcv_name');
my $xsrhcv_color = $web_input->param('xsrhcv_color');
  if ( $xsrhcv_name ne "") {
print <<DONE;
<br>xsrhcv;999,999,10,999;999,999,2,999;999,999,999,999;90;10;90;10;$xsrhcv_color
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
my $snd1_d3_loc = $web_input->param('snd1_d3_loc');
my $snd2_d3_loc = $web_input->param('snd2_d3_loc');
my $snd3_d3_loc = $web_input->param('snd3_d3_loc');
my $snd4_d3_loc = $web_input->param('snd4_d3_loc');
my $snd5_d3_loc = $web_input->param('snd5_d3_loc');

my $snd1_d4_name = $web_input->param('snd1_d4_name');
my $snd2_d4_name = $web_input->param('snd2_d4_name');
my $snd3_d4_name = $web_input->param('snd3_d4_name');
my $snd4_d4_name = $web_input->param('snd4_d4_name');
my $snd5_d4_name = $web_input->param('snd5_d4_name');
my $snd1_d4_loc = $web_input->param('snd1_d4_loc');
my $snd2_d4_loc = $web_input->param('snd2_d4_loc');
my $snd3_d4_loc = $web_input->param('snd3_d4_loc');
my $snd4_d4_loc = $web_input->param('snd4_d4_loc');
my $snd5_d4_loc = $web_input->param('snd5_d4_loc');

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
,$snd2_d1_loc
DONE
  }
  if ( $snd3_d1_name ne "") { 
print <<DONE;
,$snd3_d1_loc
DONE
  }
  if ( $snd4_d1_name ne "") { 
print <<DONE;
,$snd4_d1_loc
DONE
  }
  if ( $snd5_d1_name ne "") {
print <<DONE;
,$snd5_d1_loc
DONE
  }

  if ( $snd1_d2_name ne "") {
print <<DONE;
<br>2;$snd1_d2_loc
DONE
  }
  if ( $snd2_d2_name ne "") { 
print <<DONE;
,$snd2_d2_loc
DONE
  }
  if ( $snd3_d2_name ne "") {
print <<DONE;
,$snd3_d2_loc
DONE
  }
  if ( $snd4_d2_name ne "") {
print <<DONE;
,$snd4_d2_loc
DONE
  }
  if ( $snd5_d2_name ne "") {
print <<DONE;
,$snd5_d2_loc
DONE
  }

  if ( $snd1_d3_name ne "") {
print <<DONE;
<br>3;$snd1_d3_loc
DONE
  }
  if ( $snd2_d3_name ne "") {
print <<DONE;
,$snd2_d3_loc
DONE
  }
  if ( $snd3_d3_name ne "") {
print <<DONE;
,$snd3_d3_loc
DONE
  }
  if ( $snd4_d3_name ne "") {
print <<DONE;
,$snd4_d3_loc
DONE
  }
  if ( $snd5_d3_name ne "") {
print <<DONE;
,$snd5_d3_loc
DONE
  }

  if ( $snd1_d4_name ne "") {
print <<DONE;
<br>4;$snd1_d4_loc
DONE
  }
  if ( $snd2_d4_name ne "") {
print <<DONE;
,$snd2_d4_loc
DONE
  }
  if ( $snd3_d4_name ne "") {
print <<DONE;
,$snd3_d4_loc
DONE
  }
  if ( $snd4_d4_name ne "") {
print <<DONE;
,$snd4_d4_loc
DONE
  }
  if ( $snd5_d4_name ne "") {
print <<DONE;
,$snd5_d4_loc
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

print <<DONE;
***END OF INPUT***
DONE

print $web_input->end_html();

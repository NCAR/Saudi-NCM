#! /usr/bin/perl


#$HYBRID = "TRUE"; # moved into flexinput.pl

$ENV{"RADIANCEDA"} = "0 0 0"; 
$ENV{"IAF_3DVAR"} = "0"; 
$ENV{"fddaObs_Name"} = "ncep_prepbufr_decoded";
$ENV{"WRFOutSufix"} = "_P+FCST";   ## previous forecast, when choose to be as first guess field.
$ENV{"REG_OBS"} = "TRUE";   ## use regular observation
$ENV{"SMOOTH"} = "FALSE";  #FALSE or TRUE case sensitive

$ENV{"RADAR_DIR"} = ""; # We are not using radar 3DVAR
$ENV{"timeWindow"} = "2";   ## hour. Time windown of the data for 3dvar analysis at one time level.
$ENV{"varCase"} = "1 2 2";  ##First guess field: 1 uses GFS, 2 uses previous forecast
$ENV{"FRCGFS"} = "0 0 0"; # 1: force to use GFS when the first guess field is chosen as background field.
                         # 0: no nudging if no data added during grid-nudging period, even if using GFS.
$ENV{"domArr"} = "1 2";   ### chose domain, which you want to do 3DVAR
$ENV{"GRDSTEP"} = "3 3 1"; ###interval*60 minutes for grid nudging
$ENV{"GRDFDDA"}= "1 1 1" ;      ## optional for grid-fdda to be 1 or 2(spectral nudging)

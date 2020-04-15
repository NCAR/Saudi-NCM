; This program reads in WRF-obs surface pairs data in binary format, and 
; writes it out in netCDF format
; Program written by Joe Grim (grim@ucar.edu) on 14 Nov 2013
begin
; set parameters
 range = "CRTC"  ; ATEC range (e.g., CRTC)
 sta_date = (/2012,01,01,00/)
 end_date = (/2012,12,31,18/)
 n_vars = 28  ; # of fields
 setfileoption("bin","ReadByteOrder","LittleEndian")
 time_units = "minutes since "+sta_date(0)+"-"+sta_date(1)+"-"+sta_date(2)+" 00:00:0.0"

; calculate date parameters in Julian units
 sta_date_jul = toint(ut_inv_calendar(sta_date(0),sta_date(1),sta_date(2),sta_date(3),0,0,time_units,0))
 end_date_jul = toint(ut_inv_calendar(end_date(0),end_date(1),end_date(2),end_date(3),0,0,time_units,0))
 sta_date_jul@units = time_units
 end_date_jul@units = time_units
 all_dates = ispan(sta_date_jul,end_date_jul,360)
 all_dates@units = time_units
 n_dates = dimsizes(all_dates)

; Loop between "final" and "fcst" data
 data_type = (/"final","fcst"/)
 n_data_types = dimsizes(data_type)
 do ff=0,n_data_types-1
  if (ff.eq.0) then
   file_suffix = "_F"
  else
   file_suffix = "_P+FCST"
  end if

; Loop through all dates
  do ii=0,n_dates-1
   this_time_greg = ut_calendar(all_dates(ii),-5)
   this_CCYY = this_time_greg(0,0)
   this_MM   = sprinti("%0.2i",this_time_greg(0,1))
   this_DD   = sprinti("%0.2i",this_time_greg(0,2))
   this_hh   = sprinti("%0.2i",this_time_greg(0,3))

 ; read in the data
   in_file = "/d1/MM5SYS/WRF/GW"+range+"/veri_dat/"+this_CCYY+"/veri_dat/sfc/"+data_type(ff)+"/"+this_CCYY+this_MM+this_DD+this_hh+"_veri_dat_"+range+file_suffix
   ;in_file = "/d1/MM5SYS/WRF/GWCRTC/veri_dat/2012/veri_dat/sfc/final/2012091600_veri_dat_CRTC_F"
   if (isfilepresent(in_file)) then
    file_info = systemfunc("ls -l "+in_file)
    file_size = str_get_field(file_info,5," ")
    if (file_size.gt.0) then
     n_rec = dimsizes(fbindirread(in_file,0,-1,"short"))/n_vars  ; the # of records
     raw_data_short = fbindirread(in_file,0,(/n_rec,n_vars/),"short")     ; all data, extracted in 2-byte integer format (aka "short")
     raw_data_char  = fbindirread(in_file,0,(/n_rec,n_vars/),"character") ; all data, extracted in 1-byte character format (used for the "platform" array)
     CCYY = toint(raw_data_short(:,0))
     MMDD = toint(raw_data_short(:,1))
     MM   = MMDD/100
     DD   = MMDD%100
     hhmm = toint(raw_data_short(:,2))
     hh   = hhmm/100
     mm   = hhmm%100
     ss   = mm
     ss   = 0
     this_time = toint(ut_inv_calendar(CCYY,MM,DD,hh,mm,ss,time_units,0))  ; convert date to Julian format
     this_time@units = time_units
     lat = tofloat(raw_data_short(:,3))/100.
     lon = tofloat(raw_data_short(:,4))/100.
     domain = toint(raw_data_short(:,5))
     platform = raw_data_char(:,12:15)  ; 6&7
     mod_sfc_pres = tofloat(raw_data_short(:,8))*10.
     obs_sfc_pres = tofloat(raw_data_short(:,9))*10.
     obs_sfc_pres@_FillValue = -88880.
     obs_sfc_pres_QC = toint(raw_data_short(:,10))
     mod_slp = tofloat(raw_data_short(:,11))*10.
     obs_slp = tofloat(raw_data_short(:,12))*10.
     obs_slp@_FillValue = -88880.
     obs_slp_QC = toint(raw_data_short(:,13))
     mod_ter_elev = toint(raw_data_short(:,14))
     obs_ter_elev = toint(raw_data_short(:,15))
     mod_temp = tofloat(raw_data_short(:,16))/100.
     obs_temp = tofloat(raw_data_short(:,17))/100.
     obs_temp@_FillValue = -88.88
     obs_temp_QC = toint(raw_data_short(:,18))
     mod_mix_rat = tofloat(raw_data_short(:,19))/100.
     obs_mix_rat = tofloat(raw_data_short(:,20))/100.
     obs_mix_rat@_FillValue = -88.88
     obs_mix_rat_QC = toint(raw_data_short(:,21))
     mod_wspd = tofloat(raw_data_short(:,22))/100.
     obs_wspd = tofloat(raw_data_short(:,23))/100.
     obs_wspd@_FillValue = -88.88
     obs_wspd_QC = toint(raw_data_short(:,24))
     mod_wdir = toint(raw_data_short(:,25))
     obs_wdir = toint(raw_data_short(:,26))
     obs_wdir@_FillValue = -8888
     obs_wdir_QC = toint(raw_data_short(:,27))

 ; add array attributes
     this_time@long_name = "Julian date/time"
     lat@units = "degreesN"
     lat@long_name = "station latitude"
     lon@units = "degreesE"
     lon@long_name = "station longitude"
     domain@long_name = "model domain"
     platform@long_name = "observation platform"
     mod_sfc_pres@units = "Pa"
     mod_sfc_pres@long_name = "model surface pressure"
     obs_sfc_pres@units = "Pa"
     obs_sfc_pres@long_name = "observed surface pressure"
     mod_slp@units = "Pa"
     mod_slp@long_name = "model sea level pressure"
     obs_slp@units = "Pa"
     obs_slp@long_name = "observed sea level pressure"
     mod_ter_elev@units = "m"
     mod_ter_elev@long_name = "model terrain elevation"
     obs_ter_elev@units = "m"
     obs_ter_elev@long_name = "station terrain elevation"
     mod_temp@units = "degC"
     mod_temp@long_name = "model temperature"
     obs_temp@units = "degC"
     obs_temp@long_name = "observed temperature"
     mod_mix_rat@units = "g/kg"
     mod_mix_rat@long_name = "model mixing ratio"
     obs_mix_rat@units = "g/kg"
     obs_mix_rat@long_name = "observed mixing ratio"
     mod_wspd@units = "m/s"
     mod_wspd@long_name = "model wind speed"
     obs_wspd@units = "m/s"
     obs_wspd@long_name = "observed wind speed"
     mod_wdir@units = "degrees"
     mod_wdir@long_name = "model wind direction"
     obs_wdir@units = "degrees"
     obs_wdir@long_named = "observed wind direction"

; Create output netCDF file
     out_file = "/d1/MM5SYS/WRF/GW"+range+"/veri_dat/2012/veri_dat/sfc/"+data_type(ff)+"_nc/pairs_"+this_CCYY+this_MM+this_DD+this_hh+".nc"
     if (isfilepresent(out_file)) then
      system("rm -f "+out_file)
     end if
     ncdf_out = addfile(out_file,"c")
     setfileoption("nc","Format","NetCDF4Classic")
     setfileoption(ncdf_out,"CompressionLevel",1)
     ncdf_out->time            = this_time
     ncdf_out->lat             = lat
     ncdf_out->lon             = lon
     ncdf_out->domain          = domain
     ncdf_out->platform        = platform
     ncdf_out->mod_sfc_pres    = mod_sfc_pres
     ncdf_out->obs_sfc_pres    = obs_sfc_pres
     ncdf_out->obs_sfc_pres_QC = obs_sfc_pres_QC
     ncdf_out->mod_slp         = mod_slp
     ncdf_out->obs_slp         = obs_slp
     ncdf_out->obs_slp_QC      = obs_slp_QC
     ncdf_out->mod_ter_elev    = mod_ter_elev
     ncdf_out->obs_ter_elev    = obs_ter_elev
     ncdf_out->mod_temp        = mod_temp
     ncdf_out->obs_temp        = obs_temp
     ncdf_out->obs_temp_QC     = obs_temp_QC
     ncdf_out->mod_mix_rat     = mod_mix_rat
     ncdf_out->obs_mix_rat     = obs_mix_rat
     ncdf_out->obs_mix_rat_QC  = obs_mix_rat_QC
     ncdf_out->mod_wspd        = mod_wspd
     ncdf_out->obs_wspd        = obs_wspd
     ncdf_out->obs_wspd_QC     = obs_wspd_QC
     ncdf_out->mod_wdir        = mod_wdir
     ncdf_out->obs_wdir        = obs_wdir
     ncdf_out->obs_wdir_QC     = obs_wdir_QC
     print("Created output file: "+out_file)
     delete([/this_time,lat,lon,domain,platform,mod_sfc_pres,obs_sfc_pres,obs_sfc_pres_QC,mod_slp,obs_slp,obs_slp_QC,mod_ter_elev,obs_ter_elev,mod_temp,obs_temp,obs_temp_QC,mod_mix_rat,obs_mix_rat,obs_mix_rat_QC,mod_wspd,obs_wspd,obs_wspd_QC,mod_wdir,obs_wdir,obs_wdir_QC,raw_data_short,raw_data_char,CCYY,MMDD,MM,DD,hhmm,hh,mm,ss/])
    end if
   end if
  end do
 end do

end

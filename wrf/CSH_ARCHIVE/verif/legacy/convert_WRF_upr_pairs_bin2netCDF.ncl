; This program reads in WRF-obs upper air pairs data in binary format, and
; writes it out in netCDF format
; Program written by Joe Grim (grim@ucar.edu) on 14 Nov 2013
begin
; set parameters
 range = "CRTC"  ; ATEC range (e.g., CRTC)
 sta_date = (/2012,01,01,00/)
 end_date = (/2012,12,31,18/)
 n_lines_rec = 41  ; the # of lines in each atmospheric profile record
 setfileoption("bin","ReadByteOrder","LittleEndian")
 time_units = "minutes since "+sta_date(0)+"-"+sta_date(1)+"-"+sta_date(2)+" 00:00:0.0"

; calculate date parameters
 sta_date_jul = toint(ut_inv_calendar(sta_date(0),sta_date(1),sta_date(2),sta_date(3),0,0,time_units,0))
 end_date_jul = toint(ut_inv_calendar(end_date(0),end_date(1),end_date(2),end_date(3),0,0,time_units,0))
 sta_date_jul@units = time_units
 end_date_jul@units = time_units
 all_dates = ispan(sta_date_jul,end_date_jul,1440)
 all_dates@units = time_units
 n_dates = dimsizes(all_dates)
 CR = tostring(inttochar(10))  ; carriage return character
 space = " "                   ; space character

; Loop between "final" and "fcst" data
 data_type = (/"final","fcst"/)
 n_data_types = dimsizes(data_type)
 do ff=0,n_data_types-1
  if (ff.eq.0) then
   file_suffix = "_F"
   gen_back = 0  ; how many gen times to go back (gen every 6 hours)
  else
   file_suffix = "_FCST"
   gen_back = 4  ; how many gen times to go back (gen every 6 hours)
  end if

; Loop through all dates
  do ii=0,n_dates-1
   this_time_greg = ut_calendar(all_dates(ii),-5)
   this_CCYY = this_time_greg(0,0)
   this_MM   = sprinti("%0.2i",this_time_greg(0,1))
   this_DD   = sprinti("%0.2i",this_time_greg(0,2))
   this_hh   = sprinti("%0.2i",this_time_greg(0,3))

 ; loop through all gen times
   do gg=0,gen_back
    gen_time = all_dates(ii)-gg*360
    gen_time@units = time_units
    gen_time_greg = ut_calendar(gen_time,-5)
    gen_CCYY = gen_time_greg(0,0)
    gen_MM   = sprinti("%0.2i",gen_time_greg(0,1))
    gen_DD   = sprinti("%0.2i",gen_time_greg(0,2))
    gen_hh   = sprinti("%0.2i",gen_time_greg(0,3))

 ; read in the data
    in_file = "/d1/MM5SYS/WRF/GW"+range+"/veri_dat/"+this_CCYY+"/veri_dat/upr/"+data_type(ff)+"/"+gen_CCYY+gen_MM+gen_DD+gen_hh+"_"+this_CCYY+this_MM+this_DD+this_hh+"_veri_dat_upr_"+range+file_suffix
    ;in_file = "/d1/MM5SYS/WRF/GWCRTC/veri_dat/2012/veri_dat/upr/final/2012010200_2012010200_veri_dat_upr_CRTC_F"
    if (isfilepresent(in_file)) then
     file_info = systemfunc("ls -l "+in_file)
     file_size = str_get_field(file_info,5," ")
     if (file_size.gt.0) then
      raw_data_string = tostring(fbindirread(in_file,0,-1,"character"))  ; all data in character format, converted to string
      n_rec = str_fields_count(raw_data_string,CR)/n_lines_rec  ; the number of separate atmospheric profiles of pairs data
      raw_data_string2 = str_split(raw_data_string,CR)  ; separate each vertical profile
      raw_data_string3 = onedtond(raw_data_string2,(/n_rec,n_lines_rec/))  ; rearrange the profile data into a 2d array, in order to later obtain the header line
      header = raw_data_string3(:,0)  ; extract the header line
      CCYYMMDDhh = str_get_field(header,1,space)  ; extract the data from the header line
      CCYY = tointeger(str_get_cols(CCYYMMDDhh,0,3))
      MM   = tointeger(str_get_cols(CCYYMMDDhh,4,5))
      DD   = tointeger(str_get_cols(CCYYMMDDhh,6,7))
      hh   = tointeger(str_get_cols(CCYYMMDDhh,8,9))
      mm   = hh
      mm   = 0
      ss   = mm
      the_date = ut_inv_calendar(CCYY,MM,DD,hh,mm,ss,time_units,0)  ; convert time to Julian units
      the_date@units = time_units
      st_id = tointeger(str_get_field(header,2,space))  ; station id #
      st_lat = tofloat(str_get_field(header,3,space))
      st_lon = tofloat(str_get_field(header,4,space))
      mod_ter_elv = tointeger(str_get_field(header,5,space))
      obs_ter_elv = tointeger(str_get_field(header,6,space))
      domain = tointeger(str_get_field(header,7,space))
      data3d = tofloat(str_split_csv(str_squeeze(raw_data_string3(:,1:40)),space,0))  ; extract the profile data into a 3d array
      p_levels = data3d(0,:,0)*100.
      mod_temp = data3d(:,:,1)
      mod_temp@_FillValue = -8888.0
      obs_temp = data3d(:,:,2)
      obs_temp@_FillValue = -8888.0
      obs_temp_QC = data3d(:,:,3)
      mod_mix_rat = data3d(:,:,4)
      mod_mix_rat@_FillValue = -8888.0
      obs_mix_rat = data3d(:,:,5)
      obs_mix_rat@_FillValue = -8888.0
      obs_mix_rat_QC = data3d(:,:,6)
      mod_RH = data3d(:,:,7)
      mod_RH@_FillValue = -8888.0
      obs_RH = data3d(:,:,8)
      obs_RH@_FillValue = -8888.0
      obs_RH_QC = data3d(:,:,9)
      mod_wspd = data3d(:,:,10)
      mod_wspd@_FillValue = -8888.0
      obs_wspd = data3d(:,:,11)
      obs_wspd@_FillValue = -8888.0
      obs_wspd_QC = data3d(:,:,12)
      mod_wdir = data3d(:,:,13)
      mod_wdir@_FillValue = -8888.0
      obs_wdir = data3d(:,:,14)
      obs_wdir@_FillValue = -8888.0
      obs_wdir_QC = data3d(:,:,15)
      mod_hght = data3d(:,:,16)
      mod_hght@_FillValue = -8888.0
      obs_hght = data3d(:,:,17)
      obs_hght@_FillValue = -8888.0
      obs_hght_QC = data3d(:,:,18)

 ; add array attributes
      the_date@long_name = "Julian date/time"
      st_id@long_name = "station identification number"
      st_lat@units = "degreesN"
      st_lat@long_name = "station latitude"
      st_lon@units = "degreesE"
      st_lon@long_name = "station longitude"
      mod_ter_elv@units = "m"
      mod_ter_elv@long_name = "model terrain elevation"
      obs_ter_elv@units = "m"
      obs_ter_elv@long_name = "station terrain elevation"
      domain@long_name = "model domain"
      p_levels@units = "Pa"
      p_levels@long_name = "pressure levels"
      mod_temp!0 = "profile"
      mod_temp!1 = "level"
      mod_temp&level = p_levels
      mod_temp@units = "K"
      mod_temp@long_name = "model temperature"
      obs_temp!0 = "profile"
      obs_temp!1 = "level"
      obs_temp&level = p_levels
      obs_temp@units = "K"
      obs_temp@long_name = "observed temperature"
      mod_mix_rat!0 = "profile"
      mod_mix_rat!1 = "level"
      mod_mix_rat&level = p_levels
      mod_mix_rat@units = "g/kg"
      mod_mix_rat@long_name = "model mixing ratio"
      obs_mix_rat!0 = "profile"
      obs_mix_rat!1 = "level"
      obs_mix_rat&level = p_levels
      obs_mix_rat@units = "g/kg"
      obs_mix_rat@long_name = "observed mixing ratio"
      mod_RH!0 = "profile"
      mod_RH!1 = "level"
      mod_RH&level = p_levels
      mod_RH@units = "%"
      mod_RH@long_name = "model relative humidity"
      obs_RH!0 = "profile"
      obs_RH!1 = "level"
      obs_RH&level = p_levels
      obs_RH@units = "%"
      obs_RH@long_name = "observed relative humidity"
      mod_wspd!0 = "profile"
      mod_wspd!1 = "level"
      mod_wspd&level = p_levels
      mod_wspd@units = "m/s"
      mod_wspd@long_name = "model wind speed"
      obs_wspd!0 = "profile"
      obs_wspd!1 = "level"
      obs_wspd&level = p_levels
      obs_wspd@units = "m/s"
      obs_wspd@long_name = "observed wind speed"
      mod_wdir!0 = "profile"
      mod_wdir!1 = "level"
      mod_wdir&level = p_levels
      mod_wdir@units = "degrees"
      mod_wdir@long_name = "model wind direction"
      obs_wdir!0 = "profile"
      obs_wdir!1 = "level"
      obs_wdir&level = p_levels
      obs_wdir@units = "degrees"
      obs_wdir@long_named = "observed wind direction"
      mod_hght!0 = "profile"
      mod_hght!1 = "level"
      mod_hght&level = p_levels
      mod_hght@units = "m"
      mod_hght@long_name = "model height"
      obs_hght!0 = "profile"
      obs_hght!1 = "level"
      obs_hght&level = p_levels
      obs_hght@units = "m"
      obs_hght@long_name = "observed height"

; Create output netCDF file
      out_file = "/d1/MM5SYS/WRF/GW"+range+"/veri_dat/2012/veri_dat/upr/"+data_type(ff)+"_nc/pairs_"+gen_CCYY+gen_MM+gen_DD+gen_hh+"_"+this_CCYY+this_MM+this_DD+this_hh+".nc"
      out_dirs = str_split(out_file,"/")  ; this and the following lines create output directories, if they don't already exist
      n_dirs = dimsizes(out_dirs)-1
      do zz=0,n_dirs-1
       out_dir = "/"+str_join(out_dirs(0:zz),"/")
       if (.not.isfilepresent(out_dir)) then
        system("/bin/mkdir "+out_dir)
       end if
      end do
      if (isfilepresent(out_file)) then
       system("rm -f "+out_file)
      end if
      ncdf_out = addfile(out_file,"c")
      setfileoption("nc","Format","NetCDF4Classic")
      setfileoption(ncdf_out,"CompressionLevel",1)
      ncdf_out->time            = the_date
      ncdf_out->st_id           = st_id
      ncdf_out->st_lat          = st_lat
      ncdf_out->st_lon          = st_lon
      ncdf_out->domain          = domain
      ncdf_out->pressure        = p_levels
      ncdf_out->mod_RH          = mod_RH
      ncdf_out->obs_RH          = obs_RH
      ncdf_out->obs_RH_QC       = obs_RH_QC
      ncdf_out->mod_ter_elev    = mod_ter_elv
      ncdf_out->obs_ter_elev    = obs_ter_elv
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
      ncdf_out->mod_hght        = mod_hght
      ncdf_out->obs_hght        = obs_hght
      ncdf_out->obs_hght_QC     = obs_hght_QC
      print("Created output file: "+out_file)
      delete([/raw_data_string,raw_data_string2,raw_data_string3,header,CCYYMMDDhh,CCYY,MM,DD,hh,mm,ss,the_date,st_id,st_lat,st_lon,mod_ter_elv,obs_ter_elv,domain,data3d,out_dirs,the_date,st_id,st_lat,st_lon,domain,p_levels,mod_RH,obs_RH,obs_RH_QC,mod_ter_elv,obs_ter_elv,mod_temp,obs_temp,obs_temp_QC,mod_mix_rat,obs_mix_rat,obs_mix_rat_QC,mod_wspd,obs_wspd,obs_wspd_QC,mod_wdir,obs_wdir,obs_wdir_QC,mod_hght,obs_hght,obs_hght_QC/])
     end if
    end if
   end do
  end do
 end do

end

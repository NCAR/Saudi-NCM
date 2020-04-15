load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
;======================DESCRIPTION========================
; This program opens wrfout files and extracts data at   ;
; certain lat/lon location(s) and level(s) and outputs   ;
; their values at all forecast times for the field(s) of ;
; interest                                               ;
; Called by:                                             ;
; eval ncl cycle="CCYYMMDDHH" get_wrf_model_pinpoint.ncl ;
; Written by Joe Grim for Wyoming Project                ;
; 14 Dec 2012                                            ;
; A param file called get_wrf_pinpoint_params.asc is     ;
; called and looks like this, minus the leading ";"      ;
;wrfout_path   | /model/atecuser/cycles/GWDPG/DPG
;output_path   | /home/grim/wyoming/wrf_model_output.txt
;vertical_level| 700; 700 ; 700
;vertical_coord| p  ; p   ; p
;fields        | tc ; wspd; wdir
;dom           | 1
;lat_lon       | 41.45,-106.81:43.0,-108.0:44.0,-108.5
;
; Each field is separated by a ";", while multiple       ;
; locations for each field are separated by a ":"        ;
; In this example, 700 mb tc, 700 mb wspd and 700 mb     ;
; wdir will be extracted for 3 locations                 ;
;========================================================;
begin
 n_hrs_sim = 48   ; Number of hours in the simulation

; Read in parameters from param file
 params_raw = asciiread("get_wrf_model_pinpoint_params.asc",-1,"string")
 wrfout_path = str_strip(str_get_field(params_raw(0),2,"|"))          ; the location of the wrfout files that are read in
 output_path = str_strip(str_get_field(params_raw(1),2,"|"))+"wrf_model_output_"+cycle+".txt"  ; the directory where the point data are output
 vertical_level_temp = str_get_field(params_raw(2),2,"|")
 vertical_level = tofloat(str_split(vertical_level_temp,";"))         ; the vertical level(s)
 vertical_coord_temp = str_strip(str_get_field(params_raw(3),2,"|"))  ; limited to "p" and "z"
 vertical_coord = str_strip(str_split(vertical_coord_temp,";"))
 fields_temp = str_strip(str_get_field(params_raw(4),2,"|"))
 fields = str_strip(str_split(fields_temp,";"))                       ; field names are limited to those from NCL wrf_user_getvar variable names
 n_fields = dimsizes(fields)
 dom = str_strip(str_get_field(params_raw(5),2,"|"))                  ; the WRF domain
 lat_lon_temp = str_get_field(params_raw(6),2,"|")
 lat_lon = str_split(lat_lon_temp,":")
 lat_locs = tofloat(str_get_field(lat_lon,1,","))                     ; the latitudinal location
 lon_locs = tofloat(str_get_field(lat_lon,2,","))                     ; the longitudinal location
 n_locs = dimsizes(lat_locs)
 ;print(wrfout_path+" "+output_path+" "+vertical_level+" "+vertical_coord+" "+fields+" "+lat_locs+" "+lon_locs)

; Extract the date for this cycle
 cycle_str = tostring(cycle)
 cycle_CCYY = toint(str_get_cols(cycle_str,0,3))
 cycle_MM   = toint(str_get_cols(cycle_str,4,5))
 cycle_DD   = toint(str_get_cols(cycle_str,6,7))
 cycle_hh   = toint(str_get_cols(cycle_str,8,9))
 time_units = "hours since 2000-1-1 00:00:0.0"
 cycle_sta_time = toint(ut_inv_calendar(cycle_CCYY,cycle_MM,cycle_DD,cycle_hh,0,0,time_units,0)-1.)
 cycle_end_time = toint(cycle_sta_time + n_hrs_sim)
 cycle_all_times = ispan(cycle_sta_time,cycle_end_time,1)
 cycle_all_times@units = time_units
 n_times = dimsizes(cycle_all_times)

; Loop through all cycle output times
 fields_all_times = new((/n_fields,n_locs,n_times/),"float")
 fields_descrip = new(n_fields,"string")
 fields_units   = new(n_fields,"string")
 fields_level   = new(n_fields,"string")
 valid_times    = new(n_times,"string")
 forecast_hour  = new(n_times,"string")
 do ii=0,n_times-1
  fcst_time = ut_calendar(cycle_all_times(ii),-5)
  fcst_time_CCYY = sprinti("%0.4i",fcst_time(0,0))
  fcst_time_MM   = sprinti("%0.2i",fcst_time(0,1))
  fcst_time_DD   = sprinti("%0.2i",fcst_time(0,2))
  fcst_time_hh   = sprinti("%0.2i",fcst_time(0,3))
  forecast_hour(ii) = sprinti("%0.3i",ii)
  valid_times(ii) = fcst_time_CCYY+"/"+fcst_time_MM+"/"+fcst_time_DD+" "+fcst_time_hh+"Z"
  print("Reading in forecast hour "+ii+" of "+n_hrs_sim)

; Open wrfout file and extract data
  wrfout_filename = wrfout_path+"/"+cycle+"/"+"wrfout_d0"+dom+"_"+fcst_time_CCYY+"-"+fcst_time_MM+"-"+fcst_time_DD+"_"+fcst_time_hh+":00:00.DPG_P+FCST"
  if (isfilepresent(wrfout_filename)) then
   aa = addfile(wrfout_filename+".nc","r")
   vert_arr = wrf_user_getvar(aa,vertical_coord,0)
   lats = wrf_user_getvar(aa,"lat",0)
   lons = wrf_user_getvar(aa,"lon",0)
   do jj=0,n_locs-1
    latlon_ind=wrf_latlon_to_ij(lats,lons,lat_locs(jj),lon_locs(jj))     ; obtain the lat/lon indices
    lat_ind = latlon_ind(0)               ; Get the lat index
    lon_ind = latlon_ind(1)               ; Get the lon index
    do kk=0,n_fields-1
     if (fields(kk).ne."wspd".and.fields(kk).ne."wdir") then        ; All non-wind fields are obtained by their NCL wrf_user_getvar variable names
      field_3d = wrf_user_getvar(aa,fields(kk),0)
      field_level = wrf_user_intrp3d(field_3d,vert_arr,"h",vertical_level(kk),0.,False)
      fields_all_times(kk,jj,ii) = field_level(lat_ind,lon_ind)
      fields_descrip(kk) = field_level@description
      fields_units(kk)   = field_level@units
      fields_level(kk)   = field_level@PlotLevelID
     else                                  ; Wind speed and direction need to be calculated separately from their u and v components
      uvm = wrf_user_getvar(aa,"uvmet",0)
      umet = uvm(0,:,:,:)
      vmet = uvm(1,:,:,:)
      ulvl = wrf_user_intrp3d(umet,vert_arr,"h",vertical_level(kk),0.,False)
      vlvl = wrf_user_intrp3d(vmet,vert_arr,"h",vertical_level(kk),0.,False)
      u_loc = ulvl(lat_ind,lon_ind)
      v_loc = vlvl(lat_ind,lon_ind)
      if (fields(kk).eq."wspd") then
       fields_all_times(kk,jj,ii) = (u_loc*u_loc + v_loc*v_loc)^0.5
       fields_descrip(kk) = "Wind Speed"
       fields_units(kk)   = ulvl@units
       fields_level(kk)   = ulvl@PlotLevelID
      end if
      if (fields(kk).eq."wdir") then
       wdir = atan2(u_loc,v_loc)*180./3.14159265359 + 180.
       wdir = where(wdir.ge.359.9999, 0., wdir)  ; force 360 "north winds" to 0
       fields_all_times(kk,jj,ii) = wdir
       fields_descrip(kk) = "Wind Direc"
       fields_units(kk)   = "degrees"
       fields_level(kk)   = ulvl@PlotLevelID
      end if
     end if
    end do
   end do
  end if
 end do

; Create the output text lines
 output_text = new(n_times+5,"string")
 output_text = ""
 output_text(0) =   "Fcst |     Valid Time | "
 output_text(1) =   "Hour |          (UTC) | "
 output_text(2:3) = "     |                | "
 output_text(4) =   "-----|----------------|-"
 output_text(5:) = " "+forecast_hour+" | "+valid_times+" | "
 do kk=0,n_fields-1
  do jj=0,n_locs-1
   n_char_fld = strlen(fields_descrip(kk))
   blnk_dsc = new(13-n_char_fld,"string")
   blnk_dsc = " "
   output_text(0) = output_text(0)+str_concat(blnk_dsc)+fields_descrip(kk)+" | "
   n_char_fld = strlen(fields_units(kk))
   blnk_uni = new(13-n_char_fld,"string")
   blnk_uni = " "
   output_text(1) = output_text(1)+str_concat(blnk_uni)+fields_units(kk)+" | "
   n_char_fld = strlen(fields_level(kk))
   blnk_lvl = new(13-n_char_fld,"string")
   blnk_lvl = " "
   output_text(2) = output_text(2)+str_concat(blnk_lvl)+fields_level(kk)+" | "
   output_text(3) = output_text(3)+sprintf("%6.1f",lat_locs(jj))+","+sprintf("%6.1f",lon_locs(jj))+" | "
   output_text(4) = output_text(4)+"--------------|-"
   output_text(5:) = output_text(5:)+sprintf("%13.1f",fields_all_times(kk,jj,:))+" | "
   delete(blnk_dsc)
   delete(blnk_uni)
   delete(blnk_lvl)
  end do
 end do

; Output the text lines
 asciiwrite(output_path,output_text)
 print("Created file: "+output_path)

end

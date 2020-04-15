; This script reads in the latest analog ensemble file for the given cycle, and
;  creates time series spaghetti plots of the relevant fields.
; It should be run following this command line:
; eval ncl 'range=\"$RANGE\"' cycle=$CYCLE day_to_plot=$DATE 'meas=\"$MEAS\"' plot_analog_ensemble_time_series.ncl
; for example,
; eval ncl 'range=\"crtc\"' cycle=18 day_to_plot=20180131 'meas=\"Eng\"' plot_analog_ensemble_time_series.ncl
; OR
; eval ncl 'range=\"atc\"' cycle=14 day_to_plot=20180131 'meas=\"Met\"' plot_analog_ensemble_time_series.ncl
; eval ncl 'range=\"atc\"' cycle=14 day_to_plot=20180131 'meas=\""' plot_analog_ensemble_time_series.ncl
;-----------------------------
; Read in other NCL scripts
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
; Begin main section of code
begin
; parameters
 ;range = "crtc"
 ;cycle = 18
 ;day_to_plot = 20150729
 output_dir = ""
 PlotType = "png"
 ;n_mem = 30 ; number of ensemble members
 time_units = "hours since 2000-01-01 00:00:00"
 dq = str_get_dq()
 opt_debug=0
 opt_cache=0
 P_INFO = "  INFO  "
 P_WARN = "  WARN  "

; define fields and their attributes, depending on the measurement system
 fields =          (/"temp",          "wspd"/)
 field_long_name = (/"2m temperature","wind speed"/)
 if (.not.isvar("meas").or.(meas.ne."Eng".and.meas.ne."Met")) then
  print("WARNING!!! Please set 'meas' in the command line!")
  print("   example: eval ncl 'range=\"+dq+"crtc\"+dq+"' cycle=18 day_to_plot=20180131 'meas=\"+dq+"Eng\"+dq+ \
   "' plot_analog_ensemble_time_series.ncl")
  print("            eval ncl 'range=\"+dq+"crtc\"+dq+"' cycle=18 day_to_plot=20180131 'meas=\"+dq+"Met\"+dq+ \
   "' plot_analog_ensemble_time_series.ncl")
 end if
 if (meas.eq."Met") then
  field_units    = (/"C",             "m/s"/)
  scale_factor   = (/1.,              1./)
  offset         = (/0.,              0./)
  y_tick_int     = (/5.,              2./)
  n_minor_ticks  = (/4,               3/)
  diff_threshold = (/20,              15/)
  meas = "_"+meas
 end if
 if (meas.eq."Eng") then
  field_units =    (/"F",             "kts"/)
  scale_factor   = (/1.8,             1.94384/)
  offset         = (/32.,             0./)
  y_tick_int     = (/10.,             4./)
  n_minor_ticks  = (/4,               3/)
  diff_threshold = (/36,              29.1576/)
  meas = "_"+meas
 end if
 if (meas.eq."") then
  field_units    = (/"C",             "m/s"/)
  scale_factor   = (/1.,              1./)
  offset         = (/0.,              0./)
  y_tick_int     = (/5.,              2./)
  n_minor_ticks  = (/4,               3/)
  diff_threshold = (/20,              15/)
 end if

; a few derived parameters
 n_fields = dimsizes(fields)
 cycle_str = sprinti("%0.2i",cycle)
 
 anen_inputs = new((/n_fields/),string)

; define arrays for field attributes
 field_missing   = new(n_fields,"float")

; Read in supporting data
 in_base_dir = "/model"
 ret_val = systemfunc("test -d "+in_base_dir+"; echo $?")
 if(ret_val .ne. 0) then
   in_base_dir = "/p/work1"
 end if
 if (getenv("LOGNAME").ne."grim") then
  in_base_dir = in_base_dir+"/"+getenv("LOGNAME")+"/datainput/"+range  ; non-testing directory
 else
  in_base_dir = in_base_dir+"/"+getenv("LOGNAME")+"/datainput/ops/"+range  ; testing directory
 end if

 current_date = systemfunc("date '+%Y%m%d%H'")

;
; Loop through the two fields to get AnEn data
 total_valid_data_count = 0
 do ff=0,n_fields-1
 
  ;diff_threshold(ff) = diff_threshold(ff)*scale_factor(ff)

  in_file = in_base_dir+"/veri_pair.anen/ANEN."+fields(ff)+"."+cycle_str+".nc"
  ncdf_in = addfile(in_file,"r")
  dim_names = getfilevardims(ncdf_in,"AnEn")
  dim_sizes = filevardimsizes(ncdf_in,"AnEn")
  ndays = dim_sizes(ind(dim_names.eq."NDAYS"))
  n_members = dim_sizes(ind(dim_names.eq."NMEMBERS"))
  n_lead_times = dim_sizes(ind(dim_names.eq."NLEADTIMES"))
  n_sites = dim_sizes(ind(dim_names.eq."NSTATIONS"))

  date_index = ndays-1
  temp_date = ncdf_in->NDAYS(date_index)
  last_date = "20"+temp_date
  do while(last_date.ne.day_to_plot)
   date_index = date_index - 1
   if (date_index.lt.0) then
    print("   ERROR: can not find the plot day: " + day_to_plot + " from " + in_file)
    break
   end if
   temp_date = ncdf_in->NDAYS(date_index)
   last_date = "20"+temp_date
  end do
  
  ;if (n_members.ne.n_mem) then
  ; print("The number of ensemble members is not the same as the default. Please check parameters at top of script.")
  ;end if
  if (date_index .ge. 0) then
   this_field = ncdf_in->AnEn(:,:,date_index,:) ; file array dimensions::(NMEMBERS, NLEADTIMES, NDAYS, NSTATIONS)
   this_field = scale_factor(ff)*this_field+offset(ff)
   valid_count = num(.not.ismissing(this_field))
   missing_count = num(ismissing(this_field))
   total_valid_data_count = total_valid_data_count + valid_count
   if (missing_count .gt. 0) then
    if (valid_count.eq.0) then
     print("INFO:   All AnEn data (" + missing_count + ") is missing from " + in_file)
    else
     print("INFO:   Some missing AnEn data (" + missing_count + "), valid="+valid_count + " for " + ncdf_in->NDAYS(date_index) + " from " + in_file)
     lead_index = 0
     do while(lead_index.lt.n_lead_times)
      missing_count_lead = num(ismissing(this_field(:,lead_index,:)))
      if (missing_count_lead.eq.(n_members*n_sites)) then
       print(P_INFO + fields(ff) + "(all): No valid data for lead time " + lead_index + " missing=" + missing_count_lead)
      end if
      lead_index = lead_index + 1
     end do
    end if
   end if
   
   mean_all = dim_avg_n_Wrap(this_field,0)
   stddev_all = dim_stddev_n_Wrap(this_field,0)
   max_all = max(mean_all+stddev_all)
   min_all = min(mean_all-stddev_all)
   print(P_INFO + fields(ff) + "(all): min=" + min(this_field) + " max=" + max(this_field) + " stat: min="+min_all+" max="+max_all)
   delete([/mean_all,stddev_all/])
   ;in_file@this_field = this_field
  else
   this_field = ncdf_in->AnEn(:,:,0,:) ; file array dimensions::(NMEMBERS, NLEADTIMES, NDAYS, NSTATIONS)
   this_field = ncdf_in->AnEn@_FillValue
  end if
  copy_VarAtts(ncdf_in->AnEn, this_field)
  if (ff.eq.0) then
   temp_field = this_field
   temp_nc = ncdf_in
  else if (ff.eq.1) then
   wind_field = this_field
   wind_nc = ncdf_in
  end if
  end if
  in_file@date_index = date_index
  in_file@last_date = last_date
  in_file@lead_time = ncdf_in->NLEADTIMES

  anen_inputs(ff) = in_file
  delete([/this_field, in_file, ncdf_in/])
 end do
 
;
; Exit if the day_to_plot does not exist at AnEn data
 if (total_valid_data_count.eq.0) then
  print("   ERROR: No plot created because it can not find the plot day: " + day_to_plot)
  exit()
 end if
 
 n_lead_times_model = n_lead_times
 
;
; Read Obs. data
 in_file = in_base_dir+"/veri_pair/recent.veri_pair."+cycle_str+".cdf"
 ncdf_in = addfile(in_file,"r")
 lead_time = ncdf_in->lead_time
 n_lead_times = dimsizes(lead_time)
 if (n_lead_times.ne.n_lead_times_model) then
  print(P_WARN + "The lead times are different. obs: " + n_lead_times + ", AnEn: " + n_lead_times_model)
  if (n_lead_times.gt.n_lead_times_model) then
   delete([/lead_time/])
   anen_input = anen_inputs(0)
   lead_time = anen_input@lead_time
   n_lead_times = dimsizes(lead_time)
  end if
 end if
 site  = str_squeeze(tostring(ncdf_in->site))
 site_CAP = str_upper(site)
 n_sites = dimsizes(site)
 n_dates = filevardimsizes(ncdf_in,"gen_date")
 ;We want to grab specific dates based on an day input parameter
 ;This loop finds the index of that day to use further on
 date_index = (n_dates-1)
 gen_date = ncdf_in->gen_date(date_index)
 do while(gen_date.ne.day_to_plot)
  date_index = date_index - 1
  if (date_index.lt.0) then
   print("   ERROR: can not find the plot day: " + day_to_plot + " from " + in_file)
   print("   gen_dates: " + ncdf_in->gen_date)
   gen_date = toint(day_to_plot)
   break
  end if
  gen_date = ncdf_in->gen_date(date_index)
 end do
 field_obs = new((/n_fields,n_lead_times,n_sites/),"float")
 do ff=0,n_fields-1
  field_name = fields(ff)+"_m"
  field_name_obs = fields(ff)+"_o"
  ;field_long_name(ff) = ncdf_in->$field_name$@long_name
  ;field_units(ff)     = ncdf_in->$field_name$@units
  field_missing(ff)   = ncdf_in->$field_name$@_FillValue
  if (date_index .ge. 0) then
   field_obs(ff,:,:) = ncdf_in->$field_name_obs$(date_index,:(n_lead_times-1),:)  ; (gen_date,lead_time,site)
   missing_count = num(ismissing(field_obs(ff,:,:)))
   valid_count = num(.not.ismissing(field_obs(ff,:,:)))
   field_obs(ff,:,:) = scale_factor(ff)*field_obs(ff,:,:)+offset(ff)
   max_all = max(field_obs(ff,:,:))
   min_all = min(field_obs(ff,:,:))
   print(P_INFO + field_name_obs + "(all): min=" + min_all + " max=" + max_all + " missing=" + missing_count+", valid=" + valid_count)
  else
   field_obs(ff,:,:) = field_missing(ff)  ; (gen_date,lead_time,site)
  end if
 end do

; Create forecast time array
 colon = inttochar(58)
 slash = inttochar(47)
 gen_date_CCYY = gen_date/10000
 gen_date_MM = (gen_date-gen_date_CCYY*10000)/100
 gen_date_DD = gen_date%100
 gen_date_jul = toint(ut_inv_calendar(gen_date_CCYY,gen_date_MM,gen_date_DD,cycle,0,0,time_units,0))
 all_dates_jul = gen_date_jul+lead_time
 all_dates_jul@units = time_units
 all_dates_greg = ut_calendar(all_dates_jul,-5)
 all_dates_lbls = sprinti("%0.2i",all_dates_greg(:,3))+"00 ~C~"+sprinti("%0.2i",all_dates_greg(:,1))+slash+sprinti("%0.2i",all_dates_greg(:,2))
 all_dates_lbls_bw = sprinti("%0.2i",all_dates_greg(:,3))+"00 "+sprinti("%0.2i",all_dates_greg(:,1))+slash+sprinti("%0.2i",all_dates_greg(:,2))
 x_axis_lbl_intv = round(ceil(tofloat(max(lead_time))/12.),3)
 lead_time_X_vals = lead_time(:(n_lead_times-1):x_axis_lbl_intv)
 lead_time_X_lbls = all_dates_lbls(::x_axis_lbl_intv)

; Loop through all sites at range
 do ss=0,n_sites-1

; Prepare a few things ahead of time for creating plot
; PlotName = output_dir+"AnEn_time_series_"+site(ss)+"_gen"+gen_date+"_"+cycle_str
; PlotName = output_dir+"d3_ts_"+site(ss)+"_"+cycle_str
  PlotName = output_dir+"d3_ts_"+site(ss)+""+meas
  ;PlotType@wkWidth = "1048"
  ;PlotType@wkHeight = "1048"
  wks = gsn_open_wks(PlotType,PlotName)
  gsn_define_colormap(wks,"default")
  plot = new(2,"graphic")
  plot_polyline = new((/2,5/),"graphic")         ; these graphic arrays are given a rightmost dimension size of 5 in case any
  ;plot_spag_lines = new((/2,n_mem,5/),"graphic") ;  of the lines are broken by missing data, resulting in multiple lines
  plot_obs_polyline = new((/2,5/),"graphic")     ; Same goes for here

; Loop through the two fields
  do ff=0,n_fields-1
   cur_anen_input = anen_inputs(ff)
   min_max_threshold = diff_threshold(ff)
   date_index = cur_anen_input@date_index
   if (date_index.lt.0) then
     continue
   end if
   last_date = cur_anen_input@last_date
  
; Read in latest data file for this cycle
   in_file = cur_anen_input
   ;in_file = in_base_dir+"/veri_pair.anen/ANEN."+fields(ff)+"."+cycle_str+".nc"
   
   if (opt_cache .ne. 0) then
    if (ff.eq.0) then
     ncdf_in = temp_nc
     this_field = temp_field
    else if (ff.eq.1) then
     ncdf_in = wind_nc
     this_field = wind_field
    else
     ncdf_in = addfile(in_file,"r")
     if (date_index .ge. 0) then
      this_field = ncdf_in->AnEn(:,:(n_lead_times-1),date_index,:) ; file array dimensions::(NMEMBERS, NLEADTIMES, NDAYS, NSTATIONS)
      this_field = scale_factor(ff)*this_field+offset(ff)
     else
      this_field = ncdf_in->AnEn(:,:(n_lead_times-1),0,:) ; file array dimensions::(NMEMBERS, NLEADTIMES, NDAYS, NSTATIONS)
      this_field = ncdf_in->AnEn@_FillValue
     end if
    end if
    end if
   else
    ncdf_in = addfile(in_file,"r")
    if (date_index .ge. 0) then
     this_field = ncdf_in->AnEn(:,:(n_lead_times-1),date_index,:) ; file array dimensions::(NMEMBERS, NLEADTIMES, NDAYS, NSTATIONS)
     this_field = scale_factor(ff)*this_field+offset(ff)
    else
     this_field = ncdf_in->AnEn(:,:(n_lead_times-1),0,:) ; file array dimensions::(NMEMBERS, NLEADTIMES, NDAYS, NSTATIONS)
     this_field = ncdf_in->AnEn@_FillValue
    end if
   end if
   
   dim_names = getfilevardims(ncdf_in,"AnEn")
   dim_sizes = filevardimsizes(ncdf_in,"AnEn")
;   ndays = dim_sizes(ind(dim_names.eq."NDAYS"))
   n_members = dim_sizes(ind(dim_names.eq."NMEMBERS"))
   ;H n_lead_times = dim_sizes(ind(dim_names.eq."NLEADTIMES"))
;  sample_days = getfilevaratts(ncdf_in,"NDAYS_ORIG")
   sample_days = ncdf_in@NDAYS_ORIG
   ;if (n_members.ne.n_mem) then
   ; print("The number of ensemble members is not the same as the default. Please check parameters at top of script.")
   ;end if
   
   if (date_index .ge. 0) then
    missing_count = num(ismissing(this_field(:,:,ss)))
    if (missing_count .gt. 0) then
     valid_count = num(.not.ismissing(this_field(:,:,ss)))
     if (valid_count.eq.0) then
      print(P_WARN + fields(ff) + "(" + site(ss) + ") No valid data" + " from " + in_file)
     else
      print(P_INFO + fields(ff) + "(" + site(ss) + ") AnEn missing=" + missing_count+", valid=" + valid_count + " from " + in_file)
      lead_index = 0
      do while(lead_index.lt.n_lead_times)
       missing_count_lead = num(ismissing(this_field(:,lead_index,ss)))
       if (missing_count_lead.eq.n_members) then
         print(P_INFO + fields(ff) + "(" + site(ss) + ") No valid data for lead time " + lead_index)
       end if
       lead_index = lead_index + 1
      end do
     end if
    end if
    lead_index = 0
    do while(lead_index.lt.n_lead_times)
     if (num(.not.ismissing(this_field(:,lead_index,ss))).gt.0) then
      max_per_lead = max(this_field(:,lead_index,ss))
      min_per_lead = min(this_field(:,lead_index,ss))
      diff_per_lead = max_per_lead - min_per_lead
      if (diff_per_lead.gt.min_max_threshold) then
       print(P_WARN + fields(ff) + "(" + site(ss) + ") diff(" + lead_index + ")=" + diff_per_lead + " min=" + min_per_lead + " max=" + max_per_lead)
      end if
     end if
     lead_index = lead_index + 1
    end do
   end if
   
   mean_all = dim_avg_n_Wrap(this_field,0)
   stddev_all = dim_stddev_n_Wrap(this_field,0)
   max_all = max(mean_all+stddev_all)
   min_all = min(mean_all-stddev_all)
   field2plot = this_field(:,:,ss) ; array dimensions::(NMEMBERS, NLEADTIMES, NSTATIONS)
   field2plot@_FillValue = field_missing(ff)
   field2plot_obs = field_obs(ff,:,ss)
   field2plot_obs@_FillValue = field_missing(ff)
   delete([/mean_all,stddev_all,this_field/])

; Derive data that will be shown on the plot (e.g., +/- 1 std. dev.)
   field2plot_stddev = dim_stddev_n_Wrap(field2plot,0)
   field2plot_mean   = dim_avg_n_Wrap(field2plot,0)
   field2plot_pm_stddev = new((/2,n_lead_times/),"float")
   field2plot_pm_stddev(1,:) = field2plot_mean-field2plot_stddev
   field2plot_pm_stddev(0,:) = field2plot_mean+field2plot_stddev
   field2plot_pm_stddev@_FillValue = field_missing(ff)
   if (opt_debug.ne.0) then
    print("  DEBUG field: " + fields(ff) + "(" + site(ss) + ") dimsizes mean"+dimsizes(field2plot_mean)+" dimsizes stddev "+dimsizes(field2plot_stddev))
   end if
   
; Calculate parameters for plot (e.g., min/max X/Y axis)
   Ymax = ceil(max_all/y_tick_int(ff))*y_tick_int(ff)
   if (fields(ff).eq."wspd") then
    Ymin = 0
   else
    Ymin = floor(min_all/y_tick_int(ff))*y_tick_int(ff)
   end if
   Xmax = max(lead_time)
   Xmin = min(lead_time)
  
   if (.not.all(ismissing(field2plot)) .and. current_date.ne.last_date ) then

; Set plot attributes
    res                    = True              ; plot mods desired
    res@gsnDraw            = False
    res@gsnFrame           = False             ; don't advance frame yet
    res@gsnMaximize        = True
    res@vpHeightF          = 0.3               ; change aspect ratio of plot
    res@vpWidthF           = 0.95
    ;res@gsnBoxMargin       = 0.3
    ;res@gsPaperMargin      = 0.6
    res@gsnXYFillColors    = 9
    res@xyLineColor        = -1                           ; We don't want the line, so make it transparent.
    res@trYMaxF            = Ymax
    res@trYMinF            = Ymin
    res@tmYLMode           = "Manual"
    res@tmYLTickSpacingF   = y_tick_int(ff)
    res@trXMaxF            = Xmax
    res@trXMinF            = Xmin
    res@tiMainString       = "station "+site_CAP(ss)+" ("+gen_date+cycle_str+") samples used "+sample_days
    res@tiMainFontHeightF  = 0.02
    res@tiXAxisString      = "valid time  "
    res@tiXAxisFontHeightF = 0.020
    res@tiYAxisString      = field_long_name(ff)+" ("+field_units(ff)+")"
    res@tiYAxisFontHeightF = 0.020
    res@tmYMajorGrid                = True
    res@tmYMajorGridThicknessF      = 0.7
    ;res@tmYMajorGridLineDashPattern = 2
    res@tmYLMinorPerMajor = n_minor_ticks(ff)
    res@tmYMajorGridLineColor       = 30
    res@tmYMinorGrid                = True
    res@tmYMinorGridThicknessF      = 0.1
    res@tmYMinorGridLineColor       = 30
    res@tmXMajorGrid                = True
    res@tmXMajorGridThicknessF      = 0.7
    ;res@tmXMajorGridLineDashPattern = 2
    res@tmXMajorGridLineColor       = 30
    res@tmXMinorGrid                = True
    res@tmXMinorGridThicknessF      = 0.1
    res@tmXMinorGridLineColor       = 30
    res@tmXBMinorValues             = lead_time
    res@tmXBMode   = "Explicit"
    res@tmXBValues = lead_time_X_vals
    res@tmXBLabels = lead_time_X_lbls
    res@tmXBLabelFontHeightF = 0.018
    res@tmYLLabelFontHeightF = 0.018
    res@tmYLFormat = "f"
    plot(ff) = gsn_csm_xy(wks,lead_time,field2plot_pm_stddev,res)  ; Create filled XY plot.

   ; plot all member lines
    res_line                  = True
    res_line@gsLineColor      = 2
    res_linegsLineThicknessF  = 2
    if (isvar("plot_temp")) then  ; put this in to overcome NCL bug where
     delete(plot_temp)            ;  "plot_temp" is not always deleted
    end if
    if (isvar("plot_obs_temp")) then  ; put this in to overcome NCL bug where
     delete(plot_obs_temp)            ;  "plot_obs_temp" is not always deleted
    end if
    ;do mm=0,n_mem-1
    ; plot_temp = gsn_add_polyline(wks,plot(ff),lead_time,field2plot(mm,:),res_line)
    ; if (dimsizes(plot_temp).eq.1) then
    ;  plot_spag_lines(ff,mm,0) = plot_temp
    ; else
    ;  plot_spag_lines(ff,mm,0:dimsizes(plot_temp)-1) = plot_temp
    ; end if
    ; delete(plot_temp)
    ;end do

   ; plot mean line
    res_line@gsLineColor      = 10
    res_line@gsLineThicknessF = 5
    if (isvar("plot_temp")) then  ; put this in to overcome NCL bug where 
     delete(plot_temp)            ;  "plot_temp" is not always deleted
    end if                        ;  like it should be.
    plot_temp = gsn_add_polyline(wks,plot(ff),lead_time,field2plot_mean,res_line)
    if (dimsizes(plot_temp).eq.1) then
     plot_polyline(ff,0) = plot_temp
    else
     plot_polyline(ff,0:dimsizes(plot_temp)-1) = plot_temp
    end if

   ; plot obs verification line
    if (.not.all(ismissing(field2plot_obs))) then  ; only plot obs line if obs are present
     res_line@gsLineColor      = 30
     if (isvar("plot_obs_temp")) then  ; put this in to overcome NCL bug where
      delete(plot_obs_temp)            ;  "plot_temp" is not always deleted
     end if                            ;  like it should be.
     plot_obs_temp = gsn_add_polyline(wks,plot(ff),lead_time,field2plot_obs,res_line)
     if (dimsizes(plot_obs_temp).eq.1) then
      plot_obs_polyline(ff,0) = plot_obs_temp
     else
      plot_obs_polyline(ff,0:dimsizes(plot_obs_temp)-1) = plot_obs_temp
     end if
    else
     plot_obs_temp = new(1,"graphic") ; if all obs are missing, just create blank graphic
    end if

   ; plot key
    txres               = True                      ; text mods desired
    txres@txFontHeightF = 0.012
    txres@txFontColor   = 10
    gsn_text_ndc(wks,"dark blue = mean",.93,.985,txres)
    txres@txFontColor   = 6
    gsn_text_ndc(wks,"cyan = +/-1 stddev",.93,.96,txres)
    txres@txFontColor   = 31
    gsn_text_ndc(wks,"gray = observed",.93,.935,txres)
    delete([/res,res_line,plot_temp,plot_obs_temp/])
   end if
   delete([/dim_names,dim_sizes,field2plot,field2plot_stddev,field2plot_mean,field2plot_pm_stddev/])
  end do
  
  resP = True
  resP@gsnPanelYWhiteSpacePercent = 5
  if (.not.all(ismissing(plot))) then
   gsn_panel(wks,plot,(/2,1/),resP)
   print("Created plot: "+PlotName+"."+PlotType)
  else
   print("All missing data for station: "+site(ss)+"; no plot created.")
  end if
  delete([/wks,plot,plot_polyline,plot_obs_polyline/])

; Create the plotting resources ahead of time
  plot = new(2,"graphic")
  plot_ovrly = new(2,"graphic")
  plot_obs_polyline = new((/2,5/),"graphic")     ; Same goes for here
;  PlotName = output_dir+"AnEn_box_whisker_"+site(ss)+"_gen"+gen_date+"_"+cycle_str
; PlotName = output_dir+"d3_bw_"+site(ss)+"_"+cycle_str
  PlotName = output_dir+"d3_bw_"+site(ss)+""+meas
  wks = gsn_open_wks(PlotType,PlotName)
  gsn_define_colormap(wks,"default")

; Loop through the two fields
  do ff=0,n_fields-1
   cur_anen_input = anen_inputs(ff)
   date_index = cur_anen_input@date_index
   if (date_index.lt.0) then
     continue
   end if
   last_date = cur_anen_input@last_date
  
; Read in latest data file for this cycle
   in_file = cur_anen_input
   if (opt_cache .ne. 0) then
  
    if (ff.eq.0) then
     ncdf_in = temp_nc
     this_field = temp_field
    else if (ff.eq.0) then
     ncdf_in = wind_nc
     this_field = wind_field
    else
     ncdf_in = addfile(in_file,"r")
     if (date_index .ge. 0) then
      this_field = ncdf_in->AnEn(:,:(n_lead_times-1),date_index,:) ; file array dimensions::(NMEMBERS, NLEADTIMES, NDAYS, NSTATIONS)
      this_field = scale_factor(ff)*this_field+offset(ff)
     else
      this_field = ncdf_in->AnEn(:,:(n_lead_times-1),0,:) ; file array dimensions::(NMEMBERS, NLEADTIMES, NDAYS, NSTATIONS)
      this_field = ncdf_in->AnEn@_FillValue
     end if
    end if
    end if
   else
    ncdf_in = addfile(in_file,"r")
    if (date_index .ge. 0) then
     this_field = ncdf_in->AnEn(:,:(n_lead_times-1),date_index,:) ; file array dimensions::(NMEMBERS, NLEADTIMES, NDAYS, NSTATIONS)
     this_field = scale_factor(ff)*this_field+offset(ff)
    else
     this_field = ncdf_in->AnEn(:,:(n_lead_times-1),0,:) ; file array dimensions::(NMEMBERS, NLEADTIMES, NDAYS, NSTATIONS)
     this_field = ncdf_in->AnEn@_FillValue
    end if
   end if
   
   dim_names = getfilevardims(ncdf_in,"AnEn")
   dim_sizes = filevardimsizes(ncdf_in,"AnEn")
   ndays = dim_sizes(ind(dim_names.eq."NDAYS"))
;  sample_days = getfilevaratts(ncdf_in,"NDAYS_ORIG")
   sample_days = ncdf_in@NDAYS_ORIG
   n_members = dim_sizes(ind(dim_names.eq."NMEMBERS"))
   ;if (n_members.ne.n_mem) then
   ; print("The number of ensemble members is not the same as the default. Please check parameters at top of script.")
   ;end if
   
   field2plot = this_field(:,:,ss) ; file array dimensions::(NMEMBERS, NLEADTIMES, NSTATIONS)
   field2plot@_FillValue = field_missing(ff)
   field2plot_obs = field_obs(ff,:,ss)
   field2plot_obs@_FillValue = field_missing(ff)

; Derive data that will be shown on the plot (e.g., +/- 1 std. dev.)
   field2plot_4boxplot = new((/n_lead_times,5/),"float")
   ;field2plot_4boxplot(:,0) = -2.
   ;field2plot_4boxplot(:,1) = -1.
   ;field2plot_4boxplot(:,2) = 0.
   ;field2plot_4boxplot(:,3) = 1.
   ;field2plot_4boxplot(:,4) = 2.
   do tt=0,n_lead_times-1
    field2plot_sub = field2plot(:,tt)
    field2plot_sub@_FillValue = field2plot@_FillValue
    if (.not.all(ismissing(field2plot_sub))) then
     qsort(field2plot_sub)
     miss_ind = ind(ismissing(field2plot_sub))
     if (.not.all(ismissing(miss_ind))) then
      n_miss_ind = dimsizes(miss_ind)
     else
      n_miss_ind = 0
     end if
     frst_quart_ind = n_miss_ind+round(tofloat(n_members-n_miss_ind)*0.25,3)-1
     scnd_quart_ind = n_miss_ind+round(tofloat(n_members-n_miss_ind)*0.5,3)-1
     thrd_quart_ind = n_miss_ind+round(tofloat(n_members-n_miss_ind)*0.75,3)-1
     field2plot_4boxplot(tt,0) = min(field2plot_sub)        ; min
     field2plot_4boxplot(tt,1) = field2plot_sub(frst_quart_ind) ; 1st quartile limit
     field2plot_4boxplot(tt,2) = field2plot_sub(scnd_quart_ind) ; 2nd quartile limit (i.e., median)
     field2plot_4boxplot(tt,3) = field2plot_sub(thrd_quart_ind) ; 3rd quartile limit
     field2plot_4boxplot(tt,4) = max(field2plot_sub)        ; max
     ;print(field2plot_sub)
     ;print(field2plot_4boxplot(tt,:))
     ;print(nada)
     delete(miss_ind)
    end if
    delete(field2plot_sub)
   end do

; Calculate parameters for plot (e.g., min/max X/Y axis)
   Ymax = ceil(max(this_field)/y_tick_int(ff))*y_tick_int(ff)
   if (fields(ff).eq."wspd") then
    Ymin = 0.
   else
    Ymin = floor(min(this_field)/y_tick_int(ff))*y_tick_int(ff)
   end if
   Xmax = max(lead_time)
   Xmin = min(lead_time)
   delete(this_field)

;If last_date which is the last date in the ANEN file is not equal to the last date in the obs file
;then don't plot the AnEn date. This assumes the obs are more reliable or that the AnEn is not worthwhile
;without obs
   if (.not.all(ismissing(field2plot)) .and. current_date.ne.last_date ) then

; Prepare box and whiskers plots
    res            = True
    ;res@gsnDraw            = False
    ;res@gsnFrame           = False             ; don't advance frame yet
    res@gsnMaximize        = True
    res@vpHeightF          = 0.4               ; change aspect ratio of plot
    res@vpWidthF           = 0.95
    ;res@gsnXYFillColors    = 9
    ;res@xyLineColor        = -1                           ; We don't want the line, so make it transparent.
    res@trYMaxF            = Ymax
    res@trYMinF            = Ymin
    res@trXMaxF            = Xmax+0.5
    res@trXMinF            = Xmin-0.5
    res@tiMainString       = "station "+site_CAP(ss)+" ("+gen_date+cycle_str+") samples used "+sample_days
    res@tiMainFontHeightF  = 0.02
    res@tiXAxisString      = "valid time"
    res@tiXAxisFontHeightF = 0.02
    res@tiYAxisString      = field_long_name(ff)+" ("+field_units(ff)+")"
    res@tiYAxisFontHeightF = 0.02
    res@tmXBLabelFontHeightF = 0.01
    ;res@tmXBMinorPerMajor = 1
    ;res@tmXBMode          = "Manual"
    ;res@tmXBTickEndF      = max(lead_time)
    ;res@tmXBTickSpacingF  = 2
    ;res@tmXBTickStartF    = min(lead_time)
    res@tmYMajorGrid                = True
    res@tmYMajorGridThicknessF      = 0.1
    ;res@tmYMajorGridLineDashPattern = 2
    res@tmYMajorGridLineColor       = 30
    res@tmXMajorGrid                = True
    res@tmXMajorGridThicknessF      = 0.1
    ;res@tmXMajorGridLineDashPattern = 2
    res@tmXMajorGridLineColor       = 30
    res@tmXBMode   = "Explicit"
    ;res@tmXBValues = lead_time_X_vals
    ;res@tmXBLabels = lead_time_X_lbls
    res@tmXBValues = lead_time-1  ; this adapts for a bug; the following line should be used instead when/if this bug is fixed
    ;res@tmXBValues = lead_time
    res@tmXBLabels = all_dates_lbls_bw
    res@tmXBLabelAngleF = 90.
    ;res@tmXBLabelJust  = "TopCenter"
    ;res@tmXBLabelStride   = 4
    res@tmXTOn      = True
    res@tmXTBorderOn = True
    res@tmYROn      = True
    res@tmYRBorderOn = True
    res@tmYLLabelFontHeightF = 0.018
    res@tmXBMinorValues             = lead_time
    res@tmYLMode           = "Manual"
    res@tmYLTickSpacingF   = y_tick_int(ff)
    res@tmYLFormat = "f"

    llres                   = True
    llres@gsLineThicknessF  = 4.5
    llres@gsLineDashPattern = 0
    opti          = True
    opti@boxWidth = 0.7
    blues = new(n_lead_times,"string")
    blues = 4
    opti@boxColors = blues

    plot(ff) = boxplot(wks,lead_time,field2plot_4boxplot,opti,res,llres)

   ; overlay obs verification line
    if (.not.all(ismissing(field2plot_obs))) then  ; only plot obs line if obs are present
     res_line                  = True
     res_line@gsLineColor      = 30
     res_line@gsLineThicknessF = 5
     if (isvar("plot_obs_temp")) then  ; put this in to overcome NCL bug where
      delete(plot_obs_temp)            ;  "plot_temp" is not always deleted
     end if                            ;  like it should be.
     plot_obs_temp = gsn_add_polyline(wks,plot(ff),lead_time,field2plot_obs,res_line)
     if (dimsizes(plot_obs_temp).eq.1) then
      plot_obs_polyline(ff,0) = plot_obs_temp
     else
      plot_obs_polyline(ff,0:dimsizes(plot_obs_temp)-1) = plot_obs_temp
     end if
     delete(res_line)
    else
     plot_obs_temp = new(1,"graphic")
    end if
    delete([/res,llres,opti,blues,plot_obs_temp/])

    txres               = True                      ; text mods desired
    txres@txFontHeightF = 0.012
    txres@txFontColor   = 4
    gsn_text_ndc(wks,"blue = AnEn quartiles",.91,.99,txres)
    txres@txFontColor   = 30
    gsn_text_ndc(wks,"gray = observed",.91,.97,txres)
   end if
   delete([/dim_names,dim_sizes,field2plot,field2plot_4boxplot/])
  end do

  resP = True
  resP@gsnPanelYWhiteSpacePercent = 5
  if (.not.all(ismissing(plot))) then
   gsn_panel(wks,plot,(/2,1/),resP)
   print("Created plot: "+PlotName+"."+PlotType)
  else
   print("All missing data for station: "+site(ss)+"; no plot created.")
  end if
  delete([/wks,plot,plot_obs_polyline/])

 end do

end

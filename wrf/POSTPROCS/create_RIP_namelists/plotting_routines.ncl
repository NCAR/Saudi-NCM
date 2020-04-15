;************************************************************
; Functions that create the appropriate lines of RIP output, 
; given input for each plot type.  Used with create_RIP.ncl
; Created by Joe Grim (grim@ucar.edu) July 2010
; Last edited 26 May 2011
;************************************************************

;************************************************************
; The following function returns the line(s) needed for
; setting the color scales.  Unless the color scale is 
; "Cust"om or "Uniform" (defined in their color scale name
; in color_names.asc), the colors are evenly distributed 
; between the max and min values.  The "Cust" scales include 
; the possibility of using shading intervals that are
; uneven.  We have also added the option to use a "Uniform"
; color scale, where the colors don't depend on the min
; and max shaded value.  "Uniform" scales are used for
; consistent color between multiple upper levels
;************************************************************
undef ("get_color_table")
function get_color_table(clr_tbl:string,sha_max:string,sha_min:string)
local data, ii, color_list, n_colors, colors, sh_nu, color_line, color_lines, comma_inds, color_chars
begin
; Read in all the color scales and find the right one,
; then get a list of all the colors and/or values
 data = asciiread("color_names.asc",-1,"string")
 do ii=4,dimsizes(data)-1
  if (str_strip(str_get_field(data(ii),1,";")).eq.clr_tbl) then
   colors = str_get_field(data(ii),2,";")
   n_colors = str_fields_count(colors,",")
   color_list = new(n_colors,"string")
   do jj=0,n_colors-1
    color_list(jj) = str_strip(str_get_field(colors,jj+1,","))
   end do
  end if
 end do

; If the color scale is "Cust"om, then determine the values needed
; for each interval in the color scale.  If the min/max values in
; the custom scale are different than the min/max values for the 
; shaded field, then the values are linearly interpolated by adding
; the difference between the minimum defined shading value and the
; minimum defined value in the custom color scale.  Then create the
; RIP color line for the range of values and colors.
 if (isStrSubset(clr_tbl,"Cust")) then
  color_line = "   cosq="
  do ii=0,n_colors-1
   if (ii.eq.0) then
    min_val = stringtofloat(color_list(ii))
   end if
   if (mod(ii,2).eq.1) then
    color_line = color_line+color_list(ii)+","
   else
         ; Multiply each interval by 10000, convert it to an integer, divide it by 10000 and convert back to a
         ; float to make intervals have a maximum of 4 digits after the decimal place. Same elsewhere in fx.
    sha_val = int2flt(floattoint(10000.*(stringtofloat(sha_min)-min_val+stringtofloat(color_list(ii)))))/10000.
    color_line = color_line+sha_val+","
   end if
  end do
 end if

; If the color scale is "Uniform" (e.g., upper air T plots), determine
; the max and min values needed in the plot (must be evenly divisible
; by the largest contour interval for any domain).  Then create the
; RIP color line for the range of values and colors.
 if (isStrSubset(clr_tbl,"Uniform")) then
  colors_cha = stringtochar(colors+",")
  sta_ind = indStrSubset(colors,(sha_min+","))
  end_ind = indStrSubset(colors,(","+sha_max+","))
  last_end_ind = end_ind(dimsizes(end_ind-1)-1)
  comma = stringtochar(",")
  comma1 = comma(0)
  comma_ind = ind(colors_cha.eq.comma1)
  comma_ind2 = ind(comma_ind.gt.last_end_ind)
  final_end_ind = comma_ind(comma_ind2(0))
  color_line = "   cosq="+chartostring(colors_cha(sta_ind(0):final_end_ind))
 end if

; If the color scale is neither "Uniform" or "Cust"om, just linearly 
; interpolate the values between the max and min values.  Then create 
; the RIP color line for the range of values and colors.
 if (.not.isStrSubset(clr_tbl,"Uniform").and.(.not.isStrSubset(clr_tbl,"Cust"))) then
  sh_nu = int2flt(floattoint(10000.*fspan(stringtofloat(sha_min),stringtofloat(sha_max),n_colors)))/10000.
  color_line = "   cosq="
  do ii=0,n_colors-1
   color_line = color_line+sh_nu(ii)+","+color_list(ii)+","
  end do
 end if

; Remove final comma from color_line
 color_temp = stringtochar(color_line)
 delete(color_line)
 color_line = chartostring(color_temp(0:dimsizes(color_temp)-3))
 delete(color_temp)

; Divide the single (sometimes very long) color line into multiple
; color lines so that none of the color lines are more than 75
; characters long
 color_lines = color_line
 color_chars = stringtochar(color_line)
 n_color_chars = dimsizes(color_chars)
 if (n_color_chars.ge.75) then
  loop_ctrl_var = 1
  ctr = 0
 else
  loop_ctrl_var = 0
 end if
 do while (loop_ctrl_var.eq.1)
  comma_inds  = str_index_of_substr(color_line,",",0)
  comma_inds2 = ind(comma_inds.lt.74)
  n_comma_inds2 = dimsizes(comma_inds2)
  ending_ind = comma_inds(comma_inds2(n_comma_inds2-1))
  color_lines(ctr) = chartostring(color_chars(0:ending_ind))+">"
  color_line = "   "+chartostring(color_chars(ending_ind+1:))
  color_temp = color_lines
  delete(color_lines)
  color_lines = array_append_record(color_temp,color_line,0)
  delete(color_temp)
  delete(color_chars)
  delete(comma_inds)
  delete(comma_inds2)
  color_chars = stringtochar(color_line)
  n_color_chars = dimsizes(color_chars)
  ctr = ctr + 1
  if (n_color_chars.le.75) then
   loop_ctrl_var = 0
  end if
 end do
 return(color_lines)

end

;************************************************************
; Creates the header section of each plot
;************************************************************
undef ("get_descrip")
function get_descrip(plot_name:string)
local output_lines
begin

 output_lines = new(3,"string")
 output_lines(0) = "#"
 output_lines(1) = "# "+plot_name
 output_lines(2) = "#"
 return(output_lines)

end

;************************************************************
; Creates Range boundary(s) overlays for each plot
;************************************************************
undef ("Overlay_Boundaries")
function Overlay_Boundaries(range_location:string)
local new_lines
begin

 new_lines = new(4,"string")
 new_lines(0) = "feld=map; ptyp=hb; ouds=solid; oulw=4; colr=gray2;>"
 new_lines(1) = "   outy="+str_lower(range_location)+"_map.ascii"
 new_lines(2) = "feld=map; ptyp=hb; ouds=solid; oulw=2; colr=gray8;>"
 new_lines(3) = "   outy="+str_lower(range_location)+"_map.ascii"
 return(new_lines)

end

;************************************************************
; Creates county overlays for each plot
;************************************************************
undef ("Overlay_Counties")
function Overlay_Counties()
local new_line
begin

 new_line = "feld=map; ptyp=hb; ouds=solid; oulw=1; colr=gray5; outy=us_county.ascii"
 return(new_line)

end

;************************************************************
; Creates station overlays for each plot
;************************************************************
; The bullet is the station location, and the 2- to 4-character
; name is offset by a fraction of a degree longitude, based on
; the domain and model grid size
undef ("Overlay_Stations")
function Overlay_Stations(station_overlay_info:string,hh:integer,lbl_offset:float)
local n_sta,jj,sta_id,sta_lat,sta_lon,lbl_lon,temp,new_lines,lbl_lon_offset,offset_const1,offset_const2,n_char
begin

 offset_const1 = 0.00010
 offset_const2 = 0.00023
 n_sta = dimsizes(station_overlay_info)
 sta_id = str_get_field(station_overlay_info,1,";")
 sta_lat = str_get_field(str_get_field(station_overlay_info,2,";"),1,",")
 sta_lon = str_get_field(str_get_field(station_overlay_info,2,";"),2,",")
 n_char = int2flt(dimsizes(stringtochar(sta_id))-1)
 lbl_lon_offset = (offset_const1+offset_const2*n_char)*lbl_offset
 lbl_lon = str_strip(sprintf("%9.4f",stringtofloat(sta_lon)+lbl_lon_offset))
 if (sta_id.ne."*") then
  all_lines = new(2,"string")
  all_lines(0) = "feld=bullet; ptyp=hb; linw=2; crsa="+sta_lat+"lat,"+lbl_lon+"lon; titl="+sta_id+"; tslb=0.0120"
  all_lines(1) = "feld=bullet; ptyp=hb; linw=2; crsa="+sta_lat+"lat,"+sta_lon+"lon; tslb=0.0100"
 else
  all_lines = "feld=bullet; ptyp=hb; linw=2; crsa="+sta_lat+"lat,"+sta_lon+"lon; tslb=0.0100"
 end if
 return(all_lines)

end

;************************************************************
; Creates the shaded field in a plot, along with its outline
; contour
;************************************************************
undef ("Shade")
function Shade(shaded:string,sha_int:string,sha_max:string,sha_min:string,plt_vcor_sha:string,plt_lev_sha:string,plt_sm_sha:string,clr_tbl:string,extra_line:string,hh:integer,hide:string,clrbar:string,sha_lbl_int:string,abv:string,HB:string)
local first_lines,color_lines,colors,output_lines,n_colors,temp,sha_tech,lb_phrase,dmn
begin

; This section checks to see if the plot is of terrain height.  If so, don't contour
; the outline of the shaded terrain height
 if (shaded.eq."ter") then
  sha_tech = "fill"
 else
  sha_tech = "both"
 end if

; This section checks to see if certain shaded plots with contour intervals that change with domain
; need contour interval labels
 if (sha_lbl_int.eq."M") then
  lb_phrase = " nolb;"
 else
  lb_phrase = " pwlb=2; mjsk="+sha_lbl_int+";"
 end if

; This section writes out the first line of the shading definition
; It also checks to see if the plot has an uneven contour interval (e.g., 1-hr rain rate).
; If so, it indicates this by using "mult" in the output line
 first_lines = "feld="+shaded+"; ptyp=hc; cmth="+sha_tech+"; cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; smth="+plt_sm_sha+hide+"; hvbr="+clrbar+"; nohl;"+lb_phrase+">"
 if (shaded.eq."rtot1h") then
  if (abv.eq."Pre1hr".or.abv.eq."Pre1hrZ".or.abv.eq."Pre1hrZ2") then
   first_lines = "feld="+shaded+"; ptyp=hc; cmth="+sha_tech+"; cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; smth="+plt_sm_sha+"; mult"+hide+"; hvbr="+clrbar+"; nohl;"+lb_phrase+">"
  end if
  if (abv.eq."Pref0ac") then
   if (HB.eq."-1") then
    first_lines = "feld="+shaded+"; ptyp=hc; cmth="+sha_tech+"; cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; smth="+plt_sm_sha+"; mult"+hide+"; hvbr="+clrbar+"; nohl;"+lb_phrase+">"
   else
    first_lines = "feld=rtotFCLEADHRS; ptyp=hc; cmth="+sha_tech+"; cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; smth="+plt_sm_sha+"; mult"+hide+"; hvbr="+clrbar+"; nohl;"+lb_phrase+">"
   end if
  end if
 end if
 if (shaded.eq."SNOWH") then
  if (abv.eq."Snow1hr") then
   first_lines = "feld="+shaded+"; diff=-1,rel; ptyp=hc; cmth="+sha_tech+"; cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; smth="+plt_sm_sha+"; mult"+hide+"; hvbr="+clrbar+"; nohl;"+lb_phrase+">"
  end if
  if (abv.eq."Snowf0ac") then
   first_lines = "feld="+shaded+"; diff="+HB+",rel; ptyp=hc; cmth="+sha_tech+"; cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; smth="+plt_sm_sha+"; mult"+hide+"; hvbr="+clrbar+"; nohl;"+lb_phrase+">"
  end if
 end if

; If the vertical coordinate of the shaded field is not missing, list the vertical coordinate parameter line.
; Then append this line onto the end of the shading line.
 if (plt_vcor_sha.ne."MMM") then
  second_line = "   vcor="+plt_vcor_sha+"; levs="+plt_lev_sha+";>"
  temp = first_lines
  delete(first_lines)
  first_lines = array_append_record(temp,second_line,0)
  delete(temp)
  delete(second_line)
 end if

; Get the color line(s) from the get_color_table function and append those onto the end of the shading lines
 color_lines = get_color_table(clr_tbl,sha_max,sha_min)
 output_lines = array_append_record(first_lines,color_lines,0)
 next_lines = new(2,"string")

; If an extra line is to be drawn to indicate a particular level (e.g., 0 C temperature line), then
; append the appropriate lines onto the shading lines
 if (extra_line.ne."MMM") then
  next_lines(0) = "feld="+shaded+"; ptyp=hc; linw=5; cint=2; cbeg="+extra_line+"; cend="+extra_line+"; colr=red6;>"
  next_lines(1) = "   dash=21; smth="+plt_sm_sha+"; nolb; nohl; nmsg; nttl; pwlb=0; tslb=.015; nmsg; nttl"
  temp = output_lines
  delete(output_lines)
  output_lines = array_append_record(temp,next_lines,0)
 end if
 return(output_lines)

end

;************************************************************
; Creates the contoured (non-shaded) field in a plot
;************************************************************
undef ("Contour")
function Contour(contoured:string,ctr_int:string,plt_vcor_con:string,plt_lev_con:string,plt_sm_con:string,hide:string)
local output_lines
begin

 output_lines = new(2,"string")
; If the vertical coordinate of the contoured field is not missing, include the vertical coordinate parameters.
 if (plt_vcor_con.ne."MMM") then
  output_lines(0) = "feld="+contoured+"; ptyp=hc; cmth=cont; cint="+ctr_int+"; smth="+plt_sm_con+"; vcor="+plt_vcor_con+"; levs="+plt_lev_con+";>"
 else
  output_lines(0) = "feld="+contoured+"; ptyp=hc; cmth=cont; cint="+ctr_int+"; smth="+plt_sm_con+";>"
 end if
 output_lines(1) = "   linw=3; colr=blue8; nohl; pwlb=0; tslb=0.015; nsmm; mjsk=0"+hide 
 return(output_lines)

end

;************************************************************
; Creates the dashed field in a plot
;************************************************************
undef ("Dash")
function Dash(dashed:string,dsh_int:string,plt_vcor_dsh:string,plt_lev_dsh:string,plt_sm_dsh:string,hghlt_line:string,hide:string)
local output_lines
begin

 output_lines = new(2,"string")
; If the vertical coordinate of the dashed field is not missing, include the vertical coordinate parameters.
 if (plt_vcor_dsh.ne."MMM") then
  output_lines(0) = "feld="+dashed+"; ptyp=hc; cmth=cont; cint="+dsh_int+"; smth="+plt_sm_dsh+"; vcor="+plt_vcor_dsh+"; levs="+plt_lev_dsh+";>"
 else
  output_lines(0) = "feld="+dashed+"; ptyp=hc; cmth=cont; cint="+dsh_int+"; smth="+plt_sm_dsh+";>"
 end if
 output_lines(1) = "   linw=2; colr=red5; dash=21; nohl; pwlb=0; tslb=0.013; nsmm; mjsk=0"+hide

; If an extra line is to be drawn to indicate a particular level (e.g., 5400 m 1000-500 mb height), then
; append the appropriate lines onto the shading lines
 if (hghlt_line.ne."999") then
  next_lines = new(2,"string")
  next_lines(0) = "feld="+dashed+"; ptyp=hc; cmth=cont; cint=2; cbeg="+hghlt_line+"; cend="+hghlt_line+"; smth="+plt_sm_dsh+";>"
  next_lines(1) = "   linw=4; colr=red6; dash=21; nohl; pwlb=0; tslb=0.013; nsmm; mjsk=0; nmsg; nttl"
  temp = output_lines
  delete(output_lines)
  output_lines = array_append_record(temp,next_lines,0)
  delete(next_lines)
 end if
 return(output_lines)

end

;************************************************************
; Creates the wind barb/streamline field in a plot
;************************************************************
undef ("Wind")
function Wind(wnd_field:string,KtsorMS:string,vec_type:string,hh:integer,brb_density:string)
local output_lines,brb_den
begin

; If the vector type is barb, then plot barbs; otherwise, plot streamlines
 if (vec_type.eq."Brb") then
  ptyp = "hv"
 else
  ptyp = "hs"
 end if
 brb_den=str_get_field(brb_density,(hh+1),",") 
 output_lines = new(2,"string")
 output_lines(0) = "feld="+wnd_field+"; ptyp="+ptyp+"; vcmx=-1.5; fulb="+KtsorMS+"; linw=1; smth=0; intv="+brb_den+";>"
 output_lines(1) = "   colr=black"
 return(output_lines)

end

;************************************************************
; Creates the raster field in a plot
;************************************************************
undef ("Raster")
function Raster(shaded:string,sha_int:string,sha_max:string,sha_min:string,plt_vcor_sha:string,plt_lev_sha:string,plt_sm_sha:string,clr_tbl:string,clrbar:string)
local first_lines,color_lines,colors,output_lines,n_colors,temp
begin

; This first section checks to see if the plot has an uneven contour interval (so far, only 1-hr rain rate)
 if (shaded.eq."rtot1h".or.shaded.eq."SNOWH") then
  first_lines = "feld="+shaded+"; ptyp=hc; cmth=cell; cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; smth="+plt_sm_sha+"; hvbr="+clrbar+"; mult; nohl;>"
 else
  first_lines = "feld="+shaded+"; ptyp=hc; cmth=cell; cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; smth="+plt_sm_sha+"; hvbr="+clrbar+";>"
 end if
; If the vertical coordinate of the raster field is not missing, include the vertical coordinate parameters.
 if (plt_vcor_sha.ne."MMM") then
  second_line = "   vcor="+plt_vcor_sha+"; levs="+plt_lev_sha+";>"
  temp = first_lines
  delete(first_lines)
  first_lines = array_append_record(temp,second_line,0)
  delete(temp)
 end if
 color_lines = get_color_table(clr_tbl,sha_max,sha_min)
 output_lines = array_append_record(first_lines,color_lines,0)
 return(output_lines)

end

;************************************************************
; Creates the shaded XS field in a plot, along with its 
; outline contour.
;************************************************************
undef ("Shade_XS")
function Shade_XS(shaded:string,sha_int:string,sha_max:string,sha_min:string,plt_vcor_sha:string,plt_sm_sha:string,clr_tbl:string,ext_line:string,end_pts:string,clrbar:string,sha_lbl_int:string,plt_lev_sha:string)
local first_lines,color_lines,colors,output_lines,temp,XS_lat_beg,XS_lon_beg,XS_lat_end,XS_lon_end,next_lines,lb_phrase
begin

; This section checks to see if certain shaded plots with contour intervals that change with domain
; need contour interval labels
 if (sha_lbl_int.eq."M") then
  lb_phrase = " nolb;"
 else
  lb_phrase = " pwlb=2; mjsk="+sha_lbl_int+";"
 end if

; Checks to see if a max and min shaded value is needed for certain fields
 if (shaded.eq."www") then
  sha_max = sprinti("%0.1i",5*stringtointeger(sha_int))
  sha_min = sprinti("%0.1i",-5*stringtointeger(sha_int))
 end if

; Extracts the XS end points
 XS_lat_beg = str_strip(str_get_field(str_get_field(end_pts,1,">"),1,","))
 XS_lon_beg = str_strip(str_get_field(str_get_field(end_pts,1,">"),2,","))
 XS_lat_end = str_strip(str_get_field(str_get_field(end_pts,2,">"),1,","))
 XS_lon_end = str_strip(str_get_field(str_get_field(end_pts,2,">"),2,","))

; Creates the shaded field lines and determines if a top and bottom of the XS
; is required; otherwise, it uses 0 and 12 km
 first_lines = new(3,"string")
 first_lines(0) = "feld="+shaded+"; ptyp=vc; cmth=both; cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; smth="+plt_sm_sha+";>"
 if (plt_lev_sha.ne."MMM") then
  first_lines(1) = "   vcor="+plt_vcor_sha+"; vwin="+plt_lev_sha+"; nohl; hvbr="+clrbar+";"+lb_phrase+">"
 else
  first_lines(1) = "   vcor="+plt_vcor_sha+"; vwin=0.,12.; nohl; hvbr="+clrbar+";"+lb_phrase+">"
 end if
 first_lines(2) = "   crsa="+XS_lat_beg+"lat,"+XS_lon_beg+"lon; crsb="+XS_lat_end+"lat,"+XS_lon_end+"lon;>"
 color_lines = get_color_table(clr_tbl,sha_max,sha_min)
 output_lines = array_append_record(first_lines,color_lines,0)

; Adds the possibility of adding an extra line (e.g., 0C level)
 if (ext_line.ne."MMM") then
  next_lines = new(3,"string")
  next_lines(0) = "feld="+shaded+"; ptyp=vc; linw=2; cint=2; cbeg="+ext_line+"; cend="+ext_line+"; colr=red;>"
  next_lines(1) = "   crsa="+XS_lat_beg+"lat,"+XS_lon_beg+"lon; crsb="+XS_lat_end+"lat,"+XS_lon_end+"lon;>"
  next_lines(2) = "   smth="+plt_sm_sha+"; nolb; orlb=2; nohl; nmsg; nttl; pwlb=0; tslb=.015; vwin=0.,12."
  temp = output_lines
  delete(output_lines)
  output_lines = array_append_record(temp,next_lines,0)
  delete(temp)
  delete(next_lines)
 end if
 return(output_lines)

end

;************************************************************
; Creates the contoured (non-shaded) XS field in a plot
;************************************************************
undef ("Contour_XS")
function Contour_XS(contoured:string,ctr_int:string,plt_vcor_con:string,plt_sm_con:string,end_pts,plt_lev_sha:string)
local output_lines,XS_lat_beg,XS_lon_beg,XS_lat_end,XS_lon_end
begin

; Extracts the XS end points
 XS_lat_beg = str_strip(str_get_field(str_get_field(end_pts,1,">"),1,","))
 XS_lon_beg = str_strip(str_get_field(str_get_field(end_pts,1,">"),2,","))
 XS_lat_end = str_strip(str_get_field(str_get_field(end_pts,2,">"),1,","))
 XS_lon_end = str_strip(str_get_field(str_get_field(end_pts,2,">"),2,","))

; Creates the contoured field lines and determines if a top and bottom of the XS
; is required; otherwise, it uses 0 and 12 km
 output_lines = new(4,"string")
 output_lines(0) = "feld="+contoured+"; ptyp=vc; linw=2; cint="+ctr_int+"; vcor="+plt_vcor_con+"; colr=black;>"
 output_lines(1) = "   crsa="+XS_lat_beg+"lat,"+XS_lon_beg+"lon; crsb="+XS_lat_end+"lat,"+XS_lon_end+"lon;>"
 if (plt_lev_sha.ne."MMM") then
  output_lines(2) = "   smth="+plt_sm_con+"; mjsk=0; orlb=2; nohl; nmsg; nttl; pwlb=0; tslb=.015; vwin="+plt_lev_sha+";>"
 else
  output_lines(2) = "   smth="+plt_sm_con+"; mjsk=0; orlb=2; nohl; nmsg; nttl; pwlb=0; tslb=.015; vwin=0.,12.;>"
 end if

; Starts contouring at 0, with the exception of ThetaE, which starts at 220
 if (contoured.eq."the") then
  output_lines(3) = "   cbeg=220; cend=500"
 else
  output_lines(3) = "   cbeg=0; cend=200"
 end if
 return(output_lines)

end

;************************************************************
; Creates the dashed XS field in a plot
;************************************************************
undef ("Dash_XS")
function Dash_XS(dashed:string,dsh_int:string,plt_vcor_dsh:string,plt_sm_dsh:string,end_pts:string,plt_lev_sha)
local output_lines,XS_lat_beg,XS_lon_beg,XS_lat_end,XS_lon_end
begin

; Extracts the XS end points
 XS_lat_beg = str_strip(str_get_field(str_get_field(end_pts,1,">"),1,","))
 XS_lon_beg = str_strip(str_get_field(str_get_field(end_pts,1,">"),2,","))
 XS_lat_end = str_strip(str_get_field(str_get_field(end_pts,2,">"),1,","))
 XS_lon_end = str_strip(str_get_field(str_get_field(end_pts,2,">"),2,","))

; Creates the dashed field lines and determines if a top and bottom of the XS
; is required; otherwise, it uses 0 and 12 km
 output_lines = new(3,"string")
 output_lines(0) = "feld="+dashed+"; ptyp=vc; linw=2; cint="+dsh_int+"; vcor="+plt_vcor_dsh+"; colr=black;>"
 if (dashed.eq."xnt") then
  output_lines(1) = "   crsa="+XS_lat_beg+"lat,"+XS_lon_beg+"lon; crsb="+XS_lat_end+"lat,"+XS_lon_end+"lon; dash=21;cbeg=-100; cend=0;>"
 else
  output_lines(1) = "   crsa="+XS_lat_beg+"lat,"+XS_lon_beg+"lon; crsb="+XS_lat_end+"lat,"+XS_lon_end+"lon; dash=21;>"
 end if
 if (plt_lev_sha.ne."MMM") then
  output_lines(2) = "   smth="+plt_sm_dsh+"; mjsk=0; orlb=2; nohl; nmsg; nttl; pwlb=0; tslb=.015; vwin="+plt_lev_sha+""
 else
  output_lines(2) = "   smth="+plt_sm_dsh+"; mjsk=0; orlb=2; nohl; nmsg; nttl; pwlb=0; tslb=.015; vwin=0.,12."
 end if
 return(output_lines)

end

;************************************************************
; Creates the XS wind barb/arrow field in a plot
;************************************************************
undef ("Wind_XS")
function Wind_XS(wnd_field:string,KtsorMS:string,vec_type:string,end_pts:string,plt_lev_sha:string)
local output_lines,XS_lat_beg,XS_lon_beg,XS_lat_end,XS_lon_end,ptyp
begin

; Extracts the XS end points
 XS_lat_beg = str_strip(str_get_field(str_get_field(end_pts,1,">"),1,","))
 XS_lon_beg = str_strip(str_get_field(str_get_field(end_pts,1,">"),2,","))
 XS_lat_end = str_strip(str_get_field(str_get_field(end_pts,2,">"),1,","))
 XS_lon_end = str_strip(str_get_field(str_get_field(end_pts,2,">"),2,","))

; Determines the necessary vector type
 if (vec_type.eq."Brb") then
  ptyp = "vw"
 else
  ptyp = "vv"
 end if

; Creates the vector field and determines if a top and bottom of the XS
; is required; otherwise, it uses 0 and 12 km
 output_lines = new(3,"string")
 output_lines(0) = "feld="+wnd_field+"; ptyp="+ptyp+"; vcmx=-1.5; fulb="+KtsorMS+"; linw=1; smth=0; intv=4;>"
 output_lines(1) = "   crsa="+XS_lat_beg+"lat,"+XS_lon_beg+"lon; crsb="+XS_lat_end+"lat,"+XS_lon_end+"lon;>"
 if (plt_lev_sha.ne."MMM") then
  output_lines(2) = "   colr=black; vwin="+plt_lev_sha+"; vcor=z"
 else
  output_lines(2) = "   colr=black; vwin=0.,12.; vcor=z"
 end if
 return(output_lines)

end

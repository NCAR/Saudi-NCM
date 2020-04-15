load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "plotting_routines.ncl"

;************************************************************
; This program creates RIP namelist files for each season and 
; domain for a given range.  It is run from create_plots.csh
; using output from the web form display_rip_form.pl
; Created by Joe Grim (grim@ucar.edu) July 2010
; Last edited 2 Feb 2017
;************************************************************

begin

; Add possibility to do testing, but otherwise make it set up to work operationally
 testing = 0    ; 1 = set up for testing, 0 = set up for operational
 header_lines = asciiread("hdr.asc",-1,"string")
 if (testing.ne.0) then
  header_lines(3) = " ptimes="+plttime+","
  header_lines(6) = " iinittime=0,"
  temp = header_lines(0:7)
  temp2 = header_lines(9)
  delete(header_lines)
  header_lines = array_append_record(temp,temp2,0)
  delete(temp)
  delete(temp2)
  HB = "-1"
  LH = "1h"
 else
  plttime = 0
  HB = "HOURSBACK"
  LH = "FCLEADHRS"
 end if

; Some up front variables that might need updated at a later time
 poli_bdry_thick = "3"  ; political boundary line weight
 seasons = (/"summer","winter"/)
 n_seasons = dimsizes(seasons)
 end_line = "==========================================================================="
 
; Read in a data file containing the deafult settings for each plot type
; and extract the settings from each line
 plot_defs = asciiread("plot_defaults.asc",-1,"string")     ; Read in defaults file
 plot_abv = str_strip(str_get_field(plot_defs,1,";"))       ; All plot abbreviation names for all possible plots
 plot_desc = str_strip(str_get_field(plot_defs,2,";"))      ; All plot description fields  "   "      "      "
 plot_type = str_strip(str_get_field(plot_defs,3,";"))      ; All plot type fields  "   "      "      "
 plot_shaded = str_strip(str_get_field(plot_defs,4,";"))    ; All shaded fields
 plot_contoured = str_strip(str_get_field(plot_defs,5,";")) ; All contoured fields
 plot_dashed = str_strip(str_get_field(plot_defs,6,";"))    ; All Dashed fields
 plot_wind = str_strip(str_get_field(plot_defs,7,";"))      ; All wind vector fields
 plot_vcor_sha = str_strip(str_get_field(plot_defs,8,";"))  ; All vertical coordinates for shaded fields
 plot_vcor_con = str_strip(str_get_field(plot_defs,9,";"))  ; All vertical coordinates for contoured fields
 plot_vcor_dsh = str_strip(str_get_field(plot_defs,10,";")) ; All vertical coordinates for dashed fields
 plot_lev_sha = str_strip(str_get_field(plot_defs,11,";"))  ; All vertical levels for shaded fields
 plot_lev_con = str_strip(str_get_field(plot_defs,12,";"))  ; All vertical levels for contoured fields
 plot_lev_dsh = str_strip(str_get_field(plot_defs,13,";"))  ; All vertical levels for dashed fields
 plot_sm_sha = str_strip(str_get_field(plot_defs,14,";"))   ; All smoothing levels for shaded fields
 plot_sm_con = str_strip(str_get_field(plot_defs,15,";"))   ; All smoothing levels for contoured fields
 plot_sm_dsh = str_strip(str_get_field(plot_defs,16,";"))   ; All smoothing levels for dashed fields
 extra_line = str_strip(str_get_field(plot_defs,17,";"))    ; All values at which to plot an extra line (e.g., 1000-500 540 line)
 plot_vec_type = str_strip(str_get_field(plot_defs,18,";")) ; All vector types (e.g., barb, arrow, streamline)
 shade_lbl_int = str_strip(str_get_field(plot_defs,19,";")) ; Contour label interval for shaded field for each domain

; Read in the input data file containing specialized settings for each plot for each range
 form_name = "form"+range_location+".out"                   ; parameter file name for given range
 form_data = asciiread(form_name,-1,"string")               ; read in parameter file contents

; Get universal parameters and begining lines for each section
 KtsorMS = str_strip(str_get_field(form_data(2),1,";"))     ; Use 1 full barb = 10 kts or 5 m/s?
 num_doms = stringtoint(str_get_field(form_data(2),2,";"))  ; Number of domains for range
 underground = str_strip(str_get_field(form_data(2),3,";")) ; Plot "Upper Level" fields below ground level?
 clrbar_loc = str_strip(str_get_field(form_data(2),4,";"))  ; Place colorbar on right of plot or default location?
 brb_density = str_strip(str_get_field(form_data(2),5,";")) ; Get the wind barb density for each domain
 dup_snding = str_strip(str_get_field(form_data(2),6,";"))  ; Determine if each sounding needs to be duplicated w/o a Parcel Info box
 maj_tic_freq = str_strip(str_get_field(form_data(2),7,";")); The frequency for labeling major tick marks
 mnr_tic_freq = str_strip(str_get_field(form_data(2),8,";")); The frequency for making minor tick marks
 if (clrbar_loc.eq."side") then
  clrbar = "1"
 else
  clrbar = "-1"
 end if
 first_plot_ind = ind(form_data.eq."***Plot Specific Parameters***")+2                   ; Find the index of the begining of each
 first_snd_ind = ind(form_data.eq."***Sounding Parameters***")+2                         ; plotting parameter section in form*.out
 first_xs_ind = ind(form_data.eq."***Cross Section Parameters***")+2
 first_overlay_ind = ind(form_data.eq."***Overlays***")+1
 first_terrain_ind = ind(form_data.eq."***Terrain Heights***")+1
 last_ind = ind(form_data.eq."***END OF INPUT***".or.form_data.eq."***END OF INPUT*** ")
 form_data_plots = form_data(first_plot_ind:first_snd_ind-3)

; Get range-specific grid dimensions and locations for use in adding station bullets and their labels
; It is also used for the plot which shows the model grid configuration
; This information is in the respective WPS namelist file for each range
; Fields obtained are, reference latitude, East-West domain width in grid points, grid resolution, and
; the grid ratio between each grid and its next smaller grid
; This information is then later used in the "Plot overlay(s) of range boundary(s)" section of the code
 range_wdth = new(num_doms,"float")
 range_hght = new(num_doms,"float")
 i_parent   = new(num_doms,"float")
 j_parent   = new(num_doms,"float")
 wps_file = asciiread("~/cvs/projects/atec/rtfdda/"+str_lower(range_location)+"/wrf/GW"+range_location+"/namelists/namelist.wps",-1,"string")
 do ii=0,dimsizes(wps_file)-1
  if (isStrSubset(wps_file(ii),"ref_lat")) then
   range_lat = stringtofloat(str_get_field(str_get_field(wps_file(ii),2,"="),1,","))
  end if
  if (isStrSubset(wps_file(ii),"e_we")) then
   do hh=0,num_doms-1
    range_wdth(hh) = stringtofloat(str_get_field(str_get_field(wps_file(ii),2,"="),hh+1,","))
   end do
  end if
  if (isStrSubset(wps_file(ii),"e_sn")) then
   do hh=0,num_doms-1
    range_hght(hh) = stringtofloat(str_get_field(str_get_field(wps_file(ii),2,"="),hh+1,","))
   end do
  end if
  if (isStrSubset(wps_file(ii),"dx")) then
   range_d01_res = stringtofloat(str_get_field(str_get_field(wps_file(ii),2,"="),1,","))*0.001
  end if
  if (isStrSubset(wps_file(ii),"parent_grid_ratio")) then
   range_dmn_rat = stringtofloat(str_get_field(str_get_field(wps_file(ii),2,"="),2,","))
  end if
  if (isStrSubset(wps_file(ii),"i_parent_start")) then
   do hh=0,num_doms-1
    i_parent(hh) = stringtofloat(str_get_field(str_get_field(wps_file(ii),2,"="),hh+1,","))
   end do
  end if
  if (isStrSubset(wps_file(ii),"j_parent_start")) then
   do hh=0,num_doms-1
    j_parent(hh) = stringtofloat(str_get_field(str_get_field(wps_file(ii),2,"="),hh+1,","))
   end do
  end if
 end do

; Get info for overlaying: range boundaries, county boundaries and station locations
 range_overlay_doms = str_strip(str_get_field(form_data(first_overlay_ind),2,":"))
 county_overlay_doms = str_strip(str_get_field(form_data(first_overlay_ind+1),2,":"))
 poli_bdry_info = str_strip(str_get_field(form_data(first_overlay_ind+2),2,":"))
 station_overlay_info = str_strip(form_data(first_overlay_ind+3:first_terrain_ind-1))

; Start looping through each season and domains and create a RIP namelist file for each
 n_plots = dimsizes(form_data_plots)
 do gg=0,n_seasons-1   ; Loop through each season
  do hh=0,num_doms-1   ; Loop through each domain

   ; select the political boundary for this domain
   if (hh.ge.2) then
    poli_bdry = str_strip(str_get_field(poli_bdry_info,hh-1,","))
   end if

   ; Update the header lines based on: domain, range DST flag, and range time zone offset in hours
   header_lines(1) = " titlecolor='dark.green', idotitle=1, title='rangename 4DWX Cfg 1 Domain "+(hh+1)+"  Cycle=cycle'"
   range_tzones_dst = asciiread("range_tzones_dst.asc",-1,"string")
   rtd = range_tzones_dst(1:)
   delete(range_tzones_dst)
   range_temp = str_get_field(rtd,1,",")
   tzone = str_get_field(rtd,2,",")
   dst = str_get_field(rtd,3,",")
   range_ind = ind(range_temp.eq.range_location)
   header_lines(5) = " timezone="+tzone(range_ind)+", iusdaylightrule="+dst(range_ind)+","
   delete(rtd)
   delete(range_temp)
   delete(tzone)
   delete(dst)
   delete(range_ind)
   all_lines = header_lines           ; "all_lines" keeps getting appended to throughout this script so that
                                      ; it eventually contains all the lines for each RIP namelist file and
                                      ; is written out as the namelist file

   ; Insert custom RIP color table lines to the namelist file, defined by the RGB settings in rip_color.asc
   temp_lines = all_lines
   delete(all_lines)
   new_lines = asciiread("rip_color.asc",-1,"string")
   all_lines = array_append_record(temp_lines,new_lines,0)
   delete(temp_lines)
   delete(new_lines)

   ; Insert Plot Specification Table lines needed for indicating that the individual plots start here
   temp_lines = all_lines
   delete(all_lines)
   new_lines = new(3,"string")
   new_lines(0) = end_line
   new_lines(1) = "----------------------    Plot Specification Table    ---------------------"
   new_lines(2) = end_line
   all_lines = array_append_record(temp_lines,new_lines,0)
   delete(temp_lines)
   delete(new_lines)

   ; Get some info up front for certain fields
   xs_end_pts = str_get_field(form_data(first_xs_ind+hh),2,";")      ; XS end points
   n_XSs = str_fields_count(xs_end_pts,":")                          ; number of XSs
   terr_min = str_get_field(form_data(first_terrain_ind+hh+1),2,";") ; minimum value for terrain shading plots
   terr_max = str_get_field(form_data(first_terrain_ind+hh+1),3,";") ; maximum value for terrain shading plots

   ; Loop through each plot and creates its code, then append it onto the namelist file
   do ii=0,n_plots-1

    ; Use the plot abbrevation from form*.out to check in plot_defaults.asc for the
    ; same plot abbrevation, and then get its index for obtaining the appropriate
    ; default parameters
    form_line = form_data_plots(ii)
    plot_name = str_strip(str_get_field(form_line,1,";"))
    plot_ind = ind(plot_abv.eq.plot_name)

    ; Using the index from the previous step, get the corresponding plot info 
    ; for each plotted field
    abv = plot_abv(plot_ind)                 ; Plot abbreviation
    plt_descrip = plot_desc(plot_ind)        ; Plot description
    plt_type = plot_type(plot_ind)           ; Plot type (e.g., Planview, XS, Special, etc.)
    shaded = plot_shaded(plot_ind)           ; Shaded field
    contoured = plot_contoured(plot_ind)     ; Contoured field
    dashed = plot_dashed(plot_ind)           ; Dashed field
    wnd_field = plot_wind(plot_ind)          ; Wind field
    plt_vcor_sha = plot_vcor_sha(plot_ind)   ; Vertical coordinate for shaded field
    plt_vcor_con = plot_vcor_con(plot_ind)   ; Vertical coordinate for contoured field
    plt_vcor_dsh = plot_vcor_dsh(plot_ind)   ; Vertical coordinate for dashed field
    plt_lev_sha  = plot_lev_sha(plot_ind)    ; Vertical level(s) for shaded field
    plt_lev_con  = plot_lev_con(plot_ind)    ; Vertical level(s) for contoured field
    plt_lev_dsh  = plot_lev_dsh(plot_ind)    ; Vertical level(s) for dashed field
    plt_sm_sha   = plot_sm_sha(plot_ind)     ; Smoothing level for shaded field
    plt_sm_con   = plot_sm_con(plot_ind)     ; Smoothing level for contoured field
    plt_sm_dsh   = plot_sm_dsh(plot_ind)     ; Smoothing level for dashed field
    ext_line     = extra_line(plot_ind)      ; Value of extra line to plot (e.g., 1000-500 540 line)
    vec_type     = plot_vec_type(plot_ind)   ; Vector type for wind field (e.g., barb, arrow, streamline)
    sha_lbl_int  = str_get_field(shade_lbl_int(plot_ind),(hh+1),",")             ; Shaded contour label interval
    sha_int  = str_strip(str_get_field(str_get_field(form_line,2,";"),hh+1,",")) ; Shaded field contour interval
    ctr_int = str_strip(str_get_field(str_get_field(form_line,3,";"),hh+1,","))  ; Contoured field contour interval
    dsh_int = str_strip(str_get_field(str_get_field(form_line,4,";"),hh+1,","))  ; Dashed field contour interval
    sha_max = str_strip(str_get_field(form_line,5+2*gg,";"))                     ; Max shaded value
    sha_min = str_strip(str_get_field(form_line,6+2*gg,";"))                     ; Min shaded value
    clr_tbl = str_strip(str_get_field(form_line,9,";"))                          ; Color table name for shaded plots
    hghlt_line = str_strip(str_get_field(form_line,10,";"))                      ; Line to highlight (e.g., 1000-500 540 line)

;   Create exceptions for: 
;   Wind speed, which can be knots or m/s
;   All other plots use the same units for all ranges
    if (shaded.eq."wsp".and.KtsorMS.eq."10kts") then
     shaded = "wspk"
    end if

; Determine if field should be hidden or visible below ground level (for upper air plots)
    hide = ""
    if (underground.eq."hide".and.plt_vcor_sha.eq."p") then
     hide = "; hide"
    end if
    if (plt_vcor_sha.eq."t".or.plt_vcor_sha.eq."m") then
     hide = "; hide"
    end if

;   Call function in plotting_routines.ncl to get the description lines 
;   at the top of each plot code
    temp = all_lines
    delete(all_lines)
    if (sha_int.ne.999.0.or.ctr_int.ne.999.0.or.dsh_int.ne.999.0) then
     all_lines = array_append_record(temp,get_descrip(plt_descrip),0)
    else
     all_lines = temp
    end if
    delete(temp)

;*******************************************************;
;**************PLAN VIEW PLOTTING SECTION***************;
;*******************************************************;
; Start with code for making plan-view, non-raster plots
    if (plt_type.eq."Plan") then

;    Call function in plotting_routines.ncl to plot shaded field first
;    If plot has shaded field, plot only the map outlines
;    If plot does not have shaded field (indicated by "MMM"), plot default 
;    shaded background map
     if (shaded.ne."MMM".and.sha_int.ne."none") then
      temp = all_lines
      delete(all_lines)
      if (shaded.eq."ter") then   ; Terrain uses specific mins and maxes for each range and domain
       sha_min = terr_min
       sha_max = terr_max
       sha_int = sprinti("%0.1i",(stringtointeger(terr_max)-stringtointeger(terr_min))/20)
      end if
      all_lines = array_append_record(temp,Shade(shaded,sha_int,sha_max,sha_min,plt_vcor_sha,plt_lev_sha,plt_sm_sha,clr_tbl,ext_line,hh,hide,clrbar,sha_lbl_int,abv,HB),0)
      delete(temp)
      end_lines = new(2,"string")
      if ((hh+1).ge.3) then                                                   ; If 3rd or 4th domain, use high-res coastlines
       end_lines(0) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black; outy="+poli_bdry
      else
       end_lines(0) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
      end if
      end_lines(1) = "feld=tic; ptyp=hb; linw=1; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
      temp = all_lines
      delete(all_lines)
      all_lines = array_append_record(temp,end_lines,0)
      delete(end_lines)
      delete(temp)
     else
      end_lines = new(2,"string")
      if ((hh+1).ge.3) then                                                   ; If 3rd or 4th domain, use high-res coastlines
       end_lines(0) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black; outy="+poli_bdry+"; mfco=blue1,yellow1"
      else
       end_lines(0) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black; mfco=blue1,yellow1"
      end if
      end_lines(1) = "feld=tic; ptyp=hb; linw=1; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
      temp = all_lines
      delete(all_lines)
      all_lines = array_append_record(temp,end_lines,0)
      delete(end_lines)
      delete(temp)
     end if

;    Call function in plotting_routines.ncl to plot dashed field, if necessary
     if (dashed.ne."MMM".and.dsh_int.ne."none") then
      temp = all_lines
      delete(all_lines)
      all_lines = array_append_record(temp,Dash(dashed,dsh_int,plt_vcor_dsh,plt_lev_dsh,plt_sm_dsh,hghlt_line,hide),0)
      delete(temp)
     end if

;    Call function in plotting_routines.ncl to plot contoured (unshaded) field, if necessary
     if (contoured.ne."MMM".and.ctr_int.ne."none") then
      temp = all_lines
      delete(all_lines)
      all_lines = array_append_record(temp,Contour(contoured,ctr_int,plt_vcor_con,plt_lev_con,plt_sm_con,hide),0)
      delete(temp)
     end if

;    Call function in plotting_routines.ncl to plot wind barbs or streamlines, if necessary
     if (wnd_field.ne."MMM") then
      temp = all_lines
      delete(all_lines)
      all_lines = array_append_record(temp,Wind(wnd_field,KtsorMS,vec_type,hh,brb_density),0)
      delete(temp)
     end if

    end if  ; End IF statement for plan view non-Raster plots

; Set parameters for plan-view, Raster plots
    if (plt_type.eq."Raster") then

;   Call function in plotting_routines.ncl to plot raster field
     temp = all_lines
     delete(all_lines)
     all_lines = array_append_record(temp,Raster(shaded,sha_int,sha_max,sha_min,plt_vcor_sha,plt_lev_sha,plt_sm_sha,clr_tbl,clrbar),0)
     delete(temp)

; Add map and grid
     map_lines = new(2,"string")
     if ((hh+1).ge.3) then                                                   ; If 3rd or 4th domain, use high-res coastlines
      map_lines(0) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black; outy="+poli_bdry
     else
      map_lines(0) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
     end if
     map_lines(1) = "feld=tic; ptyp=hb; linw=1; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
     temp = all_lines
     delete(all_lines)
     all_lines = array_append_record(temp,map_lines,0)
     delete(map_lines)
     delete(temp)

    end if ; End IF statement for plan view Raster plots

;*******************************************************;
;*************SPECIAL PLOT PLOTTING SECTION*************;
;*******************************************************;
; Set parameters for a few unique plots that don't conveniently fit into
; the Plan View plotting structure
    if (plt_type.eq."Special".and.(sha_int.ne.999.0.or.ctr_int.ne.999.0.or.dsh_int.ne.999.0)) then

     ; If one-hour precip is plotted, read in the plotting parameter lines from
     ; an ascii file and then adjust as necessary
     if (plot_name.eq."Pre1hr") then
      new_lines = asciiread("Pre1hr.asc",-1,"string")
      new_lines(6) = "    nohl; hvbr="+clrbar
     end if
     ; If accumulated precip is plotted, read in the plotting parameter lines from
     ; an ascii file and then adjust as necessary
     if (plot_name.eq."Pref0ac") then
      new_lines = asciiread("PreAcc.asc",-1,"string")
      new_lines(6) = "    nohl; hvbr="+clrbar
     end if
     ; If surface visibility is plotted, make the irregular shading intervals for the plot
     if (plot_name.eq."sfcvis") then
      new_lines = new(4,"string")
      if (sha_max.eq.16) then
       new_lines(0) = "feld=visb; ptyp=hc; cmth=cell; hvbr="+clrbar+"; cval=0.25,0.5,0.75,1,2,3,4,5,6,8,10,12,14,16;>"
       new_lines(1) = "   cosq=0.25,red8,0.99,red2,1,purple8,4.99,purple2,5,blue8,16,white"
      else
       new_lines(0) = "feld=visb; ptyp=hc; cmth=cell; hvbr="+clrbar+"; cval=0.5,1,1.5,2,4,6,8,10,15,20,25,30;>"
       new_lines(1) = "   cosq=0.5,red8,1.99,red2,2,purple8,9.99,purple2,10,blue8,30,white"
      end if
      new_lines(2) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
      new_lines(3) = "feld=tic; ptyp=hb; linw=1; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
     end if
     ; If PBL Height is plotted, make the irregular shading intervals for the plot
     if (plot_name.eq."pblhgt") then
      new_lines = new(6,"string")
      new_lines(0) = "feld=PBLH; ptyp=hc; cmth=both; hvbr="+clrbar+";>"
      new_lines(1) = "   cval=100,200,300,400,500,600,800,1000,1250,1500,1750,2000,2500,3000,3500,4000;>"
      new_lines(2) = "   cosq=0,white,133,cyan3,399,cyan5,400,purple2,999,purple5,1000,green2,1999,green5,>"
      new_lines(3) = "   2000,magenta2,3999,magenta5,4000,yellow2"
      new_lines(4) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
      new_lines(5) = "feld=tic; ptyp=hb; linw=1; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
     end if
     ; If 600 m vertical velocity is plotted, read in the plotting parameter lines from
     ; an ascii file and then adjust as necessary
     if (plot_name.eq."VV600m") then
      terr_int = sprinti("%0.1i",(stringtointeger(terr_max)-stringtointeger(terr_min))/20)
      new_lines = asciiread("VV600m.asc",-1,"string")
      new_lines(0) = "feld=ter; ptyp=hc; cmth=fill; cbeg="+terr_min+"; cend="+terr_max+"; cint="+terr_int+"; smth=1; nohl;>"
      new_lines(3) = "feld=www; ptyp=hc; vcor=s; levs=8fb; cmth=cont; cbeg=0; cend=300; cint="+ctr_int+";>"
      new_lines(6) = "feld=www; ptyp=hc; vcor=s; levs=8fb; cmth=cont; cbeg=0; cend=-300; cint="+ctr_int+";>"
      new_lines(9) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
      temp = array_append_record(new_lines(0:1),get_color_table(clr_tbl,sha_max,sha_min),0)
      temp2 = array_append_record(temp,new_lines(3:10),0)
      delete(new_lines)
      delete(temp)
      new_lines = temp2
      delete(temp2)
     end if
     ; If 700 mb RH and Omega are plotted, read in the plotting parameter lines from
     ; an ascii file and then adjust as necessary
     if (plot_name.eq."Omeg700RH") then
      read_lines = asciiread("Omeg700RH.asc",-1,"string")
      read_lines(1) = "   vcor=p; hvbr="+clrbar+"; levs=700; nohl; nolb;>"
      read_lines(7) = "feld=omg; ptyp=hc; vcor=p; levs=700; cmth=cont; cbeg=0; cend=300; cint="+dsh_int+";>"
      read_lines(9)= "feld=omg; ptyp=hc; vcor=p; levs=700; cmth=cont; cbeg=0; cend=-300; cint="+dsh_int+";>"
      color_lines = get_color_table(clr_tbl,sha_max,sha_min)
      new_lines = new(dimsizes(read_lines)-3+dimsizes(color_lines),"string")
      new_lines(0:1) = read_lines(0:1)
      new_lines(2:1+dimsizes(color_lines)) = color_lines
      new_lines(2+dimsizes(color_lines):dimsizes(read_lines)-4+dimsizes(color_lines)) = (/read_lines(5:12)/)
     end if
     ; If the cross section location(s) map is plotted, shade terrain first and
     ; then overlay with the cross section(s)
     if (plot_name.eq."xsloc") then
      if (shaded.eq."ter") then   ; Terrain uses specific mins and maxes for each range and domain
       sha_min = terr_min
       sha_max = terr_max
       sha_int = sprinti("%0.1i",(stringtointeger(terr_max)-stringtointeger(terr_min))/20)
      end if
      new_lines = new(5+2*n_XSs,"string")
      new_lines(0) = "feld="+shaded+"; ptyp=hc; cmth=fill; vcor="+plt_vcor_sha+"; levs="+plt_lev_sha+";smth="+plt_sm_sha+";>"
      new_lines(1) = "   cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; hvbr="+clrbar+"; nohl;>"
      do jj=0,n_XSs-1
       XS_lat_beg = str_strip(str_get_field(str_get_field(str_get_field(xs_end_pts,jj+1,":"),1,">"),1,","))
       XS_lon_beg = str_strip(str_get_field(str_get_field(str_get_field(xs_end_pts,jj+1,":"),1,">"),2,","))
       XS_lat_end = str_strip(str_get_field(str_get_field(str_get_field(xs_end_pts,jj+1,":"),2,">"),1,","))
       XS_lon_end = str_strip(str_get_field(str_get_field(str_get_field(xs_end_pts,jj+1,":"),2,">"),2,","))
       new_lines(3+2*jj) = "feld=line; ptyp=hb; vcor="+plt_vcor_sha+"; levs="+plt_lev_sha+"; linw=2; dash=31;>"
       new_lines(4+2*jj) = "   colr=red; crsa="+XS_lat_beg+"lat,"+XS_lon_beg+"lon; crsb="+XS_lat_end+"lat,"+XS_lon_end+"lon"
      end do
      new_lines(5+2*(n_XSs-1)) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
      new_lines(6+2*(n_XSs-1)) = "feld=tic; ptyp=hb; linw=1; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
      temp = array_append_record(new_lines(0:1),get_color_table("Terrain",sha_max,sha_min),0)
      temp2 = array_append_record(temp,new_lines(3:dimsizes(new_lines)-1),0)
      delete(new_lines)
      delete(temp)
      new_lines = temp2
      delete(temp2)
     end if
     ; If Land Use is plotted, read in the plotting parameter lines from
     ; an ascii file and then adjust as necessary
     if (plot_name.eq."landuse") then
      new_lines = asciiread("landuse.asc",-1,"string")
      new_lines(5) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
     end if
     ; If Land Use Table is plotted, read in land use types and colors
     if (plot_name.eq."LandUseTbl") then
      LU_lines = asciiread("landuse_table.asc",-1,"string")
      LU_colors = str_get_field(LU_lines,1,";")
      LU_names = str_sub_str(str_get_field(LU_lines,2,";")," ","_")
      n_LU_types = dimsizes(LU_lines)
      LU_n_chars = new(n_LU_types,"float")
      delete(LU_lines)
      do kk=0,n_LU_types-1
       LU_n_chars(kk) = tofloat(dimsizes(stringtochar(LU_names(kk))))
      end do
      top_line = range_hght(hh) - 6.
      bottom_line = 2.
      LU_inds = fspan(top_line,bottom_line,n_LU_types)
      new_lines = new(2*n_LU_types+1,"string")
      new_lines(0) = "feld=bullet; ptyp=hb; linw=2; crsa="+sprintf("%5.1f",(4.+14.*range_wdth(hh)/110.))+","+(range_hght(hh)-2.5)+"; titl=Land_Use_Table; tslb=0.0150; colr=black"
      do kk=0,n_LU_types-1
       new_lines(kk*2+1) = "feld=bullet; ptyp=hb; linw=2; crsa=3,"+sprintf("%5.1f",LU_inds(kk))+"; tslb=0.015; colr="+LU_colors(kk)
       new_lines(kk*2+2) = "feld=bullet; ptyp=hb; linw=2; crsa="+sprintf("%5.1f",2.+(range_wdth(hh)/14.+LU_n_chars(kk)*range_wdth(hh)/235.))+","+sprintf("%5.1f",LU_inds(kk))+"; titl="+LU_names(kk)+"; tslb=0.0070; colr=black"
      end do
      delete(LU_colors)
      delete(LU_names)
      delete(LU_n_chars)
      delete(LU_inds)
     end if
     ; If the WRF grid configuration is plotted, shade terrain first and
     ; then overlay with grid boxes for each subsequent inner domain
     ; For example, 3 grid boxes would be drawn on a D1 map with 4 domains
     if (plot_name.eq."WRFgrids") then
      if (shaded.eq."ter") then   ; Terrain uses specific mins and maxes for each range and domain
       sha_min = terr_min
       sha_max = terr_max
       sha_int = sprinti("%0.1i",(stringtointeger(terr_max)-stringtointeger(terr_min))/20)
      end if
      first_lines = new(4,"string")
      first_lines(0) = "feld="+shaded+"; ptyp=hc; cmth=fill; vcor="+plt_vcor_sha+"; levs="+plt_lev_sha+";smth="+plt_sm_sha+";>"
      first_lines(1) = "   cbeg="+sha_min+"; cend="+sha_max+"; cint="+sha_int+"; hvbr="+clrbar+"; nohl;>"
      first_lines(2) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
      first_lines(3) = "feld=tic; ptyp=hb; linw=1; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
      temp = array_append_record(first_lines(0:1),get_color_table("Terrain",sha_max,sha_min),0)
      temp2 = array_append_record(temp,first_lines(2:3),0)
      delete(first_lines)
      delete(temp)
      new_lines = temp2
      delete(temp2)
      if(hh.lt.num_doms-1) then                  ; Only plot grids for domains with subdomain(s)
       last_lines = new(num_doms-hh-1,"string")
       do kk=0,num_doms-hh-2                     ; Loop through each grid box for the current domain's subdomain(s)
        left = i_parent(hh+1)
        lowr = j_parent(hh+1)
        rght = left+range_wdth(hh+1)/range_dmn_rat
        uppr = lowr+range_hght(hh+1)/range_dmn_rat
        if (kk.ne.0) then
         do ll=0,num_doms-hh-kk-2                ; Loop through each subdomain to calculate current domain coordinates of subdomain(s)
          left = left + i_parent(hh+ll+2)/(range_dmn_rat^(ll+1))
          lowr = lowr + j_parent(hh+ll+2)/(range_dmn_rat^(ll+1))
          rght = left + range_wdth(hh+ll+2)/(range_dmn_rat^(ll+2))
          uppr = lowr + range_hght(hh+ll+2)/(range_dmn_rat^(ll+2))
         end do
        end if
        Lft = sprinti("%0.1i",floattoint(left))
        Lwr = sprinti("%0.1i",floattoint(lowr))
        Rgt = sprinti("%0.1i",floattoint(rght))
        Upr = sprinti("%0.1i",floattoint(uppr))
        last_lines(kk) = "feld=box; linw=2; colr=red; ptyp=hb; crsa="+Lft+","+Lwr+"; crsb="+Rgt+","+Upr
       end do
       temp = new_lines
       delete(new_lines)
       new_lines = array_append_record(temp,last_lines,0)
       delete(last_lines)
       delete(temp)
      end if
     end if
     if (plot_name.eq."FzLevHgt") then
      new_lines = asciiread("frzlvlhgt.asc",-1,"string")
      new_lines(4) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
      new_lines(5) = "feld=tic; ptyp=hb; linw=1; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
     end if
     if (plot_name.eq."FzLevHgtAMSL") then
      new_lines = asciiread("frzlvlhgtAMSL.asc",-1,"string")
      new_lines(4) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
      new_lines(5) = "feld=tic; ptyp=hb; linw=1; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
     end if

     ; Make map line use high-res coastlines for domains 3 and 4
     if ((hh+1).ge.3) then             ; If 3rd or 4th domain, use high-res coastlines
      do rr=0,dimsizes(new_lines)-1    ; Once NCL v6.0.0 is installed, use str_match_ind instead of loop and if statement
       if (isStrSubset(new_lines(rr),"feld=map").and.(.not.(isStrSubset(new_lines(rr),"outy="+poli_bdry)))) then
        new_lines(rr) = new_lines(rr) + "; outy="+poli_bdry
       end if
      end do
     end if

     temp = all_lines
     delete(all_lines)
     all_lines = array_append_record(temp,new_lines,0)
     delete(temp)
     delete(new_lines)

    end if  ; End IF statement for "Special" plots

;*******************************************************;
;***************OVERLAY PLOTTING SECTION****************;
;*******************************************************;
;  Plot overlay(s) of range boundary(s), county boundaries and station location(s), 
;  onto Plan View Raster, non-Raster plots and Special Plots, if necessary.
;  Overlay functions are in plotting_routines.ncl
    if (plt_type.eq."Raster".or.plt_type.eq."Plan".or.plt_type.eq."Special".and.plot_name.ne."LandUseTbl") then
     if (isStrSubset(range_overlay_doms,sprinti("%0.1i",hh+1))) then
      temp = all_lines
      delete(all_lines)
      all_lines = array_append_record(temp,Overlay_Boundaries(range_location),0)
      delete(temp)
     end if
     if (isStrSubset(county_overlay_doms,sprinti("%0.1i",hh+1))) then
      temp = all_lines
      delete(all_lines)
      all_lines = array_append_record(temp,Overlay_Counties(),0)
      delete(temp)
     end if
     do kk=0,dimsizes(station_overlay_info)-1
      if (isStrSubset(str_get_field(station_overlay_info(kk),3,";"),sprinti("%0.1i",hh+1))) then
       ; The following line offsets the station name from the station bullet, using the model domain information
       ; obtained earlier in the code
       lbl_offset = sin(range_lat/180.*3.14)*range_wdth(hh)*range_d01_res*range_dmn_rat^int2flt(0-hh)
       temp = all_lines
       delete(all_lines)
       all_lines = array_append_record(temp,Overlay_Stations(station_overlay_info(kk),hh,lbl_offset),0)
       delete(temp)
      end if
     end do
    end if ; End IF statement for Overlays

;*******************************************************;
;************CROSS SECTION PLOTTING SECTION*************;
;*******************************************************;
; Set parameters for XS plots
; Don't create a plot if all contour intervals are set to missing (e.g., 999)
    if (plt_type.eq."XS".and.(sha_int.ne.999.0.or.ctr_int.ne.999.0.or.dsh_int.ne.999.0)) then

;    Loop through all cross-section planes
     do jj=0,n_XSs-1

;     Call function to plot shaded XS field, if necessary
;     Obtain the end points of the cross section (only requested in the
;     shaded plot section for now, since all current XS have shading)
;     Function is in plotting_routines.ncl
      if (shaded.ne."MMM") then
       temp = all_lines
       delete(all_lines)
       end_pts = str_strip(str_get_field(xs_end_pts,jj+1,":"))
       all_lines = array_append_record(temp,Shade_XS(shaded,sha_int,sha_max,sha_min,plt_vcor_sha,plt_sm_sha,clr_tbl,ext_line,end_pts,clrbar,sha_lbl_int,plt_lev_sha),0)
       delete(temp)
      end if

;     Call function to plot contoured XS field, if necessary
;     Function is in plotting_routines.ncl
      if (contoured.ne."MMM") then
       temp = all_lines
       delete(all_lines)
       all_lines = array_append_record(temp,Contour_XS(contoured,ctr_int,plt_vcor_con,plt_sm_con,end_pts,plt_lev_sha),0)
       delete(temp)
      end if

;     Call function to plot dashed XS field, if necessary
;     Function is in plotting_routines.ncl
      if (dashed.ne."MMM") then
       temp = all_lines
       delete(all_lines)
       all_lines = array_append_record(temp,Dash_XS(dashed,dsh_int,plt_vcor_dsh,plt_sm_dsh,end_pts,plt_lev_sha),0)
       delete(temp)
      end if

;     Call function to plot XS wind barbs or streamlines, if necessary
;     Function is in plotting_routines.ncl
      if (wnd_field.ne."MMM") then
       temp = all_lines
       delete(all_lines)
       all_lines = array_append_record(temp,Wind_XS(wnd_field,KtsorMS,vec_type,end_pts,plt_lev_sha),0)
       delete(temp)
      end if

;     Get lines to plot surface level and grid
      end_lines = "feld=tic; ptyp=vb; linw=1; axld=300; axtd=50; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
      temp = all_lines
      delete(all_lines)
      all_lines = array_append_record(temp,end_lines,0)
      delete(end_lines)
      delete(temp)

;     Add a separation line between XS plots, if more than one XS is being plotted, and it is not the final XS
      if (n_XSs.gt.1.and.jj.ne.(n_XSs-1)) then
       temp = all_lines
       delete(all_lines)
       all_lines = array_append_record(temp,end_line,0)
       delete(temp)
      end if

     end do
    end if  ; End IF statement for XS plots

;   Set parameters for Special XS plots
    if (plt_type.eq."XSSpcl") then
     do jj=0,n_XSs-1  ; Loop through all XS planes

;     If XS Precip hydrometeor mixing ratio is to be plotted
      if (plot_name.eq."xsPcp") then
       XS_lines = asciiread("xsPcp.asc",-1,"string")
       end_pts = str_strip(str_get_field(xs_end_pts,jj+1,":"))
       XS_lat_beg = str_strip(str_get_field(str_get_field(end_pts,1,">"),1,","))
       XS_lon_beg = str_strip(str_get_field(str_get_field(end_pts,1,">"),2,","))
       XS_lat_end = str_strip(str_get_field(str_get_field(end_pts,2,">"),1,","))
       XS_lon_end = str_strip(str_get_field(str_get_field(end_pts,2,">"),2,","))
       XS_lines(2) = "   crsa="+XS_lat_beg+"lat,"+XS_lon_beg+"lon; crsb="+XS_lat_end+"lat,"+XS_lon_end+"lon;>"
       XS_lines(5) = "feld=www; ptyp=vc; cmth=cont; vcor=z; smth=0; cbeg=0; cend=500; cint="+ctr_int+";>"
       XS_lines(6) = XS_lines(2)
       XS_lines(8) = "feld=www; ptyp=vc; cmth=cont; vcor=z; smth=0; cbeg=-500; cend=0; cint="+ctr_int+"; dash=21;>"
       XS_lines(9) = XS_lines(2)
       n_XS_lns = dimsizes(XS_lines)
       color_lines = get_color_table(clr_tbl,sha_max,sha_min)
       n_clr_lns = dimsizes(color_lines)
       temp = new(n_XS_lns-2+n_clr_lns,"string")
       temp(0:2) = XS_lines(0:2)
       temp(3:n_clr_lns+2) = (/color_lines/)
       temp(n_clr_lns+3:n_XS_lns-3+n_clr_lns) = XS_lines(5:n_XS_lns-1)
       delete(XS_lines)
       XS_lines = temp
       delete(temp)
       delete(color_lines)
       delete(XS_lat_beg)
       delete(XS_lon_beg)
       delete(XS_lat_end)
       delete(XS_lon_end)
      end if

;     Paste XS Special lines onto RIP namelist file
      temp = all_lines
      delete(all_lines)
      all_lines = array_append_record(temp,XS_lines,0)
      delete(XS_lines)
      delete(temp)

;     Add a separation line between XS plots, if more than one XS is being plotted, and it is not the final XS
      if (n_XSs.gt.1.and.jj.ne.(n_XSs-1)) then
       temp = all_lines
       delete(all_lines)
       all_lines = array_append_record(temp,end_line,0)
       delete(temp)
      end if

     end do  ; End looping through all cross section planes
    end if  ; End of Special XS plotting section

;   Get final_line to end plot parameters for the current Plan View, XS, and Custom plots
    temp = all_lines
    delete(all_lines)
    all_lines = array_append_record(temp,end_line,0)
    delete(temp)

; Remove double 'end_line's at end of namelist file, if any
    if (all_lines(dimsizes(all_lines)-1).eq.end_line.and.all_lines(dimsizes(all_lines)-2).eq.end_line) then
     temp = all_lines
     delete(all_lines)
     all_lines = temp(0:dimsizes(temp)-2)
     delete(temp)
    end if

;*******************************************************;
;****************CUSTOM PLOTTING SECTION****************;
;*******************************************************;
;   Use custom plots for those rare plots that are already completely defined for the field and no changes are
;   necessary to the RIP input.  These are typically specialized plots used by one range only
;   Overlay any stations and/or range boundaries on these plots
    if (plt_type.eq."Custom".and.(isfilepresent("Mdomain"+(hh+1)+"."+seasons(gg)+"."+range_location+".asc"))) then
     new_lines = asciiread("Mdomain"+(hh+1)+"."+seasons(gg)+"."+range_location+".asc",-1,"string")
     final_lines = ind(new_lines.eq.end_line)
     num_cust_plots = dimsizes(final_lines)
     if (num_cust_plots.eq.1) then
      initial_lines = 0
     else
      initial_lines = array_append_record(0,final_lines(0:num_cust_plots-2)+1,0)
     end if
     do uu=0,num_cust_plots-1
      cust_lines = new_lines(initial_lines(uu):final_lines(uu)-1)
      map_lines = new(2,"string")
      if ((hh+1).ge.3) then                                                   ; If 3rd or 4th domain, use high-res coastlines
       map_lines(0) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black; outy="+poli_bdry
      else
       map_lines(0) = "feld=map; ptyp=hb; ouds=solid; cint=10; oulw="+poli_bdry_thick+"; colr=black"
      end if
      map_lines(1) = "feld=tic; ptyp=hb; linw=1; axlg="+maj_tic_freq+"; axtg="+mnr_tic_freq
      temp = cust_lines
      delete(cust_lines)
      cust_lines = array_append_record(temp,map_lines,0)
      delete(temp)
      delete(map_lines)
      if (isStrSubset(range_overlay_doms,sprinti("%0.1i",hh+1))) then
       temp = cust_lines
       delete(cust_lines)
       cust_lines = array_append_record(temp,Overlay_Boundaries(range_location),0)
       delete(temp)
      end if
      if (isStrSubset(county_overlay_doms,sprinti("%0.1i",hh+1))) then
       temp = cust_lines
       delete(cust_lines)
       cust_lines = array_append_record(temp,Overlay_Counties(),0)
       delete(temp)
      end if
      do kk=0,dimsizes(station_overlay_info)-1
       if (isStrSubset(str_get_field(station_overlay_info(kk),3,";"),sprinti("%0.1i",hh+1))) then
        ; The following line offsets the station name from the station bullet, using the model domain information
        ; obtained earlier in the code
        lbl_offset = sin(range_lat/180.*3.14)*range_wdth(hh)*range_d01_res*range_dmn_rat^int2flt(0-hh)
        temp = cust_lines
        delete(cust_lines)
        cust_lines = array_append_record(temp,Overlay_Stations(station_overlay_info(kk),hh,lbl_offset),0)
        delete(temp)
       end if
      end do
      temp = cust_lines
      delete(cust_lines)
      cust_lines = array_append_record(temp,end_line,0)
      delete(temp)
      temp = all_lines
      delete(all_lines)
      all_lines = array_append_record(temp,cust_lines,0)
      delete(temp)
      delete(cust_lines)
     end do
     delete(new_lines)
     delete(final_lines)
     delete(initial_lines)
    end if

   end do  ; end loop through all plots in the namelist except soundings

; Create the sounding namelist lines for each domain
; Each sounding is plotted twice, once with the plotting parameters box,
; and once without it
   if (dup_snding.eq."no") then
    n_dup = 0
   else
    n_dup = 1
   end if
   sounding_loc = form_data(first_snd_ind:first_xs_ind-3)
   sounding_locs = str_strip(str_get_field(sounding_loc(hh),2,";"))
   delete(sounding_loc)
   n_soundings = str_fields_count(sounding_locs,":")
   sounding_lines = new(n_soundings*6*(n_dup+1),"string")
   do ii=0,n_soundings-1
    indiv_sounding = str_get_field(sounding_locs,ii+1,":")
    ; If there is more than one sounding, prepare for this;
    ; otherwise, just make the single sounding
    if (isStrSubset(indiv_sounding,",")) then
     sloc_lat = str_strip(str_get_field(indiv_sounding,1,","))
     sloc_lon = str_strip(str_get_field(indiv_sounding,2,","))
     sloc = sloc_lat+"lat,"+sloc_lon+"lon"
    else
     sloc = str_strip(indiv_sounding)
    end if
    sounding_lines(ii*6*(1+n_dup))   = "feld=tic; ptyp=sb; sndg; hodo"
    sounding_lines(ii*6*(1+n_dup)+1) = "feld=uuu,vvv; ptyp=sv; colr=dark.green; fulb=5mps; vvms=.03; nttl; linw=1;>"
    sounding_lines(ii*6*(1+n_dup)+2) = "  sloc="+sloc+"; sndg; hodo"
    sounding_lines(ii*6*(1+n_dup)+3) = "feld=tmc; ptyp=sc; sloc="+sloc+"; colr=red; linw=2"
    sounding_lines(ii*6*(1+n_dup)+4) = "feld=tdp; ptyp=sc; sloc="+sloc+"; colr=blue; linw=2"
    sounding_lines(ii*6*(1+n_dup)+5) = end_line
    if (dup_snding.eq."yes") then
     sounding_lines(ii*6*(1+n_dup)+6) = "feld=tic; ptyp=sb; hodo"
     sounding_lines(ii*6*(1+n_dup)+7) = "feld=uuu,vvv; ptyp=sv; colr=dark.green; fulb=5mps; vvms=.03; nttl; linw=1;>"
     sounding_lines(ii*6*(1+n_dup)+8) = "  sloc="+sloc+"; hodo"
     sounding_lines(ii*6*(1+n_dup)+9) = "feld=tmc; ptyp=sc; sloc="+sloc+"; colr=red; linw=2"
     sounding_lines(ii*6*(1+n_dup)+10)= "feld=tdp; ptyp=sc; sloc="+sloc+"; colr=blue; linw=2"
     sounding_lines(ii*6*(1+n_dup)+11)= "==========================================================================="
    end if
   end do
   delete(sounding_locs)
   temp = all_lines
   delete(all_lines)
   all_lines = array_append_record(temp,sounding_lines,0)
   delete(temp)
   delete(sounding_lines)

   ; Output each namelist file for each season
   outfile = "testMdomain"+sprinti("%0.1i",hh+1)+"."+seasons(gg)+"."+range_location
   asciiwrite(outfile,all_lines)
   print("Created file: "+outfile)
   delete(all_lines)

  end do  ; end loop for each domain
 end do  ; end loop for each season

end

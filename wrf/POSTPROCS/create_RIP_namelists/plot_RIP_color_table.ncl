load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

; This program creates a plot of the RIP color palette

begin

; Read in the color table
 ;infile = "/raid4/deirdre/CVS_reference.wrfv3/cvs/apps/4dwx/RTFDDA/src/RT_RIP4/color.tbl"   ; Use default color table
 infile = "rip_color.asc"                                                                   ; Use another color table
 data = asciiread (infile,-1,"string")
 n_lines = dimsizes(data)
 color_names = str_get_field(data(6:n_lines-2),1,"|")
 n_colors = dimsizes(color_names)
 RR = stringtofloat(str_get_field(data(6:n_lines-2),2,"|"))
 GG = stringtofloat(str_get_field(data(6:n_lines-2),3,"|"))
 BB = stringtofloat(str_get_field(data(6:n_lines-2),4,"|"))
 RGB = new((/n_colors,3/),"float")
 RGB(:,0) = RR
 RGB(:,1) = GG
 RGB(:,2) = BB

; Plot the colormap
 PlotName = "RIPcolormap"
 wks = gsn_open_wks("ps",PlotName) ; open a ps file
 gsn_define_colormap(wks,RGB)           ; change colormap 
 gsn_draw_colormap(wks)                 ; draw new colormap
 print("Created plot: "+PlotName+".ps")

end

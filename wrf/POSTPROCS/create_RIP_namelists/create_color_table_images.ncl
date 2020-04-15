;************************************************
; This program creates color scale images for
; each RIP color scale that are displayed in the
; web form
;************************************************

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; Read in RIP color palette
 color_data = asciiread("rip_color.asc",-1,"string")  ; New table
 n_colors = dimsizes(color_data)-5
 color_data2 = color_data(4:3+n_colors)
 delete(color_data)
 color_names = str_strip(str_get_field(color_data2,1,"|"))
 color_red   = stringtofloat(str_strip(str_get_field(color_data2,2,"|")))
 color_green = stringtofloat(str_strip(str_get_field(color_data2,3,"|")))
 color_blue  = stringtofloat(str_strip(str_get_field(color_data2,4,"|")))
 color_triplets = new((/n_colors,3/),"float")
 color_triplets(:,0) = color_red
 color_triplets(:,1) = color_green
 color_triplets(:,2) = color_blue
 delete(color_data2)
 delete(color_red)
 delete(color_green)
 delete(color_blue)

; Read in the color scales used in the RIP plots
 color_data = asciiread("color_names.asc",-1,"string")
 color_data2 = color_data(4:)
 color_scale = str_strip(str_get_field(color_data2,1,";"))
 color_scale_names = str_strip(str_get_field(color_data2,2,";"))
 n_color_scales = dimsizes(color_scale)

; Loop through all the color scales
 do ii=0,n_color_scales-1

; Get all of the colors from each color scale, including those
; ("Cust" and "Uniform") with values embedded within them
; The "Cust" color scales have values embedded within them
; indicating at which levels to switch colors.  This color
; scale type uses the same colors in every plot, no matter
; the max and/or min values.
; The "Uniform" color scales are similar to the "Cust" scales,
; except that the lowest and highest colors are determined
; from the values within the very large color scale.  This
; is most useful for upper air T, as there is a very wide 
; range of values and a need for consistent color usage 
; between all of the vertical levels
  n_names = str_fields_count(color_scale_names(ii),",")
  csn = new(n_names,"string")
  do jj=0,n_names-1
   csn(jj) = str_strip(str_get_field(color_scale_names(ii),jj+1,","))
  end do
  if (isStrSubset(color_scale(ii),"Cust").or.isStrSubset(color_scale(ii),"Uniform")) then
   csn_temp = onedtond(csn,(/(n_names/2),2/))
   delete(csn)
   csn = csn_temp(:,1)
   delete(csn_temp)
   n_names = n_names/2
  end if

; Assign the RGB color triplets from the RIP color palette
; to the appropriate colors in the color scales
  output_color_triplets = new((/n_names,3/),"float")
  do jj=0,n_names-1
   xx = ind(color_names.eq.csn(jj))
   output_color_triplets(jj,:) = color_triplets(xx,:)
  end do

; Start getting things ready to create the color scale images
  default_plot_name = "colormap"
  wks = gsn_open_wks("ps",default_plot_name)
  gsn_define_colormap(wks,output_color_triplets)      ; Set the color map.
  delete(output_color_triplets)

;
; Define some labelbar resources.
;
   lbres                   = True
   lbres@vpWidthF          = 0.20    ; Width of labelbar
   lbres@vpHeightF         = 0.05    ; Height of labelbar
   lbres@lbAutoManage      = False   ; Allows us to set more labelbar resources
   lbres@lbOrientation     = "Horizontal"
   lbres@lbMonoFillPattern = True
   lbres@lbFillColors      = ispan(0,n_names-1,1)
   lbres@lbPerimOn         = False   ; Turn off box around labelbar.
   lbres@lbBoxLinesOn      = False
   labels = "Color ~C~   " + ispan(0,n_names-1,1)   ; Labels for boxes.
   labels = "" ; remove all labels

; Create the color table plot
   gsn_labelbar_ndc(wks,n_names,labels,0.02,0.20,lbres)  ; Draw a labelbar

; Crop and convert the color scale image to a jpg image
   system("convert "+default_plot_name+".ps -crop 106x7+47+567 "+color_scale(ii)+".jpg")
   system("rm -f "+default_plot_name+".ps")

   print("Created output color scale: "+color_scale(ii)+".jpg")

  delete(csn)
  delete(lbres@lbFillColors)
  delete(labels)

 end do

end

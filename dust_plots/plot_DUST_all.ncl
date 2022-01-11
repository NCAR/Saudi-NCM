;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"

begin

  if (.not. isvar("file_in")) then
       print(" ")
       print(" You did not define the input wrf file --> EXIT")
       exit
  end if

 ff = addfile(file_in+".nc","r")

;sfile = "DUST_ALL"
 sfile = "DUSTall"
 var = ff->DUST_1(0,:,:,:) + ff->DUST_2(0,:,:,:) + \
       ff->DUST_3(0,:,:,:) + ff->DUST_4(0,:,:,:) + \
       ff->DUST_5(0,:,:,:)
 ave_dust = dim_sum_n(var,0) ; vertical integrate
;stitle = "Dust concentration (ug/kg, 0.5-8.0um effective radius)"
 stitle = "Vertically integrated dust concentration (ug/kg, 0.5-8.0 um)"

 ymdt = chartostring(ff->Times)

; create plots

;wks=gsn_open_wks("epsi","d"+dom+"_"+sfile+"_"+ymdt)
 wks=gsn_open_wks("epsi","d"+dom+"_"+sfile+"_"+nfcst+"hfcst")
;gsn_define_colormap(wks,"WhBlGrYeRe")  ; Change color map.
;gsn_define_colormap(wks,"GMT_copper")  ; Change color map.
 gsn_define_colormap(wks,"DUST_color")  ; Change color map.
 gsn_reverse_colormap(wks)


 res                    = True
 res@cnFillOn           = True
 res@cnLinesOn          = False
 res@cnLineLabelsOn     = False
 res@cnInfoLabelOn      = False
 res@gsnSpreadColors            = True
;res@gsnDraw            = False
;res@gsnFrame           = False

 res@gsnAddCyclic       = False
 res@lbOrientation	= "vertical"

 res@gsnLeftString      = ""
 res@gsnRightString     = ""
 res@gsnCenterString    = ""

 res@mpFillOn                 = False
 res@mpOutlineDrawOrder       = "PostDraw"
 res@mpOutlineBoundarySets    = "AllBoundaries"
 res@mpOutlineSpecifiers      = (/"Saudi Arabia","Yemen","Oman","Qatar","Kuwait","Iran","Iraq","Turkey","Egypt","Sudan"/)
 res@mpDataBaseVersion        = "mediumres"
 res@mpDataSetName            = "Earth..4"

 WRF_map_c(ff,res,0)
 res@tfDoNDCOverlay     = True
 res@pmTickMarkDisplayMode  = "Always"
 res@tmXTOn     = False
 res@tmYROn     = False

; manually set contour levels

 cnLevels = (/2000,4000,6000,8000,10000,12000,14000,16000,18000,20000,22000,24000,26000,28000,30000/)
 res@cnLevelSelectionMode = "ExplicitLevels"
 res@cnLevels             = cnLevels

 res@tiMainFontHeightF  = 0.015

 res@tiMainString       = stitle+" valid at "+ymdt+"Z"
 plot = gsn_csm_contour_map(wks,ave_dust,res)

;draw(plot)
;frame(wks)

;system("convert d"+dom+"_"+sfile+"_"+ymdt+".epsi d"+dom+"_"+sfile+"_"+ymdt+".jpg")
 system("convert -trim -density 120 d"+dom+"_"+sfile+"_"+nfcst+"hfcst.epsi d"+dom+"_"+sfile+"_"+nfcst+"hfcst.png")
 system("rm -f d"+dom+"_"+sfile+"_"+nfcst+"hfcst.epsi")

end

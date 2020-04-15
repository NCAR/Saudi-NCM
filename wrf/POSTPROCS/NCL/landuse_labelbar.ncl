load "/usr/local/ncarg/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "/usr/local/ncarg/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  wks = gsn_open_wks("eps","landuse_label")          ; open a workstation
  colors = (/"white","black", \
             "Purple",      \     ;  1: Urban and Built-Up Land
             "LightGoldenRod", \  ;  2: Dryland Cropland and Pasture
             "LightGoldenRod3", \ ;  3: Irrigated Cropland and Pasture
             "DarkGoldenRod1", \  ;  4: Mixed Dryland/Irrigated Cropland and Pasture
             "DarkGoldenRod3", \  ;  5: Cropland/Grassland Mosaic
             "DarkGoldenRod4", \  ;  6: Cropland/Woodland Mosaic
             "DarkOliveGreen1", \ ;  7: Grassland
             "DarkOliveGreen3", \ ;  8: Shrubland
             "DarkOliveGreen4", \ ;  9: Mixed Shrubland/Grassland
             "DarkOliveGreen", \  ; 10: Savanna
             "PaleGreen1", \      ; 11: Deciduous Broadleaf Forest
             "PaleGreen3", \      ; 12: Deciduous Needleleaf Forest
             "ForestGreen", \     ; 13: Evergreen Broadleaf Forest
             "DarkGreen", \       ; 14: Evergreen Needleleaf Forest
             "Chartreuse", \      ; 15: Mixed Forest
             "RoyalBlue",   \     ; 16: Water Bodies
             "CornFlowerBlue"  , \; 17: Herbaceous Wetland
             "Cyan", \            ; 18: Wooded Wetland
             "LightSalmon4", \    ; 19: Barren or Sparsely Vegetated
             "SlateGray1", \      ; 20: Herbaceous Tundra
             "SlateGray3", \      ; 21: Wooded Tundra
             "SlateGray", \       ; 22: Mixed Tundra
             "Tan", \             ; 23: Bare Ground Tundra
             "Thistle1", \        ; 24: Snow or Ice
             "Thistle3", \        ; 25: Playa
             "Thistle4", \        ; 26: Lava
             "Pink2" \            ; 27: White Sand
             /)
  gsn_define_colormap(wks, colors)  

  nboxes = 27

  fill_colors = ispan(2,nboxes+1,1)
  labels =(/" 1: Urban and Built-Up Land",\
           " 2: Dryland Cropland and Pasture",\
           " 3: Irrigated Cropland and Pasture",\
           " 4: Mixed Dryland/Irrigated Cropland and Pasture",\
           " 5: Cropland/Grassland Mosaic",\
           " 6: Cropland/Woodland Mosaic",\
           " 7: Grassland",\
           " 8: Shrubland",\
           " 9: Mixed Shrubland/Grassland",\
           "10: Savanna",\
           "11: Deciduous Broadleaf Forest",\
           "12: Deciduous Needleleaf Forest",\
           "13: Evergreen Broadleaf Forest",\
           "14: Evergreen Needleleaf Forest",\
           "15: Mixed Forest",\
           "16: Water Bodies",\
           "17: Herbaceous Wetland",\
           "18: Wooded Wetland",\
           "19: Barren or Sparsely Vegetated",\
           "20: Herbaceous Tundra",\
           "21: Wooded Tundra",\
           "22: Mixed Tundra",\
           "23: Bare Ground Tundra",\
           "24: Snow or Ice",\
           "25: Playa",\
           "26: Lava",\
           "27: White Sand"/)
  
  lbres = True                    ; Indicate you want to set some resources.
;;  lbres@lbOrientation         = "Horizontal"      ; ncl default is vertical
  lbres@vpWidthF = 0.50
  lbres@vpHeightF = 0.88
  lbres@lbAutoManage          = False
  lbres@lbPerimOn             = False
  lbres@lbLabelFontHeightF    = .055              ; default is HUGE
  lbres@lbLabelAlignment      = "BoxCenters"
  lbres@lbLabelPosition       = "Right"
  lbres@lbLabelJust           = "CenterLeft"
  lbres@lbMonoFillPattern     = True
  lbres@lbFillColors          = fill_colors(::-1)

  gsn_labelbar_ndc(wks,nboxes,labels(::-1),0.2,0.95,lbres)   ; Draw labelbar.
;  gsn_labelbar_ndc(wks,nboxes,lblabels,0.125,0.125,lbres)
  frame(wks)                                     ; Advance frame.
end

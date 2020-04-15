load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "/home/hahmann/HRLDAS/advance_ccyymmddhh.ncl"

begin

  Range = "DPG"
  Domain = 3
  FileName = "/d1/HRLDAS/CONSTANT_DATA/TERRAIN_DOMAIN"+Domain+"."+Range+".nc"
;  FileName = "/d3/GREECE/TERRAIN/TERRAIN_DOMAIN"+Domain+"."+Range+".nc"
  ff = addfile(FileName,"r")
  xlat = ff->LATITCRS(0,:,:)
  xlon = ff->LONGICRS(0,:,:)
  dims = dimsizes(xlat)
  nlat = dims(0)
  nlon = dims(1)
  terrain = ff->TERRAIN(0,:,:)
  landuse = ff->LAND_USE(0,:,:)

  wks = gsn_open_wks("ps","QCplot")          ; open a workstation

  res = True
  res@cnLinesOn          = False
  res@cnFillOn           = True               ; color plot desired
  res@cnLineLabelsOn     = False              ; turn off contour lines
  res@mpPerimOn          = True ; draw box around map
  res@mpOutlineDrawOrder = "PostDraw"
;  res@mpDataBaseVersion  = "HighRes"
;  res@cnFillDrawOrder    = "Predraw"  ; draw contours first
;  res@mpOceanFillColor   = 0            ; array index in color map
;  res@mpLandFillColor    = -1          ; for land, ocean, and inland h20
;  res@mpInlandWaterFillColor = 0
;  res@pmTickMarkDisplayMode = "Always"

  res@cnRasterModeOn     = True
  res@cnLevelSelectionMode = "ManualLevels"
  res@cnMinLevelValF   = 1.5  ; set min contour level
  res@cnMaxLevelValF   = 26.5  ; set max contour level
  res@cnLevelSpacingF  = 1.0 ; set contour spacing
  res@lbLabelBarOn       = False

  if (ff@map_proj .eq. 1) then
    res@mpProjection       = "LambertConformal"
    res@mpLambertParallel1F = ff@truelat1
    res@mpLambertParallel2F = ff@truelat2
    res@mpLambertMeridianF = ff@central_longitude
  end if
  if (ff@map_proj .eq. 2) then
    res@mpProjection       = "Stereographic"
    res@mpRelativeCenterLon         = True
    res@mpRelativeCenterLat         = True
    res@mpCenterLatF                = 90. 
    res@mpCenterLonF       = ff@central_longitude
  end if
  if (ff@map_proj .ne. 1 .and. ff@map_proj .ne. 2) then
    print("Do not know projection= "+ff@map_proj)
    exit
  end if

  res@gsnAddCyclic       = False
  res@mpLimitMode        = "Corners" ; choose range of map
  res@tfDoNDCOverlay     = True
  res@mpLeftCornerLatF   = xlat(0,0)
  res@mpLeftCornerLonF   = xlon(0,0)
  res@mpRightCornerLatF  = xlat(nlat-2,nlon-2)
  res@mpRightCornerLonF  = xlon(nlat-2,nlon-2)

  res@gsnDraw     = False ; so we can add poly stuff
  res@gsnFrame    = False

  ff = addfile(QCFile,"r")
  res@gsnLeftString = "SFC Obs - "+Range
  res@gsnRightString = ff@start_date
  res@gsnMaximize      = True

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
;;  gsn_merge_colormaps(wks,colors,"wgne15")
  map = gsn_csm_contour_map(wks,landuse(:nlat-2,:nlon-2),res)

  date = ff->date

  latitude = ff->latitude
  longitude = ff->longitude
  platform = ff->platform
  id = ff->id
  name = ff->name
  printVarSummary(name)
  is_surface = ff->is_surface
;  indx = ind(chartostring(name(:,0:3)).eq."SAMS") 
;  print(date(indx,:)+"  "+platform(indx,:)+"  "+id(indx,:)+"  "+name(indx,:))
;  delete(indx)

;  print(name(:,0:3)+"  "+platform+"  "+id+"  "+name)
 
  mres = True
  mres@gsMarkerColor     = 1
  mres@gsMarkerIndex     = 16       ; open circle
  mres@gsMarkerSizeF     = 0.01    ; twice normal size

  nn = dimsizes(latitude)
  print("NN= "+nn)
  marker = new(nn,"graphic")

  indx = ind(latitude .ge. xlat(0,0) .and. latitude .le. xlat(nlat-2,nlon-2) \
         .and. longitude .ge. xlon(0,0) .and. longitude .le. xlon(nlat-2,nlon-2))
  if (.not.all(ismissing(indx))) then
    print(name(indx,0:3)+"  "+platform(indx,:)+"  "+id(indx,:)+"  "+name(indx,:)+ \
          "  "+latitude(indx)+"   "+longitude(indx))
  end if
  delete(indx)

  indx = ind(is_surface.eq.1)
  if (.not.all(ismissing(indx))) then
;;    print(name(indx,0:3)+"  "+platform(indx,:)+"  "+id(indx,:)+"  "+name(indx,:))
    marker(indx) = gsn_add_polymarker(wks,map,longitude(indx),latitude(indx),mres)
  end if
  delete(indx)

;  mres@gsMarkerColor     = 1
;  indx = ind(is_surface.ne.1)
;  if (.not.all(ismissing(indx))) then
;    marker(indx) = gsn_add_polymarker(wks,map,longitude(indx),latitude(indx),mres)
;  end if
;  delete(indx)

;  mres@gsMarkerColor     = 35
;  indx = ind(chartostring(name(:,0:3)).eq."SAMS") 
;  if (.not.all(ismissing(indx))) then
;    marker(indx) = gsn_add_polymarker(wks,map,longitude(indx),latitude(indx),mres)
;  end if

  draw(map)
  frame(wks)

end

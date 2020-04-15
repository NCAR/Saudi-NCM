load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "/home/hahmann/HRLDAS/advance_ccyymmddhh.ncl"

begin

  Range = "GRM" ;;"DPG"
  Domain = 1 ;3
;  FileName = "/d1/HRLDAS/CONSTANT_DATA/TERRAIN_DOMAIN"+Domain+"."+Range+".nc"
  FileName = "/d3/GREECE/TERRAIN/TERRAIN_DOMAIN"+Domain+"."+Range+".nc"
  ff = addfile(FileName,"r")
  xlat = ff->LATITCRS(0,:,:)
  xlon = ff->LONGICRS(0,:,:)
  dims = dimsizes(xlat)
  nlat = dims(0)
  nlon = dims(1)
  terrain = ff->TERRAIN(0,:,:)

  wks = gsn_open_wks("ps","QCplot")          ; open a workstation

  res = True
  res@cnLinesOn          = False
  res@cnFillOn           = True               ; color plot desired
  res@cnLineLabelsOn     = False              ; turn off contour lines
;  res@cnRasterModeOn     = True
  res@mpPerimOn          = True ; draw box around map
  res@mpOutlineBoundarySets = "National"
  res@mpOutlineDrawOrder = "PostDraw"
  if (Domain .gt. 2) then
    res@mpDataBaseVersion  = "HighRes"
  else
    res@mpDataBaseVersion  = "MediumRes"
  end if
  res@cnFillDrawOrder    = "Predraw"  ; draw contours first
  res@mpOceanFillColor   = 0            ; array index in color map
  res@mpLandFillColor    = -1          ; for land, ocean, and inland h20
  res@mpInlandWaterFillColor = 0

  res@cnLevelSelectionMode = "ManualLevels"
  res@cnMinLevelValF     = 50.
  res@cnMaxLevelValF     = 3000.
  res@cnLevelSpacingF    = 50.
  res@gsnSpreadColors    = True
  res@gsnSpreadColorStart= 12
  res@gsnSpreadColorEnd  = 31
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
  res@gsnLeftString = "Upper-air observations - "+Range
  res@gsnRightString = ff@start_date
  res@gsnMaximize      = True

  gsn_merge_colormaps(wks,"gsdtol","wgne15")
  map = gsn_csm_contour_map(wks,terrain(:nlat-2,:nlon-2),res)

  date = ff->date

  latitude = ff->latitude
  longitude = ff->longitude
  platform = ff->platform
  id = ff->id
  name = ff->name
  printVarSummary(name)
;  is_surface = ff->is_surface
;  indx = ind(chartostring(name(:,0:3)).eq."SAMS") 
;  print(date(indx,:)+"  "+platform(indx,:)+"  "+id(indx,:)+"  "+name(indx,:))
;  delete(indx)

  print(name(:,0:3)+"  "+platform+"  "+id+"  "+name)
 
  mres = True
  mres@gsMarkerColor     = 40
  mres@gsMarkerIndex     = 16       ; open circle
  mres@gsMarkerSizeF     = 0.01    ; twice normal size

  nn = dimsizes(latitude)
  print("NN= "+nn)
  marker = new(nn,"graphic")

;;  indx = ind(is_surface.eq.1)
  marker = gsn_add_polymarker(wks,map,longitude,latitude,mres)
;  delete(indx)

;  mres@gsMarkerColor     = 38
;  indx = ind(is_surface.ne.1)
;  marker(indx) = gsn_add_polymarker(wks,map,longitude(indx),latitude(indx),mres)
;  delete(indx)

;  mres@gsMarkerColor     = 36
;  indx = ind(chartostring(name(:,0:3)).eq."SAMS") 
;  marker(indx) = gsn_add_polymarker(wks,map,longitude(indx),latitude(indx),mres)


  draw(map)
  frame(wks)

end

;------------------------------------------------------------------------------#
; Copyright UCAR (c) 1992 - 2006.
; University Corporation for Atmospheric Research (UCAR),
; National Center for Atmospheric Research (NCAR),
; Research Applications Laboratory (RAL),
; P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
;------------------------------------------------------------------------------#
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "advance_ccyymmddhh.ncl"
;------------------------------------------------------------------------------#

begin

  Range = "GCAT"
  Domain = 1
  FileName = "wrfstatic_d0"+Domain+".nc"

  ff = addfile(FileName,"r")
; XLAT(Time, south_north, west_east) ;
  xlat = ff->XLAT(0,:,:)
  xlon = ff->XLONG(0,:,:)
  dims = dimsizes(xlat)
  nlat = dims(0)
  nlon = dims(1)
  terrain = ff->HGT(0,:,:)

  wks = gsn_open_wks("ncgm","QCplot")          ; open a workstation

  res = True
  res@cnLinesOn          = False
  res@cnFillOn           = True               ; color plot desired
  res@cnLineLabelsOn     = False              ; turn off contour lines
; res@cnRasterModeOn     = True
  res@cnFillDrawOrder    = "Predraw"   ; draw contours first

  res@mpPerimOn          = True ; draw box around map
  res@mpOutlineDrawOrder = "PostDraw"
; res@mpDataBaseVersion  = "HighRes"   ; High resolution coastline
  res@mpOutlineBoundarySets = "AllBoundaries" ; add state boundaries  
  res@mpOceanFillColor   = 0           ; array index in color map
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

  if (isatt(ff,"MAP_PROJ")) then
     if (ff@MAP_PROJ.eq.1) then
        res@mpProjection = "LambertConformal"
     end if
     if (ff@MAP_PROJ.eq.2) then
        res@mpProjection = "Stereographic"
     end if
     if (ff@MAP_PROJ.eq.3) then
        res@mpProjection = "Mercator"
     end if
  else
     print ("WRF_mapProj: no MAP_PROJ attribute")
  end if

  res@gsnAddCyclic       = False
  res@mpLimitMode        = "Corners" ; choose range of map
  res@tfDoNDCOverlay     = True
  res@mpLeftCornerLatF   = xlat(0,0)
  res@mpLeftCornerLonF   = xlon(0,0)
  res@mpRightCornerLatF  = xlat(nlat-1,nlon-1)
  res@mpRightCornerLonF  = xlon(nlat-1,nlon-1)
  res@mpCenterLonF       = ff@STAND_LON ;;f@CEN_LON
  res@mpCenterLatF       = ff@CEN_LAT

  if (res@mpProjection.eq."LambertConformal") then
    res@mpLambertParallel1F   = ff@TRUELAT1
    res@mpLambertParallel2F   = ff@TRUELAT2
    res@mpLambertMeridianF    = ff@STAND_LON  ;;f@CEN_LON
  end if

  res@gsnDraw     = False ; so we can add poly stuff
  res@gsnFrame    = False

  delete (ff)

  ff = addfile("upper-air_obs_r_2003-12-30_00_mpl.nc","r")
  res@gsnLeftString = "Single-level observations - "+Range
  res@gsnRightString = ff@start_date
  res@gsnMaximize      = True

  gsn_merge_colormaps(wks,"gsdtol","wgne15")
  map = gsn_csm_contour_map(wks,terrain(:nlat-2,:nlon-2),res)

  date = ff->date
  printVarSummary (date)

  latitude = ff->latitude
  longitude = ff->longitude
  platform = ff->platform
  id = ff->id
  name = ff->name
  printVarSummary(name)

  is_surface = ff->is_surface
  indx = ind(chartostring(name(:,0:2)).eq."SFC") 
	;if (.not. all (ismissing (indx))) then
  ;print(date(indx,:)+"  "+platform(indx,:)+"  "+id(indx,:)+"  "+name(indx,:))
	;end if
  ;delete(indx)

  print(name(:,0:3)+"  "+platform+"  "+id+"  "+name)
	indx = ind(latitude .ge. xlat(0,0) .and. latitude .le. xlat(nlat-1,nlon-1) \
	       .and. longitude .ge. xlon(0,0) .and. longitude .le. \
				 xlon(nlat-1,nlon-1))
	if (.not. all (ismissing (indx))) then
  print(date(indx,:)+"  "+platform(indx,:)+"  "+id(indx,:)+"  "+name(indx,:)+"  "+latitude+"  "+longitude)
	end if
  delete(indx)

	; Make up a bogus observation
;latitude(0) = xlat(50,50) 
;longitude(0) = xlon(50,50)
 
  mres = True
  mres@gsMarkerColor     = 40
  mres@gsMarkerIndex     = 16       ; open circle
  mres@gsMarkerSizeF     = 0.01    ; twice normal size

  nn = dimsizes(latitude)
  print("NN= "+nn)
  marker = new(nn,"graphic")

  indx = ind(is_surface.eq.1)
  if (.not. all (ismissing (indx))) then
  print (is_surface)
  marker(indx) = gsn_add_polymarker(wks,map,longitude(indx),latitude(indx),mres)
  end if
  delete (indx)
	print(latitude+"  "+longitude+"  "+name(:,0:9))
  ;marker = gsn_add_polymarker(wks,map,longitude,latitude,mres)

  mres@gsMarkerColor     = 38
  indx = ind(is_surface.ne.1)
  if (.not. all (ismissing (indx))) then
  marker(indx) = gsn_add_polymarker(wks,map,longitude(indx),latitude(indx),mres)
  end if
  delete(indx)

  mres@gsMarkerColor     = 36
  indx = ind(chartostring(name(:,0:2)).eq."SFC") 
  if (.not. all (ismissing (indx))) then
  marker(indx) = gsn_add_polymarker(wks,map,longitude(indx),latitude(indx),mres)
  end if


  draw(map)
  frame(wks)

end

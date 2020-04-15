load "/$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "/$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "/$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "./RTFDDAUser.ncl"

;===============================================================
; NCL script creates landuse map from the contents of a WRF 
; "geo" file.  To run requires env variable RANGE to be set to
; desired ATEC range.
; Andrea Hahmann, October 2007
;===============================================================
begin

  if (.not.isdefined("d")) then
    print("Domain not defined... default to d=1")
    d = 1
  end if
  Range = getenv("RANGE")
  print("Plotting landuse for: ")
  print("   RANGE:  "+Range)
  print("   DOMAIN: "+d)
  FileName = "/d1/WRF/"+Range+"/geo_em.d0"+d+".nc"
  ff = addfile(FileName,"r")

  xlat = ff->XLAT_M(0,:,:)
  xlon = ff->XLONG_M(0,:,:)
  dims = dimsizes(xlat)
  nlat = dims(0)                                ; assign # lat/lon points
  nlon = dims(1)
  lonE = xlon(nlat-1,nlon-1)
  lonW = xlon(0,0)

  dlon = (lonE-lonW)
  print("DLON= "+dlon)

  res                    = True                    ; plot mods desired
  res@cnLinesOn          = False
  res@cnFillOn           = True               ; color plot desired
  res@cnRasterModeOn     = True
  res@cnLineLabelsOn     = False              ; turn off contour lines

  res@mpDataBaseVersion     = "Ncarg4_1"          ; higher res data base
  if (d .eq. 4) then
    res@mpDataBaseVersion  = "HighRes"
  end if

  if (d .eq. 1) then
    res@mpGridAndLimbOn      = True
  end if

  res@mpGridSpacingF       = 1.            ; change to match labels
  if (dlon.gt.20) then
    res@mpGridSpacingF       = 5.0            ; change to match labels
  end if
  if (dlon.ge.10 .and. dlon.lt.20.) then
    res@mpGridSpacingF       = 2.5            ; change to match labels
  end if
  res@mpGridLineDashPattern = 1
  
  if (ff@MAP_PROJ .eq. 1) then
    res@mpProjection       = "LambertConformal"
    res@mpLambertParallel1F = ff@TRUELAT1
    res@mpLambertParallel2F = ff@TRUELAT2
    res@mpLambertMeridianF  = ff@STAND_LON
  end if
  if (ff@MAP_PROJ .eq. 2) then
    res@mpProjection        = "Stereographic"
    res@mpCenterLonF        = ff@STAND_LON
    res@mpCenterLatF        = ff@CEN_LAT ;;TRUELAT1
  end if
  if (ff@MAP_PROJ .eq. 3) then
    res@mpProjection        = "Mercator"
    res@mpCenterLonF        = ff@STAND_LON
    res@mpCenterLatF        = 0.0
  end if
  print("Proj: "+res@mpProjection )

  res@mpPerimOn          = True ; draw box around map
  res@mpPerimDrawOrder   = "PostDraw"
  res@mpOutlineOn           = True                ; turn on map outline
  res@mpOutlineBoundarySets = "GeophysicalAndUSStates"

  PlotName = "WRF_landuse_"+Range+"_domain"+d
  PlotType = "eps"
  print("PlotName: "+PlotName+"."+PlotType)
  wks = gsn_open_wks(PlotType ,PlotName)    ; ps,pdf,x11,ncgm,eps

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
  res@cnLevelSelectionMode = "ManualLevels"
  res@cnMinLevelValF   = 1.5  ; set min contour level
  res@cnMaxLevelValF   = 26.5  ; set max contour level
  res@cnLevelSpacingF  = 1.0 ; set contour spacing

  res@cnInfoLabelOn     = False           ; turn off cn info label

  res@gsnMaximize      = True

  landuse  = ff->LU_INDEX(0,:,:)
  landuse@lat2d = xlat
  landuse@lon2d = xlon
  res@tfDoNDCOverlay     = True

  print("Max/Min: "+min(landuse)+"  "+max(landuse))
  
  res@mpLimitMode       = "Corners" ; choose range of map
  res@mpLeftCornerLatF  = xlat(0,0)
  res@mpLeftCornerLonF  = xlon(0,0)
  res@mpRightCornerLatF = xlat(nlat-1,nlon-1)
  res@mpRightCornerLonF = xlon(nlat-1,nlon-1)
  res@pmTickMarkDisplayMode = "Always"

  res@gsnRightString = " "
  res@gsnLeftStringFontHeightF = 0.025
  
  res@gsnLeftString = Range + " WRF, DOMAIN "+d+ \
      ", ~F33~D~F21~x="+sprintf("%4.1f",ff@DX/1000.)+" km"

  res@lbLabelBarOn      = False
  res@lbTitleString = "land use category"
  res@lbTitlePosition      = "bottom"
  res@lbLabelPosition  = "Center"             ; label position
  res@lbLabelAlignment = "BoxCenters"         ; label orientation
  res@lbLabelStrings =  ispan(1,27,1)
  res@lbTitleFontHeightF = 0.018

  res@gsnAddCyclic = False
  res@gsnFrame          = False
  res@gsnDraw           = False

  map = gsn_csm_contour_map(wks,landuse,res)

  if (d .gt. 2. .and. \
       (Range .eq. "ATC" .or. Range .eq. "DPG" .or. \
       Range .eq. "WSMR" .or. Range .eq. "YPG")) then
    bndry_file = "/d1/WRF/"+Range+"/"+Range+ "_map.ascii"
    lat_out  = new((/1000/),float)    ; Fix this later, set 1000 here
    lon_out  = new((/1000/),float)    ; and pass it in
    npts_out = new((/1000/),integer)

    read_map_ascii(bndry_file, lat_out, lon_out, npts_out)

    igood = ind(.not.ismissing(lat_out))  ; indices of non-missing values
    ipts = ind(.not.ismissing(npts_out))  ; indices of non-missing values
    if (.not.all(ismissing(igood)))
      bnd_lat = lat_out(igood)
      bnd_lon = lon_out(igood)
    else
      print ("bnd_lat, bnd_lon not created: all values are missing")
    end if

    if (.not.all(ismissing(ipts)))
      nptsline = npts_out(ipts)
    else
      print ("nptsline not created: all values are missing")
    end if

    delete (igood)
    delete (ipts)
    npts_dim = dimsizes(nptsline)

    plot_bndry = "True"
    gres                  = True          ; polyline mods desired
    gres@gsLineThicknessF = 2.0           ; line thickness
    gres@gsLineColor      = "black"       ; line color
    jcount = 0
    dum = new(npts_dim,graphic)
    do i = 0,npts_dim - 1
      npairs = (nptsline(i)/2) - 1
      dum(i) = gsn_add_polyline(wks, map, bnd_lon(jcount:jcount+npairs),\
      bnd_lat(jcount:jcount+npairs), gres)
      jcount = jcount + npairs + 1
    end do
  end if
  draw(map)
  frame(wks)

end

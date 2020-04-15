load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

;===============================================================
; NCL script creates surface elevation map from the contents of 
; a WRF "geo" file.  To run requires env variable RANGE to be 
; set to desired ATEC range.
; Andrea Hahmann, October 2007
;===============================================================
begin

  if (.not.isdefined("d")) then
    print("Domain not defined... default to d=1")
    d = 1
  end if

  Range = getenv("RANGE")
  print("Plotting landmask for: ")
  print("   RANGE:  "+Range)
  print("   DOMAIN: "+d)

  diri = "/d1/WRF/"+Range+"/"
  FileName = diri+"geo_em.d01.nc"
  ff = addfile(FileName,"r")
  corner_lats = ff@corner_lats
  corner_lons = ff@corner_lons
  latS = corner_lats(0)
  latN = corner_lats(2)
  lonW = corner_lons(0)
  lonE = corner_lons(2)

  lonW_up = corner_lons(1)
  if (lonW_up .gt. 0.) then
    lonW_up = lonW_up - 360.
  end if
  latN_up = max(ff->XLAT_M(0,:,:))

  dlon = (lonE-lonW)
  print("DLON= "+dlon)

  res                    = True                    ; plot mods desired
  res@cnLinesOn          = False
  res@cnFillOn           = True               ; color plot desired
  res@cnLineLabelsOn     = False              ; turn off contour lines
  res@mpDataBaseVersion     = "Ncarg4_1"          ; higher res data base

  if (d .eq. 1) then
    res@mpGridAndLimbOn      = True
  end if

  if (d .eq. 4) then
    res@mpDataBaseVersion  = "HighRes"
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
    res@mpCenterLatF        = 90. ;;ff@CEN_LAT
  end if
  if (ff@MAP_PROJ .eq. 3) then
    res@mpProjection        = "Mercator"
    res@mpCenterLonF        = ff@STAND_LON
    res@mpCenterLatF        = ff@CEN_LAT
  end if
  print("Proj: "+res@mpProjection )

  res@mpLimitMode        = "Corners" ; choose range of map
  res@tfDoNDCOverlay     = True

  res@mpPerimOn          = True ; draw box around map
  res@mpPerimDrawOrder   = "PostDraw"
  res@mpOutlineOn           = True                ; turn on map outline
  res@mpOutlineBoundarySets = "GeophysicalAndUSStates"
  res@cnFillDrawOrder    = "Predraw"  ; draw contours first
  res@mpOutlineDrawOrder = "PostDraw"
  res@mpOceanFillColor   = 0            ; array index in color map
  res@mpInlandWaterFillColor     = 0
  res@mpLandFillColor    = -1          ; for land, ocean, and inland h20
  res@gsnSpreadColors    = True             ; use full range of colormap

  dom = sprinti("%i",d)
  PlotName = "WRF_topo_"+Range+"_domain"+dom
  PlotType = "eps"
  print("PlotName: "+PlotName+"."+PlotType)
  wks = gsn_open_wks(PlotType ,PlotName)    ; ps,pdf,x11,ncgm,eps
  gsn_define_colormap(wks,"temp1") ; select color map

  res@cnInfoLabelOn     = False           ; turn off cn info label
  res@gsnFrame          = False
  res@gsnDraw           = False

  gres                  = True                ; polyline mods desired
  gres@gsLineThicknessF = 3.0                 ; line thickness
  gres@gsLineColor      = "black"               ; line color 

  res@gsnMaximize      = True

  FileName = diri + "geo_em.d0"+dom+".nc"
  ff = addfile(FileName,"r")
  xlat = ff->XLAT_M(0,:,:)
  xlon = ff->XLONG_M(0,:,:)
  dims = dimsizes(xlat)
  nlat = dims(0)                                ; assign # lat/lon points
  nlon = dims(1)
  
  terrain = ff->HGT_M(0,:,:)

  print("Max/Min: "+min(terrain)+"  "+max(terrain))
  
  res@mpLeftCornerLatF  = xlat(0,0)
  res@mpLeftCornerLonF  = xlon(0,0)
  res@mpRightCornerLatF = xlat(nlat-1,nlon-1)
  res@mpRightCornerLonF = xlon(nlat-1,nlon-1)

  res@gsnRightString = " "
  res@gsnLeftStringFontHeightF = 0.025
  res@gsnLeftString = Range + " WRF, DOMAIN "+d+ \
      ", ~F33~D~F21~x="+sprintf("%4.1f",ff@DX/1000.)+" km"

  res@lbLabelStride    = 2
  res@pmTickMarkDisplayMode = "Always"
  res@lbTitleString = "surface elevation (m)"
  res@lbTitlePosition      = "bottom"
  res@lbTitleFontHeightF = 0.015

  res@gsnAddCyclic = False
  map = gsn_csm_contour_map(wks,terrain,res)

  line = new(12,"graphic")
  i = 0

  if (d .lt. 4) then
  do d1=d+1,4

    FileName = diri + "geo_em.d0"+d1+".nc"
    print("FileName: "+FileName)
    if (isfilepresent(FileName)) then
    ff = addfile(FileName,"r")
    var = "SOUTH-NORTH_GRID_DIMENSION"
    nlat = ff@$var$ - 1
    var = "WEST-EAST_GRID_DIMENSION"
    nlon = ff@$var$ - 1
    
    ybottom = ff->XLAT_M(0,0,0:nlon-1)
    xbottom = ff->XLONG_M(0,0,0:nlon-1)
    line(i) = gsn_add_polyline (wks, map, xbottom, ybottom, gres)
    i = i + 1

    yright = ff->XLAT_M(0,0:nlat-1,0)
    xright = ff->XLONG_M(0,0:nlat-1,0)
    line(i) = gsn_add_polyline (wks, map, xright, yright, gres)
    i = i + 1

    ytop = ff->XLAT_M(0,nlat-1,0:nlon-1)
    xtop = ff->XLONG_M(0,nlat-1,0:nlon-1)
    line(i) = gsn_add_polyline (wks, map, xtop, ytop, gres)
    i = i + 1

    yleft = ff->XLAT_M(0,0:nlat-1,nlon-1)
    xleft = ff->XLONG_M(0,0:nlat-1,nlon-1)
    line(i) = gsn_add_polyline (wks, map, xleft, yleft, gres)
    i = i + 1

    delete(ybottom)
    delete(xbottom)
    delete(yright)
    delete(xright)
    delete(ytop)
    delete(xtop)
    delete(yleft)
    delete(xleft)
  
    end if
  end do
  end if

  draw(map)
  frame(wks)
  
end

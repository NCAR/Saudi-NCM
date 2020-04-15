load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "./RTFDDAUser.ncl"

;===============================================================
; NCL script creates landmask map from the contents of a WRF 
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
  print("Plotting landmask for: ")
  print("   RANGE:  "+Range)
  print("   DOMAIN: "+d)

  diri = "/d1/WRF/"+Range+"/"
  FileName = diri+"geo_em.d0"+d+".nc"
  print("FileName+ "+FileName)
  ff = addfile(FileName,"r")

  xlat = ff->XLAT_M(0,:,:)
  xlon = ff->XLONG_M(0,:,:)
  dims = dimsizes(xlat)
  nlat = dims(0)                                ; assign # lat/lon points
  nlon = dims(1)
  delete(dims)
  
  res                    = True                    ; plot mods desired

  res@mpDataBaseVersion     = "Ncarg4_1"          ; higher res data base
  if (d .eq. 4) then
    res@mpDataBaseVersion  = "HighRes"
  end if

  if (d .eq. 1) then
    res@mpGridAndLimbOn      = True
  end if

; Projection information
  if (ff@MAP_PROJ .eq. 1) then
    res@mpProjection       = "LambertConformal"
    res@mpLambertParallel1F = ff@TRUELAT1
    res@mpLambertParallel2F = ff@TRUELAT2
    res@mpLambertMeridianF  = ff@STAND_LON
  end if
  if (ff@MAP_PROJ .eq. 2) then
    res@mpProjection        = "Stereographic"
    res@mpCenterLonF        = ff@STAND_LON
    res@mpCenterLatF        = 90. ;;ff@CEN_LAT ;;TRUELAT1
  end if
  if (ff@MAP_PROJ .eq. 3) then
    res@mpProjection        = "Mercator"
    res@mpCenterLonF        = ff@STAND_LON
    res@mpCenterLatF        = 0.0
  end if
  print("Proj: "+res@mpProjection )
  res@mpLimitMode       = "Corners" ; choose range of map
  res@mpLeftCornerLatF  = xlat(0,0)
  res@mpLeftCornerLonF  = xlon(0,0)
  res@mpRightCornerLatF = xlat(nlat-1,nlon-1)
  res@mpRightCornerLonF = xlon(nlat-1,nlon-1)

  res@mpPerimOn          = True ; draw box around map
  res@mpPerimDrawOrder   = "PostDraw"
  res@mpOutlineOn           = True                ; turn on map outline
  res@mpOutlineBoundarySets = "GeophysicalAndUSStates"

  res@cnLinesOn          = False
  res@cnFillOn           = True               ; color plot desired
  res@cnRasterModeOn     = True
  res@cnLineLabelsOn     = False              ; turn off contour lines

; Countours, labels, etc.
  res@cnLevelSelectionMode = "ExplicitLevels"   ; set explicit contour levels
  res@cnLevels            = (/1.0/)
  res@cnFillColors         = (/4,21/)

  PlotName = "WRF_landmask_"+Range+"_domain"+d
  PlotType = "eps"
  print("PlotName: "+PlotName+"."+PlotType)
  wks = gsn_open_wks(PlotType ,PlotName)    ; ps,pdf,x11,ncgm,eps

  res@gsnMaximize      = True

  landmask  = ff->LANDMASK(0,:,:)
  landmask@lat2d = xlat
  landmask@lon2d = xlon
  res@tfDoNDCOverlay     = True

  print("Max/Min: "+min(landmask)+"  "+max(landmask))

  res@lbTitleString = landmask@description
  res@lbTitlePosition      = "bottom"
  res@lbLabelPosition  = "Center"             ; label position
  res@lbLabelAlignment = "BoxCenters"         ; label orientation
  res@lbLabelStrings =  (/"OCEAN/LAKE","LAND"/) 
  res@lbLabelFontColor = "white"
  res@lbTitleFontHeightF = 0.018

  res@pmTickMarkDisplayMode = "Always"

  res@gsnLeftStringFontHeightF = 0.025
  res@gsnLeftString = Range + " WRF, DOMAIN "+d+ \
      ", ~F33~D~F21~x="+sprintf("%4.1f",ff@DX/1000.)+" km"
  res@gsnRightString = " "

  if (all(landmask.eq.1.)) then
    landmask(0,0) = 0.
  end if
  if (all(landmask.eq.0.)) then
    landmask(0,0) = 1.
  end if
 
  res@gsnFrame          = False
  res@gsnDraw           = False
  map = gsn_csm_contour_map(wks,landmask,res)

  gres                  = True                ; polyline mods desired
  gres@gsLineThicknessF = 3.0                 ; line thickness
  gres@gsLineColor      = "black"               ; line color

  line = new(12,"graphic")
  i = 0
  j = 0

  if (d .lt. 4) then
    d = d + 1
    FileName = diri + "geo_em.d0"+d+".nc"
    print("FileName: "+FileName)
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
  end if
  
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

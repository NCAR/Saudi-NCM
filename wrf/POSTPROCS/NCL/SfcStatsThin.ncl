load "$NCL_LIB/ncarg/nclscripts/csm/contributed.ncl"
load "./StationModel.ncl"
load "./RTFDDAUser.ncl"

; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
function replace_missing(x:float)
begin
  if (.not.ismissing(x)) then
    px = x
  else
    px = -999.
  end if
  px@_FillValue = -999.
  return(px)
end
; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

begin

;==============================================================
;; Test for plot parameters, make sure they are defined

; Range = getenv("RANGE")
  if (.not.isdefined("Range")) then
    print("Range must be defined... exit")
    exit
  end if

  if (.not.isdefined("Domain")) then
    print("Domain not defined... default to Domain=1")
    Domain = 1
  end if

  if (.not.isdefined("Date")) then
    print("Date (ccyymmddhh format) not defined... exiting")
    exit
  end if

  print("Running surface plot for:")
  print("Range : "+Range)
  print("Domain: "+Domain)
  print("Date  : "+Date)

;==============================================================
;; Look for geo files, exit if not available

  GEO_FILES_DIR = getenv("HIRESMAP")
; geo_file_name = GEO_FILES_DIR+"/"+Range+"/geo_em.d0"+Domain+".nc"
  geo_file_name = "geo_em.d0"+Domain+".nc"
  print("Terrain file name: "+geo_file_name)
  if (.not.isfilepresent(geo_file_name)) then
    print(geo_file_name+" not found... exit")
    exit
  end if
  ff_geo = addfile(geo_file_name,"r")

; Read projection parameters and ff_geo field
  corner_lats = ff_geo@corner_lats
  corner_lons = ff_geo@corner_lons
  print(corner_lats+"  "+corner_lons)

  terrain = ff_geo->HGT_M(0,:,:)

  latS = corner_lats(0)
  latN = corner_lats(2)
  lonW = corner_lons(0)
  lonE = corner_lons(2)

  lonW_up = corner_lons(1)
  if (lonW_up .gt. 0.) then
    lonW_up = lonW_up - 360.
  end if
  latN_up = max(ff_geo->XLAT_M(0,:,:))

  dlon = (lonE-lonW)
  print("DLON= "+dlon)

; Diri = "/d1/WRF/"+Range+"/"
  Diri = "./"
; QCFile = Diri+Date+".all.obs_sgl.nc"
  QCFile = Diri+Date+".hourly.obs_sgl.nc"
  print("Reading file: "+QCFile)
  ff = addfile(QCFile,"r")

  wks = gsn_open_wks("eps","SFCplot")          ; open a workstation
  gsn_merge_colormaps(wks,"gsdtol","wgne15")
  ;gsn_draw_colormap(wks) 
  ;exit
  
  res = True
  res@cnLinesOn          = False
  res@cnFillOn           = True               ; color plot desired
  res@cnLineLabelsOn     = False              ; turn off contour lines
  res@cnInfoLabelOn      = False
  res@mpPerimOn          = True ; draw box around map
  res@mpOutlineDrawOrder = "PostDraw"
  res@mpDataBaseVersion  = "MediumRes" ;;"HighRes"
  if (Domain .eq. 4) then
    res@mpDataBaseVersion  = "HighRes"
  end if
  res@cnFillDrawOrder    = "Predraw"  ; draw contours first
  res@mpOceanFillColor   = 0            ; array index in color map
  res@mpLandFillColor    = -1          ; for land, ocean, and inland h20
  res@mpInlandWaterFillColor = 0
  res@gsnSpreadColors    = True

; Terrain in ligh to dark color scale 
; Let the contour level spacing be free
  res@gsnSpreadColorStart= 32 ;;12
  res@gsnSpreadColorEnd  = 12 ;;32
;;  res@cnLevelSpacingF    = 100.
  res@lbLabelBarOn       = False
  res@mpOutlineBoundarySets = "GeophysicalAndUSStates"
  res@pmTickMarkDisplayMode = "Always"
  res@mpGridAndLimbOn      = False          ; default is every 15 deg
  if (Domain .eq. 1) then
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

  res@tmXBMinorOn        = False           ; no lon minior tickmarks
  res@tmYLMinorOn        = False           ; no lon minior tickmarks
  res@tmXBLabelFontHeightF = 0.012         ; change maj lat tm spacing

  if (ff_geo@MAP_PROJ .eq. 1) then
    res@mpProjection       = "LambertConformal"
    res@mpLambertParallel1F = ff_geo@TRUELAT1
    res@mpLambertParallel2F = ff_geo@TRUELAT2
    res@mpLambertMeridianF = ff_geo@STAND_LON
  end if
  if (ff_geo@MAP_PROJ .eq. 2) then
    res@mpProjection       = "Stereographic"
    res@mpRelativeCenterLon = True
    res@mpRelativeCenterLat = True
    res@mpCenterLatF        = 90.
    res@mpCenterLonF       = ff_geo@STAND_LON
  end if
  if (ff_geo@MAP_PROJ .eq. 3) then
     res@mpProjection       = "Mercator"
     res@mpLimitMode        = "Corners"
  end if
  if (ff_geo@MAP_PROJ .ne. 1 .and. ff_geo@MAP_PROJ .ne. 2 .and. ff_geo@MAP_PROJ .ne. 3) then
    print("Do not know projection= "+ff_geo@MAP_PROJ)
    exit
  end if

  res@gsnAddCyclic       = False
  res@mpLimitMode        = "Corners" ; choose range of map
  res@tfDoNDCOverlay     = True
  res@mpLeftCornerLatF   = latS
  res@mpLeftCornerLonF   = lonW
  res@mpRightCornerLatF  = latN
  res@mpRightCornerLonF  = lonE

  res@gsnDraw     = False ; so we can add poly stuff
  res@gsnFrame    = False
  res@gsnMaximize = True
  res@gsnPaperOrientation = "portrait"

  res@gsnStringFontHeightF = 0.018
; res@gsnLeftString = "ATEC "+Range+" Domain "+Domain
  res@gsnLeftString = Range+" Domain "+Domain
  start_date = stringtochar(ff@start_date)
; res@gsnRightString = "Valid time: "+start_date(0:9)+" "+start_date(11:15)
  res@gsnRightString = start_date(0:9)
  map = gsn_csm_contour_map(wks,terrain,res)

; Read in range boundary lat/lon points, only if domain 3 or 4 and not for CRTC
  if (Domain .gt. 2. .and. \
       (Range .eq. "ATC" .or. Range .eq. "DPG" .or. \
       Range .eq. "WSMR" .or. Range .eq. "YPG")) then

    Range_lc = changeCase(Range,"low")

   ;bndry_file = "$CONSTANT_FILES/RIP4/"+Range_lc+ "_map.ascii"
    bndry_file = "$RIP_ROOT/custom_maps/"+Range_lc+ "_map.ascii"

    print("bndry_file = "+bndry_file)

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
  
  latitude = ff->latitude
  longitude = ff->longitude

  indx = ind(longitude.gt.180.) 
  if (.not.all(ismissing(indx))) then
    longitude(indx) = longitude(indx) - 360.
  end if
  delete(indx)

;  platform = ff->platform
;  id = ff->id
;  name = ff->name
;  printVarSummary(name)
  is_surface = ff->is_surface

  date = chartostring(ff->date(:,0:11))
  minutes = stringtointeger(chartostring(ff->date(:,10:11)))
  indx = ind(is_surface.eq.1 .and. \
        (minutes .le. 10 .or. minutes .ge. 50) .and. \
        (latitude .gt. latS .and. latitude .lt. latN_up) .and. \
        (longitude .gt. lonW_up .and. longitude .lt. lonE))
  nrec = dimsizes(.not.ismissing(indx))
  print("There are "+nrec+" surface stations")
  if (nrec.eq.0) then
    print("No stations to plot... exit")
    exit
  end if

  pmres             = True
  pmres@gsMarkerColor = 36 ;;"Blue2"
  pmres@gsMarkerIndex = 16
  pmres@gsMarkerThicknessF = 3.
  pmres@gsMarkerSizeF  = 0.002
  markers = gsn_add_polymarker(wks, map, longitude(indx), latitude(indx), pmres)

  temp = ff->temperature
  temp = temp - 273.15
  dewp = ff->dew_point
  dewp = dewp - 273.15

  rad = 4.*atan(1.0)/180.
  u = ff->u_wind
  v = ff->v_wind
  wspd = 1.9438445*sqrt(u*u + v*v)   ; m/s -> knots
  wdir = atan2(u,v)/rad +180.
  wspd = mask(wspd,(abs(u).gt.150.or.abs(v).gt.150.),False)
  wdir = mask(wdir,(abs(u).gt.150.or.abs(v).gt.150.),False)
;  delete(u)
;  delete(v)
  
  sm = new(nrec,graphic)
  anno = new(nrec,graphic)

; dmax is teh minimum distance between stations allowed for plotting
  dmax = dlon/20.
  if (Domain.eq.4) then
    dmax = 0.001 ;; make number small so all SAMS will be plotted
  end if

  resT = True  ;;; Use station model default options
  resT@ArrowLength = 3.5

  m = 0
  do n=0,nrec-1

    far = True
    dist = 0.
    kk = 1

    k = indx(n)
    if (n.gt.0) then
      delete(dist)
      delete(kk)

      dist = new(n,"float")
      kk = indx(0:n-1)
      dist = sqrt( (latitude(kk)-latitude(k))^2 + \
                   (longitude(kk)-longitude(k))^2 )
      if (min(dist) .lt. dmax) then
        far = False
      end if
    end if

    fmt = "%8.2f"
    miss = -999.

    if (far) then
      ;print("N= "+sprinti(" %0.4i",n)+sprintf(fmt,min(dist)))
      T = replace_missing(temp(k))
      Td = replace_missing(dewp(k))
      ws = replace_missing(wspd(k))
      wd = replace_missing(wdir(k))
      print(date(k)+sprintf(fmt,latitude(k))+sprintf(fmt,longitude(k))+ \
            sprintf(fmt,T)+sprintf(fmt,Td)+sprintf(fmt,ws)+sprintf(fmt,wd)+ \
            sprintf(fmt,u(k))+sprintf(fmt,v(k)))

      sm(m) = StationModel(wks, ws, wd, T, Td, resT)

; Attach this station to the map using NhlAddAnnotation
      anno(m) = NhlAddAnnotation(map,sm(m))

; Tell NCL where to attach the annotation.
      setvalues anno(m)
       "amTrackData"    : True    ; Use data coords to position each annotation
       "amDataXF"       : longitude(k)
       "amDataYF"       : latitude(k)
       "amResizeNotify" : True      ; Resize annotations if map resizes.
      end setvalues
      m = m + 1
    else
      latitude(k) = latitude@_FillValue
      longitude(k) = longitude@_FillValue
    end if
  end do

  print("Total number of stations: "+m)
  print("DMAX: "+dmax)


  draw(map)
  frame(wks)

end


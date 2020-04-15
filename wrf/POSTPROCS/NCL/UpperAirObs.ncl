load "$NCL_LIB/ncarg/nclscripts/csm/contributed.ncl"
load "./StationModel.ncl"
load "./RTFDDAUser.ncl"

; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
procedure minmax(text:string,x:numeric)
begin
  fmt = "%5.2f"
  print(text+" Min/Max: "+sprintf(fmt,min(x))+ \
        "  "+sprintf(fmt,max(x)))
end
; =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
function sub_string_float(s:string,i:integer,j:integer)
begin
  s_char = stringtochar(s)
  sub_str = s_char(i:j)
  return (stringtofloat(chartostring(sub_str)))
end
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

 ;Range = getenv("RANGE")
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

  print("Running upper-air plots for:")
  print("Range : "+Range)
  print("Domain: "+Domain)
  print("Date  : "+Date)

;==============================================================
;; Look for geo files, exit if not available

  GEO_FILES_DIR = getenv("HIRESMAP")
 ;geo_file_name = GEO_FILES_DIR+"/"+Range+"/geo_em.d0"+Domain+".nc"
  geo_file_name = "./geo_em.d0"+Domain+".nc"
  print("Terrain file name: "+geo_file_name)
  if (.not.isfilepresent(geo_file_name)) then
    print(geo_file_name+" not found... exit")
    exit
  end if
  ff_geo = addfile(geo_file_name,"r")
  terrain = ff_geo->HGT_M(0,:,:)

; Read projection parameters and ff_geo field
  corner_lats = ff_geo@corner_lats
  corner_lons = ff_geo@corner_lons
  print(corner_lats+"  "+corner_lons)

; Get corners of domain, used to construct map
  latS = corner_lats(0)
  latN = corner_lats(2)
  lonW = corner_lons(0)
  lonE = corner_lons(2)

; These are defined to find the observations to be plotted
  lonW_up = corner_lons(1)
  if (lonW_up .gt. 0.) then
    lonW_up = lonW_up - 360.
  end if
  latN_up = max(ff_geo->XLAT_M(0,:,:))

  dlon = (lonE-lonW)
  print("DLON= "+dlon)

  dmax = 0.1
  if (Domain.eq.4) then
    dmax = 0.001 ;; make number small so all SAMS will be plotted
  end if
  print("DMAX= "+dmax)

  Diri = "/d1/WRF/"+Range+"/"
 ;QCFile = Diri + Date+".hourly.obs_mpl.nc"
  QCFile = Date+".hourly.obs_mpl.nc"
  if (.not.isfilepresent(QCFile)) then
    print(QCFile+" not found... exit")
    exit
  end if
  print("Reading file: "+QCFile)
  ff = addfile(QCFile,"r")

  dsizes = getfiledimsizes(ff)
 
  if (dsizes(0) .le. 0) then
      print ("")
      print ("File is empty, skipping time "+Date)
      print ("")
      exit
  end if

  wks = gsn_open_wks("ps","RAOBplot")          ; open a workstation
  gsn_define_colormap(wks,"gsdtol")
  i = NhlNewColor(wks,1.,0.0,0.0)
  i = NhlNewColor(wks,0.,1.0,1.0)
  
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

  res@gsnStringFontHeightF = 0.015
  start_date = stringtochar(ff@start_date)
  res@gsnRightStringOrthogonalPosF = 0.06
 ;res@gsnRightString = "Valid time: "+start_date(0:9)+" "+start_date(11:15)
  res@gsnRightString = start_date(0:9)

  latitude = ff->latitude
  longitude = ff->longitude
  name = chartostring(ff->name(:,0:5))
  id = chartostring(ff->id(:,0:5))
  source = chartostring(ff->source(:,0:3))
  date = chartostring(ff->date(:,0:11))
  numlevs = ff->numlevs

  plot_station = new(dimsizes(latitude),logical)
  plot_station(:) = False

  fmt = "%8.2f"
  print(date+sprintf(fmt,latitude)+sprintf(fmt,longitude)+"  "+name+ \
  "  "+id+"  "+source+"  "+numlevs)
  delete(numlevs)
  
  indx = ind( \
        (source .eq. "GTS " .or. name .eq. "CLASS " \
        .or. name .eq. " RAOB " .or. name .eq. " PROFI") .and. \
        (latitude .gt. latS .and. latitude .lt. latN_up) .and. \
        (longitude .gt. lonW_up .and. longitude .lt. lonE))

  rad = 4.*atan(1.0)/180.

; Station Model colors options, use red for all GTS
  Opts = True
  Opts@ArrowLength = 4.5   ; Default is 3.0
  color_raob = "Red"
  color_prof = "Cyan1"

; Legend resources
  lgres                    = True
  lgres@lgMarkerColors     = (/color_raob,color_prof/)
  lgres@lgItemType         = "Markers"       ; show markers only
  lgres@lgMonoMarkerIndex  = True
  lgres@lgMarkerIndex      = 16             ; solid circle
  lgres@lgMarkerSizeF      = 0.015
  lgres@lgMonoMarkerSize   = True
  lgres@lgLabelFontHeightF = .08            ; set the legend label font thickness
  lgres@vpWidthF           = 0.15           ; width of legend (NDC)
  lgres@vpHeightF          = 0.1            ; height of legend (NDC)
  lgres@lgPerimColor       = "black"       ; draw the box perimeter in orange
  lgres@lgPerimThicknessF  = 2.0            ; thicken the box perimeter
  lbid = gsn_create_legend(wks, 2,(/" RAOB"," PROF"/),lgres)         ; create legend

  amres = True
  amres@amParallelPosF   = 0.385		    ; move legend to the right
  amres@amOrthogonalPosF = 0.42            ; move the legend down

  if (.not.all(ismissing(indx))) then
    nrec = dimsizes(indx)
    print("There are "+nrec+" RAOB/PROFILER stations")

    numlevs = ff->numlevs(indx)
    do k=0,nrec-1
      kk = indx(k)
      print(k+"  "+kk+"  "+date(kk)+sprintf(fmt,latitude(kk))+\
            sprintf(fmt,longitude(kk))+ \
            "  "+name(kk)+ "  "+id(kk)+"  "+source(kk)+"  "+numlevs(k))
    end do

; Read all data for stations within search limits
    pressure = ff->pressure(indx,:)
    temperature = ff->temperature(indx,:)
    dew_point = ff->dew_point(indx,:)
    u_wind = ff->u_wind(indx,:)
    v_wind = ff->v_wind(indx,:)
    lat_indx = latitude(indx)
    lon_indx = longitude(indx)
    
    printVarSummary(pressure)
    printVarSummary(temperature)

    print("nrec = "+nrec)

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

      if (far) then
        plot_station(k) = True
      end if
    end do
        
    wanted_levels = 100.*(/850.,700.,500.,300./)

    do j=0,dimsizes(wanted_levels)-1

    ; res@gsnLeftString = "ATEC "+Range+" Domain "+Domain+ \
      res@gsnLeftString = Range+" Domain "+Domain+ \
        " RAOBS and profilers "+(wanted_levels(j)/100)+" hPa "
      map = gsn_csm_contour_map(wks,terrain,res)

      annoid1 = gsn_add_annotation(map,lbid,amres)   ; add legend to plot

      print("Finding observations for level: "+sprintf(fmt,wanted_levels(j)))

      sm = new(nrec,graphic)
      anno = new(nrec,graphic)

      m = 0
      do n=0,nrec-1

        k = indx(n)
        if (plot_station(k)) then

        if (source(k) .eq. "GTS " .or. name(k) .eq. "CLASS " \
             .or. name(k) .eq. " RAOB " ) then
          print("NUMLEVS: "+numlevs(n))
          if (nrec .le. 1) then
              indp = ind(abs(pressure(:)-wanted_levels(j)).le.10.)
          else
              indp = ind(abs(pressure(n,:)-wanted_levels(j)).le.10.)
          end if
          Opts@CloudCircleColor = color_raob
          Opts@WindColor = color_raob
          Opts@TempColor = color_raob
          Opts@DewpColor = color_raob
        else
          if (nrec .le. 1) then
             indp = ind_nearest_coord (wanted_levels(j),pressure(:), 0)
          else
             indp = ind_nearest_coord (wanted_levels(j),pressure(n,:), 0)
          end if

          if (.not.all(ismissing(indp))) then
            if (nrec .le. 1) then
               if (abs(wanted_levels(j)-pressure(indp(0))).gt.10000.) then
                 indp = indp@_FillValue
               end if
            else
               if (abs(wanted_levels(j)-pressure(n,indp(0))).gt.10000.) then
                 indp = indp@_FillValue
               end if
            end if
          end if
          Opts@CloudCircleColor = color_prof
          Opts@WindColor = color_prof
          Opts@TempColor = color_prof
          Opts@DewpColor = color_prof
        end if

        if (.not.all(ismissing(indp))) then
          jj = indp(0)
          if (nrec .le. 1) then
             print(name(k)+"  "+source(k)+"  "+indp(0)+"  "+pressure(jj)+"  "\
                   +lat_indx(n)+"  "+lon_indx(n)+"  "+plot_station(k))

             temp = replace_missing(temperature(jj)-273.15)
             dewp = replace_missing(dew_point(jj)-273.15)
             uu = replace_missing(u_wind(jj))
             vv = replace_missing(v_wind(jj))
             print(sprinti("%2i  ",jj)+sprintf(fmt,pressure(jj))+ \
             sprintf(fmt,temp)+sprintf(fmt,dewp)+\
             sprintf(fmt,uu)+ sprintf(fmt,vv))
          else
             print(name(k)+"  "+source(k)+"  "+indp(0)+"  "+pressure(n,jj)+"  "\
                   +lat_indx(n)+"  "+lon_indx(n)+"  "+plot_station(k))

             temp = replace_missing(temperature(n,jj)-273.15)
             dewp = replace_missing(dew_point(n,jj)-273.15)
             uu = replace_missing(u_wind(n,jj))
             vv = replace_missing(v_wind(n,jj))
             print(sprinti("%2i  ",jj)+sprintf(fmt,pressure(n,jj))+ \
             sprintf(fmt,temp)+sprintf(fmt,dewp)+\
             sprintf(fmt,uu)+ sprintf(fmt,vv))
          end if
          
          wspd = 1.9438445*sqrt(uu*uu + vv*vv)   ; m/s -> knots
          wdir = atan2(uu,vv)/rad +180.
          delete(uu)
          delete(vv)

          sm(m) = StationModel(wks, wspd, wdir, temp, dewp, Opts)

; Attach this station to the map using NhlAddAnnotation
          anno(m) = NhlAddAnnotation(map,sm(m))

; Tell NCL where to attach the annotation.
          setvalues anno(m)
          "amTrackData"    : True ; Use data coords to position each annotation
          "amDataXF"       : lon_indx(n)
          "amDataYF"       : lat_indx(n)
          "amResizeNotify" : True      ; Resize annotations if map resizes.
          end setvalues
          m = m + 1
        end if
        delete(indp)
        end if
      end do

      draw(map)
      frame(wks)
      
      NhlRemoveAnnotation(map,anno)
      delete(sm)
      delete(anno)

    end do
  else
    print("No GTS stations...")
  end if
end

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
  xlat = ff_geo->XLAT_M(0,:,:)
  xlon = ff_geo->XLONG_M(0,:,:)
  dims = dimsizes(xlat)
  nlat = dims(0)
  nlon = dims(1)
  delete(dims)

; Get corners of domain, used to construct map
  latS = xlat(0,0)
  latN = xlat(nlat-1,nlon-1)
  lonW = xlon(0,0)
  lonE = xlon(nlat-1,nlon-1)

  fmt = "%8.2f"
  print("Corners of domain: "+sprintf(fmt,latS)+sprintf(fmt,latN)+\
        sprintf(fmt,lonW)+sprintf(fmt,lonE))

; These are defined to find the observations to be plotted
  lonW_up = lonW
  if (lonW_up .gt. 0.) then
    lonW_up = lonW_up - 360.
  end if
  latN_up = max(xlat)
  print("Corners of domain: "+sprintf(fmt,latN_up)+sprintf(fmt,lonW_up))

  dlon = (lonE-lonW)
  print("DLON= "+dlon)

  dmax = dlon/25.
  if (Domain.eq.4) then
    dmax = 0.001 ;; make number small so all SAMS will be plotted
  end if
  print("DMAX= "+dmax)

  Diri = "/d1/WRF/"+Range+"/"
 ;QCFile = Diri + Date+".all.obs_sgl.nc"
  QCFile = Date+".hourly.obs_sgl.nc"
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

  wks = gsn_open_wks("ps","UPPplot")          ; open a workstation
  gsn_define_colormap(wks,"gsdtol")
  i = NhlNewColor(wks,1.,0.0,0.0)  ; red
  i = NhlNewColor(wks,0.,1.0,1.0)  ; cyan
  i = NhlNewColor(wks,1.,1.,0.)    ; yellow
  i = NhlNewColor(wks,0.,0.2,1.)   ; blue
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
; Use high-resolution coaslines if on domain 4
  if (Domain .eq. 4) then
    res@mpDataBaseVersion  = "HighRes"
  end if

; Determine ocean masking of background topo
  res@cnFillDrawOrder    = "Predraw"  ; draw contours first
  res@mpOceanFillColor   = 0            ; array index in color map
  res@mpLandFillColor    = -1          ; for land, ocean, and inland h20
  res@mpInlandWaterFillColor = 0
  res@gsnSpreadColors    = True

; Terrain in light to dark color scale 
; Let the contour level spacing be free
  res@gsnSpreadColorStart= 32 ;;12
  res@gsnSpreadColorEnd  = 12 ;;32
;;  res@cnLevelSpacingF    = 100.
  res@lbLabelBarOn       = False
  res@mpOutlineBoundarySets = "GeophysicalAndUSStates"
  res@pmTickMarkDisplayMode = "Always"
  res@mpGridAndLimbOn      = False          ; default is every 15 deg

; Add lat/lon grid lines if on domain 1
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
     res@mpLimitMode           = "Corners"
  end if
  if (ff_geo@MAP_PROJ .ne. 1 .and. ff_geo@MAP_PROJ .ne. 2 .and. ff_geo@MAP_PROJ .ne. 3) then
    print("Do not know projection= "+ff_geo@MAP_PROJ)
    exit
  end if

  res@gsnAddCyclic       = False
  res@tfDoNDCOverlay     = True
  res@mpLimitMode        = "Corners" ; choose range of map
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
 ;res@gsnRightString = "Valid time: "+start_date(0:9)+" "+start_date(11:15)+" GMT"
  res@gsnRightString = start_date(0:9)
                       
  latitude = ff->latitude
  longitude = ff->longitude
  name = chartostring(ff->name(:,0:5))
  id = chartostring(ff->id(:,0:5))
  source = chartostring(ff->source(:,0:3))
  date = chartostring(ff->date(:,0:11))

  plot_station = new(dimsizes(latitude),logical)
  plot_station(:) = False

  fmt = "%8.2f"
  ;print(date+sprintf(fmt,latitude)+sprintf(fmt,longitude)+"  "+name+ \
  ;"  "+id+"  "+source)
  
  indx = ind( (name .eq. " ACARS" .or. name .eq. " SATWN") .and. \
        (latitude .gt. latS .and. latitude .lt. latN_up) .and. \
        (longitude .gt. lonW_up .and. longitude .lt. lonE))

  rad = 4.*atan(1.0)/180.

; Station Model colors options
  Opts = True
  Opts@ArrowLength = 4.5   ; Default is 3.0
  color_acars = "Red"; "Blue"
  color_satwnd = "Cyan1"

  txres = True
  txres@txFontHeightF = 0.015
  
  pmres = True
  pmres@gsMarkerColor = "blue" ;color_raob
  pmres@gsMarkerIndex = 4
  pmres@gsMarkerThicknessF = 1.
  pmres@gsMarkerSizeF  = 0.005

  lgres                    = True
  lgres@vpWidthF           = 0.15          ; width of legend (NDC)
  lgres@vpHeightF          = 0.1           ; height of legend (NDC)
  lgres@lgMarkerColors     = (/color_acars,color_satwnd/)
  lgres@lgItemType         = "Markers"     ; show markers only
  lgres@lgMonoMarkerIndex  = True          ; Markers only
  lgres@lgMarkerIndex      = 16            ; solid circle
  lgres@lgMonoMarkerSize   = True
  lgres@lgMarkerSizeF      = 0.015         ; Marker size
  lgres@lgLabelFontHeightF = 0.08          ; set the legend label font height
  lgres@lgPerimColor       = "black"       ; draw the box perimeter in orange
  lgres@lgPerimThicknessF  = 2.0           ; thicken the box perimeter
  lbid = gsn_create_legend(wks, 2,(/" ACARS"," SATWND"/),lgres)         ; create legend

  amres = True
  amres@amParallelPosF   = 0.385	   ; move legend to the right
  amres@amOrthogonalPosF = 0.42            ; move the legend down
  if (Range .eq. "DPG") then
    amres@amParallelPosF   = -0.385
  end if

  if (.not.all(ismissing(indx))) then
    nrec = dimsizes(indx)
    print("There are "+nrec+" ACARS stations")

    do k=0,nrec-1
      kk = indx(k)
      print(k+"  "+kk+"  "+date(kk)+sprintf(fmt,latitude(kk))+\
            sprintf(fmt,longitude(kk))+ \
            "  "+name(kk)+ "  "+id(kk)+"  "+source(kk))
    end do

; Read all data for stations within search limits
    pressure = ff->pressure(indx)
    temperature = ff->temperature(indx)
    dew_point = ff->dew_point(indx)
    u_wind = ff->u_wind(indx)
    v_wind = ff->v_wind(indx)
    lat_indx = latitude(indx)
    lon_indx = longitude(indx)
    
;   wanted_levels = 100.*(/800.,475.,200./)
;   delta_p = 100.*(/200.,125.,150./)

    wanted_levels = 100.*(/200.,475.,800./)
    delta_p = 100.*(/150.,125.,200./)

;    wanted_levels = 100.*(/200./)
;    delta_p = 100.*(/150./)

    do j=0,dimsizes(wanted_levels)-1   ; Wanted level loop

      pdown = (wanted_levels(j)+delta_p(j))/100.
      pup = (wanted_levels(j)-delta_p(j))/100.

     ;res@gsnLeftString = "ATEC "+Range+" Domain "+Domain+ "~C~"+ \
     ;res@gsnLeftString = "ATEC "+Range+" Domain "+Domain+ " "+ \
      res@gsnLeftString = Range+" Domain "+Domain+ " "+ \
           "ACARS and satellite winds "+pup+"-"+pdown +" hPa "

      map = gsn_csm_contour_map(wks,terrain,res)

      annoid1 = gsn_add_annotation(map,lbid,amres)   ; add legend to plot

      print("Finding observations for level: "+sprintf(fmt,wanted_levels(j)))

      sm = new(nrec,graphic)
      anno = new(nrec,graphic)
      ball = new(nrec,graphic)

      m = 0
      l = 0

      indp = ind(abs(pressure-wanted_levels(j)).le.delta_p(j))
      if (.not.all(ismissing(indp))) then
        npres = dimsizes(indp)
        print("Obs at this level: "+npres)

        do n=0,npres-1
          k = indp(n)
          kk = indx(k)
          ;print("n,k,kk: "+sprinti(" %4i",n)+sprinti(" %4i",k)+\
          ;  sprinti(" %4i",kk)+"  "+pressure(k))
            
          if (name(kk) .eq. " ACARS") then
            Opts@CloudCircleColor = color_acars
            Opts@WindColor = color_acars
            Opts@TempColor = color_acars
            Opts@DewpColor = color_acars
          else
            Opts@CloudCircleColor = color_satwnd
            Opts@WindColor = color_satwnd
            Opts@TempColor = color_satwnd
            Opts@DewpColor = color_satwnd
          end if
        
; Thin observations if they are too close together

          far = True
          dist = 0.
          ii = 1
        
          if (n.gt.0) then
            delete(dist)
            delete(ii)
        
            dist = new(n,"float")
            ii = indx(indp(0:n-1))
            dist = sqrt( (latitude(ii)-latitude(kk))^2 + \
                         (longitude(ii)-longitude(kk))^2 )
            if (ismissing(min(dist))) then
               far = False
            else
               if (min(dist) .lt. dmax) then
                  far = False
               end if
            end if
          end if

          if (far) then
            plot_station(kk) = True
          else
            latitude(kk) = latitude@_FillValue
            longitude(kk) = longitude@_FillValue
          end if
          delete(ii)
        
          print(name(kk)+"  "+source(kk)+"  "+pressure(k)+"  "\
                +lat_indx(k)+"  "+lon_indx(k)+"  "+plot_station(kk))

          temp = replace_missing(temperature(k)-273.15)
          dewp = replace_missing(dew_point(k)-273.15)
          uu = replace_missing(u_wind(k))
          vv = replace_missing(v_wind(k))
         ; print(sprintf(fmt,pressure(k))+ \
         ;    sprintf(fmt,temp)+sprintf(fmt,dewp)+\
         ;    sprintf(fmt,uu)+ sprintf(fmt,vv))
          
          wspd = 1.9438445*sqrt(uu*uu + vv*vv)   ; m/s -> knots
          wdir = atan2(uu,vv)/rad + 180.
          delete(uu)
          delete(vv)

          if (plot_station(kk)) then

            sm(m) = StationModel(wks, wspd, wdir, temp, dewp, Opts)

; Attach this station to the map using NhlAddAnnotation
            anno(m) = NhlAddAnnotation(map,sm(m))

; Tell NCL where to attach the annotation.
            setvalues anno(m)
            "amTrackData"    : True ; Use data coords to position each annotation
            "amDataXF"       : lon_indx(k)
            "amDataYF"       : lat_indx(k)
            "amResizeNotify" : True      ; Resize annotations if map resizes.
            end setvalues
            m = m + 1
          else

            if (name(kk) .eq. " ACARS") then
               pmres@gsMarkerColor = color_acars
            else
               pmres@gsMarkerColor = color_satwnd
            end if

            ball(l) = gsn_add_polymarker(wks, map, lon_indx(k), lat_indx(k), pmres)
            l = l + 1
          end if
        end do
      end if
      delete(indp)

      draw(map)
      frame(wks)
      
      NhlRemoveAnnotation(map,anno)
      delete(sm)
      delete(anno)

    end do
  else
    print("No ACAR or SATWND observations...")
  end if
end

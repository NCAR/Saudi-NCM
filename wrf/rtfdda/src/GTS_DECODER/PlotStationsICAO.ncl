load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

; ==============================================
; Start map

  wks = gsn_open_wks("ps","Stations")     ; Open a workstation and
  gsn_define_colormap(wks,"WhViBlGrYeOrRe")      ; define a different colormap.

  nc1 = NhlNewColor(wks,.8,.8,.8)        ; Add light gray to colormap,
                                         ; for continents.
;
; Set up some map resources.
;
  mpres              = True
  mpres@gsnDraw  = False                          ; don't draw
  mpres@gsnFrame = False                          ; don't advance frame
;
; Zoom in on United States (if world is False)
;

  date = getenv("DATE")
  if (date.eq."NOW") then
    date = systemfunc("date +%y%m%d") 
  end if
  odate = getenv("ODATE")

  WW = getenv("WORLD")
  print("World is: " +WW)
  if (WW .eq. "T") then
    World = True
  else
    World = False
  end if

;  print("If North America map is desired, use World=False in command line ")
;  print("  ")

  print("World is "+World)
  if (World .eq. False) then
    mpres@mpMinLatF    = 15.
    mpres@mpMaxLatF    = 70.
    mpres@mpMinLonF    = 215.
    mpres@mpMaxLonF    = 290.
  end if

  mpres@mpFillColors = (/-1,-1,nc1,-1/)    ;assign light gray to land masses
  mpres@mpFillDrawOrder = "PreDraw"

  map = new(3,graphic)  

; ==============================================
; Polymarker and text resources
  gsres               = True
  gsres@gsMarkerIndex = 16          ; Use filled dots for markers.
  gsres@gsMarkerSizeF = 0.005

  txres               = True
  txres@txFontHeightF = 0.012
  txres@txJust        = "CenterLeft"

  if (World .eq. True) then
    LegLeft = 0.27
    LegTop = 0.46
  else
    gsres@gsMarkerSizeF = 0.008
    LegLeft = 0.58
    LegTop = 0.62
  end if

  xleg = new(5,"float")
  xleg(:) = LegLeft
  xtxt = xleg + 0.02

  yleg = fspan(LegTop-0.08,LegTop,5)
  ytxt = yleg 

; Second plot - moved stations

  data = readAsciiTable("MovedStations.dat", 5, "integer", 0)
  lat = data(:,0)/100.
  lon = data(:,2)/100.
  R = data(:,4)/1000             ;; previously distance

  arr = (/5,10,50/)    ; bin settings 
  colors = (/30,51,72,94/)	; marker colors, dimsizes must 
  labels = new(dimsizes(arr)+1,string)  ; Labels for legend.

  num_distinct_markers = dimsizes(arr) + 1        ; number of distinct markers
  pol_moved = new(num_distinct_markers,graphic)
  elev_change = new(num_distinct_markers,graphic)

  mpres@gsnLeftString = "Moved Stations"
  mpres@gsnRightString = "distance (km)"
  map(1) = gsn_csm_map(wks,mpres)

  do i = 0, num_distinct_markers-1
    if (i.eq.0) then
      index = ind(R.lt.arr(0))
      labels(i) = "x < " + arr(0)
    end if
    if (i.eq.num_distinct_markers-1) then
      index = ind(R.ge.max(arr))
      labels(i) = "x >= " + max(arr)
    end if
    if (i.gt.0.and.i.lt.num_distinct_markers-1) then       
      index = ind(R.ge.arr(i-1).and.R.lt.arr(i))
      labels(i) = arr(i-1) + " <= x < " + arr(i)
    end if

    if (.not.all(ismissing(index))) then

      gsres@gsMarkerColor      = colors(i)
      gsres@gsMarkerThicknessF = 1.0
      pol_moved(i) = gsn_add_polymarker(wks,map(1),lon(index),lat(index),gsres)
;
; Add marker and text for the legend.
;
      gsn_polymarker_ndc(wks,          xleg(i),yleg(i),gsres)
      gsn_text_ndc      (wks,labels(i),xtxt(i),ytxt(i),txres)
    end if

    delete(index)

  end do

  mpres@gsnLeftString = "Changed Elevation"
  mpres@gsnRightString = "elev difference (m)"

  delete(lat)
  delete(lon)
  delete(data)

;=============================================  
  map(2) = gsn_csm_map(wks,mpres)

  data = readAsciiTable("UpDownStations.dat", 4, "integer", 0)
  lat = data(:,0)/100.
  lon = data(:,1)/100.
  elev = abs(data(:,2) - data(:,3))
;;  print(lat+"  "+lon+"  "+elev)

  arr = (/10,100,200/)    ; bin settings 

   if (World .eq. True) then
    LegLeft = 0.27
    LegTop = 0.145
  else
    LegLeft = 0.33
    LegTop = 0.23
  end if

  xleg = new(5,"float")
  xleg(:) = LegLeft
  xtxt = xleg + 0.02

  yleg = fspan(LegTop-0.08,LegTop,5)
  ytxt = yleg 

  do i = 0, num_distinct_markers-1
    if (i.eq.0) then
      index = ind(elev.lt.arr(0))
      labels(i) = "x < " + arr(0)
    end if
    if (i.eq.num_distinct_markers-1) then
      index = ind(elev.ge.max(arr))
      labels(i) = "x >= " + max(arr)
    end if
    if (i.gt.0.and.i.lt.num_distinct_markers-1) then       
      index = ind(elev.ge.arr(i-1).and.elev.lt.arr(i))
      labels(i) = arr(i-1) + " <= x < " + arr(i)
    end if
    if (.not.all(ismissing(index))) then

      gsres@gsMarkerColor      = colors(i)
      gsres@gsMarkerThicknessF = 1.0
      elev_change(i) = \
      gsn_add_polymarker(wks,map(2),lon(index),lat(index),gsres)
;
; Add marker and text for the legend.
;
      gsn_polymarker_ndc(wks,          xleg(i),yleg(i),gsres)
      gsn_text_ndc      (wks,labels(i),xtxt(i),ytxt(i),txres)
      delete(index)
    end if
  end do
  delete(lat)
  delete(lon)
  delete(data)

;=============================================  
  data = readAsciiTable("NewStations.dat", 3, "integer", 0)
  lat = data(:,0)/100.
  lon = data(:,1)/100.
  new_stats = new(dimsizes(lat),graphic)

  mpres@gsnLeftString = "New Stations"
  mpres@gsnRightString = ""
  map(0) = gsn_csm_map(wks,mpres)

 if (.not.all(ismissing(lat))) then
    gsres@gsMarkerColor      = colors(1)
    gsres@gsMarkerThicknessF = 1.0
    new_stats = \
    gsn_add_polymarker(wks,map(0),lon,lat,gsres)
  end if


  resP                  = True
  resP@gsnMaximize  = True             ; Maximize plot in frame.
  resP@txString = "ICAO station update, Olddate: "+odate + ", Newdate: "+ date
  if (World .eq. True) then
    gsn_panel(wks,map,(/3,1/),resP)
  else
    gsn_panel(wks,map,(/2,2/),resP)
  end if
end

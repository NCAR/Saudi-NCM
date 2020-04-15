load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "/home/hahmann/WXPLOTS/StationModel/RTFDDAUser.ncl"

;===============================================================
; NCL script creates grid configuration map from the contents of 
; a WRF "geo" file.  To run requires env variable RANGE to be 
; set to desired ATEC range.
; Andrea Hahmann, October 2007
;===============================================================

function getcoord(tabf:character)
local f
begin
  len = dimsizes(tabf)
  if (len .eq. 9) then
    f = stringtofloat(chartostring(tabf(0:2)))
    if (f .gt. 0.) then
      f = f + stringtofloat(chartostring(tabf(4:5)))/60. + \
          stringtofloat(chartostring(tabf(7:8)))/3600.
    else
      f = - f + stringtofloat(chartostring(tabf(4:5)))/60. + \
          stringtofloat(chartostring(tabf(7:8)))/3600.
      f = - f
    end if
  else
    f = stringtofloat(chartostring(tabf(0:3)))
    if (f .gt. 0.) then
      f = f + stringtofloat(chartostring(tabf(5:6)))/60. + \
          stringtofloat(chartostring(tabf(8:9)))/3600.
    else
      f = - f + stringtofloat(chartostring(tabf(5:6)))/60. + \
          stringtofloat(chartostring(tabf(8:9)))/3600.
      f = -f
    end if
  end if
  return(f)
end

begin

  File = "newlist"
  exists = isfilepresent(File)
  if (.not.exists) then
    print("Could not find station file "+File)
    print("exiting")
    exit
  end if
  nRow = numAsciiRow(File) ; number of rows
  nCol = 26 ; number of columns in file
  print("nRow = "+nRow)
  tabf = asciiread(File, (/nRow,nCol/) , "character")
  ;print(tabf)
  ;exit

  stat_lat = new((/nRow/),float)
  stat_lon = new((/nRow/),float)
  stat_name = new((/nRow/),string)
  do i=0,nRow-1
    stat_lat(i) = getcoord(tabf(i,4:12))
    stat_lon(i) = getcoord(tabf(i,14:23))
    stat_name(i) = chartostring(tabf(i,1:3))
  end do
  ;print(stat_lat+"  "+stat_lon+"  "+stat_name)
  ;exit

  if (.not.isdefined("d")) then
    print("Domain not defined... default to d=1")
    d = 1
  end if
  Range = getenv("RANGE")
  print("Plotting landmask for: ")
  print("   RANGE:  "+Range)
  print("   DOMAIN: "+d)

  dom = sprinti("%i",d)
  diri = "/d1/WRF/"+Range+"/"
  FileName = diri + "geo_em.d0"+dom+".nc"
  ff = addfile(FileName,"r")
  xlat = ff->XLAT_M(0,:,:)
  xlon = ff->XLONG_M(0,:,:)
  dims = dimsizes(xlat)
  nlat = dims(0)                                ; assign # lat/lon points
  nlon = dims(1)
  
  latS = min(xlat)
  latN = max(xlat)
  lonW = min(xlon)
  lonE = max(xlon)

  if (lonW .gt. 0.) then
    lonW = lonW - 360.
  end if
  print("Corners are: "+latS+" "+ latN+" "+lonW+" "+lonE)

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
  res@mpLeftCornerLatF  = xlat(0,0)
  res@mpLeftCornerLonF  = xlon(0,0)
  res@mpRightCornerLatF = xlat(nlat-1,nlon-1)
  res@mpRightCornerLonF = xlon(nlat-1,nlon-1)

  res@tfDoNDCOverlay     = True
  res@gsnAddCyclic = False

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
  res@gsnSpreadColorEnd  = 15

  PlotName = "WRF_config_"+Range+"_domain"+dom
  PlotType = "eps"
  print("PlotName: "+PlotName+"."+PlotType)
  wks = gsn_open_wks(PlotType ,PlotName)    ; ps,pdf,x11,ncgm,eps

  cmap = RGBtoCmap("~/ColorTables/topo_colors2.rgb") 
  
  gsn_define_colormap(wks,cmap)                     ; create the color map 
  
  res@cnInfoLabelOn     = False           ; turn off cn info label
  res@gsnFrame          = False
  res@gsnDraw           = False
  res@gsnMaximize      = True

  terrain = ff->HGT_M(0,:,:)

  print("Terrain Max/Min: "+min(terrain)+"  "+max(terrain))
  
  res@gsnRightString = " "
  res@gsnLeftStringFontHeightF = 0.025
  
  res@gsnLeftString = Range + " WRF, DOMAIN "+d+ \
      ", ~F33~D~F21~x="+sprintf("%4.1f",ff@DX/1000.)+" km"

  res@lbLabelStride    = 2
  res@pmTickMarkDisplayMode = "Always"
  res@lbTitleString = "surface elevation (m)"
  res@lbTitlePosition      = "bottom"
  res@lbTitleFontHeightF = 0.015

  map = gsn_csm_contour_map(wks,terrain,res)

  nstrs = dimsizes(stat_lat)
  xndc    = new(nstrs,float)     ;  X center of box in NDC coords
  yndc    = new(nstrs,float)     ;  Y center of box in NDC coords
  datatondc(map,stat_lon,stat_lat,xndc,yndc)
  ;print(xndc+"  "+yndc)

  indx = ind(.not.ismissing(xndc))
  nrec = dimsizes(.not.ismissing(indx))
  if (all(ismissing(indx))) then
    print("No stations to plot... exit")
    exit
  end if
  delete(xndc)
  delete(yndc)
  
  txres               = True                            
  txres@txFontHeightF = 0.012
;;  txres@txJust = "BottomCenter"

  text = gsn_add_text(wks, map, stat_name(indx), \
              stat_lon(indx), stat_lat(indx), txres)

  nstrs = dimsizes(text)

  getvalues map
    "pmAnnoViews" : text_ids
  end getvalues

  xndc    = new(nstrs,float)     ;  X center of box in NDC coords
  yndc    = new(nstrs,float)     ;  Y center of box in NDC coords
  rgt     = new(nstrs,float)     ; Hold right position of text box.
  lft     = new(nstrs,float)     ;  "   left     "     "   "    " 
  top     = new(nstrs,float)     ;  "   top      "     "   "    " 
  bot     = new(nstrs,float)     ;  "   bottom   "     "   "    " 
  width   = new(nstrs,float)     ;  width of each text box
  height  = new(nstrs,float)     ;  height of each text box

; Loop through and get the width and height of each box.
;
  do i=0,nstrs-1
    getvalues text_ids(i)
      "vpWidthF"  : width(i)
      "vpHeightF" : height(i)
    end getvalues
  end do

; Convert the lat/lon center of each box to NDC coordinates, since this
; is what the width and height values are in.
;
  datatondc(map,stat_lon(indx),stat_lat(indx),xndc,yndc)

; Calculate the four corners of each text box in NDC coordinates.
;
  top = yndc + height/2.
  bot = yndc - height/2.
  lft = xndc -  width/2.
  rgt = xndc +  width/2.

  ;;print(width+"  "+height+"  "+top+"  "+bot+"  "+lft+"  "+rgt)

;
; Now we're going to loop through each text string that has been added,
; and compare it to all other text strings that have been added to see if
; they overlap. If an overlap is encountered, one of the strings will be
; tagged for removal, and a box will be drawn around it (just to show,
; for debug purposes, which text strings are getting removed).
;
  removed_list = new(nstrs,integer)
  rcount = 0                         ; Number of removed text boxes.

;
; ibox1 is index of box that we're checking to see if it overlaps.
; ibox2 is the index of the box that we're checking box ibox1
; against.
;
  do ibox1 = 0,nstrs-1
    ibox2 = 0
    overlap_found = False 

    do while(ibox2.lt.nstrs.and..not.overlap_found)

;
; Check if any one of the corners represented by box "ibox1" is inside
; the box representd by "ibox2".  If so, remove it.  Make sure you are
; not checking a box against itself, or against a box that has already
; been removed.
;
      if (ibox1.ne.ibox2.and.(.not.any(ibox2.eq.removed_list))) then
        if ( (top(ibox1).le.top(ibox2).and.top(ibox1).ge.bot(ibox2).and. \
              lft(ibox1).le.rgt(ibox2).and.lft(ibox1).ge.lft(ibox2)).or. \
             (bot(ibox1).le.top(ibox2).and.bot(ibox1).ge.bot(ibox2).and. \
              lft(ibox1).le.rgt(ibox2).and.lft(ibox1).ge.lft(ibox2)).or. \
             (bot(ibox1).le.top(ibox2).and.bot(ibox1).ge.bot(ibox2).and. \
              rgt(ibox1).le.rgt(ibox2).and.rgt(ibox1).ge.lft(ibox2)).or. \
             (top(ibox1).le.top(ibox2).and.top(ibox1).ge.bot(ibox2).and. \
              rgt(ibox1).le.rgt(ibox2).and.rgt(ibox1).ge.lft(ibox2))) then

          overlap_found = True         ; Mark that an overlap has been found.

          removed_list(rcount) = ibox1    ; Update removed_list and
          rcount = rcount + 1             ; its counter.
        end if
      end if
      ibox2 = ibox2 + 1               ; Update the box counter
    end do
  end do

;
; Here's the code that removes the overlapping text strings.
;
  if (rcount.gt.0) then
    NhlRemoveAnnotation(map,text_ids(removed_list(:rcount-1)))
  end if

; Add domain boundaries of next nested grid 
  gres                  = True                ; polyline mods desired
  gres@gsLineThicknessF = 3.0                 ; line thickness
  gres@gsLineColor      = "black"               ; line color

  line = new(12,"graphic")
  i = 0

  if (d .lt. 4) then
    d1 = d + 1
    FileName = diri + "geo_em.d0"+d1+".nc"
    print("Domain boundary FileName: "+FileName)
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
    gres@gsLineColor      = "red"       ; line color
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

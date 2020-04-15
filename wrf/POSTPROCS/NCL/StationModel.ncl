load "$NCL_LIB/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCL_LIB/ncarg/nclscripts/csm/gsn_csm.ncl"

function StationModel(wks:graphic, wspd:numeric, wdir:numeric, \
                      temp:numeric, dewp:numeric, stRes:logical)
begin

  res = True
  opts = True     ; local and default options
  if (stRes) then
    opts = stRes
  end if

  if (.not.ismissing(temp)) then
    if (temp .ge. -0.5) then
      ctemp = sprinti("%i",floattoint(temp+0.501))
    else
      ctemp = sprinti("%i",floattoint(temp-0.499))
    end if
  else
    ctemp = " "
  end if
  ;;print("ctemp: "+temp+"  "+ctemp)

  if (.not.ismissing(dewp)) then
    if (dewp .ge. -0.5) then
      cdewp = sprinti("%i",floattoint(dewp+0.501))
    else
      cdewp = sprinti("%i",floattoint(dewp-0.499))
    end if
  else
    cdewp = " "
  end if
  ;;print("cdewp: "+dewp+"  "+cdewp)

  rad    = 4.*atan(1.0)/180.
  circFr = 5.
  circsz = 0.9/2.                          ;user can choose size

  circlex = sin(fspan(0,360,361)*rad)*circsz
  circley = cos(fspan(0,360,361)*rad)*circsz

  res_circle = True
  extraSpace        = max((/3.,circFr/))    ; Extra space beyond outer circle
  res_circle@trXMinF      = -extraSpace  ; min X 
  res_circle@trXMaxF      =  extraSpace  ; max X
  res_circle@trYMinF      = -extraSpace  ; min Y
  res_circle@trYMaxF      =  extraSpace  ; max Y
  res_circle@tmXTOn       = False            ; turn off tick marks on each side
  res_circle@tmXBOn       = False
  res_circle@tmYLOn       = False
  res_circle@tmYROn       = False
  res_circle@tmXBBorderOn = False            ; turn off borders on each side
  res_circle@tmXTBorderOn = False
  res_circle@tmYLBorderOn = False
  res_circle@tmYRBorderOn = False
  res_circle@xyMonoDashPattern = True                ; set all circles to solid
  res_circle@gsnFrame     = False                    ; do not advance frame
  res_circle@gsnDraw      = False 
  res_circle@vpWidthF     = 0.12  
  res_circle@vpHeightF    = 0.12
  res_circle@xyLineThicknessF  = 3

  if (ismissing(wspd)) then
    res_circle@xyLineColors = (/"Transparent"/)
  else
    if (isatt(opts,"CloudCircleColor")) then
      res_circle@xyLineColors = (/opts@CloudCircleColor/)
    else
      res_circle@xyLineColors = (/"black"/)
    end if
  end if
  plot = gsn_xy(wks,circlex,circley,res_circle)

  pmres             = True
; Check for circle color setting
  if (isatt(opts,"CloudCircleColor")) then
    pmres@gsMarkerColor = opts@CloudCircleColor
  else
    pmres@gsMarkerColor = "Blue"  
  end if
  pmres@gsMarkerIndex = 16
  pmres@gsMarkerThicknessF = 3.
  pmres@gsMarkerSizeF  = 0.003

  string0 = unique_string("CloudCircle")
  plot@$string0$ = gsn_add_polymarker(wks, plot, 0., 0., pmres)

  if (isatt(opts,"ArrowLength")) then
    arrow = opts@ArrowLength
  else
    arrow = 3.0
  end if
  barb = arrow/3.

  gsres = True
  if (isatt(opts,"WindColor")) then
    gsres@gsLineColor = opts@WindColor
    gsres@gsFillColor = opts@WindColor 
  else
    gsres@gsLineColor = "black"
    gsres@gsFillColor = "black"
  end if
  gsres@gsLineThicknessF  = 3

  x = 0.
  y = 0.

  if (.not.ismissing(wspd) .and. .not.ismissing(wdir)) then
    n_flag = floattoint(wspd/50.)
    n_whole_barb = floattoint((wspd - 50.*n_flag)/10.)
    n_half_barb = floattoint((wspd - 50.*n_flag - 10.*n_whole_barb)/5.+0.5)
    ;;print(n_flag+"   "+n_whole_barb +"  "+n_half_barb)

    str = new(n_flag+n_whole_barb+n_half_barb+10,"string")

    x_tip = x + arrow*sin(wdir*rad)
    y_tip = y + arrow*cos(wdir*rad)
    str0 = unique_string("WindArrow")

    ; Plot main wind "spike"
    if (wspd.ge.1.) then
      plot@$str0$ = gsn_add_polyline(wks,plot,(/x,x_tip/),(/y,y_tip/),gsres)
    end if

    ; Plot "flags" first
    nflag = 0
    if (n_flag .ge. 1) then
      do nflag = 0, n_flag-1
        x1 = x + (1.-2*nflag*0.1)*arrow*sin(wdir*rad)
        y1 = y + (1.-2*nflag*0.1)*arrow*cos(wdir*rad)
        x2 = x1 + barb*sin((wdir+105.)*rad)
        y2 = y1 + barb*cos((wdir+105.)*rad)
        x3 = x + (1.-2*(nflag+1)*0.1)*arrow*sin(wdir*rad)
        y3 = y + (1.-2*(nflag+1)*0.1)*arrow*cos(wdir*rad)
        str(nflag) = unique_string("Barb1")
        plot@$str(nflag)$ = gsn_add_polygon(wks,plot,(/x1,x2,x3/),(/y1,y2,y3/),gsres)
      end do
    end if

    if (nflag .eq. 0) then
      nbarb = 0
      Nbarb = 0
    else
      nbarb = 2*nflag + 1
      Nbarb = nbarb
    end if
    ; Plot "whole" barbs
    if (n_whole_barb .gt. 0) then
      do nbarb = Nbarb,Nbarb+n_whole_barb-1
        x1 = x + (1.-nbarb*0.1)*arrow*sin(wdir*rad)
        y1 = y + (1.-nbarb*0.1)*arrow*cos(wdir*rad)
        x2 = x1 + barb*sin((wdir+70.)*rad)
        y2 = y1 + barb*cos((wdir+70.)*rad)
        str(nbarb) = unique_string("Barb1")
        plot@$str(nbarb)$ = gsn_add_polyline(wks,plot,(/x1,x2/),(/y1,y2/),gsres)
      end do
    end if
    ;;print("nbarb="+nbarb)

    if (n_half_barb .eq. 1) then
      x1 = x + (1.-nbarb*0.1)*arrow*sin(wdir*rad)
      y1 = y + (1.-nbarb*0.1)*arrow*cos(wdir*rad)
      x2 = x1 + 0.5*barb*sin((wdir+70.)*rad)
      y2 = y1 + 0.5*barb*cos((wdir+70.)*rad)
      str(nbarb) = unique_string("Barb1")
      plot@$str(nbarb)$ = gsn_add_polyline(wks,plot,(/x1,x2/),(/y1,y2/),gsres)
    end if


  end if
  
  txres             = True                     ; text mods desired
  txres@txFontHeightF = 0.015                     ; font smaller. default big
  if (isatt(opts,"TempColor")) then
    txres@txFontColor = opts@TempColor
  else
    txres@txFontColor = "Red3"
  end if

  string3 = unique_string("TempText")
  plot@$string3$ = gsn_add_text(wks,plot,ctemp,x-1.5,y+1.0,txres)

  if (isatt(opts,"DewpColor")) then
    txres@txFontColor = opts@DewpColor
  else
    txres@txFontColor = "ForestGreen"
  end if
  string4 = unique_string("DewPText")
  plot@$string4$ = gsn_add_text(wks,plot,cdewp,x-1.5,y-1.0,txres)
 
  return(plot)
end


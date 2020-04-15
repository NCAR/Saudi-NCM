load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "StationModel.ncl"

begin

  wks = gsn_open_wks("X11","SFCplot")          ; open a workstation
  gsn_define_colormap(wks,"wgne15")

;; Open observation file; name comes from the command line
;; 'QCFile = "<filename>"'

  ff = addfile(QCFile,"r")

  latitude = ff->latitude
  nrec = dimsizes(latitude)
  longitude = ff->longitude
  platform = ff->platform
  id = ff->id
  name = ff->name
  printVarSummary(name)
  is_surface = ff->is_surface

  n = 45
;;  do n=0,nrec-1
    if (is_surface(n).eq.1) then
      temp = ff->temperature(n)
      temp@_FillValue = temp@missing_value
      temp = temp - 273.15
      
      dewp = ff->dew_point(n)
      dewp@_FillValue = dewp@missing_value
      dewp = dewp - 273.15
      
      ;wspd = 1.9438445*ff->wind_speed(n)
      ;wspd = 110.4 ;;10*wspd
      ;wdir = 90. ;;ff->wind_direction(n)
      
      print(temp+"  "+dewp+"  "+wspd+"  "+wdir)
      
      plot = StationModel(wks, wspd, wdir, temp, dewp)
      draw(plot)
      frame(wks)
    end if
;;  end do

end

#!/bin/csh -f
##Usage:  draw_ncl.csh  netcdf_file
##or for stage 4 
##        draw_ncl.csh  netcdf_file validated_time[yyyymmddhh]

##2. set the path of the ncl script
cat >! RGBrain.txt << EOF
255 255 255
0   0   0
255 255 255
124 254 252
4   170 252
4    52 204
100 254   4
252 222   4
252 134   4
252  70   4
212   2  52
115   2 100 
150 150 150
EOF

cat >! PlotRain_3hr.ncl << EOF

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

function format_date(cdate[*]:character)
local nn,aux,date
begin

  ;FileName = getenv("FileName") 

  nn = dimsizes(chartostring(cdate))
  aux  = chartostring(cdate(0:3))+chartostring(cdate(5:6)) \
       + chartostring(cdate(8:9))+chartostring(cdate(11:12))
  date = stringtointeger(aux)
  return(date)
end

begin


  Date = 20050312
  Date = 100*(Date/100)

  res = True
  res@cnLinesOn          = False
  res@cnFillOn           = True               ; color plot desired
  res@cnLineLabelsOn     = False              ; turn off contour lines
  res@cnInfoLabelOn       = False           ; turn off cn info label

  res@mpGeophysicalLineColor= "black" ; color of continental outlines
  res@mpPerimOn             = True ; draw box around map
  res@mpPerimDrawOrder      = "PostDraw"
  res@mpGridLineDashPattern = 2 ; lat/lon lines as dashed
  res@mpOutlineBoundarySets = "GeophysicalAndUSStates"
  res@mpUSStateLineColor    = "black"

  FileName = getenv("FileName") 
  RGBrain_file = getenv("RGBrain_file") 
  ff = addfile(FileName,"r")

  if (ff@map_proj .eq. 1) then
    res@mpProjection        = "LambertConformal"
  end if
  res@mpLambertMeridianF    = ff@central_longitude
  res@mpLambertParallel1F   = ff@truelat1
  res@mpLambertParallel2F   = ff@truelat2

  res@gsnAddCyclic          = False
  res@tfDoNDCOverlay        = True
  res@mpLimitMode           = "Corners"
  res@mpLeftCornerLatF      = ff@lat_SW_corner
  res@mpLeftCornerLonF      = ff@lon_SW_corner
  res@mpRightCornerLatF     = ff@lat_NE_corner
  res@mpRightCornerLonF     = ff@lon_NE_corner

  res@cnLevelSelectionMode = "ExplicitLevels"
  res@cnLevels  = (/0.1,0.2,0.4,0.8,1.6,3.2,6.2,12.8,25.6,51.2/)
  res@gsnSpreadColors   = True
  res@cnMissingValFillColor = 13

  time = ff->time
  nn = dimsizes(time)

  Times = ff->Times
  aux = dimsizes(Times)
  ;print("aux="+aux+"   "+Times)
  prec3d = ff->RAIN(:,:,:)

  res@tiMainString = "3 Hour Precipitation (mm)"
  ;MainString = getenv("MainString")
  ;res@tiMainString = MainString
  res@gsnRightString = " "
  res@gsnMaximize      = True

  outFile =FileName
  n = 0
     date = format_date(Times(n,:))
     wks = gsn_open_wks("ps",""+date)
     cmap = RGBtoCmap(RGBrain_file)
     gsn_define_colormap(wks,cmap) 

     prec = prec3d(n,:,:)
     ;print(Times(n,:)+" Min/Max: "+min(prec)+"  "+max(prec))
     res@gsnLeftString = "Validated at "+date
     map = gsn_csm_contour_map(wks,prec,res)


;  opt=True
;  opt@fout=outFile+".txt"
;  write_matrix(prec(:,:),"10f12.6",opt)
end
EOF

cat > PlotRain_st4_3hr.ncl << EOF

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

function format_date(cdate[*]:character)
local nn,aux,date
begin

  nn = dimsizes(chartostring(cdate))
  aux  = chartostring(cdate(0:3))+chartostring(cdate(5:6)) \
       + chartostring(cdate(8:9))+chartostring(cdate(11:12))
  date = stringtointeger(aux)
  return(date)
end

begin


  Date = stringtointeger(getenv("DATE"))

  res = True
  res@cnLinesOn          = False
  res@cnFillOn           = True               ; color plot desired
  res@cnLineLabelsOn     = False              ; turn off contour lines
  res@cnInfoLabelOn       = False           ; turn off cn info label

  res@mpGeophysicalLineColor= "black" ; color of continental outlines
  res@mpPerimOn             = True ; draw box around map
  res@mpPerimDrawOrder      = "PostDraw"
  res@mpGridLineDashPattern = 2 ; lat/lon lines as dashed
  res@mpOutlineBoundarySets = "GeophysicalAndUSStates"
  res@mpUSStateLineColor    = "black"

  FileName = getenv("FileName")
  RGBrain_file = getenv("RGBrain_file")
  ff = addfile(FileName,"r")

  if (ff@map_proj .eq. 1) then
    res@mpProjection        = "LambertConformal"
  end if
  res@mpLambertMeridianF    = ff@central_longitude
  res@mpLambertParallel1F   = ff@truelat1
  res@mpLambertParallel2F   = ff@truelat2

  res@gsnAddCyclic          = False
  res@tfDoNDCOverlay        = True
  res@mpLimitMode           = "Corners"
  res@mpLeftCornerLatF      = ff@lat_SW_corner
  res@mpLeftCornerLonF      = ff@lon_SW_corner
  res@mpRightCornerLatF     = ff@lat_NE_corner
  res@mpRightCornerLonF     = ff@lon_NE_corner

  res@cnLevelSelectionMode = "ExplicitLevels"
  res@cnLevels  = (/0.1,0.2,0.4,0.8,1.6,3.2,6.2,12.8,25.6,51.2/)
  res@gsnSpreadColors   = True
  res@cnMissingValFillColor = 13

  time = ff->time
  nn = dimsizes(time)

  Times = ff->Times
  aux = dimsizes(Times)
  prec3d = ff->RAIN(:,:,:)

  res@gsnRightString = " "
  res@gsnMaximize      = True


  do n=1,nn-3,3
     date = format_date(Times(n+2,:))
     if(Date .eq. date) then
        wks = gsn_open_wks("ps",date+".3hrain")
        cmap = RGBtoCmap(RGBrain_file)
        gsn_define_colormap(wks,cmap) 
   
        prec = dim_sum(prec3d(south_north|:,west_east|:,Time|n:n+2))
        ;print(Times(n+2,:)+" Min/Max: "+min(prec)+"  "+max(prec))
        res@tiMainString = "Stage IV precipitation (mm)"
        ;res@gsnLeftString = "3-hour ending at: "+Times(n+2,:)
        res@gsnLeftString = "validated at "+date
        map = gsn_csm_contour_map(wks,prec,res)
     end if
  end do

  do n=0,nn-1
     date = format_date(Times(n,:))
     if(Date .eq. date ) then
         wks = gsn_open_wks("ps",date+".1hrain")
         cmap = RGBtoCmap(RGBrain_file)
         gsn_define_colormap(wks,cmap) 

         prec = prec3d(n,:,:)
         ;print(Times(n,:)+" Min/Max: "+min(prec)+"  "+max(prec))
         res@tiMainString = "Stage IV 1 hour precipitation (mm)"
         ;res@gsnLeftString = "1-hour ending at: "+Times(n,:)
         res@gsnLeftString = "validated at "+date
         map = gsn_csm_contour_map(wks,prec,res)
     end if
  end do
end
EOF

if(  $#argv == 1 ) then
    setenv ncl_exe PlotRain_3hr.ncl
else
    setenv ncl_exe PlotRain_st4_3hr.ncl
    setenv DATE $2
endif

##3. set the input netcdf file.
setenv FileName   $1
if(!(-e $FileName)) then
   echo "Error: input netcdf file can not be found."
   exit
endif

##4. set the path of the color scale.
setenv RGBrain_file  RGBrain.txt


##5. run the ncl script. 
##   the output ps file is the "validated time".ps
ncl $ncl_exe

##6. assign proper name for the final plot

if(  $#argv == 1 ) then
 set FileImg = `echo $FileName |sed s/.nc/.ps/g`
 set FilePS  = `ls *.ps |tail -1 `
 mv $FilePS $FileImg
else
 #set FileImg = `echo $FileName |sed s/.nc/.$1.ps/g`
 #set FilePS  = `ls *.ps |grep 3hrain |tail -1 `
endif

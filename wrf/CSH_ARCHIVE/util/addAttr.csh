#!/bin/csh -f

set _this_script_name = `basename $0` 
echo "${_this_script_name} is called $1, $2"
if ( ! $?NCARG_ROOT ) then
  echo "${_this_script_name}: === ERROR === The env. variable NCARG_ROOT is not defined!!!"
  exit 10
endif

ln -sf "$1" inFile.nc 
ln -sf "$2" outFile.nc 

if (-e tmp.ncl ) rm tmp.ncl

cat > tmp.ncl <<EOF
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin
  inFile = addfile("inFile.nc","r")
  outFile = addfile("outFile.nc","w")
;  outFile@NUM_LAND_CAT = inFile@NUM_LAND_CAT 
;  copy global attributes
  att_names = getvaratts(inFile)           ; get input file's global attributes
  if(.not.all(ismissing(att_names))) then
    do i = 0,dimsizes(att_names)-1
      outFile@\$att_names(i)\$ = inFile@\$att_names(i)\$     ; copy input file's global attributes
    end do
  end if

end
EOF

if ( -e $NCARG_ROOT ) then
  $NCARG_ROOT/bin/ncl < tmp.ncl
else
  echo "${_this_script_name}: === WARN ==== Can not add the global attributes because $NCARG_ROOT does not exist!"
endif
rm tmp.ncl inFile.nc outFile.nc

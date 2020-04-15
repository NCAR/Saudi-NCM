;------------------------------------------------------------------------------#
; Copyright UCAR (c) 1992 - 2006.
; University Corporation for Atmospheric Research (UCAR),
; National Center for Atmospheric Research (NCAR),
; Research Applications Laboratory (RAL),
; P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
;------------------------------------------------------------------------------#

procedure change_date(ccyy:integer, mm:integer, dd:integer, delta:integer)

begin
  mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
  mmday(1) = 28

  aux = ccyy/4
  mod = ccyy - 4*aux
  if ( mod .eq. 0 ) then
    aux = ccyy/400
    mod = ccyy -400*aux
    if ( mod .eq. 0 ) then
      mmday(1) = 29
    else 
      mmday(1) = 29
    end if
  end if

  dd = dd + delta
  if ( dd .eq. 0 ) then
    mm = mm - 1
    if ( mm .eq. 0 ) then
      mm = 12
      ccyy = ccyy - 1
    end if
    dd = mmday(mm-1)
  end if
  if ( dd .gt. mmday(mm-1) ) then
    dd = 1
    mm = mm + 1
    if ( mm .gt. 12 ) then
      mm = 1
      ccyy = ccyy + 1
    end if
  end if
  end

function advance_ccyymmddhh(ccyymmddhh:integer,dh:integer)
begin

  ccyy   = ccyymmddhh / 1000000
  mmddhh = ccyymmddhh   - (ccyymmddhh/1000000) * 1000000
  mm     = mmddhh / 10000
  dd     = (mmddhh -  ( mmddhh/10000 ) * 10000) / 100
  hh     = mmddhh - ( mmddhh/100 ) * 100
  
  hh = hh + dh
  do while (hh .gt. 23 .or. hh .lt. 0)
    if ( hh .lt. 0 ) then
      hh = hh + 24
      change_date ( ccyy, mm, dd, -1 )
    end if
    if ( hh .gt. 23 ) then
      hh = hh - 24
      change_date ( ccyy, mm, dd, 1 )
    end if
  end do
  cymdh_new = ccyy * 1000000 + mm * 10000 + dd * 100 + hh

  return cymdh_new

end

function geth_idts (ndate:integer, odate:integer)

;  From 2 input mdates ('YYYYMMDDHH),
;  compute the time difference.

;  on entry     -  ndate  -  the new hdate.
;                  odate  -  the old hdate.

;  on exit      -  idt    -  the change in time.

begin

  aux = sprinti("%0.10i",ndate)
  cdate = stringtochar(aux)
  yrnew = stringtointeger(chartostring(cdate(0:3)))
  monew = stringtointeger(chartostring(cdate(4:5)))
  dynew = stringtointeger(chartostring(cdate(6:7)))
  hrnew = stringtointeger(chartostring(cdate(8:9)))

  aux = sprinti("%0.10i",odate)
  cdate = stringtochar(aux)
  yrold = stringtointeger(chartostring(cdate(0:3)))
  moold = stringtointeger(chartostring(cdate(4:5)))
  dyold = stringtointeger(chartostring(cdate(6:7)))
  hrold = stringtointeger(chartostring(cdate(8:9)))

  time = 0
  time@units = "hours since 1995-01-01 00:00:0.0"
  time_new = ut_inv_calendar(yrnew,monew,dynew,hrnew,0,0,time@units, 0)
;  print(time_new)
  time_old = ut_inv_calendar(yrold,moold,dyold,hrold,0,0,time@units, 0)
;  print(time_old)
  idt = time_new - time_old
  return idt
end

 
  




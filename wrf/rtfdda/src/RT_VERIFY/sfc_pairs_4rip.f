  program sfc_pairs_4rip
!
  integer :: ierro=0,ierrm=0
  character(len=14) :: time_strm,time_stro,abc
  character(len=10) :: atime_new,model_time_new
  character(len=28) :: otll,mtll,cndom
  character(len=4) :: platform
  character(len=1) :: adid
  real :: alat,alon,psfc_obs,slp_obs,h_obs,t_obs,td_obs,ws_obs,wd_obs,q_obs
  integer*2 :: psfc_obs_qc,slp_obs_qc,t_obs_qc,td_obs_qc,ws_obs_qc, &
             wd_obs_qc,q_obs_qc
  real :: blat,blon,psfc_m,slp_m,h_m,t_m,td_m,ws_m,wd_m,q_m,q2_m,u10_m,v10_m,tmk_m
  integer*2 :: psfc_m_qc,slp_m_qc,t_m_qc,td_m_qc,ws_m_qc, &
             wd_m_qc,q_m_qc
  integer*2 :: iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
               ipmstn,ipsfc_obs,islpstn,islp_obs,iterstn,ih_obs,it2stn,it_obs, &
               iqmstn,iq_obs,iwsstn,iws_obs,iwdstn,iwd_obs

  character (len=120), parameter :: infmto = &
            '(a14,f8.3,f9.3,x,a4,2(f9.2,i7),f9.2,4(f9.2,i7))'
  character (len=120), parameter :: infmtm = &
            '(i10,i3,f6.2,a1,f7.2,a6,12(f9.2))'

  real, parameter :: badvalue=-9999., missing=-8888.
  integer :: ierr, ier
  real, parameter :: grav=9.81,rgas=287.04
  real :: rhcalc
  integer :: integerize
  logical :: found
  found = .FALSE.
  irec = 0
  pi=acos(-1.)

  !  cndom = domain #
  CALL getarg(1, cndom)
  adid=TRIM(cndom)
  read(adid,FMT='(i1)') domain_id
  !domain_id=1
  !write(adid,'(i1)') domain_id
  open(31,file='obs.dat')
  open(32,file='verify_surface_rip.dat')
  open(71,file='pairs_domain'//adid,access='direct',recl=56)

SFC: DO WHILE (ierro == 0)
!
   read(31,infmto,iostat=ierro) time_stro,alat,alon,platform, &
     psfc_obs,psfc_obs_qc,slp_obs,slp_obs_qc,h_obs,t_obs, &
     t_obs_qc,td_obs,td_obs_qc,ws_obs,ws_obs_qc,wd_obs,wd_obs_qc
   if(ierro /= 0) exit SFC
   call nearest_hour(time_stro,atime_new)
   !write(otll,"(a14,2f7.2)") time_stro, alat, alon
   write(otll,"(a10,2f7.2)") atime_new, alat, alon
   print*,'obs sfc. time, lat, lon: ',atime_new, alat, alon

   if(abs(psfc_obs_qc-666666) < 1 .or. abs(slp_obs_qc -666666) < 1 .or. &
      abs(t_obs_qc -666666) < 1   .or. abs(td_obs_qc -666666) < 1  .or. &
      abs(ws_obs_qc -666666) < 1  .or. abs(wd_obs_qc -666666) < 1 ) then
      psfc_obs_qc=26666
      slp_obs_qc=26666
      t_obs_qc=26666
      td_obs_qc=26666
      ws_obs_qc=26666
      wd_obs_qc=26666
   endif

!            if(psfc_obs_qc >= 32768) psfc_obs_qc=32767
!            if(slp_obs_qc >= 32768) slp_obs_qc=32767
!            if(t_obs_qc >= 32768) t_obs_qc=32767
!            if(td_obs_qc >= 32768) td_obs_qc=32767
!            if(ws_obs_qc >= 32768) ws_obs_qc=32767
!            if(wd_obs_qc >= 32768) wd_obs_qc=32767

!obtain q obs from t obs and td obs:

    call qfromttd(t_obs,t_obs_qc,td_obs,td_obs_qc, &
                  psfc_obs,psfc_obs_qc,q_obs,q_obs_qc)
    if(q_obs > missing) q_obs=q_obs*1000.    !! in g/kg
    if(t_obs > missing) t_obs=t_obs-273.15
    if(td_obs > missing) td_obs=td_obs-273.15
!
!    Find the matching MM5 Pseudo-soundings
!
    MODEL:  do while(ierrm== 0)
!
    if(.not. found) then
     read(32,infmtm,iostat=ierrm) mdate,mfcsth,blat,abc(1:1),blon,abc(1:6), &
!        psfc_m,slp_m,h_m,t_m,td_m,ws_m,wd_m,q_m       ! Use Q2 -- bad for mm5 known problem
         psfc_m,slp_m,h_m,t_m,td_m,ws_m,wd_m,q2_m,q_m,u10_m,v10_m,tmk_m   ! Use first sigma Q

!  ws_m wd_m from rip4 are 1st sigma winds, use 10 m instead
     ws_m = sqrt(u10_m*u10_m + v10_m*v10_m)
     wd_m=(1.5*pi-atan2(v10_m,u10_m))*180./pi
     if(wd_m >= 360.) wd_m=wd_m-360.

     if(ierrm /= 0) exit SFC
     call get_time_str(time_strm,mdate,0)
     call nearest_hour(time_strm,model_time_new)
     !write(mtll,"(a14,2f7.2)") time_strm, blat, blon
     write(mtll,"(a10,2f7.2)") model_time_new, blat, blon
     print*,'mod sfc. time, lat, lon: ',model_time_new, blat, blon
    endif

     if(mtll > otll) then
      print*, "   Pass O -- Should not happen ... "
      print*, "       O ",atime_new,alat,alon
      print*, "       M ",model_time_new,blat,blon
      found = .TRUE.
      cycle SFC
     endif

     found =.FALSE.
     ifound = 0

     if(mtll < otll) then 
      print*,'   Pass M time, lat, lon: ',model_time_new, blat, blon
      cycle MODEL
     endif

     print*,'--- Get pairs time, lat, lon: ',atime_new, alat, alon
     ifound = 1
     exit MODEL

    enddo MODEL

    if(ifound == 1) then

    t2stn=t_m
    pmstn=psfc_m
    qmstn=q2_m
    terstn=h_m
    slpstn=slp_m
!   u10stn=
!   v10stn=
!   if((u > missing) .and. (v > missing)) then
!       arg=(rlon-xlonc)*xn*pi/180.
!       u10stn=u*cos(arg)+v*sin(arg)
!       v10stn=-u*sin(arg)+v*cos(arg)
!       wsstn=sqrt(u**2+v**2)
!       wdstn=(1.5*pi-atan2(v10stn,u10stn))*180./pi
!     if(wdstn >= 360.) wdstn=wdstn-360.
!   else
!     wsstn=missing
!     wdstn=missing
!   endif
    wsstn=ws_m 
    wdstn=wd_m
!
!   temperature correction accounting for elevation difference in model
!   and station, use corrected t to calculate rh
!
    if(t2stn > missing) then
       if(t2stn < 100.) t2stn = t2stn + 273.16
       t2stn_orig=t2stn
       t2stn=t2stn+0.0065*(terstn-h_obs)
       t2stn=t2stn-273.15
    endif
!
!   Psfc adjustment for Terrain difference
!
    if((pmstn > missing) .and. (t2stn > missing)) then
       zdiff=terstn-h_obs
       pmstn=pmstn*exp(zdiff*grav/rgas/(0.5*(t2stn+273.15+t2stn_orig)))
    endif

    read(time_stro,'(i4,4i2)') iyear,imonth,iday,ihour,imin
    imonthday=imonth*100+iday
    ihourmin=ihour*100+imin

    ilat=nint(alat*100)
    ilon=nint(alon*100)

    ipmstn=nint(pmstn*10.)                   !! pressure is multiply
    if(psfc_obs > 0.) then                   !! by a factor of 10.
     ipsfc_obs=nint(psfc_obs*10.)
    else
     ipsfc_obs=missing
    endif

    islpstn=nint(slpstn*10.)                 !! same comment as
    if(slp_obs > 0.) then                    !! above
     islp_obs=nint(slp_obs*10.)
    else
     islp_obs=missing
    endif

    iterstn=nint(terstn)
    ih_obs=int(h_obs)
       
    it2stn=nint(t2stn*100.)
    it_obs=integerize(t_obs,missing)

    iqmstn=nint(qmstn*100)
    iq_obs=integerize(q_obs,missing)

    iwsstn=nint(wsstn*100.)
    iws_obs=integerize(ws_obs,missing)

    iwdstn=nint(wdstn)
    iwd_obs=nint(wd_obs)

    irec=irec+1
       !      print*, iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
       !        platform, &
       !        ipmstn,ipsfc_obs,psfc_obs_qc, &
       !        islpstn,islp_obs,slp_obs_qc, &
       !        iterstn,ih_obs, &
       !        it2stn,it_obs,t_obs_qc, &
       !        iqmstn,iq_obs,q_obs_qc, &
       !        iwsstn,iws_obs,ws_obs_qc, &
       !        iwdstn,iwd_obs,wd_obs_qc
     write(71,rec=irec) iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
        platform, &
        ipmstn,ipsfc_obs,psfc_obs_qc, &
        islpstn,islp_obs,slp_obs_qc, &
        iterstn,ih_obs, &
        it2stn,it_obs,t_obs_qc, &
        iqmstn,iq_obs,q_obs_qc, &
        iwsstn,iws_obs,ws_obs_qc, &
        iwdstn,iwd_obs,wd_obs_qc
  
    endif
 
END DO SFC

 end program sfc_pairs_4rip

!
!
  function rhcalc(t,q,pm)
!
     es=10**(-2937.4/t-4.9283*log10(t)+23.5518)  !! in mb
     re=q/(1.-q)
     ee=pm*re/(0.622+re)
     rhcalc=ee/es*100.
     if(rhcalc > 100.) rhcalc=100.

  return
  end function rhcalc
!
!
  function integerize(val,missing)
!
  real :: val,missing
  integer :: integerize
!
  if(val > missing) then
    integerize=nint(val*100)
  else
    integerize=missing
  endif
!
  return
  end function integerize
!
!
!
  subroutine qfromttd(t,t_qc,td,td_qc,pp,pp_qc,q,q_qc)

  real :: t,td
  integer :: t_qc,td_qc,pp_qc,q_qc
  real, parameter :: eps=0.622, xrv=461.51

  if((t < 0.) .or. (td < 0.) .or. (pp < 0.)) then
    q=-8888.
  else
    xlv=(2.5-0.002274*(t-273.15))*1000000.
    x=xlv/xrv*(t-td)/t/td
    rh=exp(-x)
    if(rh > 1.) rh=1.
    if(rh < 0.) rh =0.
    es=10**(-2937.4/t-4.9283*log10(t)+23.5518)   !! in mb
    ee=rh*es                                     !! in mb
    q=eps*ee/(pp-ee)                      !! pp needs to be in mb
  endif

  q_qc=max(t_qc,td_qc,pp_qc)

  return
  end subroutine qfromttd
!
!
!
  function slpcal(psfc,t2,ter,q)
!
  real :: psfc,t2,ter,q,slpcal
  real, parameter :: rgas=287.05, gamma=0.0065, g=9.8

  qs=q*0.001              !! Note: Do not use q=q*0.001, because in so doing
                          !! you overwrite the q unit in the main program!
  tv=t2*(1.+0.61*qs/(1.+qs))
! slpcal=psfc*(1.+gamma/tv*ter)**(g/rgas/gamma)
  slpcal=psfc*exp(ter*g/rgas/tv)

  return
  end function slpcal
!
subroutine nearest_hour(atime,atime_new)

  character (len=14) :: atime
  character (len=10) :: atime_new
  character (len=4) :: ayear
  character (len=3) :: amonth,aday,ahour

  integer, dimension(12) :: days=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  integer :: year,month,day,hour,minute,sec

  read(atime,'(i4,5i2)') year,month,day,hour,minute,sec

  if(mod(year,400) == 0) then
    days(2)=29
  else
    if((mod(year,4) == 0) .and. (mod(year,100) /= 0)) days(2)=29
  endif

  if(sec >= 30) then
    minute=minute+1
    if(minute >= 60) then
      minute=minute-60
      hour=hour+1
      if(hour >= 24) then
        hour=hour-24
        day=day+1;
        if(day > days(month)) then
          day=day-days(month)
          month=month+1
          if(month > 12) then
            month=month-12
            year=year+1
          endif
        endif
      endif
    endif
  endif

  if(minute >= 45) then
    minute = 0
    hour=hour+1
    if(hour >= 24) then
      hour=hour-24
      day=day+1
      if(day > days(month)) then
        day=day-days(month)
        month=month+1
        if(month > 12) then
          month=month-12
          year=year+1
        endif
      endif
    endif
  endif

  write(ayear,'(i4)') year

  month=100+month
  write(amonth,'(i3)') month

  day=100+day
  write(aday,'(i3)') day

  hour=100+hour
  write(ahour,'(i3)') hour

  atime_new=ayear//amonth(2:3)//aday(2:3)//ahour(2:3)

  return

end subroutine nearest_hour
!
!
subroutine get_time_str(time_str,mdate,ih)
!
  character(len=14) :: time_str
!
  integer :: year_o,month_o,day_o,hour_o,min_o,ih
  integer, dimension(12) :: days
!
  days=(/31,28,31,30,31,30,31,31,30,31,30,31/)

  year_o=mdate/1000000
  month_o=(mdate - year_o*1000000)/10000
  day_o=(mdate - year_o*1000000 - month_o*10000)/100
  hour_o=mdate - year_o*1000000 - month_o*10000 - day_o*100
  min_o=0

  hour_o=hour_o+ih

  if(mod(year_o,4) == 0) then
    if(mod(year_o,100) == 0) then
      if(mod(year_o,400) == 0) then
        days(2)=29
      endif
    else
      days(2)=29
    endif
  endif

  if(min_o >= 59) then
    min_o=0
    hour_o = hour_o + 1
    if(hour_o >= 24) then
      hour_o = hour_o - 24
      day_o = day_o + 1
      if(day_o > days(month_o)) then
        day_o = day_o - days(month_o)
        month_o = month_o + 1
        if(month_o >= 12) then
          month_o = month_o -12
          year_o = year_o + 1
        endif
      endif
    endif
  endif
!
  write(time_str,"(i4,5i2.2)") year_o,month_o,day_o,hour_o,0,0
  return

  end subroutine get_time_str




  program cress_sfc_interp
!
  integer, dimension(50,20) :: bhi
  real, dimension(20,20) :: bhr
  character(len=80), dimension(50,20) :: bhic
  character(len=80), dimension(20,20) :: bhrc
  character(len=120) :: flnm
  integer :: iunit = 10, iout=20
  integer :: flag, i, ihcount, incount
  integer :: ierro=0,ierrm=0

  character(len=14) :: time_stro,abc
  character(len=10) :: atime_new,otime_new
  character(len=28) :: otll,mtll
  character(len=4) :: platform
  character(len=1) :: adid
  real, dimension(100) :: xlat,xlon,xlatj,xloni,psfc,slp,h,t,ws,wd,q,rh,u,v
  character(len=14), dimension(100) :: time_str,htime_str
  real, dimension(10) :: hxlat,hxlon,hxlatj,hxloni,hpsfc,hslp,hh,ht,hws,hwd,hq,hrh,hu,hv
  real, allocatable, dimension(:,:) :: Dpsfc,Dslp,Dh,Dt,Dws,Dwd,Dq,Drh,Du,Dv
  real :: alat,alon,psfc_obs,slp_obs,h_obs,t_obs,ws_obs,wd_obs,q_obs
  real :: psfc_model,slp_model,h_model,t_model,ws_model,wd_model,q_model
  integer*2 :: psfc_obs_qc,slp_obs_qc,t_obs_qc,ws_obs_qc,wd_obs_qc,q_obs_qc
  integer*2,dimension(100):: psfc_qc,slp_qc,t_qc,ws_qc,wd_qc,q_qc
  integer*2,dimension(10):: hpsfc_qc,hslp_qc,ht_qc,hws_qc,hwd_qc,hq_qc
  integer*2 :: iyear,imonthday,ihourmin,ilat,ilon,idomain, &
               ipmstn,ipsfc_obs,islpstn,islp_obs,iterstn,ih_obs,it2stn,it_obs, &
               iqmstn,iq_obs,iwsstn,iws_obs,iwdstn,iwd_obs

  character (len=120), parameter :: infmto = &
            '(a14,f8.3,f9.3,x,a4,2(f9.2,i7),f9.2,4(f9.2,i7),7f9.2)'

  real, parameter :: badvalue=-9999., missing=-8888.
  integer :: ierr, ier
  real, parameter :: grav=9.81,rgas=287.04
  real :: rhcalc
  integer :: integerize
  logical :: found
  found = .FALSE.
  pi=acos(-1.)

!
  iunit=32
  idomain=1
  open(31,file='obs.dat')
  write(adid,'(i1)') idomain
  open(71,file='pairs_domain'//adid,access='direct',recl=56)

  call getarg(1,flnm)
  open(iunit,file=flnm,form='unformatted',status='old')
  read(iunit,iostat=ierr) flag
  if(flag == 0) then
     read(iunit,iostat=ier) bhi, bhr, bhic, bhrc
     close(iunit)
      if(ier/=0) then
        write(*,'("Error reading big header")')
        call abort()
      endif
    idomain = BHI( 13, 1)
    MIX=BHI( 16, 1)
    MJX=BHI( 17, 1)
    allocate(Dpsfc(mix,mjx))
    allocate(Dslp(mix,mjx))
    allocate(Dh(mix,mjx))
    allocate(Dt(mix,mjx))
    allocate(Du(mix,mjx))
    allocate(Dv(mix,mjx))
    allocate(Dq(mix,mjx))
  endif

  irec=0
  incount=0
  ihcount=0
THISHH: DO WHILE (ierro == 0)
   otime_new=atime_new
   read(31,infmto,iostat=ierro) time_stro,alat,alon,platform, &
     psfc_obs,psfc_obs_qc,slp_obs,slp_obs_qc,h_obs,t_obs, &
     t_obs_qc,q_obs,q_obs_qc,ws_obs,ws_obs_qc,wd_obs,wd_obs_qc, &
     psfc_model,slp_model,h_model,t_model,q_model,ws_model,wd_model

   if(ierro /= 0) then
        time_stro="21000000000000"
   endif

     if(abs(h_obs-h_model).ge.400.0) then
       psfc_obs=missing
       slp_obs=missing
       t_obs=missing
       q_obs=missing
       ws_obs=missing
       wd_obs=missing
     endif
     if(abs(t_obs-t_model).ge.15.0) t_obs=missing
     if(abs(psc_obs-psc_model).ge.10.0) psc_obs=missing
     if(abs(slp_obs-slp_model).ge.10.0) slp_obs=missing
     if(abs(q_obs-q_model).ge.10.0) q_obs=missing
     if(abs(wd_obs-wd_model).ge.360.0 .or. abs(ws_obs-ws_model).gt.20.0) then
       wd_obs=missing
       ws_obs=missing
     endif

   call nearest_hour(time_stro,atime_new)
   write(otll,"(a14,2f7.2)") time_stro, alat, alon
   if((incount+ihcount)==0) otime_new = atime_new
   ! print *, time_stro, otime_new, atime_new, alat, alon
   !   stop

  if(atime_new .eq. otime_new) then 
     call llxy(alat,alon,yi,xj,bhi,bhr) 
      xj = xj - 0.5  ! crs point
      yi = yi - 0.5  ! crs point
     if(xj < 1 .or. yi < 1 .or. xj > mix-1 .or. yi > mjx-1) then
      cycle THISHH
     else if (abs(psfc_obs_qc-26666) < 1 .or. abs(slp_obs_qc -26666) < 1 .or. &
              abs(t_obs_qc -26666) < 1   .or. abs(q_obs_qc -26666) < 1 .or. &
              abs(ws_obs_qc -26666) < 1  .or. abs(wd_obs_qc -26666) < 1 ) then
       print("(2a,2f9.2,2f5.1)"),'held: time,lat,lon,xj,yi:',atime_new, alat,alon,xj,yi
      ihcount = ihcount + 1
      htime_str(ihcount)=time_stro
      hxlat(ihcount)=alat
      hxlon(ihcount)=alon
      hxlatj(ihcount)=xj
      hxloni(ihcount)=yi
      hpsfc(ihcount)=psfc_obs
      hpsfc_qc(ihcount)=psfc_obs_qc
      hslp(ihcount)=slp_obs
      hslp_qc(ihcount)=slp_obs_qc
      hh(ihcount)=h_obs
      ht(ihcount)=t_obs
      ht_qc(ihcount)=t_obs_qc
      hq(ihcount)=q_obs
      hq_qc(ihcount)=q_obs_qc
      hws(ihcount)=ws_obs
      hws_qc(ihcount)=ws_obs_qc
      hwd(ihcount)=wd_obs
      hwd_qc(ihcount)=wd_obs_qc
      if(ws_obs > missing .and. wd_obs > missing) then
       hu(ihcount)=-ws_obs*sin(wd_obs*pi/180)
       hv(ihcount)=-ws_obs*cos(wd_obs*pi/180)
      else
       hu(ihcount)=missing
       hv(ihcount)=missing 
      endif
     else
      print ("(a,a,2f9.2,2f5.1)"),'Not held: time,lat,lon,xj,yi:',atime_new, alat,alon,xj,yi
      incount = incount + 1
      time_str(incount)=time_stro
      xlat(incount)=alat
      xlon(incount)=alon
      xlatj(incount)=xj
      xloni(incount)=yi
      psfc(incount)=psfc_obs
      psfc_qc(incount)=psfc_obs_qc
      slp(incount)=slp_obs
      slp_qc(incount)=slp_obs_qc
      h(incount)=h_obs
      t(incount)=t_obs
      t_qc(incount)=t_obs_qc
      q(incount)=q_obs
      q_qc(incount)=q_obs_qc
      ws(incount)=ws_obs
      ws_qc(incount)=ws_obs_qc
      wd(incount)=wd_obs
      wd_qc(incount)=wd_obs_qc
      if(ws_obs > missing .and. wd_obs > missing) then
       u(incount)=-ws_obs*sin(wd_obs*pi/180)
       v(incount)=-ws_obs*cos(wd_obs*pi/180)
      else
       u(incount)=missing
       v(incount)=missing
      endif
     endif
  else
      NSTA=incount
     do i = 1,ihcount
      incount = incount + 1
      time_str(incount)=htime_str(i)
      xlat(incount)=hxlat(i)
      xlon(incount)=hxlon(i)
      xlatj(incount)=hxlatj(i)
      xloni(incount)=hxloni(i)
      psfc(incount)=hpsfc(i)
      psfc_qc(incount)=hpsfc_qc(i)
      slp(incount)=hslp(i)
      slp_qc(incount)=hslp_qc(i)
      h(incount)=hh(i)
      t(incount)=ht(i)
      t_qc(incount)=ht_qc(i)
      q(incount)=hq(i)
      q_qc(incount)=hq_qc(i)
      ws(incount)=hws(i)
      ws_qc(incount)=hws_qc(i)
      wd(incount)=hwd(i)
      wd_qc(incount)=hwd_qc(i)
      u(incount)=hu(i)
      v(incount)=hv(i)
     enddo
      
      if(IDOMAIN.EQ.2) then
      RIN = 30    
      elseif(IDOMAIN.EQ.3) then
      RIN = 60    
      elseif(IDOMAIN.EQ.4) then
      RIN = 30
      endif

        print *, "T",NSTA,RIN
        print *, "t",(t(i),i=1,nsta)
        print *, "q",(q(i),i=1,nsta)
        print *, "u",(u(i),i=1,nsta)
        print *, "v",(v(i),i=1,nsta)
        print *, "psfc",(psfc(i),i=1,nsta)
        print *, "slp",(slp(i),i=1,nsta)


      RIN = 5.
      CALL CRESSAN(Dt,t,XLONI,XLATJ,MIX,MJX,NSTA,RIN,1)
      CALL CRESSAN(Dq,q,XLONI,XLATJ,MIX,MJX,NSTA,RIN,1)
      CALL CRESSAN(Du,u,XLONI,XLATJ,MIX,MJX,NSTA,RIN,1)
      CALL CRESSAN(Dv,v,XLONI,XLATJ,MIX,MJX,NSTA,RIN,1)
      CALL CRESSAN(Dh,h,XLONI,XLATJ,MIX,MJX,NSTA,RIN,1)
      CALL CRESSAN(Dpsfc,psfc,XLONI,XLATJ,MIX,MJX,NSTA,RIN,1)
      CALL CRESSAN(Dslp,slp,XLONI,XLATJ,MIX,MJX,NSTA,RIN,1)
      print ("(f7.1,10f7.2)"),RIN,(Dt(i,32),i=32,41)
      print ("(f7.1,10f7.1)"),RIN,(Dt(32,i),i=32,41)
      !print *,DT
      i=1
     if(i==1) then
      RIN = 10.
      CALL CRESSAN(Dt,T,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dq,q,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Du,u,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dv,v,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dh,h,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dpsfc,psfc,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dslp,slp,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      print ("(f7.1,10f7.2)"),RIN,(Dt(i,32),i=32,41)
      print ("(f7.1,10f7.2)"),RIN,(Dt(32,i),i=32,41)
      RIN = 20.
      CALL CRESSAN(Dt,T,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dq,q,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Du,u,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dv,v,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dh,h,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dpsfc,psfc,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dslp,slp,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      print ("(f7.1,10f7.2)"),RIN,(Dt(i,32),i=32,41)
      print ("(f7.1,10f7.2)"),RIN,(Dt(32,i),i=32,41)
      RIN = 30.
      CALL CRESSAN(Dt,T,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dq,q,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Du,u,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dv,v,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dh,h,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dpsfc,psfc,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dslp,slp,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      print ("(f7.1,10f7.2)"),RIN,(Dt(i,32),i=32,41)
      print ("(f7.1,10f7.2)"),RIN,(Dt(32,i),i=32,41)
      RIN = 15.
      CALL CRESSAN(Dt,T,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dq,q,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Du,u,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dv,v,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dh,h,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dpsfc,psfc,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dslp,slp,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      print ("(f7.1,10f7.2)"),RIN,(Dt(i,32),i=32,41)
      print ("(f7.1,10f7.2)"),RIN,(Dt(32,i),i=32,41)
      RIN = 5.
      CALL CRESSAN(Dt,T,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dq,q,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Du,u,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dv,v,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dh,h,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dpsfc,psfc,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dslp,slp,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      print ("(f7.1,10f7.2)"),RIN,(Dt(i,32),i=32,41)
      print ("(f7.1,10f7.2)"),RIN,(Dt(32,i),i=32,41)
      RIN = 2
      CALL CRESSAN(Dt,T,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dq,q,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Du,u,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dv,v,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dh,h,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dpsfc,psfc,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dslp,slp,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      print ("(f7.1,10f7.2)"),RIN,(Dt(i,32),i=32,41)
      print ("(f7.1,10f7.2)"),RIN,(Dt(32,i),i=32,41)
      RIN = 0.8
      CALL CRESSAN(Dt,T,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dq,q,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Du,u,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dv,v,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dh,h,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dpsfc,psfc,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      CALL CRESSAN(Dslp,slp,XLONI,XLATJ,MIX,MJX,NSTA,RIN,2)
      print ("(f7.1,10f7.2)"),RIN,(Dt(i,32),i=32,41)
      print ("(f7.1,10f7.2)"),RIN,(Dt(32,i),i=32,41)
     endif
    do i = 1, incount 
    iy=int(xlatj(i)+0.5)
    jx=int(xloni(i)+0.5)
    t2stn=Dt(iy,jx)
    pmstn=Dpsfc(iy,jx)
    qmstn=Dq(iy,jx)
    terstn=Dh(iy,jx)
    slpstn=Dslp(iy,jx)
    ustn=Du(iy,jx)
    vstn=Dv(iy,jx)
    print ("(a,3i3,8f10.2)"), "i,iy,jx",i,iy,jx,xlat(i),xlon(i),terstn,t2stn,qmstn,ustn,vstn,pmstn
    print ("(a,3i3,8f10.2)"), "  iy,jx",i,iy,jx,xlat(i),xlon(i),h(i),t(i),q(i),u(i),v(i),psfc(i)
    print *, " "
    !stop

    wsstn=sqrt(ustn**2+vstn**2)
    wdstn=(1.5*pi-atan2(vstn,ustn))*180./pi
    if(wdstn >= 360.) wdstn=wdstn-360.
!
!   temperature correction accounting for elevation difference in model
!   and station, use corrected t to calculate rh
!
    if(t2stn > missing) then
       t2stn_orig=t2stn
       t2stn=t2stn+0.0065*(terstn-h(i))
    endif
!
!   Psfc adjustment for Terrain difference
!
    if((pmstn > missing) .and. (t2stn > missing)) then
       zdiff=terstn-h(i)
       pmstn=pmstn*exp(zdiff*grav/rgas/(0.5*(t2stn+t2stn_orig)+273.15))
    endif

    read(time_str(i),'(i4,4i2)') iyear,imonth,iday,ihour,imin
    imonthday=imonth*100+iday
    ihourmin=ihour*100+imin

    ilat=nint(xlat(i)*100)
    ilon=nint(xlon(i)*100)

    ipmstn=nint(pmstn*10.)                   !! pressure is multiply
    if(psfc(i) > 0.) then                   !! by a factor of 10.
     ipsfc_obs=nint(psfc(i)*10.)
    else
     ipsfc_obs=missing
    endif

    islpstn=nint(slpstn*10.)                 !! same comment as
    if(slp(i) > 0.) then                    !! above
     islp_obs=nint(slp(i)*10.)
    else
     islp_obs=missing
    endif

    iterstn=nint(terstn)
    ih_obs=int(h(i))
       
    it2stn=nint(t2stn*100.)
    it_obs=integerize(t(i),missing)

    iqmstn=nint(qmstn*100)
    iq_obs=integerize(q(i),missing)

    iwsstn=nint(wsstn*100.)
    iws_obs=integerize(ws(i),missing)

    iwdstn=nint(wdstn)
    iwd_obs=nint(wd(i))

    irec=irec+1
    !         print*, iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
    !           platform, &
    !           ipmstn,ipsfc_obs,psfc_obs_qc, &
    !           islpstn,islp_obs,slp_obs_qc, &
    !           iterstn,ih_obs, &
    !           it2stn,it_obs,t_obs_qc, &
    !           iqmstn,iq_obs,q_obs_qc, &
    !           iwsstn,iws_obs,ws_obs_qc, &
    !           iwdstn,iwd_obs,wd_obs_qc
     write(71,rec=irec) iyear,imonthday,ihourmin,ilat,ilon,idomain, &
        platform, &
        ipmstn,ipsfc_obs,psfc_qc(i), &
        islpstn,islp_obs,slp_qc(i), &
        iterstn,ih_obs, &
        it2stn,it_obs,t_qc(i), &
        iqmstn,iq_obs,q_qc(i), &
        iwsstn,iws_obs,ws_qc(i), &
        iwdstn,iwd_obs,wd_qc(i)
    enddo
  
  incount=0
  ihcount=0
  endif
 
END DO THISHH

end program cress_sfc_interp

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



!
!
  subroutine bi_linear(rfgrd,rfstn,ix,jx, &
             xlat,xlon,xlatgrd,xlongrd,IDOTCRS,dx,dy,i0,j0,iflag,jflag)
!
!     This Sub. interpolate the MODEL GRID data to the
!     observation (station) points.
!
!      input: RFGRD (Model Grid value.)
!     output: RFSTN (Random or observed station point value).
!      const: XX,YY
!
!
!      iflag : =0, the station is not in the model grid domain
!              =1, the station is in the model grid domain
!

  real, dimension(ix,jx) :: rfgrd,xlatgrd,xlongrd
!
!     I+1,J |          | I+1,J+1
!         --+----------+---
!           |          | DYM
!           |    *     + -
!           |   X,Y    | DY
!           |          |
!         --+----+-----+---
!        I,J|<DX>|<DXM>| I,J+1
!
      i0=0
      j0=0
      rfstn=-99.0
!
      iflag=0
      do j=1,jx-1-idotcrs
      do i=1,ix-1-idotcrs
         if(xlat.ge.xlatgrd(i,j) .and. &
            xlon.ge.xlongrd(i,j) .and. &
            xlat.le.xlatgrd(i+1,j) .and. &
            xlon.le.xlongrd(i,j+1)) THEN
         dx =(xlon-xlongrd(i,j))/(xlongrd(i,j+1)-xlongrd(i,j))
         dy =(xlat-xlatgrd(i,j))/(xlatgrd(i+1,j)-xlatgrd(i,j))
         dxm= 1.0-dx
         dym= 1.0-dy
         iflag=1
         i0=i
         j0=j
         write(8,*)'i0=',i0,' j0=',j0,' xlat= ',xlatgrd(i,j), &
              xlatgrd(i+1,j),' xlon= ',xlongrd(i,j),xlongrd(i,j+1)
         write(9,*) rfgrd(i,j),rfgrd(i,j+1),rfgrd(i+1,j),rfgrd(i+1,j+1)
         goto 10
         endif
      enddo
      enddo
      goto 30
   10 continue
!
      rfstn=dxm*(dym*rfgrd(i0,j0)+dy*rfgrd(i0+1,j0))+ &
            dx *(dym*rfgrd(i0,j0+1)+dy*rfgrd(i0+1,j0+1))
!        if(jflag .eq. 1) print*,i,j,rfgrd(i0,j0),rfgrd(i0+1,j0), &
!                                rfgrd(i0,j0+1),rfgrd(i0+1,j0+1)
!
   30 continue
      return
      end subroutine bi_linear
!
!
!
  function bi_linear_func(array,ix,jx,dx,dy,i0,j0)
!
  real :: bi_linear_func
  real, dimension(ix,jx) :: array
!
  dxm=1.0-dx
  dym=1.0-dy

  bi_linear_func=dxm*(dym*array(i0,j0)+dy*array(i0+1,j0))+ &
                 dx *(dym*array(i0,j0+1)+dy*array(i0+1,j0+1))

  return
  end
!
!
!
  subroutine right_time(time_str,current_date,iflag,iexceed)
!
  character(len=14) :: time_str
  character(len=24) :: current_date
!
  integer :: year_o,month_o,day_o,hour_o,min_o
  integer :: year_m,month_m,day_m,hour_m,min_m
  integer, dimension(12) :: days
  integer*8 :: time_o,time_m
!
  days=(/31,28,31,30,31,30,31,31,30,31,30,31/)

  read(time_str,'(i4,4i2)') year_o,month_o,day_o,hour_o,min_o
  read(current_date,'(i4,4(1x,i2))') year_m,month_m,day_m,hour_m,min_m

  if(mod(year_o,4) == 0) then
    if(mod(year_o,100) == 0) then
      if(mod(year_o,400) == 0) then
        days(2)=29
      endif
    else
      days(2)=29
    endif
  endif
      
  if(min_o >= 50) then
    min_o=0
    hour_o = hour_o + 1
    if(hour_o >= 24) then
      hour_o = hour_o - 24
      day_o = day_o + 1
      if(day_o >= days(month_o)) then
        day_o = day_o - days(month_o)
        month_o = month_o + 1
        if(month_o >= 12) then
          month_o = month_o -12
          year_o = year_o + 1
        endif
      endif
    endif
  elseif(min_o <= 10) then
    min_o=0
  endif
!
!
  if(min_o /= 0) then
    iflag=0
  else
    if((year_o == year_m) .and. (month_o == month_m) .and. &
       (day_o == day_m) .and. (hour_o == hour_m)) then
       iflag=1
    else
       iflag=0
    endif
  endif
!
  time_o=year_o*1000000+month_o*10000+day_o*100+hour_o
  time_m=year_m*1000000+month_m*10000+day_m*100+hour_m

  if(time_o > time_m) then
    iexceed=1
  else
    iexceed=0
  endif

  return
  end subroutine right_time

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

  SUBROUTINE CRESSAN(A2,ASTA1,XOBS1,YOBS1,IMAX,JMAX,NSTA1,RIN,ID)
!
!   Purpose : Create the gridded fields on mesoscale grids by
!             using the Cressman-type objective analysis technique.
!
!   ASTA(NSTA): Station Data Source
!   XOBS(NSTA): J indices of the source data.
!   YOBS(NSTA): I indices of the source data.
!   NSTA      : Number of data points.
!   IMAX,JMAX : The dimension of the mesoscale domain.
!   RIN       : INFLUENCE RADIUM (in grid units)
!   A2(IMAX,JMAX): Gridded objective analysis field
!   BADVAL: bad observation
!   ID: =1, no first guess field; =2, A2 as first guess.
!
      PARAMETER (MX=200,NSTAT=100000)
      DIMENSION A2(IMAX,JMAX),XOBS1(NSTA1),YOBS1(NSTA1),ASTA1(NSTA1),&
                COR(MX,MX),SUM(MX,MX),NS(MX,MX)
      DIMENSION XOBS(NSTAT),YOBS(NSTAT),ASTA(NSTAT) 
!
!     OBJECTIVE ANALYSIS TO FILL A GRID BASED ON OBSERVATIONS
!     XOBS AND YOBS ARE X AND Y POSITIONS ON OBSERVATIONS, NOT
!     NECESSARILY GRID POINTS.
!-----GRID LENGTHS IN X AND Y DIRECTIONS ARE UNITY.
!
       NSTA=NSTA1
       DO KK=1,NSTA
       XOBS(KK)=XOBS1(KK)
       YOBS(KK)=YOBS1(KK)
       ASTA(KK)=ASTA1(KK)
       ENDDO
      IF(ID.EQ.2) THEN
       IF(IMAX*JMAX.GT.NSTAT) then
       print *, 'need to increase NSTAT'
       stop 888
       ENDIF
       NSTA=NSTA1+IMAX*JMAX
       KK=NSTA1
       DO I=1,IMAX
       DO J=1,JMAX
       KK=KK+1
       XOBS(KK)=J
       YOBS(KK)=I
       ASTA(KK)=A2(I,J) 
       ENDDO
       ENDDO
      ENDIF
      

      BADVAL=-8888.0
      IE     = IMAX
      JE     = JMAX
      NSCAN  = 1
      RIS   = RIN**2
      do i = 1,IMAX
      do j = 1,JMAX
      COR(I,J) = 0.0
      SUM(I,J) = 0.0
      NS(I,J)  = 0
      enddo
      enddo
!
!-----BEGIN TO PROCESS THE NSTA OBSERVATIONS:
!
   CRESS: DO KK = 1,NSTA
      IF (abs(ASTA(KK)-BADVAL) < 1000. ) CYCLE CRESS
!
!-----DEFINE MAX AND MIN I AND J VALUES TO LIMIT THE NUMBER OF POINTS
!-----MUST BE CONSIDERED.
!
      RIOBS = YOBS(KK)
      RJOBS = XOBS(KK)
!
      IF(RJOBS.GT.JMAX+RIN .OR. RJOBS.LT.-RIN .OR. &
        RIOBS.GT.IMAX+RIN .OR. RIOBS.LT.-RIN) CYCLE CRESS
!
      YMAXI = RIOBS + RIN
      MAXI  = IFIX(YMAXI + 0.99)
      MAXI  = MIN0(MAXI,IE)
!
      YMINI = RIOBS - RIN
      MINI  = IFIX(YMINI)
      MINI  = MAX0(MINI,1)
!
      XMAXJ = RJOBS + RIN
      MAXJ  = IFIX(XMAXJ + 0.99)
      MAXJ  = MIN0(MAXJ,JE)
!
      XMINJ = RJOBS - RIN
      MINJ  = IFIX(XMINJ)
      MINJ  = MAX0(MINJ,1)
!
      do I=MINI,MAXI
      do J=MINJ,MAXJ
!
      RX = FLOAT(J) - RJOBS
      RY = FLOAT(I) - RIOBS
      RSQ = RX**2+RY**2
      IF (RSQ.LT.RIS) THEN
!
      WT = (RIS - RSQ)/(RIS + RSQ)
!
!-----SAVE MAX. WEIGHTING FACTOR AND TERRAIN HEIGHT TO CHECK IF GRID
!-----POINT SHOULD BE TREATED AS A LAND OR SEA POINT.
!
      IF (WT.GT.1.0E-5) THEN
         COR(I,J)   = COR(I,J) + WT*ASTA(KK)
         SUM(I,J)   = SUM(I,J) + WT
         NS(I,J)    = NS(I,J) + 1
      ENDIF
      ENDIF
      enddo
      enddo
      ENDDO CRESS
!
!-----NOW APPLY SUMMED WEIGHTS AND WEIGHTED OBSERVATIONS TO DETERMINE
!-----FIELD VALUE AT I,J POINTS
!
      DO I = 1,IE
      DO J = 1,JE
      IF (NS(I,J) .NE. 0) THEN
         COR(I,J) = COR(I,J)/SUM(I,J)
         A2(I,J)  = COR(I,J)
      ELSE
         A2(I,J)  = BADVAL
      ENDIF
      ENDDO 
      ENDDO 
!
!-----MAY WANT TO SMOOTH FINAL FIELD A2 HERE
      RETURN
      END subroutine CRESSAN

      subroutine llxy(xlat,xlon,x,y,bhi,bhr)
!
!     CALCULATE X AND Y GIVEN LATITUDE AND LONGITUDE.
!
      integer bhi(50,20)
      real    bhr(20,20)
 
      conv = 57.29578
      a = 6370.0
      xlatc = BHR(2,1)
      xlonc = BHR(3,1)
      kproj = BHI(7,1)
      psi1  = BHR(5,1)
      psi2  = BHR(6,1)
      ds    = BHR(9,1)/1000.
      xn    = BHR(4,1)
      imax=(BHI(5,1)-1)*BHI(20,1)+1
      jmax=(BHI(6,1)-1)*BHI(20,1)+1
      imapst=(BHR(10,1)-1)*BHI(20,1)+1
      jmapst=(BHR(11,1)-1)*BHI(20,1)+1
      phi1 = 90.0-psi2
      pole = 90.0
 
      if ( xlatc.lt.0.0 ) then
        phi1 = -90.0-psi2
        pole = -pole
      endif
 
      if (kproj.eq.3) then
! MERCATOR PROJECTION
        C2     = A*COS(PSI1)
        XC     = 0.0
        PHICR  = XLATC/CONV
        CELL   = COS(PHICR)/(1.0+SIN(PHICR))
        YC     = - C2*ALOG(CELL)
        IF (XLAT.NE.-90.) THEN
           XLATR = XLAT/CONV
           CELL = COS(XLATR)/(1.0+SIN(XLATR))
           YY = -C2*ALOG(CELL)
           IF (XLONC.LT.0.0) THEN
             IF (XLON.GT.0.0) XLON=XLON-360.
           ELSE
             IF (XLON.LT.0.0) XLON=360.+XLON
           ENDIF
           XX = C2*(XLON-XLONC)/CONV
        ENDIF
 
      ELSE IF (KPROJ.LE.2) THEN
! LAMBERT-COMFORMAL or POLAR-STEREO PROJECTION
      PHIC = ( POLE - XLATC )/CONV
      PHI1 = PHI1/CONV
      XC = 0.0
      YC = -A/XN*SIN(PHI1)*(TAN(PHIC/2.0)/TAN(PHI1/2.0))**XN
!
!     CALCULATE X,Y COORDS. RELATIVE TO POLE
!
      YLON = XLON - XLONC
      IF(YLON.GT.180) YLON = YLON - 360.
      IF(YLON.LT.-180) YLON = YLON + 360.
      FLP = XN*YLON/CONV
      PSX = ( POLE - XLAT )/CONV
      R = -A/XN*SIN(PHI1)*(TAN(PSX/2.0)/TAN(PHI1/2.0))**XN
      IF ( XLATC.LT.0.0 ) THEN
         XX = R*SIN(FLP)
         YY = R*COS(FLP)
      ELSE
         XX = -R*SIN(FLP)
         YY = R*COS(FLP)
      END IF
      END IF
!
!  TRANSFORM (1,1) TO THE ORIGIN
!
      CENTRI = (IMAX + 1.)/2.0
      CENTRJ = (JMAX + 1.)/2.0
      X = ( XX - XC )/DS + CENTRJ  - jmapst + 1
      Y = ( YY - YC )/DS + CENTRI  - imapst + 1
      RETURN
      end subroutine llxy

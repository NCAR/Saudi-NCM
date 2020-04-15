program domain_bias_remove
!
  integer, dimension(50,20) :: bhi
  real, dimension(20,20) :: bhr
  character(len=80), dimension(50,20) :: bhic
  character(len=80), dimension(20,20) :: bhrc
  character(len=120) :: flnm
  integer :: iunit = 10, iout=20
  integer :: flag, i, ihcount
  integer :: ierro=0,ierrm=0

  character(len=14) :: time_stro,abc
  character(len=10) :: atime_new,otime_new
  character(len=4) :: platform
  character(len=1) :: adid
  real, dimension(500) :: xlat,xlon,xlatj,xloni,psfc,slp,h,t,ws,wd,q,rh,u,v
  integer*2,dimension(500):: psfc_qc,slp_qc,t_qc,ws_qc,wd_qc,q_qc
  character(len=14), dimension(500) :: time_str,htime_str
  real :: alat,alon,psfc_obs,slp_obs,h_obs,t_obs,ws_obs,wd_obs,q_obs
  real :: psfc_model,slp_model,h_model,t_model,ws_model,wd_model,q_model
  integer*2 :: psfc_obs_qc,slp_obs_qc,t_obs_qc,ws_obs_qc,wd_obs_qc,q_obs_qc
  real,dimension(500) :: psfc_m,slp_m,h_m,t_m,ws_m,wd_m,q_m,u_m,v_m
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
  endif

  irec=0
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

     if(abs(t_obs-missing) .le. 100.) t_model=missing
     if(abs(q_obs-missing) .le. 100.) q_model=missing
     if(abs(psfc_obs-missing) .le. 100.) psfc_model=missing
     if(abs(slp_obs-missing) .le. 100.) slp_model=missing
     if(abs(ws_obs-missing) .le. 100.) ws_model=missing
     if(abs(wd_obs-missing) .le. 100.) wd_model=missing

   call nearest_hour(time_stro,atime_new)
   if(ihcount==0) otime_new = atime_new

   !print *, time_stro, otime_new, atime_new, alat, alon
   !   stop

  if(atime_new .eq. otime_new) then 
     call llxy(alat,alon,yi,xj,bhi,bhr) 
      xj = xj - 0.5  ! crs point
      yi = yi - 0.5  ! crs point
     if(xj < 1 .or. yi < 1 .or. xj > mix-1 .or. yi > mjx-1) then
      cycle THISHH
     else 
             print("(2a,2f9.2,2f5.1)"),'in Domain: time,lat,lon,xj,yi:',atime_new, alat,alon,xj,yi
      ihcount = ihcount + 1
      time_str(ihcount)=time_stro
      xlat(ihcount)=alat
      xlon(ihcount)=alon
      xlatj(ihcount)=xj
      xloni(ihcount)=yi
      psfc_m(ihcount)=psfc_model
      psfc(ihcount)=psfc_obs
      psfc_qc(ihcount)=psfc_obs_qc
      slp_m(ihcount)=slp_model
      slp(ihcount)=slp_obs
      slp_qc(ihcount)=slp_obs_qc
      h_m(ihcount)=h_model
      h(ihcount)=h_obs
      t_m(ihcount)=t_model
      t(ihcount)=t_obs
      t_qc(ihcount)=t_obs_qc
      q_m(ihcount)=q_model
      q(ihcount)=q_obs
      q_qc(ihcount)=q_obs_qc
      ws_m(ihcount)=ws_model
      ws(ihcount)=ws_obs
      ws_qc(ihcount)=ws_obs_qc
      wd_m(ihcount)=wd_model
      wd(ihcount)=wd_obs
      wd_qc(ihcount)=wd_obs_qc
       
      if(ws_obs-1. > missing .and. wd_obs-1. > missing) then
       u_m(ihcount)=ws_model*cos(wd_model)
       v_m(ihcount)=ws_model*sin(wd_model)
       u(ihcount)=ws_obs*cos(wd_obs)
       v(ihcount)=ws_obs*sin(wd_obs)
      else
       u(ihcount)=missing
       v(ihcount)=missing 
       u_m(ihcount)=missing
       v_m(ihcount)=missing
      endif
     endif
  else
      NSTA=ihcount
        print *, "NSTA",NSTA
        print *, "t",(t(i),i=1,nsta)
        print *, "t_m",(t_m(i),i=1,nsta)
        print *, "q",(q(i),i=1,nsta)
        print *, "u",(u(i),i=1,nsta)
        print *, "u_m",(u_m(i),i=1,nsta)
        print *, "v",(v(i),i=1,nsta)
        print *, "psfc",(psfc(i),i=1,nsta)
        print *, "psfc_m",(psfc_m(i),i=1,nsta)
        print *, "slp",(slp(i),i=1,nsta)

      CALL GETBIAS(t,t_m,tbias,NSTA)
      CALL GETBIAS(q,q_m,qbias,NSTA)
      CALL GETBIAS(u,u_m,ubias,NSTA)
      CALL GETBIAS(v,v_m,vbias,NSTA)
      CALL GETBIAS(psfc,psfc_m,psfcbias,NSTA)
      CALL GETBIAS(slp,slp_m,slpbias,NSTA)
      print ("(a,6f7.1)"), "\nBIAS: t,q,u,v,psfc,slp:",tbias,qbias,ubias,vbias,psfcbias,slpbias
    do i = 1, ihcount 
    print ("(a,i3,8f10.2)"), "i,orig",i,xlat(i),xlon(i),h_m(i),t_m(i),q_m(i),u_m(i),v_m(i),psfc_m(i)
    t2stn=missing
    pmstn=missing
    qmstn=missing
    slpstn=missing
    ustn=missing
    vstn=missing
    if(abs(t_m(i)-missing).gt.100.) t2stn=t_m(i)-tbias
    if(abs(psfc_m(i)-missing).gt.100.) pmstn=psfc_m(i)-psfcbias
    if(abs(q_m(i)-missing).gt.100.) qmstn=q_m(i)-qbias
    terstn=h_m(i)
    if(abs(slp_m(i)-missing).gt.100.) slpstn=slp_m(i)-slpbias
    if(abs(u_m(i)-missing).gt.100.) ustn=u_m(i)-ubias
    if(abs(v_m(i)-missing).gt.100.) vstn=v_m(i)-vbias
    print ("(a,i3,8f10.2)"), "i,crct",i,xlat(i),xlon(i),terstn,t2stn,qmstn,ustn,vstn,pmstn
    print ("(a,i3,8f10.2)"), "  obs ",i,xlat(i),xlon(i),h(i),t(i),q(i),u(i),v(i),psfc(i)
    print *, " "
    !stop

    wsstn=sqrt(ustn**2+vstn**2)
    wdstn=(1.5*pi-atan2(vstn,ustn))*180./pi
    if(wdstn >= 360.) wdstn=wdstn-360.
!
!   temperature correction accounting for elevation difference in model
!   and station, use corrected t to calculate rh. 
!   yliu: In this routine, the t2stn and psfcstn are from the pairs which has been
!   corrected already, so comment them out
!   if(t2stn-1. > missing) then
!      t2stn_orig=t2stn
!      t2stn=t2stn+0.0065*(terstn-h(i))
!   endif
!
!   Psfc adjustment for Terrain difference
!
!   if((pmstn-1. > missing) .and. (t2stn-1. > missing)) then
!      zdiff=terstn-h(i)
!      pmstn=pmstn*exp(zdiff*grav/rgas/(0.5*(t2stn+273.15+t2stn_orig)))
!   endif

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
  
  ihcount=0
  endif
 
END DO THISHH

end program domain_bias_remove

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


  SUBROUTINE GETBIAS(a,am,abias,NSTA1)

     DIMENSION A(NSTA1),AM(NSTA1)
     BADVAL=-8888.0

     asum=0.0
     amsum=0.0
     acnt=0.0
     amcnt=0.0
     do i = 1,NSTA1
      if(abs(A(i)-BADVAL) > 100) then
      asum=asum+A(i)
      acnt=acnt+1.0
      endif 
      if(abs(AM(i)-BADVAL) > 100) then
      amsum=amsum+AM(i)
      amcnt=amcnt+1.0
      endif 
     enddo 

     if(acnt > 1. .and. amcnt > 1.) then
     abias=amsum/amcnt-asum/acnt
     else
     abias=0.0
     endif
     RETURN
  END subroutine GETBIAS

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

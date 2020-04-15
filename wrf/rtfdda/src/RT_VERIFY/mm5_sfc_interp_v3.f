  program mm5_sfc_interp_v3
!
  integer, dimension(50,20) :: bhi
  real, dimension(20,20) :: bhr
  character(len=80), dimension(50,20) :: bhic
  character(len=80), dimension(20,20) :: bhrc
!
  character(len=120) :: flnm
  integer :: iunit = 10, iout=20
  integer :: flag
!
  integer :: ndim
  real :: time
  integer, dimension(4) :: start_index, end_index
  character (len=80) :: exe_name
  character (len=10) :: opt
  character (len=2) :: skip_flag
  character (len= 4) :: staggering
  character (len= 4) :: ordering
  character (len=24) :: start_date
  character (len=24) :: current_date,date_old='0000-00-00_00:00:00.0000'
  character (len= 9) :: name
  character (len=25) :: units
  character (len=46) :: description
!
  character(len=1) :: a_did
  real, allocatable, dimension(:,:,:,:) :: data
  real, allocatable, dimension(:,:,:) :: pp,q,t
  real, allocatable, dimension(:,:) :: pstr
  real, allocatable, dimension(:,:) :: t2,u10,v10,pm,qbot,rh
  real, allocatable, dimension(:,:) :: alat,alon,ter
  real, allocatable, dimension(:) :: sigma

  integer :: end_index1,end_index2,end_index3
  integer :: t_count
  logical :: OUTPUT
  character :: adummy

  character(len=14) :: time_str
  character(len=4) :: platform
  real :: rlat,rlon,psfc_obs,slp_obs,h_obs,t_obs,td_obs,ws_obs,wd_obs
  integer*2 :: psfc_obs_qc,slp_obs_qc,t_obs_qc,td_obs_qc,ws_obs_qc, &
             wd_obs_qc,q_obs_qc
  integer*2 :: iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
               ipmstn,ipsfc_obs,islpstn,islp_obs,iterstn,ih_obs,it2stn,it_obs, &
               iqmstn,iq_obs,iwsstn,iws_obs,iwdstn,iwd_obs

  character (len=120), parameter :: infmt1 = &
            '(a14,f8.3,f9.3,x,a4,2(f9.2,i7),f9.2,4(f9.2,i7))'
  character (len=120), parameter :: infmt2 = &
            '(a14,f8.3,f9.3,x,a4,2(f9.2,i7),f9.2,4(f9.2,i7),x,a8)'


  real, parameter :: badvalue=-9999., missing=-8888.
  integer :: ierr, ier
  real, parameter :: grav=9.81,rgas=287.04
  real :: rhcalc
  integer :: integerize
  integer :: iarg
  character(len=2) :: stage="  "
  character(len=8) :: st_id
  logical :: laddstid = .FALSE.
  pi=acos(-1.)

!
  j=iargc()
  if(j == 0) then
    call getarg(0,exe_name)
    l=index(exe_name,' ')
    print*,'Usage: ',exe_name(1:l-1),' filename [-p for prelim] [-f for fcst] [-add_stid]'
    stop
  endif
!
  call getarg(1,flnm)
  open(iunit,file=flnm,form='unformatted',status='old')
!
  open(31,file='obs.dat')
!
  if(j > 1) then
     do iarg = 2, j
        call getarg(iarg,opt)
        if(index(opt,'-p') < 1 .and. index(opt,'-f') < 1 .and. &
           index(opt,'-add_stid') < 1) then
          print*,'Flag ',opt,' unknown! Should be -p or -f, and/or -add_stid'
          stop
        endif
        if (index(opt,'-p') > 0) stage = '-p'
        if (index(opt,'-f') > 0) stage = '-f'
        if (index(opt,'-add_stid') > 0) laddstid = .TRUE.
     end do
  endif
!
  t_count=0
  irec=0
  read(iunit,iostat=ierr) flag
  do while(ierr == 0)

     if(flag == 0) then
        read(iunit,iostat=ier) bhi, bhr, bhic, bhrc
        if(ier/=0) then
           write(*,'("Error reading big header")')
           call abort()
        endif
!
!
        p0=bhr(2,5)
        ptop=bhr(2,2)
        p0mb=p0/100.
        ts0=bhr(3,5)
        tlp=bhr(4,5)
        xlonc=bhr(3,1)        !! in degrees
        xn=bhr(4,1)
        domain_id=bhi(13,1)
        if(t_count == 0) then
          write(a_did,'(i1)') domain_id
          if (laddstid) then
             open(71,file='pairs_domain'//a_did,access='direct',recl=64)
          else
             open(71,file='pairs_domain'//a_did,access='direct',recl=56)
          end if
        endif
!
     elseif(flag == 1) then
        OUTPUT=.FALSE.
        READ (iunit,iostat=ier) ndim, start_index, end_index, time, &
             staggering, ordering, current_date, name, units, description
        if(ier/=0) then
           write(*,'("Error reading subheader")')
           call abort()
        endif
!
        if(index(stage,'-p') > 0) then      !! When dealing with prilim only,
          if(t_count < 3) OUTPUT=.TRUE.   !! only first 3 output times used.
        elseif(index(stage,'-f') > 0) then  !! When dealing with fcst only,
          if(t_count >= 3) OUTPUT=.TRUE.  !! skip first 3 output times.
        else                              !! When dealing with FINAL,
          OUTPUT=.TRUE.                   !! all output times are used.
        endif
!
        if(time == 0.) OUTPUT=.FALSE.     !! added May 20, 2003
!
        read(current_date,'(14x,i2)') iminute
        if(iminute /= 0) OUTPUT=.FALSE.
!
        if (ndim == 1) then
           allocate(data(end_index(1), 1, 1, 1))
        elseif (ndim == 2) then
           allocate(data(end_index(1), end_index(2), 1, 1))
        elseif (ndim == 3) then
           allocate(data(end_index(1), end_index(2), end_index(3), 1))
        endif
!
        read(iunit) data
!
          if(name(1:4) == 'U10 ') then
            allocate(u10(end_index(1),end_index(2)))
            do i=1,end_index(1)
            do j=1,end_index(2)
               u10(i,j)=data(i,j,1,1)
            enddo
            enddo
          elseif(name(1:4) == 'V10 ') then
            allocate(v10(end_index(1),end_index(2)))
            do i=1,end_index(1)
            do j=1,end_index(2)
               v10(i,j)=data(i,j,1,1)
            enddo
            enddo
          elseif(name(1:4) == 'T2  ') then
            allocate(t2(end_index(1),end_index(2)))
            do i=1,end_index(1)
            do j=1,end_index(2)
               t2(i,j)=data(i,j,1,1)
            enddo
            enddo
          elseif(name(1:4) == 'T   ') then
            allocate(t(end_index(1),end_index(2),end_index(3)))
            do k=1,end_index(3)
            do i=1,end_index(1)
            do j=1,end_index(2)
               t(i,j,k)=data(i,j,k,1)
            enddo
            enddo
            enddo
            end_index3=end_index(3)
          elseif(name(1:4) == 'Q   ') then
            allocate(q(end_index(1),end_index(2),end_index(3)))
            do k=1,end_index(3)
            do i=1,end_index(1)
            do j=1,end_index(2)
               q(i,j,k)=data(i,j,k,1)
            enddo
            enddo
            enddo
          elseif(name(1:4) == 'SIGM' ) then
            allocate(sigma(end_index(1)))
            do k=1,end_index(1)
               sigma(k)=data(k,1,1,1)
            enddo
          elseif(name(1:4) == 'PP  ') then
            allocate(pp(end_index(1),end_index(2),end_index(3)))
            do k=1,end_index(3)
            do i=1,end_index(1)
            do j=1,end_index(2)
               pp(i,j,k)=data(i,j,k,1)
            enddo
            enddo
            enddo
          elseif(name(1:8) == 'PSTARCRS') then
            allocate(pstr(end_index(1),end_index(2)))
            do i=1,end_index(1)
            do j=1,end_index(2)
               pstr(i,j)=data(i,j,1,1)
            enddo
            enddo
            end_index1=end_index(1)
            end_index2=end_index(2)
          elseif(name(1:8) == 'LATITCRS') then
            allocate(alat(end_index(1),end_index(2)))
            do i=1,end_index(1)
            do j=1,end_index(2)
               alat(i,j)=data(i,j,1,1)
            enddo
            enddo
          elseif(name(1:8) == 'LONGICRS') then
            allocate(alon(end_index(1),end_index(2)))
            do i=1,end_index(1)
            do j=1,end_index(2)
               alon(i,j)=data(i,j,1,1)
            enddo
            enddo
          elseif(name(1:8) == 'TERRAIN ') then
            allocate(ter(end_index(1),end_index(2)))
            do i=1,end_index(1)
            do j=1,end_index(2)
               ter(i,j)=data(i,j,1,1)
            enddo
            enddo
          endif
 
!       endif
!
        deallocate(data)
!
     elseif(flag == 2) then
!
        if(OUTPUT) then
          allocate(pm(end_index1,end_index2))
          allocate(qbot(end_index1,end_index2))
          do i=1,end_index1
          do j=1,end_index2
             pm(i,j)=(pstr(i,j)*sigma(end_index3)+ptop+pp(i,j,end_index3))*0.01
             qbot(i,j)=q(i,j,end_index3)
          enddo
          enddo

          call d2c(u10,end_index1,end_index2)
          call d2c(v10,end_index1,end_index2)
!
          ix=end_index1
          jx=end_index2

          ierror=0

loop_obs: DO WHILE (ierror == 0)
!
             if (laddstid) then
             read(31,infmt2,iostat=ierror) time_str,rlat,rlon,platform, &
                 psfc_obs,psfc_obs_qc,slp_obs,slp_obs_qc,h_obs,t_obs, &
                 t_obs_qc,td_obs,td_obs_qc,ws_obs,ws_obs_qc,wd_obs,wd_obs_qc, &
                 st_id
             else
             read(31,infmt1,iostat=ierror) time_str,rlat,rlon,platform, &
                 psfc_obs,psfc_obs_qc,slp_obs,slp_obs_qc,h_obs,t_obs, &
                 t_obs_qc,td_obs,td_obs_qc,ws_obs,ws_obs_qc,wd_obs,wd_obs_qc
             end if

             if(ierror /= 0) exit loop_obs

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

             call right_time(time_str,current_date,itime_flag,iexceed)

             if(iexceed == 1) then
               backspace(31)
               exit loop_obs
             endif

             if(itime_flag == 0) cycle loop_obs

!            obtain q obs from t obs and td obs:

             call qfromttd(t_obs,t_obs_qc,td_obs,td_obs_qc, &
                           psfc_obs,psfc_obs_qc,q_obs,q_obs_qc)
             if(q_obs > missing) q_obs=q_obs*1000.    !! in g/kg

             call bi_linear(t2,val,ix,jx,rlat,rlon,alat,alon,1, &
                  dx,dy,i0,j0,iflag,jflag)

             if(iflag == 0) cycle loop_obs

             t2stn=val
             if(t_obs > missing) t_obs=t_obs-273.15
             if(td_obs > missing) td_obs=td_obs-273.15

             pmstn=bi_linear_func(pm,ix,jx,dx,dy,i0,j0)

             u10stn=bi_linear_func(u10,ix,jx,dx,dy,i0,j0)
             v10stn=bi_linear_func(v10,ix,jx,dx,dy,i0,j0)
             qmstn=bi_linear_func(qbot,ix,jx,dx,dy,i0,j0)
             qmstn=qmstn*1000.   !! g/kg
             terstn=bi_linear_func(ter,ix,jx,dx,dy,i0,j0)
             slpstn=slpcal(pmstn,t2stn,terstn,qmstn)
!
!
!         temperature correction accounting for elevation difference in model
!         and station, use corrected t to calculate rh
!
             if(t2stn > missing) then
               t2stn_orig=t2stn
               t2stn=t2stn+0.0065*(terstn-h_obs)
               t2stn=t2stn-273.15
             endif
!
!
!
             if((pmstn > missing) .and. (t2stn > missing)) then
               zdiff=terstn-h_obs
               pmstn=pmstn*exp(zdiff*grav/rgas/(0.5*(t2stn+273.15+t2stn_orig)))
             endif
!
!         rotate model wind to geographical coordinate, calculate speed, dir
!
             u=u10stn
             v=v10stn
             if((u > missing) .and. (v > missing)) then
               arg=(rlon-xlonc)*xn*pi/180.
               u10stn=u*cos(arg)+v*sin(arg)
               v10stn=-u*sin(arg)+v*cos(arg)
               wsstn=sqrt(u**2+v**2)
               wdstn=(1.5*pi-atan2(v10stn,u10stn))*180./pi
               if(wdstn >= 360.) wdstn=wdstn-360.
             else
               wsstn=missing
               wdstn=missing
             endif
!
!
!            if(current_date /= date_old) then
               read(time_str,'(i4,4i2)') iyear,imonth,iday,ihour,imin
!              write(6,*) iyear,imonth,iday,ihour,imin
!              date_old=current_date
!            endif
!
             imonthday=imonth*100+iday
             ihourmin=ihour*100+imin

             ilat=nint(rlat*100)
             ilon=nint(rlon*100)

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
!            write(6,*) iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
!              platform, &
!              ipmstn,ipsfc_obs,psfc_obs_qc, &
!              islpstn,islp_obs,slp_obs_qc, &
!              iterstn,ih_obs, &
!              it2stn,it_obs,t_obs_qc, &
!              iqmstn,iq_obs,q_obs_qc, &
!              iwsstn,iws_obs,ws_obs_qc, &
!              iwdstn,iwd_obs,wd_obs_qc
             if (laddstid) then
             write(71,rec=irec) iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
               platform, &
               ipmstn,ipsfc_obs,psfc_obs_qc, &
               islpstn,islp_obs,slp_obs_qc, &
               iterstn,ih_obs, &
               it2stn,it_obs,t_obs_qc, &
               iqmstn,iq_obs,q_obs_qc, &
               iwsstn,iws_obs,ws_obs_qc, &
               iwdstn,iwd_obs,wd_obs_qc, &
               st_id
             else
             write(71,rec=irec) iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
               platform, &
               ipmstn,ipsfc_obs,psfc_obs_qc, &
               islpstn,islp_obs,slp_obs_qc, &
               iterstn,ih_obs, &
               it2stn,it_obs,t_obs_qc, &
               iqmstn,iq_obs,q_obs_qc, &
               iwsstn,iws_obs,ws_obs_qc, &
               iwdstn,iwd_obs,wd_obs_qc
             end if
!
          END DO loop_obs

          deallocate(pm)
          deallocate(qbot)

        endif
!
        deallocate(t)
        deallocate(q)
        deallocate(t2)
        deallocate(u10)
        deallocate(v10)
        deallocate(pstr)
        deallocate(pp)
        deallocate(sigma)
        deallocate(alat)
        deallocate(alon)
        deallocate(ter)
!
        t_count=t_count+1
!
     endif 
!
     read(iunit,iostat=ierr) flag
  enddo
!
  end program mm5_sfc_interp_v3

!
!
!
  subroutine d2c(arr,mm5i,mm5j)
  real, dimension(mm5i,mm5j) :: arr,arr_temp
!
  mmim = mm5i-1
  mmjm = mm5j-1
!
  do i=1,mm5i
  do j=1,mm5j
     arr_temp(i,j)=arr(i,j)
  enddo
  enddo
!
  do i=1,mmim
  do j=1,mmjm
   arr(i,j)=0.25*(arr_temp(i,j)+arr_temp(i+1,j)+arr_temp(i,j+1)+ & 
            arr_temp(i+1,j+1))
  enddo
  enddo
!
  return
  end subroutine d2c
!
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
  read(current_date,'(i4,4(x,i2))') year_m,month_m,day_m,hour_m,min_m

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
      if(day_o > days(month_o)) then
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

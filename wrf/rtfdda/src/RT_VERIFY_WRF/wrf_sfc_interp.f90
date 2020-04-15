  program wrf_sfc_interp
!
  use WRF_ARRAYS
  use WRF_TOOLS
  use NETCDF
  use WRF_CONSTANTS

  implicit none

  character (len=192) :: flnm, exe_name
  character (len=5) :: opt
  character (len=19) :: start_date
  integer :: i,j,k,it,ncid,varid,iargc,istatus,ierror,itime_flag,iexceed, &
             itime,imonth,iday,ihour,imin,irec
  real :: rpi,q_obs,t2stn,pmstn,u10stn,v10stn,qmstn,terstn,slpstn, &
          slpcal,t2stn_orig,zdiff,xn,wsstn,wdstn,arg,u,v
  integer :: julday,map_proj,idomain
  character :: a_did

  character(len=14) :: time_str
  character(len=4) :: platform
  real :: rlat,rlon,psfc_obs,slp_obs,h_obs,t_obs,td_obs,ws_obs,wd_obs,rh_obs
  integer*2 :: psfc_obs_qc,slp_obs_qc,t_obs_qc,td_obs_qc,ws_obs_qc, &
             wd_obs_qc,q_obs_qc,rh_obs_qc
  integer*2 :: iyear,imonthday,ihourmin,ilat,ilon,domain_id, &
               ipmstn,ipsfc_obs,islpstn,islp_obs,iterstn,ih_obs,it2stn,it_obs, &
               iqmstn,iq_obs,iwsstn,iws_obs,iwdstn,iwd_obs

  character (len=120), parameter :: infmt1 = &
            '(a14,f8.3,f9.3,x,a4,2(f9.2,i7),f9.2,5(f9.2,i7))'
  character (len=120), parameter :: infmt2 = &
            '(a14,f8.3,f9.3,x,a4,2(f9.2,i7),f9.2,5(f9.2,i7),x,a8)'

  real, parameter :: badvalue=-9999., rmissing=-8888.
  integer :: ierr, ier
  real, parameter :: grav=9.81,rgas=287.04
  real, parameter :: sclht=rgas*256./grav
  integer :: integerize

  real, pointer, dimension(:,:) :: znu,znw,znfac
  real, pointer, dimension(:,:,:,:) :: t,p,pb,ph,phb,q, &
                                       prs,um,vm,escale,ght,zagl
  real, pointer, dimension(:,:,:) :: pm,t2,q2,u10,v10
  real, pointer, dimension(:,:,:) :: ter,alat,alon,pbl,hfx
  character, pointer, dimension(:,:) :: times
  character(len=40), allocatable, dimension(:) :: dtstring

  integer :: imax,jmax,kmax,tmax,icsize,jcsize

  character(len=2) :: proj
  real :: bi_linear_func

  real :: x,y,dx,standlon,truelat1,truelat2,rhcalc,tv
  real*8 :: lat_r8,lon_r8,x_r8,y_r8,dx_r8,reflat,reflon,standlon_r8, &
            truelat1_r8,truelat2_r8,confac

  character(len=80) :: carg
  character(len=8) :: st_id
  integer :: ist_id
  logical :: laddstid = .false.

  rpi=acos(-1.)

!
  j=iargc()
  if(j == 0) then
    call getarg(0,exe_name)
    print*,'Usage: ',trim(exe_name),' filename [-add_stid]'
    stop
  endif
!
  call getarg(1,flnm)
  istatus=nf90_open(trim(flnm),0,ncid)
  if(istatus /= nf90_noerr) then
    print*,'error opening NetCDF file: ',trim(flnm)
    stop
  end if
!
  if (j == 2) then
     call getarg(2,carg)
     if (index(carg,'-add_stid') > 0) then
        laddstid = .true.
     else
        print*,'Unrecognized argument: ',trim(carg)
        stop
     end if
  end if
!
  open(31,file='obs.dat')
!
! if(j > 1) then
!    call getarg(2,opt)
!    if((opt /= '-p') .and. (opt /= '-f')) then
!      print*,'Flag ',opt,' unknown! Should be -p or -f, or no flag'
!      stop
!    endif
! endif
!
! reading global attributes
!
  istatus=nf90_get_att(ncid,nf90_global,'START_DATE',start_date)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: START_DATE'
  print*,'START DATE = ',start_date

  istatus=nf90_get_att(ncid,nf90_global,'DX',dx)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: DX'
  print*,'DX = ',dx
  dx_r8=dble(dx/1000.)

  istatus=nf90_get_att(ncid,nf90_global,'STAND_LON',standlon)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: STAND_LON'
  print*,'STAND_LON = ',standlon
  standlon_r8=dble(standlon)

  istatus=nf90_get_att(ncid,nf90_global,'TRUELAT1',truelat1)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: TRUELAT1'
  print*,'TRUELAT1 = ',truelat1
  truelat1_r8=dble(truelat1)

  istatus=nf90_get_att(ncid,nf90_global,'TRUELAT2',truelat2)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: TRUELAT2'
  print*,'TRUELAT2 = ',truelat2
  truelat2_r8=dble(truelat2)

  istatus=nf90_get_att(ncid,nf90_global,'GRID_ID',idomain)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: GRID_ID'
  print*,'GRID_ID = ',idomain
  domain_id=idomain

  istatus=nf90_get_att(ncid,nf90_global,'MAP_PROJ',map_proj)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: MAP_PROJ'
  print*,'MAP_PROJ = ',map_proj

  if (map_proj == 0 ) proj = "CE"
  if (map_proj == 1 ) proj = "LC"
  if (map_proj == 2 ) proj = "ST"
  if (map_proj == 3 ) proj = "ME"

  if(proj == 'LC') then
    call lccone(truelat1_r8,truelat2_r8,idint(dsign(c1, truelat2_r8)),confac)
    xn=real(confac)
  else
    confac=1.d0
    xn=1.
  end if

  write(a_did,'(i1)') idomain
  if (laddstid) then
     open(71,file='pairs_domain'//a_did,access='direct',recl=64)
  else
     open(71,file='pairs_domain'//a_did,access='direct',recl=56)
  end if
!
!
! reading data arrays
!
  istatus=nf90_inq_varid(ncid,'U10',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: U10'
  call get_array2d(ncid,varid,'U10',u10)

  istatus=nf90_inq_varid(ncid,'V10',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: V10'
  call get_array2d(ncid,varid,'V10',v10)

  istatus=nf90_inq_varid(ncid,'T2',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: T2'
  call get_array2d(ncid,varid,'T2',t2)

  istatus=nf90_inq_varid(ncid,'Q2',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: Q2'
  call get_array2d(ncid,varid,'Q2',q2)

  istatus=nf90_inq_varid(ncid,'T',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: T'
  call get_array3d(ncid,varid,'T',t)

  istatus=nf90_inq_varid(ncid,'QVAPOR',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: Q'
  call get_array3d(ncid,varid,'QVAPOR',q)

  istatus=nf90_inq_varid(ncid,'P',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: P'
  call get_array3d(ncid,varid,'P',p) ! perturbation pressure in Pa

  istatus=nf90_inq_varid(ncid,'PB',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: PB'
  call get_array3d(ncid,varid,'PB',pb) ! base state pressure in Pa

  istatus=nf90_inq_varid(ncid,'XLAT',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: XLAT'
  call get_array2d(ncid,varid,'XLAT',alat)
  reflat=dble(alat(1,1,1))

  istatus=nf90_inq_varid(ncid,'XLONG',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: XLONG'
  call get_array2d(ncid,varid,'XLONG',alon)
  reflon=dble(alon(1,1,1))

  istatus=nf90_inq_varid(ncid,'PH',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: PH'
  call get_array3d(ncid,varid,'PH',ph) ! perturbation geopotential in m^2/s^2

  istatus=nf90_inq_varid(ncid,'PHB',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: PHB'
  call get_array3d(ncid,varid,'PHB',phb) ! base state geopotential in m^2/s^2

  istatus=nf90_inq_varid(ncid,'HGT',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: HGT'
  call get_array2d(ncid,varid,'HGT',ter) ! terrain height in m

  istatus=nf90_inq_varid(ncid,'ZNU',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: ZNU'
  call get_array1d(ncid,varid,'ZNU',znu)

  istatus=nf90_inq_varid(ncid,'ZNW',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: ZNW'
  call get_array1d(ncid,varid,'ZNW',znw)

  istatus=nf90_inq_varid(ncid,'Times',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: Times'
  call get_carray(ncid,varid,'Times',times)

  icsize=size(times,1)
  jcsize=size(times,2)

  allocate(dtstring(jcsize))

  do j=1,jcsize
  do i=1,icsize
     dtstring(j)(i:i)=times(i,j)
  end do
  end do

  imax=size(t,1)
  jmax=size(t,2)
  kmax=size(t,3)
  tmax=size(t,4)

  allocate(prs(imax,jmax,kmax,tmax))
  allocate(pm(imax,jmax,tmax))
  allocate(escale(imax,jmax,kmax+1,tmax))
  allocate(ght(imax,jmax,kmax,tmax))
  allocate(zagl(imax,jmax,kmax,tmax))

  do it=1,tmax
  do k=1,kmax+1
  do j=1,jmax
  do i=1,imax
     escale(i,j,k,it)=exp(-(phb(i,j,k,it)+ph(i,j,k,it))/(grav*sclht))
  end do
  end do
  end do
  end do

  allocate(znfac(kmax,tmax))

  do it=1,tmax
  do k=1,kmax
     znfac(k,it)=(znu(k,it)-znw(k+1,it))/(znw(k,it)-znw(k+1,it))
  end do
  end do

  do it=1,tmax
  do k=1,kmax
  do j=1,jmax
  do i=1,imax
     ght(i,j,k,it)=znfac(k,it)*escale(i,j,k,it)+ &
                   (1.-znfac(k,it))*escale(i,j,k+1,it)
     ght(i,j,k,it)=-sclht*log(ght(i,j,k,it))
     zagl(i,j,k,it)=ght(i,j,k,it)-ter(i,j,it)
  end do
  end do
  end do
  end do

  prs=p+pb  ! 3-D array addition in Pa

  do it=1,tmax
  do j=1,jmax
  do i=1,imax
     tv=t2(i,j,it)*(1.+0.61*q2(i,j,it)/(1.+q2(i,j,it)))
     pm(i,j,it)=(prs(i,j,1,it)*0.01)*exp(zagl(i,j,1,it)*grav/rgas/tv)
  end do
  end do
  end do

! call d2c(u10,imax,jmax)
! call d2c(v10,imax,jmax)
!
  ierror=0

  irec=0

loop_obs: &
  DO WHILE (ierror == 0)
!
     if (laddstid) then
        read(31,infmt2,iostat=ierror) time_str,rlat,rlon,platform, &
            psfc_obs,psfc_obs_qc,slp_obs,slp_obs_qc,h_obs,t_obs, &
            t_obs_qc,td_obs,td_obs_qc,ws_obs,ws_obs_qc,wd_obs,wd_obs_qc, &
            rh_obs,rh_obs_qc,st_id
     else
        read(31,infmt1,iostat=ierror) time_str,rlat,rlon,platform, &
            psfc_obs,psfc_obs_qc,slp_obs,slp_obs_qc,h_obs,t_obs, &
            t_obs_qc,td_obs,td_obs_qc,ws_obs,ws_obs_qc,wd_obs,wd_obs_qc, &
            rh_obs,rh_obs_qc
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

     lat_r8=dble(rlat)
     lon_r8=dble(rlon)

     call right_time(time_str,icsize,jcsize,dtstring,50,itime_flag,iexceed,itime)

     if(iexceed == 1) exit loop_obs

     if(itime_flag == 0) cycle loop_obs

!    obtain q obs from t obs and td obs:

     if ( (t_obs > 0.) .and. (td_obs > 0.) .and. (psfc_obs > 0.) ) then
        call qfromttd(t_obs,t_obs_qc,td_obs,td_obs_qc, &
                      psfc_obs,psfc_obs_qc,q_obs,q_obs_qc,rmissing)
        if(q_obs > rmissing) q_obs=q_obs*1000.    !! in g/kg
     elseif ( rh_obs > 0. .and. (t_obs > 0.) .and. (psfc_obs > 0.)) then
        call qfromrh(rh_obs,rh_obs_qc,t_obs,t_obs_qc,psfc_obs,psfc_obs_qc, &
                     q_obs,q_obs_qc)
        if(q_obs > rmissing) q_obs=q_obs*1000.    !! in g/kg
     else
        q_obs = rmissing
        q_obs_qc = -8888
     endif

     call lltoxy_generic(lat_r8,lon_r8,x_r8,y_r8,proj,dx_r8,reflat,reflon, &
          dble(1.),dble(1.),standlon_r8,truelat1_r8,truelat2_r8)

     x=real(x_r8)
     y=real(y_r8)

     if((x < 2.) .or. (x > float(imax-1)) .or. &
        (y < 2.) .or. (y > float(jmax-1))) cycle loop_obs

     t2stn=bi_linear_func(t2,imax,jmax,tmax,itime,x,y)
     if(t_obs > rmissing) t_obs=t_obs-273.15
     if(td_obs > rmissing) td_obs=td_obs-273.15

     pmstn=bi_linear_func(pm,imax,jmax,tmax,itime,x,y)

     u10stn=bi_linear_func(u10,imax,jmax,tmax,itime,x+0.5,y)
     v10stn=bi_linear_func(v10,imax,jmax,tmax,itime,x,y+0.5)
     qmstn=bi_linear_func(q2,imax,jmax,tmax,itime,x,y)
     qmstn=qmstn*1000.   !! g/kg
     terstn=bi_linear_func(ter,imax,jmax,tmax,itime,x,y)
     slpstn=slpcal(pmstn,t2stn,terstn,qmstn)
!
!
!         temperature correction accounting for elevation difference in model
!         and station, use corrected t to calculate rh
!
     if(t2stn > rmissing) then
       t2stn_orig=t2stn
       t2stn=t2stn+0.0065*(terstn-h_obs)
       t2stn=t2stn-273.15
     endif
!
!
!
     if((pmstn > rmissing) .and. (t2stn > rmissing)) then
       zdiff=terstn-h_obs
       pmstn=pmstn*exp(zdiff*grav/rgas/(0.5*(t2stn+273.15+t2stn_orig)))
     endif
!
!         rotate model wind to geographical coordinate, calculate speed, dir
!
     u=u10stn
     v=v10stn
     if((u > rmissing) .and. (v > rmissing)) then
       arg=(rlon-standlon)*xn*rpi/180.
       u10stn=u*cos(arg)+v*sin(arg)
       v10stn=-u*sin(arg)+v*cos(arg)
       wsstn=sqrt(u**2+v**2)
       wdstn=(1.5*rpi-atan2(v10stn,u10stn))*180./rpi
       if(wdstn >= 360.) wdstn=wdstn-360.
     else
       wsstn=rmissing
       wdstn=rmissing
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
       ipsfc_obs=rmissing
     endif

     islpstn=nint(slpstn*10.)                 !! same comment as
     if(slp_obs > 0.) then                    !! above
       islp_obs=nint(slp_obs*10.)
     else
       islp_obs=rmissing
     endif

     iterstn=nint(terstn)
     ih_obs=nint(h_obs)
       
     it2stn=nint(t2stn*100.)
     it_obs=integerize(t_obs,rmissing)

     iqmstn=nint(qmstn*100)
     iq_obs=integerize(q_obs,rmissing)

     iwsstn=nint(wsstn*100.)
     iws_obs=integerize(ws_obs,rmissing)

     iwdstn=nint(wdstn)
     iwd_obs=nint(wd_obs)

     if (psfc_obs_qc < 0 .or. psfc_obs_qc > 10) psfc_obs_qc = 0
     if (slp_obs_qc < 0 .or. slp_obs_qc > 10) slp_obs_qc = 0
     if (t_obs_qc < 0 .or. t_obs_qc > 10) t_obs_qc = 0
     if (q_obs_qc < 0 .or. q_obs_qc > 10) q_obs_qc = 0
     if (ws_obs_qc < 0 .or. ws_obs_qc > 10) ws_obs_qc = 0
     if (wd_obs_qc < 0 .or. wd_obs_qc > 10) wd_obs_qc = 0

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

!       Correcting IAF station number that has xxxxx0 id, and make it just
!       xxxxx

        read(st_id,*,iostat=ierr) ist_id

        if (ierr == 0) then
           if ((mod(ist_id,10) == 0) .and. (ist_id >= 100000) .and. & 
               (ist_id < 1000000)) then
              print *,'Adjusting station number : ',st_id
              ist_id = ist_id/10
              st_id = '        '
              write(st_id(1:5),'(i5)') ist_id
              print *,'New station number : ',st_id
           end if
        end if
!
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
     endif
!
  END DO loop_obs

!
  end program wrf_sfc_interp

!
!
!
  subroutine d2c(arr,imax,jmax)
  integer :: imax,jmax,i,j
  real, dimension(imax+1,jmax+1) :: arr,arr_temp
!
  do j=1,jmax
  do i=1,imax
     arr_temp(i,j)=arr(i,j)
  enddo
  enddo
!
  do j=1,jmax
  do i=1,imax
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
  real function rhcalc(t,q,pm)
!
     real :: es,re,ee,t,q,pm

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
  function bi_linear_func(array,imax,jmax,tmax,itime,x,y)
!
  integer :: imax,jmax,tmax,i0,j0,itime
  real :: bi_linear_func
  real :: x,y,dx,dy,dxm,dym
  real, dimension(imax,jmax,tmax) :: array
!
  i0=int(x)
  j0=int(y)

  dx=x-i0
  dy=y-j0

  dxm=1.0-dx
  dym=1.0-dy

  bi_linear_func=dxm*(dym*array(i0,j0,itime)+dy*array(i0,j0+1,itime))+ &
                 dx *(dym*array(i0+1,j0,itime)+dy*array(i0+1,j0+1,itime))

  return
  end
!
!
!
  function integerize(val,rmissing)
!
  real :: val,rmissing
  integer :: integerize
!
  if(val > rmissing) then
    integerize=nint(val*100)
  else
    integerize=rmissing
  endif
!
  return
  end function integerize
!
!
!
  real function slpcal(psfc,t2,ter,q)
!
  real :: psfc,t2,ter,q,qs,tv
  real, parameter :: rgas=287.05, gamma=0.0065, g=9.8

  qs=q*0.001              !! Note: Do not use q=q*0.001, because in so doing
                          !! you overwrite the q unit in the main program!
  tv=t2*(1.+0.61*qs/(1.+qs))
! slpcal=psfc*(1.+gamma/tv*ter)**(g/rgas/gamma)
  slpcal=psfc*exp(ter*g/rgas/tv)

  return
  end function slpcal

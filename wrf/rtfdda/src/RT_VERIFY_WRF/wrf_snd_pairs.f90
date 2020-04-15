  program wrf_snd_pairs

  use WRF_ARRAYS
  use WRF_TOOLS
  use NETCDF
  use WRF_CONSTANTS
  use WRITE_SND_BIN

  implicit none

  INTERFACE

    subroutine wrf_pseudo_snd(kx,it,x,y,array,x_ps)

    real :: x,y
    integer :: kx,it
    real, pointer, dimension(:,:,:,:) :: array
    real, allocatable, dimension(:) :: x_ps
    real :: dx,dy,dxm,dym
    integer :: i,j
    real :: rmissing = -8888.

    end subroutine wrf_pseudo_snd 

    subroutine wrf_pseudo_2d(it,x,y,array,x_point)

    integer :: it
    real :: x,y
    real, pointer, dimension(:,:,:) :: array
    real :: dx,dy,dxm,dym,x_point
    integer :: i,j

    end subroutine wrf_pseudo_2d

  END INTERFACE

  character(len=192) :: exe_name,flnm,opt
  character(len=2) :: proj
  integer :: varid

  integer, parameter :: ks=40
  Real,Parameter     ::badvalue=-8888.
  integer :: iprs_obs,it_obs,itd_obs,irh_obs,iws_obs,iwd_obs,iqc,ihgt_obs
  real :: qv
  integer :: itime_flag,iexceed,itime,icnt2
  real :: x,y,arg
  real :: elevm, elev_diff
  real, dimension(ks) :: um,vm,tm,qm,rhm,wsm,wdm,ghtm
  real, dimension(ks) :: t_int,u_int,v_int,q_int,rh_int,ws_int,wd_int,hgt_int
  integer, dimension(ks) :: t_int_qc,u_int_qc,v_int_qc,q_int_qc,rh_int_qc, &
                            ws_int_qc,wd_int_qc,hgt_int_qc
  real, pointer, dimension(:,:,:,:) :: u,v,theta,t,q,p,pb,prs, &
                                       ph,phb,escale,ght,zagl
  real, pointer, dimension(:,:,:) :: alat,alon,ter
  character, pointer, dimension(:,:) :: times
  character(len=40), allocatable, dimension(:) :: dtstring
  real, pointer, dimension(:,:) :: znu,znw,znfac
  real, allocatable, dimension(:) :: t_ps,q_ps,u_ps,v_ps,prs_ps,ght_ps
!!! wuyh added 20140221
  real, allocatable, dimension(:) :: mod_hadj,mod_tadj,mod_padj
  Real :: elevo
!!! wuyh added 20140221
  integer :: ihr
  integer, parameter :: snd_unit=61,wrf_unit=71,out_unit=81

  integer :: AllocateStatus,flag

  real, allocatable, dimension(:) :: prs_obs,t_obs,td_obs,q_obs,u_obs,v_obs, &
                                     ws_obs,wd_obs,hgt_obs,rh_obs
  integer, allocatable, dimension(:) :: prs_qc,t_qc,td_qc,ws_qc,wd_qc,q_qc, &
                                        u_qc,v_qc,hgt_qc,rh_qc
  character (len=8) :: st_id
  integer :: ist_id
  character (len=19) :: start_date
  integer :: elev
  integer :: i,j,k,it,iarg,iargc,istatus,ncid,idomain,map_proj,n_levels
  integer :: ierr,irec
  integer :: imax,jmax,kmax,tmax,icsize,jcsize
  character :: adomain_id
  real :: truelat1,truelat2,dx,standlon,xn,xlat,xlon,ul,vl,pr
  real*8 :: lat_r8,lon_r8,x_r8,y_r8,dx_r8,reflat,reflon,standlon_r8, &
            truelat1_r8,truelat2_r8,confac

  character (len=10) :: atime_new,wrf_time='0000000000',atime_new1
  integer :: t_count
  logical :: OUTPUT,found,early

  character (len=14) :: atime
  integer, parameter :: imissing=-8888
  real, parameter :: rmissing=-8888.
  real :: rhcalc

  real, parameter :: grav=9.81,rgas=287.04
  real, parameter :: sclht=rgas*256./grav
  real :: rpi

  logical :: lheight = .false., ladd_hr = .false.

  rpi=acos(-1.)

  j=iargc()
  if(j == 0) then
    call getarg(0,exe_name)
    call usage(exe_name)
    stop
  endif

  if (j > 1) then
     do iarg = 2,j
        call getarg(iarg,opt)
        if(index(opt,'-height') > 0) then
          lheight = .true.
        else if(index(opt,'-add_hr') > 0) then
          ladd_hr = .true.
        else
          print*,'Unknown option: ',trim(opt)
          call getarg(0,exe_name)
          call usage(exe_name)
          stop
        end if
     end do
  end if 
!
  call getarg(1,flnm)
  istatus=nf90_open(trim(flnm),0,ncid)
  if(istatus /= nf90_noerr) then
    print*,'error opening NetCDF file: ',trim(flnm)
    stop
  else
    print*,'processing NetCDF file: ',trim(flnm)
  end if
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
  write(adomain_id,'(i1)') idomain

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
!
!
! reading data arrays
!
  istatus=nf90_inq_varid(ncid,'T',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: T'
  call get_array3d(ncid,varid,'T',theta)

  istatus=nf90_inq_varid(ncid,'U',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: U'
  call get_array3d(ncid,varid,'U',u)

  istatus=nf90_inq_varid(ncid,'V',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: V'
  call get_array3d(ncid,varid,'V',v)

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

  imax=size(theta,1)
  jmax=size(theta,2)
  kmax=size(theta,3)
  tmax=size(theta,4)

  allocate(prs(imax,jmax,kmax,tmax))
  prs=pb+p  !! array operation

  theta=theta+300.!!pot. temp. = perturbation pot. temp. + base state pot. temp.
  allocate(t(imax,jmax,kmax,tmax))
  t=theta*(prs/100000)**(2./7.) !! array operation

  allocate(escale(imax,jmax,kmax+1,tmax))
  allocate(ght(imax,jmax,kmax,tmax))
! allocate(zagl(imax,jmax,kmax,tmax))

  do it=1,tmax
  do k=1,kmax+1
  do j=1,jmax
  do i=1,imax
    !escale(i,j,k,it)=exp(-(phb(i,j,k,it)+ph(i,j,k,it))/(grav*sclht))
     escale(i,j,k,it)=(phb(i,j,k,it)+ph(i,j,k,it))/grav
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
!    ght(i,j,k,it)=znfac(k,it)*escale(i,j,k,it)+ &
!                  (1.-znfac(k,it))*escale(i,j,k+1,it)
     ght(i,j,k,it)=znfac(k,it)*escale(i,j,k,it)+ &
                   (1.-znfac(k,it))*escale(i,j,k+1,it)
!    ght(i,j,k,it)=-sclht*log(ght(i,j,k,it))
!    zagl(i,j,k,it)=ght(i,j,k,it)-ter(i,j,it)
  end do
  end do
  end do
  end do

! ght = (ph + phb)/grav

  ierr=0
  irec=0         !! counter for direct access record number
  icnt2=0

SND:  do while(ierr == 0)

     read(snd_unit,'(a14,x,a8,f9.3,f9.3,i10,i4)',IOSTAT=ierr) atime,st_id,xlat,xlon,elev,n_levels
     print*,atime,st_id,xlat,xlon
     if(ierr /= 0) exit SND
     call nearest_hour(atime,atime_new)

     if(ladd_hr) then
       read(atime_new,'(8x,i2)') ihr
       if (ihr == 11 .or. ihr == 23) then
          call adjust_hour(atime_new,1,atime_new1)
          print*,'adjust sounding time from ',atime_new,' to ',atime_new1
          atime_new = atime_new1
          atime = atime_new//'0000'
          print*,'adjusted time is ',atime
       end if
     end if

     lat_r8=dble(xlat)
     lon_r8=dble(xlon)

     if(allocated(prs_obs)) deallocate(prs_obs)
     if(allocated(prs_qc)) deallocate(prs_qc)
     if(allocated(t_obs)) deallocate(t_obs)
     if(allocated(t_qc)) deallocate(t_qc)
     if(allocated(td_obs)) deallocate(td_obs)
     if(allocated(td_qc)) deallocate(td_qc)
     if(allocated(ws_obs)) deallocate(ws_obs)
     if(allocated(ws_qc)) deallocate(ws_qc)
     if(allocated(wd_obs)) deallocate(wd_obs)
     if(allocated(wd_qc)) deallocate(wd_qc)
     if(allocated(q_obs)) deallocate(q_obs)
     if(allocated(q_qc)) deallocate(q_qc)
     if(allocated(rh_obs)) deallocate(rh_obs)
     if(allocated(rh_qc)) deallocate(rh_qc)
     if(allocated(u_obs)) deallocate(u_obs)
     if(allocated(v_obs)) deallocate(v_obs)
     if(allocated(u_qc)) deallocate(u_qc)
     if(allocated(v_qc)) deallocate(v_qc)
     if(allocated(hgt_obs)) deallocate(hgt_obs)
     if(allocated(hgt_qc)) deallocate(hgt_qc)

     allocate(prs_obs(n_levels), stat=AllocateStatus)
     allocate(prs_qc(n_levels),  stat=AllocateStatus)
     allocate(t_obs(n_levels),   stat=AllocateStatus)
     allocate(t_qc(n_levels),    stat=AllocateStatus)
     allocate(td_obs(n_levels),  stat=AllocateStatus)
     allocate(td_qc(n_levels),   stat=AllocateStatus)
     allocate(ws_obs(n_levels),  stat=AllocateStatus)
     allocate(ws_qc(n_levels),   stat=AllocateStatus)
     allocate(wd_obs(n_levels),  stat=AllocateStatus)
     allocate(wd_qc(n_levels),   stat=AllocateStatus)
     allocate(q_obs(n_levels),   stat=AllocateStatus)
     allocate(q_qc(n_levels),    stat=AllocateStatus)
     allocate(rh_obs(n_levels),   stat=AllocateStatus)
     allocate(rh_qc(n_levels),    stat=AllocateStatus)
     allocate(u_obs(n_levels),   stat=AllocateStatus)
     allocate(v_obs(n_levels),   stat=AllocateStatus)
     allocate(u_qc(n_levels),    stat=AllocateStatus)
     allocate(v_qc(n_levels),    stat=AllocateStatus)
     allocate(hgt_obs(n_levels), stat=AllocateStatus)
     allocate(hgt_qc(n_levels),  stat=AllocateStatus)

     do k=1,n_levels
        if (lheight) then
        read(snd_unit,*) iprs_obs,prs_qc(k),it_obs,t_qc(k),itd_obs,td_qc(k), &
                   iws_obs,ws_qc(k),iwd_obs,wd_qc(k),irh_obs,rh_qc(k), &
                   ihgt_obs,hgt_qc(k)
        else
        read(snd_unit,*) iprs_obs,prs_qc(k),it_obs,t_qc(k),itd_obs,td_qc(k), &
                   iws_obs,ws_qc(k),iwd_obs,wd_qc(k),irh_obs,rh_qc(k)
        endif

        if(iprs_obs > imissing) then
          prs_obs(k)=iprs_obs*0.01      ! now in mb
        else
          prs_obs(k)=rmissing
        endif

        if(it_obs > imissing) then
          t_obs(k)=it_obs*0.01          ! in K
        else
          t_obs(k)=rmissing
        endif

        if(itd_obs > imissing) then
          td_obs(k)=itd_obs*0.01        ! in K
        else
          td_obs(k)=rmissing
        endif

        if(iws_obs > imissing) then
          ws_obs(k)=iws_obs*0.01           ! in m/s
        else
          ws_obs(k)=rmissing
        endif

        wd_obs(k)=iwd_obs*1.             ! in deg

        rh_obs(k)=irh_obs * 1.0

        if ((t_obs(k) > 0.) .and. (td_obs(k) > 0.) .and. (prs_obs(k) > 0.)) then
           call qfromttd(t_obs(k),t_qc(k),td_obs(k),td_qc(k), &
                         prs_obs(k),prs_qc(k),qv,iqc,rmissing)
           q_obs(k)=qv
           q_qc(k)=iqc
        elseif ((rh_obs(k) > 0.) .and. (t_obs(k) > 0.) .and. (prs_obs(k) > 0.)) then
           call qfromrh(rh_obs(k),rh_qc(k),t_obs(k),t_qc(k), &
                        prs_obs(k),prs_qc(k),qv,iqc)
           q_obs(k)=qv
           q_qc(k)=iqc
        else
           q_obs(k)=rmissing
           q_qc(k)=imissing
        endif

        if(ws_obs(k) > rmissing .and. wd_obs(k) > rmissing) then
          u_obs(k)=-ws_obs(k)*sin(wd_obs(k)*rpi/180.)
          v_obs(k)=-ws_obs(k)*cos(wd_obs(k)*rpi/180.)
        else
          u_obs(k)=rmissing
          v_obs(k)=rmissing
        endif

        u_qc(k)=min(ws_qc(k),wd_qc(k))
        v_qc(k)=u_qc(k)

        if(ihgt_obs > imissing) then
          hgt_obs(k)=ihgt_obs*1.
        else
          hgt_obs(k)=rmissing
        endif

     enddo
!
!    Find the matching WRF output time
!
     call right_time(atime,icsize,jcsize,dtstring,30,itime_flag,iexceed,itime)
     print*,'iexceed = ',iexceed

     if(iexceed == 1) exit SND

     if(itime_flag == 0) cycle SND

     call lltoxy_generic(lat_r8,lon_r8,x_r8,y_r8,proj,dx_r8,reflat,reflon, &
          dble(1.),dble(1.),standlon_r8,truelat1_r8,truelat2_r8)

     x=real(x_r8)
     y=real(y_r8)

     print*,'lat lon x y = ',xlat,xlon,x,y

     if((x < 2.) .or. (x > float(imax-1)) .or. &
        (y < 2.) .or. (y > float(jmax-1))) cycle SND

     if(allocated(t_ps)) deallocate(t_ps)
     if(allocated(q_ps)) deallocate(q_ps)
     if(allocated(u_ps)) deallocate(u_ps)
     if(allocated(v_ps)) deallocate(v_ps)
     if(allocated(prs_ps)) deallocate(prs_ps)
     if(allocated(ght_ps)) deallocate(ght_Ps)

     allocate(t_ps(kmax))
     allocate(q_ps(kmax))
     allocate(u_ps(kmax))
     allocate(v_ps(kmax))
     allocate(prs_ps(kmax))
     allocate(ght_ps(kmax))

     call wrf_pseudo_2d(itime,x,y,ter,elevm)

     call wrf_pseudo_snd(kmax,itime,x,y,t,t_ps)
     call wrf_pseudo_snd(kmax,itime,x,y,q,q_ps)
     call wrf_pseudo_snd(kmax,itime,x+0.5,y,u,u_ps)
     call wrf_pseudo_snd(kmax,itime,x,y+0.5,v,v_ps)
     call wrf_pseudo_snd(kmax,itime,x,y,prs,prs_ps)
     call wrf_pseudo_snd(kmax,itime,x,y,ght,ght_ps)

!!! wuyh added 20140221
     allocate(mod_hadj(kmax))
     allocate(mod_tadj(kmax))
     allocate(mod_padj(kmax))
     elevo = float(elev)
     write(*,*) " before call reverse_adjust_height"
     write(*,*) " modp,prs_ps =",prs_ps(1),prs_obs(1)*100.
     Call reverse_adjust_height(kmax,n_levels,badvalue,elevm,elevo,   &
          ght_ps,t_ps,prs_ps,prs_obs,hgt_obs,mod_hadj,mod_tadj,mod_padj)

 !    Do i=1,kmax
 !     write(22,1022)i,ght_ps(i),mod_hadj(i),prs_ps(i),mod_padj(i),t_ps(i),mod_tadj(i)
 !    Enddo
1022 Format(I2,6(1x,F10.2))

          ght_ps = mod_hadj
          t_ps   = mod_tadj
          prs_ps = mod_padj

     write(*,*) " after call reverse_adjust_height"
!!! wuyh added 20140221

     call eta2p(t_ps,tm,prs_ps,2,kmax,ks,rmissing)
!     Do i=1,kmax
!      write(23,1023)i,prs_ps(i),t_ps(i)
!     Enddo
1023 Format(I2,2(1x,F10.2))

     call eta2p(u_ps,um,prs_ps,1,kmax,ks,rmissing)
     call eta2p(v_ps,vm,prs_ps,1,kmax,ks,rmissing)
     call eta2p(q_ps,qm,prs_ps,1,kmax,ks,rmissing)
     call eta2p(ght_ps,ghtm,prs_ps,2,kmax,ks,rmissing)

  !   Do i=1,kmax
  !    write(23,1022)i,ght_ps(i),mod_hadj(i),prs_ps(i),mod_padj(i),t_ps(i),mod_tadj(i)
  !   Enddo

     call vertical_interp(t_obs,t_int,t_qc,t_int_qc,prs_obs,ks, &
                            n_levels,rmissing,2)
     call vertical_interp(u_obs,u_int,u_qc,u_int_qc,prs_obs,ks, &
                            n_levels,rmissing,1)
     call vertical_interp(v_obs,v_int,v_qc,v_int_qc,prs_obs,ks, &
                            n_levels,rmissing,1)
     call vertical_interp(q_obs,q_int,q_qc,q_int_qc,prs_obs,ks, &
                            n_levels,rmissing,1)
     call vertical_interp(hgt_obs,hgt_int,hgt_qc,hgt_int_qc, &
                            prs_obs,ks,n_levels,rmissing,2)

     arg=(xlon-standlon)*xn*rpi/180.

     do k=1,ks
       if(um(k) > rmissing .and. vm(k) > rmissing) then
         ul=um(k)*cos(arg)+vm(k)*sin(arg)
         vl=-um(k)*sin(arg)+vm(k)*cos(arg)
         wsm(k)=sqrt(um(k)**2+vm(k)**2)
         wdm(k)=(1.5*rpi-atan2(vl,ul))*180./rpi
         if(wdm(k) > 360.) wdm(k)=wdm(k)-360.
       else
         wsm(k)=rmissing
         wdm(k)=rmissing
       endif

       if(u_int(k) > rmissing .and. v_int(k) > rmissing) then
         ws_int(k)=sqrt(u_int(k)**2+v_int(k)**2)
         wd_int(k)=(1.5*rpi-atan2(v_int(k),u_int(k)))*180./rpi
         if(wd_int(k) >  360.) wd_int(k)=wd_int(k)-360.
       else
         ws_int(k)=rmissing
         wd_int(k)=rmissing
       endif
       ws_int_qc(k)=min(u_int_qc(k),v_int_qc(k))
       wd_int_qc(k)=ws_int_qc(k)

!      Calculate RH for both model and Obs

       if(k == 1) then
         pr=1010.
       else
         pr=1000.-(k-2)*25.
       endif

       if(tm(k) > rmissing .and. qm(k) > rmissing) then
!!! wuyh modified 20140214
!         rhm(k)=rhcalc(tm(k),qm(k),pr)
         call r2rhw(qm(k),tm(k),pr,rhm(k),0)
!!! wuyh modified 20140214
       else
         rhm(k)=rmissing
       endif

       if(t_int(k) > rmissing .and. q_int(k) > rmissing) then
!!! wuyh modified 20140214
!         rh_int(k)=rhcalc(t_int(k),q_int(k),pr)
         call r2rhw(q_int(k),t_int(k),pr,rh_int(k),0)
!!! wuyh modified 20140214
       else
         rh_int(k)=rmissing
       endif

       rh_int_qc(k)=min(t_int_qc(k),q_int_qc(k))

     end do

     if(lheight) then
     do k=1,ks
        print*,'ghtm(',k,') = ',ghtm(k),' hgt_int(',k,') = ',hgt_int(k)
     end do
     end if
        

     icnt2=icnt2+1

!    if(icnt2 == 1) open(out_unit,file='snd_pairs_domain'//adomain_id, &
!                          status='REPLACE')
     if(icnt2 == 1) then
       if(lheight) then
         open(out_unit,file='snd_pairs_domain'//adomain_id, &
                       status="REPLACE", &
                       access='direct', &
                       recl=76)
       else
         open(out_unit,file='snd_pairs_domain'//adomain_id, &
                       status="REPLACE", &
                       access='direct', &
                       recl=64)
       end if
     end if

     where (t_int_qc < 0 .or. t_int_qc > 10)
           t_int_qc = 0
     end where

     where (q_int_qc < 0 .or. q_int_qc > 10)
           q_int_qc = 0
     end where

     where (rh_int_qc < 0 .or. rh_int_qc > 10)
           rh_int_qc = 0
     end where

     where (ws_int_qc < 0 .or. ws_int_qc > 10)
           ws_int_qc = 0
     end where

     where (wd_int_qc < 0 .or. wd_int_qc > 10)
           wd_int_qc = 0
     end where

     where (hgt_int_qc < 0 .or. hgt_int_qc > 10)
           hgt_int_qc = 0
     end where

!    Correcting IAF station number that has xxxxx0 id, and make it just
!    xxxxx

     read(st_id,*,iostat=ierr) ist_id

     if (ierr == 0) then
        if ((mod(ist_id,10) == 0) .and. (ist_id >= 100000) .and. &
            (ist_id < 1000000)) then
           print *,'Adjusting station number : ',st_id
           ist_id = ist_id/10
           st_id = '        '
           write(st_id(1:5),'(i5)') ist_id
           print *,'New station number : ', st_id
        end if
     end if
!

!      write(out_unit,'(a10,f7.2,f8.2,i2)') atime_new, xlat, xlon, idomain

     irec=irec+1
     write(out_unit,rec=irec) atime_new, st_id, xlat, xlon, nint(elevm), elev, idomain
!    call write_snd_pairs(out_unit,ks,tm,t_int,t_int_qc,qm,q_int,q_int_qc, &
!                           rhm,rh_int,rh_int_qc,wsm,ws_int,ws_int_qc, &
!                           wdm,wd_int,wd_int_qc,rmissing)

     if(lheight) then
       call write_snd_pairs_bin(out_unit,ks,tm,t_int,t_int_qc, &
                                qm,q_int,q_int_qc, &
                                rhm,rh_int,rh_int_qc, &
                                wsm,ws_int,ws_int_qc, &
                                wdm,wd_int,wd_int_qc, &
                                ghtm,hgt_int,hgt_int_qc,rmissing,irec)
     else
       call write_snd_pairs_bin(out_unit,ks,tm,t_int,t_int_qc, &
                                qm,q_int,q_int_qc, &
                                rhm,rh_int,rh_int_qc, &
                                wsm,ws_int,ws_int_qc, &
                                wdm,wd_int,wd_int_qc,rmissing,irec)
     end if

     !deallocate(t_ps)      !! PGF does not need to deallocate these arrays
     !deallocate(q_ps)      !! because they have been passed on to subroutine
     !deallocate(u_ps)      !! sig2p and dealloacated there after exiting the
     !deallocate(v_ps)      !! subroutine
     !deallocate(prs_ps)

     deallocate(prs_obs)
     deallocate(prs_qc)
     deallocate(t_obs)
     deallocate(t_qc)
     deallocate(td_obs)
     deallocate(td_qc)
     deallocate(ws_obs)
     deallocate(ws_qc)
     deallocate(wd_obs)
     deallocate(wd_qc)
     deallocate(q_obs)
     deallocate(q_qc)
     deallocate(rh_obs)
     deallocate(rh_qc)
     deallocate(u_obs)
     deallocate(u_qc)
     deallocate(v_obs)
     deallocate(v_qc)
!!! wuyh added 20140221
     deallocate(mod_hadj)
     deallocate(mod_tadj)
     deallocate(mod_padj)
!!! wuyh added 20140221

     if(lheight) deallocate(hgt_obs)
     if(lheight) deallocate(hgt_qc)

  enddo SND 

  end program wrf_snd_pairs
!
!
!
  real function rhcalc(t,q,pm)
!
!   To ensure this function works properly, temperature "t" has to be in K,
!   whereas mixing ration "q" has to be in kg/kg (or g/g), and pressure "pm"
!   in mb.
! 
    real :: t,q,pm,es,re,ee

    es=10**(-2937.4/t-4.9283*log10(t)+23.5518)  !! in mb
    re=q/(1.-q)
    ee=pm*re/(0.622+re)
    rhcalc=ee/es*100.
    if(rhcalc < 0.) rhcalc=0.
    if(rhcalc > 100.) rhcalc=100.

  return
  end function rhcalc
!
!
!
  subroutine usage(exe_name)

  character(len=*) :: exe_name

  print*,'Usage: ',trim(exe_name),' filename [-height]'
  print*,'  filename : WRF output file to be processed'
  print*,'  -height: optional, turns on the pairing of height field'

  return
  end subroutine usage
!
!
!
  subroutine adjust_hour(atime,iadd_hr,atime_new)

  implicit none

  character (len=10) :: atime
  character (len=10) :: atime_new
  character (len=4) :: ayear
  character (len=3) :: amonth,aday,ahour
  integer :: iadd_hr

  integer, dimension(12) :: days=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  integer :: year,month,day,hour,minute,sec

  read(atime,'(i4,3i2)') year,month,day,hour

  if(mod(year,400) == 0) then
    days(2)=29
  else
    if((mod(year,4) == 0) .and. (mod(year,100) /= 0)) days(2)=29
  endif

  hour=hour+iadd_hr
  do while (hour >= 24)
    hour=hour-24
    day=day+1;
    do while (day > days(month))
      day=day-days(month)
      month=month+1
      do while (month > 12)
        month=month-12
        year=year+1
      end do
    end do
  end do

  do while (hour < 0)
    hour=hour+24
    day=day-1
    do while (day < 1)
       if(month == 1) then
         day=day+days(12)
         month=12
       else
         day=day+days(month-1)
         month=month-1
       end if

       do while (month < 1)
          month=month+12
          year=year-1
       end do
    end do
  end do

  write(ayear,'(i4)') year

  month=100+month
  write(amonth,'(i3)') month

  day=100+day
  write(aday,'(i3)') day

  hour=100+hour
  write(ahour,'(i3)') hour

  atime_new=ayear//amonth(2:3)//aday(2:3)//ahour(2:3)

  return

  end subroutine adjust_hour

!*************************************************************************
  Subroutine r2rhw(r,t,p,rh,iice)
!*************************************************************************

! convert mixing ratio to rh
! if iice=1, use saturation with respect to ice
! rh is 0-100.
! r is g/g
! t is K
! p is mb
      Implicit None
      REAL, intent(out)  :: rh
      REAL, intent(in)  :: t
      REAL, intent(in)  :: p
      REAL, intent(in)  :: r
      INTEGER, intent(in)  :: iice

!***  DECLARATIONS FOR IMPLICIT NONE                                    
      real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
      real esat, rsat

      eps=0.62197
      e0=6.1078
      eslcon1=17.2693882
      eslcon2=35.86
      esicon1=21.8745584
      esicon2=7.66
      t0=260.
      if(r .gt.-0.01)then
!       if(iice.eq.1.and.t.le.t0)then
!        esat=e0*exp(esicon1*(t-273.16)/(t-esicon2))
!       else
!        esat=e0*exp(eslcon1*(t-273.16)/(t-eslcon2))
!       endif
!       rsat=eps*esat/(p-esat)
      ESAT = 6.112 * EXP (17.67*(T-273.15)/(T-29.60))
      RSAT = 0.622 * ESAT /(P-ESAT)

       rh=r*100./rsat
       rh=MIN(rh,100.)
      else
       rh=-8888.
      endif
      return
  END SUBROUTINE r2rhw

  Subroutine reverse_adjust_height(kzmax,nlevs,bad_val,mod_terr,obs_elev,  &
                 mod_ht,mod_t,mod_p,obs_p,obs_ht,mod_htadj,mod_tadj,mod_padj)

      Implicit None

! parameters
      Real,Parameter::alpha = 10.,max_hag = 1000., min_ht = -500.,lapse_rate=0.0065
! input
      Real::bad_val
      Integer,Intent(IN):: kzmax,nlevs
      Real,Intent(IN)::mod_terr,obs_elev
      Real,dimension(kzmax),Intent(IN):: mod_ht,mod_t,mod_p
      Real,dimension(nlevs)::obs_p,obs_ht
! out
      Real,dimension(kzmax),Intent(out):: mod_htadj,mod_tadj,mod_padj
! local
      Integer:: i,kmax
      Real:: d_ht,hmax,mod_pp

      d_ht = obs_elev - mod_terr
      hmax = alpha * abs(d_ht)
      hmax = min(hmax,abs(d_ht)+max_hag)

      kmax = 0
      Do i=1,kzmax
       if(mod_ht(i)+d_ht .ge. min_ht  .and. mod_ht(i) .gt. (obs_elev + hmax )) then
        kmax = i
        go to 100
       endif
      Enddo
      write(*,*) "error in height adjustment"
      stop 658

100   Continue

      mod_htadj = mod_ht
      mod_tadj = mod_t
      mod_padj = mod_p

      write(*,*) "in reverse_adj_height kmax=",kmax
      Do i=1,kmax
        if(mod_ht(i)+d_ht .ge. min_ht .and. mod_ht(i) .lt. ( obs_elev+0.5*hmax-d_ht))then 
          mod_htadj(i) = mod_ht(i) + d_ht 
          mod_tadj(i)  = mod_t(i) - d_ht*lapse_rate
        else
          mod_htadj(i) = (2.*d_ht*(hmax +obs_elev)+hmax*mod_ht(i))/(hmax + 2.*d_ht)
          mod_tadj(i)  = mod_t(i) - (mod_htadj(i)-mod_ht(i))*lapse_rate
        endif
        write(*,*) " modht,modhtadj=",mod_ht(i),mod_htadj(i)
        mod_pp = mod_p(i)
        Call  reverse_adjust_prs(kzmax,nlevs,d_ht,bad_val,mod_pp,mod_p,mod_htadj(i), &
                mod_ht,obs_ht,obs_p,mod_padj(i))
        Call check_adjust_prs(kzmax,bad_val,mod_padj,mod_htadj,mod_tadj)
      Enddo

     write(21,*)kmax
     Do i=1,kzmax
      write(21,1021)i,mod_ht(i),mod_htadj(i),mod_p(i),mod_padj(i),mod_t(i),mod_tadj(i)
     Enddo
1021 Format(I2,6(1x,F10.2))
! adjust model pressure

      return

  End subroutine reverse_adjust_height

  Subroutine reverse_adjust_prs(kzmax,nlevs,d_ht,bad_val,mod_pp,mod_p,mod_htadj, &
                mod_ht,obs_ht,obs_p,mod_padj)

      Implicit None
! in
      Integer::kzmax,nlevs
      Real::bad_val,d_ht
      Real,Dimension(kzmax)::mod_ht,mod_p
      Real,Dimension(nlevs)::obs_p,obs_ht
      Real::mod_htadj,mod_pp
! out
      Real::mod_padj
! local
      Integer::i
      Real::wgt

      If(d_ht .gt. 0.)Then          !obs_h > mod_h
       Do i=2,kzmax 
         if (mod_htadj .lt. mod_ht(i))then
          wgt = (mod_htadj - mod_ht(i-1))/(mod_ht(i)-mod_ht(i-1))  
          mod_padj = mod_p(i)*wgt + mod_p(i-1)*(1.-wgt) 
          go to 200
         endif  
       Enddo
      Else                          !obs_h < mod_h
       Do i=2,nlevs
         if (mod_htadj .gt. obs_ht(i-1) .and. mod_htadj .le. obs_ht(i))then
          wgt = (mod_htadj - obs_ht(i-1))/(obs_ht(i)-obs_ht(i-1))  
          mod_padj = (obs_p(i)*wgt + obs_p(i-1)*(1.-wgt))*100.
          mod_padj = max(mod_pp,mod_padj) 
          go to 200
         endif  
       Enddo
       mod_padj = bad_val
      Endif
200   Continue
      Return
  End Subroutine reverse_adjust_prs


  Subroutine check_adjust_prs(kzmax,bad_val,mod_padj,mod_hadj,mod_tadj)

      Implicit None
      Real,Parameter::lapse = 0.0065, xisu=0.1902
      Integer::kzmax
      Real::bad_val
      Real,Dimension(kzmax)::mod_hadj,mod_padj,mod_tadj
! local
      Integer::i,k,klow,kupp
      Real::wgt

      Do i=1,kzmax
      IF(ABS(mod_padj(i)-bad_val) .le. 0.1)then
        
        If(i.EQ.1) then
          Do k=2,kzmax
           if (mod_padj(k) .gt. 0.) then
            call h2P(mod_hadj(k),mod_padj(k),mod_tadj(k),mod_hadj(i),mod_padj(i)) 
            GO TO 100                    
           endif
          Enddo 
        Else  
          kupp = -1
          Do k=i+1,kzmax
           if(mod_padj(k) .gt. 0 ) then
             wgt = (mod_hadj(i)-mod_hadj(i-1))/(mod_hadj(k)-mod_hadj(i-1))
             mod_padj(i) = mod_padj(i-1)*(1.-wgt) + mod_padj(k)*wgt
             go to 100
           endif    
          Enddo
          call h2P(mod_hadj(i-1),mod_padj(i-1),mod_tadj(i-1),mod_hadj(i),mod_padj(i))
        Endif
      ENDIF
100   Continue
      Enddo
  End Subroutine check_adjust_prs

  Subroutine h2P(h0,p0,t0,h1,p1)
   Implicit None
   Real,Parameter::lapse = 0.0065, xisu=0.1902
   Real::p0,t0,h0   !a known level obs
   Real::p1,h1
   p1 = p0*EXP(LOG(1.+lapse*(h0-h1)/t0)/xisu)
   Return
  End Subroutine h2P

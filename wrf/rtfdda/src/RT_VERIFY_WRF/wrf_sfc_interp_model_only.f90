  program wrf_sfc_interp_model_only
!
  use WRF_ARRAYS
  use WRF_TOOLS
  use NETCDF
  use WRF_CONSTANTS
  use DATETIME_MODULE

  implicit none

  character (len=192) :: sarg, flnm, exe_name, samsncdfile, outfile
  character (len=5) :: opt
  character (len=19) :: start_date
  integer :: i,j,k,it,ncid,varid,iargc,istatus,ierror,itime_flag,iexceed, &
             itime,imonth,iday,ihour,imin,irec,ncidc,nstations,ista
  real :: rpi,t2stn,pmstn,u10stn,v10stn,qmstn,terstn,slpstn, &
          slpcal,t2stn_orig,zdiff,xn,wsstn,wdstn,arg,u,v
  integer :: julday,map_proj,idomain
  character :: a_did

  character(len=12) :: time_start,time_model
  integer :: itime_start_short, itime_model_short
  integer :: isecs_start, isecs_model, isecs_lead, lead_hour

  real, parameter :: badvalue=-9999., rmissing=-8888.
  integer :: ierr, ier
  real, parameter :: grav=9.81,rgas=287.04
  real, parameter :: sclht=rgas*256./grav

! For SAMS NetCDF
  character, pointer, dimension(:,:) :: platform
  real, pointer, dimension(:) :: samslat,samslon,samsalt

! For WRF NetCDF
  real, pointer, dimension(:,:) :: znu,znw,znfac
  real, pointer, dimension(:,:,:,:) :: t,p,pb,ph,phb,q, &
                                       prs,um,vm,escale,ght,zagl
  real, pointer, dimension(:,:,:) :: pm,t2,q2,u10,v10
  real, pointer, dimension(:,:,:) :: ter,alat,alon,pbl,hfx
  character, pointer, dimension(:,:) :: times
  character(len=40), allocatable, dimension(:) :: dtstring
  character(len=20), allocatable, dimension(:) :: station_name

  integer :: imax,jmax,kmax,tmax,icsize,jcsize

  character(len=2) :: proj
  real :: bi_linear_func

  real :: x,y,dx,standlon,truelat1,truelat2,tv
  real*8 :: lat_r8,lon_r8,x_r8,y_r8,dx_r8,reflat,reflon,standlon_r8, &
            truelat1_r8,truelat2_r8,confac

  character(len=80) :: carg
  character(len=8) :: st_id
  integer :: ist_id
  logical :: fopt=.false., copt=.false., oopt=.false.
  logical :: file_exist

  rpi=acos(-1.)

!
  j=iargc()
  if(j == 0) then
    call usage()
    stop
  endif
!
  i=1
  do while(i <= j)
     call getarg(i,sarg)
     if(index(sarg,'-f') > 0) then
       i=i+1
       fopt=.true.
       call getarg(i,flnm)
     else if(index(sarg,'-c') >0 ) then
       i=i+1
       copt=.true.
       call getarg(i,samsncdfile)
     else if(index(sarg,'-o') >0 ) then
       i=i+1
       oopt=.true.
       call getarg(i,outfile)
     else
       print*,'Unrecognized flag: ',trim(sarg)
       call usage()
       stop
     end if
     i=i+1
  end do

  if((.not. fopt) .or. (.not. copt) .or. (.not. oopt)) then
    call usage()
    stop
  end if

  print*,'flnm = ',trim(flnm),' samsncdfile = ',trim(samsncdfile),' outfile = ',trim(outfile)

! Use SAMS NetCDF file to retrieve station configuration

  istatus=nf90_open(trim(samsncdfile),0,ncidc)
  if(istatus /= nf90_noerr) then
    print*,'error opening SAMS NetCDF file: ',trim(samsncdfile)
    stop
  end if

  istatus=nf90_inq_varid(ncidc,'platform',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: platform'
  call get_carray(ncidc,varid,'platform',platform)

  istatus=nf90_inq_varid(ncidc,'lat',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: lat'
  call get_array0d(ncidc,varid,'lat',samslat)

  istatus=nf90_inq_varid(ncidc,'lon',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: lon'
  call get_array0d(ncidc,varid,'lat',samslon)

  istatus=nf90_inq_varid(ncidc,'alt',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: alt'
  call get_array0d(ncidc,varid,'alt',samsalt)

  icsize=size(platform,1)
  jcsize=size(platform,2)

  allocate(station_name(jcsize))

  do j=1,jcsize
  do i=1,icsize
     station_name(j)(i:i)=platform(i,j)
  end do
  end do

  nstations=jcsize   ! total number of SAMS stations

! Below for WRF NetCDF file

  istatus=nf90_open(trim(flnm),0,ncid)
  if(istatus /= nf90_noerr) then
    print*,'error opening NetCDF file: ',trim(flnm)
    stop
  end if
!

! reading global attributes
!
  istatus=nf90_get_att(ncid,nf90_global,'START_DATE',start_date)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: START_DATE'
  print*,'START DATE = ',start_date

  istatus=nf90_get_att(ncid,nf90_global,'DX',dx)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: DX'
 !print*,'DX = ',dx
  dx_r8=dble(dx/1000.)

  istatus=nf90_get_att(ncid,nf90_global,'STAND_LON',standlon)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: STAND_LON'
 !print*,'STAND_LON = ',standlon
  standlon_r8=dble(standlon)

  istatus=nf90_get_att(ncid,nf90_global,'TRUELAT1',truelat1)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: TRUELAT1'
 !print*,'TRUELAT1 = ',truelat1
  truelat1_r8=dble(truelat1)

  istatus=nf90_get_att(ncid,nf90_global,'TRUELAT2',truelat2)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: TRUELAT2'
 !print*,'TRUELAT2 = ',truelat2
  truelat2_r8=dble(truelat2)

  istatus=nf90_get_att(ncid,nf90_global,'GRID_ID',idomain)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: GRID_ID'
 !print*,'GRID_ID = ',idomain

  istatus=nf90_get_att(ncid,nf90_global,'MAP_PROJ',map_proj)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: MAP_PROJ'
 !print*,'MAP_PROJ = ',map_proj

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

  time_start=start_date(1:4)//start_date(6:7)//start_date(9:10)// &
             start_date(12:13)//start_date(15:16)
  read(time_start,'(i10)') itime_start_short  ! (yyyymmddhh)
  isecs_start=date2secs(itime_start_short)

! open output file

  inquire(file=trim(outfile),exist=file_exist)

  if(file_exist) then
    open(301,file=trim(outfile),access='append')
  else
    open(301,file=trim(outfile),status='new')
    write(301,'(a)') 'YYYYMMDDHHMN, FCST_HR, GRID,      STTN, PSFC_mb, PMSL_mb,     T_C,  Q_g/kg, SPD_m/s,  DIR'
  end if

  do it = 1,tmax
     time_model=dtstring(it)(1:4)//dtstring(it)(6:7)// &
                dtstring(it)(9:10)//dtstring(it)(12:13)//dtstring(it)(15:16)

     read(time_model,'(i10)') itime_model_short ! (yyyymmddhh)
     isecs_model=date2secs(itime_model_short)

     isecs_lead=isecs_model - isecs_start

     lead_hour=isecs_lead/3600
     
  do ista=1,nstations

     lat_r8=dble(samslat(ista))
     lon_r8=dble(samslon(ista))

     call lltoxy_generic(lat_r8,lon_r8,x_r8,y_r8,proj,dx_r8,reflat,reflon, &
          dble(1.),dble(1.),standlon_r8,truelat1_r8,truelat2_r8)

     x=real(x_r8)
     y=real(y_r8)

     if((x < 2.) .or. (x > float(imax-1)) .or. &
        (y < 2.) .or. (y > float(jmax-1))) cycle


     t2stn=bi_linear_func(t2,imax,jmax,tmax,it,x,y)

     pmstn=bi_linear_func(pm,imax,jmax,tmax,it,x,y)

     u10stn=bi_linear_func(u10,imax,jmax,tmax,it,x+0.5,y)
     v10stn=bi_linear_func(v10,imax,jmax,tmax,it,x,y+0.5)
     qmstn=bi_linear_func(q2,imax,jmax,tmax,it,x,y)
     qmstn=qmstn*1000.   !! g/kg
     terstn=bi_linear_func(ter,imax,jmax,tmax,it,x,y)
     slpstn=slpcal(pmstn,t2stn,terstn,qmstn)
!
!
!         temperature correction accounting for elevation difference in model
!         and station, use corrected t to calculate rh
!
     if(t2stn > rmissing) then
       t2stn_orig=t2stn
       t2stn=t2stn+0.0065*(terstn-samsalt(ista))
       t2stn=t2stn-273.15
     endif
!
!
!
     if((pmstn > rmissing) .and. (t2stn > rmissing)) then
       zdiff=terstn-samsalt(ista)
       pmstn=pmstn*exp(zdiff*grav/rgas/(0.5*(t2stn+273.15+t2stn_orig)))
     endif
!
!         rotate model wind to geographical coordinate, calculate speed, dir
!
     u=u10stn
     v=v10stn
     if((u > rmissing) .and. (v > rmissing)) then
       arg=(samslon(ista)-standlon)*xn*rpi/180.
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
!    iwdstn=nint(wdstn)

     write(301,"(a12,',',i8,',',i5,',',a10,',',5(f8.2,','),i5)") &
          time_model,lead_hour,idomain, &
          trim(station_name(ista)),pmstn,slpstn,t2stn,qmstn,wsstn,nint(wdstn)

  end do ! ista loop
  end do ! it loop

  deallocate(station_name)
  close(301)

!
  end program wrf_sfc_interp_model_only

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
  function slpcal(psfc,t2,ter,q)
!
  real :: psfc,t2,ter,q,slpcal,qs,tv
  real, parameter :: rgas=287.05, gamma=0.0065, g=9.8

  qs=q*0.001              !! Note: Do not use q=q*0.001, because in so doing
                          !! you overwrite the q unit in the main program!
  tv=t2*(1.+0.61*qs/(1.+qs))
! slpcal=psfc*(1.+gamma/tv*ter)**(g/rgas/gamma)
  slpcal=psfc*exp(ter*g/rgas/tv)

  return
  end function slpcal
!
!
!
  subroutine usage()
!
  character(len=192) :: exe_name

  call getarg(0,exe_name)

  print*,'Usage: ',trim(exe_name),'-f <WRF NetCDF file> -c <SAMS NetCDF file> -o <output file>'

  return
  end subroutine usage

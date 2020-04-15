!------------------------------------------------------------------------------!
! © the University Corporation for Atmospheric Research 2007. Note that        !
! Government rights to use, modify, reproduce, release, perform, display or    !
! disclose this data, computer software and related documentation are          !
! restricted to a nonexclusive, nontransferable, irrevocable, royalty-free     !
! license to exercise or have exercised for or on behalf of the U.S. throughout!
! the world all the exclusive rights provided by copyrights. Such license      !
! does not include the right to sell copies or phonorecords of the copyrighted !
! works to the public. Any reproduction of data, computer software and related !
! documentation marked with this legend must also reproduce these markings.    !
!                                                                              !
! Rong-Shyang Sheu, sheu@ucar.edu, January 2007.                               !
!------------------------------------------------------------------------------!

  program wrf2prf

  use WRF_ARRAYS
  use NETCDF
  use subs

  implicit none

  character (len=256) :: fn, exe_name
  character (len=2) :: top
  integer :: ktop=20,ix0=0,iy0=0,ixrange,iyrange,istart,iend,jstart,jend

  character (len=19) :: start_date
  real :: dx,dy,xlatc,xlonc
  integer :: julday,iproj,iseason,grid_id,ncount

  integer :: iunit_config=11, iunit_flist=13

  integer :: it,i,j,k,kk

  real, parameter :: grav=9.81, rgas=287.04
  real, parameter :: sclht=rgas*256./grav

  integer :: istatus,ncid
  integer :: varid,xtype,ndims,natts
  integer, dimension(4) :: dimids

  real, pointer, dimension(:,:) :: znu,znw,znfac
  real, pointer, dimension(:,:,:,:) :: u,v,w,t,p,pb,ph,phb,qvp, &
                                       theta,ta,prs,rh,um,vm,whalf, &
                                       escale,ght,zagl
  real, pointer, dimension(:,:,:) :: ter,xlat,xlon,pbl,hfx,zruf,rmol

  character, pointer, dimension(:,:) :: times
  character(len=40), allocatable, dimension(:) :: dtstring

  integer :: imax,jmax,kmax,tmax,icsize,jcsize

  character(len=1), dimension(5) :: adomain=(/'A','B','C','D','E'/)

  character (len= 4) :: yyyy
  character (len= 2) :: mm,dd,hh,mn

  integer :: year,month,day,hour,minute
  real :: hr, rhp
! character :: domain

  integer, parameter :: iout0=21
  integer :: id
  character(len=132) :: carg, config, input_wrf, flist, atop
  character(len=7) :: aid
  logical :: lexist

  integer :: iargc

  j=iargc()

  if(j < 1) then
    call getarg(0,exe_name)
    print*,'Usage: ',trim(exe_name),' [-c <config file>] -l <input file list> [-n <number of vertical levels>]'
    stop
  end if

  config=''
  do i=1,j-1,2
     call getarg(i,carg)
     if(index(carg,'-c') > 0) then
       call getarg(i+1,config)
     else if(index(carg,'-f') > 0) then
       call getarg(i+1,input_wrf)
     else if(index(carg,'-l') > 0) then
       call getarg(i+1,flist)
     else if(index(carg,'-n') > 0) then
       call getarg(i+1,atop)
       read(atop,*) ktop
       print*,'Overwrite default number of levels to ',ktop,'!'
     else
       print*,'Invalid argument ',trim(carg)
       stop 
     end if
  enddo
     
  inquire(file=trim(config),EXIST=lexist)
  if(lexist) then
     open(iunit_config,file=trim(config))
     read(iunit_config,*) ix0
     read(iunit_config,*) iy0
     read(iunit_config,*) ixrange
     read(iunit_config,*) iyrange
     read(iunit_config,*) ktop
     close(iunit_config)
  end if

  open(iunit_flist,file=trim(flist))
  print*,'list file = ',trim(flist)

  ncount = 1
  do
     read(iunit_flist,'(a)',end=9999) fn

     istatus=nf90_open(trim(fn),0,ncid)
     if(istatus /= nf90_noerr) print*,'error opening NetCDF file: ',trim(fn)
   
   ! get some global attribute values
   
     istatus=nf90_get_att(ncid,nf90_global,'START_DATE',start_date)
     if(istatus /= nf90_noerr) print*,'Error getting global attribute: start_date'
     print*,'START DATE = ',start_date
   
     istatus=nf90_get_att(ncid,nf90_global,'DX',dx)
     if(istatus /= nf90_noerr) print*,'Error getting global attribute: dx'
     print*,'DX = ',dx
     dx=dx*0.001
   
     istatus=nf90_get_att(ncid,nf90_global,'DY',dy)
     if(istatus /= nf90_noerr) print*,'Error getting global attribute: dy'
     print*,'DY = ',dy
     dy=dy*0.001
   
     istatus=nf90_get_att(ncid,nf90_global,'MOAD_CEN_LAT',xlatc)
     if(istatus /= nf90_noerr) print*,'Error getting global attribute: MOAD_CEN_LAT'
     print*,'MOAD_CEN_LAT = ',xlatc
   
     istatus=nf90_get_att(ncid,nf90_global,'STAND_LON',xlonc)
     if(istatus /= nf90_noerr) print*,'Error getting global attribute: STAND_LON'
     print*,'STAND_LON = ',xlonc
   
     istatus=nf90_get_att(ncid,nf90_global,'JULDAY',julday)
     if(istatus /= nf90_noerr) print*,'Error getting global attribute: JULDAY'
     print*,'JULDAY = ',julday
   
     istatus=nf90_get_att(ncid,nf90_global,'GRID_ID',grid_id)
     if(istatus /= nf90_noerr) print*,'Error getting global attribute: GRID_ID'
     print*,'GRID_ID = ',grid_id
   
   ! inquire dimension ID and length
   
     istatus=nf90_inq_varid(ncid,'Times',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: Times'
   
     call get_carray(ncid,varid,'Times',times)
     icsize=size(times,1)
     jcsize=size(times,2)
     print*,'icsize, jcsize = ',icsize,jcsize
   
     allocate(dtstring(jcsize))
   
     do j=1,jcsize
     do i=1,icsize
        dtstring(j)(i:i)=times(i,j)
     end do
     end do
   
     print*,'dtstring = ',dtstring(1)
   
     read(dtstring(1),'(a4,4(x,a2))') yyyy,mm,dd,hh,mn
     read(dtstring(1),'(i4,4(x,i2))') year,month,day,hour,minute
   
     istatus=nf90_inq_varid(ncid,'U',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: U'
     call get_array3d(ncid,varid,'U',u) ! m/s (west_east_stag,south_north,bottom_top)
   
     istatus=nf90_inq_varid(ncid,'V',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: V'
     call get_array3d(ncid,varid,'V',v) ! m/s (west_east,south_north_stag,bottom_top)
   
     istatus=nf90_inq_varid(ncid,'W',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: W'
     call get_array3d(ncid,varid,'W',w) ! m/s (west_east,south_north,bottom_top_stag)
   
     istatus=nf90_inq_varid(ncid,'T',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: T'
     call get_array3d(ncid,varid,'T',t) ! perturbation potential temp in K (theta-t0)
   
     istatus=nf90_inq_varid(ncid,'P',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: P'
     call get_array3d(ncid,varid,'P',p) ! perturbation pressure in Pa
   
     istatus=nf90_inq_varid(ncid,'PB',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: PB'
     call get_array3d(ncid,varid,'PB',pb) ! base state pressure in Pa
   
     istatus=nf90_inq_varid(ncid,'PH',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: PH'
     call get_array3d(ncid,varid,'PH',ph) ! perturbation geopotential in m^2/s^2
   
     istatus=nf90_inq_varid(ncid,'PHB',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: PHB'
     call get_array3d(ncid,varid,'PHB',phb) ! base state geopotential in m^2/s^2
   
     istatus=nf90_inq_varid(ncid,'QVAPOR',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: QVAPOR'
     call get_array3d(ncid,varid,'QVAPOR',qvp) ! water vapor mixing ratio in kg/kg
   
   ! print*,'u(1,1,1,1),v(1,1,1,1),t(1,1,1,) = ',u(1,1,1,1),v(1,1,1,1),t(1,1,1,1)
     print*,'pb(1,1,1,1),phb(1,1,1,1) = ',pb(1,1,1,1),phb(1,1,1,1)
   
     istatus=nf90_inq_varid(ncid,'HGT',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: HGT'
     call get_array2d(ncid,varid,'HGT',ter) ! terrain height in m
   
     istatus=nf90_inq_varid(ncid,'XLAT',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: XLAT'
     call get_array2d(ncid,varid,'XLAT',xlat)
   
     istatus=nf90_inq_varid(ncid,'XLONG',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: XLONG'
     call get_array2d(ncid,varid,'XLONG',xlon)
   
     istatus=nf90_inq_varid(ncid,'PBLH',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: PBLH'
     call get_array2d(ncid,varid,'PBLH',pbl)
   
     istatus=nf90_inq_varid(ncid,'HFX',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: HFX'
     call get_array2d(ncid,varid,'HFX',hfx)
   
     istatus=nf90_inq_varid(ncid,'ZNT',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: ZNT'
     call get_array2d(ncid,varid,'ZNT',zruf)
   
     istatus=nf90_inq_varid(ncid,'RMOL',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: RMOL'
     call get_array2d(ncid,varid,'RMOL',rmol)
   
     istatus=nf90_inq_varid(ncid,'ZNU',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: ZNU'
     call get_array1d(ncid,varid,'ZNU',znu)
   
     istatus=nf90_inq_varid(ncid,'ZNW',varid)
     if(istatus /= nf90_noerr) print*,'error inquiring variable ID: ZNW'
     call get_array1d(ncid,varid,'ZNW',znw)

     istatus = nf90_close(ncid)
   
     imax=size(T,1)
     jmax=size(T,2)
     kmax=size(T,3)
     tmax=size(T,4)
   
     allocate(theta(imax,jmax,kmax,tmax))
     allocate(ta(imax,jmax,kmax,tmax))
     allocate(prs(imax,jmax,kmax,tmax))
     allocate(rh(imax,jmax,kmax,tmax))
   
     prs=p+pb  ! 3-D array addtion
   
     theta=t+300. ! 3-D array addition
     ta=theta*(prs*1.E-5)**(2./7.) !! array operation to get air temp in K

     do it = 1,tmax
     do k = 1, kmax
     do j = 1, jmax
     do i = 1, imax
        call r2rh(qvp(i,j,k,it),ta(i,j,k,it),prs(i,j,k,it)*0.01,rhp,0)
        rh(i,j,k,it) = rhp
     enddo
     enddo
     enddo
     enddo
   
     allocate(um(imax,jmax,kmax,tmax))
     allocate(vm(imax,jmax,kmax,tmax))
   
     do k=1,kmax
     do j=1,jmax
     do i=1,imax
        um(i,j,k,1)=0.5*(u(i,j,k,1)+u(i+1,j,k,1))
     end do
     end do
     end do
   
     do k=1,kmax
     do j=1,jmax
     do i=1,imax
        vm(i,j,k,1)=0.5*(v(i,j,k,1)+v(i,j+1,k,1))
     end do
     end do
     end do
   
     allocate(escale(imax,jmax,kmax+1,tmax))
   
     do k=1,kmax+1
     do j=1,jmax
     do i=1,imax
        escale(i,j,k,1)=exp(-(phb(i,j,k,1)+ph(i,j,k,1))/(grav*sclht))
     end do
     end do
     end do
   
     allocate(znfac(kmax,tmax))
   
     do k=1,kmax
        znfac(k,1)=(znu(k,1)-znw(k+1,1))/(znw(k,1)-znw(k+1,1))
     end do
   
     allocate(ght(imax,jmax,kmax,tmax))
     allocate(zagl(imax,jmax,kmax,tmax))
   
     do k=1,kmax
     do j=1,jmax
     do i=1,imax
        ght(i,j,k,1)=znfac(k,1)*escale(i,j,k,1)+(1.-znfac(k,1))*escale(i,j,k+1,1)
        ght(i,j,k,1)=-sclht*log(ght(i,j,k,1))
        zagl(i,j,k,1)=ght(i,j,k,1)-ter(i,j,1)
     end do
     end do
     end do
   
     print*,'ght(50,50,1,1),ter(50,50,1),zagl(50,50,1,1) = ',ght(50,50,1,1),ter(50,50,1),zagl(50,50,1,1)
   
     allocate(whalf(imax,jmax,kmax,tmax))
   
     do k=1,kmax
     do j=1,jmax
     do i=1,imax
        whalf(i,j,k,1)=znfac(k,1)*w(i,j,k,1)+(1.-znfac(k,1))*w(i,j,k+1,1)
     end do
     end do
     end do
   
     if ( ncount == 1 ) then
   
        open(iout0,file=adomain(grid_id)//'_wrf.prf')

        write(iout0,'(a)') 'PROFILE'
        write(iout0,'(a)') '9 7'
        write(iout0,'(a)') 'ID      YYYYMMDDHOUR    LAT     LON     ELEV    HFLUX   ZI      RMOL'
        write(iout0,'(a)') '                HOURS   N       E       M       W/M2    M       1/L'
        write(iout0,'(a)') 'Z       U       V       T       T       P       H'
        write(iout0,'(a)') 'M       M/S     M/S     POT     K      MB       %'
        write(iout0,'(a)') '-9999.0'
     end if
   
     if (ix0 > 0 .and. iy0 > 0) then
        istart = ix0
        iend = ix0+ixrange
        jstart = iy0
        jend = iy0+iyrange
     else
        istart = 1
        iend = imax
        jstart = 1
        jend = jmax
     end if

     do j=jstart,jend
     do i=istart,iend
        id = 1000000+i*1000+j
        write(aid,'(i7)') id
   
        hr = float(hour) + float(minute)/60.

        write(iout0,'("ID: ",a6,2x,a4,a2,a2,f8.4,2f10.4,i6,2f8.2,f10.4)') aid(2:7), &
             yyyy,mm,dd,hr,xlat(i,j,1),xlon(i,j,1),nint(ter(i,j,1)), &
             hfx(i,j,1),zruf(i,j,1),rmol(i,j,1)
   
        do k=1,ktop
           write(iout0,'(f7.1,2f7.2,4f8.2)') zagl(i,j,k,1),um(i,j,k,1), &
                vm(i,j,k,1),theta(i,j,k,1), ta(i,j,k,1), prs(i,j,k,1)*0.01,rh(i,j,k,1)
        end do
        ncount = ncount+1
     enddo
     enddo
   
     deallocate(u,v,w,t,p,pb,ph,phb,qvp,theta,ta,prs,um,vm,escale,ght,zagl,whalf)
     deallocate(ter,xlat,xlon,pbl,hfx,zruf,rmol)
     deallocate(znu,znw,znfac)
     deallocate(times,dtstring)

  end do

9999 continue

  close(iout0)
   
  end program wrf2prf

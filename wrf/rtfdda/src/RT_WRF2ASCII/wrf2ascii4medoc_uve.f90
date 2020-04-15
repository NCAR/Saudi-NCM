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

  program wrf2ascii4medoc

  use WRF_ARRAYS
  use NETCDF

  implicit none

  INTERFACE

    subroutine get_ruf(iunit,asource,ruf)

    integer :: iunit
    character (len=*) :: asource
    real, pointer, dimension(:,:) :: ruf
    logical :: THERE
    character (len=120) :: line
    character (len=80) :: acolumns,vegetation
    character (len=10) :: aseason

    end subroutine get_ruf

  END INTERFACE

  character (len=256) :: fn, exe_name
  character (len=2) :: top
  integer :: ktop=20

  character (len=19) :: start_date,mminlu
  real :: dx,dy,xlatc,xlonc
  integer :: julday,iproj,iseason,grid_id

  integer :: ilu_unit=11

  integer :: i,j,k,kk

  real, parameter :: grav=9.81, rgas=287.04
  real, parameter :: sclht=rgas*256./grav

  integer :: istatus,ncid
  integer :: varid,xtype,ndims,natts
  integer, dimension(4) :: dimids

  real, pointer, dimension(:,:) :: znu,znw,znfac,ruf
  real, pointer, dimension(:,:,:,:) :: u,v,w,t,p,pb,ph,phb,qvp, &
                                       theta,prs,um,vm,whalf,escale,ght,zagl
! real, pointer, dimension(:,:,:,:) :: ua, va
  real, pointer, dimension(:,:,:,:) :: uue, uve, vve
  real, pointer, dimension(:,:,:) :: rlu,mapfac,ter,xlat,xlon,pbl,hfx
  real, pointer, dimension(:,:) :: zruf

  character, pointer, dimension(:,:) :: times
  character(len=40), allocatable, dimension(:) :: dtstring

  integer :: imax,jmax,kmax,tmax,icsize,jcsize

  character(len=1), dimension(5) :: adomain=(/'A','B','C','D','E'/)

  character (len= 4) :: yyyy
  character (len= 2) :: mm,dd,hh,mn

  integer :: year,month,day,hour,minute
  integer*8 :: nsecs0,nsecs
  integer :: isec_diff,e_hour,e_min,e_sec,total_min
! character :: domain

  integer, parameter :: iout0=21, iout1=22

  integer :: iargc

  j=iargc()

  if(j < 1) then
    call getarg(0,exe_name)
    print*,'Usage: ',trim(exe_name),' WRF_file [number of desired levels]'
    stop
  end if

  call getarg(1,fn)

  if(j > 1) then
    call getarg(2,top)
    print*,'Overwrite default number of levels to ',top,'!'
    read(top,'(i2)') ktop
  end if

! open NetCDF file:

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

  istatus=nf90_get_att(ncid,nf90_global,'MMINLU',mminlu)
  if(istatus /= nf90_noerr) print*,'Error getting global attribute: MMINLU'
  print*,'MMINLU = ',trim(mminlu) 
  call get_ruf(ilu_unit,mminlu,ruf)

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

  call sec_from_date_time(year,1,1,0,0,nsecs0)
  call sec_from_date_time(year,month,day,hour,minute,nsecs)

  isec_diff=nsecs-nsecs0
  call elaps(isec_diff,e_hour,e_min,e_sec)

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


! istatus=nf90_inq_varid(ncid,'UA',varid)
! if(istatus /= nf90_noerr) print*,'error inquiring variable ID: UA'
! call get_array3d(ncid,varid,'UA',ua) ! x-wind component on mass grids m/s            

! istatus=nf90_inq_varid(ncid,'VA',varid)
! if(istatus /= nf90_noerr) print*,'error inquiring variable ID: VA'
! call get_array3d(ncid,varid,'VA',ua) ! y-wind component on mass grids m/s            

  istatus=nf90_inq_varid(ncid,'UUE',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: VA'
  call get_array3d(ncid,varid,'UUE',uue) ! x-wind variance  on mass grids m/s            

  istatus=nf90_inq_varid(ncid,'UVE',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: VA'
  call get_array3d(ncid,varid,'UVE',uve) ! wind co-variance  on mass grids m/s            

  istatus=nf90_inq_varid(ncid,'VVE',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: VA'
  call get_array3d(ncid,varid,'VVE',vve) ! y-wind variance  on mass grids m/s            

! print*,'u(1,1,1,1),v(1,1,1,1),t(1,1,1,) = ',u(1,1,1,1),v(1,1,1,1),t(1,1,1,1)
  print*,'pb(1,1,1,1),phb(1,1,1,1) = ',pb(1,1,1,1),phb(1,1,1,1)


  istatus=nf90_inq_varid(ncid,'LU_INDEX',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: LU_INDEX'
  call get_array2d(ncid,varid,'LU_INDEX',rlu)

  istatus=nf90_inq_varid(ncid,'MAPFAC_M',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: MAPFAC_M'
  call get_array2d(ncid,varid,'MAPFAC_M',mapfac)

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

  istatus=nf90_inq_varid(ncid,'ZNU',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: ZNU'
  call get_array1d(ncid,varid,'ZNU',znu)

  istatus=nf90_inq_varid(ncid,'ZNW',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: ZNW'
  call get_array1d(ncid,varid,'ZNW',znw)

  imax=size(T,1)
  jmax=size(T,2)
  kmax=size(T,3)
  tmax=size(T,4)

  allocate(zruf(imax,jmax))

  if(julday < 105 .or. julday > 288 ) then
    iseason=2
  else
    iseason=1
  endif

  do j=1,jmax
  do i=1,imax
     zruf(i,j)=ruf(nint(rlu(i,j,1)),iseason)   !! in m already
  end do
  end do

  allocate(theta(imax,jmax,kmax,tmax))
  allocate(prs(imax,jmax,kmax,tmax))

  prs=p+pb  ! 3-D array addtion

  theta=t+300. ! 3-D array addition

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

  open(iout0,file=adomain(grid_id)//yyyy//mm//dd//hh//mn,form='formatted')

  open(iout1,file=adomain(grid_id)//yyyy//mm//dd//hh//mn//'.cfg',form='formatted')

  write(iout1,'(i2,2i4," : kmax3d, jmax, imax")') ktop,imax,jmax !! yes
  write(iout1,'(2f11.6," : x,y (km) - grid spacing")') dx,dx
  write(iout1,'(2f11.6," : xlonc, xlatc - projection center lon/lat (deg)", &
       " of coarse domain")') xlonc,xlatc
  write(iout1,'(i4,4i3," : year,month,day,hour,min of forecast", &
       " start time (UTC)")') year,1,1,0,0
  write(iout1,'("6 : numfields - number of fields in this file")')
  write(iout1,'("TERRAIN")')
  write(iout1,'("1 1 :k,ktop")')
  do i=1,imax
     write(iout1,'(100f7.1)') (ter(i,j,1),j=1,jmax)
  end do
!
  write(iout1,'("LATITCRS")')
  write(iout1,'("1 1 :k,ktop")')
  do i=1,imax
     write(iout1,'(100f10.4)') (xlat(i,j,1),j=1,jmax)
  end do
!
  write(iout1,'("LONGICRS")')
  write(iout1,'("1 1 :k,ktop")')
  do i=1,imax
     write(iout1,'(100f10.4)') (xlon(i,j,1),j=1,jmax)
  end do
!
  write(iout1,'("MAPFACCR")')
  write(iout1,'("1 1 :k,ktop")')
  do i=1,imax
     write(iout1,'(100f8.5)') (mapfac(i,j,1),j=1,jmax)
  end do
!
  write(iout1,'("ZRUF")')
  write(iout1,'("1 1 :k,ktop")')
  do i=1,imax
     write(iout1,'(100f8.5)') (zruf(i,j),j=1,jmax)
  end do
!
  write(iout1,'("ZAGL")')
  do k=1,ktop
     write(iout1,'(2i3," :k,ktop")') k,ktop
     do i=1,imax
        write(iout1,'(100f8.1)') (zagl(i,j,k,1),j=1,jmax)
     end do
  end do

  close(iout1)
!
  write(iout0,'(i2,2i4," : kmax3d, jmax, imax")') ktop,imax,jmax  !! yes
  write(iout0,'(i4,4i3," : year,month,day,hour,min of forecast", &
       " start time (UTC)")') year,1,1,0,0
  write(iout0,'(i4,2i3," : elapsed time hours,min,seconds")') e_hour,e_min,e_sec
  write(iout0,'("9 : numfields - number of fields in this file")')  
  write(iout0,'("THETA")')
  do k=1,ktop
     write(iout0,'(2i3," :k,ktop")') k,ktop
     do i=1,imax
        write(iout0,'(100f6.1)') (theta(i,j,k,1),j=1,jmax)
     end do
  end do
!
  write(iout0,'("UC")')
  do k=1,ktop
     write(iout0,'(2i3," :k,ktop")') k,ktop
     do i=1,imax
        write(iout0,'(100f7.2)') (um(i,j,k,1),j=1,jmax)
     end do
  end do
!
  write(iout0,'("VC")')
  do k=1,ktop
     write(iout0,'(2i3," :k,ktop")') k,ktop
     do i=1,imax
        write(iout0,'(100f7.2)') (vm(i,j,k,1),j=1,jmax)
     end do
  end do
!
  write(iout0,'("WHALF")')
  do k=1,ktop
     write(iout0,'(2i3," :k,ktop")') k,ktop
     do i=1,imax
        write(iout0,'(100f8.4)') (whalf(i,j,k,1),j=1,jmax)
     end do
  end do
!
  kk=1
  write(iout0,'("PBL HT")')
  write(iout0,'(2i3," :k,ktop")') kk,kk
  do i=1,imax
     do j=1,jmax
        if(pbl(i,j,1) < 1.0) pbl(i,j,1)=1.0
     end do
     write(iout0,'(100f8.2)') (pbl(i,j,1),j=1,jmax)
  end do
!
  write(iout0,'("HFX")')
  write(iout0,'(2i3," :k,ktop")') kk,kk
  do i=1,imax
     write(iout0,'(100f7.1)') (hfx(i,j,1),j=1,jmax)
  end do
!
  write(iout0,'("UUE")')
  do k=1,ktop
     write(iout0,'(2i3," :k,ktop")') k,ktop
     do i=1,imax
        write(iout0,'(100f7.2)') (uue(i,j,k,1),j=1,jmax)
     end do
  end do
!
  write(iout0,'("UVE")')
  do k=1,ktop
     write(iout0,'(2i3," :k,ktop")') k,ktop
     do i=1,imax
        write(iout0,'(100f7.2)') (uve(i,j,k,1),j=1,jmax)
     end do
  end do
!
  write(iout0,'("VVE")')
  do k=1,ktop
     write(iout0,'(2i3," :k,ktop")') k,ktop
     do i=1,imax
        write(iout0,'(100f7.2)') (vve(i,j,k,1),j=1,jmax)
     end do
  end do

  close(iout0)

  deallocate(u,v,w,t,p,pb,ph,phb,qvp,theta,prs,um,vm,escale,ght,zagl,whalf)
  deallocate(mapfac,ter,xlat,xlon,pbl,hfx)
  deallocate(znu,znw,znfac)
  deallocate(ruf,zruf)
  deallocate(times)

  end program wrf2ascii4medoc 
!
!
!
  subroutine get_ruf(iunit,asource,ruf)

  integer :: iunit
  character (len=*) :: asource
  real, pointer, dimension(:,:) :: ruf
  logical :: THERE
  character (len=120) :: line
  character (len=80) :: acolumns,vegetation
  character (len=10) :: aseason

  inquire(file='LANDUSE.TBL',exist=THERE)

  if(.not. THERE) then
    print*,'Error: Require LANDUSE.TBL file in the working directory!'
    stop
  end if

  open(iunit,file='LANDUSE.TBL',status='old')

  nb=index(asource,' ')

  do while (1)
     read(11,'(a)',iostat=ierr) line

     if(ierr /= 0) exit

     nb=index(line,' ')
     if(index(asource,line(1:nb-1)) > 0) then
       read(11,*) ncats,nseasons,acolumns
       allocate(ruf(ncats,nseasons))
       do j=1,nseasons
          read(11,*) aseason
          do i=1,ncats
             read(11,*) icat,albd,slmo,sfem,sfz0,therin,scfx,sfhc,vegetation
             ruf(i,j)=sfz0*0.01  ! in meters
          end do
       end do
       exit
     end if

  end do

  return

  end subroutine get_ruf
!
!
!
  subroutine elaps(isecs,e_hour,e_min,e_sec)

  integer :: e_hour,e_min,e_sec
!
  e_hour=int(isecs/3600.)
  e_min=int((mod(isecs,3600))/60.)
  e_sec=isecs-e_hour*3600-e_min*60

  return
  end subroutine elaps
!
!
!
  subroutine sec_from_date_time(iyear,month,iday,ihour,iminute,n_secs)
!
  integer, dimension(12) :: mdays
  integer*8 :: n_secs
!
  mdays=(/31,28,31,30,31,30,31,31,30,31,30,31/)
!
  n_secs=0
!
  do iy=1,iyear-1970
     idays=365
!
     if(mod(iy+1969,100) == 0) then
       if(mod(iy+1969,400) == 0) then
         idays=366
       endif
     else
       if(mod(iy+1,4) == 0) idays=366
     endif
!
     n_secs=n_secs+idays*86400
  enddo
!
  if(mod(iyear,100) == 0) then
    if(mod(iyear,400) == 0) then
      mdays(2)=29
    endif
  else
    if(mod(iyear,4) == 0) mdays(2)=29
  endif
!
  do im=1,month-1
     n_secs=n_secs+mdays(im)*86400
  enddo
!
  do id=1,iday-1
     n_secs=n_secs+86400
  enddo
!
  do ih=1,ihour
     n_secs=n_secs+3600
  enddo
!
  do im=1,iminute
     n_secs=n_secs+60
  enddo
!
  return
  end subroutine sec_from_date_time

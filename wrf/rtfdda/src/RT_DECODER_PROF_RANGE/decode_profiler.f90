  program decode_profiler
!
  use WRF_ARRAYS
  use NETCDF
  use DATETIME_MODULE

  implicit none

  character(len=192) :: flnm, sarg
  character(len=20) :: byyyymmddhh, eyyyymmddhh
  integer :: ibyyyymmddhh, ieyyyymmddhh
  integer :: ibegin, iend
  integer :: i, iargc, j, ncid, istatus, varid, icsize, jcsize, base_time, &
             nprofilers, nlevels, ntimes, il, ip, it
  logical :: fopt=.false., bopt=.false., eopt=.false.

  character, pointer, dimension(:,:) :: platform
  real, pointer, dimension(:) :: lat, lon, alt   ! dimension is 'platform'
  real, pointer, dimension(:) :: zagl   ! dimension is 'elevation'
  real, pointer, dimension(:,:,:) :: wdir, wspd, wdir_qc, wspd_qc
  integer, allocatable, dimension(:,:,:) :: iwdir_qc, iwspd_qc
  double precision, pointer, dimension(:) :: time_offset
  integer(8) :: time, datetime
  character(len=20), allocatable, dimension(:) :: station_name
  character(len=20) :: mdate
  real :: missing_value, new_missing=-888888.
  integer :: imissing=-888888
  character(len=40) :: string1  = '99001                                   ', &
                       string2  = ' PROFILER ATEC                          ', &
                       string3  = 'FM-132 PROFL                            ', &
                       string4  = '                                        '
  integer :: ilen
  integer :: stindx=999, iunit=31
!
!
!
  j=iargc()
  if(j == 0) then
    call usage()
    stop
  endif

  i=1
  do while(i <= j)
     call getarg(i,sarg)
     if(index(sarg,'-f') > 0) then
       i=i+1
       fopt=.true.
       call getarg(i,flnm)
     else if(index(sarg,'-b') >0 ) then
       i=i+1
       bopt=.true.
       call getarg(i,byyyymmddhh)
     else if(index(sarg,'-e') >0 ) then
       i=i+1
       eopt=.true.
       call getarg(i,eyyyymmddhh)
     else
       print*,'Unrecognized flag: ',trim(sarg)
       call usage()
       stop
     end if
     i=i+1
  end do

  if((.not. fopt) .or. (.not. bopt) .or. (.not. eopt)) then
    call usage()
    stop
  end if

  read(byyyymmddhh,*) ibyyyymmddhh
  read(eyyyymmddhh,*) ieyyyymmddhh

  ibegin=date2secs(ibyyyymmddhh)
  iend=date2secs(ieyyyymmddhh)

  print*,'flnm = ',trim(flnm),' begin = ',byyyymmddhh,' end = ',eyyyymmddhh, &
         ' ibegin = ',ibegin,' iend = ',iend

  istatus=nf90_open(trim(flnm),0,ncid)
  if(istatus /= nf90_noerr) then
    print*,'error opening profiler NetCDF file: ',trim(flnm)
    stop
  end if

  istatus=nf90_inq_varid(ncid,'platform',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: platform'
  call get_carray(ncid,varid,'platform',platform)

  istatus=nf90_inq_varid(ncid,'lat',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: lat'
  call get_array0d(ncid,varid,'lat',lat)

  istatus=nf90_inq_varid(ncid,'lon',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: lon'
  call get_array0d(ncid,varid,'lat',lon)

  istatus=nf90_inq_varid(ncid,'alt',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: alt'
  call get_array0d(ncid,varid,'alt',alt)

  istatus=nf90_inq_varid(ncid,'zagl',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: zagl'
  call get_array0d(ncid,varid,'zagl',zagl)

  istatus=nf90_inq_varid(ncid,'wdir',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: wdir'
  call get_array2d(ncid,varid,'wdir',wdir)

  istatus=nf90_inq_varid(ncid,'Dir_qc',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: Dir_qc'
  call get_array2d(ncid,varid,'Dir_qc',wdir_qc)

  istatus=nf90_inq_varid(ncid,'wspd',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: wspd'
  call get_array2d(ncid,varid,'wspd',wspd)

  istatus=nf90_inq_varid(ncid,'Spd_qc',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: Spd_qc'
  call get_array2d(ncid,varid,'Spd_qc',wspd_qc)

  istatus=nf90_inq_varid(ncid,'time_offset',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: time_offset'
  call get_darray0d(ncid,varid,'time_offset',time_offset)

  istatus=nf90_inq_varid(ncid,'base_time',varid)
  if(istatus /= nf90_noerr) print*,'error inquiring variable ID: base_time'
  istatus=nf90_get_var(ncid,varid,base_time)
  if(istatus /= nf90_noerr) print*,'error reading variable base_time'

  istatus=nf90_get_att(ncid,nf90_global,'missing_value',missing_value)
  if(istatus /= nf90_noerr) print*,'error reading global attribute missing_value'

  icsize=size(platform,1)
  jcsize=size(platform,2)   ! jcize is total number of stations

  allocate(station_name(jcsize))

  do j=1,jcsize
  do i=1,icsize
     station_name(j)(i:i)=platform(i,j)
  end do
  end do

  nprofilers=jcsize
  nlevels=size(wdir,1)
  ntimes=size(wdir,3)

  allocate(iwdir_qc(nlevels,nprofilers,ntimes))
  allocate(iwspd_qc(nlevels,nprofilers,ntimes))

  where (wdir == missing_value)
     wdir=new_missing
     iwdir_qc=imissing
  elsewhere
     iwdir_qc=nint(wdir_qc)
  end where

  where (wspd == missing_value)
     wspd=new_missing
     iwspd_qc=imissing
  elsewhere
     iwspd_qc=nint(wspd_qc)
  end where

  do it=1,ntimes
     time=int(base_time+time_offset(it))
     if(time < ibegin) cycle
     if(time > iend) exit
     call secs2datetime(time,datetime)
     write(mdate,'(i14)') datetime
     do ip=1,nprofilers
!       do il=1,nlevels
!          print*,il,alt,zagl(il),wdir(il,ip,it),wspd(il,ip,it)
!       end do
        ilen=len_trim(station_name(ip))
        string2(16:16+ilen-1)=trim(station_name(ip))
        open(iunit,file=trim(mdate)//'_PROF')
        call write_obs(stindx,zagl,wspd(:,ip,it),wdir(:,ip,it), &
             alt(ip),lat(ip),lon(ip),iwdir_qc(:,ip,it),iwspd_qc(:,ip,it), &
             mdate,nlevels,string1,string2,string3,string4,iunit)
        close(iunit)
     end do
  end do

  deallocate(iwdir_qc)
  deallocate(iwspd_qc)

  end program decode_profiler
!
!
!
  subroutine usage()
  character(len=192) :: exe_name

  call getarg(0,exe_name)

  print*,'Usage: ',trim(exe_name),'-f <WRF NetCDF file> -b <begin time in epoch seconds> -e <end time in epoch seconds>'

  return
  end subroutine usage
!
!
!
  subroutine write_obs (stindx,  z ,  spd , dir , &
             ter , xlat , xlon , wdir_qc, wspd_qc, &
             mdate , kx , string1, string2, string3, string4, iunit)

  implicit none

  integer :: stindx,iunit,kx, k
  real :: z(kx),spd(kx),dir(kx)
  integer :: wdir_qc(kx),wspd_qc(kx)
  real :: ter, xlat, xlon
  real :: uu,vv

  character(len=20) :: date_char
  character(len=14) :: mdate
  character(len=40) :: string1, string2 , string3 , string4
  character(len=120) :: rpt_format
  character(len=120) :: meas_format
  character(len=120) :: end_format
  integer :: iwindqc, iseq_num=99
  logical :: bogus=.false.
  real :: deg2rad=acos(-1.)/180.

  rpt_format =  ' ( 2f20.5 , 2a40 , ' &
             // ' 2a40 , 1f20.5 , 5i10 , 3L10 , ' &
             // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
  meas_format =  ' ( 10( f13.5 , i7 ) ) '
  end_format = ' ( 3 ( i7 ) ) '

  date_char(7:20) = mdate(1:14)
  date_char(1:6)='      '
 
  write ( unit = iunit , err = 19 , fmt = rpt_format ) &
        xlat,xlon, string1 , string2 , &
        string3 , string4 , ter, kx*6, 0,0,iseq_num,0, &
        .true.,bogus,.false., &
         -888888, -888888, date_char , &
         -888888.,0,-888888.,0, -888888.,0, -888888.,0, -888888.,0, &
               -888888.,0, &
               -888888.,0, -888888.,0, -888888.,0, -888888.,0, &
               -888888.,0, &
               -888888.,0, -888888.,0

  do 100 k = 1 , kx
     if(dir(k) > 0. .and. spd(k) > 0.) then
       uu=-spd(k)*sin(dir(k)*deg2rad)
       vv=-spd(k)*cos(dir(k)*deg2rad)
     else
       uu=-888888.
       vv=-888888.
     endif

     iwindqc=min(wdir_qc(k),wspd_qc(k))

     write ( unit = iunit , err = 20 , fmt = meas_format ) &
        -888888.,0,(z(k)+ter)*1000.,-888888,-888888.,-888888,-888888.,-888888, &
        spd(k),iwindqc, dir(k),iwindqc, &
        uu,iwindqc, vv,iwindqc, -888888.,0, -888888.,0
!
!        print *,' k= ',k,' z= ',z(k), &
!             ' spd-dir= ',spd(k),dir(k),' uu-uuqc= ',uu(k),zuu_qc(k), &
!             ' vv-vvqc= ',vv(k),zvv_qc(k)
!
100   continue

  write ( unit = iunit , err = 21 , fmt = meas_format ) &
         -777777.,0, -777777.,0,float(kx),0, &
         -888888.,0, -888888.,0, -888888.,0, &
         -888888.,0, -888888.,0, -888888.,0, &
         -888888.,0

  write ( unit = iunit , err = 19 , fmt = end_format )  kx, 0, 0

  return

19    continue
      print *,' troubles writing a sounding - err=19 '
      stop 19
20    continue
      print *,' troubles writing a sounding - err=20 '
      stop 20
21    continue
      print *,' troubles writing a sounding - err=21 '
      stop 21

  end subroutine write_obs

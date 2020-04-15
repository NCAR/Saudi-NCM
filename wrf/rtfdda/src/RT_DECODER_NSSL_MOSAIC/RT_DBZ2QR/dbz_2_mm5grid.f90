!---------------------------------------------------------------------
! Mei Xu  03-22-2006
!
! read in dbz datasets (merged from tiles for the specific range)
! read in model grid; the dbz grid matches mm5grid horizontally 
! map dbz onto model grid vertically
! filter/interpolate in time 
! write out dbz on model grid
!
!---------------------------------------------------------------------
!
!     program main
!     use MODULE_READ_MM5
      implicit none
      INTERFACE 
      subroutine readmm5_z(infile,nx,ny,nz,zz)
      character(len=*) infile
      integer nx,ny,nz,i,j,k, num
      integer :: iunit_in  = 10, iunit_out = 11
      real :: r=287.04,g=9.8
      real, allocatable, dimension(:,:,:) :: sigma, pstar
      real, pointer, dimension(:,:,:) :: zz
      integer bhi(50,20)
      real bhr(20,20)
      character*80 bhic(50,20),bhrc(20,20)
      integer start_index(4),end_index(4)
      character*9 name
      character*4 staggering,ordering
      character*24 current_date
      character*25 unit
      character*46 description
      integer flag, ierr, ier
      end subroutine readmm5_z
      END  INTERFACE 
      character(len=256) mm5_file,radar_file,date_str,out_file
      LOGICAL OPENED,exist
      integer IYYD,IMMD,IDDD,IHHD,IMND
! radar file dimension and variables
      integer nx, ny, nzrad
      real,allocatable,dimension(:)::  v_level
      real,allocatable,dimension(:,:,:)::dz
      real BADPT
! mm5 grid and variables, note mix is for S-N, mjx W-E and mkx downward 
      integer mix,mjx,mkx,nz
      real,pointer,dimension(:,:,:):: zz
      real,allocatable,dimension(:,:,:):: dzmm5, slopef
      integer,allocatable,dimension(:,:,:):: kindex
      real,allocatable,dimension(:,:,:):: cs1, cs2
      integer i,j,k
      character(len=19) dateStr, range, domain
      integer domainID
      real,allocatable,dimension(:)::  z_level
      
      BADPT=-999.

!     read the argument 
      call getarg(1,mm5_file) !! mm5 configuration file.
      call getarg(2,radar_file)
      call getarg(3,dateStr)
      call getarg(4,range)
      call getarg(5,out_file)

! check existence of files
      INQUIRE (file=mm5_file,EXIST=exist)
      if( .not. exist ) then
        print*, ' mm5 grid file not exist, do not process'  
        stop
      endif
      INQUIRE (file=radar_file,EXIST=exist)
      if( .not. exist ) then
        print*, ' merged radar file not exist, do not process'  
        stop
      endif
      
! read in 3D mm5grid infomation from the given file
      call readmm5_z(mm5_file,mix,mjx,mkx,zz)
      nz=mkx
!
!  get sizes of the merged radar file.
!
      call getdim(radar_file, nx,ny,nzrad,3,"dz")

! check horizontal dimensions - should have the same horizontal grid 
      IF((mix.NE.nx).OR.(mjx.NE.ny)) THEN
        print*,'radar datasets was not prepared for the mm5 grid'
        STOP
      ENDIF 

! read in nssl mosaic dbz obs (merged for the domain)
      allocate(dz(nx,ny,nzrad))
      allocate(v_level(nzrad))     ! for the vertical level
      call readnetcdf(radar_file,dz,"dz",v_level,"Level",nx,ny,nzrad)
!     print*,((v_level(k),dz(10,10,k)),k=1,nzrad)

! now do vertical interpolation to MM5 grid.

      allocate(kindex(mix,mjx,mkx))
      allocate(slopef(mix,mjx,mkx))
      call zparamsM(mix,mjx,mkx,zz,nzrad,v_level,slopef,kindex)
      allocate(dzmm5(mix,mjx,mkx))
      call zinterpM(mix,mjx,nzrad,dz,mkx,slopef,kindex,dzmm5,BADPT)
!     print*,((zz(nx/2,ny/2,k),dzmm5(nx/2,ny/2,k)),k=1,nz)

! process additional datasets for data filtering/filling purpose
!
!     call readnetcdf(radar_file,dz,"dz",v_level,"Level",nx,ny,nz)
!     call readnetcdf(radar_file,dz,"dz",v_level,"Level",nx,ny,nz)

! temporal filling

      deallocate(kindex) 
      deallocate(slopef) 
      deallocate(dz)
      deallocate(v_level)

! output the dbz filed on mm5grid

      call output_dbz_netcdf(out_file,dzmm5,"dz",dateStr,range,&
                         nx,ny,nz)

      deallocate(dzmm5)
      deallocate(zz)

! test read
!     allocate(zz(mix,mjx,mkx))
!     call read_dbz_netcdf(out_file,zz,"dz",nx,ny,nz)

      stop
      end

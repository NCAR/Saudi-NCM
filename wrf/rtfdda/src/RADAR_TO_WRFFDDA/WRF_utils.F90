module WRF_utils

    use WRF_kinds
    use WRF_ncread

   implicit none
contains

!===============================================================================
   subroutine wrf_hgt(filename,nTimeInd,zCoord)

   implicit none

! !INPUT/OUTPUT PARAMETERS:
   character(*),intent(in) :: filename                  ! name
   integer(INT),intent(in) :: nTimeInd
   real(R8),intent(out) :: zCoord(:,:,:) !

   !----- local -----
   integer(INT) :: i,iz,j,k          ! loop indices
   integer(INT) :: ndims             ! number of dimensions in a field
   integer(INT) :: nx,ny,nz          ! dimensions of netCDF fields
   integer(INT) :: nVert_stag        ! value of netCDF bottom_top_stag dimension
   integer(INT) :: nVert,nWE,nSN ! value of netCDF west_east,south_north stag dimension
   integer(INT) :: nWE_stag,nSN_stag ! value of netCDF west_east,south_north stag dimension
   integer(INT) :: nTimes
   real(R8),parameter :: c1      = 1.0_R8
   real(R8),parameter :: g0          = 9.80665_R8   ! Acceleration due to gravity
   real(R8),parameter :: Rd          = 287._R8      ! Gas constant for dry air
   real(R8),parameter :: scale_ht    = Rd*256._R8/g0! Scale height

   real(R8),allocatable :: work4d(:,:,:,:)    ! 4d work array
   real(R8),allocatable :: work3d(:,:,:)      ! 3d work array
   real(R8),allocatable :: work1d(:)          ! 1d work array
   real(R8),allocatable :: znw(:),znu(:)  ! eta values, full, half levels
   real(R8),allocatable :: znWeight(:)      ! eta weights
   real(R8),allocatable :: geoExp(:,:,:)      ! Exponential height variable
   real(R8),allocatable :: zValues(:,:,:)       ! Vert. values from netCDF file
   real(R8),allocatable :: terrain_hgt(:,:)   ! Model elevation

   real(R8) ,dimension(:,:,:),allocatable :: Zbase,Zpert   ! Height dependent

   !--- Get dimensions 
   !---------------------------------------------------------------------------
   call get_dimension(filename, "bottom_top", nVert)
   call get_dimension(filename, "bottom_top_stag", nVert_stag)
   call get_dimension(filename, "south_north_stag", nSN_stag)
   call get_dimension(filename, "west_east_stag"  , nWE_stag)
   call get_dimension(filename, "south_north", nSN)
   call get_dimension(filename, "west_east"  , nWE)

   !---------------------------------------------------------------------------
   !--- Read in the vertical "eta" coordinate values of both the full (w) and
   !--- half (mass) levels
   !---------------------------------------------------------------------------
    allocate(work4d(nWE,nSN,1,1))
    allocate(terrain_hgt(nWE,nSN))
    print*, filename,nTimes,nTimeInd
    call ncread_field4dG(filename, 'HGT', rfld=work4d, dim3i=nTimeInd)
    terrain_hgt(:,:) = work4d(:,:,1,1)
    deallocate(work4d)

    allocate(work4d(nVert_stag,1,1,1))
    call ncread_field4dG(filename, 'ZNW', rfld=work4d, dim2i=nTimeInd)
    allocate(znw(nVert_stag))
    znw = work4d(:,1,1,1)
    deallocate(work4d)

    allocate(work4d(nVert,1,1,1))
    call ncread_field4dG(filename, 'ZNU', rfld=work4d, dim2i=nTimeInd)
    allocate(znu(nVert))
    znu = work4d(:,1,1,1)
    deallocate(work4d)

    allocate(znWeight(nVert))
     do k=1,nVert
         znWeight(k)=(znu(k)-znw(k))/(znw(k+1)-znw(k))
     enddo
    deallocate(znu, znw)

    allocate(Zbase(nWE,nSN,nVert_stag), Zpert(nWE,nSN,nVert_stag))

    call ncread_varDimNum(filename,'PHB',ndims)
    call ncread_varDimSizes(filename,'PHB',nx,ny,nz)
    allocate(work4d(nx, ny, nz, 1))
    call ncread_field4dG(trim(filename), 'PHB', &
     &                    rfld=work4d, dim4i=nTimeInd)
     Zbase = work4d(:,:,:,1)

    call ncread_varDimNum(filename,'PH',ndims)
    call ncread_varDimSizes(filename,'PH',nx,ny,nz)
    allocate(work4d(nx, ny, nz, 1))
    call ncread_field4dG(trim(filename), 'PH', &
     &                    rfld=work4d, dim4i=nTimeInd)
     Zpert(:,:,:) = work4d(:,:,:,1)
    deallocate(work4d)

   !---------------------------------------------------------------------
   !--- Calculate values for point(s)
   !--- For the geopotential: the WRF vertical coordinate is mass based.
   !--- (1) Convert the geopotential to an exponential height variable,
   !---     exp(-z/H), where H is a scale height.
   !--- (2) Linearly interpolate in WRF vert. coord. ("eta") from full
   !---     (w) levels to "mass levels", and then convert back to
   !---     geopotential height.
   !--- (3) Interpolate exp(-z/H) to "mass levels", convert to height
   !---------------------------------------------------------------------
      allocate(zValues(nWE,nSN,nVert),geoExp(nWE,nSN,nVert_stag))
      geoExp(:,:,:) = dexp(-(Zbase(:,:,:) + Zpert(:,:,:))/(g0*scale_ht))  
      do k = 1,nVert
       zValues(:,:,k) = znWeight(k)*geoExp(:,:,k+1) &
                        + (c1 - znWeight(k))*geoExp(:,:,k)
       zValues(:,:,k) = -scale_ht*dlog(zValues(:,:,k))  ! Height above sea level
!      zValues(:,:,k) = zValues(:,:,k) - terrain_hgt(:,:) ! Height AGL
      enddo
      zCoord = zValues
      deallocate(geoExp)
      deallocate(zValues) 
    deallocate(znWeight)
    deallocate(terrain_hgt)
    deallocate(Zbase)
    deallocate(Zpert)

    END subroutine wrf_hgt
END module WRF_utils

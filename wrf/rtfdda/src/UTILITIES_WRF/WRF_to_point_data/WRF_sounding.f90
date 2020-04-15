!=======================================================================
! CVS: $Id: WRF_sounding.f90,v 1.29 2017/06/01 05:27:55 sheu Exp $
! CVS: $Source: /cvs/apps/netcdf_utils/src/WRF_to_point_data/WRF_sounding.f90,v $
! CVS: $Name:  $
!=======================================================================
!BOP ===================================================================
!
! !MODULE: WRF_sounding -  retreive a WRF virtual sounding at a point for NAPS
!
! !DESCRIPTION:
!    Contains a subroutine to retreive WRF virtual sounding at a point for NAPS
!
! !REVISION HISTORY: !
! 2005-Sep-9 - J. Schramm - first version
!
! !INTERFACE: ----------------------------------------------------------

module WRF_sounding

! !USES:

    use WRF_kinds
    use WRF_ncread
    use WRF_constants
    use WRF_tools
    use WRF_input
    use derived_fields
    use write_headers

!EOP

   implicit none

   private    ! except

! !PUBLIC TYPES:

   ! none

! !PUBLIC MEMBER FUNCTIONS:

   public :: sounding_data   ! Get WRF virtual sounding at a point for NAPS
   public :: metcm           ! Get METCM output

! !PUBLIC DATA MEMBERS:

   ! none

!EOP
!===============================================================================
contains
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: sounding_data -- retrieve WRF virtual sounding at a location
!
! !DESCRIPTION:  
!    Read in time slices of fields from a WRF netCDF file, average to unstaggered
!    grid if necessary, interpolate model output to obs point, calculate necessary
!    fields, return sounding of the following values:
!
!---  (1) Height of model levels (m)
!---  (2) Temperature (C)
!---  (3) Relative Humidity (%)
!---  (4) Wind speed (m/s)
!---  (5) Wind direction (degrees)
!---  (6) Pressure (mb)
!
! !REVISION HISTORY:
!     2005-Sep-9 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine sounding_data(out_unit, zCoord, dz, im, topz, writeBufkit, writeYPG2, writeREDI, rnge, statusFlag, statusInfo)

  implicit none

! !INPUT/OUTPUT PARAMETERS:

   integer(INT),intent(in)  :: out_unit       ! Unit specifier for met output
   character(*),intent(in)  :: zCoord         ! Vertical coordinate type
   real(R8),intent(in)      :: dz             ! Vertical interval (m) for snd
   integer,intent(in)       :: im             ! if im > 0, output levels in m
   real(R8),intent(in)      :: topz           ! Max height (m) for snd
   logical,intent(in)       :: writeBufkit    ! Output in BUFKIT format
   logical,intent(in)       :: writeYPG2      ! Output in YPG2 format
   logical,intent(in)       :: writeREDI      ! Output in REDI format
   character(char_short),intent(in)  :: rnge  ! Range (Test Center) name

   integer(INT),intent(out) :: statusFlag     ! Status: 0=good, all other bad
   character(*),intent(out) :: statusInfo     ! Status information

!EOP

   !----- local -----
   integer(INT) :: i,j,k             ! loop indices
   integer(INT) :: nlat              ! Number of obs points
   integer(INT) :: ndims             ! number of dimensions in a field
   integer(INT) :: nx,ny,nz          ! dimensions of netCDF fields
   integer(INT) :: nVert             ! value of netCDF bottom_top dimension
   integer(INT) :: nVert_stag        ! value of netCDF bottom_top_stag dimension
   integer(INT) :: nWE_stag,nSN_stag ! value of netCDF west_east,south_north stag dimension
   integer(INT) :: kz                ! number of levels in a fixed dz interpolated snd
   real(R8)     :: wrf_interp        ! WRF output interpolated to obs point
   real(R8)     :: u_rot, v_rot      ! Rotated wind vectors
   real(R8)     :: speed10m,dir10m   ! Wind speed and dir at 10 m
   real(R8),allocatable               :: work4d(:,:,:,:) ! 4d work array
   real(R8),allocatable               :: work3d(:,:,:)   ! 3d work array
   real(R8),allocatable               :: work1d(:)       ! 1d work array
   real(R8), dimension(:),allocatable :: znw,znu         ! eta values, full, half levels
   real(R8), allocatable :: znWeight(:)     ! eta weights
   real(R8), allocatable :: U10(:),V10 (:)  ! Wind components at 10m
   real(R8), allocatable :: elev(:)         ! Terrain height (m)
   real(R8), allocatable :: tsk(:)          ! Skin temperature (K)
   real(R8), allocatable :: Psfc(:)         ! Surface pressure (mb)
   real(R8), allocatable :: dcapex(:)       ! DCAPE
   real(R8), allocatable :: t1(:)           ! T at lowest eta level (C)
   real(R8), allocatable :: p1(:)           ! Pressure at lowest eta level (mb)
   real(R8), allocatable :: q1(:)           ! Q at lowest eta level (kg/kg)
   real(R8), allocatable :: ght1(:)         ! Height at lowest eta level (m)
   real(R8), allocatable :: u1(:)           ! U at lowest eta level (m/s)
   real(R8), allocatable :: v1(:)           ! V at lowest eta level (m/s)
   real(R8), allocatable :: alifted(:)
   real(R8), allocatable :: akindex(:)
   real(R8), allocatable :: totals(:)
   real(R8), allocatable :: sweat(:)
   real(R8), allocatable :: cape(:)
   real(R8), allocatable :: cins(:)
   real(R8), allocatable :: swi(:)
   real(R8), allocatable :: prec_w(:)
   real(R8), allocatable :: plcl(:)
   real(R8), allocatable :: tlcl(:)
   real(R8), allocatable :: plfc(:)
   real(R8), allocatable :: pel(:)
   real(R8), allocatable :: brch(:)
   real(R8), allocatable :: geoExp(:,:)     ! Exponential height variable
   real(R8), dimension(:),allocatable :: P2m,T2m,Q2m,RH2m,td2m,rho2m,spd2m,dir2m  ! Fields at 2m
   real(R8), dimension(:,:),allocatable :: Pbase,Ppert,Zbase,Zpert ! Height dependent
   real(R8), dimension(:,:),allocatable :: Qmix,pertT,Uz,Vz,Wz_stg,Wz ! fields from WRF
   real(R8), dimension(:,:),allocatable :: Pz,geoZ,Tz,RHz,tdz,dirz,speedz,omg,cldfr,twbz,thetae ! Calculated
   real(R8), dimension(:),allocatable :: pzi,tzi,tdzi,uzi,vzi,thetae1d
   real(R8) :: cldfrh,cldfrm,cldfrl
   real(R8) :: u10m,v10m,u2m,v2m ! temp variables for calculating spd2m, dir2m
   integer(INT) :: kbase

   real(R8), dimension(:), allocatable :: Tint,Pint,Uint,Vint,Qint
   real(R8), dimension(:), allocatable :: rhoint,RHint,TDint,DIRint,SPDint
   real(R8), dimension(:), allocatable :: TWBint,THTEint,OMGint,CLDint

   real(R8) :: height, t_in_f, td_in_f, t2m_f, td2m_f
   integer :: iheight

   character(char_long)               :: path_and_file   ! path//filename

   integer(INT),parameter :: nfld_2m = 7               ! # of 2m fields to read in
   character(len=4)       :: field_2m(nfld_2m) = &     ! Names of fields to read
   &  (/'PSFC', 'T2  ', 'Q2  ', 'U10 ', 'V10 ', 'HGT ', 'TSK '/)

   integer(INT),parameter :: nfld_in=9                 ! # of fields to read in
   character(len=6)       :: field_in(nfld_in) = &     ! Names of fields to read
   &  (/'PH    ', 'PHB   ', 'PB    ', 'P     ',  &
   &    'T     ', 'QVAPOR', 'U     ', 'V     ','W     '/)

   !----- formats -----
   character(*),parameter :: subName = "(sounding_data)"
   character(*),parameter :: F00 = '(I5,F9.1,5F9.2)'
   character(*),parameter :: F01 = '(A13,A2,A14,A6,F5.1,A1,F5.1, &
   &                                 A12,F6.3,A1,F8.3,A1)'
   character(*),parameter :: F02 = '(I9,7F9.2)'
   character(*),parameter :: F03 = '(I9,F12.1,F9.1,I7,F7.1,F12.2,F9.1,F11.1)'
   character(*),parameter :: F04 = '(I7,F6.1,F8.3,F7.1,I4,F7.1,I6)'

   integer :: h_in_ft, imod
   integer :: lcount

  !print*,'zCoord = ',trim(zCoord)

   nlat = size(lat)
   !---------------------------------------------------------------------------
   !--- Get dimensions 
   !---------------------------------------------------------------------------
   path_and_file = trim(wrf_path_name)//"/"//trim(wrf_file_name)
   call get_dimension(path_and_file, "bottom_top"      , nVert)
   call get_dimension(path_and_file, "bottom_top_stag" , nVert_stag)
   call get_dimension(path_and_file, "south_north_stag", nSN_stag)
   call get_dimension(path_and_file, "west_east_stag"  , nWE_stag)

   !---------------------------------------------------------------------------
   !--- Read in the vertical "eta" coordinate values of both the full (w) and
   !--- half (mass) levels
   !---------------------------------------------------------------------------
   allocate(work4d(nVert_stag,1,1,1))
   call ncread_field4dG(path_and_file, 'ZNW', rfld=work4d, dim2i=nTimeInd)
   allocate(znw(nVert_stag)) 
   znw = work4d(:,1,1,1)
   deallocate(work4d)

   allocate(work4d(nVert,1,1,1))
   call ncread_field4dG(path_and_file, 'ZNU', rfld=work4d, dim2i=nTimeInd)
   allocate(znu(nVert)) 
   znu = work4d(:,1,1,1)
   deallocate(work4d)

   allocate(znWeight(nVert)) 
   do k=1,nVert
      znWeight(k)=(znu(k)-znw(k))/(znw(k+1)-znw(k))
   enddo

   !---------------------------------------------------------------------------
   !--- Allocate arrays for the sounding data.
   !---------------------------------------------------------------------------
   allocate(Pbase(nlat,nVert),Ppert(nlat,nVert), Zbase(nlat,nVert_stag))
   allocate(Qmix(nlat,nVert) ,pertT(nlat,nVert), Zpert(nlat,nVert_stag))
   allocate(Uz(nlat,nVert)   ,Vz(nlat,nVert))
   allocate(Wz_stg(nlat,nVert_stag))
   allocate(Wz(nlat,nVert))

   !---------------------------------------------------------------------------
   !--- Read in the (x,y,z) fields for sounding data.
   !---------------------------------------------------------------------------
   do j = 1,nfld_in          ! Loop through the vertical fields to read in

      !------------------------------------------------------------------------
      ! Get number of dimensions and size of nx, ny, nz
      !------------------------------------------------------------------------
      call ncread_varDimNum(trim(path_and_file),trim(field_in(j)),ndims)
      call ncread_varDimSizes(trim(path_and_file),trim(field_in(j)),nx,ny,nz)

      allocate(work4d(nx, ny, nz, 1))
      allocate(work3d(nWE,nSN,nz))  ! work3d in horiz. non-stagggered dimensions
      allocate(work1d(nz))

      call ncread_field4dG(trim(path_and_file), trim(field_in(j)), &
      &                    rfld=work4d, dim4i=nTimeInd)

      !------------------------------------------------------------------------
      ! Check for horizontally staggered fields, averaged to non-staggered
      !------------------------------------------------------------------------
      if ( nx == nWE_stag .or. ny == nSN_stag ) then 
         call unstagger(path_and_file, work4d(:,:,:,1), work3d)
      else
         work3d = work4d(:,:,:,1)
      endif

      do i = 1,nlat          ! Loop through the obs points, saving fields

         !---------------------------------------------------------------------
         !--- Horizontally interpolate each point in sounding to lat,lon
         !---------------------------------------------------------------------
         do k = 1,nz
            work1d(k) = four_point(work3d(:,:,k),nWE,nSN,x_obs(i),y_obs(i))
         enddo

         if (trim(field_in(j)) == 'PB'    ) Pbase(i,:) = work1d(:)
         if (trim(field_in(j)) == 'P'     ) Ppert(i,:) = work1d(:)
         if (trim(field_in(j)) == 'PHB'   ) Zbase(i,:) = work1d(:)
         if (trim(field_in(j)) == 'PH'    ) Zpert(i,:) = work1d(:)
         if (trim(field_in(j)) == 'QVAPOR')  Qmix(i,:) = work1d(:)
         if (trim(field_in(j)) == 'T'     ) pertT(i,:) = work1d(:)
         if (trim(field_in(j)) == 'U'     )    Uz(i,:) = work1d(:)
         if (trim(field_in(j)) == 'V'     )    Vz(i,:) = work1d(:)
         if (trim(field_in(j)) == 'W'     ) Wz_stg(i,:) = work1d(:)
           
      enddo
      deallocate(work4d)
      deallocate(work3d)
      deallocate(work1d)
   enddo                  ! nfld_in

   !---------------------------------------------------------------------
   !--- Calculate values for sounding
   !--- For the geopotential: the WRF vertical coordinate is mass based.
   !--- (1) Convert the geopotential to an exponential height variable,
   !---     exp(-z/H), where H is a scale height. 
   !--- (2) Linearly interpolate in WRF vert. coord. ("eta") from full
   !---     (w) levels to "mass levels", and then convert back to
   !---     geopotential height.
   !--- (3) Interpolate exp(-z/H) to "mass levels", convert to height
   !---------------------------------------------------------------------
   allocate(Pz(nlat,nVert),geoZ(nlat,nVert),Tz(nlat,nVert),RHz(nlat,nVert))
   allocate(tdz(nlat,nVert))
   allocate(dirz(nlat,nVert),speedz(nlat,nVert), geoExp(nlat,nVert_stag))
   allocate(twbz(nlat,nVert))
   allocate(thetae(nlat,nVert))
   allocate(omg(nlat,nVert))
   allocate(cldfr(nlat,nVert))
   allocate(dcapex(nlat))
   allocate(pzi(nVert))
   allocate(tzi(nVert))
   allocate(tdzi(nVert))
   allocate(uzi(nVert))
   allocate(vzi(nVert))
   allocate(thetae1d(nVert))
   allocate(t1(nlat))
   allocate(p1(nlat))
   allocate(q1(nlat))
   allocate(ght1(nlat))
   allocate(u1(nlat))
   allocate(v1(nlat))
   allocate(alifted(nlat))
   allocate(akindex(nlat))
   allocate(totals(nlat))
   allocate(sweat(nlat))
   allocate(cape(nlat))
   allocate(cins(nlat))
   allocate(swi(nlat))
   allocate(prec_w(nlat))
   allocate(plcl(nlat))
   allocate(tlcl(nlat))
   allocate(plfc(nlat))
   allocate(pel(nlat))

   do i = 1,nlat        
      Pz(i,:)   = (Pbase(i,:) + Ppert(i,:))*pa_to_mb
      pzi(:) = Pz(i,:)
      geoExp(i,:) = dexp(-(Zbase(i,:) + Zpert(i,:))/(g0*scale_ht))

      do k = 1,nVert
         geoZ(i,k) = znWeight(k)*geoExp(i,k+1) + (c1 - znWeight(k))*geoExp(i,k)
         geoZ(i,k) = -scale_ht*dlog(geoZ(i,k))
         Wz(i,k)   = znWeight(k)*Wz_stg(i,k+1) + (c1 - znWeight(k))*Wz_stg(i,k)
         Tz(i,k)   = (pertT(i,k)+T0)*((Pz(i,k)/P00)**(Rd/cp))     ! Convert potential temp
         RHz(i,k)  = rhcalc(Tz(i,k), Qmix(i,k), Pz(i,k))*c100     ! RH in %
         tdz(i,k)  = tdcalc(Tz(i,k), RHz(i,k))
         call rotate_wind (Uz(i,k), Vz(i,k), map_proj, stand_lon, lat(i), &
         &                 lon(i), truelat1, truelat2, u_rot, v_rot)
         call wind_dir_speed(u_rot, v_rot, dirz(i,k), speedz(i,k))
         Tz(i,k) = Tz(i,k) - 273.15
         tzi(k) = Tz(i,k)
         tdz(i,k) = tdz(i,k) - 273.15
         tdzi(k) = tdz(i,k)
         uzi(k) = -sin(dirz(i,k)/c180*pi)*speedz(i,k)
         vzi(k) = -cos(dirz(i,k)/c180*pi)*speedz(i,k)
         omg(i,k) = -g0*pzi(k)*100./(Rd*virtual(tzi(k)+273.15,Qmix(i,k)))*Wz(i,k)
      enddo

      call cal_dcape(pzi,tzi,tdzi,nVert,dcapex(i))
      if (writeBufkit) then
      call wetbulbcalc(pzi,Tz(i,:),Qmix(i,:),twbz(i,:),nVert)
      call cal_index(tzi,pzi,Qmix(i,:),uzi,vzi,nVert,alifted(i),akindex(i), &
                     totals(i),sweat(i),cape(i),cins(i),swi(i),prec_w(i), &
                     plcl(i),tlcl(i),plfc(i),pel(i),thetae1d)
      thetae(i,:) = thetae1d(:)
      call cloud(tzi,pzi,Qmix(i,:),nVert,cldfr(i,:),cldfrh,cldfrm,cldfrl,kbase)
      end if   ! end if (writeBufkit)

!     The following is required for calculating Richardson numbers

      t1(i) = Tzi(1)
      p1(i) = pzi(1)
      q1(i) = Qmix(i,1)
      ght1(i) = geoZ(i,1)
      u1(i) = uzi(1)
      v1(i) = vzi(1)
   enddo

!  deallocate(Pbase, Ppert, Zbase, Zpert, Qmix, pertT, Uz, Vz)
   deallocate(Pbase, Ppert, Zbase, Zpert, pertT,Wz,Wz_stg)

   !---------------------------------------------------------------------------
   !--- Read in fields and calculate values for 2m
   !---------------------------------------------------------------------------
   allocate(work4d(nWE, nSN, 1, 1))
   allocate(P2m(nlat),T2m(nlat),Q2m(nlat),RH2m(nlat))
   allocate(td2m(nlat),rho2m(nlat))
   allocate(U10(nlat),V10(nlat),spd2m(nlat),dir2m(nlat),brch(nlat))
   allocate(elev(nlat),Psfc(nlat),tsk(nlat))

   do j = 1,nfld_2m          ! Loop through the 2m fields to read in

      call ncread_field4dG(trim(path_and_file), trim(field_2m(j)), rfld=work4d, &
      &                    dim3i=nTimeInd)

      do i = 1,nlat          ! Loop through the obs points

         wrf_interp = four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))

         !---------------------------------------------------------------------
         !--- Adjust sfc pressure to 2m pressure (hydrostatic eqn)
         !---------------------------------------------------------------------
         if (trim(field_2m(j)) == 'PSFC') P2m(i) = wrf_interp 
         if (trim(field_2m(j)) == 'T2'  ) T2m(i) = wrf_interp   ! in K
         if (trim(field_2m(j)) == 'Q2'  ) Q2m(i) = wrf_interp
         if (trim(field_2m(j)) == 'U10' ) U10(i) = wrf_interp
         if (trim(field_2m(j)) == 'V10' ) V10(i) = wrf_interp
         if (trim(field_2m(j)) == 'HGT' ) elev(i)= wrf_interp
         if (trim(field_2m(j)) == 'TSK' ) tsk(i) = wrf_interp

      enddo      ! Loop through lat points
   enddo         ! Loop through 2m fields

   !---------------------------------------------------------------------
   !--- Subtract station elevation from geopotential height 
   !---------------------------------------------------------------------
   if (.not. writeYPG2) then  !! YPG2 format requires AMSL, others do AGL
   do i = 1,nlat        
      geoZ(i,:) = geoZ(i,:) - elev(i)
   enddo
   end if

   !---------------------------------------------------------------------
   !--- Calculate 2m values for sounding
   !---------------------------------------------------------------------
   do i = 1,nlat        
      Psfc(i) = P2m(i)*pa_to_mb
      P2m(i) = (P2m(i) - g0*rho0*c2)*pa_to_mb
      RH2m(i) = rhcalc(T2m(i), Q2m(i), P2m(i))*c100   ! RH in %

      td2m(i) = tdcalc(T2m(i),RH2m(i))

      rho2m(i) = P2m(i)*100./Rd/T2m(i) ! density in kg/m^3

      call rotate_wind (U10(i), V10(i), map_proj, stand_lon, lat(i), &
      &                 lon(i), truelat1, truelat2, u_rot, v_rot)
      call wind_dir_speed(u_rot, v_rot, dir10m, speed10m)
      T2m(i) = T2m(i) - 273.15
      td2m(i) = td2m(i) - 273.15

      !------------------------------------------------------------------
      ! Roughly linear extrapolate 10m wind speed and dir to 2m.
      !------------------------------------------------------------------
      u10m = -speed10m*sin(dir10m/c180*pi)
      v10m = -speed10m*cos(dir10m/c180*pi)
      if (writeYPG2) then
         u2m = u10m - (u1(i) - u10m)*c8/(geoZ(i,1) - elev(i) - c10)
         v2m = v10m - (v1(i) - v10m)*c8/(geoZ(i,1) - elev(i) - c10)
      else
         u2m = u10m - (u1(i) - u10m)*c8/(geoZ(i,1) - c10)
         v2m = v10m - (v1(i) - v10m)*c8/(geoZ(i,1) - c10)
      end if

      call wind_dir_speed(u2m, v2m, dir2m(i), spd2m(i))

      brch(i) = rib(t1(i),p1(i),q1(i),ght1(i),u1(i),v1(i),elev(i), &
                    Psfc(i),tsk(i))

   enddo

   deallocate(work4d)

   !---------------------------------------------------------------------
   !--- Write out results
   !---------------------------------------------------------------------

   if (dz > 0.) then

      do i = 1,nlat
!        if (.not. writeYPG2) print*,'geoZ(',i,',',nVert,') = ',geoZ(i,nVert)
         kz = dint(geoZ(i,nVert)/dz) 
!        if (.not. writeYPG2) print*,'dz = ',dz,' kz = ',kz
         allocate(Tint(kz))
         allocate(Pint(kz))
         allocate(Uint(kz))
         allocate(Vint(kz))
         allocate(Qint(kz))
         allocate(rhoint(kz))
         allocate(RHint(kz))
         allocate(TDint(kz))
         allocate(DIRint(kz))
         allocate(SPDint(kz))
         allocate(TWBint(kz))
         allocate(THTEint(kz))
         allocate(OMGint(kz))
         allocate(CLDint(kz))

         call hinterp(dz,kz,1,nVert,geoZ(i,:),Tz(i,:),Tint)
         call hinterp(dz,kz,2,nVert,geoZ(i,:),omg(i,:),OMGint)
         call hinterp(dz,kz,2,nVert,geoZ(i,:),Pz(i,:),Pint)
         call hinterp(dz,kz,2,nVert,geoz(i,:),Uz(i,:),Uint)
         call hinterp(dz,kz,2,nVert,geoz(i,:),Vz(i,:),Vint)
         call hinterp(dz,kz,2,nVert,geoz(i,:),Qmix(i,:),Qint)
         call hinterp(dz,kz,2,nVert,geoz(i,:),twbz(i,:),TWBint)
         call hinterp(dz,kz,2,nVert,geoz(i,:),thetae(i,:),THTEint)
         call hinterp(dz,kz,2,nVert,geoz(i,:),cldfr(i,:),CLDint)

         rhoint = Pint*100./Rd/(Tint+273.15)*1000. ! (g/m^3)

         do k = 1,kz
            RHint(k) = rhcalc(Tint(k)+273.15,Qint(k),Pint(k))*c100
            TDint(k) = tdcalc(Tint(k)+273.15,RHint(k))
            call rotate_wind (Uint(k),Vint(k),map_proj,stand_lon,lat(i), &
                              lon(i),truelat1,truelat2,u_rot,v_rot)
            call wind_dir_speed(u_rot, v_rot, DIRint(k),SPDint(k))
            SPDint(k) = SPDint(k)*ms2knots  ! convert m/s to knots
         end do

         if (.not. writeBufkit) then
            if (writeYPG2) then
               call writeYPG2_header(out_unit,station_name(i),lat(i),lon(i),elev(i),date_time)
               t2m_f = T2m(i)*1.8+32.
               td2m_f = td2m(i)*1.8+32
!              rho2m = Psfc(i)*100./Rd/(T2m(i)+273.15)*1000.
               write (out_unit,F03) nint(elev(i)*m2ft),Psfc(i),t2m_f, &
                      nint(RH2m(i)),td2m_f,rho2m(i)*1000.,dir2m(i), &
                      spd2m(i)*ms2knots 
            else if (writeREDI) then
               call writeREDI_header(out_unit,station_name(i),elev(i),rnge,date_time)
               write(out_unit,F04) 0,T2m(i),rho2m(i),Psfc(i), &
                     nint(dir2m(i)),spd2m(i),nint(RH2m(i))
            else
               if ( topz > 0.) then
                  lcount = 0
                  do k = 1,kz
                     if ( dz*k > topz ) exit
                     lcount = lcount + 1
                  end do
                  write (out_unit,'(I3,a9)') lcount, "NSNDS"
               else
                  write (out_unit,'(I3,a9)') kz, "NSNDS"
               end if
               write (out_unit,F01) date_time(1:13),'Z,', station_name(i), &
                 ' i,j=(',x_obs(i),',',y_obs(i),'),lat,long=(',lat(i),',', lon(i),')'
               write (out_unit,'(A25,I4)') ' SFALT,   SFPRES,   DCAPE'
               write (out_unit,'(I5,2F9.1)') idint(elev(i)+0.4_R8), Psfc(i),dcapex(i)

               if (im > 0) then
                  write (out_unit,'(A72)') '  AGL(m)     P(mb)     T(C)    RH(%)    TD(C) D(g/m^3)     WDD WSPD(kts)'
               else
                  write (out_unit,'(A72)') '  AGL(ft)    P(mb)     T(C)    RH(%)    TD(C) D(g/m^3)     WDD WSPD(kts)'
               end if
            end if ! end if (writeYPG2) for header

            do k = 1,kz
               if (im > 0) then
                  iheight = nint(dz*k)
               else
                  iheight = nint(dz*k*m2ft)
                  imod = mod(iheight,10)
                  if (imod > 8) iheight = iheight + (10-imod)
               end if

               if ( topz > 0. .and. dz*k > topz) exit

               if (writeYPG2) then
                  if(Pint(k) == missing_long) cycle
                  t_in_f = Tint(k)*1.8+32.
                  td_in_f = (TDint(k)-273.15)*1.8+32.
                  write(out_unit,F03), iheight,Pint(k),t_in_f,nint(RHint(k)), &
                     td_in_f,rhoint(k),DIRint(k),SPDint(k)
               else if (writeREDI) then
                  write(out_unit,F04) iheight,Tint(k),rhoint(k)*0.001,Pint(k), &
                     nint(DIRint(k)),SPDint(k)/ms2knots,nint(RHint(k))
               else
                  write(out_unit,F02), iheight,Pint(k),Tint(k),RHint(k), &
                     TDint(k)-273.15,rhoint(k),DIRint(k),SPDint(k)
               end if
            end do
         else   ! write in BUFKIT format
            write(out_unit,"('SNPARM = PRES;TMPC;TMWC;DWPC;THTE;DRCT;SKNT;OMEG;CFRL;HGHT')")
            write(out_unit,"('STNPRM = SHOW;LIFT;SWET;KINX;LCLP;PWAT;TOTL;CAPE;LCLT;CINS;EQLV;LFCT;BRCH',/)")
            write(out_unit,"('STID = ',a14,' STNM = ',a14, &
                  ' TIME = ',a2,a2,a2,'/',a2,a2)") station_name(i), &
                 station_name(i),date_time(3:4),date_time(6:7), &
                 date_time(9:10),date_time(12:13),date_time(15:16)
            write(out_unit,"('SLAT = ',f6.2,' SLON = ',f7.2,' SELV = ', &
                  f6.1)") lat(i),lon(i),elev(i)
            write(out_unit,"('STIM = 0',/)")
            write(out_unit,"('SHOW = ',f7.2,' LIFT = ',f7.2,' SWET = ', &
                  f7.2,' KINX = ',f7.2)") swi(i),alifted(i),sweat(i),akindex(i)
            write(out_unit,"('LCLP = ',f7.2,' PWAT = ',f7.2,' TOTL = ', &
                  f7.2,' CAPE = ',f7.2)") plcl(i),prec_w(i),totals(i),cape(i)
            write(out_unit,"('LCLT = ',f7.2,' CINS = ',f7.2,' EQLV = ', &
                  f7.2,' LFCT = ',f7.2)") tlcl(i),cins(i),pel(i),plfc(i)
            write(out_unit,"('BRCH = ',f7.2,/)") brch(i)
            write(out_unit,"('PRES TMPC TMWC DWPC THTE DRCT SKNT OMEG')")
            write(out_unit,"('CFRL HGHT')")

            do k = 1,kz
               if (im > 0) then
                  height = dz*k
               else
                  height = dz*k*m2ft
               end if

               if ( topz > 0. .and. dz*k > topz) exit

               write(out_unit,'(8f9.2)') Pint(k),Tint(k),TWBint(k), &
               TDint(k),THTEint(k),DIRint(k),SPDint(k)*ms2knots, &
               OMGint(k),CLDint(k),height
            end do

         end if ! end whether to write in BUFKIT format

         deallocate(Tint,Pint,Uint,Vint,Qint,rhoint,RHint,TDint,DIRint,SPDint)
         deallocate(TWBint,THTEint,OMGint,CLDint)

      end do

   else

      do i = 1,nlat
         if (.not. writeBufkit) then
!           write (out_unit,'(" ")')  ! removed as requested
            if ( topz > 0.) then
               lcount = 0
               do k = 1,nVert
                  lcount = lcount + 1
                  if (geoZ(i,k) > topz) exit
               end do
               write (out_unit,'(I3,a9)') lcount, "NSNDS"
            else
               write (out_unit,'(I3,a9)') nVert+1, "NSNDS"
            end if
            write (out_unit,F01) date_time(1:13),'Z,', station_name(i), &
            ' i,j=(',x_obs(i),',',y_obs(i),'),lat,long=(',lat(i),',', lon(i),')'
            write (out_unit,'(A25,I4)') ' SFALT,   SFPRES,   DCAPE'
            write (out_unit,'(I5,2F9.1)') idint(elev(i)+0.4_R8), Psfc(i),dcapex(i)
            write (out_unit,'(A60)') '  LINE  AGL(m)     T(C)    RH(%)   WSPD(m/s)  WDD    P(mb)  '
            write (out_unit,F00) 1, c2,  T2m(i), RH2m(i), spd2m(i), dir2m(i),  P2m(i)
            do k = 1,nVert
               if ( topz > 0. .and. geoZ(i,k) > topz ) exit
               write (out_unit,F00) k+1,   geoZ(i,k),  Tz(i,k), RHz(i,k), &
                     speedz(i,k),dirz(i,k),  Pz(i,k)
            enddo
         else   ! write in BUFKIT format
            write(out_unit,"('SNPARM = PRES;TMPC;TMWC;DWPC;THTE;DRCT;SKNT;OMEG;CFRL;HGHT')")
            write(out_unit,"('STNPRM = SHOW;LIFT;SWET;KINX;LCLP;PWAT;TOTL;CAPE;LCLT;CINS;EQLV;LFCT;BRCH',/)")
            write(out_unit,"('STID = ',a14,' STNM = ',a14, &
                  ' TIME = ',a2,a2,a2,'/',a2,a2)") station_name(i), &
                 station_name(i),date_time(3:4),date_time(6:7), &
                 date_time(9:10),date_time(12:13),date_time(15:16)
            write(out_unit,"('SLAT = ',f6.2,' SLON = ',f7.2,' SELV = ', &
                  f6.1)") lat(i),lon(i),elev(i)
            write(out_unit,"('STIM = 0',/)")
            write(out_unit,"('SHOW = ',f7.2,' LIFT = ',f7.2,' SWET = ', &
                  f7.2,' KINX = ',f7.2)") swi(i),alifted(i),sweat(i),akindex(i)
            write(out_unit,"('LCLP = ',f7.2,' PWAT = ',f7.2,' TOTL = ', &
                  f7.2,' CAPE = ',f7.2)") plcl(i),prec_w(i),totals(i),cape(i)
            write(out_unit,"('LCLT = ',f7.2,' CINS = ',f7.2,' EQLV = ', &
                  f7.2,' LFCT = ',f7.2)") tlcl(i),cins(i),pel(i),plfc(i)
            write(out_unit,"('BRCH = ',f7.2,/)") brch(i)
            write(out_unit,"('PRES TMPC TMWC DWPC THTE DRCT SKNT OMEG')")
            write(out_unit,"('CFRL HGHT')")

            do k = 1,nVert
               if ( topz > 0. .and. geoZ(i,k) > topz ) exit
               write(out_unit,'(8f9.2)') Pz(i,k),Tz(i,k),twbz(i,k), &
               tdz(i,k),thetae(i,k),dirz(i,k),speedz(i,k)*ms2knots, &
               omg(i,k),cldfr(i,k)*100.,geoZ(i,k)
            end do
         end if ! end whether to output in BUFKIT format
      enddo

   end if

   deallocate(Pz,geoZ,geoExp,Tz,RHz,tdz,dirz,speedz,omg,cldfr,twbz,thetae)
   deallocate(P2m,T2m,RH2m,td2m,rho2m,dir2m,spd2m,Q2m,U10,V10,brch)
   deallocate(elev,Psfc,tsk,znw,znu,znWeight)
   deallocate(pzi,tzi,tdzi,uzi,vzi,thetae1d)
   deallocate(t1,p1,q1,ght1,u1,v1)
   deallocate(alifted,akindex,totals,sweat,cape,cins,swi,prec_w,plcl,tlcl,plfc,pel)
   deallocate(dcapex)

   statusFlag = 0
   statusInfo = "Normal"

   return

end subroutine sounding_data
!
!
!
subroutine hinterp(dz,nz,itr,nOrig,zOrig,fieldOrig,arrayInterp)

  implicit none

  real(R8),intent(in)      :: dz             ! Vertical interval (m) for snd
  integer(INT),intent(in)  :: nz             ! number of levels interpolated
  integer(INT),intent(in)  :: itr            ! vertical interpolation type
  integer(INT),intent(in)  :: nOrig          ! number of vertical levels before
  real(R8), dimension(:)   :: zOrig          ! heights of original levels
  real(R8), dimension(:)   :: fieldOrig      ! field on original levels
  real(R8), dimension(:)   :: arrayInterp    ! field on interpolated levels

  real(R8), dimension(nz)  :: zNew,alz
  real(R8), dimension(nOrig)  :: d1,als
  real(R8), dimension(nz)  :: d2

  real(R8), parameter :: badvalue = -9999.0_R8

  integer(INT) :: k, kst, l
  real(R8) :: a, au, ad

 !print*,'nz = ',nz
  do k = 1,nz
     zNew(k) = dz *k
    !print*,'zNew(',k,') = ',zNew(k)
  end do

  kst = nOrig

  if (itr == 1) then
     do k = 1,nz
        alz(k) = zNew(k)
     end do
  else if(itr == 2) then
     do k = 1,nz
        alz(k) = exp(zNew(k)*0.001)
     end do
  end if

 !do k = 1,nz
 !   print*,'alz(',k,') = ',alz(k)
 !end do

  if (itr == 1) then
     do k = 1,kst
        a = zOrig(k)
        als(k) = a
        d1(k) = fieldOrig(k)
     end do  
  else if(itr == 2) then
     do k = 1,kst
        a = zOrig(k)
        als(k) = exp(a*0.001)
        d1(k) = fieldOrig(k)
     end do
  end if

 !do k = 1,kst
 !   print*,'als(',k,') = ',als(k)
 !end do

  Loop1: do k = 1,nz
 
    !print*,'alz(k),als(kst),als(1) = ',alz(k),als(kst),als(1)

     if (alz(k) > als(kst) .or. alz(k) < als(1)) then
        arrayInterp(k) = badvalue
     else
       !iflag = 0
        Loop2: do l = 2,kst
           if (alz(k) <= als(l) .and. alz(k) >= als(l-1)) then
              au = als(l) - alz(k)
              ad = alz(k) - als(l-1) 

             !print*,' k l = ',k,l
              d2(k) = (d1(l)*ad+d1(l-1)*au)/(als(l)-als(l-1))

              arrayInterp(k) = d2(k)
              exit Loop2
           end if
        end do Loop2
     end if

  end do Loop1

  return

  end subroutine hinterp

!=======================================================================

subroutine metcm(writeIAF,statusFlag,statusInfo)

   implicit none

   logical, intent(in)      :: writeIAF       ! whether METCM for ATEC or IAF
   integer(INT),intent(out) :: statusFlag     ! Status: 0=good, all other bad
   character(*),intent(out) :: statusInfo     ! Status information

!EOP

   !----- local -----
   integer(INT) :: i,j,k             ! loop indices
   integer(INT) :: nlat              ! Number of obs points
   integer(INT) :: ndims             ! number of dimensions in a field
   integer(INT) :: nx,ny,nz          ! dimensions of netCDF fields
   integer(INT) :: nVert             ! value of netCDF bottom_top dimension
   integer(INT) :: nVert_stag        ! value of netCDF bottom_top_stag dimension
   integer(INT) :: nWE_stag,nSN_stag ! value of netCDF west_east,south_north stag dimension
   integer(INT) :: kz                ! number of levels in a fixed dz interpolated snd
   real(R8)     :: wrf_interp        ! WRF output interpolated to obs point
   real(R8)     :: u_rot, v_rot      ! Rotated wind vectors
   real(R8)     :: speed10m,dir10m   ! Wind speed and dir at 10 m
   real(R8),allocatable               :: work4d(:,:,:,:) ! 4d work array
   real(R8),allocatable               :: work3d(:,:,:)   ! 3d work array
   real(R8),allocatable               :: work1d(:)       ! 1d work array
   real(R8), dimension(:),allocatable :: znw,znu         ! eta values, full, half levels
   real(R8), allocatable :: znWeight(:)     ! eta weights
   real(R8), allocatable :: U10(:),V10 (:)  ! Wind components at 10m
   real(R8), allocatable :: elev(:)         ! Terrain height (m)
   real(R8), allocatable :: u1(:)           ! U at lowest eta level (m/s)
   real(R8), allocatable :: v1(:)           ! V at lowest eta level (m/s)
   real(R8), allocatable :: geoExp(:,:)     ! Exponential height variable
   real(R8), dimension(:),allocatable :: P2m,T2m,spd2m,dir2m  ! Fields at 2m
   real(R8), dimension(:,:),allocatable :: Pbase,Ppert,Zbase,Zpert ! Height dependent
   real(R8), dimension(:,:),allocatable :: pertT,Uz,Vz ! fields from WRF
   real(R8), dimension(:,:),allocatable :: Pz,geoZ,dirz,speedz,Tz ! Calculated
   real(R8), dimension(:),allocatable :: uzi,vzi
   real(R8) :: cldfrh,cldfrm,cldfrl
   real(R8) :: u10m,v10m,u2m,v2m ! temp variables for calculating spd2m, dir2m
   integer(INT) :: kbase

   real(R8), dimension(:), allocatable :: Tint,Pint,Uint,Vint,Qint

   integer :: iheight

   character(char_long)               :: path_and_file   ! path//filename

   integer(INT),parameter :: nfld_2m = 7               ! # of 2m fields to read in
   character(len=4)       :: field_2m(nfld_2m) = &     ! Names of fields to read
   &  (/'PSFC', 'T2  ', 'Q2  ', 'U10 ', 'V10 ', 'HGT ', 'TSK '/)

   integer(INT),parameter :: nfld_in=9                 ! # of fields to read in
   character(len=6)       :: field_in(nfld_in) = &     ! Names of fields to read
   &  (/'PH    ', 'PHB   ', 'PB    ', 'P     ',  &
   &    'T     ', 'QVAPOR', 'U     ', 'V     ','W     '/)

   integer(INT) :: istatus

   !----- formats -----
   character(*),parameter :: subName = "(metcm)"
   character(*),parameter :: F00 = '(I5,F9.1,5F9.2)'
   character(*),parameter :: F01 = '(A13,A2,A14,A6,F5.1,A1,F5.1, &
   &                                 A12,F6.3,A1,F8.3,A1)'
   character(*),parameter :: F02 = '(I9,7F9.2)'
   character(*),parameter :: F03 = '(I9,F12.1,F9.1,I7,F7.1,F12.2,F9.1,F11.1)'
   character(*),parameter :: F04 = '(I7,F6.1,F8.3,F7.1,I4,F7.1,I6)'

   integer :: h_in_ft, imod
   integer :: lcount
   integer :: k_count,k_count2
   real(R8):: dz
   real(R8):: speedavg
   real(R8):: diravg
   real(R8):: this_height

   character(len=3) :: clon,clat
   character(len=5) :: audit_type = 'METCM'
   character :: earth_round_number = '3'
   character(len=6) :: audit_location
   character(len=2) :: cmonth,cdate,chour
   character :: cmin = '0', cvalidity = '1'
   character(len=4) :: cprs
   character(len=5) :: calt
   character(len=12) :: audit_meta
   character(len=28) :: outfile

   character(len=3) :: czone
   character(len=4) :: cwd, cws, ctemp
   character(len=5) :: cpr

   integer :: kzone


   TYPE Snd_record
     REAL(R8) :: pressure
     REAL(R8) :: temp
     REAL(R8) :: ws
     REAL(R8) :: wd
     REAL(R8) :: agl
   END TYPE Snd_record

   TYPE Level_node
      TYPE(Snd_record) :: data
      TYPE(Level_node), POINTER :: next
   END TYPE Level_node

   TYPE(Level_node), POINTER :: tail, head, ptr

   nlat = size(lat)

   cmonth=date_time(6:7)
   cdate=date_time(9:10)
   chour=date_time(12:13) 

   !---------------------------------------------------------------------------
   !--- Get dimensions 
   !---------------------------------------------------------------------------
   path_and_file = trim(wrf_path_name)//"/"//trim(wrf_file_name)
   call get_dimension(path_and_file, "bottom_top"      , nVert)
   call get_dimension(path_and_file, "bottom_top_stag" , nVert_stag)
   call get_dimension(path_and_file, "south_north_stag", nSN_stag)
   call get_dimension(path_and_file, "west_east_stag"  , nWE_stag)

   call get_dimension(path_and_file, "west_east_stag"  , nWE_stag)

   !---------------------------------------------------------------------------
   !--- Read in the vertical "eta" coordinate values of both the full (w) and
   !--- half (mass) levels
   !---------------------------------------------------------------------------
   allocate(work4d(nVert_stag,1,1,1))
   call ncread_field4dG(path_and_file, 'ZNW', rfld=work4d, dim2i=nTimeInd)
   allocate(znw(nVert_stag)) 
   znw = work4d(:,1,1,1)
   deallocate(work4d)

   allocate(work4d(nVert,1,1,1))
   call ncread_field4dG(path_and_file, 'ZNU', rfld=work4d, dim2i=nTimeInd)
   allocate(znu(nVert)) 
   znu = work4d(:,1,1,1)
   deallocate(work4d)

   allocate(znWeight(nVert)) 
   do k=1,nVert
      znWeight(k)=(znu(k)-znw(k))/(znw(k+1)-znw(k))
   enddo

   !---------------------------------------------------------------------------
   !--- Allocate arrays for the sounding data.
   !---------------------------------------------------------------------------
   allocate(Pbase(nlat,nVert),Ppert(nlat,nVert), Zbase(nlat,nVert_stag))
   allocate(pertT(nlat,nVert), Zpert(nlat,nVert_stag))
   allocate(Uz(nlat,nVert)   ,Vz(nlat,nVert))

   !---------------------------------------------------------------------------
   !--- Read in the (x,y,z) fields for sounding data.
   !---------------------------------------------------------------------------
   do j = 1,nfld_in          ! Loop through the vertical fields to read in

      !------------------------------------------------------------------------
      ! Get number of dimensions and size of nx, ny, nz
      !------------------------------------------------------------------------
      call ncread_varDimNum(trim(path_and_file),trim(field_in(j)),ndims)
      call ncread_varDimSizes(trim(path_and_file),trim(field_in(j)),nx,ny,nz)

      allocate(work4d(nx, ny, nz, 1))
      allocate(work3d(nWE,nSN,nz))  ! work3d in horiz. non-stagggered dimensions
      allocate(work1d(nz))

      call ncread_field4dG(trim(path_and_file), trim(field_in(j)), &
      &                    rfld=work4d, dim4i=nTimeInd)

      !------------------------------------------------------------------------
      ! Check for horizontally staggered fields, averaged to non-staggered
      !------------------------------------------------------------------------
      if ( nx == nWE_stag .or. ny == nSN_stag ) then 
         call unstagger(path_and_file, work4d(:,:,:,1), work3d)
      else
         work3d = work4d(:,:,:,1)
      endif

      do i = 1,nlat          ! Loop through the obs points, saving fields

         !---------------------------------------------------------------------
         !--- Horizontally interpolate each point in sounding to lat,lon
         !---------------------------------------------------------------------
         do k = 1,nz
            work1d(k) = four_point(work3d(:,:,k),nWE,nSN,x_obs(i),y_obs(i))
         enddo

         if (trim(field_in(j)) == 'PB'    ) Pbase(i,:) = work1d(:)
         if (trim(field_in(j)) == 'P'     ) Ppert(i,:) = work1d(:)
         if (trim(field_in(j)) == 'PHB'   ) Zbase(i,:) = work1d(:)
         if (trim(field_in(j)) == 'PH'    ) Zpert(i,:) = work1d(:)
         if (trim(field_in(j)) == 'T'     ) pertT(i,:) = work1d(:)
         if (trim(field_in(j)) == 'U'     )    Uz(i,:) = work1d(:)
         if (trim(field_in(j)) == 'V'     )    Vz(i,:) = work1d(:)

      enddo
      deallocate(work4d)
      deallocate(work3d)
      deallocate(work1d)
   enddo                  ! nfld_in

   !---------------------------------------------------------------------
   !--- Calculate values for sounding
   !--- For the geopotential: the WRF vertical coordinate is mass based.
   !--- (1) Convert the geopotential to an exponential height variable,
   !---     exp(-z/H), where H is a scale height. 
   !--- (2) Linearly interpolate in WRF vert. coord. ("eta") from full
   !---     (w) levels to "mass levels", and then convert back to
   !---     geopotential height.
   !--- (3) Interpolate exp(-z/H) to "mass levels", convert to height
   !---------------------------------------------------------------------
   allocate(Pz(nlat,nVert),geoZ(nlat,nVert),Tz(nlat,nVert))
   allocate(dirz(nlat,nVert),speedz(nlat,nVert), geoExp(nlat,nVert_stag))
   allocate(uzi(nVert))
   allocate(vzi(nVert))
   allocate(u1(nlat))
   allocate(v1(nlat))

   do i = 1,nlat        
      Pz(i,:)   = (Pbase(i,:) + Ppert(i,:))*pa_to_mb
      geoExp(i,:) = dexp(-(Zbase(i,:) + Zpert(i,:))/(g0*scale_ht))

      do k = 1,nVert
         geoZ(i,k) = znWeight(k)*geoExp(i,k+1) + (c1 - znWeight(k))*geoExp(i,k)
         geoZ(i,k) = -scale_ht*dlog(geoZ(i,k))
         Tz(i,k)   = (pertT(i,k)+T0)*((Pz(i,k)/P00)**(Rd/cp))     ! Convert potential temp
         call rotate_wind (Uz(i,k), Vz(i,k), map_proj, stand_lon, lat(i), &
         &                 lon(i), truelat1, truelat2, u_rot, v_rot)
         call wind_dir_speed(u_rot, v_rot, dirz(i,k), speedz(i,k))
         uzi(k) = -sin(dirz(i,k)/c180*pi)*speedz(i,k)
         vzi(k) = -cos(dirz(i,k)/c180*pi)*speedz(i,k)
      enddo
      u1(i) = uzi(1)
      v1(i) = vzi(1)
   enddo

!  deallocate(Pbase, Ppert, Zbase, Zpert, Qmix, pertT, Uz, Vz)
   deallocate(Pbase, Ppert, Zbase, Zpert, pertT)

   !---------------------------------------------------------------------------
   !--- Read in fields and calculate values for 2m
   !---------------------------------------------------------------------------
   allocate(work4d(nWE, nSN, 1, 1))
   allocate(P2m(nlat),T2m(nlat))
   allocate(U10(nlat),V10(nlat),spd2m(nlat),dir2m(nlat))
   allocate(elev(nlat))

   do j = 1,nfld_2m          ! Loop through the 2m fields to read in

      call ncread_field4dG(trim(path_and_file), trim(field_2m(j)), rfld=work4d, &
      &                    dim3i=nTimeInd)

      do i = 1,nlat          ! Loop through the obs points

         wrf_interp = four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))

         !---------------------------------------------------------------------
         !--- Adjust sfc pressure to 2m pressure (hydrostatic eqn)
         !---------------------------------------------------------------------
         if (trim(field_2m(j)) == 'PSFC') P2m(i) = wrf_interp 
         if (trim(field_2m(j)) == 'T2'  ) T2m(i) = wrf_interp   ! in K
         if (trim(field_2m(j)) == 'U10' ) U10(i) = wrf_interp
         if (trim(field_2m(j)) == 'V10' ) V10(i) = wrf_interp
         if (trim(field_2m(j)) == 'HGT' ) elev(i)= wrf_interp

      enddo      ! Loop through lat points
   enddo         ! Loop through 2m fields

   !---------------------------------------------------------------------
   !--- Subtract station elevation from geopotential height to get AGL (m)
   !---------------------------------------------------------------------
   do i = 1,nlat
      geoZ(i,:) = geoZ(i,:) - elev(i)
   enddo


   !---------------------------------------------------------------------
   !--- Calculate 2m values for sounding
   !---------------------------------------------------------------------
   do i = 1,nlat        
      P2m(i) = (P2m(i) - g0*rho0*c2)*pa_to_mb

      call rotate_wind (U10(i), V10(i), map_proj, stand_lon, lat(i), &
      &                 lon(i), truelat1, truelat2, u_rot, v_rot)
      call wind_dir_speed(u_rot, v_rot, dir10m, speed10m)

      !------------------------------------------------------------------
      ! Roughly linear extrapolate 10m wind speed and dir to 2m.
      !------------------------------------------------------------------
      u10m = -speed10m*sin(dir10m/c180*pi)
      v10m = -speed10m*cos(dir10m/c180*pi)
      u2m = u10m - (u1(i) - u10m)*c8/(geoZ(i,1) - elev(i) - c10)
      v2m = v10m - (v1(i) - v10m)*c8/(geoZ(i,1) - elev(i) - c10)

      call wind_dir_speed(u2m, v2m, dir2m(i), spd2m(i))

   enddo

   deallocate(work4d)

   do i = 1,nlat

      calt = "     "

      if (.not. associated(head)) then
         allocate(head,stat=istatus)
         tail => head
      else
         allocate(tail%next,stat=istatus)
         tail => tail%next
      endif

      nullify(tail%next)

      tail%data%pressure = P2m(i)         ! mb
      tail%data%temp = T2m(i)             ! K
      tail%data%ws = spd2m(i)*ms2knots    ! knots
      tail%data%wd = dir2m(i)*deg2mills*c10 ! mills (1/1000 radian) * 10
      tail%data%agl = 2.0_R8

      dz = 50.0_R8

      kz = idint(geoZ(i,nVert)/dz)

      allocate(Tint(kz))
      allocate(Pint(kz))
      allocate(Uint(kz))
      allocate(Vint(kz))

      call hinterp(dz,kz,1,nVert,geoZ(i,:),Tz(i,:),Tint)
      call hinterp(dz,kz,2,nVert,geoZ(i,:),Pz(i,:),Pint)
      call hinterp(dz,kz,2,nVert,geoz(i,:),Uz(i,:),Uint)
      call hinterp(dz,kz,2,nVert,geoz(i,:),Vz(i,:),Vint)

      do k = 1,kz
         call rotate_wind (Uint(k), Vint(k), map_proj, stand_lon, lat(i), &
                           lon(i), truelat1, truelat2, u_rot, v_rot)
         call wind_dir_speed(u_rot, v_rot, diravg, speedavg)

         this_height = dz*k

         if (this_height == 100.0_R8 .or. this_height == 350.0_R8) then
            
            allocate(tail%next,stat=istatus)
            tail => tail%next
            nullify(tail%next)

            tail%data%pressure = Pint(k)        ! mb
            tail%data%temp = Tint(k)            ! K
            tail%data%ws = speedavg*ms2knots    ! knots
            tail%data%wd = diravg*deg2mills*c10 ! mills (1/1000 radian) * 10
            tail%data%agl = this_height
         else if (this_height >= 750.0_R8 .and. this_height <= 4750.0_R8) then
            
            if (mod(idint(this_height),500) == 250) then
               allocate(tail%next,stat=istatus)
               tail => tail%next
               nullify(tail%next)

               tail%data%pressure = Pint(k)        ! mb
               tail%data%temp = Tint(k)            ! K
               tail%data%ws = speedavg*ms2knots    ! knots
               tail%data%wd = diravg*deg2mills*c10 ! mills (1/1000 radian) * 10
               tail%data%agl = this_height
            endif
         else if (this_height >= 5500.0_R8 .and. this_height <= 19500.0_R8) then
            if (mod(idint(this_height),1000) == 500) then

               allocate(tail%next,stat=istatus)
               tail => tail%next
               nullify(tail%next)

               tail%data%pressure = Pint(k)        ! mb
               tail%data%temp = Tint(k)            ! K
               tail%data%ws = speedavg*ms2knots    ! knots
               tail%data%wd = diravg*deg2mills*c10 ! mills (1/1000 radian)*10
               tail%data%agl = this_height

            endif
         else if (this_height >= 21000.0_R8) then
            if (mod(idint(this_height),2000) == 1000) then

               allocate(tail%next,stat=istatus)
               tail => tail%next
               nullify(tail%next)

               tail%data%pressure = Pint(k)        ! mb
               tail%data%temp = Tint(k)            ! K
               tail%data%ws = speedavg*ms2knots    ! knots
               tail%data%wd = diravg*deg2mills*c10 ! mills (1/1000 radian)*10
               tail%data%agl = this_height

            endif
         endif
      enddo

      ptr => head

      if (idint(zObsPt(i)) < 1000) then
         write(calt,'(i4)') idint(zObsPt(i))+1000
      else
         write(calt,'(i5)') idint(zObsPt(i))+10000
      endif

      if (writeIAF) then
         write(clon,'(i3)') idint(lon(i)*10)
         write(clat,'(i3)') idint(lat(i)*10)
         audit_location = clon//clat

         write(cprs,'(i4)') idint(P2m(i))+1000

         if (idint(zObsPt(i)) < 1000) then
            audit_meta = cdate//chour//cmin//cvalidity//calt(2:4)//cprs(2:4)
         else
            audit_meta = cdate//chour//cmin//cvalidity//calt(2:5)//cprs(2:4)
         endif

         outfile = audit_type//earth_round_number//audit_location// &
                   audit_meta//'.txt'
      else
         if (idint(zObsPt(i)) < 1000) then
            outfile = trim(station_name(i))//cmonth//cdate//chour//calt(2:4)// &
                      '.txt'
         else
            outfile = trim(station_name(i))//cmonth//cdate//chour//calt(2:5)// &
                      '.txt'
         endif
      endif

      open(99,file=trim(outfile),status='unknown')

      if (writeIAF) then
         write(99,'(a)') audit_type//earth_round_number//audit_location//audit_meta
         write(99,'(6x,a,2x,a,2x,a,2x,a)') 'Dir','Speed','Temp','Press'
         write(99,'(x,a,x,a,x,a,2x,a,5x,a)') 'Zone','Mils','Knots','K','mBar'
      endif

      kzone = 0
      do
         if(.not. associated(ptr)) exit

         write(czone,'(i3)') kzone + 100
         write(cwd,'(i4)') idint(ptr%data%wd) + 1000
         write(cws,'(i4)') idint(ptr%data%ws) + 1000
         write(ctemp,'(i4)') idint(ptr%data%temp)*10
         write(cpr,'(i5)') idint(ptr%data%pressure) + 10000

!        write(99,'(2x,a,2x,a,2x,a,4x,a,2x,a,3x,i8)') czone(2:3),cwd(2:4),cws(2:4),ctemp,cpr(2:5),idint(ptr%data%agl)
         write(99,'(2x,a,2x,a,2x,a,4x,a,2x,a)') czone(2:3),cwd(2:4),cws(2:4),ctemp,cpr(2:5)

         kzone = kzone + 1

         ptr => ptr%next
      enddo

      nullify(head)

      close(99)

      deallocate(Tint,Pint,Uint,Vint)

   enddo

   deallocate(Pz,geoZ,geoExp,Tz)
   deallocate(P2m,T2m,dir2m,spd2m,U10,V10)
   deallocate(elev,znw,znu,znWeight)
   deallocate(uzi,vzi)
   deallocate(u1,v1)

   statusFlag = 0
   statusInfo = "Normal"

   return

end subroutine metcm

!=======================================================================

end module WRF_sounding

!=======================================================================


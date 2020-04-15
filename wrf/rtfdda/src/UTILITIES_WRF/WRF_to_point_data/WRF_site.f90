!=======================================================================
! CVS: $Id: WRF_site.f90,v 1.17 2018/08/30 22:22:38 sheu Exp $
! CVS: $Source: /cvs/apps/netcdf_utils/src/WRF_to_point_data/WRF_site.f90,v $
! CVS: $Name:  $
!=======================================================================
!BOP ===================================================================
!
! !MODULE: WRF_site - Get WRF data at a point for tabular site info
!
! !DESCRIPTION:
!
! !REVISION HISTORY:
!
! 2005-Sep-9 - J. Schramm - first version
!
! !INTERFACE: ----------------------------------------------------------

module WRF_site

! !USES:

    use WRF_tools
    use WRF_kinds
    use WRF_ncread
    use WRF_constants
    use WRF_input
    use derived_fields
    use site_arrays

!EOP

   implicit none

   private    ! except

! !PUBLIC TYPES:

   ! none

! !PUBLIC MEMBER FUNCTIONS:

   public :: site_data   ! Get WRF surface data at a point
   public :: site_gbc_data

! !PUBLIC DATA MEMBERS:

   ! none

!EOP
!===============================================================================
contains
!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: site_data -- retrieve WRF surface data at a site
!
! !DESCRIPTION:  
!    Given WRF netCDF file, date/time character string, time index of field, 
!    number and location of points, site elevation, returns:
!
!---  (1) MSL Pressure (hPa)  
!---  (2) Temperature 2m (C)
!---  (3) Dewpoint Temperature (C)
!---  (4) Relative Humidity (%)
!---  (5) 10 m Wind direction (degrees)
!---  (6) 10 m Wind speed (m/s)
!---  (7) Cloud cover (fraction)
!---  (8) Precipitation (mm)
!---  (9) PBL Height (m)
!
! ASSUMPTIONS: 
!      1) Precip from previous hour is on the same grid as current precip
!
! !REVISION HISTORY:
!     2005-Sep-9 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine site_data(outputFiles, out_unit, output_type, writeBufkit, statusFlag, statusInfo)
  implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*),intent(in)  :: outputFiles    ! Single or multiple output files
   integer(INT),intent(in)  :: out_unit       ! Unit specifier for met output
   character(*),intent(in)  :: output_type    ! fcst, preli, or final
   logical, intent(in)      :: writeBufkit    ! output in BUFKIT format
   integer(INT),intent(out) :: statusFlag     ! Status: 0=good, all other bad
   character(*),intent(out) :: statusInfo     ! Status information

!EOP

   !----- local -----
   integer(INT) :: i,j               ! loop indices
   integer(INT) :: nzObs             ! number of height values
   integer(INT) :: nlat              ! number of obs points
   integer(INT) :: nTimes            ! value of netCDF Time dimension
   integer(INT) :: nTimePrev         ! Index of previous hour of time
   integer(INT) :: rCode             ! error code
   integer(INT) :: prev_fid          ! netCDF file ID for the previous hour
   real(R8)     :: wrf_interp        ! WRF output interpolated to obs point
   real(R8)     :: Tbar              ! Average temperature
   real(R8)     :: T2m_corr          ! 2 m temperature, height corrected
   real(R8)     :: u_rot, v_rot      ! Rotated wind vectors
   real(R8)     :: current_rain      ! Total precip for current hour
   real(R8)     :: prev_rainc        ! Cumulus precip for previous hour
   real(R8)     :: prev_rainnc       ! Cumulus precip for previous hour
   real(R8)     :: zCorrection       ! Correct for model and site elev difference
   real(R8)     :: alt_density       ! density altitude
   real(R8)     :: alt_pressure      ! pressure altitude
   integer(INT) :: precipFlag        ! =1 if no file or field found for hour -1
   character(len=4) :: cyear,chour   ! character year and hour of date_time
   character(len=2) :: cmonth,cday   ! character month and day of date_time
   real(R8),allocatable :: work4d(:,:,:,:)  ! 4d work array
   character(char_long) :: prev_file_name   ! File with hour-1 data
   character(char_date) :: prev_date_time   ! hour-1 date string
   character(char_long) :: path_and_file    ! WRF NetCDF path//filename
   character(char_long) :: prev_path_file   ! path//filename of hour-1 file
   character(char_long) :: out_filename     ! filename of appended site output

   integer(INT),parameter :: nfld_in= 18               ! # of fields to read in
   integer :: nfld_out = 9
   logical :: exists   ! Does precious WRF output file exist?
   logical :: there    ! site_fields.nml exist?

   real(R8) :: rho

   real(R8) :: pmsl_out, t2m_out, rh_out, td_out, dir_out, spd_out
   real(R8) :: precip_out,precip_con,prev_snow,snow_out
   real(R8) :: prev_sfroff,sfroff_out,prev_udroff,udroff_out,prev_evp,evp_out
   real(R8) :: cldfrh,cldfrm,cldfrl,sm_spd,wxtr,wxts

   integer :: ism,ism_dir, kbase
   integer :: isite_fields = 21  ! file unit number for site_fields.nml
   integer :: nWE_stag,nSN_stag,ssl_stag
   integer :: k

   integer :: count

   character(len=9)       :: field_in(nfld_in) = &     ! Names of fields to read
      (/'HGT      ', 'T2       ', 'PSFC     ', 'Q2       ',  &
        'PBLH     ', 'U10      ', 'V10      ', 'RAINC    ',  &     
        'RAINNC   ', 'SWDOWN   ', 'SNOW     ', 'SNOWC    ', 'SNOWH    ', &
        'TSK      ', 'SFROFF   ', 'UDROFF   ', 'POTEVP   ', 'WSPD10MAX'/)                                     

   namelist /model_fields/ terrain, t_2m, p_sfc, rh_2m, td_2m, wind_10m, rain, pbl, swdown, max_gust
   namelist /derived_fields/ p_msl, vis, alt_den, alt_pres

   !----- formats -----
   character(*),parameter :: subName = "(site_data)"
   character(*),parameter :: F00 =   "('(site_data) ',20a)"
   character(*),parameter :: F03 =   "('(site_data) ',2(a,f20.3))"

   character(*),parameter :: F05 =   "(a30,',',a3,',',f7.3,',',f8.3,',',f6.1,',',a4,',',a2,',',a2,',',a4,',',a5)"
! This format statement will write RH, wind dir, and PBL with no decimal point
! and wind speed with one decimal place.
!  character(*),parameter :: F04 =   '(a30,1x,a3,f8.3,f9.3,f7.1,1x,a4,1x,a2,1x,'//&
!        &        'a2,1x,a4,1x,a5,1x,f6.1,1x,f5.1,1x,f5.1,1x,i4,1x,'//&
!        &        'i4,1x,f4.1,1x,f4.0,1x,f7.3,1x,i6)'
!  character(*),parameter :: F04 =   '(a30,1x,a3,f7.3,f9.3,f7.1,1x,a4,1x,a2,1x,'//&
!        &        'a2,1x,a4,1x,a5,1x,f6.1,1x,f5.1,1x,f5.1,1x,f4.0,1x,'//&
!        &        'f4.0,1x,f3.0,1x,f4.0,1x,f7.3,1x,f6.0)'

   nlat = size(lat)
   nzObs = size(zObsPt)

   inquire(file='site_fields.nml',exist=there)

   if (there) then
      open(isite_fields,file='site_fields.nml')
      read(isite_fields,nml=model_fields)
      read(isite_fields,nml=derived_fields)
   end if

   if (nzObs /= nlat) then
      statusFlag = 1
      statusInfo = 'ERROR nlat not equal to nzObs, are all sites at same elevation?'
      return
   endif

   path_and_file = trim(wrf_path_name)//"/"//trim(wrf_file_name)
   !---------------------------------------------------------------------------
   !--- Get dimensions from netCDF file
   !---------------------------------------------------------------------------
   call get_dimension(path_and_file, "Time", nTimes)
   call get_dimension(path_and_file, "west_east_stag"  , nWE_stag)
   call get_dimension(path_and_file, "south_north_stag", nSN_stag)
   call get_dimension(path_and_file,"soil_layers_stag",ssl_stag)

   !---------------------------------------------------------------------------
   !--- Find file for hour - 1 precip data
   !---------------------------------------------------------------------------
   precipFlag = 0
   if (nTimes == 1) then
      !------------------------------------------------------------------------
      ! Previous hour is in another file
      !------------------------------------------------------------------------
      call getPrevDayHour(date_time, prev_date_time)
      call getPrevFn(wrf_file_name, date_time, prev_date_time, &
      &              prev_file_name, statusFlag, statusInfo)
      prev_path_file = trim(wrf_path_name)//"/"//trim(prev_file_name)
      
      inquire(file=prev_path_file, exist=exists)
      if (.not.exists) then
         write(lstdout,*) 'WARNING: Previous WRF file not found:', trim(prev_path_file)
         write(lstdout,*) 'WARNING: Setting hourly precip value to missing'
         precipFlag = 1
      else
         call getTimeIndex(trim(prev_path_file), prev_date_time, nTimePrev, &
         &                 statusFlag, statusInfo)
      endif

      if (statusFlag == 1) precipFlag = 1
   else 
      !------------------------------------------------------------------------
      ! Previous hour is in this file
      !------------------------------------------------------------------------
      nTimePrev = nTimeInd - 1
   endif

   !---------------------------------------------------------------------------
   !--- Read in the (x,y) fields for sfc site data.
   !---------------------------------------------------------------------------

   allocate(work4d(nWE, nSN, 1, 1))  

   call field_track(nlat,nWE,nSN,nBT,nfld_out)

   do j = 1,nfld_in          ! Loop through the fields to read in

      if(trim(field_in(j)) .eq. 'POTEVP' .and. (.not. writeBufkit)) cycle

      if ((imax_gust < 1) .and. (field_in(j)(1:6) == 'WSPD10')) cycle

      call ncread_field4dG(trim(path_and_file), trim(field_in(j)), &
      &                    rfld=work4d, dim3i=nTimeInd)

      do i = 1,nlat          ! Loop through obs points in the domain

         wrf_interp = four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))

         if ((trim(field_in(j)) == 'HGT') .and. terrain) terrain_hgt(i) = wrf_interp
         if ((trim(field_in(j)) == 'PSFC') .and. (ip_sfc > 0))  Psfc(i) = wrf_interp
         if ((trim(field_in(j)) == 'T2') .and. (it_2m > 0))  T2m(i) = wrf_interp 
         if ((trim(field_in(j)) == 'Q2') .and. (iq_2m > 0))  Q2m(i) = wrf_interp 
         if ((trim(field_in(j)) == 'U10') .and. (iu_10m > 0))  u10m(i) = wrf_interp
         if ((trim(field_in(j)) == 'V10') .and. (iv_10m > 0))  v10m(i) = wrf_interp
         if ((trim(field_in(j)) == 'RAINC') .and. (irain_c > 0)) cumulus_prec(i) = wrf_interp
         if ((trim(field_in(j)) == 'RAINNC') .and. (irain_nc > 0)) gdscale_prec(i) = wrf_interp
         if ((trim(field_in(j)) == 'PBLH') .and. (ipbl > 0))  pblHeight(i) = wrf_interp
         if ((field_in(j)(1:6) == 'WSPD10') .and. (imax_gust > 0))  wspd10max(i) = wrf_interp

         if ((trim(field_in(j)) == 'SWDOWN') .and. (iswdown > 0)) swDownFlux(i) = wrf_interp
         if ((trim(field_in(j)) == 'TSK') .and. (writeBufkit)) tsk(i) = wrf_interp
         if ((trim(field_in(j)) == 'SNOWC') .and. (writeBufkit)) snowc(i) = wrf_interp
         if ((trim(field_in(j)) == 'SNOW') .and. (rain)) snow(i) = wrf_interp
         if ((trim(field_in(j)) == 'SNOWH') .and. (rain)) snowh(i) = wrf_interp
         if ((trim(field_in(j)) == 'SFROFF') .and. (rain)) sfroff(i) = wrf_interp
         if ((trim(field_in(j)) == 'UDROFF') .and. (rain)) udroff(i) = wrf_interp
         if ((trim(field_in(j)) == 'POTEVP') .and. (rain)) evp(i) = wrf_interp

      enddo
   enddo

   if (vis) then

      call ncread_field4dG(trim(path_and_file), 'QCLOUD', &
                           rfld=clw, dim4i=nTimeInd)
      call ncread_field4dG(trim(path_and_file), 'QRAIN', &
                           rfld=rnw, dim4i=nTimeInd)
      do i = 1, nlat
         rho = Psfc(i)/Rd/T2m(i)
         call visibility(clw,rnw,x_obs(i),y_obs(i),Dx,rho,visMax(i),visMin(i))
      end do

   end if

   if (writeBufkit) then

      allocate(t4d(nWe, nSN, nBT, 1))
      allocate(q4d(nWe, nSN, nBT, 1))
      allocate(pb4d(nWe, nSN, nBT, 1))
      allocate(pt4d(nWe, nSN, nBT, 1))
      allocate(p4d(nWe, nSN, nBT, 1))
      allocate(u4d_stag(nWE_stag, nSN, nBT, 1))
      allocate(v4d_stag(nWE, nSN_stag, nBT, 1))
      allocate(u3d(nWe, nSN, nBT))
      allocate(v3d(nWe, nSN, nBT))
      allocate(tslb(nWe,nSN,sSL_stag,1))

      call ncread_field4dG(trim(path_and_file),'T', &
           rfld=t4d, dim4i=nTimeInd)
      call ncread_field4dG(trim(path_and_file),'QVAPOR', &
           rfld=q4d, dim4i=nTimeInd)
      call ncread_field4dG(trim(path_and_file),'P', &
           rfld=pt4d, dim4i=nTimeInd)
      call ncread_field4dG(trim(path_and_file),'PB', &
           rfld=pb4d, dim4i=nTimeInd)
      call ncread_field4dG(trim(path_and_file),'U', &
           rfld=u4d_stag, dim4i=nTimeInd)
      call ncread_field4dG(trim(path_and_file),'V', &
           rfld=v4d_stag, dim4i=nTimeInd)
      call ncread_field4dG(trim(path_and_file),'TSLB', &
           rfld=tslb, dim4i=nTimeInd)

      call unstagger(path_and_file, u4d_stag(:,:,:,1), u3d)
      call unstagger(path_and_file, v4d_stag(:,:,:,1), v3d)

      p4d = pt4d + pb4d

      allocate(tz(nlat,nBT))
      allocate(qz(nlat,nBT))
      allocate(pz(nlat,nBT))
      allocate(uz(nlat,nBT))
      allocate(vz(nlat,nBT))
      allocate(tslbz(nlat,nSL_stag))
      allocate(cldfr(nBT))
      allocate(cldfr2d(nlat,nBT))
      allocate(cldfrh1d(nlat))
      allocate(cldfrm1d(nlat))
      allocate(cldfrl1d(nlat))
      allocate(hlcy(nlat))
      allocate(sm_u(nlat))
      allocate(sm_v(nlat))
      allocate(cdbp(nlat))
      allocate(snowr(nlat))

      do i = 1, nlat
         do k = 1,nBT
            tz(i,k) = four_point(t4d(:,:,k,1),nWE,nSN,x_obs(i),y_obs(i))-273.15
            qz(i,k) = four_point(q4d(:,:,k,1),nWE,nSN,x_obs(i),y_obs(i))
            pz(i,k) = four_point(p4d(:,:,k,1),nWE,nSN,x_obs(i),y_obs(i))*.01
            uz(i,k) = four_point(u3d(:,:,k),nWE,nSN,x_obs(i),y_obs(i))
            vz(i,k) = four_point(v3d(:,:,k),nWE,nSN,x_obs(i),y_obs(i))
         end do
      end do

      do i = 1,nlat
         do k = 1,nSL_stag
            tslbz(i,k) = four_point(tslb(:,:,k,1),nWE,nSN,x_obs(i),y_obs(i))
         end do
      end do

      do i = 1,nlat
         call cloud(tz(i,:),pz(i,:),qz(i,:),nBT,cldfr,cldfrh,cldfrm,cldfrl,kbase)
         cldfr2d(i,:) = cldfr(:)
         cldfrh1d(i) = cldfrh
         cldfrm1d(i) = cldfrm
         cldfrl1d(i) = cldfrl

         ism = storm_motion(pz(i,:),uz(i,:),vz(i,:),nBT) ! in DDDSS (SS in knots)
         hlcy(i) = storm_helicity(pz(i,:),uz(i,:),vz(i,:),ism,nBT) ! storm relative helicity

         ism_dir = nint(ism/100.)
         sm_spd = mod(ism,100)/ms2knots
         sm_u(i) = -sm_spd*sin(ism_dir*pi/c180) ! U component of storm motion (m/s)
         sm_v(i) = -sm_spd*cos(ism_dir*pi/c180) ! V component of storm motion (m/s)

         if (kbase /= imissing) then
            cdbp(i) = pz(i,kbase)
         else
            cdbp(i) = missing_long
         end if

         if (snowh(i) == 0.) then
            snowr(i) = missing_long
         else
            snowr(i) = snow(i)/snowh(i)*0.001
         end if
      end do

   end if
   !---------------------------------------------------------------------
   !--- Get integer year, month, day, hour from date
   !---------------------------------------------------------------------
   cyear  = date_time(1:4)
   cmonth = date_time(6:7)
   cday = date_time(9:10)
   chour = date_time(12:13)//date_time(15:16)

   !---------------------------------------------------------------------
   !--- Calculate values for site sfc
   !---------------------------------------------------------------------

   do i = 1,nlat          ! Loop through the obs points

      count = 0

      if (trim(outputFiles) == "multiple") then
         out_filename = 'site'//station_id(i)//'.dat'
         open(out_unit, file=out_filename, status = 'unknown', position='append')
      endif

      if (zObsPt(i) == zmissing) then
         if (terrain) then
            zObsPt(i) = terrain_hgt(i)
         else
            zObsPt(i) = zmissing
         end if
      end if

      if (terrain) then
         zCorrection = terrain_hgt(i) - zObsPt(i)
      else
         zCorrection = 0.
      end if

      !---------------------------------------------------------------------
      !--- Calculate precip if file for previous hour has been found
      !---------------------------------------------------------------------
      if (precipFlag /= 1) then
         call ncread_open(trim(prev_path_file), prev_fid)

         current_rain = gdscale_prec(i) + cumulus_prec(i)
         !------------------------------------------------------------------
         ! Get precip from previous hourly file
         !------------------------------------------------------------------
         call ncread_field4dG(trim(prev_path_file), 'RAINC', &
                           rfld=work4d, dim3i=nTimePrev)

         prev_rainc = four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))

         call ncread_field4dG(trim(prev_path_file), 'RAINNC', &
                              rfld=work4d, dim3i=nTimePrev)

         prev_rainnc= four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))
         precip_out = current_rain - (prev_rainc + prev_rainnc)
         precip_con = cumulus_prec(i) - prev_rainc

         call ncread_field4dG(trim(prev_path_file), 'SNOW', &
                           rfld=work4d, dim3i=nTimePrev)

         prev_snow = four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))
         snow_out  = snow(i) - prev_snow

         call ncread_field4dG(trim(prev_path_file), 'SFROFF', &
                           rfld=work4d, dim3i=nTimePrev)

         prev_sfroff = four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))
         sfroff_out  = sfroff(i) - prev_sfroff

         call ncread_field4dG(trim(prev_path_file), 'UDROFF', &
                           rfld=work4d, dim3i=nTimePrev)

         prev_udroff = four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))
         udroff_out  = udroff(i) - prev_udroff

         if (writeBufkit) then
            call ncread_field4dG(trim(prev_path_file), 'POTEVP', &
                              rfld=work4d, dim3i=nTimePrev)

            prev_evp = four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))
            evp_out  = evp(i) - prev_evp
         endif
         
         rCode = ncread_close(prev_fid, subName)   

      else
         if (writeBufkit) then
            precip_out = missing_long
            precip_con = missing_long
            snow_out   = missing_long
            sfroff_out = missing_long
            udroff_out = missing_long
            evp_out    = missing_long
         else
            precip_out = missing
            precip_con = missing
            snow_out   = missing
            sfroff_out = missing
            udroff_out = missing
            evp_out    = missing
         end if

         if (precip_out > 0.) then
            wxtr = 1.
         else
            wxtr = 0.
         end if

         if (snow_out > 0.) then
            wxts = 1.
         else
            wxts = 0.
         end if
      endif

      !---------------------------------------------------------------------
      !--- Calculate MSLP, using mean temp between 2m and "sea level"
      !---------------------------------------------------------------------
      if (terrain) then
         T2m_corr = T2m(i) + LapseRate*terrain_hgt(i)
      else
         T2m_corr = T2m(i)
      end if

      Tbar  = half*(T2m(i) + T2m_corr)

      if (terrain) then
         pmsl_out = pa_to_mb*Psfc(i)*dexp(terrain_hgt(i)*g0/Rd/Tbar)
      else
         pmsl_out = pmissing
      end if

      !---------------------------------------------------------------------
      !--- Correct T 2m for elev. diff. between model and site
      !---------------------------------------------------------------------
      t2m_out = T2m(i) + LapseRate*zCorrection

      !---------------------------------------------------------------------
      !--- Calculate dew point and relative humidity
      !---------------------------------------------------------------------
      rh_out = rhcalc(T2m(i), Q2m(i), Psfc(i)*pa_to_mb)
      td_out = T2m(i)/(-Rv*dlog(rh_out)*T2m(i)/LHeat + c1)
      rh_out = rh_out*c100 ! Convert RH to %

      !---------------------------------------------------------------------
      !--- Rotate wind vector from model grid to Cartesian coordinates
      !---------------------------------------------------------------------
      call rotate_wind (u10m(i), v10m(i), map_proj, stand_lon, lat(i), &
                        lon(i), truelat1, truelat2, u_rot, v_rot)
      call wind_dir_speed(u_rot, v_rot, dir_out, spd_out)

      !---------------------------------------------------------------------
      !--- Calculate density altitude and pressure altitude for station
      !---------------------------------------------------------------------

      alt_density = Tsl/LapseRate*(1.-((Psfc(i)/Psl)/(t2m_out/Tsl))** &
                    (LapseRate*Rd/(g0-LapseRate*Rd)))*m2ft  ! in ft

      alt_pressure = Tsl/LapseRate*(1.-(Psfc(i)/Psl)**(LapseRate*Rd/g0))*m2ft

      if (.not. writeBufkit) then !***** output regular site format *****

         if (useEqual) then
            write (out_unit,"('stationName=',a30,',stationID=',a3,',lat=', &
                   f7.3,',lon=',f8.3,',alt=',f6.1,',year=',a4,',month=',a2, &
                   ',day=',a2,',hour=',a2,',outputType=',a5)",advance='no') &
                  station_name(i), station_id(i), lat(i), lon(i), zObsPt(i), &
                  cyear, cmonth, cday, chour, trim(adjustl(output_type))
         else
            write (out_unit,F05,advance='no') station_name(i), station_id(i), &
                   lat(i), lon(i), zObsPt(i), cyear, cmonth, cday, chour, &
                   trim(adjustl(output_type))
         end if

         if (p_msl) then
            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",mslPres=",f6.1)',advance='no') pmsl_out
               else
                  write (out_unit,'(",mslPres=",f6.1)') pmsl_out
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",f6.1)',advance='no') pmsl_out
               else
                  write (out_unit,'(",",f6.1)') pmsl_out
               end if
            end if
         end if

         if (p_sfc) then
            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",sfcPres=",f6.1)',advance='no') Psfc(i)*0.01 
               else
                  write (out_unit,'(",sfcPres=",f6.1)') Psfc(i)*0.01 
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",f6.1)',advance='no') Psfc(i)*0.01
               else
                  write (out_unit,'(",",f6.1)') Psfc(i)*0.01
               end if
            end if
         end if

         if (t_2m) then
            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",t2m=",f5.1)',advance='no') t2m_out-273.15
               else
                  write (out_unit,'(",t2m=",f5.1)') t2m_out-273.15
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",f5.1)',advance='no') t2m_out-273.15
               else
                  write (out_unit,'(",",f5.1)') t2m_out-273.15
               end if
            end if
         end if

         if (rh_2m .or. td_2m) then
            if (td_2m) then
               count = count + 1
               if (useEqual) then
                  if (count < nfld_out) then
                     write (out_unit,'(",td2m=",f5.1)',advance='no') td_out-273.15
                  else
                     write (out_unit,'(",td2m=",f5.1)') td_out-273.15
                  end if
               else 
                  if (count < nfld_out) then
                     write (out_unit,'(",",f5.1)',advance='no') td_out-273.15
                  else
                     write (out_unit,'(",",f5.1)') td_out-273.15
                  end if
               end if
            end if

            if (rh_2m) then
               count = count + 1
               if (useEqual) then
                  if (count < nfld_out) then
                     write (out_unit,'(",rh2m=",i4)',advance='no') nint(rh_out)
                  else
                     write (out_unit,'(",rh2m=",i4)') nint(rh_out)
                  end if
               else
                  if (count < nfld_out) then
                     write (out_unit,'(",",i4)',advance='no') nint(rh_out)
                  else
                     write (out_unit,'(",",i4)') nint(rh_out)
                  end if
               end if
            end if
         end if

         if (wind_10m) then
            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",wd10m=",i4)',advance='no') nint(dir_out)
               else
                  write (out_unit,'(",wd10m=",i4)') nint(dir_out)
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",i4)',advance='no') nint(dir_out)
               else
                  write (out_unit,'(",",i4)') nint(dir_out)
               end if
            end if

            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",ws10m=",f4.1)',advance='no') spd_out
               else
                  write (out_unit,'(",ws10m=",f4.1)') spd_out
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",f4.1)',advance='no') spd_out
               else
                  write (out_unit,'(",",f4.1)') spd_out
               end if
            end if
         end if

         if (rain) then
            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",prec=",f7.3)',advance='no') precip_out
               else
                  write (out_unit,'(",prec=",f7.3)') precip_out
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",f7.3)',advance='no') precip_out
               else
                  write (out_unit,'(",",f7.3)') precip_out
               end if
            end if
         end if

         if (pbl) then
            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",pblh=",i6)',advance='no') nint(pblHeight(i))
               else
                  write (out_unit,'(",pblh=",i6)') nint(pblHeight(i))
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",i6)',advance='no') nint(pblHeight(i))
               else
                  write (out_unit,'(",",i6)') nint(pblHeight(i))
               end if
            end if
         end if

         if (swdown) then
            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",sw=",f6.1)',advance='no') swDownFlux(i)
               else
                  write (out_unit,'(",sw=",f6.1)') swDownFlux(i)
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",f6.1)',advance='no') swDownFlux(i)
               else
                  write (out_unit,'(",",f6.1)') swDownFlux(i)
               end if
            end if
         end if

         if (vis) then
            count = count + 1
            if (useEqual) then
               write (out_unit,'(",visMax=",f6.1)',advance='no') visMax(i)
            else
               write (out_unit,'(",",f6.1)',advance='no') visMax(i)
            end if

            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",visMin=",f6.1)',advance='no') visMin(i)
               else
                  write (out_unit,'(",visMin=",f6.1)') visMin(i)
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",f6.1)',advance='no') visMin(i)
               else
                  write (out_unit,'(",",f6.1)') visMin(i)
               end if
            end if
         end if

         if (alt_den) then
            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",alt_density=",i6)',advance='no') nint(alt_density)
               else
                  write (out_unit,'(",alt_density=",i6)') nint(alt_density)
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",i6)',advance='no') nint(alt_density)
               else
                  write (out_unit,'(",",i6)') nint(alt_density)
               end if
            end if
         end if

         if (alt_pres) then
            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",alt_pres=",i6)',advance='no') nint(alt_pressure)
               else
                  write (out_unit,'(",alt_pres=",i6)') nint(alt_pressure)
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",i6)',advance='no') nint(alt_pressure)
               else
                  write (out_unit,'(",",i6)') nint(alt_pressure)
               end if
            end if
         end if

         if (max_gust) then
            count = count + 1
            if (useEqual) then
               if (count < nfld_out) then
                  write (out_unit,'(",max_gust=",f5.2)',advance='no') wspd10max(i)
               else
                  write (out_unit,'(",max_gust=",f5.2)') wspd10max(i)
               end if
            else
               if (count < nfld_out) then
                  write (out_unit,'(",",f5.2)',advance='no') wspd10max(i)
               else
                  write (out_unit,'(",",f5.2)') wspd10max(i)
               end if
            end if
         end if

      else    !***** writeBufkit is true, write BUFKIT format *****
         write(out_unit,"('STN YYMMDD/HHMM PMSL PRES SKTC STC1 SNFL WTNS')")
         write(out_unit,"('P01M C01M STC2 LCLD MCLD HCLD')")
         write(out_unit,"('SNRA UWND VWND R01M BFGR T2MS')")
         write(out_unit,"('Q2MS WXTS WXTP WXTZ WXTR USTM')")
         write(out_unit,"('VSTM HLCY SLLH WSYM CDBP VSBK')")
         write(out_unit,"('TD2M')")

         write(out_unit,"(a6,x,a2,a2,a2,'/',a4,6(x,f8.2))") station_name(i), &
               cyear(3:4),cmonth,cday,chour, &
               pmsl_out,Psfc(i)*0.01,tsk(i)-273.15,tslbz(i,1),snow_out, &
               -9999.
         write(out_unit,'(f8.2,6(x,f8.2))') precip_out,precip_con, &
               tslbz(i,2),cldfrl1d(i)*100.,cldfrm1d(i)*100.,cldfrh1d(i)*100.
         write(out_unit,'(f8.2,5(x,f8.2))') snowr(i),u_rot,v_rot,sfroff_out, &
               udroff_out,t2m_out-273.15
         write(out_unit,'(f8.2,5(x,f8.2))') Q2m(i)*1000.,wxts,-9999., &
               -9999.,wxtr,sm_u(i)
         write(out_unit,'(f8.2,5(x,f8.2))') sm_v(i),hlcy(i),evp_out, &
               -9999.,cdbp(i),visMax(i)
         write(out_unit,'(f8.2)') td_out-273.15

      end if  !***** end whether to output the regular or BUFKIT format *****

      if (trim(outputFiles) == "multiple") close(out_unit)

   enddo

   deallocate(work4d)  
   if (terrain) deallocate(terrain_hgt)
   if (ip_sfc > 0) deallocate(Psfc)
   if (it_2m > 0) deallocate(T2m)
   if (iq_2m > 0) deallocate(Q2m)
   if (wind_10m) deallocate(u10m,v10m)
   if (rain) deallocate(cumulus_prec, gdscale_prec)
   if (rain) deallocate(snow,snowh,sfroff,udroff,evp)
   if (pbl) deallocate(pblHeight)
   if (max_gust) deallocate(wspd10max)
   if (swdown) deallocate(swDownFlux)
   if (vis) deallocate(clw,rnw,visMax,visMin)
   if (writeBufkit) deallocate(t4d,q4d,pb4d,pt4d,p4d,u3d,v3d,tslb)
   if (writeBufkit) deallocate(u4d_stag,v4d_stag)
   if (writeBufkit) deallocate(tz,qz,pz,uz,vz,tslbz)
   if (writeBufkit) deallocate(cldfr,cldfr2d,cldfrh1d,cldfrm1d,cldfrl1d)
   if (writeBufkit) deallocate(hlcy,sm_u,sm_v,cdbp,snowr)

end subroutine site_data
!
!
!
subroutine site_gbc_data(outputFiles, out_unit, output_type, statusFlag, statusInfo)
   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*),intent(in)  :: outputFiles    ! Single or multiple output files
   integer(INT),intent(in)  :: out_unit       ! Unit specifier for met output
   character(*),intent(in)  :: output_type    ! fcst, preli, or final
   integer(INT),intent(out) :: statusFlag     ! Status: 0=good, all other bad
   character(*),intent(out) :: statusInfo     ! Status information

!EOP

   !----- local -----
   integer(INT) :: i,j               ! loop indices
   integer(INT) :: nzObs             ! number of height values
   integer(INT) :: nlat              ! number of obs points
   integer(INT) :: nTimes            ! value of netCDF Time dimension
   integer(INT) :: nTimePrev         ! Index of previous hour of time
   integer(INT) :: rCode             ! error code
   real(R8)     :: wrf_interp        ! WRF output interpolated to obs point
   real(R8)     :: Tbar              ! Average temperature
   real(R8)     :: T2m_corr          ! 2 m temperature, height corrected
   real(R8)     :: u_rot, v_rot      ! Rotated wind vectors
   real(R8)     :: current_rain      ! Total precip for current hour
   character(len=4) :: cyear,chour   ! character year and hour of date_time
   character(len=2) :: cmonth,cday   ! character month and day of date_time
   real(R8),allocatable :: work4d(:,:,:,:)  ! 4d work array
   character(char_long) :: out_filename     ! filename of appended site output

   integer(INT),parameter :: nfld_in= 5     ! # of fields to read in
   integer :: nfld_out = 9
   logical :: there    ! site_fields.nml exist?

   real(R8) :: rho

   real(R8) :: t2m_out, rh_out, td_out, dir_out, spd_out

   integer :: ism,ism_dir, kbase
   integer :: isite_fields = 21  ! file unit number for site_fields.nml

   character(len=6)       :: field_in(nfld_in) = &     ! Names of fields to read
      (/'T2    ', 'PSFC  ', 'Q2    ', 'U10   ', 'V10   '/)

   namelist /model_fields/ terrain, t_2m, p_sfc, rh_2m, td_2m, wind_10m, rain, pbl, swdown, max_gust

   !----- formats -----
   character(*),parameter :: subName = "(site_gbc_data)"
   character(*),parameter :: F00 =   "('(site_gbc_data) ',20a)"
   character(*),parameter :: F03 =   "('(site_gbc_data) ',2(a,f20.3))"

   character(*),parameter :: F05 =   "(a30,',',a3,',',f7.3,',',f8.3,',',f6.1,',',a4,',',a2,',',a2,',',a4,',',a5)"

   nlat = size(lat)
   nzObs = size(zObsPt)

   inquire(file='site_fields.nml',exist=there)

   if (there) then
      open(isite_fields,file='site_fields.nml')
      read(isite_fields,nml=model_fields)
   end if

   if (nzObs /= nlat) then
      statusFlag = 1
      statusInfo = 'ERROR nlat not equal to nzObs, are all sites at same elevation?'
      return
   endif

   path_and_file = trim(wrf_path_name)//"/"//trim(wrf_file_name)
   !---------------------------------------------------------------------------
   !--- Get dimensions from netCDF file
   !---------------------------------------------------------------------------
   call get_dimension(path_and_file, "Time", nTimes)

   allocate(work4d(nWE, nSN, 1, 1))  

   call field_track(nlat,nWE,nSN,nBT,nfld_out)

   do j = 1,nfld_in          ! Loop through the fields to read in

      call ncread_field4dG(trim(path_and_file), trim(field_in(j)), &
      &                    rfld=work4d, dim3i=nTimeInd)

      do i = 1,nlat          ! Loop through obs points in the domain

         wrf_interp = four_point(work4d(:,:,1,1),nWE,nSN,x_obs(i),y_obs(i))

         if ((trim(field_in(j)) == 'PSFC') .and. (ip_sfc > 0))  Psfc(i) = wrf_interp
         if ((trim(field_in(j)) == 'T2') .and. (it_2m > 0))  T2m(i) = wrf_interp 
         if ((trim(field_in(j)) == 'Q2') .and. (iq_2m > 0))  Q2m(i) = wrf_interp 
         if ((trim(field_in(j)) == 'U10') .and. (iu_10m > 0))  u10m(i) = wrf_interp
         if ((trim(field_in(j)) == 'V10') .and. (iv_10m > 0))  v10m(i) = wrf_interp

      enddo
   enddo

   !---------------------------------------------------------------------
   !--- Get integer year, month, day, hour from date
   !---------------------------------------------------------------------
   cyear  = date_time(1:4)
   cmonth = date_time(6:7)
   cday = date_time(9:10)
   chour = date_time(12:13)//date_time(15:16)

   !---------------------------------------------------------------------
   !--- Calculate values for site sfc
   !---------------------------------------------------------------------

   do i = 1,nlat          ! Loop through the obs points

      if (trim(outputFiles) == "multiple") then
         out_filename = 'site'//station_id(i)//'.dat'
         open(out_unit, file=out_filename, status = 'unknown', position='append')
      endif

      t2m_out = T2m(i)

      !---------------------------------------------------------------------
      !--- Calculate dew point and relative humidity
      !---------------------------------------------------------------------
      rh_out = rhcalc(T2m(i), Q2m(i), Psfc(i)*pa_to_mb)
      td_out = T2m(i)/(-Rv*dlog(rh_out)*T2m(i)/LHeat + c1)
      rh_out = rh_out*c100 ! Convert RH to %

      !---------------------------------------------------------------------
      !--- Rotate wind vector from model grid to Cartesian coordinates
      !---------------------------------------------------------------------
      call rotate_wind (u10m(i), v10m(i), map_proj, stand_lon, lat(i), &
                        lon(i), truelat1, truelat2, u_rot, v_rot)
      call wind_dir_speed(u_rot, v_rot, dir_out, spd_out)

      if (useEqual) then
         write (out_unit,"('stationName=',a30,',stationID=',a3,',lat=', &
                f7.3,',lon=',f8.3,',alt=',f6.1,',year=',a4,',month=',a2, &
                ',day=',a2,',hour=',a2,',outputType=',a5)",advance='no') &
               station_name(i), station_id(i), lat(i), lon(i), zObsPt(i), &
               cyear, cmonth, cday, chour, trim(adjustl(output_type))
      else
         write (out_unit,F05,advance='no') station_name(i), station_id(i), &
                lat(i), lon(i), zObsPt(i), cyear, cmonth, cday, chour, &
                trim(adjustl(output_type))
      end if

      if (useEqual) then
         write (out_unit,'(",t2m=",f5.1,",td2m=",f5.1,",rh2m=",i4, &
                ",wd10m=",i4,",ws10m=",f4.1)') &
         t2m_out-273.15,td_out-273.15,nint(rh_out),nint(dir_out),spd_out

      else
         write (out_unit,'(",",f5.1,",",f5.1,",",i4,",",i4,",",f4.1)') &
         t2m_out-273.15,td_out-273.15,nint(rh_out),nint(dir_out),spd_out
      end if

      if (trim(outputFiles) == "multiple") close(out_unit)

   enddo

   deallocate(work4d)  
   if (terrain) deallocate(terrain_hgt)
   if (ip_sfc > 0) deallocate(Psfc)
   if (it_2m > 0) deallocate(T2m)
   if (iq_2m > 0) deallocate(Q2m)
   if (wind_10m) deallocate(u10m,v10m)

end subroutine site_gbc_data

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: getPrevDayHour -- Construct a date string one hour previous
!
! !DESCRIPTION:  
!     Given WRF date string, return a date string with the previous hour.
!
! !REVISION HISTORY:
!     2005-Sep-16 - J. Schramm - first version
!     2005-Dec-29 - J. Schramm - works for month,year boundaries and leap years
!
! !INTERFACE: ------------------------------------------------------------------

subroutine getPrevDayHour(date_time, prev_date_time)

  implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*), intent(in)  :: date_time       ! Current CCYY-MM-DD_HH:MM:SS
   character(*), intent(out) :: prev_date_time  ! Date one hour before date_time

!EOP

   !----- local -----
   character(len=2) :: cmonth, cday, chour           ! Day, hour char string
   character(len=2) :: prev_month,prev_day,prev_hour ! Previous day and hour as char string
   character(len=4) :: cyear                         ! Year char string
   character(len=4) :: prev_year                     ! Previous year as char string
   integer          :: iyear, imonth, iday, ihour    ! Day and hour integers
   integer, dimension(12) :: ndays_in_mo = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   cyear  = date_time(1:4)
   cmonth = date_time(6:7)
   cday   = date_time(9:10)
   chour  = date_time(12:13)

   !---------------------------------------------------------------------------
   ! Convert characters to integers
   !---------------------------------------------------------------------------
   read (cyear, '(i4)') iyear
   read (cmonth,'(i4)') imonth
   read (cday,  '(i2)') iday
   read (chour, '(i2)') ihour

   !---------------------------------------------------------------------------
   ! Is it a leap year?
   !---------------------------------------------------------------------------
   if ((mod(iyear,4).eq.0.and.mod(iyear,100).ne.0).or.mod(iyear,400).eq.0) then
      ndays_in_mo(2) = 29
   endif

   !---------------------------------------------------------------------------
   !  Find previous hour, may have to change day, month or year
   !---------------------------------------------------------------------------
   if (ihour == 0) then               ! On a day boundary, change day too
      ihour = 23
      if (iday == 1) then             ! On a month boundary
         if (imonth == 1) then        ! On a year boundary
            iyear = iyear - 1
            imonth = 12
         else
            imonth = imonth - 1
         endif
         iday = ndays_in_mo(imonth)
      else
         iday = iday - 1              ! Change hour and day
      endif
   else                               ! Only change the hour
      ihour = ihour - 1
   endif
   !---------------------------------------------------------------------------
   ! Create previous hour and day as character string  
   !---------------------------------------------------------------------------
   call int_to_char(iyear, prev_year)
   call int_to_char(imonth,prev_month)
   call int_to_char(ihour, prev_hour)
   call int_to_char(iday , prev_day )
   
   prev_date_time = prev_year//'-'//prev_month//'-'//prev_day &
                    &//'_'//prev_hour//date_time(14:19)

end subroutine getPrevDayHour

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: int_to_char -- Convert an integer to character
!
! !DESCRIPTION:  
!     Convert integers <= 99999 to a character string
!
! !REVISION HISTORY:
!     2005-Sep-16 - J. Schramm - first version
!     2005-Dec-29 - J. Schramm - can convert up to 5 digit integers
!
! !INTERFACE: ------------------------------------------------------------------

subroutine int_to_char(int_in, prev_int)

  implicit none

! !INPUT/OUTPUT PARAMETERS:

   integer         ,intent(in)  :: int_in    ! Integer
   character(len=*),intent(out) :: prev_int  ! Integer as string

!EOP

  if (int_in >= 0 .and. int_in <= 9) then
     write (prev_int, "('0',i1)") int_in
  elseif (int_in >= 10 .and. int_in <= 99) then
     write (prev_int, '(i2)') int_in
  elseif (int_in >= 100 .and. int_in <= 999) then
     write (prev_int, '(i3)') int_in
  elseif (int_in >= 1000 .and. int_in <= 9999) then
     write (prev_int, '(i4)') int_in
  elseif (int_in >= 10000 .and. int_in <= 99999) then
     write (prev_int, '(i5)') int_in
  else
     write (6,*) 'int_to_char ERROR: integer out of range'
  endif

end subroutine int_to_char

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: getPrevFn -- Construct name of previous WRF file name
!
! !DESCRIPTION:  
!     Using filename of current day and previous date string, construct file
!     name of previous hourly file
!
! !REVISION HISTORY:
!     2005-Sep-16 - J. Schramm - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine getPrevFn(file_name, date_time, prev_date_time, prev_file_name, &
                     & statusFlag, statusInfo)

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*),intent(in) :: file_name       ! File with current data
   character(*),intent(in) :: date_time       ! Current date/time data
   character(*),intent(in) :: prev_date_time  ! Date string for previous hour
   character(*),intent(out):: prev_file_name  ! File with hour-1 data
   integer(INT),intent(out):: statusFlag      ! Status: 0=good, all other bad
   character(*),intent(out):: statusInfo      ! Status information

!EOP

   !----- local -----
   integer(INT)         :: i           ! loop index
   character(char_long) :: fn          ! WRF NetCDF file name, no spaces
   character(char_date) :: CdateTime   ! Current CCYY-MM-DD_HH:MM:SS, no spaces
   character(char_date) :: PdateTime   ! Previous CCYY-MM-DD_HH:MM:SS, no spaces
   integer(int)         :: fn_length   ! location of beginning of date string
   integer(int)         :: end_prefix  ! location of beginning of date string
   integer(int)         :: beg_suffix  ! location of end       of date string
   character(char_short):: fn_prefix   ! file name before date string
   character(char_short):: fn_suffix   ! file name after  date string


   !----- formats -----
   character(*),parameter :: subName = "(getPrevFn)"
   character(*),parameter :: F00 =   "('(getPrevFn) ',256a)"

   statusFlag = 0
   !-------------------------------------------------------------------
   ! Find the input file name prefix and suffix to create new file name
   !-------------------------------------------------------------------
   fn = trim(adjustl(file_name))
   CdateTime = trim(adjustl(date_time))
   PdateTime = trim(adjustl(prev_date_time))

   fn_length = len(fn)
   end_prefix = index(fn, CdateTime)

   if (end_prefix == 0) then
      statusFlag = 1
      statusInfo =  'ERROR:  dateTime did not occur as a substring in wrf_file_name'
   else
      fn_prefix = fn(1:end_prefix-1)
   endif
   beg_suffix = end_prefix + len(CdateTime)

   fn_suffix = fn(beg_suffix:fn_length)
   prev_file_name = fn_prefix(1:end_prefix-1)//PdateTime//fn_suffix
   write (lstdout,F00) trim(prev_file_name)

end subroutine getPrevFn

!=======================================================================

end module WRF_site

!=======================================================================


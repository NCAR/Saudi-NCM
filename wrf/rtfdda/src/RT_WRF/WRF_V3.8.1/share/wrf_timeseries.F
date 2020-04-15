!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This routine prints out the current value of variables at all specified
!   time series locations that are within the current patch.
!
! Michael G. Duda -- 25 August 2005
!
! --------- modifications by Will Cheng ----------------
!           2009-05-20
!
! This subroutine is specifically hardwired for Xcel realtime forecast 
! so that d01 time series are ignored. To output d01 time series, go
! to the line after
!
! 'CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )'
!
! and comment out
!
! 'if ( grid%id == 1 ) return'
!
! 1) output time series every 15-min at i,j points closest to 
!    specified lat/lon locations.
!
!    Note: if you want to change the time interval of output, go
!    to 'SUBROUTINE calc_ts' and change 
!    'REAL, PARAMETER :: xmin_out_interval'. The parameter is in minutes.
!
! 2) output the following fields for the first 36 levels (hardwired for ARW core):
!
! a) geopotential height (m):          grid%ts_z${k}l(n,i)                  
! b) pressure (Pa):                    grid%ts_p${k}l(n,i)
! c) temperature (deg C):              grid%ts_t${k}l(n,i)
! d) Earth-relative u-wind (m/s):      grid%ts_u${k}l(n,i)
! e) Earth-relative v-wind (m/s):      grid%ts_v${k}l(n,i)
! f) water vapor mixing ratio (g/kg):  grid%ts_q${k}l(n,i)*1000,   
!
! where ${k} = ( 1, 2, 3, ...., 36 ) starting from bottom up, it refers to
! the vertical index number
!
! 'n' is the time buffer index. Once it has reached its maximum 
! ('ts_buf_size' specified in 'namelist.input'), it will
! write the time series into the file. 
!
! 'i' refers to the station index
!
! 3) The output file prefix has been modified from the original, so that
!    the i, j points are part of the file name, e.g., 
!
!    zTWBU.d03.i0140.j0043.TS
!
!    TWBU is the station name specified in tstlist. The i, j points are
!    140 and 43, respectively.
!
! 4) The header is the same as before, but it is in sequential binary. 
!    Useless things like brackets and commas are deleted.
!
! I) first line is a header, it has the following info. 	
!
!  a) station name
!  b) grid number
!  c) point number in tslist (can ignore)
!  d) file prefix (in the form of z+4 letter station code, e.g., zCEDC)
!  e) latitude specified in tslist (user specified latitude for extraction)
!  g) longitude specified in tslist (user specified longitude for extraction)
!  g) model i-point
!  i) model j-point
!  l) model latitude corresponding to i,j point (this is the one used in time series)
!  n) model longitude corresponding to i,j point (this is the one used in time series)
!  p) model elevation (meters)
!
! II) the rest of the file contains the model output itself. Each line consists
! of one time record (at 15 min intervals), starting at 15-min model time
!
!  a) grid id (integer)
!  b) Julian year of cold start (integer)
!  c) Julian day of cold start  (integer)
!  d) UTC hour of cold start (real number)
!  e) model hour relative to cold start time (real number)
!  - note: to get the proper forecast time with respect to the current forecast cycle 
!    (not the cold start time), you need to compute the time difference between
!    the current forecast cycle time and the cold start time, then subtract this
!    offset from the model hour
!  f) point number in tslist (integer) (can ignore)
!  g) model i-point (integer)
!  h) model j-point (integer)
!  i) 2-m temperature (deg C) (integer)
!  j) surface pressure (hPa) (real number)
!
!  k) first level height (meters)
!  l) first level pressure (Pa)
!  m) first level temperature (deg C)
!  n) first level Earth-relative u-wind (m/s)
!  o) first level Earth-relative v-wind (m/s)
!  p) first level water vapor mixing ratio (g/kg)
!
!   .
!   .
!   .
!
!   36th level height (meters)
!   36th level pressure (Pa)
!   36th level temperature (deg C)
!   36th level Earth-relative u-wind (m/s)
!   36th level Earth-relative v-wind (m/s)
!   36th level water vapor mixing ratio (g/kg)
!
! When reading the file, just make sure that you are reading the binary
! in Big Endian format.
!<======================================================
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE calc_ts_locations( grid )

   USE module_domain
   USE module_configure
   USE module_dm
   USE module_llxy

   IMPLICIT NONE

   ! Arguments
   TYPE (domain), INTENT(INOUT) :: grid

   ! Externals
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER, EXTERNAL :: get_unused_unit

   ! Local variables
   INTEGER :: ntsloc_temp
   INTEGER :: i, k, iunit
   REAL :: ts_rx, ts_ry, ts_xlat, ts_xlong, ts_hgt
   REAL :: known_lat, known_lon
   CHARACTER (LEN=132) :: message
   TYPE (PROJ_INFO) :: ts_proj
   TYPE (grid_config_rec_type) :: config_flags

   INTEGER :: ids, ide, jds, jde, kds, kde,        &
              ims, ime, jms, jme, kms, kme,        &
              ips, ipe, jps, jpe, kps, kpe,        &
              imsx, imex, jmsx, jmex, kmsx, kmex,  &
              ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
              imsy, imey, jmsy, jmey, kmsy, kmey,  &
              ipsy, ipey, jpsy, jpey, kpsy, kpey

   ! ======= added by WC: 2009-05-16 =========
   ! new filename with I, J index
   CHARACTER (LEN=200) :: new_ts_filename
   CHARACTER (LEN=4) :: prefix_ts_iloc, prefix_ts_jloc

   ! integer to identify the domain number from station name
   INTEGER :: igrid_from_station
   ! =========================================

   IF ( grid%ntsloc .LE. 0 ) RETURN

#if ((EM_CORE == 1) && (DA_CORE != 1))
   IF ( grid%dfi_stage == DFI_FST ) THEN
#endif
      CALL get_ijk_from_grid ( grid ,                               &
                               ids, ide, jds, jde, kds, kde,        &
                               ims, ime, jms, jme, kms, kme,        &
                               ips, ipe, jps, jpe, kps, kpe,        &
                               imsx, imex, jmsx, jmex, kmsx, kmex,  &
                               ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                               imsy, imey, jmsy, jmey, kmsy, kmey,  &
                               ipsy, ipey, jpsy, jpey, kpsy, kpey )
   
      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
  
      ! <======= added by WC: 2009-05-20 ===========
      ! This is specifically hardwired for Xcel realtime forecast
      ! do not output time series for domain 1
      if ( grid%id == 1 ) return
      ! ===========================================> 
      ! Set up map transformation structure
      CALL map_init(ts_proj)
   
      IF (ips <= 1 .AND. 1 <= ipe .AND. &
          jps <= 1 .AND. 1 <= jpe) THEN
#if(NMM_CORE==1)
         known_lat = grid%hlat(1,1)
         known_lon = grid%hlon(1,1)
#else
         known_lat = grid%xlat(1,1)
         known_lon = grid%xlong(1,1)
#endif
      ELSE
         known_lat = 9999.
         known_lon = 9999.
      END IF
      known_lat = wrf_dm_min_real(known_lat)
      known_lon = wrf_dm_min_real(known_lon)
   
      ! Mercator
      IF (config_flags%map_proj == PROJ_MERC) THEN
         CALL map_set(PROJ_MERC, ts_proj,               &
                      truelat1 = config_flags%truelat1, &
                      lat1     = known_lat,             &
                      lon1     = known_lon,             &
                      knowni   = 1.,                    &
                      knownj   = 1.,                    &
                      dx       = config_flags%dx)
   
      ! Lambert conformal
      ELSE IF (config_flags%map_proj == PROJ_LC) THEN
      CALL map_set(PROJ_LC, ts_proj,                  &
                      truelat1 = config_flags%truelat1,  &
                      truelat2 = config_flags%truelat2,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   
      ! Polar stereographic
      ELSE IF (config_flags%map_proj == PROJ_PS) THEN
         CALL map_set(PROJ_PS, ts_proj,                  &
                      truelat1 = config_flags%truelat1,  &
                      stdlon   = config_flags%stand_lon, &
                      lat1     = known_lat,              &
                      lon1     = known_lon,              &
                      knowni   = 1.,                     &
                      knownj   = 1.,                     &
                      dx       = config_flags%dx)
   
      ! Cassini (global ARW)
      ELSE IF (config_flags%map_proj == PROJ_CASSINI) THEN
         CALL map_set(PROJ_CASSINI, ts_proj,                            &
                      latinc   = grid%dy*360.0/(2.0*EARTH_RADIUS_M*PI), &
                      loninc   = grid%dx*360.0/(2.0*EARTH_RADIUS_M*PI), & 
                      lat1     = known_lat,                             &
                      lon1     = known_lon,                             &
! We still need to get POLE_LAT and POLE_LON metadata variables before
!   this will work for rotated poles.
                      lat0     = 90.0,                                  &
                      lon0     = 0.0,                                   &
                      knowni   = 1.,                                    &
                      knownj   = 1.,                                    &
                      stdlon   = config_flags%stand_lon)

      ! Rotated latitude-longitude
      ELSE IF (config_flags%map_proj == PROJ_ROTLL) THEN
         CALL map_set(PROJ_ROTLL, ts_proj,                      &
! I have no idea how this should work for NMM nested domains
                      ixdim    = grid%e_we-1,                   &
                      jydim    = grid%e_sn-1,                   &
                      phi      = real(grid%e_sn-2)*grid%dy/2.0, &
                      lambda   = real(grid%e_we-2)*grid%dx,     &
                      lat1     = config_flags%cen_lat,          &
                      lon1     = config_flags%cen_lon,          &
                      latinc   = grid%dy,                       &
                      loninc   = grid%dx,                       &
                      stagger  = HH)
   
      END IF
   
      ! Determine time series locations for domain
      IF (.NOT. grid%have_calculated_tslocs) THEN
         grid%have_calculated_tslocs = .TRUE.
         WRITE(message, '(A43,I3)') 'Computing time series locations for domain ', grid%id
         CALL wrf_message(message)
   
         ntsloc_temp = 0
         DO k=1,grid%ntsloc

            CALL latlon_to_ij(ts_proj, grid%lattsloc(k), grid%lontsloc(k), ts_rx, ts_ry)

            ntsloc_temp = ntsloc_temp + 1
            grid%itsloc(ntsloc_temp) = NINT(ts_rx)
            grid%jtsloc(ntsloc_temp) = NINT(ts_ry)
            grid%id_tsloc(ntsloc_temp) = k
   
            ! Is point outside of domain (or on the edge of domain)?
            IF (grid%itsloc(ntsloc_temp) < ids .OR. grid%itsloc(ntsloc_temp) > ide .OR. &
                grid%jtsloc(ntsloc_temp) < jds .OR. grid%jtsloc(ntsloc_temp) > jde) THEN
               ntsloc_temp = ntsloc_temp - 1
   
            END IF
   
         END DO
   
         grid%next_ts_time = 1
   
         grid%ntsloc_domain = ntsloc_temp
   
         DO k=1,grid%ntsloc_domain
   
            ! If location is outside of patch, we need to get lat/lon of TS grid cell from another patch
            IF (grid%itsloc(k) < ips .OR. grid%itsloc(k) > ipe .OR. &
                grid%jtsloc(k) < jps .OR. grid%jtsloc(k) > jpe) THEN
               ts_xlat  = 1.E30
               ts_xlong = 1.E30
               ts_hgt   = 1.E30
            ELSE
#if(NMM_CORE==1)
               ts_xlat  = grid%hlat(grid%itsloc(k),grid%jtsloc(k))
               ts_xlong = grid%hlon(grid%itsloc(k),grid%jtsloc(k))
#else
               ts_xlat  = grid%xlat(grid%itsloc(k),grid%jtsloc(k))
               ts_xlong = grid%xlong(grid%itsloc(k),grid%jtsloc(k))
#endif
#if (EM_CORE == 1)
               ts_hgt   = grid%ht(grid%itsloc(k),grid%jtsloc(k))
#endif
            END IF
#if DM_PARALLEL
            ts_xlat  = wrf_dm_min_real(ts_xlat)
            ts_xlong = wrf_dm_min_real(ts_xlong)
            ts_hgt   = wrf_dm_min_real(ts_hgt)
#endif
   
            IF ( wrf_dm_on_monitor() ) THEN

               iunit = get_unused_unit()
               IF ( iunit <= 0 ) THEN
                  CALL wrf_error_fatal('Error in calc_ts_locations: could not find a free Fortran unit.')
               END IF

               WRITE(grid%ts_filename(k),'(A)') TRIM(grid%nametsloc(grid%id_tsloc(k)))//'.d00.TS'
               i = LEN_TRIM(grid%ts_filename(k))
               WRITE(grid%ts_filename(k)(i-4:i-3),'(I2.2)') grid%id


               ! ======== added by WC: 2009-05-20 ============
               ! This is added for Xcel realtime forecast
               ! Do not bother writing stations that can create multiple time records.
               ! This happens when we use the same name for the station but different
               ! specified lat/lon but same i, j point (because of the bizarre naming
               ! convention.
               !
               ! I am tricking WRF to do the d02 and d03 stations in tslist separately.
               ! The station identifer has 5 spaces. The last space is a number. If that
               ! number
               !
               read(grid%ts_filename(k)(5:5),*) igrid_from_station
               if (igrid_from_station /= grid%id ) goto 1000
               ! =============================================


               ! ================== added by WC for renaming the time series ==========
               !      file with the I, J location 
               !     on 2009-05-16
               write(prefix_ts_iloc,'(I4.4)') grid%itsloc(k)
               write(prefix_ts_jloc,'(I4.4)') grid%jtsloc(k)
               new_ts_filename=grid%ts_filename(k)(1:i-3)   &
                               //'.'//'i'//prefix_ts_iloc   &
                               //'.'//'j'//prefix_ts_jloc   &
                               //'.TS'
         
               !========== comment out by WC: 2009-05-16
               !OPEN(UNIT=iunit, FILE=TRIM(grid%ts_filename(k)), FORM='FORMATTED', STATUS='REPLACE')
               !Use unformatted, sequential access binary
               OPEN(UNIT=iunit, FILE=new_ts_filename(1:len_trim(new_ts_filename)), FORM='UNFORMATTED', STATUS='REPLACE')
! < ================ end of modification ====================
#if (EM_CORE == 1)
! ====== comment out by WC: 2009-05-21
!               WRITE(UNIT=iunit, &
!                     FMT='(A26,I2,I3,A6,A2,F7.3,A1,F8.3,A3,I4,A1,I4,A3,F7.3,A1,F8.3,A2,F6.1,A7)') &
!                     grid%desctsloc(grid%id_tsloc(k))//' ', grid%id, grid%id_tsloc(k), &
!                     ' '//grid%nametsloc(grid%id_tsloc(k)), &
!                     ' (', grid%lattsloc(grid%id_tsloc(k)), ',', grid%lontsloc(grid%id_tsloc(k)), ') (', &
!                     grid%itsloc(k), ',', grid%jtsloc(k), ') (', &
!                     ts_xlat, ',', ts_xlong, ') ', &
!                     ts_hgt,' meters'
               WRITE(UNIT=iunit)     &
                     grid%desctsloc(grid%id_tsloc(k))(1:25), grid%id, grid%id_tsloc(k), &
                     grid%nametsloc(grid%id_tsloc(k))(1:5),                             &
                     grid%lattsloc(grid%id_tsloc(k)), grid%lontsloc(grid%id_tsloc(k)),  &
                     grid%itsloc(k), grid%jtsloc(k),                                    &
                     ts_xlat, ts_xlong,                                                 &
                     ts_hgt
#else
! ====== comment out by WC: 2009-05-21
!               WRITE(UNIT=iunit, &
!                     FMT='(A26,I2,I3,A6,A2,F7.3,A1,F8.3,A3,I4,A1,I4,A3,F7.3,A1,F8.3,A2)') &
!                     grid%desctsloc(grid%id_tsloc(k))//' ', grid%id, grid%id_tsloc(k), &
!                     ' '//grid%nametsloc(grid%id_tsloc(k)), &
!                     ' (', grid%lattsloc(grid%id_tsloc(k)), ',', grid%lontsloc(grid%id_tsloc(k)), ') (', &
!                     grid%itsloc(k), ',', grid%jtsloc(k), ') (', &
!                     ts_xlat, ',', ts_xlong, ') '
               WRITE(UNIT=iunit)     &
                     grid%desctsloc(grid%id_tsloc(k))(1:25), grid%id, grid%id_tsloc(k), &
                     grid%nametsloc(grid%id_tsloc(k))(1:5),                             &
                     grid%lattsloc(grid%id_tsloc(k)), grid%lontsloc(grid%id_tsloc(k)),  &
                     grid%itsloc(k), grid%jtsloc(k),                                    &
                     ts_xlat, ts_xlong

#endif
               CLOSE(UNIT=iunit)
            END IF
! ======= added by WC: 2009-05-20===============
1000     CONTINUE
! ==============================================
         END DO
   
      END IF
#if ((EM_CORE == 1) && (DA_CORE != 1))
   END IF
#endif

END SUBROUTINE calc_ts_locations


SUBROUTINE calc_ts( grid )

   USE module_domain
   USE module_model_constants

   IMPLICIT NONE

   ! Arguments
   TYPE (domain), INTENT(INOUT) :: grid

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   ! Local variables
   INTEGER :: i, k, mm, n, ix, iy, rc
   REAL :: earth_u, earth_v, output_t, output_q, clw, xtime_minutes
   REAL, ALLOCATABLE, DIMENSION(:) :: p8w

   ! Parameter ts_model_level:  
       ! TRUE to output T, Q, and wind at lowest model level
       ! FALSE to output T and Q at 2-m and wind at 10-m diagnostic levels:
   LOGICAL, PARAMETER :: ts_model_level = .TRUE.  

   ! ======= added by WC: 2009-02-18 ========
   ! output every X minutes
   !REAL, PARAMETER :: xmin_out_interval = 10
   !
   !CALL domain_clock_get( grid, minutesSinceSimulationStart=xtime_minutes )
   !if (mod(int(xtime_minutes),int(xmin_out_interval)).ne.0) return

   ! ====== added by WC: 2009-03-31 ==========
   ! use seconds instead of minutes, Wanli found a problem with using minutes, output
   ! was irregular for the coarser grids
   !
   ! This subroutine can run into problems if the time in seconds is in fraction of
   ! seconds
   REAL, PARAMETER :: xsec_out_interval = 900    ! output interval in seconds
   REAL :: xtime_wrf_time_seconds                ! WRF time in seconds      

   CALL domain_clock_get( grid, minutesSinceSimulationStart=xtime_minutes )

   xtime_wrf_time_seconds=xtime_minutes*60.
   !write(660,*) 'xtime_wrf_time_seconds, grid, time_step, dt, use_adaptive_time_step=', xtime_wrf_time_seconds, grid%id, grid%time_step, grid%dt, grid%use_adaptive_time_step


   ! ===== added by WC: 2009-09-23 =============
   !       output for adaptive time step
   if (grid%use_adaptive_time_step) then
    if (abs(int(xtime_wrf_time_seconds)-                                          &
            xsec_out_interval*int(xtime_wrf_time_seconds/xsec_out_interval)).gt.  &
                               (1.0*grid%dt)) return
   else
    if (mod(int(xtime_wrf_time_seconds),int(xsec_out_interval)).ne.0) return
   endif

   !if (mod(int(xtime_wrf_time_seconds),int(xsec_out_interval)).ne.0) return

   ! =====================================================

   IF ( grid%ntsloc_domain .LE. 0 ) RETURN

#if ((EM_CORE == 1) && (DA_CORE != 1))
   IF ( grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage /= DFI_FST ) RETURN
#endif

   n = grid%next_ts_time

   ALLOCATE(p8w(grid%sm32:grid%em32))

   DO i=1,grid%ntsloc_domain

      ix = grid%itsloc(i)
      iy = grid%jtsloc(i)
  
      IF (grid%sp31 <= ix .AND. ix <= grid%ep31 .AND. &
          grid%sp33 <= iy .AND. iy <= grid%ep33) THEN
       
         IF (ts_model_level) THEN
   
            !
            ! Output from the lowest model computational level:
            !
#if (EM_CORE == 1)
            earth_u = grid%u_2(ix,1,iy)*grid%cosa(ix,iy)+grid%v_2(ix,1,iy)*grid%sina(ix,iy)
            earth_v = grid%v_2(ix,1,iy)*grid%cosa(ix,iy)-grid%u_2(ix,1,iy)*grid%sina(ix,iy)
            output_t = grid%t_2(ix,1,iy)


       ! ===== added by WC: 2009-05-15 ============
       !       to output u, v, T, pressure, z, qv at the first 36 model levels

       k = 1 
       grid%ts_u1l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v1l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 2 
       grid%ts_u2l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v2l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 3 
       grid%ts_u3l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v3l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 4 
       grid%ts_u4l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v4l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 5 
       grid%ts_u5l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v5l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 6 
       grid%ts_u6l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v6l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 7 
       grid%ts_u7l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v7l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 8 
       grid%ts_u8l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v8l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 9 
       grid%ts_u9l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v9l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 10 
       grid%ts_u10l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v10l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 11 
       grid%ts_u11l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v11l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 12 
       grid%ts_u12l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v12l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 13 
       grid%ts_u13l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v13l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 14 
       grid%ts_u14l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v14l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 15 
       grid%ts_u15l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v15l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 16 
       grid%ts_u16l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v16l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 17 
       grid%ts_u17l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v17l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 18 
       grid%ts_u18l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v18l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 19 
       grid%ts_u19l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v19l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 20 
       grid%ts_u20l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v20l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 21 
       grid%ts_u21l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v21l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 22 
       grid%ts_u22l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v22l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 23 
       grid%ts_u23l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v23l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 24 
       grid%ts_u24l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v24l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 25 
       grid%ts_u25l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v25l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 26 
       grid%ts_u26l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v26l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 27 
       grid%ts_u27l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v27l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 28 
       grid%ts_u28l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v28l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 29 
       grid%ts_u29l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v29l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 30 
       grid%ts_u30l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v30l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 31 
       grid%ts_u31l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v31l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 32 
       grid%ts_u32l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v32l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 33 
       grid%ts_u33l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v33l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 34 
       grid%ts_u34l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v34l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 35 
       grid%ts_u35l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v35l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 
       k = 36 
       grid%ts_u36l(n,i) = grid%u_2(ix,k,iy)*grid%cosa(ix,iy)+grid%v_2(ix,k,iy)*grid%sina(ix,iy)
       grid%ts_v36l(n,i) = grid%v_2(ix,k,iy)*grid%cosa(ix,iy)-grid%u_2(ix,k,iy)*grid%sina(ix,iy)
 


       k = 1 
       grid%ts_z1l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 2 
       grid%ts_z2l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 3 
       grid%ts_z3l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 4 
       grid%ts_z4l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 5 
       grid%ts_z5l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 6 
       grid%ts_z6l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 7 
       grid%ts_z7l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 8 
       grid%ts_z8l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 9 
       grid%ts_z9l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 10 
       grid%ts_z10l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 11 
       grid%ts_z11l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 12 
       grid%ts_z12l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 13 
       grid%ts_z13l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 14 
       grid%ts_z14l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 15 
       grid%ts_z15l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 16 
       grid%ts_z16l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 17 
       grid%ts_z17l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 18 
       grid%ts_z18l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 19 
       grid%ts_z19l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 20 
       grid%ts_z20l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 21 
       grid%ts_z21l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 22 
       grid%ts_z22l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 23 
       grid%ts_z23l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 24 
       grid%ts_z24l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 25 
       grid%ts_z25l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 26 
       grid%ts_z26l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 27 
       grid%ts_z27l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 28 
       grid%ts_z28l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 29 
       grid%ts_z29l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 30 
       grid%ts_z30l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 31 
       grid%ts_z31l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 32 
       grid%ts_z32l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 33 
       grid%ts_z33l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 34 
       grid%ts_z34l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 35 
       grid%ts_z35l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 
       k = 36 
       grid%ts_z36l(n,i) = 0.5*(grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy)+grid%phb(ix,k+1,iy)+grid%ph_2(ix,k+1,iy))/g
 

       k = 1 
       grid%ts_p1l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 2 
       grid%ts_p2l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 3 
       grid%ts_p3l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 4 
       grid%ts_p4l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 5 
       grid%ts_p5l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 6 
       grid%ts_p6l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 7 
       grid%ts_p7l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 8 
       grid%ts_p8l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 9 
       grid%ts_p9l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 10 
       grid%ts_p10l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 11 
       grid%ts_p11l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 12 
       grid%ts_p12l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 13 
       grid%ts_p13l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 14 
       grid%ts_p14l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 15 
       grid%ts_p15l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 16 
       grid%ts_p16l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 17 
       grid%ts_p17l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 18 
       grid%ts_p18l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 19 
       grid%ts_p19l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 20 
       grid%ts_p20l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 21 
       grid%ts_p21l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 22 
       grid%ts_p22l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 23 
       grid%ts_p23l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 24 
       grid%ts_p24l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 25 
       grid%ts_p25l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 26 
       grid%ts_p26l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 27 
       grid%ts_p27l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 28 
       grid%ts_p28l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 29 
       grid%ts_p29l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 30 
       grid%ts_p30l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 31 
       grid%ts_p31l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 32 
       grid%ts_p32l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 33 
       grid%ts_p33l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 34 
       grid%ts_p34l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 35 
       grid%ts_p35l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 
       k = 36 
       grid%ts_p36l(n,i) = grid%pb(ix,k,iy)+grid%p(ix,k,iy)
 


       k = 1 
       grid%ts_t1l(n,i) = grid%t_2(ix,k,iy)
 
       k = 2 
       grid%ts_t2l(n,i) = grid%t_2(ix,k,iy)
 
       k = 3 
       grid%ts_t3l(n,i) = grid%t_2(ix,k,iy)
 
       k = 4 
       grid%ts_t4l(n,i) = grid%t_2(ix,k,iy)
 
       k = 5 
       grid%ts_t5l(n,i) = grid%t_2(ix,k,iy)
 
       k = 6 
       grid%ts_t6l(n,i) = grid%t_2(ix,k,iy)
 
       k = 7 
       grid%ts_t7l(n,i) = grid%t_2(ix,k,iy)
 
       k = 8 
       grid%ts_t8l(n,i) = grid%t_2(ix,k,iy)
 
       k = 9 
       grid%ts_t9l(n,i) = grid%t_2(ix,k,iy)
 
       k = 10 
       grid%ts_t10l(n,i) = grid%t_2(ix,k,iy)
 
       k = 11 
       grid%ts_t11l(n,i) = grid%t_2(ix,k,iy)
 
       k = 12 
       grid%ts_t12l(n,i) = grid%t_2(ix,k,iy)
 
       k = 13 
       grid%ts_t13l(n,i) = grid%t_2(ix,k,iy)
 
       k = 14 
       grid%ts_t14l(n,i) = grid%t_2(ix,k,iy)
 
       k = 15 
       grid%ts_t15l(n,i) = grid%t_2(ix,k,iy)
 
       k = 16 
       grid%ts_t16l(n,i) = grid%t_2(ix,k,iy)
 
       k = 17 
       grid%ts_t17l(n,i) = grid%t_2(ix,k,iy)
 
       k = 18 
       grid%ts_t18l(n,i) = grid%t_2(ix,k,iy)
 
       k = 19 
       grid%ts_t19l(n,i) = grid%t_2(ix,k,iy)
 
       k = 20 
       grid%ts_t20l(n,i) = grid%t_2(ix,k,iy)
 
       k = 21 
       grid%ts_t21l(n,i) = grid%t_2(ix,k,iy)
 
       k = 22 
       grid%ts_t22l(n,i) = grid%t_2(ix,k,iy)
 
       k = 23 
       grid%ts_t23l(n,i) = grid%t_2(ix,k,iy)
 
       k = 24 
       grid%ts_t24l(n,i) = grid%t_2(ix,k,iy)
 
       k = 25 
       grid%ts_t25l(n,i) = grid%t_2(ix,k,iy)
 
       k = 26 
       grid%ts_t26l(n,i) = grid%t_2(ix,k,iy)
 
       k = 27 
       grid%ts_t27l(n,i) = grid%t_2(ix,k,iy)
 
       k = 28 
       grid%ts_t28l(n,i) = grid%t_2(ix,k,iy)
 
       k = 29 
       grid%ts_t29l(n,i) = grid%t_2(ix,k,iy)
 
       k = 30 
       grid%ts_t30l(n,i) = grid%t_2(ix,k,iy)
 
       k = 31 
       grid%ts_t31l(n,i) = grid%t_2(ix,k,iy)
 
       k = 32 
       grid%ts_t32l(n,i) = grid%t_2(ix,k,iy)
 
       k = 33 
       grid%ts_t33l(n,i) = grid%t_2(ix,k,iy)
 
       k = 34 
       grid%ts_t34l(n,i) = grid%t_2(ix,k,iy)
 
       k = 35 
       grid%ts_t35l(n,i) = grid%t_2(ix,k,iy)
 
       k = 36 
       grid%ts_t36l(n,i) = grid%t_2(ix,k,iy)
 
       ! ===========================================================

#else
            earth_u = grid%u(ix,1,iy)
            earth_v = grid%v(ix,1,iy)
            output_t = grid%t(ix,1,iy)
#endif
            output_q = grid%moist(ix,1,iy,P_QV)
  
            ! =========== added by WC: 2009-05-15 ===========
            !          for water vapor mixing ratio time series


            k = 1 
            grid%ts_q1l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 2 
            grid%ts_q2l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 3 
            grid%ts_q3l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 4 
            grid%ts_q4l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 5 
            grid%ts_q5l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 6 
            grid%ts_q6l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 7 
            grid%ts_q7l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 8 
            grid%ts_q8l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 9 
            grid%ts_q9l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 10 
            grid%ts_q10l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 11 
            grid%ts_q11l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 12 
            grid%ts_q12l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 13 
            grid%ts_q13l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 14 
            grid%ts_q14l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 15 
            grid%ts_q15l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 16 
            grid%ts_q16l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 17 
            grid%ts_q17l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 18 
            grid%ts_q18l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 19 
            grid%ts_q19l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 20 
            grid%ts_q20l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 21 
            grid%ts_q21l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 22 
            grid%ts_q22l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 23 
            grid%ts_q23l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 24 
            grid%ts_q24l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 25 
            grid%ts_q25l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 26 
            grid%ts_q26l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 27 
            grid%ts_q27l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 28 
            grid%ts_q28l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 29 
            grid%ts_q29l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 30 
            grid%ts_q30l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 31 
            grid%ts_q31l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 32 
            grid%ts_q32l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 33 
            grid%ts_q33l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 34 
            grid%ts_q34l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 35 
            grid%ts_q35l(n,i) = grid%moist(ix,k,iy,P_QV) 
            k = 36 
            grid%ts_q36l(n,i) = grid%moist(ix,k,iy,P_QV)

            ! ============================================= 
         ELSE
   
            !
            ! Output at 2-m and 10-m diagnostic levels:
            !
#if (EM_CORE == 1)
            earth_u = grid%u10(ix,iy)*grid%cosa(ix,iy)+grid%v10(ix,iy)*grid%sina(ix,iy)
            earth_v = grid%v10(ix,iy)*grid%cosa(ix,iy)-grid%u10(ix,iy)*grid%sina(ix,iy)
            output_q = grid%q2(ix,iy)
#else
            earth_u = grid%u10(ix,iy)
            earth_v = grid%v10(ix,iy)
            output_q = grid%qsfc(ix,iy)
#endif
            output_t = grid%t2(ix,iy)
   
         END IF

! ===============================================================>
   
         CALL domain_clock_get( grid, minutesSinceSimulationStart=xtime_minutes )
         grid%ts_hour(n,i) = xtime_minutes / 60.
         grid%ts_u(n,i)   = earth_u
         grid%ts_v(n,i)   = earth_v
         grid%ts_t(n,i)    = output_t
         ! < ======= comment out by WC: 2009-02-18 ========
         !grid%ts_q(n,i)    = output_q
         ! =================================================>
         grid%ts_psfc(n,i) = grid%psfc(ix,iy)
#if (EM_CORE == 1)
         grid%ts_rainc(n,i)  = grid%rainc(ix,iy)
         grid%ts_rainnc(n,i) = grid%rainnc(ix,iy)
#else
         grid%ts_tsk(n,i)  = grid%nmm_tsk(ix,iy)
#endif
   
      ELSE
  
         grid%ts_hour(n,i) = 1.E30
         grid%ts_u(n,i)    = 1.E30
         grid%ts_v(n,i)    = 1.E30
         grid%ts_t(n,i)    = 1.E30
         grid%ts_q(n,i)    = 1.E30
         grid%ts_psfc(n,i) = 1.E30

         ! ====== added by WC: 2009-05-15 =========
         ! if you don't add these lines,
         ! the new variables will be zero


         grid%ts_u1l(n,i) = 1.E30 
         grid%ts_u2l(n,i) = 1.E30 
         grid%ts_u3l(n,i) = 1.E30 
         grid%ts_u4l(n,i) = 1.E30 
         grid%ts_u5l(n,i) = 1.E30 
         grid%ts_u6l(n,i) = 1.E30 
         grid%ts_u7l(n,i) = 1.E30 
         grid%ts_u8l(n,i) = 1.E30 
         grid%ts_u9l(n,i) = 1.E30 
         grid%ts_u10l(n,i) = 1.E30 
         grid%ts_u11l(n,i) = 1.E30 
         grid%ts_u12l(n,i) = 1.E30 
         grid%ts_u13l(n,i) = 1.E30 
         grid%ts_u14l(n,i) = 1.E30 
         grid%ts_u15l(n,i) = 1.E30 
         grid%ts_u16l(n,i) = 1.E30 
         grid%ts_u17l(n,i) = 1.E30 
         grid%ts_u18l(n,i) = 1.E30 
         grid%ts_u19l(n,i) = 1.E30 
         grid%ts_u20l(n,i) = 1.E30 
         grid%ts_u21l(n,i) = 1.E30 
         grid%ts_u22l(n,i) = 1.E30 
         grid%ts_u23l(n,i) = 1.E30 
         grid%ts_u24l(n,i) = 1.E30 
         grid%ts_u25l(n,i) = 1.E30 
         grid%ts_u26l(n,i) = 1.E30 
         grid%ts_u27l(n,i) = 1.E30 
         grid%ts_u28l(n,i) = 1.E30 
         grid%ts_u29l(n,i) = 1.E30 
         grid%ts_u30l(n,i) = 1.E30 
         grid%ts_u31l(n,i) = 1.E30 
         grid%ts_u32l(n,i) = 1.E30 
         grid%ts_u33l(n,i) = 1.E30 
         grid%ts_u34l(n,i) = 1.E30 
         grid%ts_u35l(n,i) = 1.E30 
         grid%ts_u36l(n,i) = 1.E30 

         grid%ts_v1l(n,i) = 1.E30 
         grid%ts_v2l(n,i) = 1.E30 
         grid%ts_v3l(n,i) = 1.E30 
         grid%ts_v4l(n,i) = 1.E30 
         grid%ts_v5l(n,i) = 1.E30 
         grid%ts_v6l(n,i) = 1.E30 
         grid%ts_v7l(n,i) = 1.E30 
         grid%ts_v8l(n,i) = 1.E30 
         grid%ts_v9l(n,i) = 1.E30 
         grid%ts_v10l(n,i) = 1.E30 
         grid%ts_v11l(n,i) = 1.E30 
         grid%ts_v12l(n,i) = 1.E30 
         grid%ts_v13l(n,i) = 1.E30 
         grid%ts_v14l(n,i) = 1.E30 
         grid%ts_v15l(n,i) = 1.E30 
         grid%ts_v16l(n,i) = 1.E30 
         grid%ts_v17l(n,i) = 1.E30 
         grid%ts_v18l(n,i) = 1.E30 
         grid%ts_v19l(n,i) = 1.E30 
         grid%ts_v20l(n,i) = 1.E30 
         grid%ts_v21l(n,i) = 1.E30 
         grid%ts_v22l(n,i) = 1.E30 
         grid%ts_v23l(n,i) = 1.E30 
         grid%ts_v24l(n,i) = 1.E30 
         grid%ts_v25l(n,i) = 1.E30 
         grid%ts_v26l(n,i) = 1.E30 
         grid%ts_v27l(n,i) = 1.E30 
         grid%ts_v28l(n,i) = 1.E30 
         grid%ts_v29l(n,i) = 1.E30 
         grid%ts_v30l(n,i) = 1.E30 
         grid%ts_v31l(n,i) = 1.E30 
         grid%ts_v32l(n,i) = 1.E30 
         grid%ts_v33l(n,i) = 1.E30 
         grid%ts_v34l(n,i) = 1.E30 
         grid%ts_v35l(n,i) = 1.E30 
         grid%ts_v36l(n,i) = 1.E30 

         grid%ts_z1l(n,i) = 1.E30 
         grid%ts_z2l(n,i) = 1.E30 
         grid%ts_z3l(n,i) = 1.E30 
         grid%ts_z4l(n,i) = 1.E30 
         grid%ts_z5l(n,i) = 1.E30 
         grid%ts_z6l(n,i) = 1.E30 
         grid%ts_z7l(n,i) = 1.E30 
         grid%ts_z8l(n,i) = 1.E30 
         grid%ts_z9l(n,i) = 1.E30 
         grid%ts_z10l(n,i) = 1.E30 
         grid%ts_z11l(n,i) = 1.E30 
         grid%ts_z12l(n,i) = 1.E30 
         grid%ts_z13l(n,i) = 1.E30 
         grid%ts_z14l(n,i) = 1.E30 
         grid%ts_z15l(n,i) = 1.E30 
         grid%ts_z16l(n,i) = 1.E30 
         grid%ts_z17l(n,i) = 1.E30 
         grid%ts_z18l(n,i) = 1.E30 
         grid%ts_z19l(n,i) = 1.E30 
         grid%ts_z20l(n,i) = 1.E30 
         grid%ts_z21l(n,i) = 1.E30 
         grid%ts_z22l(n,i) = 1.E30 
         grid%ts_z23l(n,i) = 1.E30 
         grid%ts_z24l(n,i) = 1.E30 
         grid%ts_z25l(n,i) = 1.E30 
         grid%ts_z26l(n,i) = 1.E30 
         grid%ts_z27l(n,i) = 1.E30 
         grid%ts_z28l(n,i) = 1.E30 
         grid%ts_z29l(n,i) = 1.E30 
         grid%ts_z30l(n,i) = 1.E30 
         grid%ts_z31l(n,i) = 1.E30 
         grid%ts_z32l(n,i) = 1.E30 
         grid%ts_z33l(n,i) = 1.E30 
         grid%ts_z34l(n,i) = 1.E30 
         grid%ts_z35l(n,i) = 1.E30 
         grid%ts_z36l(n,i) = 1.E30 

         grid%ts_p1l(n,i) = 1.E30 
         grid%ts_p2l(n,i) = 1.E30 
         grid%ts_p3l(n,i) = 1.E30 
         grid%ts_p4l(n,i) = 1.E30 
         grid%ts_p5l(n,i) = 1.E30 
         grid%ts_p6l(n,i) = 1.E30 
         grid%ts_p7l(n,i) = 1.E30 
         grid%ts_p8l(n,i) = 1.E30 
         grid%ts_p9l(n,i) = 1.E30 
         grid%ts_p10l(n,i) = 1.E30 
         grid%ts_p11l(n,i) = 1.E30 
         grid%ts_p12l(n,i) = 1.E30 
         grid%ts_p13l(n,i) = 1.E30 
         grid%ts_p14l(n,i) = 1.E30 
         grid%ts_p15l(n,i) = 1.E30 
         grid%ts_p16l(n,i) = 1.E30 
         grid%ts_p17l(n,i) = 1.E30 
         grid%ts_p18l(n,i) = 1.E30 
         grid%ts_p19l(n,i) = 1.E30 
         grid%ts_p20l(n,i) = 1.E30 
         grid%ts_p21l(n,i) = 1.E30 
         grid%ts_p22l(n,i) = 1.E30 
         grid%ts_p23l(n,i) = 1.E30 
         grid%ts_p24l(n,i) = 1.E30 
         grid%ts_p25l(n,i) = 1.E30 
         grid%ts_p26l(n,i) = 1.E30 
         grid%ts_p27l(n,i) = 1.E30 
         grid%ts_p28l(n,i) = 1.E30 
         grid%ts_p29l(n,i) = 1.E30 
         grid%ts_p30l(n,i) = 1.E30 
         grid%ts_p31l(n,i) = 1.E30 
         grid%ts_p32l(n,i) = 1.E30 
         grid%ts_p33l(n,i) = 1.E30 
         grid%ts_p34l(n,i) = 1.E30 
         grid%ts_p35l(n,i) = 1.E30 
         grid%ts_p36l(n,i) = 1.E30 

         grid%ts_t1l(n,i) = 1.E30 
         grid%ts_t2l(n,i) = 1.E30 
         grid%ts_t3l(n,i) = 1.E30 
         grid%ts_t4l(n,i) = 1.E30 
         grid%ts_t5l(n,i) = 1.E30 
         grid%ts_t6l(n,i) = 1.E30 
         grid%ts_t7l(n,i) = 1.E30 
         grid%ts_t8l(n,i) = 1.E30 
         grid%ts_t9l(n,i) = 1.E30 
         grid%ts_t10l(n,i) = 1.E30 
         grid%ts_t11l(n,i) = 1.E30 
         grid%ts_t12l(n,i) = 1.E30 
         grid%ts_t13l(n,i) = 1.E30 
         grid%ts_t14l(n,i) = 1.E30 
         grid%ts_t15l(n,i) = 1.E30 
         grid%ts_t16l(n,i) = 1.E30 
         grid%ts_t17l(n,i) = 1.E30 
         grid%ts_t18l(n,i) = 1.E30 
         grid%ts_t19l(n,i) = 1.E30 
         grid%ts_t20l(n,i) = 1.E30 
         grid%ts_t21l(n,i) = 1.E30 
         grid%ts_t22l(n,i) = 1.E30 
         grid%ts_t23l(n,i) = 1.E30 
         grid%ts_t24l(n,i) = 1.E30 
         grid%ts_t25l(n,i) = 1.E30 
         grid%ts_t26l(n,i) = 1.E30 
         grid%ts_t27l(n,i) = 1.E30 
         grid%ts_t28l(n,i) = 1.E30 
         grid%ts_t29l(n,i) = 1.E30 
         grid%ts_t30l(n,i) = 1.E30 
         grid%ts_t31l(n,i) = 1.E30 
         grid%ts_t32l(n,i) = 1.E30 
         grid%ts_t33l(n,i) = 1.E30 
         grid%ts_t34l(n,i) = 1.E30 
         grid%ts_t35l(n,i) = 1.E30 
         grid%ts_t36l(n,i) = 1.E30 

         grid%ts_q1l(n,i) = 1.E30 
         grid%ts_q2l(n,i) = 1.E30 
         grid%ts_q3l(n,i) = 1.E30 
         grid%ts_q4l(n,i) = 1.E30 
         grid%ts_q5l(n,i) = 1.E30 
         grid%ts_q6l(n,i) = 1.E30 
         grid%ts_q7l(n,i) = 1.E30 
         grid%ts_q8l(n,i) = 1.E30 
         grid%ts_q9l(n,i) = 1.E30 
         grid%ts_q10l(n,i) = 1.E30 
         grid%ts_q11l(n,i) = 1.E30 
         grid%ts_q12l(n,i) = 1.E30 
         grid%ts_q13l(n,i) = 1.E30 
         grid%ts_q14l(n,i) = 1.E30 
         grid%ts_q15l(n,i) = 1.E30 
         grid%ts_q16l(n,i) = 1.E30 
         grid%ts_q17l(n,i) = 1.E30 
         grid%ts_q18l(n,i) = 1.E30 
         grid%ts_q19l(n,i) = 1.E30 
         grid%ts_q20l(n,i) = 1.E30 
         grid%ts_q21l(n,i) = 1.E30 
         grid%ts_q22l(n,i) = 1.E30 
         grid%ts_q23l(n,i) = 1.E30 
         grid%ts_q24l(n,i) = 1.E30 
         grid%ts_q25l(n,i) = 1.E30 
         grid%ts_q26l(n,i) = 1.E30 
         grid%ts_q27l(n,i) = 1.E30 
         grid%ts_q28l(n,i) = 1.E30 
         grid%ts_q29l(n,i) = 1.E30 
         grid%ts_q30l(n,i) = 1.E30 
         grid%ts_q31l(n,i) = 1.E30 
         grid%ts_q32l(n,i) = 1.E30 
         grid%ts_q33l(n,i) = 1.E30 
         grid%ts_q34l(n,i) = 1.E30 
         grid%ts_q35l(n,i) = 1.E30 
         grid%ts_q36l(n,i) = 1.E30 
         ! ========================================
#if (EM_CORE == 1)
         grid%ts_glw(n,i)  = 1.E30
         grid%ts_gsw(n,i)  = 1.E30
         grid%ts_hfx(n,i)  = 1.E30
         grid%ts_qfx(n,i)  = 1.E30
         grid%ts_clw(n,i)  = 1.E30
         grid%ts_rainc(n,i)  = 1.E30
         grid%ts_rainnc(n,i) = 1.E30
#endif
         grid%ts_tsk(n,i)  = 1.E30
         grid%ts_tslb(n,i) = 1.E30

      END IF

   END DO

   DEALLOCATE(p8w)
 
   grid%next_ts_time = grid%next_ts_time + 1

   IF ( grid%next_ts_time > grid%ts_buf_size ) CALL write_ts(grid)

END SUBROUTINE calc_ts


SUBROUTINE write_ts( grid )

   USE module_dm
   USE module_domain

   IMPLICIT NONE

   ! Arguments
   TYPE (domain), INTENT(INOUT) :: grid

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER, EXTERNAL :: get_unused_unit

   ! Local variables
   INTEGER :: i, n, ix, iy, iunit
   REAL, ALLOCATABLE, DIMENSION(:,:) :: ts_buf

   ! ======= added by WC: 2009-05-16 =========
   ! new filename with I, J index
   CHARACTER (LEN=200) :: new_ts_filename
   CHARACTER (LEN=4) :: prefix_ts_iloc, prefix_ts_jloc

   ! integer to identify the domain number from station name
   INTEGER :: igrid_from_station
   ! =========================================

   IF ( grid%ntsloc_domain .LE. 0 ) RETURN

#if ((EM_CORE == 1) && (DA_CORE != 1))
   IF ( grid%dfi_opt /= DFI_NODFI .AND. grid%dfi_stage /= DFI_FST ) RETURN
#endif

#ifdef DM_PARALLEL
   ALLOCATE(ts_buf(grid%ts_buf_size,grid%max_ts_locs))

   ts_buf(:,:) = grid%ts_hour(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_hour(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_u(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_v(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_t(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_q(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_psfc(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_psfc(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ! ======= added by WC: 2009-05-15 ================

   ts_buf(:,:) = grid%ts_u1l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u1l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u2l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u2l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u3l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u3l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u4l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u4l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u5l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u5l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u6l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u6l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u7l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u7l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u8l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u8l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u9l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u9l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u10l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u10l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u11l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u11l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u12l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u12l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u13l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u13l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u14l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u14l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u15l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u15l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u16l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u16l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u17l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u17l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u18l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u18l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u19l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u19l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u20l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u20l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u21l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u21l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u22l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u22l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u23l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u23l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u24l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u24l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u25l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u25l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u26l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u26l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u27l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u27l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u28l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u28l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u29l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u29l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u30l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u30l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u31l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u31l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u32l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u32l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u33l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u33l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u34l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u34l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u35l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u35l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_u36l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_u36l(:,:),grid%ts_buf_size*grid%max_ts_locs) 

   ts_buf(:,:) = grid%ts_v1l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v1l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v2l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v2l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v3l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v3l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v4l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v4l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v5l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v5l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v6l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v6l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v7l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v7l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v8l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v8l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v9l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v9l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v10l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v10l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v11l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v11l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v12l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v12l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v13l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v13l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v14l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v14l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v15l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v15l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v16l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v16l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v17l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v17l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v18l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v18l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v19l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v19l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v20l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v20l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v21l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v21l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v22l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v22l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v23l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v23l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v24l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v24l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v25l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v25l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v26l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v26l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v27l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v27l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v28l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v28l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v29l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v29l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v30l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v30l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v31l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v31l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v32l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v32l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v33l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v33l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v34l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v34l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v35l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v35l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_v36l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_v36l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   
   ts_buf(:,:) = grid%ts_z1l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z1l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z2l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z2l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z3l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z3l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z4l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z4l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z5l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z5l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z6l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z6l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z7l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z7l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z8l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z8l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z9l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z9l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z10l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z10l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z11l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z11l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z12l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z12l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z13l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z13l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z14l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z14l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z15l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z15l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z16l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z16l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z17l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z17l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z18l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z18l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z19l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z19l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z20l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z20l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z21l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z21l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z22l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z22l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z23l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z23l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z24l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z24l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z25l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z25l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z26l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z26l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z27l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z27l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z28l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z28l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z29l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z29l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z30l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z30l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z31l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z31l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z32l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z32l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z33l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z33l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z34l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z34l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z35l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z35l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_z36l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_z36l(:,:),grid%ts_buf_size*grid%max_ts_locs) 

   ts_buf(:,:) = grid%ts_p1l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p1l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p2l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p2l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p3l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p3l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p4l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p4l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p5l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p5l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p6l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p6l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p7l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p7l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p8l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p8l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p9l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p9l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p10l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p10l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p11l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p11l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p12l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p12l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p13l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p13l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p14l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p14l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p15l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p15l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p16l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p16l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p17l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p17l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p18l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p18l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p19l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p19l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p20l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p20l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p21l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p21l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p22l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p22l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p23l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p23l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p24l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p24l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p25l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p25l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p26l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p26l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p27l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p27l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p28l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p28l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p29l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p29l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p30l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p30l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p31l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p31l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p32l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p32l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p33l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p33l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p34l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p34l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p35l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p35l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_p36l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_p36l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
  
   ts_buf(:,:) = grid%ts_t1l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t1l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t2l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t2l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t3l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t3l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t4l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t4l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t5l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t5l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t6l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t6l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t7l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t7l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t8l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t8l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t9l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t9l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t10l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t10l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t11l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t11l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t12l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t12l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t13l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t13l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t14l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t14l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t15l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t15l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t16l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t16l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t17l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t17l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t18l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t18l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t19l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t19l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t20l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t20l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t21l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t21l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t22l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t22l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t23l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t23l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t24l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t24l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t25l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t25l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t26l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t26l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t27l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t27l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t28l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t28l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t29l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t29l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t30l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t30l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t31l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t31l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t32l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t32l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t33l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t33l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t34l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t34l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t35l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t35l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_t36l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_t36l(:,:),grid%ts_buf_size*grid%max_ts_locs) 

   ts_buf(:,:) = grid%ts_q1l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q1l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q2l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q2l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q3l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q3l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q4l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q4l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q5l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q5l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q6l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q6l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q7l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q7l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q8l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q8l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q9l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q9l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q10l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q10l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q11l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q11l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q12l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q12l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q13l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q13l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q14l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q14l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q15l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q15l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q16l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q16l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q17l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q17l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q18l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q18l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q19l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q19l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q20l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q20l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q21l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q21l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q22l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q22l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q23l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q23l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q24l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q24l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q25l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q25l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q26l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q26l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q27l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q27l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q28l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q28l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q29l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q29l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q30l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q30l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q31l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q31l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q32l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q32l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q33l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q33l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q34l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q34l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q35l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q35l(:,:),grid%ts_buf_size*grid%max_ts_locs) 
   ts_buf(:,:) = grid%ts_q36l(:,:) 
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_q36l(:,:),grid%ts_buf_size*grid%max_ts_locs) 

   ! =================================================
#if (EM_CORE == 1)
   ts_buf(:,:) = grid%ts_glw(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_glw(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_gsw(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_gsw(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_hfx(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_hfx(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_qfx(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_qfx(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_clw(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_clw(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_rainc(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_rainc(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_rainnc(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_rainnc(:,:),grid%ts_buf_size*grid%max_ts_locs)
#endif

   ts_buf(:,:) = grid%ts_tsk(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tsk(:,:),grid%ts_buf_size*grid%max_ts_locs)

   ts_buf(:,:) = grid%ts_tslb(:,:)
   CALL wrf_dm_min_reals(ts_buf(:,:),grid%ts_tslb(:,:),grid%ts_buf_size*grid%max_ts_locs)

   DEALLOCATE(ts_buf)
#endif

   IF ( wrf_dm_on_monitor() ) THEN

      iunit = get_unused_unit()
      IF ( iunit <= 0 ) THEN
         CALL wrf_error_fatal('Error in write_ts: could not find a free Fortran unit.')
      END IF

      DO i=1,grid%ntsloc_domain

         ix = grid%itsloc(i)
         iy = grid%jtsloc(i)

         ! ======== added by WC: 2009-05-20 ============
         ! This is added for Xcel realtime forecast
         ! Do not bother writing stations that can create multiple time records.
         ! This happens when we use the same name for the station but different
         ! specified lat/lon but same i, j point (because of the bizarre naming
         ! convention.
         !
         ! I am tricking WRF to do the d02 and d03 stations in tslist separately.
         ! The station identifer has 5 spaces. The last space is a number. If that
         ! number
         !
         read(grid%ts_filename(i)(5:5),*) igrid_from_station
         if (igrid_from_station /= grid%id ) goto 1000
         ! =============================================

! ================== added by WC for renaming the time series =========
!                    file with the I, J location         
!                    on 2009-05-16
!
!         Be careful here, if the number of domains exceed 9,
!         The locations of adding the i, j coordinates in the
!         file name will need to be shifted.
!           
!         The time series file prefix looks like:
!
!	         zXXXX.dYY
! 
!         where XXXX is the station name and YY is the domain number.
!       
!         I need to do it this way because the method in the above
!         will not work.
 
          write(prefix_ts_iloc,'(I4.4)') grid%itsloc(i)
          write(prefix_ts_jloc,'(I4.4)') grid%jtsloc(i)
          new_ts_filename=grid%ts_filename(i)(1:9)     &
                          //'.'//'i'//prefix_ts_iloc   &
                          //'.'//'j'//prefix_ts_jloc   &
                          //'.TS'

!      ========== comment out by WC: 2009-05-16
!
!         OPEN(UNIT=iunit, FILE=TRIM(grid%ts_filename(i)), STATUS='unknown', POSITION='append', FORM='formatted')
         OPEN(UNIT=iunit, FILE=new_ts_filename(1:len_trim(new_ts_filename)), STATUS='unknown', POSITION='append', FORM='unformatted')
! < ================= end of modification
         DO n=1,grid%next_ts_time - 1

#if (EM_CORE == 1)
!
! ----->> BEGIN STAGE RAL3.8.1R0 SOURCE UPDATE 1 - Will Cheng mods for time series calculation
! <======= new output created by WC: 2009-02-17 ===========
!          for winds at other zeta levels
!
! write out in sequential access binary instead
!            WRITE(UNIT=iunit,FMT='(i2,1x,(i4,1x,i3,1x,f10.6,1x),f13.6,1x,3(i5,1x),220(f13.5,1x))')  &
            WRITE(UNIT=iunit)                                    &
                              grid%id,                           &
                              grid%julyr,                        &  ! year of cold start
                              grid%julday,                       &  ! Julian day of cold start
                              grid%gmt,                          &  ! UTC hour of cold start, HH.xxxxxx
                              grid%ts_hour(n,i),                 &
                              grid%id_tsloc(i), ix, iy,          &
                              grid%ts_t(n,i),                    &
                              grid%ts_psfc(n,i),                 &
                              grid%ts_z1l(n,i),                  &
                              grid%ts_p1l(n,i),                  &
                            ( (grid%ts_t1l(n,i)+300.)*           &
                             (grid%ts_p1l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u1l(n,i),                  &
                              grid%ts_v1l(n,i),                  &
                              grid%ts_q1l(n,i)*1000,             &
                              grid%ts_z2l(n,i),                  &
                              grid%ts_p2l(n,i),                  &
                            ( (grid%ts_t2l(n,i)+300.)*           &
                             (grid%ts_p2l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u2l(n,i),                  &
                              grid%ts_v2l(n,i),                  &
                              grid%ts_q2l(n,i)*1000,             &
                              grid%ts_z3l(n,i),                  &
                              grid%ts_p3l(n,i),                  &
                            ( (grid%ts_t3l(n,i)+300.)*           &
                             (grid%ts_p3l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u3l(n,i),                  &
                              grid%ts_v3l(n,i),                  &
                              grid%ts_q3l(n,i)*1000,             &
                              grid%ts_z4l(n,i),                  &
                              grid%ts_p4l(n,i),                  &
                            ( (grid%ts_t4l(n,i)+300.)*           &
                             (grid%ts_p4l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u4l(n,i),                  &
                              grid%ts_v4l(n,i),                  &
                              grid%ts_q4l(n,i)*1000,             &
                              grid%ts_z5l(n,i),                  &
                              grid%ts_p5l(n,i),                  &
                            ( (grid%ts_t5l(n,i)+300.)*           &
                             (grid%ts_p5l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u5l(n,i),                  &
                              grid%ts_v5l(n,i),                  &
                              grid%ts_q5l(n,i)*1000,             &
                              grid%ts_z6l(n,i),                  &
                              grid%ts_p6l(n,i),                  &
                            ( (grid%ts_t6l(n,i)+300.)*           &
                             (grid%ts_p6l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u6l(n,i),                  &
                              grid%ts_v6l(n,i),                  &
                              grid%ts_q6l(n,i)*1000,             &
                              grid%ts_z7l(n,i),                  &
                              grid%ts_p7l(n,i),                  &
                            ( (grid%ts_t7l(n,i)+300.)*           &
                             (grid%ts_p7l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u7l(n,i),                  &
                              grid%ts_v7l(n,i),                  &
                              grid%ts_q7l(n,i)*1000,             &
                              grid%ts_z8l(n,i),                  &
                              grid%ts_p8l(n,i),                  &
                            ( (grid%ts_t8l(n,i)+300.)*           &
                             (grid%ts_p8l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u8l(n,i),                  &
                              grid%ts_v8l(n,i),                  &
                              grid%ts_q8l(n,i)*1000,             &
                              grid%ts_z9l(n,i),                  &
                              grid%ts_p9l(n,i),                  &
                            ( (grid%ts_t9l(n,i)+300.)*           &
                             (grid%ts_p9l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u9l(n,i),                  &
                              grid%ts_v9l(n,i),                  &
                              grid%ts_q9l(n,i)*1000,             &
                              grid%ts_z10l(n,i),                  &
                              grid%ts_p10l(n,i),                  &
                            ( (grid%ts_t10l(n,i)+300.)*           &
                             (grid%ts_p10l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u10l(n,i),                  &
                              grid%ts_v10l(n,i),                  &
                              grid%ts_q10l(n,i)*1000,             &
                              grid%ts_z11l(n,i),                  &
                              grid%ts_p11l(n,i),                  &
                            ( (grid%ts_t11l(n,i)+300.)*           &
                             (grid%ts_p11l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u11l(n,i),                  &
                              grid%ts_v11l(n,i),                  &
                              grid%ts_q11l(n,i)*1000,             &
                              grid%ts_z12l(n,i),                  &
                              grid%ts_p12l(n,i),                  &
                            ( (grid%ts_t12l(n,i)+300.)*           &
                             (grid%ts_p12l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u12l(n,i),                  &
                              grid%ts_v12l(n,i),                  &
                              grid%ts_q12l(n,i)*1000,             &
                              grid%ts_z13l(n,i),                  &
                              grid%ts_p13l(n,i),                  &
                            ( (grid%ts_t13l(n,i)+300.)*           &
                             (grid%ts_p13l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u13l(n,i),                  &
                              grid%ts_v13l(n,i),                  &
                              grid%ts_q13l(n,i)*1000,             &
                              grid%ts_z14l(n,i),                  &
                              grid%ts_p14l(n,i),                  &
                            ( (grid%ts_t14l(n,i)+300.)*           &
                             (grid%ts_p14l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u14l(n,i),                  &
                              grid%ts_v14l(n,i),                  &
                              grid%ts_q14l(n,i)*1000,             &
                              grid%ts_z15l(n,i),                  &
                              grid%ts_p15l(n,i),                  &
                            ( (grid%ts_t15l(n,i)+300.)*           &
                             (grid%ts_p15l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u15l(n,i),                  &
                              grid%ts_v15l(n,i),                  &
                              grid%ts_q15l(n,i)*1000,             &
                              grid%ts_z16l(n,i),                  &
                              grid%ts_p16l(n,i),                  &
                            ( (grid%ts_t16l(n,i)+300.)*           &
                             (grid%ts_p16l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u16l(n,i),                  &
                              grid%ts_v16l(n,i),                  &
                              grid%ts_q16l(n,i)*1000,             &
                              grid%ts_z17l(n,i),                  &
                              grid%ts_p17l(n,i),                  &
                            ( (grid%ts_t17l(n,i)+300.)*           &
                             (grid%ts_p17l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u17l(n,i),                  &
                              grid%ts_v17l(n,i),                  &
                              grid%ts_q17l(n,i)*1000,             &
                              grid%ts_z18l(n,i),                  &
                              grid%ts_p18l(n,i),                  &
                            ( (grid%ts_t18l(n,i)+300.)*           &
                             (grid%ts_p18l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u18l(n,i),                  &
                              grid%ts_v18l(n,i),                  &
                              grid%ts_q18l(n,i)*1000,             &
                              grid%ts_z19l(n,i),                  &
                              grid%ts_p19l(n,i),                  &
                            ( (grid%ts_t19l(n,i)+300.)*           &
                             (grid%ts_p19l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u19l(n,i),                  &
                              grid%ts_v19l(n,i),                  &
                              grid%ts_q19l(n,i)*1000,             &
                              grid%ts_z20l(n,i),                  &
                              grid%ts_p20l(n,i),                  &
                            ( (grid%ts_t20l(n,i)+300.)*           &
                             (grid%ts_p20l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u20l(n,i),                  &
                              grid%ts_v20l(n,i),                  &
                              grid%ts_q20l(n,i)*1000,             &
                              grid%ts_z21l(n,i),                  &
                              grid%ts_p21l(n,i),                  &
                            ( (grid%ts_t21l(n,i)+300.)*           &
                             (grid%ts_p21l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u21l(n,i),                  &
                              grid%ts_v21l(n,i),                  &
                              grid%ts_q21l(n,i)*1000,             &
                              grid%ts_z22l(n,i),                  &
                              grid%ts_p22l(n,i),                  &
                            ( (grid%ts_t22l(n,i)+300.)*           &
                             (grid%ts_p22l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u22l(n,i),                  &
                              grid%ts_v22l(n,i),                  &
                              grid%ts_q22l(n,i)*1000,             &
                              grid%ts_z23l(n,i),                  &
                              grid%ts_p23l(n,i),                  &
                            ( (grid%ts_t23l(n,i)+300.)*           &
                             (grid%ts_p23l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u23l(n,i),                  &
                              grid%ts_v23l(n,i),                  &
                              grid%ts_q23l(n,i)*1000,             &
                              grid%ts_z24l(n,i),                  &
                              grid%ts_p24l(n,i),                  &
                            ( (grid%ts_t24l(n,i)+300.)*           &
                             (grid%ts_p24l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u24l(n,i),                  &
                              grid%ts_v24l(n,i),                  &
                              grid%ts_q24l(n,i)*1000,             &
                              grid%ts_z25l(n,i),                  &
                              grid%ts_p25l(n,i),                  &
                            ( (grid%ts_t25l(n,i)+300.)*           &
                             (grid%ts_p25l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u25l(n,i),                  &
                              grid%ts_v25l(n,i),                  &
                              grid%ts_q25l(n,i)*1000,             &
                              grid%ts_z26l(n,i),                  &
                              grid%ts_p26l(n,i),                  &
                            ( (grid%ts_t26l(n,i)+300.)*           &
                             (grid%ts_p26l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u26l(n,i),                  &
                              grid%ts_v26l(n,i),                  &
                              grid%ts_q26l(n,i)*1000,             &
                              grid%ts_z27l(n,i),                  &
                              grid%ts_p27l(n,i),                  &
                            ( (grid%ts_t27l(n,i)+300.)*           &
                             (grid%ts_p27l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u27l(n,i),                  &
                              grid%ts_v27l(n,i),                  &
                              grid%ts_q27l(n,i)*1000,             &
                              grid%ts_z28l(n,i),                  &
                              grid%ts_p28l(n,i),                  &
                            ( (grid%ts_t28l(n,i)+300.)*           &
                             (grid%ts_p28l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u28l(n,i),                  &
                              grid%ts_v28l(n,i),                  &
                              grid%ts_q28l(n,i)*1000,             &
                              grid%ts_z29l(n,i),                  &
                              grid%ts_p29l(n,i),                  &
                            ( (grid%ts_t29l(n,i)+300.)*           &
                             (grid%ts_p29l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u29l(n,i),                  &
                              grid%ts_v29l(n,i),                  &
                              grid%ts_q29l(n,i)*1000,             &
                              grid%ts_z30l(n,i),                  &
                              grid%ts_p30l(n,i),                  &
                            ( (grid%ts_t30l(n,i)+300.)*           &
                             (grid%ts_p30l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u30l(n,i),                  &
                              grid%ts_v30l(n,i),                  &
                              grid%ts_q30l(n,i)*1000,             &
                              grid%ts_z31l(n,i),                  &
                              grid%ts_p31l(n,i),                  &
                            ( (grid%ts_t31l(n,i)+300.)*           &
                             (grid%ts_p31l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u31l(n,i),                  &
                              grid%ts_v31l(n,i),                  &
                              grid%ts_q31l(n,i)*1000,             &
                              grid%ts_z32l(n,i),                  &
                              grid%ts_p32l(n,i),                  &
                            ( (grid%ts_t32l(n,i)+300.)*           &
                             (grid%ts_p32l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u32l(n,i),                  &
                              grid%ts_v32l(n,i),                  &
                              grid%ts_q32l(n,i)*1000,             &
                              grid%ts_z33l(n,i),                  &
                              grid%ts_p33l(n,i),                  &
                            ( (grid%ts_t33l(n,i)+300.)*           &
                             (grid%ts_p33l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u33l(n,i),                  &
                              grid%ts_v33l(n,i),                  &
                              grid%ts_q33l(n,i)*1000,             &
                              grid%ts_z34l(n,i),                  &
                              grid%ts_p34l(n,i),                  &
                            ( (grid%ts_t34l(n,i)+300.)*           &
                             (grid%ts_p34l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u34l(n,i),                  &
                              grid%ts_v34l(n,i),                  &
                              grid%ts_q34l(n,i)*1000,             &
                              grid%ts_z35l(n,i),                  &
                              grid%ts_p35l(n,i),                  &
                            ( (grid%ts_t35l(n,i)+300.)*           &
                             (grid%ts_p35l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u35l(n,i),                  &
                              grid%ts_v35l(n,i),                  &
                              grid%ts_q35l(n,i)*1000,             &
                              grid%ts_z36l(n,i),                  &
                              grid%ts_p36l(n,i),                  &
                            ( (grid%ts_t36l(n,i)+300.)*           &
                             (grid%ts_p36l(n,i)/100000.)**0.287)  &
                                  - 273.15,                      &
                              grid%ts_u36l(n,i),                  &
                              grid%ts_v36l(n,i),                  &
                              grid%ts_q36l(n,i)*1000,             &
                              grid%ts_rainc(n,i),                &
                              grid%ts_rainnc(n,i)
! ----->> END STAGE RAL3.8.1R0 SOURCE UPDATE 1 - Will Cheng mods for time series calculation
! ===============================================================>
#else
! ============comment out by WC: 2009-05-21
! Use unformatted instead
!            WRITE(UNIT=iunit,FMT='(i2,f13.6,i5,i5,i5,1x,7(f13.5,1x))')  &
            WRITE(UNIT=iunit)                                    &        
                              grid%id, grid%ts_hour(n,i),        &
                              grid%id_tsloc(i), ix, iy,          &
                              grid%ts_t(n,i),                    &
                              grid%ts_q(n,i),                    &
                              grid%ts_u(n,i),                    &
                              grid%ts_v(n,i),                    &
                              grid%ts_psfc(n,i),                 &
                              grid%ts_tsk(n,i),                  &
                              grid%ts_tslb(n,i)
#endif

! ========= added by WC: 2009-05-20 ==========
1000     CONTINUE
! ============================================
         END DO

         CLOSE(UNIT=iunit)

      END DO

   END IF

   grid%next_ts_time = 1

END SUBROUTINE write_ts


#if (EM_CORE == 1)
SUBROUTINE calc_p8w(grid, ix, iy, p8w, k_start, k_end)

   USE module_domain
   USE module_model_constants

   IMPLICIT NONE

   ! Arguments
   TYPE (domain), INTENT(IN) :: grid
   INTEGER, INTENT(IN) :: ix, iy, k_start, k_end
   REAL, DIMENSION(k_start:k_end), INTENT(OUT) :: p8w

   ! Local variables
   INTEGER :: k
   REAL    :: z0, z1, z2, w1, w2 
   REAL, DIMENSION(k_start:k_end)   :: z_at_w
   REAL, DIMENSION(k_start:k_end-1) :: z


   DO k = k_start, k_end
      z_at_w(k) = (grid%phb(ix,k,iy)+grid%ph_2(ix,k,iy))/g
   END DO

   DO k = k_start, k_end-1
      z(k) = 0.5*(z_at_w(k) + z_at_w(k+1))
   END DO

   DO k = k_start+1, k_end-1
      p8w(k) = grid%fnm(k)*(grid%p(ix,k,iy)+grid%pb(ix,k,iy)) + &
               grid%fnp(k)*(grid%p(ix,k-1,iy)+grid%pb(ix,k-1,iy))
   END DO

   z0 = z_at_w(k_start)
   z1 = z(k_start)
   z2 = z(k_start+1)
   w1 = (z0 - z2)/(z1 - z2)
   w2 = 1. - w1
   p8w(k_start) = w1*(grid%p(ix,k_start,iy)+grid%pb(ix,k_start,iy)) + &
                  w2*(grid%p(ix,k_start+1,iy)+grid%pb(ix,k_start+1,iy))

   z0 = z_at_w(k_end)
   z1 = z(k_end-1)
   z2 = z(k_end-2)
   w1 = (z0 - z2)/(z1 - z2)
   w2 = 1. - w1
   p8w(k_end) = exp(w1*log(grid%p(ix,k_end-1,iy)+grid%pb(ix,k_end-1,iy)) + &
                    w2*log(grid%p(ix,k_end-2,iy)+grid%pb(ix,k_end-2,iy)))

END SUBROUTINE calc_p8w
#endif

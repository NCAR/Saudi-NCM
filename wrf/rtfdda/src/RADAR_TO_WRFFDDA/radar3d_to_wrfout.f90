!--------------------------------------------------------------
! Mei Xu  05-11-2010  (original code lost in 2/5/2010)
! 
! Functions:
!  - reading DZ, conduct basic temporal filling/filtering
!  - vertically (only) interpolate nsslMosaic3D to wrf grid 
!  - convert reflectivity into QR
!  - optional cloud analysis/adjustment of QR,QC,T, based on DZ
!  - output as wrffdda file (new or append to existing file
!          or insert the adjustment to wrfout 
! Caution:
!  - This code process wrfout/obs one timelevel at a time 
!   (previous code radar2wrffdda process allfiles in a RTFDDA cycle with one call)  
!  - Before running, check radar data to see whether DZ has YXZ or XYZ ordering
!  - Check to make sure the model height and obs height are defined in the same way (ASL)
!
! input :
!   - netcdf file of nsslMosaic3D (in raw vertical and wrf horizontal grid)
!   - wrf model file for verical grid info and background fields
!   time info extracted from wrf
! output :
!  - write fields in wrffdda format
!  - append 2 additional 3D fields RNW and SNOW to the model file 
!  - write out the model file 
! variables:
!  - DZ,
!
!  pgf90 -Mfree -I /opt/netcdf/include nsslMosaic3D_2_wrfout.F nc_utils.F -L /opt/netcdf/lib -lnetcdf -lnetcdff
!--------------------------------------------------------------
      program radar3d_to_wrfout
      use WRF_kinds
      use WRF_ncread
      use WRF_utils
      use netcdf

      implicit none
!include <netcdf.inc>

      INTERFACE
      SUBROUTINE READ_nsslDBZ(RADAR_DATE0,DOMAIN,INT_MN_NSSL,BADPT,&
          MX,MY,DZ,V_LEVEL,NZRAD,IDZ)
! input/output variables
      character*19:: radar_date0
      integer :: int_mn_nssl,mx,my
      real :: badpt
      real, pointer, dimension(:,:,:):: DZ
      real, pointer, dimension(:):: V_LEVEL
      character (len=1) :: domain
! local variables
      integer:: nx,ny,nzrad
      character(len=120):: fn_radar
      character*19:: radar_date1,radar_date2,newdate
      real, dimension(:,:,:), allocatable :: DZ1,DZ2
      integer::IDZ,IDZ1,IDZ2
      LOGICAL exist
      end subroutine 
      END  INTERFACE

      integer, parameter :: IYXZ = 1 ! =0, DZ is ordered as (nx,ny,nz); =1, DZ as (ny,nx,nz)
      integer, parameter :: IOUT = 1 ! =1, output WRF; =2, output FDDA file
! when IOUT==1, output WRF one for each input WRF, i.e, radar_mn = time interval in WRF files
      integer, parameter :: ICLOUD = 1 ! 0   QR is inserted to WRF
                                       ! 1   Sugimoto cloud analysis
                                       ! 2   modified cloud analysis
! time counter and arguments
      integer iargc
      external iargc
      integer iret, ncid,varid,ii,it,itime,ntime,n
      integer, parameter :: nmax=200,itimex=100
      character(len=120):: fn_model, fn_radar, fn_out, use, fn_model_i
      LOGICAL OPENED,exist
      character*10 :: beg_hh, end_hh, model_hh, radar_hh
      character (len= 2) :: cn_mfile, radar_mn
      integer ::n_mfile, int_mn, int_mn_nssl = 5

! original (merged) radar data
      real, parameter :: kappa=0.286, T0=273.15
      real, parameter :: DZmin=5, DZmax=60.0, BADPT=-99.0
      integer::nx,ny,nzrad,icnt
      real, pointer, dimension(:,:,:):: DZ
      real,pointer,dimension(:)::  v_level
      integer::IDZ
      character*19:: radar_date(nmax),radar_date1,radar_date2,newdate
      character*19:: model_date(itimex)
      integer,dimension(itimex):: matched 
      integer::n_radarfile,idtime,n_match
      real, dimension(:,:,:), allocatable :: QR_RAD,QS_RAD


! model variables
      integer :: nT,mx,my,mz,i,j,k
      character (len=19), dimension(:),allocatable :: file_time
      character (len=1) :: domain
      integer :: year,month,day,hour,minute

      real, dimension(:,:), allocatable :: TER
      real, dimension(:,:,:), allocatable :: ZZ
      real(R8), dimension(:,:,:,:), allocatable :: QR,QS,QC 
      real(R8), dimension(:,:,:,:), allocatable :: U,V,T,Q,PH,MU,P,PB
      real(R8), dimension(:,:,:), allocatable :: Z

! vertical interp variable
      integer :: nz
      real,allocatable,dimension(:,:,:):: dzh, dzm, slopef
      integer,allocatable,dimension(:,:,:):: kindex

! output variables
      real, allocatable, dimension(:,:,:,:)::QR_NDG_OLD,QR_NDG_NEW, &
         U_NDG_OLD,V_NDG_OLD,T_NDG_OLD,Q_NDG_OLD,PH_NDG_OLD,MU_NDG_OLD, &
         U_NDG_NEW,V_NDG_NEW,T_NDG_NEW,Q_NDG_NEW,PH_NDG_NEW,MU_NDG_NEW

! cloud analysis variables
      real, allocatable, dimension(:,:):: h_lcl,PQV,TT,TD
      real, allocatable, dimension(:,:,:):: QR_new,QC_new,QV_new,T_new
      real, parameter:: CP1=6.112, CV1=1.608, CT1=17.67, CT2=243.5, TT0=273.15
      real, parameter:: r_over_cp = 0.286, p0rcp_inv = 1.0/7.211
      real, parameter:: cpd_p = 1004, lv_p = 2.501E6, lf_p = 0.3335E6

      real, parameter:: rh_low = 50.0, rh_high = 90.0
      integer, parameter:: k_bot = 5
      real, parameter:: b1 = 0.9
      real, parameter:: qc01 = 5.0E-5, qc02 = 2.0E-4, qc03 = 3.5E-4, qc04 = 5.0E-4
      real, parameter:: qr01 = 1.0E-4, qr02 = 4.0E-4, qr03 = 7.0E-4, qr04 = 1.0E-3
      real :: TT1, QVS, QV_low, QV_high
!
      i=iargc()
      if(i < 7) then
        call getarg(0,use)
        j=index(use,' ')
        print*,'Usage: ',use(1:j),'filename fn_model n_mfile fn_out &
             beg_hh end_hh radar_mn '
        stop
      endif
!
      call getarg(1,fn_model)
      call getarg(2,cn_mfile)
      call getarg(3,fn_out)
      call getarg(4,beg_hh)
      call getarg(5,end_hh)
      call getarg(6,radar_mn)
      call getarg(7,domain)
      print*,' datasets ', fn_model, fn_out
      print*,' requested time ', beg_hh,' to ', end_hh &
      , ' every ',radar_mn,' minutes'
      read(radar_mn,"(i2.2)") int_mn
      read(cn_mfile,"(i2.2)") n_mfile
      
      if(beg_hh > end_hh) then
        print*, ' wrong time interval, stop and respecify',beg_hh,end_hh
        STOP
      endif
      if(int_mn < 5 ) then
        print*, 'too frequent output, stop and respecify', int_mn
        STOP
      endif
      if(int_mn > 60 ) then
        print*, ' output frequency too low, stop and respecify', int_mn
        STOP
      endif
!
! find out the specified output time levels - same as radar levels to be processed
!
      radar_hh = beg_hh
      n = 1
      radar_date(1) = radar_hh(1:4)//'-'//radar_hh(5:6)//'-'&
           //radar_hh(7:8)//'_'//radar_hh(9:10)//':00:00'
      idtime = int_mn * 60 
      do while ((radar_hh <= end_hh) .and. (n < nmax))
        radar_date1 = radar_date(n)
        n = n + 1
        call geth_newdate(newdate,radar_date1,idtime)
        radar_date(n) = newdate
        radar_hh=newdate(1:4)//newdate(6:7)//newdate(9:10)//newdate(12:13)
      enddo
      n_radarfile = n - 1
!     print*, 'requested time levels ',n_radarfile, &
!         (radar_date(n),n=1,n_radarfile)

! access model files for time info
! count model time levels within the specified period
!
      do i = 1, itimex
        matched(i)=0                   ! indicator for match time
      enddo
      itime=0
      
      print*,' get model time levels /dates'
! assume 1. one file may have more than one time level
!        2. Time levels in the files are in sequence and do not overlap  

      DO i = 0, n_mfile - 1
        write(cn_mfile,"(i2.2)") i
        fn_model_i = trim(fn_model)//"_"//cn_mfile(1:2)
        INQUIRE (file=fn_model_i,EXIST=exist)
        if(exist) then
          call get_dimension(trim(fn_model_i), "Time", nT)
          allocate(file_time(nT)) 
          call getCharTimes(fn_model_i,"Times",file_time)
          DO it = 1, nT
            itime = itime + 1
            model_date(itime) = file_time(it)
            model_hh=file_time(it)(1:4)//file_time(it)(6:7)// &         
            file_time(it)(9:10)//file_time(it)(12:13) 
            if((model_hh >= beg_hh) .and. (model_hh <= end_hh)) then
              matched(itime) = 1
            endif 
          ENDDO
          deallocate(file_time)
! end this file
        endif
      ENDDO 
      ntime = itime
!     print*, ((model_date(n),' ',matched(n)),n=1,ntime)

! matching the output (radar) time with the model files
!
      IF(ntime == 0 ) THEN
        print*,' no model output can be found, do nothing'
        RETURN
      ELSE IF(ntime == 1 ) THEN 
!
! if there is only one model hour, use it for grid info and T estimation
!    for all radar files
        matched(1) = n_radarfile
!       IF(matched(1) == 0) THEN
!         print*,' no model found for the time period, do nothing'
!         RETURN
!       ENDIF  

      ELSE
!
! else >1 model_hours exist, count the radar obs for each model hour
! matched(mtime) = # of radar times that are covered by this model hour
!
        DO n=1,ntime
          matched(n) = 0 
        ENDDO
        DO i=1,n_radarfile
          ii=1
          if (radar_date(i)>=model_date(ntime)) ii = ntime
          DO n=1,ntime-1
            if ( (radar_date(i)>=model_date(n)) & 
               .and. (radar_date(i)<model_date(n+1)) ) then
              ii=n
            endif
          ENDDO
          matched(ii) = matched(ii) + 1
        ENDDO
      ENDIF

      n_match = 0
      DO n=1,ntime
        n_match = n_match + matched(n)
!       if(matched(n)>0) print*,'model output at ',model_date(n),&
!                       ' will be used ',matched(n),' times'
        if(IOUT == 1 ) then
          if(matched(n) /= 1) then
            print*,'wrfout at ',model_date(n),' is matched with ',matched(n),' radar timelevel'
            STOP
          endif
        endif ! if IOUT==1
      ENDDO
      if(n_match == n_radarfile) then
        print*,'all radar levels covered by model background'
      else
        print*,'not all radar levels covered by model ?????'
        RETURN
      endif
         
! now time is sorted out, start processing the files......
!
! read model files, save the fields, read radar fields, do dz 2 qr
!
! reserve i,j,k for model domain indice hereafter
!
      itime=0                         ! counter for model time level
      icnt=0                          ! counter for radar time level
      n = 0                           ! counter for model files
      DO WHILE ( (n <= n_mfile-1) .and. (icnt <= n_radarfile) )
        write(cn_mfile,"(i2.2)") n
        fn_model_i = trim(fn_model)//"_"//cn_mfile(1:2)
        INQUIRE (file=fn_model_i,EXIST=exist)
        if(.not. exist) goto 201 

        if(n == 0) then
! check model dimension
          call getNCsize(fn_model_i,mx,my,mz)
          print*,' model dimension (non-staggered): ',mx,my,mz

          if (IOUT == 2) then
! initalize arrays for gfdda
          allocate(QR_NDG_OLD(mx,my,mz,n_radarfile-1))
          allocate(QR_NDG_NEW(mx,my,mz,n_radarfile-1))

          allocate(U_NDG_OLD(mx,my,mz,n_radarfile-1))
          allocate(V_NDG_OLD(mx,my,mz,n_radarfile-1))
          allocate(T_NDG_OLD(mx,my,mz,n_radarfile-1))
          allocate(Q_NDG_OLD(mx,my,mz,n_radarfile-1))
          allocate(PH_NDG_OLD(mx,my,mz,n_radarfile-1))
          allocate(MU_NDG_OLD(mx,my,1,n_radarfile-1))

          allocate(U_NDG_NEW(mx,my,mz,n_radarfile-1))
          allocate(V_NDG_NEW(mx,my,mz,n_radarfile-1))
          allocate(T_NDG_NEW(mx,my,mz,n_radarfile-1))
          allocate(Q_NDG_NEW(mx,my,mz,n_radarfile-1))
          allocate(PH_NDG_NEW(mx,my,mz,n_radarfile-1))
          allocate(MU_NDG_NEW(mx,my,1,n_radarfile-1))
          endif
        endif

! load model fields
        call get_dimension(fn_model_i, "Time", nT)
        allocate(file_time(nT))
        allocate(U(mx+1,my,mz,nT))
        allocate(V(mx,my+1,mz,nT))
        allocate(T(mx,my,mz,nT))
        allocate(Q(mx,my,mz,nT))
        allocate(PH(mx,my,mz+1,nT))
        allocate(MU(mx,my,1,nT))
        call ncread_field4dG(fn_model_i,'U', rfld=U)
        call ncread_field4dG(fn_model_i,'V', rfld=V)
        call ncread_field4dG(fn_model_i,'T', rfld=T)
        call ncread_field4dG(fn_model_i,'QVAPOR', rfld=Q)
        call ncread_field4dG(fn_model_i,'PH', rfld=PH)
        call ncread_field4dG(fn_model_i,'MU', rfld=MU,dim3i=1)

        allocate(P(mx,my,mz,nT))
        allocate(PB(mx,my,mz,nT))
        allocate(QR(mx,my,mz,nT))
        allocate(QC(mx,my,mz,nT))
        call ncread_field4dG(fn_model_i,'P', rfld=P)
        call ncread_field4dG(fn_model_i,'PB', rfld=PB)
        call ncread_field4dG(fn_model_i,'QRAIN', rfld=QR)
        call ncread_field4dG(fn_model_i,'QCLOUD', rfld=QC)
        P = (P + PB) * 0.01  ! mb
        deallocate(PB)

        it = 1
        DO it = 1, nT
         itime = itime + 1

! compute vertical height for wrf for this time level
!
         allocate(Z(mx,my,mz))
         call wrf_hgt(fn_model_i,it,Z)
         allocate(ZZ(mx,my,mz))
         ZZ = Z
         deallocate(Z)

! reading/processing radar data 
!
         DO ii = 1, matched(itime)
          icnt = icnt + 1
          radar_date1 = radar_date(icnt)
!
! read in nssl mosaic dbz obs (merged for the domain)
!   do not read if horizontal dimensions do not match those of the model
! return DZ as well as the vertical levels
! do temporal interp/filtering using the datasets before and after
! if IDZ = 0 is returned, then no radar data can be found 
!
          call read_nsslDBZ(radar_date1,domain,int_mn_nssl,badpt,&
               mx,my,DZ,v_level,nzrad,idz)

! v_level in meter asl
          v_level(:) = 1000.0 * v_level(:) 
! 
! map DZ onto model grid
          allocate(dzm(mx,my,mz))
          IF (IDZ == 0 ) THEN 
            dzm = BADPT
          ELSE 
! rotate dimensions of DZ (which follows MM5 convention) for WRF
            allocate(dzh(mx,my,nzrad))
            DO i = 1, mx
            DO j = 1, my
            DO k = 1, nzrad
              DZH(i,j,k) = DZ(j,i,k)
            ENDDO
            ENDDO
            ENDDO
! vertical interp   
            allocate(kindex(mx,my,mz))
            allocate(slopef(mx,my,mz))
            call zparamsM(mx,my,mz,zz,nzrad,v_level,slopef,kindex)
            call zinterpM(mx,my,nzrad,dzh,mz,slopef,kindex,dzm,BADPT)
            deallocate(kindex)
            deallocate(slopef)
            deallocate(dzh)
            deallocate(dz)
            deallocate(v_level)
          ENDIF ! if IDZ

! convert DZ to QR/QS
          nx=mx
          ny=my 
          nz = mz 
          allocate(QR_RAD(nx,ny,nz))
          call DZ_2_QR(1,nx,ny,nz,DZM,QR_RAD,BADPT)
!         call DZ_2_QR(1,nx,ny,nz,DZM,T,QR_RAD,QS_RAD,BADPT)
          QR_RAD = QR_RAD * 0.001
!
! meixu 2010-5-13
! Modify model fields (U,V,Q,T,QR,QS,QC)  using the observations QRN(:,:,:) (presently in g/kg)
! at this obs time level 

          IF (ICLOUD == 0) then
            print*, 'insert radar QR into model'
            allocate(QR_new(mx,my,mz))
            QR_new = QR_RAD 

          ELSE IF (ICLOUD == 1) then 
            print*, 'Sugimoto cloud analysis based on radar QR '

            allocate(QR_new(mx,my,mz))
            allocate(QC_new(mx,my,mz))
            allocate(QV_new(mx,my,mz))
            allocate(T_new(mx,my,mz))
!
! a simple cloud analysis scheme to modify QR, QC, Q(VAPOR), T
! Scheme similar to Soichiro Sugimoto
! Note:  The wrf output T field is purturbation potential temp. (T0=300.0)
!
! (0) find the lcl from surface temp and dew-point:    
!     h_LCL = (T-T_d)/(gamma_d - gamma_dew) = 125 (T - T_d)  !m  above lowest model level
!     T_d = 243.5 * ln(e/6.112) /(17.67-ln(e/6.112)) 
             
             allocate(h_LCL(mx,my))
             allocate(PQV(mx,my))
             allocate(TD(mx,my))
             allocate(TT(mx,my))

! vapor pressure (lowest model level)
             PQV(:,:) = CV1 * Q(:,:,1,1) * P(:,:,1,1)
! dew point (Celsius)
             TD(:,:) = CT2 * (log(PQV(:,:)) - 1.81025) /(CT1 - log(PQV(:,:)) + 1.81025) 
! temperature
             TT(:,:) = (T(:,:,1,1) + 300.0) * P(:,:,1,1)**r_over_cp * p0rcp_inv - TT0
! level of condensation
             h_LCL(:,:) = 125.0 * (TT(:,:) - TD(:,:)) + ZZ(:,:,1) + 500.0

! (1) modify QR

            QR_new(:,:,:) = QR(:,:,:,1)
            do i = 1, mx
            do j = 1, my
            do k = 1, mz
               if(QR_RAD(i,j,k) .gt.  5.0E-5) then
                 QR_new(i,j,k) = b1 * QR(i,j,k,1) + (1.0-b1) * QR_RAD(i,j,k)
               else
                 QR_new(i,j,k) = QR_RAD(i,j,k)
               endif
             enddo
             enddo
             enddo

! (2) modify T
            T_new = 0.0
            do k = k_bot, mz
            do i = 1, mx
            do j = 1, my
              if(ZZ(i,j,k) .gt. h_LCL(i,j)) then
!
! if adding latent heating proportional to QR_RAD
                if(QR_RAD(i,j,k) .gt. 5.0E-5) then
                  T_new(i,j,k) = QR_RAD(i,j,k) * ((lv_p+lf_p)/cpd_p)
                endif

! Using modified Sugimoto computation of differential latent heat
! total local rate of change = condensation + advection;  i.e.,
!   condensation = scan-to-scan change - advection change 

!               heating in the model background
                
!               heating in the observations
                

              endif
            enddo
            enddo
            enddo

! (3) modify RH and QC
!     rh_low = 70.0, rh_high = 90.0
!     qc01 = 5.0E-5, qc02 = 2.0E-4, qc03 = 3.5E-4, qc04 = 5.0E-4
!     qr01 = 1.0E-4, qr02 = 4.0E-4, qr03 = 7.0E-4, qr04 = 1.0E-3

            QV_new(:,:,:) = Q(:,:,:,1)
            QC_new(:,:,:) = QC(:,:,:,1)
            do k = k_bot, mz
            do i = 1, mx
            do j = 1, my
              if(ZZ(i,j,k) .gt. h_LCL(i,j)) then
                TT1 = (T(i,j,k,1) + 300.0) * P(i,j,k,1)**r_over_cp * p0rcp_inv - TT0
                QVS = CV1 * CP1 * exp(CT1 * TT1 /(CT2+TT1)) * P(i,j,k,1)
                QV_low = rh_low * QVS
                QV_high = rh_high * QVS
                if (T_new(i,j,k) .gt. 0.0 .and. Q(i,j,k,1) .lt. QV_low) then
                  QV_new(i,j,k) = QV_low
                else if (T_new(i,j,k) .le. 0.0 .and. Q(i,j,k,1) .gt. QV_high) then
                  QV_new(i,j,k) = QV_high
                endif

                if(QR(i,j,k,1).gt.qr04 .and. QC(i,j,k,1).lt.qc04) then
                  QC_new(i,j,k) = qc04
                else if (QR(i,j,k,1).gt.qr03 .and. QC(i,j,k,1).lt.qc03) then
                  QC_new(i,j,k) = qc03
                else if (QR(i,j,k,1).gt.qr02 .and. QC(i,j,k,1).lt.qc02) then 
                  QC_new(i,j,k) = qc02
                else if (QR(i,j,k,1).gt.qr01 .and. QC(i,j,k,1).lt.qc01) then 
                  QC_new(i,j,k) = qc01
                endif
              endif
            enddo
            enddo
            enddo
            
          ENDIF  ! if ICLOUD

          if (IOUT == 2) then
          print*, 'update gfdda variables with radar added for',&
            radar_date(icnt-1)(1:19),' domain',domain 

          IF(icnt == 1) THEN
            QR_NDG_OLD(:,:,:,icnt) = QR_RAD(:,:,:)

            U_NDG_OLD(:,:,:,icnt) = U(:,:,:,it)
            V_NDG_OLD(:,:,:,icnt) = V(:,:,:,it)
            T_NDG_OLD(:,:,:,icnt) = T(:,:,:,it)
            Q_NDG_OLD(:,:,:,icnt) = Q(:,:,:,it)
            PH_NDG_OLD(:,:,:,icnt) = PH(:,:,:,it)
            MU_NDG_OLD(:,:,:,icnt) = MU(:,:,:,it)

          ELSE IF(icnt < n_radarfile) THEN ! icnt > 1
            QR_NDG_OLD(:,:,:,icnt)= QR_RAD(:,:,:)
            QR_NDG_NEW(:,:,:,icnt-1) = QR_NDG_OLD(:,:,:,icnt)

            U_NDG_OLD(:,:,:,icnt) = U(:,:,:,it)
            V_NDG_OLD(:,:,:,icnt) = V(:,:,:,it)
            T_NDG_OLD(:,:,:,icnt) = T(:,:,:,it)
            Q_NDG_OLD(:,:,:,icnt) = Q(:,:,:,it)
            PH_NDG_OLD(:,:,:,icnt) = PH(:,:,:,it)
            MU_NDG_OLD(:,:,:,icnt) = MU(:,:,:,it)

            U_NDG_NEW(:,:,:,icnt-1) = U_NDG_OLD(:,:,:,icnt)
            V_NDG_NEW(:,:,:,icnt-1) = V_NDG_OLD(:,:,:,icnt)
            T_NDG_NEW(:,:,:,icnt-1) = T_NDG_OLD(:,:,:,icnt)
            Q_NDG_NEW(:,:,:,icnt-1) = Q_NDG_OLD(:,:,:,icnt)
            PH_NDG_NEW(:,:,:,icnt-1) = PH_NDG_OLD(:,:,:,icnt)
            MU_NDG_NEW(:,:,:,icnt-1) = MU_NDG_OLD(:,:,:,icnt)
          ENDIF

          endif  ! if IOUT

         ENDDO ! ii=match(it) for model time it
         print*, 'finish this model time, radarfile count ',icnt 
         deallocate(ZZ)
         deallocate(dzm)
        ENDDO  ! for model file i
        print*, 'finish this model file, proceed with the next, n=',n 

        if (IOUT == 1 ) then
! output modified wrf
!         call rewrite_wrf(fn_model_i,1,QR_new,"QRAIN",mx,my,mz,1)
          call rewrite_wrf_2(fn_model_i,QR_new,T_new,"QRAIN","T",mx,my,mz,1)
!         call rewrite_wrf(fn_model_i,4,QR_new,QC_new,QV_new,T_new,"QRAIN","QCLOUD","QVAPOR","T",mx,my,mz,1)
          if(ICLOUD == 0) deallocate(QR_new)
          if(ICLOUD == 1) deallocate(QR_new,QC_new,QV_new,T_new)
        endif

        if(itime >= ntime) then
          print*, ' all model time levels are processed'
          print*, ' radar timelevels processed',icnt,n_radarfile
        endif

 201    CONTINUE
        n = n + 1
      ENDDO ! end of WHILE for all model files
      print*, ' all model files are processed, nfile = ',n
 
! The output time levels = n_radarfile - 1
!   use this to output if all the time levels are loaded into the arrays

      if (IOUT == 2 ) then
!
      call out_fdda_QR(trim(fn_out),mx,my,mz, &
       U_NDG_OLD,V_NDG_OLD,T_NDG_OLD,Q_NDG_OLD, &
       QR_NDG_OLD,PH_NDG_OLD,MU_NDG_OLD, &
       U_NDG_NEW,V_NDG_NEW,T_NDG_NEW,Q_NDG_NEW, &
       QR_NDG_NEW,PH_NDG_NEW,MU_NDG_NEW,radar_date,icnt)

      deallocate(QR_NDG_OLD,QR_NDG_NEW)
      endif ! IOUT==2

      end program radar3d_to_wrfout

      SUBROUTINE DZ_2_QR(IP,nx,ny,nz,DZ,QR,BADPT)
      implicit none
      real, parameter :: DZmin=5, DZmax=60.0
      real, parameter :: AA1=43.1, BB1=17.5, AA2=30.89, BB2=17.5
      real :: BADPT, AA, BB
      integer :: nx,ny,nz,ip,i,j,k 
!
! convert the obs refl data (dBZ) to rainwater/snow mixing ratio (g/kg)
! microphysics assumptions
! IP = 0 : warm rain
! IP = 1 : snow - wet snow at lowest 3 km; dry snow aloft
! IP = 2 : snow - wet snow at lowest 3 km; dry snow aloft
!  
      real DZ(nx,ny,nz),QR(nx,ny,nz)

      IF(IP.eq.2) THEN
        AA = AA2
        BB = BB2
      ELSE
        AA = AA1
        BB = BB1
      ENDIF

      DO K=1,NZ
!
! compute QR
        do i=1,nx
        do j=1,ny
          if(DZ(i,j,k).ne.BADPT) THEN
            if(DZ(i,j,k).lt.DZMIN)  then
              qr(i,j,k) = 0.0
            else if(DZ(i,j,k).lt.DZMAX)  then
              qr(i,j,k) = 10.**((DZ(i,j,k)-AA)/BB) 
            else
              print*,' strong reflectivity -hails ?',DZ(i,j,k)
              print*,' at ',i,j,k,DZ(i,j,k)
              qr(i,j,k) = 10.**((DZMAX-AA)/BB)
            
            endif
          else
            qr(i,j,k)=BADPT
!           qr(i,j,k)=0.001
          endif
        enddo
        enddo
      ENDDO
      RETURN
      END

      SUBROUTINE READ_nsslDBZ(RADAR_DATE0,DOMAIN,INT_MN_NSSL,BADPT,&
          MX,MY,DZ,V_LEVEL,NZRAD,IDZ)

! input/output variables
      
      character*19:: radar_date0
      integer :: int_mn_nssl,mx,my
      real :: badpt
      real, pointer, dimension(:,:,:):: DZ
      real, pointer, dimension(:):: V_LEVEL
      character (len=1) :: domain

! local variables
      integer:: nx,ny,nzrad
      character(len=120):: fn_radar 
      character*19:: radar_date1,radar_date2,newdate
      real, dimension(:,:,:), allocatable :: DZ1,DZ2
      integer::IDZ,IDZ1,IDZ2
      LOGICAL exist

! access the (merged) radar data on the model's horizontal domain 
      fn_radar = trim(radar_date0//'_DBZ_D'//domain//'.nc')
      IDZ = 1   ! assume good data
      INQUIRE (file=fn_radar,EXIST=exist)
      if( .not. exist) then
         print*,'radar dataset ',trim(fn_radar),' does not exist'
         IDZ = 0
         goto 301 ! to filter/interp
       endif
!      print*,'radar dataset ',trim(fn_radar),' exists'
!
! get sizes of the merged radar file.
!
       call getdim(fn_radar,ny,nx,nzrad,3,"dz")
       print*,' radardata dimension:',nx,ny,nzrad
       IF((mx.NE.nx).OR.(my.NE.ny)) THEN
         print*,'model dimension:',mx,my,' radardata dimension:',nx,ny
         print*,'radar data was not prepared for this model grid,stop'
         IDZ = 0
         goto 301 ! to filter/interp 
!        STOP
       ENDIF

       allocate(DZ(ny,nx,nzrad))
       allocate(v_level(nzrad))
       DZ=BADPT
       call readnetcdf(fn_radar,dz,"dz",v_level,"Level",ny,nx,nzrad)
!      call read_dbz_netcdf(fn_radar,dz,"dz",ny,nx,nzrad)
       print*,'radar dataset ',trim(fn_radar),' is read'

 301   CONTINUE
!
! filtering/temporal interpolation 
! using the nssl file before and after
!
       radar_date1 = radar_date0
       idtime = -1 * int_mn_nssl * 60
       call geth_newdate(newdate,radar_date1,idtime)
       radar_date1 = newdate

       radar_date2 = radar_date0
       idtime = int_mn_nssl * 60
       call geth_newdate(newdate,radar_date2,idtime)
       radar_date2 = newdate
       print*,'filtering with ',radar_date1,' and', radar_date2

       fn_radar = trim(radar_date1(1:19)//'_DBZ_D'//domain//'.nc')
       IDZ1=0   ! assume no data
       INQUIRE (file=fn_radar,EXIST=exist)
       if( exist) then
         print*,'radar dataset ',trim(fn_radar),' exist'
         call getdim(fn_radar,ny,nx,nzrad,3,"dz")
         IF((mx.EQ.nx).and.(my.EQ.ny)) THEN
           IDZ1 = 1
           allocate(DZ1(ny,nx,nzrad))
           DZ1=BADPT
!         call readnetcdf(fn_radar,dz1,"dz",v_level,"Level",ny,nz,nzrad)
           call read_dbz_netcdf(fn_radar,dz1,"dz",ny,nx,nzrad)
!          print*,'radar dataset ',trim(fn_radar),' is read'
         ELSE
           print*,'radar dataset ',trim(fn_radar),' does not match size'
         ENDIF
       endif

       fn_radar = trim(radar_date2(1:19)//'_DBZ_D'//domain//'.nc')
       IDZ2=0   ! assume no data
       INQUIRE (file=fn_radar,EXIST=exist)
       if( exist) then
         print*,'radar dataset ',trim(fn_radar),' exist'
         call getdim(fn_radar,ny,nx,nzrad,3,"dz")
         IF((mx.EQ.nx).and.(my.EQ.ny)) THEN
           IDZ2 = 1
           allocate(DZ2(ny,nx,nzrad))
           DZ2=BADPT
           call read_dbz_netcdf(fn_radar,dz2,"dz",ny,nx,nzrad)
!         call readnetcdf(fn_radar,dz2,"dz",v_level,"Level",ny,nx,nzrad)
!          print*,'radar dataset ',trim(fn_radar),' is read'
         ELSE
           print*,'radar dataset ',trim(fn_radar),' does not match size'
         ENDIF
       endif  

       IF(IDZ.EQ.0) THEN
! if DZ is not already defined, define if one of the datasets exist
! (and the dimensions can be determined)
         IF((IDZ1.NE.0).or.(IDZ2.NE.0)) THEN
           allocate(DZ(ny,nx,nzrad))
           DZ=BADPT
         ENDIF 
       ENDIF 
       
! filtering 
       IF ((IDZ1.EQ.0).and.(IDZ2.EQ.0)) THEN
         print*,' no datasets for temporal interp/filtering'
       ELSE IF (IDZ1.EQ.0) THEN
! 2nd dataset exists
         IF (IDZ.EQ.0) THEN
           DZ = DZ2
         ENDIF
       ELSE IF (IDZ2.EQ.0) THEN
! 1st dataset exists
         IF (IDZ.EQ.0) THEN
           DZ = DZ1
         ENDIF 
       ELSE
! both datasets exist
         IF (IDZ.EQ.0) THEN
           DO i = 1, ny
           DO j = 1, nx
           DO k = 1, nzrad
           if(dz1(i,j,k).ne.badpt.and.dz2(i,j,k).ne.badpt) then
             dz(i,j,k) = 0.5* (dz1(i,j,k) + dz2(i,j,k))
           endif
           ENDDO
           ENDDO
           ENDDO
         ELSE
           DO i = 1, ny
           DO j = 1, nx
           DO k = 1, nzrad
           if(dz1(i,j,k).ne.badpt.and.dz2(i,j,k).ne.badpt) then
             if(dz(i,j,k).ne.badpt) then
               dz(i,j,k) = 0.25* (dz1(i,j,k) + dz2(i,j,k) + 2.0*dz(i,j,k))
             else
               dz(i,j,k) = 0.5* (dz1(i,j,k) + dz2(i,j,k))
             endif 
           else if (dz1(i,j,k).ne.badpt) then
             if(dz(i,j,k).ne.badpt) dz(i,j,k) = dz1(i,j,k)
           else if (dz2(i,j,k).ne.badpt) then
             if(dz(i,j,k).ne.badpt) dz(i,j,k) = dz2(i,j,k)
           endif 
           ENDDO
           ENDDO
           ENDDO
         ENDIF
       ENDIF  
       IF(IDZ1.NE.0)deallocate(DZ1) 
       IF(IDZ2.NE.0)deallocate(DZ2) 
       print*,'finish filtering'
       END

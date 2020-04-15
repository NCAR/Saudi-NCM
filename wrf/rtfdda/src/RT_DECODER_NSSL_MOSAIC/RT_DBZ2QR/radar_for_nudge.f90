!--------------------------------------------------------------
! Mei Xu  03-20-2006
!
! read in a model forecast, find the specified hours  
! search for datasets of grided radar dbz for the time 
! convert dbz to RNW/SNOW based on T3D in the mm5v3 file 
! append 2 additional 3D fields RNW and SNOW to the model file 
! write out the model file 
!--------------------------------------------------------------
      program radar_for_nudge
!
      integer, parameter :: iun1=10,iout=31,filedone=0,nmax=200,itimex=100
      character*60 :: fn_mm5, fn_radar, fn_out, use, fn_mm5i
      LOGICAL OPENED,exist
      character*10 :: beg_hh, end_hh, mm5_hh, radar_hh, newdate
      character (len= 2) :: cn_mm5file, radar_mn
      integer ::n_mm5file, int_mn 

      real, parameter :: kappa=0.286, T0=273.15
      real, parameter :: DZmin=-5, DZmax=60.0, BADPT=-999.0
      real, parameter :: AA1=43.1, BB1=17.5, AA2=30.89, BB2=17.5
      integer, parameter :: ireplace=1

! radar variables
      character*24:: radar_date(nmax) 
      character*24:: mm5_date(itimex) 
      integer,dimension(itimex):: matched 
      integer::n_radarfile,mntime,n_match
      real, dimension(:,:,:), allocatable :: DZ,QRN,QSN,DZN

! mm5 variables
      integer, dimension(50,20) :: bhi
      real, dimension(20,20) :: bhr
      character(len=80), dimension(50,20) :: bhic
      character(len=80), dimension(20,20) :: bhrc
      integer :: flag, ndim
      real :: time
! 
      integer, dimension(4) :: start_index, end_index
      character (len=80) :: exe_name
      character (len= 4) :: staggering
      character (len= 4) :: ordering
      character (len=24) :: start_date
      character (len=24) :: current_date
      character (len= 9) :: name
      character (len=25) :: units
      character (len=46) :: description
      character (len= 4) :: yyyy
      character (len= 2) :: mm,dd,hh,mn
      integer :: AllocateStatus
      integer :: domain_id
      character (len=1) :: domain
      integer :: year,month,day,hour,minute
      real, dimension(:), allocatable :: SIGMH
      real, dimension(:,:), allocatable :: PSTAR,TER,DMAP,XMAP,COR
      real, dimension(:,:,:), allocatable :: U,V,T,Q,QC,QR,QS,PP
      integer :: end_index1,end_index2,end_index3
      integer :: ierr, ier, ifclw, ifrnw, ifsnow
      character (len=24) :: current_date1
      integer :: idtime_sec
      real :: dtime_min
!
      i=iargc()
      if(i < 5) then
        call getarg(0,use)
        j=index(use,' ')
        print*,'Usage: ',use(1:j),'filename fn_mm5 n_mm5file fn_out &
             beg_hh end_hh radar_mn '
        stop
      endif
!
      call getarg(1,fn_mm5)
      call getarg(2,cn_mm5file)
      call getarg(3,fn_out)
      call getarg(4,beg_hh)
      call getarg(5,end_hh)
      call getarg(6,radar_mn)
!     print*,' datasets ', fn_mm5, fn_out
!     print*,' requested time ', beg_hh,' to ', end_hh &
!     , ' every ',radar_mn,' minutes'
      read(radar_mn,"(i2.2)") int_mn
      read(cn_mm5file,"(i2.2)") n_mm5file
      
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
! find out time levels of output - same as radar time levels
!
      n_hourly = 60/int_mn
      n = 0
      radar_hh = beg_hh
      do while (radar_hh <= end_hh)
      do i=1, n_hourly
        itime = (i-1)* int_mn 
        write(radar_mn,"(i2.2)") itime
        n = n + 1
        radar_date(n)(1:4)=radar_hh(1:4)
        radar_date(n)(5:5) = '-'
        radar_date(n)(6:7)=radar_hh(5:6)
        radar_date(n)(8:8) = '-'
        radar_date(n)(9:10)=radar_hh(7:8)
        radar_date(n)(11:11) = '_'
        radar_date(n)(12:13)=radar_hh(9:10)
        radar_date(n)(14:14) = ':'
        radar_date(n)(15:16)=radar_mn(1:2)
        radar_date(n)(17:24) = ':00.0000'
      enddo
      call advance_hh(radar_hh, 1, newdate)  
      radar_hh = newdate
      enddo
      n_radarfile = n
!     print*, 'requested time levels ',n_radarfile, &
!         (radar_date(n),n=1,n_radarfile)


! access mm5 files

      DO i = 0, n_mm5file - 1
        write(cn_mm5file,"(i2.2)") i
        fn_mm5i = trim(fn_mm5)//"_"//cn_mm5file(1:2)
        IUN = IUN1 + i
        print*,' opening mm5 file ',fn_mm5i,' on ',IUN
        INQUIRE (file=fn_mm5i,EXIST=exist)
        if(exist) then
          open(IUN,file=fn_mm5i,status='old',form='unformatted')
        else 
          print *,'there are no mm5 file to open, stop'
          goto 404
        endif
      ENDDO

! count mm5 time levels in the specified period
!
      print*,' get mm5 time levels /dates'
      call read_dates (iun1,n_mm5file,itimex,mm5_date, &
                       beg_hh,end_hh,ntime,matched)
!     print*, ((mm5_date(n),' ',matched(n)),n=1,ntime)

! matching the output (radar) time with the mm5 files
      IF(ntime == 1 ) THEN
        IF(matched(1) == 0) THEN
          print*,' no mm5 found for the time period, do nothing'
        ELSE
          matched(1) = n_radarfile
        ENDIF  
      ELSE
        DO n=1,ntime
          matched(n) = 0 
        ENDDO
        DO i=1,n_radarfile
          ii=1
          if (radar_date(i)>=mm5_date(ntime)) ii = ntime
          DO n=1,ntime-1
            if ( (radar_date(i)>=mm5_date(n)) & 
               .and. (radar_date(i)<mm5_date(n+1)) ) then
              ii=n
            endif
          ENDDO
          matched(ii) = matched(ii) + 1
        ENDDO
      ENDIF
      n_match = 0
      DO n=1,ntime
        n_match = n_match + matched(n)
      if(matched(n)>0)print*,'mm5 tobe used:',mm5_date(n),' ',matched(n)
      ENDDO
         
! read mm5 files, save the fields, read radar fields, do dz 2 qr
!
      DO i = 0, n_mm5file - 1
        IUN = IUN1 + i
        rewind(IUN)
      ENDDO
      open(iout,file=fn_out,status='new',form='unformatted')

      itime=0                           ! counter for mm5 output level
      icnt=0                            ! counter for matched time
      ifclw=0
      ifrnw=0
      ifsnow=0
     
      i_mm5file = 1
      IUN=IUN1 + i_mm5file -1 
 101  read(IUN,iostat=ierr,end=201) flag
      if(ierr/=0) then
        write(*,'("Error reading mm5 flag")') 
        go to 404
      endif
      goto 202 
 201  if(i_mm5file < n_mm5file) then
        i_mm5file = i_mm5file + 1
        IUN = IUN1 + i_mm5file -1
        goto 101
      else
        goto 404
      endif
 202  continue

      if(flag == 0) then
        read(IUN,iostat=ier) bhi, bhr, bhic, bhrc
        if(ier/=0) then
          write(*,'("Error reading big header")')
          go to 404
        endif
        go to 101
      elseif(flag == 1) then
! proceed with data field
        read(IUN,iostat=ier) ndim, start_index, end_index, time, &
            staggering, ordering, current_date, name, units, description
        if(ier/=0) then
           write(*,'("Error reading subheader")')
           go to 404
        endif

        ix=end_index(1)
        jx=end_index(2)
        kx=end_index(3)
        dx=bhr(9,1)*0.001      !! in km
        xlatc=bhr(2,1)
        xlonc=bhr(3,1)
        domain_id=bhi(13,1)
        write(domain,'(i1)')domain_id

        if(matched(itime+1) > 0) then
!
            if(name(1:4) == 'U   ') then 
              allocate(U(ix,jx,kx))
              read(IUN) U
              nx=ix-1
              ny=jx-1
              nz=kx
            elseif(name(1:4) == 'V   ') then 
              allocate(V(ix,jx,kx))
              read(IUN) V
            elseif(name(1:4) == 'T   ') then
              allocate(T(ix,jx,kx))
              read(IUN) T
            elseif(name(1:4) == 'Q   ') then 
              allocate(Q(ix,jx,kx))
              read(IUN) Q
            elseif(name(1:4) == 'CLW ') then
              allocate(QC(ix,jx,kx))
              read(IUN) QC 
              ifclw=1
            elseif(name(1:4) == 'RNW ') then
              allocate(QR(ix,jx,kx))
              read(IUN) QR
              ifrnw=1
            elseif(name(1:6) == 'SNOW  ') then
              allocate(QS(ix,jx,kx))
              read(IUN) QS
              ifsnow=1
            elseif(name(1:4) == 'PP  ') then
              allocate(PP(ix,jx,kx))
              read(IUN) PP
            elseif(name(1:4) == 'PSTA') then
              allocate(PSTAR(ix,jx))
              read(IUN) PSTAR
            elseif(name(1:4) == 'TERR') then
              allocate(TER(ix,jx))
              read(IUN) TER
            elseif(name(1:8) == 'MAPFACDT') then
              allocate(DMAP(ix,jx))
              read(IUN) DMAP 
            elseif(name(1:8) == 'MAPFACCR') then
              allocate(XMAP(ix,jx))
              read(IUN) XMAP 
            elseif(name(1:8) == 'CORIOLIS') then
              allocate(COR(ix,jx))
              read(IUN) COR
            elseif(name(1:4) == 'SIGM') then
              allocate(SIGMH(ix))
              read(IUN) SIGMH
            else
              read(IUN) 
            endif
        else 
!         print*, ' skipping field ',name(1:4),' at time',current_date 
          read(IUN) 
        endif
! end icnt
        go to 101
!
      elseif(flag == 2) then
        itime = itime + 1
!
! read in radar obs and does output here ???
!
        do m = 1, matched(itime)
          icnt = icnt + 1
          fn_radar = radar_date(icnt)(1:19)//'_DBZ_D'//domain//'.nc'
          fn_radar = trim(fn_radar)
          INQUIRE (file=fn_radar,EXIST=exist)
          if(exist) then
            print*,'radar dataset ',fn_radar,' exists'
          else
            print*,'radar dataset ',fn_radar,' does not exist'
          endif
          
! convert reflectivity to mixing ratio
          if(icnt == 1) then
            allocate(DZ(nx,ny,nz))
            allocate(QRN(nx+1,ny+1,nz))
            allocate(QSN(nx+1,ny+1,nz))
            allocate(DZN(nx+1,ny+1,nz))
          endif 
          DZ=BADPT
          QRN=BADPT
          QSN=BADPT
          DZN=BADPT
          call read_dbz_netcdf(fn_radar,dz,"dz",nx,ny,nz)
          do i = 1, nx
          do j = 1, ny
          do k = 1, nz
            IF(DZ(i,j,k).eq.BADPT) THEN
              QRN(i,j,k) = BADPT
              QSN(i,j,k) = BADPT
            ELSE
              if(DZ(i,j,k).lt.DZMIN) then
                QRN(i,j,k) = 0.0
                QSN(i,j,k) = 0.0 
              else if(DZ(i,j,k).gt.DZMAX) then 
                QRN(i,j,k) = BADPT
                QSN(i,j,k) = BADPT 
              else
                IF (T(i,j,k) > T0) THEN
                  QRN(i,j,k) = 0.001 * 10.**((DZ(i,j,k)-AA1)/BB1)
                  QSN(i,j,k) = 0.0 
                ELSE
                  QRN(i,j,k) = 0.0
                  QSN(i,j,k) = 0.001 * 10.**((DZ(i,j,k)-AA2)/BB2)
                ENDIF
              endif
            ENDIF
            DZN(i,j,k) = DZ(i,j,k)
          enddo
          enddo
          enddo
!
! insert obs to qr/qs for checking/plotting the new fields using rip 
!
          if(ireplace == 1) then
            if(ifrnw==0 .and. icnt==1) allocate(QR(nx+1,ny+1,nz))
            if(ifsnow==0.and. icnt==1) allocate(QS(nx+1,ny+1,nz))
            QR= 0.0
            QS= 0.0
            do i = 1, nx
            do j = 1, ny
            do k = 1, nz
            if(QRN(i,j,k)>0) QR(i,j,k) = QRN(i,j,k)
            if(QSN(i,j,k)>0) QS(i,j,k) = QSN(i,j,k)
            enddo
            enddo
            enddo
          endif 
!
! big header
        write(iout) 0
        write(iout) bhi, bhr, bhic, bhrc
!
!----- 3D FORECAST VARIABLES
        current_date1 = radar_date(icnt)
        call get_dtime(current_date1,current_date,idtime_sec) 
        dtime_min = idtime_sec/60.0/10000.0
        TIME = TIME + dtime_min
        current_date = current_date1
        nest = domain_id
        ordering = 'YXS'
        ix=nx+1
        jx=ny+1
        kx=nz
!
        print*, 'writing out mm5 with radar added',current_date,TIME
        call write_mm5field(iout,3,nest,U,TIME,'U',CURRENT_DATE,'D',&
             ordering,'m/s','U COMPONENT OF HORIZONTAL WIND',&
             IX,JX,KX,IX,JX,KX)
        call write_mm5field(iout,3,nest,V,TIME,'V',CURRENT_DATE,'D',&
             ordering,'m/s','V COMPONENT OF HORIZONTAL WIND',&
             IX,JX,KX,IX,JX,KX)
        call write_mm5field(iout,3,nest,T,TIME,'T',CURRENT_DATE,'C',&
             ordering,'K','TEMPERATURE',&
             IX,JX,KX,IX,JX,KX)
        call write_mm5field(iout,3,nest,Q,TIME,'Q',CURRENT_DATE,'C',&
             ordering,'kg/kg','MIXING RATIO',&
             IX,JX,KX,IX,JX,KX)
        call write_mm5field(iout,3,nest,PP,TIME,'PP',CURRENT_DATE,'C',&
             ordering,'Pa','PRESSURE PERTURBATION',&   
             IX,JX,KX,IX,JX,KX)

        if(ifclw==1) &
        call write_mm5field(iout,3,nest,QC,TIME,'CLW',CURRENT_DATE,'C',&
             ordering,'kg/kg','CLOUD WATER MIXING RATIO',&
             IX,JX,KX,IX,JX,KX)
        if(ifrnw==1 .or. ireplace==1) &
        call write_mm5field(iout,3,nest,QR,TIME,'RNW',CURRENT_DATE,'C',&
             ordering,'kg/kg','RAIN WATER MIXING RATIO',&
             IX,JX,KX,IX,JX,KX)
        if(ifsnow==1 .or. ireplace==1) &
        call write_mm5field(iout,3,nest,QS,TIME,'SNOW',CURRENT_DATE,&
             'C',ordering,'kg/kg','SNOW MIXING RATIO',&
             IX,JX,KX,IX,JX,KX)
! new fields from radar
        call write_mm5field(iout,3,nest,QRN,TIME,'RNWN',CURRENT_DATE,&
             'C',ordering,'kg/kg','radar-rain mixing ratio',&
             IX,JX,KX,IX,JX,KX)
        call write_mm5field(iout,3,nest,QSN,TIME,'SNOWN',CURRENT_DATE,&
             'C',ordering,'kg/kg','radar-snow mixing ratio',&
             IX,JX,KX,IX,JX,KX)
         call write_mm5field(iout,3,nest,DZN,TIME,'DBZN',CURRENT_DATE,&
             'C',ordering,'dbz','radar-mosaic reflectivity',&
             IX,JX,KX,IX,JX,KX)
          
!-----2D VARIABLES
        call write_mm5field(iout,2,nest,PSTAR,TIME,'PSTARCRS',&
             CURRENT_DATE,'C','YX','Pa', &
             '(REFERENCE) SURFACE PRESSURE MINUS PTOP',&
             IX,JX,1,IX,JX,1)
        call write_mm5field(iout,2,nest,TER,TIME,'TERRAIN',&
             CURRENT_DATE,'C','YX','m','TERRAIN ELEVATION',&
             IX,JX,1,IX,JX,1)
        call write_mm5field(iout,2,nest,XMAP,TIME,'MAPFACCR',&
             CURRENT_DATE,'C','YX','(DIMENSIONLESS)',&
             'MAP SCALE FACTOR', &
             IX,JX,1,IX,JX,1)
        call write_mm5field(iout,2,nest,DMAP,TIME,'MAPFACDT',&
             CURRENT_DATE,'C','YX','(DIMENSIONLESS)',&
             'MAP SCALE FACTOR', &
             IX,JX,1,IX,JX,1)
        call write_mm5field(iout,2,nest,COR,TIME,'CORIOLIS',&
             CURRENT_DATE,'C','YX','1/s','CORIOLIS PARAMETER',&     
             IX,JX,1,IX,JX,1)
!-----1D VARIABLES 
        call write_mm5field(iout,1,nest,SIGMH,TIME,'SIGMAH',&
             CURRENT_DATE,'H','S','sigma','VERTICAL COORDINATE',&
             KX,1,1,KX,1,1)

! end for using this mm5 time
        write (iout) 2
        enddo

        if(itime >= ntime) then
          print*, ' all mm5files within are processed'
          print*, ' radar timelevels processed',icnt,n_radarfile
          goto 408
        else
          ifclw=0
          ifrnw=0
          ifsnow=0 
          goto 101
        endif
      else 
        go to 404
      endif
 406  continue
      print*, ' radar2mm5 not done for ',beg_hh,' ',end_hh
      GOTO 408
 404  print*, ' radar2mm5 unsuccessful' 
 408  CONTINUE
      deallocate(u)
      deallocate(v)
      deallocate(t)
      deallocate(q)
      deallocate(pp)
      if(ifclw==1) deallocate(qc)
      if(ifrnw==1 .or. ireplace==1) deallocate(qr)
      if(ifsnow==1 .or. ireplace==1) deallocate(qs)
      deallocate(pstar)
      deallocate(ter)
      deallocate(xmap)
      deallocate(dmap)
      deallocate(cor)
      deallocate(sigmh)
      deallocate(dz)
      deallocate(qrn)
      deallocate(qsn)
      deallocate(dzn)
      close(iout)
      DO i = 0, n_mm5file - 1
        IUN = IUN1 + i
        close(IUN)
      ENDDO
      end program radar_for_nudge

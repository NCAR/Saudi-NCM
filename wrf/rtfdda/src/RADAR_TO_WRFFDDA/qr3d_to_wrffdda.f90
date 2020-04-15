!234567
!  pgf90 -Mfree -I $HOME/netcdf/include  3dvar2fdda.F -L $HOME/netcdf/lib -lnetcdf
! read input wrffdda file "filename"
! read radar analysis file "radarfile"
! output file in wrffdda format "out_file"
!
! assume that a wrffdda file already exists
! read and insert a new qr in the wrfdda file
! assume that the time levels in the two input files are the same
      program qr3d_to_wrffdda
      use WRF_kinds
      use WRF_ncread
      use WRF_utils
      use netcdf

      implicit none
      integer iret, ncid,varid
      character(len=120):: fddafile,radarfile,out_file
      integer i,j,iwe,jsn,ktop, Ntime, itime, it, k
      character(len=24):: radarTime
      character(len=19),allocatable,dimension(:):: Time
      real, allocatable, dimension(:,:,:,:):: array, &
        U_NDG_OLD,V_NDG_OLD,T_NDG_OLD,Q_NDG_OLD, &
        QR_NDG_OLD,PH_NDG_OLD,MU_NDG_OLD,&
        U_NDG_NEW,V_NDG_NEW,T_NDG_NEW,Q_NDG_NEW, &
        QR_NDG_NEW,PH_NDG_NEW,MU_NDG_NEW
      real, allocatable, dimension(:,:,:):: QRB
      real, parameter:: BADPT = -999.0
      integer iargc
      external iargc

      call getarg(1,fddafile)
      call getarg(2,radarfile)
      call getarg(3,out_file)
      print*, trim(fddafile),trim(radarfile)
      call get_dimension(trim(fddafile),"Time",Ntime)
      call get_dimension(trim(fddafile), "bottom_top", ktop)
      call get_dimension(trim(fddafile),"south_north", jsn)
      call get_dimension(trim(fddafile),"west_east",iwe)
      print*, iwe,jsn,ktop,Ntime

      allocate(U_NDG_OLD(iwe,jsn,ktop,Ntime))
      allocate(V_NDG_OLD(iwe,jsn,ktop,Ntime))
      allocate(T_NDG_OLD(iwe,jsn,ktop,Ntime))
      allocate(Q_NDG_OLD(iwe,jsn,ktop,Ntime))
      allocate(PH_NDG_OLD(iwe,jsn,ktop,Ntime))
      allocate(MU_NDG_OLD(iwe,jsn,1,Ntime))
      allocate(U_NDG_NEW(iwe,jsn,ktop,Ntime))
      allocate(V_NDG_NEW(iwe,jsn,ktop,Ntime))
      allocate(T_NDG_NEW(iwe,jsn,ktop,Ntime))
      allocate(Q_NDG_NEW(iwe,jsn,ktop,Ntime))
      allocate(PH_NDG_NEW(iwe,jsn,ktop,Ntime))
      allocate(MU_NDG_NEW(iwe,jsn,1,Ntime))
      allocate(Time(Ntime))
!     call read_fdda_QR(trim(fddafile),iwe,jsn,ktop, &
!                U_NDG_OLD,V_NDG_OLD,T_NDG_OLD,Q_NDG_OLD, &
!                QR_NDG_OLD,PH_NDG_OLD,MU_NDG_OLD, &
!                U_NDG_NEW,V_NDG_NEW,T_NDG_NEW,Q_NDG_NEW, &
!                QR_NDG_NEW,PH_NDG_NEW,MU_NDG_NEW,Time,Ntime)
      
      PRINT*,' reading radarfile',trim(radarfile)
      OPEN(10,file=trim(radarfile),status='old',form='unformatted') 
      allocate(QRB(jsn+1,iwe+1,ktop))
      allocate(QR_NDG_OLD(iwe,jsn,ktop,Ntime))
      allocate(QR_NDG_NEW(iwe,jsn,ktop,Ntime))
      itime = 0
      DO it = 1, Ntime 
        READ(10,END=111,ERR=111) radarTime
        READ(10,END=111,ERR=111) QRB

        itime = itime + 1
        Time(it) = radarTime(1:19)
        DO i=1, iwe
        DO j=1, jsn
        DO k = 1,ktop
          QR_NDG_OLD(i,j,k,it) = QRB(j,i,ktop-k+1)
        ENDDO
        ENDDO
        ENDDO
        if(it > 1) &
        QR_NDG_NEW(:,:,:,it-1) = QR_NDG_OLD(:,:,:,it)
      ENDDO
      deallocate(QRB)
 111  CONTINUE
      PRINT*,' found ',itime, 'time levels in radarfile'

      Ntime = itime
      call out_fdda_QR(trim(out_file),iwe,jsn,ktop, &
        U_NDG_OLD,V_NDG_OLD,T_NDG_OLD,Q_NDG_OLD, &
        QR_NDG_OLD,PH_NDG_OLD,MU_NDG_OLD, &
        U_NDG_NEW,V_NDG_NEW,T_NDG_NEW,Q_NDG_NEW, &
        QR_NDG_NEW,PH_NDG_NEW,MU_NDG_NEW,Time,Ntime)

      CLOSE(10)
      stop 
      end

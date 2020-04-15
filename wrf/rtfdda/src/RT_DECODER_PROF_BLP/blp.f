      MODULE mydata
 
c f90 -g -o blp -L/usr/local/lib -I/usr/local/include convert_unix_time.o blp.f -lnetcdf

      real*8, DIMENSION ( : ),   ALLOCATABLE :: obstime
      REAL, DIMENSION ( : ),   ALLOCATABLE :: lat,lon,alt
      REAL, DIMENSION ( : ),   ALLOCATABLE :: zprs
      REAL, DIMENSION ( : ),   ALLOCATABLE :: zrh
      REAL, DIMENSION ( : ),   ALLOCATABLE :: zt
      REAL, DIMENSION ( : ),   ALLOCATABLE :: zu,zv,zdir,zspd

      REAL, DIMENSION ( :,:,:), ALLOCATABLE :: temp,temp_qc
      REAL, DIMENSION ( :,:,: ), ALLOCATABLE :: u_wind,u_wind_qc,
     &       v_wind,v_wind_qc, w_wind,w_wind_qc, 
     &       wspd,wspd_qc, wdir,wdir_qc, pres, levels
      CHARACTER*6, DIMENSION ( : ),   ALLOCATABLE :: station_name

      END MODULE mydata


      MODULE debugging

      INTEGER, PARAMETER :: CHECKPOINT = 1
      logical,PARAMETER  ::debug = .false.
      logical,PARAMETER  ::debugt = .true.
      logical,PARAMETER  ::debuguv = .true.

      END MODULE debugging

      MODULE sizes

      INTEGER  :: nplatforms
      INTEGER, PARAMETER :: nlev=50
      INTEGER :: nrecs
      character*18 outfile 

      END MODULE sizes

      program blp

      use debugging
      use date_pack

      parameter (ntimes=2)
       dimension reqtm(ntimes)
       character*60 filename
       logical      ishere

       iun2 = 50
       ntims = ntimes
 
       read (*,10) filename
 10    format (a)

       inquire (file=filename,EXIST=ishere)
       if (.not. ishere) then
         write(6,6001) filename
 6001    format(1x,'CANNOT SEE FILE: ', 60a1)
       else
         write(6,6002) filename
 6002    format(1x,'I SEE FILE: ', 60a1)
       endif
 
       mdate = 99021700

       write(6,6003) mdate
 6003  format(1x,' I see an mdate of -> ',i8)

       call rdblp(iun2,mdate,filename,reqtm,ntims)
 
       print *,' program blp completed successfully '
       stop 99999
       end
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
      subroutine rdblp(iun2,mdate,filename,reqtm,ntims)
c
c-----------------------------------------------------------------------
c
c  readnet.f                      
c  This file is a fortran template file designed to read the SAMS
c  netCDF file into memory.
c
c  History:
c  Date       Name          Action
c  Dec 19,97  D. Hansen     Wrote for C. Davis (for normal SAMS)
c  Mar  5,98  S. Low-Nam    added capability to treat SAMS as bogus data for RAP
c  Jan 25,00  S. Low-Nam    writing out more fields + qc-values for stn-fdda
c
c-----------------------------------------------------------------------
c 
c     Required includes.

      use mydata
      use debugging
      use sizes
      use date_pack

c     include 'netcdf.inc'
      include 'netcdf.inc.crfil'

c     Define Variables.
c     Variable ids run sequentially from 1 to nvars= 28
C      parameter   (nvars =28)          ! number of variables
C      parameter   (nrec  = 84)         ! change this to generalize
      
      integer*4    numhrs,offset
      integer*4    np,reqindex2
      integer*4     curplat
      parameter    (curplat = 1)

      parameter    (itmx=24,ntt=96,nvars_p=38,fldlen_p=20)
      integer*4    nplfm
      parameter    (nplfm = 100)          ! Number of platforms
                                          ! dim(# of p-levels, # of stns)
      integer*4    nvars,nrec,rcode,recdim,ilittleyr
      integer*4    base_time

      integer*4    start(10), count(10)
      integer*4    vdims(10)            ! allow up to 10 dimensions      
      INTEGER*4 STATUS
      parameter (nobs=151)
      dimension ht(nobs),spd(nobs),dir(nobs),
     1           wqc(nobs),slp(nobs)
      dimension tt(nobs),td(nobs),ww(nobs),
     1          prs(nobs),uu(nobs),vv(nobs)
      integer   ht_qc(nobs),spd_qc(nobs),dir_qc(nobs),
     1          tt_qc(nobs),pp_qc(nobs),
     2          uu_qc(nobs),vv_qc(nobs),rh_qc(nobs),
     3          td_qc(nobs),ww_qc(nobs)
      character*14 ndato, ndate
      logical bogus

      INTEGER iday(12)
      data iday /31,28,31,30,31,30,31,31,30,31,30,31 /
      data deg2rad / .0174532925 /

      logical  firsttime

      INTEGER century_year(nlev),
     &        month(nlev),day(nlev),hour(nlev),
     &        minute(nlev),second(nlev)
      dimension timwant(itmx)
      character*40 strname(nplfm), strnam2(nplfm), string2, string4

      character*20 dummy,dum1
      character*60 filename
      character    abc
      logical      ishere
      integer*4    ndims,dimsiz
      integer*4    fdims(3)    
      integer*4    ncid, ngatts,ntp,nvdim
      integer*4    i,j, k1,k2,vdim1,vdim2,vdim3,vdim4,vdim5,vdim6
      integer      timeid
 
c
      strnam2(1) = 'FM-12 BLP-P                             '

      firsttime = .true.
      nvars  = 38
      nrec   = 84
      spval  = 99999.9
      p_max  = 1090.
      p_min  = 100.
      wdir_max= 360.
      wdir_min= 0.
      wspd_max= 60.
      wspd_min= 0.
      zmiss  =-99.0
      pi     = 3.1415926535
c      xn     = 0.716
      tfact  = 273.15
      raddeg = 180./pi 
      twopi  = 2.0*pi
      tpo2   = (3.0*pi)/2.
 
      do ii=2,nplfm
       strnam2(ii) = strnam2(ii-1)
      enddo

      bogus = .true.
      klv   = nplfm

      iuno = iun2
c
 
c     Open netCDF file.
c
       write(6,4001)filename
 4001  format(1x,' sssss 1 filename= ',a20)

      ncid=ncopn(filename, ncnowrit, rcode)
c     i3=nf_open(filename, 0, ncid)
 
       write(6,4002)filename
 4002  format(1x,' sssss 2 filename= ',a20)

c      IF (STATUS .NE. NF_NOERR) THEN
c         PRINT *, 'PROBLEM WITH FILE ',filename
c         PRINT *, NF_STRERROR(STATUS)
c         STOP 'Stopped - check netCDF file'
c      ENDIF

c     Get info on the record dimension for this file.
      call ncinq(ncid,ndims,nvars,ngatts,recdim,rcode)

       write(6,4011)nvars
 4011  format(1x,' nvars= ',i5)

c     We now want to store the DIM INFO
         do i=1,ndims
            call ncdinq(ncid, i, dummy, dimsiz, rcode)
 
       write(6,4003)dummy
 4003  format(1x,' sssss 1 dummy= ',a20)


            fdims(i) = dimsiz
            print *,'DIM - ',dummy,' SIZE - ',dimsiz
            if (dummy .eq. 'fldlen') then
               if (dimsiz .ne. fldlen_p) then
                  print *,'ERROR - fldlen is .ne. to ',fldlen_p
                  STOP
               endif
            else  if (dummy .eq. 'staName') then
               if (dimsiz .gt. nplfm) then
                  print *,'ERROR: num platforms is .gt. to ',nplfm
                  STOP
               else if (dimsiz .lt. nplfm) then
                  print *, 'WARNING: resetting klv to ',dimsiz
                  klv   = dimsiz
               else if (dimsiz .eq. nplfm) then
                  print *, 'INFO: (klv=nplfm) ',klv, ' = ',nplfm
               endif
            else  if (dummy .eq. 'timeObs') then
                 print *,' attribute time is present in the dataset '
            endif
         enddo

      if (nvars .ne. nvars_p) then
         print *,' Change nvars and nvars_p in code, recompile'
         print *,' new values should be ',nvars
!        stop
      else
            print *,'File has ',nvars,' variables.'
            print *,'File has ',ndims,' dims.'
            print *,'File has ',ngatts,' global attributes.'
      endif
 
        write(6,4012)nvars
 4012   format(1x,' number of variables (nvars)= ',i5)

      call ncdinq(ncid,recdim,dummy,nrecs,rcode)
      write(6,54) nrecs
 54   format(2x,' ***** File has ',i5,' records. ')

      nrecord = nrecs

c     !nrecs! now contains the # of records for this file
      if (nrecs .gt. nlev) then
         print *,' Change vars nrecs and nrecs_p in code, recompile'
         print *,' new NRECS values should be ',nrecs
         stop
      endif

      if ( firsttime ) then
         nplatforms = nrecs
         ALLOCATE ( station_name ( nplatforms ) )
         ALLOCATE ( obstime ( nplatforms ) )
         ALLOCATE ( lat ( nplatforms ) )
         ALLOCATE ( lon ( nplatforms ) )
         ALLOCATE ( alt ( nplatforms ) )
         ALLOCATE ( zrh ( nplatforms ) )
         ALLOCATE ( zt ( nplatforms ) )
         ALLOCATE ( zu ( nplatforms ) )
         ALLOCATE ( zv ( nplatforms ) )
         ALLOCATE ( zdir ( nplatforms ) )
         ALLOCATE ( zspd ( nplatforms ) )
         ALLOCATE ( zprs ( nplatforms ) )
         ALLOCATE ( temp ( 3, nlev, nplatforms ) )
         ALLOCATE ( temp_qc ( 3, nlev, nplatforms ) )
         ALLOCATE ( u_wind ( 3, nlev, nplatforms ) )
         ALLOCATE ( u_wind_qc ( 3, nlev, nplatforms ) )
         ALLOCATE ( v_wind ( 3, nlev, nplatforms ) )
         ALLOCATE ( v_wind_qc ( 3, nlev, nplatforms ) )
         ALLOCATE ( w_wind ( 3, nlev, nplatforms ) )
         ALLOCATE ( w_wind_qc ( 3, nlev, nplatforms ) )
         ALLOCATE ( wspd  ( 3, nlev, nplatforms ) )
         ALLOCATE ( wspd_qc( 3, nlev, nplatforms ) )
         ALLOCATE ( wdir  ( 3, nlev, nplatforms ) )
         ALLOCATE ( wdir_qc( 3, nlev, nplatforms ) )
         ALLOCATE ( levels( 3, nlev, nplatforms ) )
         ALLOCATE ( pres  ( 3, nlev, nplatforms ) )
         firsttime = .false.
      endif

C     END OF DIM INFO


       write(6,4005)nvars
 4005  format(1x,' number of variables - nvars= ',i5)


C     Begin variable loop

      do i=1,nvars
         call ncvinq(ncid, i, dummy,ntp,nvdim,vdims,nvs,rcode)
 
       write(6,4006)i,dummy, nvdim
 4006  format(1x,' variable number ',i5,' dummy= ',a20, 'nvdim=',I3)

         if (rcode .ne. 0) then
            print *,'ERROR 382 - variable ',dummy
         endif

       dum1=dummy
       do j=1,nvdim
       if(j.eq.1) call ncdinq(ncid,vdims(j),dum1,vdim1,rcode)
       if(j.eq.2) call ncdinq(ncid,vdims(j),dum1,vdim2,rcode)
       if(j.eq.3) call ncdinq(ncid,vdims(j),dum1,vdim3,rcode)
       if(j.eq.4) call ncdinq(ncid,vdims(j),dum1,vdim4,rcode)
       if(j.eq.5) call ncdinq(ncid,vdims(j),dum1,vdim5,rcode)
       if(j.eq.6) call ncdinq(ncid,vdims(j),dum1,vdim6,rcode)
c      print *, "J=",J,"vdim=",vdim1,vdim2,vdim3,vdim4
       enddo
C    obs time:  BASE_TIME
         if (dummy .eq. 'timeObs') then
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1 (ncid, i,start,obstime(k1),rcode)
         
c        print *, "base_time=", obstime(K1)
         base_time = int(obstime(k1))

c the average time is between (ihr-1) and ihr, so the valid time
c for this averaged data should be ihr-0.5
         itoff = -1800
         call epoch(base_time, itoff, century_year(k1),
     &     month(k1), day(k1), hour(k1), minute(k1), 
     &      second(k1))

c        print *, base_time,itoff,century_year(k1),
c    &      month(k1),day(k1),hour(k1),minute(k1), 
c    &      second(k1)

               enddo
C     alt
         else if (dummy .eq. 'staElev') then
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1 (ncid, i,start,alt(k1),rcode)
               enddo
C     lon
         else if (dummy .eq. 'staLon') then
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1 (ncid, i,start,lon(k1),rcode)
               enddo

C     lat
         else if (dummy .eq. 'staLat') then
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1 (ncid, i,start,lat(k1),rcode)
               enddo
C     station name
         else if (dummy .eq. 'staName') then
                 do k2=1,vdim2
                   do k1=1,vdim1
                  start(1) = k1
                  start(2) = k2
                  call ncvg1c (ncid, i,start,abc,rcode)
                  station_name(k2)(k1:k1)=abc
                 enddo
               enddo
                  print *, "abc=", station_name(10)

C  sfc temperature 
         else if (dummy .eq. 'temperature') then
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1 (ncid, i,start,zt(k1),rcode)
               enddo

C  sfc temperature 
         else if (dummy .eq. 'relHumidity') then
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1 (ncid, i,start,zrh(k1),rcode)
               enddo

C   MSL pres
         else if (dummy .eq. 'pressure') then
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1 (ncid, i,start,zprs(k1),rcode)
               enddo

C  sfc  wdir
         else if (dummy .eq. 'windDirSfc') then
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1 (ncid, i,start,zdir(k1),rcode)
               enddo

C  sfc  wspd
         else if (dummy .eq. 'windSpeedSfc') then
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1 (ncid, i,start,zspd(k1),rcode)
               enddo

C     levels
         else if (dummy .eq. 'levels') then
               do k1=1,vdim1
                  do k2=1,vdim2
                    do k3=1,vdim3
                     start(1) = k1
                     start(2) = k2
                     start(3) = k3
                     call ncvgt1 (ncid, i,start,levels(k1,k2,k3),rcode)
                    enddo
                  enddo
               enddo

C     u_wind
         else if (dummy .eq. 'uComponent') then
               do k1=1,vdim1
                  do k2=1,vdim2
                    do k3=1,vdim3
                     start(1) = k1
                     start(2) = k2
                     start(3) = k3
                     call ncvgt1 (ncid, i,start,u_wind(k1,k2,k3),rcode)
                    enddo
                  enddo
               enddo

C     v_wind
         else if (dummy .eq. 'vComponent') then
               do k1=1,vdim1
                  do k2=1,vdim2
                    do k3=1,vdim3
                     start(1) = k1
                     start(2) = k2
                     start(3) = k3
                     call ncvgt1 (ncid, i,start,v_wind(k1,k2,k3),rcode)
                    enddo
                  enddo
               enddo

C     uv_wind_qc
         else if (dummy .eq. 'uvQualityCode') then
               do k1=1,vdim1
                  do k2=1,vdim2
                    do k3=1,vdim3
                     start(1) = k1
                     start(2) = k2
                     start(3) = k3
                 call ncvgt1 (ncid, i,start,u_wind_qc(k1,k2,k3),rcode)
                     v_wind_qc(k1,k2,k3)=u_wind_qc(k1,k2,k3)
                    enddo
                  enddo
               enddo

C     w_wind
         else if (dummy .eq. 'wComponent') then
               do k1=1,vdim1
                  do k2=1,vdim2
                    do k3=1,vdim3
                     start(1) = k1
                     start(2) = k2
                     start(3) = k3
                     call ncvgt1 (ncid, i,start,w_wind(k1,k2,k3),rcode)
                    enddo
                  enddo
               enddo

c  unused
         else
            write(6,89) 
 89         format(2x,' *** unused ')

         endif
      enddo
C     End variable loop

c------------ have read out the data from netcdf files --------
c------------ now create output in little_r format ------------
c------------   one profiler by one                ------------
      
      do np = 1, nplatforms
          iyr = century_year(np)
           if(iyr .lt. 2000)then
             nyr = iyr - 1900
           else
             nyr = iyr - 2000
           endif
          imo = month(np)
          idy = day(np)
          ihr = hour(np)
          imn = minute(np)
      isec =0
      igmts = 0
      print *,iyr,imo, idy,ihr
       write (ndate(1:4),fmt='(i4.4)') iyr
       write (ndate(5:6),fmt='(i2.2)') imo
       write (ndate(7:8),fmt='(i2.2)') idy
       write (ndate(9:10),fmt='(i2.2)') ihr
       write (ndate(11:12),fmt='(i2.2)') imn
       write (ndate(13:14),fmt='(i2.2)') isec
      bogus  = .false.
      spval  = -888888.0
      qc_spv = -888888.0
      strname  = '99001                                   '
      strname(1:6)=station_name(np) 
      strnam2  = 'FM-12 SYNOP                             '
      string2  = ' PROFILER BLP                           '
      string4  = '                                        '

      do no=1,nobs
       slp(no) = qc_spv
       td(no) = spval
       prs(no) = spval
       uu(no)  = spval
       vv(no)  = spval
       ww(no)  = spval
       dir(no)  = spval
       spd(no)  = spval
       tt(no)  = spval
       ht(no)   = spval
       pp_qc(no) = qc_spv
       ht_qc(no) = qc_spv
       uu_qc(no) = qc_spv
       vv_qc(no) = qc_spv
       dir_qc(no) = qc_spv
       spd_qc(no) = qc_spv
       rh_qc(no) = qc_spv
       tt_qc(no) = qc_spv
       td_qc(no) = qc_spv
       ww_qc(no) = qc_spv
      enddo

      xlev=alt(np)
      no = 2
      do im = 1,3 
       do il = 1,nlev 
       if(xlev.ge.0..and.xlev.lt.8000..and. 
     &  levels(im,il,np).ge.0..and.levels(im,il,np).lt.5000.
     &  ) then 
         ht(no)=xlev+levels(im,il,np)
         ht_qc(no)=0.
       endif
       if(u_wind(im,il,np).lt.100..and.u_wind(im,il,np).gt.-100.
     &  .and.v_wind(im,il,np).lt.100..and.v_wind(im,il,np).gt.-100.
     &  ) then
         uu(no)=u_wind(im,il,np)
         vv(no)=v_wind(im,il,np)
        endif
       if(u_wind_qc(im,il,np).lt.500..and.u_wind_qc(im,il,np).ge.0.
     &  ) then
         uu_qc(no)=amin1(u_wind_qc(im,il,np),1.0)
         vv_qc(no)=amin1(v_wind_qc(im,il,np),1.0)
        endif
       if(w_wind(im,il,np).lt.50..and.w_wind(im,il,np).gt.-50.
     &  ) then
         ww(no)=w_wind(im,il,np)
        endif
        no=no+1
       enddo
      enddo
     
      do no=1,nobs
       if(vv(no).gt.spval.and.uu(no).gt.spval) then
        spd(no)=sqrt(uu(no)**2+vv(no)**2)
        dir(no)=(1.5*pi-atan2(vv(no),uu(no)))*180./pi
        if(dir(no) >= 360.) dir(no)=dir(no)-360.
        if(uu_qc(no).lt.500..and.uu_qc(no).ge.0.) then
         spd_qc(no)=uu_qc(no)
         dir_qc(no)=uu_qc(no)
        endif
c rotate the wind to map      
          print *, uu(no),vv(no),spd(no),dir(no)
        dirmap= dir(no) 
        uu(no) = -1. * spd(no) * SIN ( dirmap * deg2rad )
        vv(no) = -1. * spd(no) * COS ( dirmap * deg2rad )
       endif
      enddo

c sort and take out levels without usable obs
      call trimsort(ht,ht_qc,spd,spd_qc,dir,dir_qc,tt,tt_qc
     &           td,td_qc,uu,uu_qc,vv,vv_qc,ww,ww_qc,nobs,nobsn)

      tindx= 999
      kkl   = nobsn
      iseq_num = 99
       print *,' kkl= ',kkl,' ndat= ',ndat,
     1     ' strname= ',strname,' strnam2= ',strnam2,
     2     ' string2= ',string2,' bogus= ',bogus

       call write_obs (stindx,prs,ht,tt,td,spd,dir,
     *                 slp,xlev,lat(np),lon(np),
     *                 uu, vv, ht_qc, tt_qc, rh_qc,
     *            dir_qc, spd_qc, ndate, imn, isec, kkl,
     *          strname,string2,strnam2,string4,bogus,
     *                                  iseq_num,iuno )


 305  continue
      enddo       !np
 101  continue
      print *,' program completed successfully '
      stop 99999
      end

c
      subroutine trimsort(ht,ht_qc,spd,spd_qc,dir,dir_qc,tt,tt_qc
     &          td,td_qc,uu,uu_qc,vv,vv_qc,ww,ww_qc,nobst,nobsn)
c
c throw out bad data
c
      dimension ht(nobst), ht_qc(nobst),spd(nobst),spd_qc(nobst),
     &         dir(nobst),dir_qc(nobst), tt(nobst), tt_qc(nobst),
     &          uu(nobst), uu_qc(nobst), vv(nobst), vv_qc(nobst),
     &          td(nobst), td_qc(nobst), ww(nobst), ww_qc(nobst)

      nobsn=0
      do n=1,nobst
      if((ht(n).gt.0..and.ht(n).lt.8000.) .and. (
     &   (abs(dir(n)).lt.361..and.abs(spd(n)).lt.100.) .or.
     &   (abs(uu(n)).lt.100..and.abs(vv(n)).lt.100.) .or.
     &   (abs(tt(n)).lt.500.) )) then
       nobsn=nobsn+1
       ht(nobsn)=ht(n)
       ht_qc(nobsn)=ht_qc(n)
       spd(nobsn)=spd(n)
       spd_qc(nobsn)=spd_qc(n)
       dir(nobsn)=dir(n)
       dir_qc(nobsn)=dir_qc(n)
       tt(nobsn)=tt(n)
       tt_qc(nobsn)=tt_qc(n)
       td(nobsn)=td(n)
       td_qc(nobsn)=td_qc(n)
       uu(nobsn)=uu(n)
       uu_qc(nobsn)=uu_qc(n)
       vv(nobsn)=vv(n)
       vv_qc(nobsn)=vv_qc(n)
       ww(nobsn)=ww(n)
       ww_qc(nobsn)=ww_qc(n)
       endif
       enddo

      do n=1,nobsn
        do nn=n+1,nobsn
       ht1  = ht(n)
       htc  = ht_qc(n)
       spd1 = spd(n)
       spdc = spd_qc(n)
       dir1 = dir(n)
       dirc = dir_qc(n)
       tt1  = tt(n)
       ttc  = tt_qc(n)
       uu1  = uu(n)
       uuc  = uu_qc(n)
       vv1  = vv(n)
       vvc  = vv_qc(n)
       td1  = td(n)
       tdc  = td_qc(n)
       ww1  = ww(n)
       wwc  = ww_qc(n)

          
c       print *, n,nn, ht1, ht(nn), nobsn
         if(ht(nn).lt.ht1) then
          ht(n)=ht(nn)
          ht_qc(n)=ht_qc(nn)
          dir(n)=dir(nn)
          dir_qc(n)=dir_qc(nn)
          spd(n)=spd(nn)
          spd_qc(n)=spd_qc(nn)
          tt(n)=tt(nn)
          tt_qc(n)=ht_qc(nn)
          td(n)=td(nn)
          td_qc(n)=td_qc(nn)
          uu(n)=uu(nn)
          uu_qc(n)=uu_qc(nn)
          vv(n)=vv(nn)
          vv_qc(n)=vv_qc(nn)
          ww(n)=ww(nn)
          ww_qc(n)=ww_qc(nn)
          ht(nn)=ht1
          ht_qc(nn) =htc
          spd(nn)= spd1
          spd_qc(nn)= spdc
          dir(nn)= dir1
          dir_qc(nn)= dirc
          tt(nn) = tt1
          tt_qc(nn)= ttc
          uu(nn) = uu1
          uu_qc(nn)= uuc
          vv(nn) = vv1
          vv_qc(nn)= vvc
          td(nn) = td1
          td_qc(nn)= tdc
          ww(nn) = ww1
          ww_qc(nn)= wwc
c       print *," ---", n,nn, ht(n), ht(nn), nobsn
         endif
        enddo
       enddo
       return
       end

c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
        subroutine rh2td(TT,T_units,RH,TD,nobs,badval)

        dimension tt(*), rh(*), td(*)


c  T:   Temperature
c  RH:  Relative Humidity
c  Td:  Dewpoint

        character*6 T_units

        RV = 461.51
        
        do n=1,nobs
           
c      write(6,3040) n,t_units
 3040      format(2x,' n= ',i3,' tunit= ',a6)
           
           if( tt(n) .eq. BADVAL .or.
     &          rh(n) .eq. BADVAL )then
              td(n) = spvltr
c              write(6,3030) n,td(n)
 3030         format(2x,'in subr rh2td n= ',i3,' td= ',f10.1)
              goto 2030
           endif
           
c     if ((abs(TT(n)-BADVAL).gt.200).AND.
c     &               (abs(RH(n)-BADVAL).gt.100))Then
           
           if (t_units.eq.'K     ') then
              ta = TT(n)
           elseif (t_units.eq.'C     ') then
              ta = TT(n) + 273.15
           else
c     stop 'T units in RH2TD'
              TD(n) = spvltr
           endif
           
           XL = XLAT_HEAT_VAP(ta)
           Td_inv = (1./ta) - (RV/XL) * ALOG(RH(n)/100.)
           Tdd = 1./Td_inv
           
           if (t_units.eq.'K     ') then
              TD(n) = Tdd
           elseif (t_units.eq.'C     ') then
              TD(n) = Tdd - 273.15
           else
c     stop 'TD units in RH2TD'
              TD(n) = spvltr
           endif
           
 2030      if(n .eq. nobs)goto 2040
        enddo
        
 2040   continue
        
        return
        end
c     
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
      real function xlat_heat_vap(Temp_Kel)
      implicit none
c
c Return the latent heat of vaporization as a function of temperature.
c
c
c  INPUT:
      real Temp_Kel              ! Temperature ( K )

c  OUTPUT:

       if( (Temp_Kel .lt. 150.) .or. (Temp_Kel .GT. 340.) )then
        print*, 'Input Temperature (K) = ', Temp_Kel
        print*, 'Function XLAT_HEAT_VAP.'
        call abort()
       endif

       xlat_heat_vap = 3.1484E6 - 2370. * Temp_Kel

       return
       end 
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
      subroutine convert(mdate,iyr,imo,idy,ihr)
c
      iyr=mdate/1000000
      imo=( mdate - iyr*1000000 ) / 10000
      idy=( mdate - iyr*1000000 -imo*10000 ) / 100
      ihr=( mdate - iyr*1000000 -imo*10000 - idy*100 )

      if (iyr.lt.60) then
         iyr = iyr + 2000
      else
         iyr = iyr + 1900
      endif
c
      return
      end
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
       SUBROUTINE write_obs (stindx, p , z , t , td , spd , dir ,
     *                    slp , ter , xlat , xlon ,
     *                    uu  , vv  , zpp_qc, tt_qc, td_qc ,
     *                    zuu_qc , zvv_qc ,
     *                    mdate , imin , isec, kx ,
     * string1 , string2 , string3 , string4 , bogus , iseq_num ,
     * iunit )

       integer stindx                                  ! station index
       dimension z(kx),t(kx),td(kx),spd(kx),dir(kx),
     *           uu(kx), vv(kx), p(kx)
       integer   zpp_qc(kx), tt_qc(kx), td_qc(kx),
     *           zuu_qc(kx), zvv_qc(kx)

      character *20 date_char
      character *14 mdate
      character *40 string1, string2 , string3 , string4
      CHARACTER *120 rpt_format
      CHARACTER *120 meas_format
      CHARACTER *120 end_format
      logical bogus

      rpt_format =  ' ( 2f20.5 , 2a40 , '
     *             // ' 2a40 , 1f20.5 , 5i10 , 3L10 , '
     *             // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
      meas_format =  ' ( 10( f13.5 , i7 ) ) '
      end_format = ' ( 3 ( i7 ) ) '

      date_char(7:20) = mdate(1:14)
      date_char(1:6)='      '
      print *,' date_char= ',date_char(1:20)

      WRITE ( UNIT = iunit , ERR = 19 , FMT = rpt_format )
     *        xlat,xlon, string1 , string2 ,
     *        string3 , string4 , ter, kx*6, 0,0,iseq_num,0,
     *        .true.,bogus,.false.,
     *         -888888, -888888, date_char ,
     *         slp,0,-888888.,0, -888888.,0, -888888.,0, -888888.,0,
     *               -888888.,0,
     *               -888888.,0, -888888.,0, -888888.,0, -888888.,0,
     *               -888888.,0,
     *               -888888.,0, -888888.,0

      do 100 k = 1 , kx
         WRITE ( UNIT = iunit , ERR = 20 , FMT = meas_format )
     *          p(k),0, z(k),zpp_qc(k), t(k),tt_qc(k),
     *          td(k),td_qc(k),
     *          spd(k),zvv_qc(k), dir(k),zuu_qc(k),
     *          uu(k),zuu_qc(k), vv(k),zvv_qc(k), -888888.,0, -888888.,0
c
         print *,' k= ',k,' p-z-ppqc= ',p(k),z(k),zpp_qc(k),
c    1        ' t-ttqc= ',t(k),tt_qc(k),' td-tdqc= ',td(k),td_qc(k),
     2        ' spd-dir= ',spd(k),dir(k),' uu-uuqc= ',uu(k),zuu_qc(k),
     3        ' vv-vvqc= ',vv(k),zvv_qc(k)
c
100   continue

      WRITE ( UNIT = iunit , ERR = 21 , FMT = meas_format )
     * -777777.,0, -777777.,0,float(kx),0,
     * -888888.,0, -888888.,0, -888888.,0,
     * -888888.,0, -888888.,0, -888888.,0,
     * -888888.,0

      WRITE ( UNIT = iunit , ERR = 19 , FMT = end_format )  kx, 0, 0

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

      END
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
      SUBROUTINE geth_newdate (ndate, odate, idt)

c  From old date ('YYYYMMDDHHMMSSffff') and
c  delta-time, compute the new date.

c  on entry     -  odate  -  the old hdate.
c                  idt    -  the change in time
c  on exit      -  ndate  -  the new hdate.

      CHARACTER*14 ndate
      CHARACTER*14 odate


c  yrold    -  indicates the year associated with "odate"
c  moold    -  indicates the month associated with "odate"
c  dyold    -  indicates the day associated with "odate"
c  hrold    -  indicates the hour associated with "odate"
c  miold    -  indicates the minute associated with "odate"
c  scold    -  indicates the second associated with "odate"

c  yrnew    -  indicates the year associated with "ndate"
c  monew    -  indicates the month associated with "ndate"
c  dynew    -  indicates the day associated with "ndate"
c  hrnew    -  indicates the hour associated with "ndate"
c  minew    -  indicates the minute associated with "ndate"
c  scnew    -  indicates the second associated with "ndate"

c  mday     -  a list assigning the number of days in each month

c  i        -  loop counter
c  nday     -  the integer number of days represented by "idt"
c  nhour    -  the integer number of hours in "idt" after taking out
c              all the whole days
c  nmin     -  the integer number of minutes in "idt" after taking out
c              all the whole days and whole hours.
c  nsec     -  the integer number of minutes in "idt" after taking out
c              all the whole days, whole hours, and whole minutes.

      INTEGER len, nlen, olen
      INTEGER yrnew, monew, dynew, hrnew, minew, scnew, frnew
      INTEGER yrold, moold, dyold, hrold, miold, scold, frold
      INTEGER mday(12), nday, nhour, nmin, nsec, nfrac, i, ifrc
      LOGICAL opass
      CHARACTER*10 hfrc
      CHARACTER*1  sp

c  Assign the number of days in a months

      mday( 1) = 31
      mday( 2) = 28
      mday( 3) = 31
      mday( 4) = 30
      mday( 5) = 31
      mday( 6) = 30
      mday( 7) = 31
      mday( 8) = 31
      mday( 9) = 30
      mday(10) = 31
      mday(11) = 30
      mday(12) = 31

c  Break down old hdate into parts

      hrold = 0
      miold = 0
      scold = 0
      frold = 0
c     olen = LEN(odate)
      olen = 14
      IF (olen.GE.11) THEN
         sp = odate(11:11)
      else
         sp = ' '
      END IF

c  Use internal READ statements to convert the CHARACTER string
c  date into INTEGER components.

      READ(odate(1:4),  '(I4)') yrold
      READ(odate(5:6),  '(I2)') moold
      READ(odate(7:8), '(I2)') dyold
      IF (olen.GE.9) THEN
         READ(odate(9:10),'(I2)') hrold
         IF (olen.GE.11) THEN
            READ(odate(11:12),'(I2)') miold
            IF (olen.GE.13) THEN
               READ(odate(13:14),'(I2)') scold
               IF (olen.GT.14) THEN
                  READ(odate(21:olen),'(I2)') frold
               END IF
            END IF
         END IF
      END IF

c  Set the number of days in February for that year.

      mday(2) = nfeb(yrold)

c  Check that ODATE makes sense.

      opass = .TRUE.

c  Check that the month of ODATE makes sense.

      IF ((moold.GT.12).or.(moold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
         opass = .FALSE.
      END IF

c  Check that the day of ODATE makes sense.

      IF ((dyold.GT.mday(moold)).or.(dyold.LT.1)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
         opass = .FALSE.
      END IF

c  Check that the hour of ODATE makes sense.

      IF ((hrold.GT.23).or.(hrold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
         opass = .FALSE.
      END IF

c  Check that the minute of ODATE makes sense.

      IF ((miold.GT.59).or.(miold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
         opass = .FALSE.
      END IF

c  Check that the second of ODATE makes sense.

      IF ((scold.GT.59).or.(scold.LT.0)) THEN
         WRITE(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
         opass = .FALSE.
      END IF

c  Check that the fractional part  of ODATE makes sense.

      IF (.not.opass) THEN
         WRITE(*,*) 'GETH_NEWDATE: Crazy ODATE: ', odate(1:olen), olen
         STOP 'odate_3'
      END IF

c  Date Checks are completed.  Continue.

c  Compute the number of days, hours, minutes, and seconds in idt

      IF (olen.GT.14) THEN !idt should be in fractions of seconds
         ifrc = olen-20
         ifrc = 10**ifrc
         nday   = ABS(idt)/(86400*ifrc)
         nhour  = MOD(ABS(idt),86400*ifrc)/(3600*ifrc)
         nmin   = MOD(ABS(idt),3600*ifrc)/(60*ifrc)
         nsec   = MOD(ABS(idt),60*ifrc)/(ifrc)
         nfrac = MOD(ABS(idt), ifrc)
      ELSE IF (olen.eq.14) THEN  !idt should be in seconds
         ifrc = 1
         nday   = ABS(idt)/86400 ! Integer number of days in delta-time
         nhour  = MOD(ABS(idt),86400)/3600
         nmin   = MOD(ABS(idt),3600)/60
         nsec   = MOD(ABS(idt),60)
         nfrac  = 0
      ELSE IF (olen.eq.12) THEN !idt should be in minutes
         ifrc = 1
         nday   = ABS(idt)/1440 ! Integer number of days in delta-time
         nhour  = MOD(ABS(idt),1440)/60
         nmin   = MOD(ABS(idt),60)
         nsec   = 0
         nfrac  = 0
      ELSE IF (olen.eq.10) THEN !idt should be in hours
         ifrc = 1
         nday   = ABS(idt)/24 ! Integer number of days in delta-time
         nhour  = MOD(ABS(idt),24)
         nmin   = 0
         nsec   = 0
         nfrac  = 0
      ELSE IF (olen.eq.8) THEN !idt should be in days
         ifrc = 1
         nday   = ABS(idt)/24 ! Integer number of days in delta-time
         nhour  = 0
         nmin   = 0
         nsec   = 0
         nfrac  = 0
      ELSE
         WRITE(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)')
     1        olen
         WRITE(*,*) odate(1:olen)
         STOP 'odate_4'
      END IF

      IF (idt.GE.0) THEN

         frnew = frold + nfrac
         IF (frnew.GE.ifrc) THEN
            frnew = frnew - ifrc
            nsec = nsec + 1
         END IF

         scnew = scold + nsec
         IF (scnew .GE. 60) THEN
            scnew = scnew - 60
            nmin  = nmin + 1
         END IF

         minew = miold + nmin
         IF (minew .GE. 60) THEN
            minew = minew - 60
            nhour  = nhour + 1
         END IF

         hrnew = hrold + nhour
         IF (hrnew .GE. 24) THEN
            hrnew = hrnew - 24
            nday  = nday + 1
         END IF

         dynew = dyold
         monew = moold
         yrnew = yrold
         DO i = 1, nday
            dynew = dynew + 1
            IF (dynew.GT.mday(monew)) THEN
               dynew = dynew - mday(monew)
               monew = monew + 1
               IF (monew .GT. 12) THEN
                  monew = 1
                  yrnew = yrnew + 1
                  ! If the year changes, recompute the number of days in February
                  mday(2) = nfeb(yrnew)
               END IF
            END IF
         END DO

      ELSE IF (idt.LT.0) THEN

         frnew = frold - nfrac
         IF (frnew .LT. 0) THEN
            frnew = frnew + ifrc
            nsec = nsec - 1
         END IF

         scnew = scold - nsec
         IF (scnew .LT. 00) THEN
            scnew = scnew + 60
            nmin  = nmin + 1
         END IF

         minew = miold - nmin
         IF (minew .LT. 00) THEN
            minew = minew + 60
            nhour  = nhour + 1
         END IF

         hrnew = hrold - nhour
         IF (hrnew .LT. 00) THEN
            hrnew = hrnew + 24
            nday  = nday + 1
         END IF

         dynew = dyold
         monew = moold
         yrnew = yrold
         DO i = 1, nday
            dynew = dynew - 1
            IF (dynew.eq.0) THEN
               monew = monew - 1
               IF (monew.eq.0) THEN
                  monew = 12
                  yrnew = yrnew - 1
                  ! If the year changes, recompute the number of days in February
                  mday(2) = nfeb(yrnew)
               END IF
               dynew = mday(monew)
            END IF
         END DO
      END IF

c  Now construct the new mdate

c     nlen = LEN(ndate)
      nlen = 14

      IF (nlen.GT.14) THEN
         WRITE(ndate(1:14),19) yrnew, monew, dynew, hrnew, minew, scnew
         WRITE(hfrc,'(I10)') frnew+1000000000
         ndate = ndate(1:14)

      ELSE IF (nlen.eq.14) THEN
         WRITE(ndate(1:14),19) yrnew, monew, dynew, hrnew, minew, scnew
 19   format(I4,I2.2,I2.2,I2.2,I2.2,I2.2)
         IF (nlen.eq.14) ndate = ndate(1:14)

      ELSE IF (nlen.eq.12) THEN
         WRITE(ndate,16) yrnew, monew, dynew, hrnew, minew
 16   format(I4,I2.2,I2.2,I2.2,I2.2)

      ELSE IF (nlen.eq.10) THEN
         WRITE(ndate,13) yrnew, monew, dynew, hrnew
 13   format(I4,I2.2,I2.2,I2.2)

      ELSE IF (nlen.eq.8) THEN
         WRITE(ndate,10) yrnew, monew, dynew
 10   format(I4,I2.2,I2.2)

      END IF

c      IF (olen.GE.11) ndate(11:11) = sp

      return
      end
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
      FUNCTION nfeb ( year ) RESULT (num_days)
  
c Compute the number of days in February for the given year
  
      IMPLICIT NONE
  
      INTEGER :: year
      INTEGER :: num_days
  
      num_days = 28 ! By default, February has 28 days ...
      IF (MOD(year,4).eq.0) THEN
         num_days = 29  ! But every four years, it has 29 days ...
         IF (MOD(year,100).eq.0) THEN
            num_days = 28  ! Except every 100 years, when it has 28 days ...
            IF (MOD(year,400).eq.0) THEN
               num_days = 29  ! Except every 400 years, when it has 29 days.
            END IF
         END IF
      END IF
  
      END FUNCTION nfeb

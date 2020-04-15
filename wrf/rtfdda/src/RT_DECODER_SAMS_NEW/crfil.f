      MODULE myconstants
       real :: spvltr = -888888.0
      END MODULE myconstants

      MODULE mydata
       REAL, DIMENSION ( : ),   ALLOCATABLE :: zprs
       REAL, DIMENSION ( :,: ), ALLOCATABLE :: Rh
       REAL, DIMENSION ( : ),   ALLOCATABLE :: zrh
       REAL, DIMENSION ( : ),   ALLOCATABLE :: zt
       REAL, DIMENSION ( : ),   ALLOCATABLE :: ztt
       REAL, DIMENSION ( :,: ), ALLOCATABLE :: tdry
       REAL, DIMENSION ( :,: ), ALLOCATABLE :: ztt_qc
       REAL, DIMENSION ( :,: ), ALLOCATABLE :: ztd_qc
       REAL, DIMENSION ( :,: ), ALLOCATABLE :: zrh_qc
       REAL, DIMENSION ( :,: ), ALLOCATABLE :: zpp_qc
       REAL, DIMENSION ( :,: ), ALLOCATABLE :: u_wind, v_wind
       REAL, DIMENSION ( :,: ), ALLOCATABLE :: zuu_qc, zvv_qc
       REAL, DIMENSION ( : ),   ALLOCATABLE :: zu,zv
      END MODULE mydata

      MODULE debugging
       INTEGER, PARAMETER :: CHECKPOINT = 1
       logical,PARAMETER  :: debug   = .false.
       logical,PARAMETER  :: debugt  = .true.
       logical,PARAMETER  :: debuguv = .true.
      END MODULE debugging

c     Set max # of records (nrec_p) to once-per-minute for a daily file
      MODULE sizes
       INTEGER            :: nplatforms
       INTEGER, PARAMETER :: nrec_p = 1440
       INTEGER            :: nrecs
       character*19 outfile 
      END MODULE sizes

      program crfil

       use debugging
       use date_pack

       character*60 filename
       logical      is_here

        iun2 = 30
 
c        filename = 'sams.020821.000000.cdf'
c        mdate = 02082100

c        OPEN ( UNIT = iun2          ,
c     &    FILE      = filename      ,
c     &    ACCESS    = 'SEQUENTIAL'  ,
c     &    STATUS    = 'OLD'         ,
c     &    FORM      = 'UNFORMATTED' ,
c     &    IOSTAT    = error_number  )

        read(*,10) filename
  10    format (a)

        inquire (file=filename,EXIST=is_here)
        if( .not. is_here)then
          write(24,6001) filename
 6001     format(1x,' CANNOT FIND FILE: ', 60a1)
        else
          write(24,6002) filename
 6002     format(1x,' FOUND FILE: ', 60a1)
        endif
 
c        read(*,15) mdate
c 15     format (i)
c        write(24,6003) mdate
c 6003   format(1x,' mdate= -> ',i8)

        call rdsams(iun2,mdate,filename)
 
       print *,' program crfile completed successfully '
       stop 99999
       end
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
      subroutine rdsams(iun2,mdate,filename)
c
c This file is a fortran template file designed to read the SAMS
c netCDF file into memory.
c
c Required includes.

      use myconstants
      use mydata
      use debugging
      use sizes
      use date_pack

      include 'netcdf.inc'

C Define Variables.
C Variable ids run sequentially from 1 to nvars=30
C parameter   (nvars = 30)         ! number of variables
C parameter   (nrec  = 84)         ! change this to generalize
C parameter   (pf_p   ... number of platforms)
C parameter   (nrec_p ... number of variables)
      
      integer*4    numhrs,offset
      integer*4    pf_p

      parameter    (ifldlen   = 20 )
      parameter    (nvars_p  = 350) 
      parameter    (ifldlen_p = 350)
      parameter    (pf_p     = 350)            ! Number of platforms
                                              ! dim(# of p-levels, # of stns)
      integer*4    nvars
      integer*4    rcode                      ! read code
      integer*4    recdim                     ! record dimension
      integer*4    ilittleyr

      integer*4    base_time_vid
      integer*4    base_time
      integer      itoff

      real*8       time_offset(nrec_p)
      real*8       time(nrec_p)
      real*8       realtime(nrec_p)
      real*8       minutes(nrec_p)
      real*8       seconds(nrec_p)

      real*4       pres(  pf_p,nrec_p)
      real*4       wdir(  pf_p,nrec_p)
      real*4       lat(pf_p)
      real*4       wspd(  pf_p,nrec_p)
      real*4       lon(pf_p)
      real*4       alt(pf_p)
      character    plf(pf_p, ifldlen)
      integer*4    start(10)
      integer*4    vdims(10)                     ! allow up to 10 dimensions      
      INTEGER*4 STATUS

      dimension    zp(pf_p),  ztd(pf_p),
     &             zdir(pf_p),zspd(pf_p),
     &             ter(pf_p), zx(pf_p), 
     &             slp(pf_p)

      real rhs
      logical  firsttime

      INTEGER century_year(nrec_p),
     &        month(nrec_p),day(nrec_p),hour(nrec_p),
     &        minute(nrec_p),second(nrec_p)
      character*40 strname(pf_p), strnam2(pf_p), string2, string4

      character*31 dummy
      character*31 vnames(pf_p)
      character*60 filename
      character*6  T_units
      logical      is_here
      integer*4    ndims,dimsiz
      integer*4    fdims(3)    
      integer*4    ncid,ngatts,ntp,nvdim
      integer*4    i,j, k1,k2, vdim1, vdim2
      integer      timeid, strng_id, nob
      logical      xyonly, bogus, already_opened
 
      iseq_num = 1

      strnam2(1) = 'FM-12 SYNOP                             '
      strng_id = 98000
  
      firsttime = .true.
      xyonly    = .true.

!      delm   = 7.5
      delm   = 0
      nvars  = nvars_p

      p_max   = 1090.
      p_min   = 500.
      wdir_max= 360.
      wdir_min= 0.
      wspd_max= 60.
      wspd_min= 0.
      zmiss   = -9999.0
      rhs     = 110.

      tfact  = 273.15
      T_units= 'K     '
 
      do ii=2,pf_p
       strnam2(ii) = strnam2(ii-1)
      enddo

      do no=1,pf_p
       strname(no)(1:40) = '                                        '
      enddo
      do no=1,pf_p
       nob = strng_id + no
       write(strname(no)(1:5),'(i5)') nob
      enddo

      bogus = .false.
      klv   = pf_p
      iuno  = iun2

      if (.not.xyonly) then
         print *,'---------------------------------------------------'
      endif

c Open netCDF file.
c
       ncid=ncopn(filename, ncnowrit, rcode)
       if(rcode .ne. 0)then
         print *,' ERROR in opening file ',filename
         stop
       else
         write(24,4001)filename
 4001    format(1x,' successfully opened filename= ',a20)
       endif

c Get info on the record dimension for this file.
       call ncinq(ncid,ndims,nvars,ngatts,recdim,rcode)

       write(24,4002)nvars
 4002  format(1x,' record dimension= ',i5)

c We now want to store the DIM INFO
       if(ndims .ne. 3)then
         print *,'ERROR - DIMS MUST EQUAL 3, not ',ndims
         stop
       else
         do i=1,ndims
           call ncdinq(ncid, i, dummy, dimsiz, rcode)
           write(24,4003)dummy
 4003      format(1x,' dummy= ',a20)
           fdims(i) = dimsiz
           if( .not. xyonly)then
              print *,' DIM= ',dummy,' SIZE= ',dimsiz
            endif
            if(dummy .eq. 'fldlen')then
              if(dimsiz .ne. ifldlen_p)then
                 print *,' WARNING - ifldlen is .ne. to ',ifldlen_p
              endif
            elseif(dummy .eq. 'platform')then
               if(dimsiz .gt. pf_p)then
                  print *,' ERROR: number platforms is .gt. than ',pf_p
                  STOP
               elseif(dimsiz .lt. pf_p)then
                  print *, ' WARNING: resetting klv to ',dimsiz
                  klv   = dimsiz
               elseif(dimsiz .eq. pf_p)then
                  print *, ' INFO: (klv=pf_p) ',klv, ' = ',pf_p
               endif
            elseif(dummy .eq. 'time')then
                 print *,' attribute time is present in the dataset '
            endif
         enddo
      endif

      if(nvars .ne. nvars_p)then
        print *,' WARNING new nvars value is ',nvars
      else
         if( .not. xyonly)then
            print *,'File has ',nvars,' variables.'
            print *,'File has ',ndims,' dims.'
            print *,'File has ',ngatts,' global attributes.'
         endif
      endif
 
      write(24,4005)nvars
 4005 format(1x,' number of variables (nvars)= ',i5)

      call ncdinq(ncid,recdim,dummy,nrecs,rcode)
      write(24,4007) nrecs
 4007 format(1x,' File has ',i5,' records. ')

      nrecord = nrecs

c nrecs contains the # of records for this file
      if(nrecs .gt. nrec_p)then
         print *,' Change nrecs and nrecs_p in code, recompile'
         print *,' new NRECS values should be ',nrecs
         stop
      else
         if( .not. xyonly)then
           write(24,4007) nrecs
         endif
      endif

      if( firsttime )then
         nplatforms = pf_p
         ALLOCATE ( zrh    ( nplatforms ) )
         ALLOCATE ( zt     ( nplatforms ) )
         ALLOCATE ( ztt    ( nplatforms ) )
         ALLOCATE ( zu     ( nplatforms ) )
         ALLOCATE ( zv     ( nplatforms ) )
         ALLOCATE ( zprs   ( nplatforms ) )
         ALLOCATE ( Rh     ( nplatforms, nrec_p ) )
         ALLOCATE ( zrh_qc ( nplatforms, nrec_p ) )
         ALLOCATE ( ztd_qc ( nplatforms, nrec_p ) )
         ALLOCATE ( tdry   ( nplatforms, nrec_p ) )
         ALLOCATE ( ztt_qc ( nplatforms, nrec_p ) )
         ALLOCATE ( u_wind ( nplatforms, nrec_p ) )
         ALLOCATE ( zuu_qc ( nplatforms, nrec_p ) )
         ALLOCATE ( v_wind ( nplatforms, nrec_p ) )
         ALLOCATE ( zvv_qc ( nplatforms, nrec_p ) )
         ALLOCATE ( zpp_qc ( nplatforms, nrec_p ) )
         firsttime = .false.
      endif

C END OF DIM INFO

      base_time_vid = 1
      vnames(base_time_vid) = 'base_time'

      do k2=1,nrec_p
       do nr=1,pf_p
        zpp_qc(nr,k2) = 0
       enddo
      enddo
 
      do i=1,ndims
        start(i) = 1
      enddo

C Begin variable loop
      do i=1,nvars
        call ncvinq(ncid,i,dummy,ntp,nvdim,vdims,nvs,rcode)
 
        write(24,4010)i,dummy
 4010   format(1x,' variable number ',i5,' dummy= ',a20)

         if(rcode .ne. 0)then
            print *,'ERROR 382 - variable ',dummy
         endif
C BASE_TIME
         if(dummy .eq. 'base_time')then
        
           write(24,4012) dummy
 4012      format(1x,' base_time= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 0)then
               print *,' base_time has nvdim -> ',nvdim
               STOP
            else
               start(1) = 1
               call ncvgt1(ncid,i,start,base_time,rcode)
               write(24,4014) base_time
 4014          format(1x,' base-time= ',i4)

               if( .not. xyonly)then
                  print *,' Base_time is: ', base_time
               endif
            endif
C time
         elseif(dummy .eq. 'time')then
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)

                  if(j .eq. 1)then
                     vdim1 = ndsize
                  else
                     print *,' ERROR 384 - variable ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1(ncid,i,start,time(k1),rcode)
               enddo

C time_offset
         elseif(dummy .eq. 'time_offset')then

           write(24,4016) dummy
 4016      format(1x,' time_offset= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 1)then
               print *,'time_offset has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)

                  if(j .eq. 1)then
                     vdim1 = ndsize
                  else
                     print *,' ERROR 386 - variable ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1(ncid,i,start,time_offset(k1),rcode)
               enddo

               if( .not. xyonly)then
                  print *,' time-offset= ',(time_offset(k1),k1=1,vdim1)
               endif
               
            endif

C compute actual time from base_time
           do k1=1,vdim1
             itoff = nint(time_offset(k1))

             call epoch(base_time, itoff, century_year(k1),
     &                month(k1), day(k1), hour(k1), minute(k1), 
     &                  second(k1))
             write(24,4020)base_time,itoff,century_year(k1),
     &                    month(k1),day(k1),hour(k1),minute(k1), 
     &                    second(k1)
 4020        format(1x,' basetime= ',i4,' itoff= ',i6,
     &                   ' century= ',i5,' month= ',i3,
     &                   ' day= ',i3,' hour= ',i3,' minute= ',i3,
     &                   ' second= ',i3)
           enddo

C realtime
         elseif(dummy .eq. 'realtime')then

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 1)then
               print *,' realtime has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
 
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  else
                     print *,' ERROR 388 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1(ncid,i,start,realtime(k1),rcode)
               enddo

               if( .not. xyonly)then
                  print *,' realtime= ',(realtime(k1),k1=1,vdim1)
               endif
               
            endif
         elseif(dummy .eq. 'seconds')then

            write(24,4022) 
 4022       format(1x,' found variable seconds ')
 
C minutes
         elseif(dummy .eq. 'minutes')then
 
            write(24,4024) dummy
 4024       format(2x,' found varname= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 1)then
               print *,'minutes has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  else
                     print *,' ERROR 390 - varname= ',dummy
                     STOP
                  endif
               enddo

               do k1=1,vdim1
                  start(1) = k1

                  call ncvgt1(ncid,i,start,minutes(k1),rcode)
                  seconds(k1) = minutes(k1) * 60.0
               enddo

               if( .not. xyonly)then
                  print *,' minutes= ',(minutes(k1),k1=1,vdim1)
               endif
            endif

C altitude
         elseif(dummy .eq. 'alt')then
c
            write(24,4026)dummy
 4026       format(2x,' found altitude= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 1)then
               print *,' altitude has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  else
                     print *,' ERROR 392 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1(ncid,i,start,alt(k1),rcode)
               enddo
            endif
C platform name
         elseif(dummy .eq. 'platform')then
c
            write(24,4027)dummy
 4027       format(2x,' found platform= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' platform has nvdim -> ',nvdim
               STOP
            else
               do j=1, nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                      vdim2 = ndsize
                  else
                    ! print *,' ERROR 392 - varname= ',dummy
                      print *,' j= varname= ',j,dummy
                      print *,vdims(j)
                      STOP
                  endif
               enddo
               do k1=1,vdim1
               do k2=1,vdim2 
                  start(1) = k1
                  start(2) = k2
                  call ncvg1c(ncid,i,start,plf(k2, k1),rcode)
                print *, plf(k1,k2), k1,k2
               enddo
               enddo
                !stop
            endif
            
C longitude
         elseif(dummy .eq. 'lon')then

            write(24,4028) dummy
 4028       format(1x,' found long= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 1)then
               print *,' long has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  else
                     print *,' ERROR 394 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1(ncid,i,start,lon(k1),rcode)
               enddo

               if( .not. xyonly)then
                 print *,' long= ', (lon(k1),k1=1,vdim1)
               endif
            endif
C lat
         elseif(dummy .eq. 'lat')then

            write(24,4030) dummy
 4030       format(1x,' found latitude= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 1)then
               print *,'lat has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if (j.eq.1) then
                     vdim1 = ndsize
                  else
                     print *,' ERROR 396 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  start(1) = k1
                  call ncvgt1(ncid,i,start,lat(k1),rcode)
               enddo

               if( .not. xyonly)then
                  print *,' latitude= ',(lat(k1),k1=1,vdim1)
               endif
            endif

C tdry
         elseif(dummy .eq. 'tdry')then

            write(24,4032) dummy
 4032       format(2x,' tdry= ',a31)
            
            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' tdry has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 398 - varname= ',dummy
                     STOP
                  endif
               enddo

               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1 (ncid,i,start,tdry(k1,k2),rcode)
                  enddo
               enddo

            endif
         elseif(dummy .eq. 'Temp_qc')then

            write(24,4034) dummy
 4034       format(1x,' found temp_qc= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' Temp_qc has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 400 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1(ncid,i,start,ztt_qc(k1,k2),rcode)
                  enddo
               enddo
            endif
C Rh
         elseif(dummy .eq. 'Rh')then

            write(24,4036) dummy
 4036       format(1x,' found rh= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' Rh has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 402 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1 (ncid,i,start,Rh(k1,k2),rcode)
                  enddo
               enddo
            endif
c
         elseif(dummy .eq. 'Rh_qc')then

            write(24,4038) dummy
 4038       format(1x,' found rh_qc= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' Rh_qc has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 404 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1 (ncid,i,start,zrh_qc(k1,k2),rcode)
                  enddo
               enddo
            endif
C pres
         elseif(dummy .eq. 'pres')then

            write(24,4040) dummy
 4040       format(1x,' found pres= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' pres has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 406 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1 (ncid,i,start,pres(k1,k2),rcode)
                  enddo
               enddo

               do k1=1,vdim1
                  do k2=1,vdim2
                    if(pres(k1,k2) .gt. p_max .or.
     &                         pres(k1,k2) .lt.  0.)then 
                      pres(k1,k2) = spvltr
                    endif
                  enddo
               enddo
            endif
C wdir
         elseif(dummy .eq. 'wdir')then

            write(24,4042) dummy
 4042       format(1x,' found wdir= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' wdir has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 408 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1 (ncid,i,start,wdir(k1,k2),rcode)
                  enddo
               enddo
            endif
C wspd
         elseif(dummy .eq. 'wspd')then

            write(24,4044) dummy
 4044       format(1x,' found wspd= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' wspd has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 410 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1 (ncid,i,start,wspd(k1,k2),rcode)
                  enddo
               enddo
            endif
C u_wind
         elseif(dummy .eq. 'u_wind')then

            write(24,4046) dummy
 4046       format(1x,' found u-wind= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' u_wind has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 412 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1 (ncid,i,start,u_wind(k1,k2),rcode)
                  enddo
               enddo
            endif
C u_wind_qc
         elseif(dummy .eq. 'u_wind_qc')then

            write(24,4048) dummy
 4048       format(1x,' found u-wind_qc= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' u_wind_qc has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 414 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1 (ncid,i,start,zuu_qc(k1,k2),rcode)
                  enddo
               enddo
            endif
C v_wind
         elseif(dummy .eq. 'v_wind')then

            write(24,4050) dummy
 4050       format(1x,' found v-wind= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' v_wind has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 416 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1 (ncid,i,start,v_wind(k1,k2),rcode)
                  enddo
               enddo
            endif
C v_wind_qc
         elseif(dummy .eq. 'v_wind_qc')then

            write(24,4052)dummy
 4052       format(1x,' found v-wind_qc= ',a31)

            if( .not. xyonly)then
               print *,' found varname ->',dummy
            endif
            if(nvdim .ne. 2)then
               print *,' v_wind_qc has nvdim -> ',nvdim
               STOP
            else
               do j=1,nvdim
                  call ncdinq(ncid,vdims(j),dummy,ndsize,rcode)
                  if(j .eq. 1)then
                     vdim1 = ndsize
                  elseif(j .eq. 2)then
                     vdim2 = ndsize
                  else
                     print *,' ERROR 418 - varname= ',dummy
                     STOP
                  endif
               enddo
               do k1=1,vdim1
                  do k2=1,vdim2
                     start(1) = k1
                     start(2) = k2
                     call ncvgt1 (ncid, i,start,zvv_qc(k1,k2),rcode)
                  enddo
               enddo
            endif

         else
C unused
            write(24,4054)dummy 
 4054       format(1x,' *** unused variable ',a31)

            if( .not. xyonly)then
               print *,' UNUSED varname ->',dummy
            endif
         endif

      enddo
C End variable loop
      
c Create output data file
c
       do nr=1,nrecs

         iyr = century_year(nr)
           if(iyr .lt. 2000)then
             nyr = iyr - 1900
           else
             nyr = iyr - 2000
           endif

          imo = month(nr)
          idy = day(nr)
          ihr = hour(nr)
          imn = minute(nr) + int(delm)
          isec= int( (delm - int(delm))*60. ) 

          ndat  = ((((nyr*1000000)+(imo*10000))+
     &            (idy*100))+ihr)
          mdat2 = realtime(nr) + 1   

          write(24,4056) nr,ndat,mdat2,
     &                   iyr,imo,idy,ihr,imn,isec
 4056     format(1x,' rec= ',i5,' ndat= ',i8,' mdat2= ',i8,
     &              ' yr= ',i5,
     &              ' mon= ',i3,' dy= ',i3,
     &              ' hr= ',i3,' min= ',i3,' sec= ',i3)

          do k1=1,pf_p
             zt(k1)   = spvltr
             ztd(k1)  = spvltr
             zspd(k1) = spvltr
             zdir(k1) = spvltr
             slp(k1)  = spvltr
          enddo 

          if(iyr .ge. 2000)then
             ilittleyr = iyr - 2000
          else
             ilittleyr = iyr - 1900
          endif
          write(outfile,'(i4.4,i2.2,i2.2,i2.2,i2.2,i2.2,a)' )
     &                    iyr,imo,idy,ihr,imn,isec,"_SAMS"
          write(24,4058) nr,outfile,
     &                   iyr,imo,idy,ihr,imn,isec
 4058     format(1x,' wrote rec= ',i5, ' file= ',a19,
     &        ' yr= ',i5,' mon= ',i3,' dy= ',i3,' hr= ',i3,
     &        ' min= ',i3,' sec= ',i3)
          
          INQUIRE(FILE=outfile,OPENED=already_opened)
          IF( .not. already_opened)THEN
               OPEN( UNIT=iuno,FILE=outfile,
     &                    ACCESS="SEQUENTIAL",
     &                    FORM="FORMATTED",
     &                    STATUS="UNKNOWN" )
 
            write(24,4060) nr,iuno
 4060       format(1x,' open rec= ',i5,' on output unit= ',i5)

          ENDIF

C ====================== INITIALIZE VALUES ========================
           k2 = nr
           do k1=1,pf_p
             zp(k1)    = 0.
             zdir(k1)  = 0.
             zspd(k1)  = 0.
           enddo
c
c perform gross checks
c
c ter (m) and press (mb convert to Pa)
c pres level
          do k1=1,pf_p
             if( pres(k1,k2) .gt. p_max .or.
     &              pres(k1,k2) .lt. p_min )then
                pres(k1,k2) = spvltr
             endif
          enddo
          do k1=1,pf_p
             zprs(k1) = spvltr
             ter(k1)  = spvltr
             zx(k1)   = spvltr
          enddo
          do k1=1,pf_p
             if(pres(k1,k2) .ne. spvltr)then
               zp(k1)   = pres(k1,k2)
               zprs(k1) = zp(k1)*100.
               zpp_qc(k1,k2) = 0.
               print *,' k1= ',k1,' zp-zprs= ',zp(k1),zprs(k1)
             else
               zprs(k1) = spvltr
               zpp_qc(k1,k2) = 1.
             endif
             ter(k1)  = alt(k1) * 1000.0
             zx(k1)   = alt(k1) * 1000.0
          enddo
c
c wind components
          do k1=1,pf_p
            zu(k1) = 0.
            zv(k1) = 0.
          enddo

          do k1=1,pf_p
             if( (u_wind(k1,k2) .lt. -1000.) .or. 
     &                      (v_wind(k1,k2) .lt. -1000.) .or.
     &           (u_wind(k1,k2) .gt.  1000.) .or. 
     &                      (v_wind(k1,k2) .gt.  1000.) .or. 
     &           (u_wind(k1,k2) .eq. spvltr) .or.
     &                      (zuu_qc(k1,k2) .lt. 0.) .or.
     &           (v_wind(k1,k2) .eq. spvltr) .or.
     &                      (zvv_qc(k1,k2) .lt. 0.) ) then
               u_wind(k1,k2) = zmiss
               v_wind(k1,k2) = zmiss
             endif
          enddo

          do k1=1,pf_p
           if( (u_wind(k1,k2) .ne. zmiss)  .and.
     &         (v_wind(k1,k2) .ne. zmiss)  )then
                 zu(k1) = u_wind(k1,k2)
                 zv(k1) = v_wind(k1,k2)
                 zuu_qc(k1,k2) = zuu_qc(k1,k2)*10.
                 zvv_qc(k1,k2) = zvv_qc(k1,k2)*10.
           else
                 zu(k1) = spvltr
                 zv(k1) = spvltr
                 zuu_qc(k1,k2) = 1.
                 zvv_qc(k1,k2) = 1.
           endif
          enddo

c dir and speed
          do k1=1,pf_p
             if( wdir(k1,k2) .gt. wdir_max .or.
     &            wdir(k1,k2) .lt. wdir_min )then
                wdir(k1,k2) = spvltr
             endif
          enddo
          do k1=1,pf_p
             if( wspd(k1,k2) .gt. wspd_max .or.
     &            wspd(k1,k2) .lt. wspd_min )then
                wspd(k1,k2) = spvltr
             endif
          enddo
          do k1=1,pf_p
             if(wdir(k1,k2) .ne. spvltr .or. 
     &                            wspd(k1,k2) .ne. spvltr)then
                 zdir(k1) = wdir(k1,k2)
                 zspd(k1) = wspd(k1,k2)
             endif
             if(zu(k1) .eq. spvltr .or. zv(k1) .eq. spvltr)then
!                 zdir(k1) = spvltr
!                 zspd(k1) = spvltr
             endif
          enddo
c
c temperature
         do k1=1,pf_p
          zt(k1) = 0.
          ztd(k1) = 0.
         enddo

         do k1=1,pf_p
           if((tdry(k1,k2) .le. zmiss) .or.
     &             (ztt_qc(k1,k2) .lt. 0.)) then
            tdry(k1,k2) = zmiss
            ztt_qc(k1,k2) = 0.
           endif
           zt(k1) = tdry(k1,k2)
           ztt_qc(k1,k2) = ztt_qc(k1,k2)*10. 
         enddo

c convert T to kelvin 
          do k1=1,pf_p
             if( zt(k1) .ne. zmiss )then
                zt(k1)  = zt(k1) + tfact
                ztt(k1) = zt(k1)
             else
                zt(k1) = spvltr
                ztt(k1) = zmiss
             endif
          enddo
c
c RH
          do k1=1,pf_p
           zrh(k1) = Rh(k1,k2)

            if(Rh(k1,k2) .gt. rhs) zrh(k1)=zmiss
            if(Rh(k1,k2) .lt. 0.)  zrh(k1)=zmiss
            if(Rh(k1,k2) .eq. -9999.0)then
               zrh(k1) = zmiss
            endif

            if(Rh(k1,k2) .eq. zmiss)then
               zrh_qc(k1,k2) = 1.
            else
               zrh_qc(k1,k2) = zrh_qc(k1,k2)*10.
            endif
          enddo
c Td
c convert rh to Td
          call rh2td(ztt, T_units, zrh, ztd, klv, zmiss)
          do k1=1,pf_p
            if(ztd(k1) .eq. zmiss)then
               ztd(k1) = spvltr
               ztd_qc(k1,k2) = 1.
            else
               ztd_qc(k1,k2) = zrh_qc(k1,k2)
            endif
          enddo
          ! yliu 20050516: simple buddy check between all SAMS at a range
          ztmean = 0
          ntmean = 0
          do k1=1,pf_p
          if(zt(k1).ne.spvltr) then
                  ntmean = ntmean + 1
                  ztmean = ztmean + zt(k1)
          endif
          enddo
          if(ntmean .gt. 5) then
          ztmean=ztmean/ntmean
          do k1=1,pf_p
           if(abs(ztmean-zt(k1)) .gt. 6) then
           ztt_qc(k1,k2) = 0
           ztd_qc(k1,k2) = 0
           zrh_qc(k1,k2) = 0
           zuu_qc(k1,k2) = 0
           zvv_qc(k1,k2) = 0
           endif
          enddo
          endif
          ! yliu end
c     
c write out SAMS data for OA 
c     
          string2= 'SAMS ATEC                               '
          !string2(11:15)=filename(1:5)
          string4='                                        '
          kkl = 1
          kk2 = 1
          do k1=1,klv
c yliu: ATC has mile/hour in the netcdf file, so convert to m/s
      if(filename(1:4).eq.'rtms'.and. filename(6:9).le."0302") then
      if (zspd(k1) .ne. spvltr) zspd(k1) = zspd(k1) / 2.237
      if (zu(k1) .ne. spvltr) zu(k1) = zu(k1) / 2.237
      if (zv(k1) .ne. spvltr) zv(k1) = zv(k1) / 2.237
      string2='SAMS ATEC - ATC                         '
      endif
             do n = 21,39
               if(plf(k1,n-20).lt."0" .or. plf(k1,n-20).gt."z") then
                  plf(k1,n-20) = " " 
               endif
               string2(n:n)= plf(k1,n-20)
             end do
             
             call write_obs(k1,zprs(k1),zx(k1),zt(k1),ztd(k1),
     &        zspd(k1),zdir(k1),slp(k1),ter(k1),lat(k1),lon(k1),
     &        zu(k1),zv(k1),zpp_qc(k1,k2),ztt_qc(k1,k2),ztd_qc(k1,k2),
     &        zuu_qc(k1,k2),zvv_qc(k1,k2), ndat, imn, isec, kkl, kk2,
     &        strname(k1),string2,strnam2(k1),string4,
     &        bogus,iseq_num,iuno )
          enddo

          close(unit=iuno)
          write(24,4080) nr,iuno
 4080     format(1x,' ### close rec= ',i5,' on unit= ',i5)
          
       enddo
C enddo for NR records

      return
      end
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
      subroutine convert(mdate,iyr,imo,idy,ihr)

      iyr=mdate/1000000
      imo=( mdate - iyr*1000000 ) / 10000
      idy=( mdate - iyr*1000000 -imo*10000 ) / 100
      ihr=( mdate - iyr*1000000 -imo*10000 - idy*100 )

      if(iyr .lt. 60)then
         iyr = iyr + 2000
      else
         iyr = iyr + 1900
      endif

      return
      end
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
       SUBROUTINE write_obs( stindx, p , z , t , td , spd , dir ,
     &                       slp , ter , xlat , xlon , 
     &                       uu  , vv  , pp_qc , tt_qc , td_qc ,
     &                       uu_qc , vv_qc ,
     &                       mdate , imin , isec , kx , kx2 ,
     &                     string1 , string2 , string3 , string4 , 
     &                                 bogus , iseq_num , iunit )

       use mydata
       use debugging
       use sizes

       integer stindx                                  ! station index
       dimension z(kx),t(kx),td(kx),spd(kx),dir(kx),
     &           uu(kx),vv(kx),p(kx)
       real      pp_qc(kx,kx2), tt_qc(kx,kx2), td_qc(kx,kx2),
     &           uu_qc(kx,kx2), vv_qc(kx,kx2)

      character *20 date_char
      character *40 string1, string2 , string3 , string4
      CHARACTER *84 rpt_format
      CHARACTER *22 meas_format
      CHARACTER *14 end_format
      logical bogus

      rpt_format =  ' ( 2f20.5 , 2a40 , '
     &             // ' 2a40 , 1f20.5 , 5i10 , 3L10 , '
     &             // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
      meas_format =  ' ( 10( f13.5 , i7 ) ) '
      end_format = ' ( 3 ( i7 ) ) '

      write (date_char(9:16),fmt='(i8.8)') mdate
      if (mdate/1000000 .GT. 70 ) then
         date_char(7:8)='19'
      else
         date_char(7:8)='20'
      endif
      write (date_char(17:18),fmt='(i2.2)') imin
      write (date_char(19:20),fmt='(i2.2)') isec
      date_char(1:6)='      '

      WRITE ( UNIT = iunit , ERR = 19 , FMT = rpt_format )
     &        xlat,xlon, string1 , string2 ,
     &        string3 , string4 , ter, kx, 0,0,iseq_num,0,
     &        .false.,bogus,.false.,
     &         -888888, -888888, date_char ,
     &         slp,0,-888888.,0, -888888.,0, -888888.,0, -888888.,0,
     &               -888888.,0,
     &               -888888.,0, -888888.,0, -888888.,0, -888888.,0,
     &               -888888.,0,
     &               -888888.,0, -888888.,0

      do k=1,kx
         idir_qc = int( 0.5*(uu_qc(k,kx2)+vv_qc(k,kx2)) )
         ispd_qc = int( 0.5*(uu_qc(k,kx2)+vv_qc(k,kx2)) )
         iu_qc   = int( uu_qc(k,kx2) )
         iv_qc   = int( vv_qc(k,kx2) )
         ip_qc   = int( pp_qc(k,kx2) )
         it_qc   = int( tt_qc(k,kx2) )
         itd_qc  = int( td_qc(k,kx2) )

         WRITE ( UNIT = iunit , ERR = 19 , FMT = meas_format )
     &              p(k),ip_qc,       z(k),0, 
     &              t(k),it_qc,       td(k),itd_qc, 
     &              spd(k),ispd_qc,   dir(k),idir_qc,
     &              uu(k),iu_qc,      vv(k),iv_qc, 
     &              -888888.,0,       -888888.,0
      enddo

      WRITE ( UNIT = iunit , ERR = 19 , FMT = meas_format )
     &           -777777.,0, -777777.,0, float(kx),0,
     &           -888888.,0, -888888.,0, -888888.,0,
     &           -888888.,0, -888888.,0, -888888.,0,
     &           -888888.,0

      WRITE ( UNIT = iunit , ERR = 19 , FMT = end_format )  kx, 0, 0

      return

19    continue
      print *,' troubles writing out data '
      stop 19
      END
c
c x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
c
        subroutine rh2td(TT,T_units,RH,TD,nobs,badval)
        use myconstants

        dimension tt(*), rh(*), td(*)

c  T:   Temperature
c  RH:  Relative Humidity
c  Td:  Dewpoint

        character*6 T_units

        RV = 461.51

        do n=1,nobs
          
           if( tt(n) .eq. BADVAL .or.
     &           rh(n) .eq. BADVAL )then
              td(n) = spvltr
              goto 2030
           endif

           if(t_units.eq.'K     ')then
              ta = TT(n)
           elseif(t_units.eq.'C     ')then
              ta = TT(n) + 273.15
           else
              TD(n) = spvltr
              goto 2030
           endif

           XL = XLAT_HEAT_VAP(ta)
           Td_inv = (1./ta) - (RV/XL) * ALOG(RH(n)/100.)
           Tdd = 1./Td_inv

           if(t_units.eq.'K     ')then
              TD(n) = Tdd
           elseif(t_units.eq.'C     ')then
              TD(n) = Tdd - 273.15
           else
              TD(n) = spvltr
              goto 2030
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
c INPUT:
      real Temp_Kel              ! Temperature ( K )

c OUTPUT:

       if( (Temp_Kel .lt. 150.) .or. (Temp_Kel .GT. 340.) )then
        print*, 'Input Temperature (K) = ', Temp_Kel
        print*, 'Function XLAT_HEAT_VAP.'
        call abort()
       endif

       xlat_heat_vap = 3.1484E6 - 2370. * Temp_Kel

       return
       end

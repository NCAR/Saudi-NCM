  program mm5_to_modis

  USE MM5_HEADER
  USE MODULE_DECODED

  character(len=2) :: opt
  integer :: end_index1,end_index2,end_index3

  integer, parameter :: ks=20,badvalue=-8888.
  real, dimension(ks) :: um,vm,tm,qm,pm,hm,rhm,tdm,wsm,wdm
  real, allocatable, dimension(:,:,:) :: uc,vc,t,q,pp,prs, hei
  real, allocatable, dimension(:,:) ::   pstr,ter,xlat,xlon
  real, allocatable, dimension(:) :: sigma
  real, allocatable, dimension(:,:,:,:) :: data
  real, allocatable, dimension(:) :: t_ps,q_ps,u_ps,v_ps,prs_ps,h_ps

  real, dimension (ks) :: pmb_modis
  data  pmb_modis /1000.0, 950.0, 920.0, 850.0, 780.0, & 
                    700.0, 620.0, 500.0, 400.0, 300.0, & 
                    250.0, 200.0, 150.0, 100.0,  70.0, & 
                     50.0,  30.0,  20.0,  10.0,   5.0/

  integer, parameter :: mm5_unit=71,out_unit=81
  integer :: domain_id
  character :: adomain_id

  integer :: AllocateStatus,flag

  character (len=10) :: atime_new,atime_old='aaaaaaaaaa',mm5_time
  integer :: t_count
  integer :: iab
  logical :: OUTPUT,found,early

  real :: aa, bb, cc, dd
  real :: alon,alat,hs,elev
  real, dimension(50) :: wk,wk1,pwk

  character (len=14) :: atime, atime_out
  character (len=80) :: mm5_file, out_file
  character (len=40) :: id, stname, platform, source
  real, parameter :: xn=0.716  !! Lambert Conformal Projection Factor
  real, parameter :: rgas = 287.04, grav = 9.81
  real :: rhcalc
  real :: tdcalc
  integer, external :: abort
!------------------------------------------------------------------------------!

  CALL arguments (alat,alon,atime,mm5_file,opt)

  pi=acos(-1.)

  ierr=0
  icnt=0         !! counter to determine when MM5 arrays need to be deallocated
  icnt2=0        !! counter to determine when to open the output file
  irec=0         !! counter for direct access record number

  t_count=0      !! counter to keep track of the time period of MM5 output

     call nearest_hour(atime,atime_new)

! Open MM5 file
!
     WRITE (*,'(2A)') "Read file ",TRIM (mm5_file)
     OPEN (UNIT=mm5_unit, FILE=mm5_file, FORM='unformatted', STATUS='old', &
           ACTION='read', IOSTAT=mm5_ierr)

     IF (mm5_ierr /= 0) THEN
         WRITE (*,'("Error opening file ",A)') TRIM (mm5_file)
         iab = abort()
     ENDIF

!    Find the matching MM5 output time

     if(atime_new /= atime_old) then      !! ready to read new MM5 data

       if(icnt > 0) then
         deallocate(uc)
         deallocate(vc)
         deallocate(t)
         deallocate(q)
         deallocate(sigma)
         deallocate(pp)
         deallocate(pstr)
         deallocate(xlat)
         deallocate(xlon)
         deallocate(ter)
         deallocate(prs)
       endif

       found=.FALSE.    !! logical variable to keep track of whether a
                        !! specific soundings have a matching time with MM5

       read(mm5_unit,iostat=mm5_ierr) flag

MM5:   do while(mm5_ierr == 0)

          if(flag == 0) then
            read(mm5_unit,iostat=ier) bhi,bhr,bhic,bhrc
            if(ier/=0) then
               write(*,'("Error reading big header")')
               iab = abort()
            endif

            p0=bhr(2,5)
            ptop=bhr(2,2)
            p0mb=p0/100.
            ts0=bhr(3,5)
            tlp=bhr(4,5)
            xlonc=bhr(3,1)        !! in degrees
            domain_id=bhi(13,1)
            write(adomain_id,'(i1)') domain_id

          elseif(flag == 1) then

            OUTPUT=.FALSE.   !! logical variable to keep track of whether a
                             !! specific time period of MM5 should be
                             !! considered in matching the soundings

            read(mm5_unit,iostat=ier) ndim,start_index,end_index,time, &
                staggering, ordering, current_date, name, units, description
            if(ier/=0) then
               write(*,'("Error reading subheader")')
               iab = abort()
            endif
!
            if(index(opt,'-p') > 0) then      !! When dealing with prilim only,
              if(t_count < 3) OUTPUT=.TRUE.   !! only first 3 output times used.
            elseif(index(opt,'-f') > 0) then  !! When dealing with fcst only,
              if(t_count >= 3) OUTPUT=.TRUE.  !! skip first 3 output times.
            else                              !! When dealing with FINAL,
              OUTPUT=.TRUE.                   !! all output times are used.
            endif

            mm5_time=current_date(1:4)//current_date(6:7)// &
                     current_date(9:10)//current_date(12:13)

            if(mm5_time > atime_new) then

              early=.TRUE.    !! if early is true, it means the specific snd
                              !! time is before the 1st MM5 output time period
            else
              if(mm5_time == atime_new) then
                found=.TRUE.
              endif
              early=.FALSE.
            endif

            if(.not. found .or. .not. OUTPUT) then
              read(mm5_unit) dummy
            else

              icnt=icnt+1

              if (ndim == 1) then
                 allocate(data(end_index(1), 1, 1, 1))
              elseif (ndim == 2) then
                 allocate(data(end_index(1), end_index(2), 1, 1))
              elseif (ndim == 3) then
                 allocate(data(end_index(1), end_index(2), end_index(3), 1))
              endif
!
              read(mm5_unit) data
!
              if(name(1:4) == 'U   ') then
                allocate(uc(end_index(1),end_index(2),end_index(3)))
                do k=1,end_index(3)
                do i=1,end_index(1)
                do j=1,end_index(2)
                   uc(i,j,k)=0.25*(data(i,j,k,1)+data(i+1,j,k,1)+ &
                                   data(i,j+1,k,1)+data(i+1,j+1,k,1))
                enddo
                enddo
                enddo
              elseif(name(1:4) == 'V   ') then
                allocate(vc(end_index(1),end_index(2),end_index(3)))
                do k=1,end_index(3)
                do i=1,end_index(1)
                do j=1,end_index(2)
                   vc(i,j,k)=0.25*(data(i,j,k,1)+data(i+1,j,k,1)+ &
                                   data(i,j+1,k,1)+data(i+1,j+1,k,1))
                enddo
                enddo
                enddo
              elseif(name(1:4) == 'T   ') then
                allocate(t(end_index(1),end_index(2),end_index(3)))
                do k=1,end_index(3)
                do i=1,end_index(1)
                do j=1,end_index(2)
                   t(i,j,k)=data(i,j,k,1)
                enddo
                enddo
                enddo
                end_index3=end_index(3)
              elseif(name(1:4) == 'Q   ') then
                allocate(q(end_index(1),end_index(2),end_index(3)))
                do k=1,end_index(3)
                do i=1,end_index(1)
                do j=1,end_index(2)
                   q(i,j,k)=data(i,j,k,1)
                enddo
                enddo
                enddo
              elseif(name(1:4) == 'SIGM' ) then
                allocate(sigma(end_index(1)))
                do k=1,end_index(1)
                   sigma(k)=data(k,1,1,1)
                enddo
              elseif(name(1:4) == 'PP  ') then
                allocate(pp(end_index(1),end_index(2),end_index(3)))
                do k=1,end_index(3)
                do i=1,end_index(1)
                do j=1,end_index(2)
                   pp(i,j,k)=data(i,j,k,1)
                enddo
                enddo
                enddo
              elseif(name(1:8) == 'PSTARCRS') then
                allocate(pstr(end_index(1),end_index(2)))
                do i=1,end_index(1)
                do j=1,end_index(2)
                   pstr(i,j)=data(i,j,1,1)
                enddo
                enddo
                end_index1=end_index(1)
                end_index2=end_index(2)
              elseif(name(1:8) == 'LATITCRS') then
                allocate(xlat(end_index(1),end_index(2)))
                do i=1,end_index(1)
                do j=1,end_index(2)
                   xlat(i,j)=data(i,j,1,1)
                enddo
                enddo
              elseif(name(1:8) == 'LONGICRS') then
                allocate(xlon(end_index(1),end_index(2)))
                do i=1,end_index(1)
                do j=1,end_index(2)
                   xlon(i,j)=data(i,j,1,1)
                enddo
                enddo
              elseif(name(1:8) == 'TERRAIN ') then
                allocate(ter(end_index(1),end_index(2)))
                do i=1,end_index(1)
                do j=1,end_index(2)
                   ter(i,j)=data(i,j,1,1)
                enddo
                enddo
              endif                                                                 
              deallocate(data)

            endif

          elseif(flag == 2) then

            t_count=t_count+1      !! increment the MM5 time period count

            if(found) then
              if(OUTPUT) then
                allocate(prs(end_index1,end_index2,end_index3))
                allocate(hei(end_index1,end_index2,end_index3))
                do kk=1,end_index3
                do ii=1,end_index1
                do jj=1,end_index2
                   prs(ii,jj,kk)=pstr(ii,jj)*sigma(kk)+ptop ! Ref pres
                   cc = alog(prs(ii,jj,kk)/p0)
                   bb = RGAS * ts0 / GRAV
                   aa = RGAS * tlp / (2.*GRAV)
                   hei(ii,jj,kk)=-(bb * cc + aa * cc * cc)  ! Ref height
                   prs(ii,jj,kk)= (prs(ii,jj,kk)+pp(ii,jj,kk))*0.01 ! Full pres
                enddo
                enddo
                enddo
              endif
              exit MM5
            endif

            if(early) exit MM5

          endif

          read(mm5_unit,iostat=mm5_ierr) flag

       enddo MM5

     endif

     if(.not. found .and. .not. early) then
       print*,'No matching MM5 output can be found for time: ',atime
       iab = abort()
     endif

     atime_old=atime_new

     allocate(t_ps(end_index3))
     allocate(q_ps(end_index3))
     allocate(u_ps(end_index3))
     allocate(v_ps(end_index3))
     allocate(h_ps(end_index3))
     allocate(prs_ps(end_index3))

     call pseudo_snd(alat,alon,end_index1,end_index2,end_index3, &
                     xlat,xlon,t,t_ps,ifound)
     call pseudo_snd(alat,alon,end_index1,end_index2,end_index3, &
                     xlat,xlon,q,q_ps,ifound)
     call pseudo_snd(alat,alon,end_index1,end_index2,end_index3, &
                     xlat,xlon,uc,u_ps,ifound)
     call pseudo_snd(alat,alon,end_index1,end_index2,end_index3, &
                     xlat,xlon,vc,v_ps,ifound)
     call pseudo_snd(alat,alon,end_index1,end_index2,end_index3, &
                     xlat,xlon,hei,h_ps,ifound)
     call pseudo_snd(alat,alon,end_index1,end_index2,end_index3, &
                     xlat,xlon,prs,prs_ps,ifound)


     write (*,'(2A)') "Load time ",atime_new

     if(ifound == 1) then

       WRITE (*,'(A,F7.3,A,F8.3,A)') "Interpolate at ",alat,"N, ",alon,"E."

       call sig2modis(t_ps,tm,prs_ps,pmb_modis,2,end_index3,ks,missing_r)
       call sig2modis(u_ps,um,prs_ps,pmb_modis,1,end_index3,ks,missing_r)
       call sig2modis(v_ps,vm,prs_ps,pmb_modis,1,end_index3,ks,missing_r)
       call sig2modis(q_ps,qm,prs_ps,pmb_modis,1,end_index3,ks,missing_r)
       call sig2modis(h_ps,hm,prs_ps,pmb_modis,2,end_index3,ks,missing_r)

       arg=(alon-xlonc)*xn*pi/180.
       do k=1,ks
          if(um(k) > missing_r .and. vm(k) > missing_r) then
            u=um(k)*cos(arg)+vm(k)*sin(arg)
            v=-um(k)*sin(arg)+vm(k)*cos(arg)
            wsm(k)=sqrt(um(k)**2+vm(k)**2)
            wdm(k)=(1.5*pi-atan2(v,u))*180./pi
            if(wdm(k) > 360.) wdm(k)=wdm(k)-360.
          else
            wsm(k)=missing_r
            wdm(k)=missing_r
          endif

!         Calculate RH

          if(tm(k) > missing_r .and. qm(k) > missing_r) then
            rhm(k)=rhcalc(tm(k),qm(k),pmb_modis(k))
          else
            rhm(k)=missing_r
          endif

!         Calculat Dew point

          if(tm(k) > missing_r .and. qm(k) > missing_r) then
!           tdm(k)=tdcalc(tm(k),rhm(k))
            tdm(k)=t_dew(qm(k),pmb_modis(k))
          else
            tdm(k)=missing_r
          endif

       enddo

       icnt2=icnt2+1

       out_file = TRIM (mm5_file)//".decoded"
       stname     = TRIM (mm5_file)
       id       = "00001"
       platform = "FM-34 MM5"
       source   = opt
       atime_out = atime_new(1:10)//"000000"

       WRITE (*,'(2A)') "Write file ",TRIM (out_file)
       call write_decoded (out_file, alat, alon, h_ps(1), ks,    &
                           id, stname, platform, source, atime_out, &
                           pmb_modis, hm, tm, tdm, rhm)

     else
       write (*,'(A,F7.3,A,F8.3,A)') "Cannot interpolate at ",alat,"N, ",alon,"E."
       write (*,'(A,F7.3,A,F8.3,A)') "Min xlat = ",MINVAL(xlat)," Max xlat = ",MAXVAL (xlat)
       write (*,'(A,F7.3,A,F8.3,A)') "Min xlon = ",MINVAL(xlon)," Max xlon = ",MAXVAL (xlon)

     endif

     !deallocate(t_ps)      !! PGF does not need to deallocate these arrays
     !deallocate(q_ps)      !! because they have been passed on to subroutine
     !deallocate(u_ps)      !! sig2p and dealloacated there after exiting the
     !deallocate(v_ps)      !! subroutine
     !deallocate(h_ps)
     !deallocate(prs_ps)

     WRITE (*,'(A)') ""
     STOP
  end program mm5_to_modis
!
!
!
  function rhcalc(t,q,pm)
!
!   To ensure this function works properly, temperature "t" has to be in K,
!   whereas mixing ration "q" has to be in kg/kg (or g/g), and pressure "pm"
!   in mb.
! 
    es=10**(-2937.4/t-4.9283*log10(t)+23.5518)  !! in mb
    re=q/(1.-q)
    ee=pm*re/(0.622+re)
    rhcalc=ee/es*100.
    if(rhcalc < 0.) rhcalc=0.
    if(rhcalc > 100.) rhcalc=100.

  return
  end function rhcalc

  function tdcalc(t,rh)
!
!   Dew point calculation
!   To ensure this function works properly, temperature "t" has to be in K,
!   and rh in %, dew point is in K.
! 
   implicit none
   real :: t, rh
   real :: invtd, invdifftd, Td
   real, parameter ::  L_over_Rv = 5418.12
   real :: tdcalc

   invdifftd = log (rh/100.) / L_over_Rv
   invtd = 1/t  - invdifftd
   td    = 1. / invtd
   tdcalc   = MIN (td, t)

  return
  end function tdcalc

      function t_dew(w_non,pres_hPa)

!     pres_hPa = Pressure in Hecto Pascals
!     w_non   = mixing ratio (non-dimensional = kg/kg)
!.
      real :: pres_hPa,w_non,t_dew

      p = pres_hPa*100.
      RR=w_non+1e-8
      ES=P*RR/(.622+RR)
      ESLN=LOG(ES)
      T_Dew=(35.86*ESLN-4947.2325)/(ESLN-23.6837)
      return
      end function t_dew


!------------------------------------------------------------------------------!
 SUBROUTINE arguments (alat, alon, atime, mm5_file, opt)

  IMPLICIT NONE
  REAL                :: alat, alon
  CHARACTER (len= 14) :: atime
  CHARACTER (len= 80) :: mm5_file
  CHARACTER (len=  2) :: opt

  CHARACTER (len= 15) :: date15
  CHARACTER (len=200) :: harg, cmd
  INTEGER :: i, numarg, iab
  LOGICAL :: present_lat  = .FALSE.
  LOGICAL :: present_lon  = .FALSE.
  LOGICAL :: present_mm5  = .FALSE.
  LOGICAL :: present_date = .FALSE.
  INTEGER, EXTERNAL :: abort, iargc


  numarg = iargc()

  IF (numarg == 0) THEN
      CALL help
  ENDIF

  i  = 1
  opt = ""

  DO while ( i <= numarg)

     CALL getarg(i, harg)

     IF ((harg (1:2) == "-h") .OR. (harg (1:4) == "-man")) THEN
          CALL help
     ELSEIF (harg (1:2) == "-p") THEN
          opt = TRIM (harg)
     ELSEIF (harg (1:2) == "-f") THEN
          opt = TRIM (harg)
     ELSEIF (harg (1:2) == "-i") THEN
          i = i + 1
          CALL getarg(i, harg)
          mm5_file = TRIM (harg)
          present_mm5 = .TRUE.
     ELSEIF (harg (1:4) == "-lat") THEN
          i = i + 1
          CALL getarg(i, harg)
          READ (harg,*) alat
          present_lat = .TRUE.
     ELSEIF (harg (1:4) == "-lon") THEN
          i = i + 1
          CALL getarg(i, harg)
          READ (harg,*) alon
          present_lon = .TRUE.
     ELSEIF (harg (1:5) == "-date") THEN
          i = i + 1
          CALL getarg(i, harg)
          READ (harg,'(A)') date15
          atime = date15(1:8)//date15(10:15)
          present_date = .TRUE.
     ENDIF

     i = i + 1

   ENDDO

    WRITE (*,'(A)') ""

! Check mandatory arguments

      IF (.NOT. present_mm5) THEN
            WRITE (*,'(A)') "Error: specify MM5 input name after -i argument"
            CALL help
      ENDIF
      IF (.NOT. present_lat) THEN
            WRITE (*,'(A)') "Error: specify latitude after -lat argument"
            CALL help
      ENDIF

      IF (.NOT. present_lon) THEN
            WRITE (*,'(A)') "Error: specify longitude after -lat argument"
            CALL help
      ENDIF

      IF (.NOT. present_date) THEN
            WRITE (*,'(A)') "Error: specify date (ccyymmdd.hhmnss) after -date argument"
            CALL help
      ENDIF

 END subroutine arguments
!------------------------------------------------------------------------------!
 SUBROUTINE help

     IMPLICIT none
     INTEGER :: iab
     CHARACTER (len=200) :: cmd
     INTEGER, EXTERNAL :: abort

     CALL getarg(0, cmd)

     WRITE (*,'(A)') "Help message:"

     WRITE (*,'(/,3A,/,34X,A,/)') "Usage: ",trim (cmd), &
     " [-h-f/-p] -lat alat -lon alon -date ccyymmdd.hhmnss -i mm5_file"

     WRITE (*,'(A)')"Where:"
     WRITE (*,'(1x,A)')"lat:  Latitude  of profile to extract."
     WRITE (*,'(1x,A)')"lon:  Longitude of profile to extract."
     WRITE (*,'(1x,A)')"date: Date ccyymmdd,hhmnss of MM5 record."
     WRITE (*,'(1x,A)')"-i:   MM5 file name to read."
     WRITE (*,'(1x,A)')"-f:   Use forecast."
     WRITE (*,'(1x,A)')"-p:   Use analysis."
     WRITE (*,'(1x,A)')"-h:   Print this help message and exit."
     WRITE (*,'(A)') ""

     iab = abort ()

 END subroutine help
!------------------------------------------------------------------------------!

  program read_snd_pairs
!
  integer :: j,irec,ierr,k
  integer, parameter :: NL=40
  character(len=80) :: fn,exename,aheight
  character(len=10) :: date
  character(len=8) :: st_id
  integer :: elevm, elev
  real :: lat,lon
  integer :: id

  integer :: qc_t,qc_q,qc_rh,qc_ws,qc_wd,qc_ght
  real :: p,tm,to,qm,qo,rhm,rho,wsm,wso,wdm,wdo,ghtm,ghto
  logical :: lheight = .false.

!
  j=iargc()
  if(j < 1) then
    call getarg(0,exename)
    call usage(exename)
    stop
  endif

  if(j == 2) then
    call getarg(2,aheight)
    if(index(aheight,'-height') > 0) then
      lheight = .true.
    else
      print*,'Unknown option: ',trim(aheight)
      call getarg(0,exename)
      call usage(exename)
      stop
    end if
  end if

  call getarg(1,fn)

  if(lheight) then
    open(17,file=fn,access='direct',recl=76)
  else
    open(17,file=fn,access='direct',recl=64)
  end if

  irec=0
  ierr=0

  LOOP1: do while(ierr == 0)
    irec=irec+1
    read(17,rec=irec,iostat=ierr) date,st_id,lat,lon,elevm, elev,id
    if(ierr /= 0) exit LOOP1

    write(81,'(a10,x,a8,f7.2,f8.2,2i10,i2)') date,st_id,lat,lon,elevm, elev,id

    do k=1,NL

       irec=irec+1

       if(lheight) then
         read(17,rec=irec) p,tm,to,qc_t,qm,qo,qc_q,rhm,rho,qc_rh, &
                           wsm,wso,qc_ws,wdm,wdo,qc_wd,ghtm,ghto,qc_ght
         write(81,'(f5.0,6(2f9.2,i7))') p,tm,to,qc_t,qm,qo,qc_q,rhm,rho,qc_rh, &
                                        wsm,wso,qc_ws,wdm,wdo,qc_wd, &
                                        ghtm,ghto,qc_ght
       else
         read(17,rec=irec) p,tm,to,qc_t,qm,qo,qc_q,rhm,rho,qc_rh, &
                           wsm,wso,qc_ws,wdm,wdo,qc_wd
         write(81,'(f5.0,5(2f9.2,i7))') p,tm,to,qc_t,qm,qo,qc_q,rhm,rho,qc_rh, &
                                        wsm,wso,qc_ws,wdm,wdo,qc_wd
       end if

    enddo
  enddo LOOP1

  end program read_snd_pairs
!
!
!
  subroutine usage(exename)

  character(len=*) :: exename

  print*,'Usage: ', TRIM(exename),' filename [-height]'
  print*,'  filename: input binary upper-air verification pairs file'
  print*,'  -height : optional, indicates input file contains height pairs'

  end subroutine usage

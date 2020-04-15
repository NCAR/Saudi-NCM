  program read_snd_pairs
!
  integer, parameter :: NL=40
  character(len=80) :: fn,exename
  character(len=10) :: date
  character(len=8) :: st_id
  real :: lat,lon
  integer :: elev
  integer :: id

  integer :: qc_t,qc_q,qc_rh,qc_ws,qc_wd
  real :: p,tm,to,qm,qo,rhm,rho,wsm,wso,wdm,wdo

!
  j=iargc()
  if(j < 1) then
    call getarg(0,exename)
    print*,'Usage: ', TRIM(exename),' filename'
    stop
  endif

  call getarg(1,fn)
  open(17,file=fn,access='direct',recl=64)

  irec=0
  ierr=0

  LOOP1: do while(ierr == 0)
    irec=irec+1
    read(17,rec=irec,iostat=ierr) date,st_id,lat,lon,elev,id
    if(ierr /= 0) exit LOOP1

    write(81,'(a10,x,a8,f7.2,f8.2,i10,i2)') date,st_id,lat,lon,elev,id

    do k=1,NL

       irec=irec+1

       read(17,rec=irec) p,tm,to,qc_t,qm,qo,qc_q,rhm,rho,qc_rh, &
                         wsm,wso,qc_ws,wdm,wdo,qc_wd
       write(81,'(f5.0,5(2f9.2,i6))') p,tm,to,qc_t,qm,qo,qc_q,rhm,rho,qc_rh, &
                                      wsm,wso,qc_ws,wdm,wdo,qc_wd

    enddo
  enddo LOOP1

  end program read_snd_pairs

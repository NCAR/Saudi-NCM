MODULE WRITE_SND_BIN

INTERFACE WRITE_SND_PAIRS_BIN
  module procedure write_snd_pairs_no_height
  module procedure write_snd_pairs_with_height
END INTERFACE

CONTAINS

  subroutine write_snd_pairs_no_height(iunit,n,tm,to,qc_t,qm,qo,qc_q, &
             rhm,rho,qc_rh,wsm,wso,qc_ws,wdm,wdo,qc_wd,badvalue,irec)

  integer :: n,iunit,irec,k
  real, dimension(n) :: tm,to,qm,qo,rhm,rho,wsm,wso,wdm,wdo
  integer, dimension(n) :: qc_t,qc_q,qc_rh,qc_ws,qc_wd
  real :: p, badvalue

  do k=1,n

     irec=irec+1

     if(k == 1) then
       p=1010.
     else
       p=1000.-(k-2)*25.
     endif

     if(qm(k) > badvalue) qm(k)=qm(k)*1000.
     if(qo(k) > badvalue) qo(k)=qo(k)*1000.

     write(iunit,rec=irec) p, tm(k),to(k),qc_t(k), &
                           qm(k),qo(k),qc_q(k), &
                           rhm(k),rho(k),qc_rh(k), &
                           wsm(k),wso(k),qc_ws(k), &
                           wdm(k),wdo(k),qc_wd(k)

  enddo

  return

  end subroutine write_snd_pairs_no_height
!
!
!
  subroutine write_snd_pairs_with_height(iunit,n,tm,to,qc_t,qm,qo,qc_q, &
             rhm,rho,qc_rh,wsm,wso,qc_ws,wdm,wdo,qc_wd, &
             ghtm,ghto,qc_ght,badvalue,irec)

  integer :: iunit,n,irec,k
  real :: badvalue,p
  real, dimension(n) :: tm,to,qm,qo,rhm,rho,wsm,wso,wdm,wdo,ghtm,ghto
  integer, dimension(n) :: qc_t,qc_q,qc_rh,qc_ws,qc_wd,qc_ght

  do k=1,n

     irec=irec+1

     if(k == 1) then
       p=1010.
     else
       p=1000.-(k-2)*25.
     endif

     if(qm(k) > badvalue) qm(k)=qm(k)*1000.
     if(qo(k) > badvalue) qo(k)=qo(k)*1000.

     write(iunit,rec=irec) p, tm(k),to(k),qc_t(k), &
                           qm(k),qo(k),qc_q(k), &
                           rhm(k),rho(k),qc_rh(k), &
                           wsm(k),wso(k),qc_ws(k), &
                           wdm(k),wdo(k),qc_wd(k), &
                           ghtm(k),ghto(k),qc_ght(k)

  enddo

  return

  end subroutine write_snd_pairs_with_height

END MODULE WRITE_SND_BIN

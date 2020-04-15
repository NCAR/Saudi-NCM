  subroutine write_snd_pairs1(iunit,n,tm,to,qc_t,qm,qo,qc_q,rhm,rho,qc_rh, &
             wsm,wso,qc_ws,wdm,wdo,qc_wd,badvalue,irec)

  real, dimension(n) :: tm,to,qm,qo,rhm,rho,wsm,wso,wdm,wdo
  integer, dimension(n) :: qc_t,qc_q,qc_rh,qc_ws,qc_wd

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

  end subroutine write_snd_pairs1

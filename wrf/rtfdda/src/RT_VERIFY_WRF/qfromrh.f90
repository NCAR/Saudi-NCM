  subroutine qfromrh(rh,rh_qc,t,t_qc,pp,pp_qc,q,q_qc)

  real :: rh,t,pp,es,ee,q
  integer :: rh_qc,t_qc,pp_qc,q_qc
  real, parameter :: eps=0.622, xrv=461.51

  if((rh < 0.) .or. (t < 0.) .or. (pp < 0.)) then
    q=-8888.
  else
    es=10**(-2937.4/t-4.9283*log10(t)+23.5518)   !! in mb
    ee=rh*0.01*es                                     !! in mb
    q=eps*ee/(pp-ee)                      !! pp needs to be in mb
    if (q > 0.1 .or. q < 0.) q=-8888.
  endif

  q_qc=min(rh_qc,t_qc,pp_qc)

  return
  end subroutine qfromrh

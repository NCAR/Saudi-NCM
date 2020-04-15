  subroutine qfromttd(t,t_qc,td,td_qc,pp,pp_qc,q,q_qc,rmissing)

  real :: t,td,q,xlv,x,rh,es,ee,pp,rmissing
  integer :: t_qc,td_qc,pp_qc,q_qc
  real, parameter :: eps=0.622, xrv=461.51

  if((t < 0.) .or. (td < 0.) .or. (pp < 0.)) then
    q=rmissing
  else
    xlv=(2.5-0.002274*(t-273.15))*1000000.
    x=xlv/xrv*(t-td)/t/td
    rh=exp(-x)
    if(rh > 1.) rh=1.
    if(rh < 0.) rh =0.
    es=10**(-2937.4/t-4.9283*log10(t)+23.5518)   !! in mb
    ee=rh*es                                     !! in mb
    q=eps*ee/(pp-ee)                      !! pp needs to be in mb
    if (q > 0.1 .or. q < 0.) q=rmissing
  endif

  q_qc=min(t_qc,td_qc,pp_qc)

  return
  end subroutine qfromttd

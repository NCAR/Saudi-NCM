  subroutine qfromttd(t,t_qc,td,td_qc,pp,pp_qc,q,q_qc,badvalue)

  real :: t,td
  integer :: t_qc,td_qc,pp_qc,q_qc
  real, parameter :: eps=0.622, xrv=461.51

  if((t == badvalue ) .or. (td == badvalue) .or. (pp == badvalue)) then
    q=badvalue
  else
    xlv=(2.5-0.002274*(t-273.15))*1000000.
    x=xlv/xrv*(t-td)/t/td
    rh=exp(-x)
    if(rh > 1.) rh=1.
    if(rh < 0.) rh =0.
    es=10**(-2937.4/t-4.9283*log10(t)+23.5518)   !! in mb
    ee=rh*es                                     !! in mb
    q=eps*ee/(pp-ee)                      !! pp needs to be in mb
  endif

  q_qc=max(t_qc,td_qc,pp_qc)

  return
  end

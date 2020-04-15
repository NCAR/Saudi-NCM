MODULE SUBS

  contains

  subroutine r2rh(r,t,p,rh,iice)
     implicit none
     real, intent(in)  :: r  ! g/g
     real, intent(in)  :: t  ! K
     real, intent(in)  :: p  ! hPa
     real, intent(out) :: rh ! %
     integer, intent(in) :: iice

     real eps, e0, eslcon1, eslcon2, esicon1, esicon2, t0, rh1
     real esat, rsat

     eps=0.62197
     e0=6.1078
     eslcon1=17.2693882
     eslcon2=35.86
     esicon1=21.8745584
     esicon2=7.66
     t0=260.
     if(r .gt.-0.01)then
!      if(iice.eq.1.and.t.le.t0)then
!       esat=e0*exp(esicon1*(t-273.16)/(t-esicon2))
!      else
!       esat=e0*exp(eslcon1*(t-273.16)/(t-eslcon2))
!      endif
!      rsat=eps*esat/(p-esat)
       esat = 6.112 * exp (17.67*(t-273.15)/(t-29.60))
       rsat = 0.622 * esat /(p-esat)

       rh=r*100./rsat
       rh=MIN(rh,100.)
     else
       rh=-8888.
     endif
     return

  end subroutine r2rh

END MODULE SUBS

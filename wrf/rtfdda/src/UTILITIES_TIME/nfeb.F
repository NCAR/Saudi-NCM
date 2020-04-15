integer function nfeb(year)
!
! Compute the number of days in February for the given year.
!
  implicit none
  integer, intent(in) :: year ! Four-digit year

  nfeb = 28 ! By default, February has 28 days ...
  if (mod(year,4).eq.0) then  
     nfeb = 29  ! But every four years, it has 29 days ...
     if (mod(year,100).eq.0) then
        nfeb = 28  ! Except every 100 years, when it has 28 days ...
        if (mod(year,400).eq.0) then
           nfeb = 29  ! Except every 400 years, when it has 29 days ...
           if (mod(year,3600).eq.0) then
              nfeb = 28  ! Except every 3600 years, when it has 28 days.
           endif
        endif
     endif
  endif
end function nfeb

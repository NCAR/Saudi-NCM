subroutine geth_idts (ndate, odate, idt)
  implicit none

!  From 2 input mdates ('YYYY-MM-DD HH:MM:SS.ffff'), 
!  compute the time difference.

!  on entry     -  ndate  -  the new hdate.
!                  odate  -  the old hdate.

!  on exit      -  idt    -  the change in time.
!                            Units depend on length of date strings.

  character (len=*) , intent(inout) :: ndate, odate
  integer           , intent(out)   :: idt

!  Local Variables

!  yrnew    -  indicates the year associated with "ndate"
!  yrold    -  indicates the year associated with "odate"
!  monew    -  indicates the month associated with "ndate"
!  moold    -  indicates the month associated with "odate"
!  dynew    -  indicates the day associated with "ndate"
!  dyold    -  indicates the day associated with "odate"
!  hrnew    -  indicates the hour associated with "ndate"
!  hrold    -  indicates the hour associated with "odate"
!  minew    -  indicates the minute associated with "ndate"
!  miold    -  indicates the minute associated with "odate"
!  scnew    -  indicates the second associated with "ndate"
!  scold    -  indicates the second associated with "odate"
!  i        -  loop counter
!  mday     -  a list assigning the number of days in each month

  character (len=24) :: tdate
  integer :: olen, nlen
  integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
  integer :: yrold, moold, dyold, hrold, miold, scold, frold
  integer :: mday(12), i, newdys, olddys
  logical :: npass, opass
  integer :: isign
  integer :: ifrc

  integer, external :: nfeb

  olen = len(odate)
  nlen = len(ndate)
  if (nlen.ne.olen) then
     write(*,'("GETH_IDTS: NLEN /= OLEN: ", A, 3x, A)') ndate(1:nlen), odate(1:olen)
     call abort
  endif

  if (odate.gt.ndate) then
     isign = -1
     tdate=ndate
     ndate=odate
     odate=tdate
  else
     isign = 1
  end if

!  Assign the number of days in a months

  mday( 1) = 31
  mday( 2) = 28
  mday( 3) = 31
  mday( 4) = 30
  mday( 5) = 31
  mday( 6) = 30
  mday( 7) = 31
  mday( 8) = 31
  mday( 9) = 30
  mday(10) = 31
  mday(11) = 30
  mday(12) = 31

!  Break down old hdate into parts

  hrold = 0
  miold = 0
  scold = 0
  frold = 0

  read(odate(1:4),  '(i4)') yrold
  read(odate(6:7),  '(i2)') moold
  read(odate(9:10), '(i2)') dyold
  if (olen.ge.13) then
     read(odate(12:13),'(i2)') hrold
     if (olen.ge.16) then
        read(odate(15:16),'(i2)') miold
        if (olen.ge.19) then
           read(odate(18:19),'(i2)') scold
           if (olen.gt.20) then
              if (olen.eq.21) then
                 read(odate(21:21),'(i1)') frold
              else if (olen.eq.22) then
                 read(odate(21:22),'(i2)') frold
              else if (olen.eq.23) then
                 read(odate(21:32),'(i3)') frold
              else if (olen.eq.24) then
                 read(odate(21:24),'(i4)') frold
              endif
           end if
        end if
     end if
  end if

!  Break down new hdate into parts

  hrnew = 0
  minew = 0
  scnew = 0
  frnew = 0

  read(ndate(1:4),  '(i4)') yrnew
  read(ndate(6:7),  '(i2)') monew
  read(ndate(9:10), '(i2)') dynew
  if (nlen.ge.13) then
     read(ndate(12:13),'(i2)') hrnew
     if (nlen.ge.16) then
        read(ndate(15:16),'(i2)') minew
        if (nlen.ge.19) then
           read(ndate(18:19),'(i2)') scnew
           if (nlen.gt.20) then
              read(ndate(21:nlen),*) frnew
           end if
        end if
     end if
  end if

!  Check that the dates make sense.

  npass = .true.
  opass = .true.

!  Check that the month of NDATE makes sense.

  if ((monew.gt.12).or.(monew.lt.1)) then
     print*, 'GETH_IDTS:  Month of NDATE = ', monew
     npass = .false.
  end if

!  Check that the month of ODATE makes sense.

  if ((moold.gt.12).or.(moold.lt.1)) then
     print*, 'GETH_IDTS:  Month of ODATE = ', moold
     opass = .false.
  end if

!  Check that the day of NDATE makes sense.

  if (monew.ne.2) then
     ! ...... For all months but February
     if ((dynew.gt.mday(monew)).or.(dynew.lt.1)) then
        print*, 'GETH_IDTS:  Day of NDATE = ', dynew
        npass = .false.
     end if
  else if (monew.eq.2) then
     ! ...... For February
     if ((dynew > nfeb(yrnew)).or.(dynew < 1)) then
        print*, 'GETH_IDTS:  Day of NDATE = ', dynew
        npass = .false.
     end if
  endif
        
!  Check that the day of ODATE makes sense.

  if (moold.ne.2) then
     ! ...... For all months but February
     if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
        print*, 'GETH_IDTS:  Day of ODATE = ', dyold
        opass = .false.
     end if
  else if (moold.eq.2) then
     ! ....... For February
     if ((dyold > nfeb(yrold)).or.(dyold < 1)) then
        print*, 'GETH_IDTS:  Day of ODATE = ', dyold
        opass = .false.
     end if
  end if

!  Check that the hour of NDATE makes sense.

  if ((hrnew.gt.23).or.(hrnew.lt.0)) then
     print*, 'GETH_IDTS:  Hour of NDATE = ', hrnew
     npass = .false.
  end if

!  Check that the hour of ODATE makes sense.

  if ((hrold.gt.23).or.(hrold.lt.0)) then
     print*, 'GETH_IDTS:  Hour of ODATE = ', hrold
     opass = .false.
  end if

!  Check that the minute of NDATE makes sense.

  if ((minew.gt.59).or.(minew.lt.0)) then
     print*, 'GETH_IDTS:  Minute of NDATE = ', minew
     npass = .false.
  end if

!  Check that the minute of ODATE makes sense.

  if ((miold.gt.59).or.(miold.lt.0)) then
     print*, 'GETH_IDTS:  Minute of ODATE = ', miold
     opass = .false.
  end if

!  Check that the second of NDATE makes sense.

  if ((scnew.gt.59).or.(scnew.lt.0)) then
     print*, 'GETH_IDTS:  SECOND of NDATE = ', scnew
     npass = .false.
  end if

!  Check that the second of ODATE makes sense.

  if ((scold.gt.59).or.(scold.lt.0)) then
     print*, 'GETH_IDTS:  Second of ODATE = ', scold
     opass = .false.
  end if

  if (.not. npass) then
     print*, 'Screwy NDATE: ', ndate(1:nlen)
     call abort()
  end if

  if (.not. opass) then
     print*, 'Screwy ODATE: ', odate(1:olen)
     call abort()
  end if

!  Date Checks are completed.  Continue.

!  Compute number of days from 1 January ODATE, 00:00:00 until ndate
!  Compute number of hours from 1 January ODATE, 00:00:00 until ndate
!  Compute number of minutes from 1 January ODATE, 00:00:00 until ndate

  newdys = 0
  do i = yrold, yrnew - 1
     newdys = newdys + 337 + nfeb(i)
  end do

  if (monew .gt. 1) then
     mday(2) = nfeb(yrnew)
     do i = 1, monew - 1
        newdys = newdys + mday(i)
     end do
     mday(2) = 28
  end if

  newdys = newdys + dynew - 1

!  Compute number of hours from 1 January ODATE, 00:00:00 until odate
!  Compute number of minutes from 1 January ODATE, 00:00:00 until odate

  olddys = 0

  if (moold .gt. 1) then
     mday(2) = nfeb(yrold)
     do i = 1, moold - 1
        olddys = olddys + mday(i)
     end do
     mday(2) = 28
  end if

  olddys = olddys + dyold -1

!  Determine the time difference

  idt = (newdys - olddys)
  if (olen.gt.10) then
     idt = idt*24 + (hrnew - hrold)
     if (olen.gt.13) then
        idt = idt*60 + (minew - miold)
        if (olen.gt.16) then
           idt = idt*60 + (scnew - scold)
           if (olen.gt.20) then
              ifrc = olen-20
              ifrc = 10**ifrc
              idt = idt * ifrc + (frnew-frold)
           endif
        endif
     endif
  endif

  if (isign .eq. -1) then
     tdate=ndate
     ndate=odate
     odate=tdate
     idt = idt * isign
  end if

end subroutine geth_idts

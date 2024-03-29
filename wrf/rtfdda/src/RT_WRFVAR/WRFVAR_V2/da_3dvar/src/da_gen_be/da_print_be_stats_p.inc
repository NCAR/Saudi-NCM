subroutine da_print_be_stats_p( outunit, ni, nj, nk, num_bins, num_bins2d, bin, bin2d, &
                                regcoeff1, regcoeff2, regcoeff3 )
!----------------------------------------------------------------------------------
! Purpose : To print out the regression coefficients for the physically transform.
!
! Input   : outunit              --- Fortran unit for writing out
!           ni,nj,nk             --- Dimensions
!           num_bins, num_bins2d --- Number of the 3d and 2d bins
!           bin, bin2d           --- bin index for the gridpoints
!           regcoeff1   --- Regression coefficients for chi = regcoeff1 * psi
!           regcoeff2   --- Regression coefficients for Ps  = SUM( regcoeff2(1:k)*psi(1:k) )
!           regcoeff3   --- Regression coefficients for T(k)= SUM( regcoeff3(k,1:k)*psi(1:k) )
!
! Output  : fort.171    --- regcoef1
!           fort.172    --- regcoeff2
!           fort.173    --- regcoeff3
!
!                                                       Yong-Run Guo  02/23/2005
!-----------------------------------------------------------------------------------

   implicit none

   integer, intent(inout) :: outunit                      ! Output file unit.
   integer, intent(in) :: ni, nj, nk                      ! Grid dimensions. 
   integer, intent(in) :: num_bins                        ! Number of 3D field bins.
   integer, intent(in) :: num_bins2d                      ! Number of 2D field bins.
   integer, intent(in) :: bin(1:ni,1:nj,1:nk)             ! Bin assigned to each 3D point.
   integer, intent(in) :: bin2d(1:ni,1:nj)                ! Bin assigned to each 3D point.
   real, intent(in)    :: regcoeff1(1:num_bins)           ! psi/chi regression cooefficient.
   real, intent(in)    :: regcoeff2(1:nk,1:num_bins2d)    ! psi/ps regression cooefficient.
   real, intent(in)    :: regcoeff3(1:nk,1:nk,1:num_bins2d) ! psi/T regression cooefficient.

   integer             :: k1, k2, i, k, j, b, number      ! Loop counters.

   write(6,'(a,i5)')' psi/chi regression coefficient in unit ', outunit

   open(outunit)
   write(outunit,'(a,i5)')' psi/chi regression coefficient in unit ', outunit
   do b = 1, num_bins
     number = 0
     do i = 1,ni
     do j = 1,nj
     do k = 1,nk
     if ( bin(i,j,k) == b) then
       number = number + 1
     endif
     enddo
     enddo
     enddo
     write(outunit,'("bin=",i6," number_in_bin=",i6,5X,1pE12.5)') &
     b, number, regcoeff1(b)
   enddo
   close(outunit)

   outunit = outunit + 1

   write(6,'(a,i5)')' psi/ps  regression coefficient in unit ', outunit

   open(outunit)
   write(outunit,'(a,i5)')' psi/ps  regression coefficient in unit ', outunit
   do b = 1, num_bins2d
     number = 0
     do i = 1,ni
     do j = 1,nj
     if ( bin2d(i,j) == b) then
       number = number + 1
     endif
     enddo
     enddo
     write(outunit,'(/"bin=",i6," number_in_bin=",i6)') b, number
     write(outunit,'((2X,9(i3,2x,1pE12.5)))') (k,regcoeff2(k,b), k=1,nk)
   enddo
   close(outunit)

   outunit = outunit + 1

   write(6,'(a,i5)')' psi/T   regression coefficient in unit ', outunit

   open(outunit)
   write(outunit,'(a,i5)')' psi/T   regression coefficient in unit ', outunit
   do b = 1, num_bins2d
     number = 0
     do i = 1,ni
     do j = 1,nj
     if ( bin2d(i,j) == b) then
       number = number + 1
     endif
     enddo
     enddo
     write(outunit,'(/"bin=",i6," number_in_bin=",i6)') b, number
     do k1 = 1,nk
        write(outunit,'(2X,"Temperature at k1=",i3)') k1
        write(outunit,'((2X,9(i3,2x,1pE12.5)))') (k2,regcoeff3(k1,k2,b), k2=1,nk)
     enddo
   enddo
   close(outunit)

   write(6,*)

   outunit = outunit + 1

end subroutine da_print_be_stats_p


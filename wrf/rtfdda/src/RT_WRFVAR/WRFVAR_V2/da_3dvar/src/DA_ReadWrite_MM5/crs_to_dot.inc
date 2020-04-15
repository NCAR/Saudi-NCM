subroutine crs_to_dot (crs, dot, miy, mjx)

   implicit none

   integer,                  intent(in)  :: miy, mjx
   real, dimension(miy,mjx), intent(in)  :: crs
   real, dimension(miy,mjx), intent(out) :: dot
   
   integer   :: ie, je, i, j


   ie = miy-1
   je = mjx-1

!  AVERAGE OVER 4 CROSS POINTS
!  ---------------------------

   do j = 2, je
   do i = 2, ie
      dot (i,j) = 0.25 * ( crs (i,j  ) + crs (i-1,j  ) &
                       +   crs (i,j-1) + crs (i-1,j-1))
   enddo
   enddo


!  EXTRAPOLATION AT BOUNDARIES
!  ---------------------------

   do i = 2, ie
     dot (i,  1) = 0.5 * (crs (i,1 ) + crs (i-1,1 ))
     dot (i,mjx) = 0.5 * (crs (i,je) + crs (i-1,je))
   enddo

   do j = 2, je
     dot (1,  j) = 0.5 * (crs (1, j) + crs (1, j-1))
     dot (miy,j) = 0.5 * (crs (ie,j) + crs (ie,j-1))
   enddo

!  ASSIGN CORNER VALUE
!  -------------------

   dot (1,  1  ) = crs (1, 1 )
   dot (1,  mjx) = crs (1, je)
   dot (miy,mjx) = crs (ie,je)
   dot (miy,1  ) = crs (ie,1 )

end subroutine crs_to_dot


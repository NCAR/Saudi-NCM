












!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE PARALLEL_MODULE
!
! This module provides routines for parallelizing.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module parallel_module


   integer, parameter :: IO_NODE = 0

   integer, parameter :: HALO_WIDTH = 3
 
   integer, pointer, dimension(:,:) :: processors, &
                                       proc_minx, proc_maxx, &
                                       proc_miny, proc_maxy
   integer :: nprocs, &
              my_proc_id, &
              nproc_x, nproc_y, &
              my_x, my_y, &
              my_minx, my_miny, my_maxx, my_maxy, &
              comm
 

   contains
 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_start
   !
   ! Purpose: For MPI, the purpose of this routine is to basically set up
   !   a communicator for a rectangular mesh, and determine how many processors
   !   in the x and y directions there will be. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_start()
 
      implicit none
  
      ! Arguments
  
      ! Local variables
      comm = 0
      my_proc_id = IO_NODE
      nprocs = 1
      my_x = 0
      my_y = 0
      nproc_x = 1
      nproc_y = 1
  
      nullify(processors)
      nullify(proc_minx)
      nullify(proc_maxx)
      nullify(proc_miny)
      nullify(proc_maxy)
  
   end subroutine parallel_start 
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_get_tile_dims
   !
   ! Purpose: To compute the starting and ending indices of the patch that the
   !   calling processor is to work on. When there are multiple processors, 
   !   appropriate data structures describing the range of indices being 
   !   worked on by other processors are also allocated and filled
   !   (processors, proc_minx, proc_maxx, proc_miny, proc_maxy).
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_get_tile_dims(idim, jdim)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: idim, jdim
  
      ! Local variables
      allocate(processors(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_minx(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_miny(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_maxx(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_maxy(0:nproc_x-1, 0:nproc_y-1))
  
      processors(0,0) = IO_NODE
      proc_minx(0,0) = 1
      proc_miny(0,0) = 1
      proc_maxx(0,0) = idim
      proc_maxy(0,0) = jdim
      my_minx = 1
      my_maxx = idim
      my_miny = 1
      my_maxy = jdim
  
 
   end subroutine parallel_get_tile_dims


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Copied from RSL_LITE's task_for_point.c until a good way can be found to
   !    get the build mechanism to use the original code in RSL_LITE.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine task_for_point(i_p, j_p, ids_p, ide_p, jds_p, jde_p, npx, npy, px, py)

      implicit none

      ! Arguments
      integer, intent(in) :: i_p, j_p, ids_p, ide_p, jds_p, jde_p, npx, npy
      integer, intent(out) :: px, py

      ! Local variables
      integer :: a, b, rem, idim, jdim, i, j, ids, jds, ide, jde

      i = i_p - 1
      j = j_p - 1
      ids = ids_p - 1
      jds = jds_p - 1
      ide = ide_p - 1
      jde = jde_p - 1

      idim = ide-ids+1
      jdim = jde-jds+1

      i = max(i,ids)
      i = min(i,ide)
      rem = mod(idim, npx)
      a = ( rem / 2 ) * ( (idim / npx) + 1 )
      b = a + ( npx - rem ) * ( idim / npx )
      if ( i-ids < a ) then
         px = (i-ids) / ( (idim / npx) + 1 )
      else if ( i-ids < b ) then
         px = ( a / ( (idim / npx) + 1 ) ) + (i-a-ids) / ( ( b - a ) / ( npx - rem ) ) 
      else 
         px = ( a / ( (idim / npx) + 1 ) ) + (b-a-ids) / ( ( b - a ) / ( npx - rem ) ) + &
                                             (i-b-ids) / ( ( idim / npx ) + 1 ) 
      end if

      j = max(j,jds)
      j = min(j,jde)
      rem = mod(jdim, npy)
      a = ( rem / 2 ) * ( (jdim / npy) + 1 )
      b = a + ( npy - rem ) * ( jdim / npy )
      if ( j-jds < a ) then
         py = (j-jds) / ( (jdim / npy) + 1 )
      else if ( j-jds < b ) then
         py = ( a / ( (jdim / npy) + 1 ) ) + (j-a-jds) / ( ( b - a ) / ( npy - rem ) )
      else
         py = ( a / ( (jdim / npy) + 1 ) ) + (b-a-jds) / ( ( b - a ) / ( npy - rem ) ) + &
                                             (j-b-jds) / ( ( jdim / npy ) + 1 )
      end if

   end subroutine task_for_point
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: gather_whole_field_i
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine gather_whole_field_i(patch_array, ms1, me1, ms2, me2, ms3, me3, &
                                 ps1, pe1, ps2, pe2, ps3, pe3, &
                                 domain_array, ds1, de1, ds2, de2, ds3, de3)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3, &
                             ds1, de1, ds2, de2, ds3, de3
      integer, dimension(ms1:me1,ms2:me2,ms3:me3), intent(in) :: patch_array
      integer, dimension(ds1:de1,ds2:de2,ds3:de3), intent(inout) :: domain_array
  
      ! Local variables
 
   end subroutine gather_whole_field_i
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: gather_whole_field_r
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine gather_whole_field_r(patch_array, ms1, me1, ms2, me2, ms3, me3, &
                                 ps1, pe1, ps2, pe2, ps3, pe3, &
                                 domain_array, ds1, de1, ds2, de2, ds3, de3)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3, &
                             ds1, de1, ds2, de2, ds3, de3
      real, dimension(ms1:me1,ms2:me2,ms3:me3), intent(in) :: patch_array
      real, dimension(ds1:de1,ds2:de2,ds3:de3), intent(inout) :: domain_array
  
      ! Local variables
 
   end subroutine gather_whole_field_r
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: scatter_whole_field_i
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine scatter_whole_field_i(patch_array, ms1, me1, ms2, me2, ms3, me3, &
                                 ps1, pe1, ps2, pe2, ps3, pe3, &
                                 domain_array, ds1, de1, ds2, de2, ds3, de3)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3, &
                             ds1, de1, ds2, de2, ds3, de3
      integer, dimension(ms1:me1,ms2:me2,ms3:me3), intent(inout) :: patch_array
      integer, dimension(ds1:de1,ds2:de2,ds3:de3), intent(in) :: domain_array
  
      ! Local variables
 
   end subroutine scatter_whole_field_i
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: scatter_whole_field_r
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine scatter_whole_field_r(patch_array, ms1, me1, ms2, me2, ms3, me3, &
                                 ps1, pe1, ps2, pe2, ps3, pe3, &
                                 domain_array, ds1, de1, ds2, de2, ds3, de3)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3, &
                             ds1, de1, ds2, de2, ds3, de3
      real, dimension(ms1:me1,ms2:me2,ms3:me3), intent(inout) :: patch_array
      real, dimension(ds1:de1,ds2:de2,ds3:de3), intent(in) :: domain_array
  
      ! Local variables
 
   end subroutine scatter_whole_field_r


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: exchange_halo_r
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine exchange_halo_r(patch_array, &
                              ms1, me1, ms2, me2, ms3, me3, &
                              ps1, pe1, ps2, pe2, ps3, pe3)
 
      implicit none

      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3
      real, dimension(ms1:me1,ms2:me2,ms3:me3), intent(inout) :: patch_array

      ! Local variables
  
   end subroutine exchange_halo_r


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: exchange_halo_i
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine exchange_halo_i(patch_array, &
                              ms1, me1, ms2, me2, ms3, me3, &
                              ps1, pe1, ps2, pe2, ps3, pe3)
 
      implicit none

      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3
      integer, dimension(ms1:me1,ms2:me2,ms3:me3), intent(inout) :: patch_array

      ! Local variables
  
   end subroutine exchange_halo_i
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_bcast_logical
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_bcast_logical(lval)
 
      implicit none
  
      ! Argument
      logical, intent(inout) :: lval
  
      ! Local variables
 
   end subroutine parallel_bcast_logical
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_bcast_int
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_bcast_int(ival, from_whom)
 
      implicit none
  
      ! Argument
      integer, intent(inout) :: ival
      integer, intent(in), optional :: from_whom
  
      ! Local variables
 
   end subroutine parallel_bcast_int
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_bcast_real
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_bcast_real(rval, from_whom)
 
      implicit none
  
      ! Argument
      real, intent(inout) :: rval
      integer, intent(in), optional :: from_whom
  
      ! Local variables
 
   end subroutine parallel_bcast_real
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_bcast_char
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_bcast_char(cval, n, from_whom)
 
      implicit none
  
      ! Argument
      integer, intent(in) :: n
      character (len=n), intent(inout) :: cval
      integer, intent(in), optional :: from_whom
  
      ! Local variables
 
   end subroutine parallel_bcast_char
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_finish
   !
   ! Purpose: Free up, deallocate, and for MPI, finalize.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_finish()
 
      implicit none
  
      ! Arguments
  
      ! Local variables
 
      if (associated(processors)) deallocate(processors)
      if (associated(proc_minx)) deallocate(proc_minx)
      if (associated(proc_maxx)) deallocate(proc_maxx)
      if (associated(proc_miny)) deallocate(proc_miny)
      if (associated(proc_maxy)) deallocate(proc_maxy)
 
   end subroutine parallel_finish


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_abort
   !
   ! Purpose: Terminate everything
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_abort()

      implicit none

      ! Arguments

      ! Local variables

      stop

   end subroutine parallel_abort
 
end module parallel_module

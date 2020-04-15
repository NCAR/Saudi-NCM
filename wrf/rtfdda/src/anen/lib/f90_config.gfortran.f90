!------------------------------------------------------------------------------
!
! f90_config.gfortran.f90 -- Compiler dependencies for gfortran (Mac) compiler.
!
! by Dave Allured
!
! Rev	Date		Notes
! 1.00	2008-oct-02	Initial version needed for fortran_eor_unformatted.
!			Cloned from f90_config.xlf.f90 v1.00.
!
! Note: This module is typically passed through the general
! configuration module config.f90.
!
!------------------------------------------------------------------------------

module f90_config

! Fortran I/O error numbers (iostat values).

   integer, parameter :: fortran_eof = -1
   integer, parameter :: fortran_eor_unformatted = 5016

end module f90_config

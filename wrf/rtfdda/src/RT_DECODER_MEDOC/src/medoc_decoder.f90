
 PROGRAM medoc_decoder
!------------------------------------------------------------------------------!
!
! This directory contained the programs for converting 3-dimensional gridded
! wind data read in a MEDOC file into MM5 LITTLE_R observations format
! wind profiles that can be assimilated into MM5 RT-FFDA and 3D-VAR system.
! See: www.mmm.ucar.edu/mm5/documents/MM5_tut_Web_notes/OA/OA.html#pgfId=644106
! for a description of the format.
!
! COMPILATION: "make" in this directory.
! -----------
!
! USAGE:  medoc_decoder [-v-help] -i MEDOC_FILE -o DECODED_FILE -r RIP_FILE
! -----
!
! WHERE:
! -----
! MEDOC_FILE:   Name of input MEDOC file to convert.
! DECODED_FILE: Name of output file to store decoded data at the MM5 
!               RAWINS/LITTLE_R input format.
! RIP_FILE:     Name of output file to write data location for MM5 RIP namelist.
!
! Argument  MEDOC_FILE is mandatory (max 80 characters).
! File name MEDOC_FILE_decoded will be used if argument DECODED_FILE is omitted.
! File name RIP_FILE_decoded will be used if argument RIP_FILE is omitted.
!
! Only MEDOC spherical coordinates (lat/lon) system is supported.
!
! Enter: "medoc_decoder -help" for on-line help.
! -----
!
! For MEDOC file format: see SCIPUFF documentation section 14.2.2. 
! For MM5 file format see MM5 documentation at:
! www.mmm.ucar.edu/mm5/documents/MM5_tut_Web_notes/OA/OA.html#pgfId=644106
!
! Copyright UCAR (c) 1992 - 2004.
! University Corporation for Atmospheric Research (UCAR),
! National Center for Atmospheric Research (NCAR),
! Research Applications Program (RAP),
! P.O.Box 3000, Boulder, Colorado, 80307-3000, USA. 
!
! Francois Vandenberghe, vandenb@ucar.edu, October 2004.
!------------------------------------------------------------------------------!
   USE module_medoc
   USE module_decoded
!------------------------------------------------------------------------------!

   IMPLICIT NONE

   TYPE (medoc_type) :: medoc

   CHARACTER (LEN =  80) :: fileinp
   CHARACTER (LEN =  80) :: fileout
   CHARACTER (LEN =  80) :: filerip
   CHARACTER (LEN =  80) :: proc_name = "medoc_decoder.f90"
   CHARACTER (LEN =  19) :: sdate0, sdatef
   CHARACTER (LEN =   6) :: stname

   LOGICAL :: debug

   INTEGER, PARAMETER :: unitinp  =  51 ! Input files unit
   INTEGER, PARAMETER :: unitout  =  52 ! Additional input file unit
   INTEGER, PARAMETER :: unitrip  =  53 ! Output files unit

!------------------------------------------------------------------------------!

! 1.  PARSE ARGUMENTS
! ===================

! 1.1 Parse arguments
!     ---------------

      CALL arguments (fileinp, fileout, filerip, debug)

! 1.2 Print date and time
!     -------------------

      CALL DATE_AND_TIME (sdate0 (1:8),sdate0 (9:18))
      WRITE (0,'(/,4A)') "Start of program medoc_decoder at ",&
                          sdate0 (9:14)," on ",sdate0 (1:8)
      WRITE (0,'(A)') &
      "-----------------------------------------------------------------------"


! 2.  READ MEDOC DATA FILE
! ========================

! 2.1 Load the file (memory is allocated in that call)
!     -------------

      CALL read_medoc (fileinp, unitinp, medoc)


! 3.  WRITE OUT MEDOC DATA IN DECODED FORMAT
! =========================================

! 3.1 Open outout files
!     -----------------

      CALL openout (fileout, unitout, .TRUE.)
      CALL openout (filerip, unitrip, .TRUE.)

! 3.2 Write data
!     ----------

      CALL write_decoded (unitout, unitrip, medoc)

! 3.3 Close output file
!     -----------------

      CLOSE (UNIT=unitout)
      CLOSE (UNIT=unitrip)

! 3.4 Free memory
!     -----------

      CALL free_medoc (medoc)

! 4.  END
! =======

! 4.1 Print out ending message
!      -----------------------

      CALL DATE_AND_TIME (sdatef (1:8),sdatef (9:18))

      WRITE (0,'(A)') &
     "-------------------------------------------------------------------------"
      WRITE (0,'(4A,/)') "End of program medoc_decoder at ",&
                          sdatef (9:14)," on ",sdatef (1:8)

!     STOP "Successful completion of program medoc_decoder"

 END PROGRAM medoc_decoder
!------------------------------------------------------------------------------!

 SUBROUTINE arguments (fileinp, fileout, filerip, ldebug)
!------------------------------------------------------------------------------!
     implicit none
     character(len=80) :: fileinp, fileout, filerip
     logical :: ldebug

     character(len=200) :: harg

     integer :: ierr, i, numarg
     logical :: present_v   = .FALSE.
     logical :: present_inp = .FALSE.
     logical :: present_out = .FALSE.
     logical :: present_rip = .FALSE.
     logical :: present_help = .FALSE.
     logical :: default_values = .FALSE.
     character (len=200) :: cmd
     integer, external :: iargc
     integer :: isb, isf
!------------------------------------------------------------------------------!

     numarg = iargc()

     WRITE (*,'(A)') ""
     i = 0
     DO while ( i <= numarg)
        CALL getarg(i, harg)
        WRITE (*,'(A,1X)',ADVANCE='no') TRIM (harg) 
        i = i + 1
     ENDDO
     WRITE (*,'(A)')""

     IF (numarg == 0) THEN
         CALL help (0)
     ENDIF

     i = 1
     ldebug  = .false.
     fileinp = ""
     fileout = ""

     DO while ( i <= numarg)

        CALL getarg(i, harg)

        IF ((harg (1:2) == "-h") .OR. (harg (1:4) == "-man")) THEN
            present_help = .TRUE.
        ELSEIF (harg (1:2) == "-v") THEN
           present_v = .TRUE.
        ELSEIF (harg (1:6) == "-debug") THEN
           ldebug = .TRUE.
        ELSEIF (harg (1:2) == "-i") THEN
           i = i + 1
           CALL getarg(i, harg)
           fileinp = TRIM (harg)
           present_inp = .TRUE.
        ELSEIF (harg (1:2) == "-o") THEN
           i = i + 1
           CALL getarg(i, harg)
           fileout = TRIM (harg)
           present_out = .TRUE.
        ELSEIF (harg (1:2) == "-r") THEN
           i = i + 1
           CALL getarg(i, harg)
           filerip = TRIM (harg)
           present_rip = .TRUE.
        ENDIF

        i = i + 1

     ENDDO


! print help and exit

      IF (present_help) THEN
           CALL help
      ENDIF

! Print version and exit

      IF (present_v) THEN
          CALL getarg(0, cmd)
          WRITE (*,'(2A)') TRIM (cmd)," Version 1.0"
          CALL ABORT
      ENDIF

! Check mandatory arguments

      IF (.NOT. present_inp) THEN
            WRITE (*,'(/,A)') "ERROR: missing name of input file (-i)"
            CALL help
      ENDIF

      IF (.NOT. present_out) THEN
          isb = SCAN (fileinp,"/",.TRUE.)
          isf = LEN_TRIM (fileinp)+1
!         isf = SCAN (fileinp,"medoc",.FALSE.)-1
          fileout =   fileinp (isb+1:isf-1)//"_decoded"
      ENDIF

      IF (.NOT. present_rip) THEN
          isb = SCAN (fileinp,"/",.TRUE.)
          isf = LEN_TRIM (fileinp)+1
!         isf = SCAN (fileinp,"medoc",.FALSE.)-1
          filerip =   fileinp (isb+1:isf-1)//"_rip"
      ENDIF


 END subroutine arguments
!------------------------------------------------------------------------------!
 SUBROUTINE help 

!------------------------------------------------------------------------------!
     IMPLICIT none
     CHARACTER (len=200) :: cmd
!------------------------------------------------------------------------------!

     CALL getarg(0, cmd)

     WRITE (*,'(A)') ""
     WRITE (*,'(A)') "HELP MESSAGE:"
     WRITE (*,'(A)') "------------"
     WRITE (*,'(A)') ""

     WRITE (*,'(A)') "USAGE:  medoc_decoder [-v-help] -i MEDOC_FILE -o DECODED_FILE -r RIP_FILE"
     WRITE (*,'(A)') "-----"
     WRITE (*,'(A)') 
     WRITE (*,'(A)') "WHERE:" 
     WRITE (*,'(A)') "-----"

     WRITE (*,'(A)') " MEDOC_FILE:   Name of input MEDOC file to convert."
     WRITE (*,'(A)') " DECODED_FILE: Name of output file to store decoded data at the MM5 RT-FDDA input format." 
     WRITE (*,'(A)') " RIP_FILE:     Name of output file to write data location for MM5 RIP namelist."

     WRITE (*,'(A)') ""
     WRITE (*,'(A)') "Argument  MEDOC_FILE is mandatory (max 80 characters)."
     WRITE (*,'(A)') "File name MEDOC_FILE_decoded will be used if argument DECODED_FILE is omitted."
     WRITE (*,'(A)') "File name RIP_FILE_decoded will be used if argument RIP_FILE is omitted."
     WRITE (*,'(/,A)') " Only MEDOC spherical coordinates (lat/lon) system is supported."
     WRITE (*,'(A)') ""

     CALL ABORT

 END subroutine help
!------------------------------------------------------------------------------!

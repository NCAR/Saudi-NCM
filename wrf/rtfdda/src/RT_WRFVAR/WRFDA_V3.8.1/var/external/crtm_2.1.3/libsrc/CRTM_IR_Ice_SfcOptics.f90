!
! CRTM_IR_Ice_SfcOptics
!
! Module to compute the surface optical properties for ICE surfaces at
! infrared frequencies required for determining the ICE surface
! contribution to the radiative transfer.
!
! This module is provided to allow developers to "wrap" their existing
! codes inside the provided functions to simplify integration into
! the main CRTM_SfcOptics module.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Jun-2005
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_IR_Ice_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, Display_Message
  USE Spectral_Units_Conversion, ONLY: Inverse_cm_to_Micron
  USE CRTM_Parameters          , ONLY: ZERO, ONE, MAX_N_ANGLES
  USE CRTM_SpcCoeff            , ONLY: SC, SpcCoeff_IsSolar
  USE CRTM_Surface_Define      , ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define , ONLY: CRTM_GeometryInfo_type
  USE CRTM_SfcOptics_Define    , ONLY: CRTM_SfcOptics_type
  USE CRTM_SEcategory          , ONLY: SEVar_type => iVar_type, &
                                       SEcategory_Emissivity
  USE CRTM_IRiceCoeff          , ONLY: IRiceC
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Compute_IR_Ice_SfcOptics
  PUBLIC :: Compute_IR_Ice_SfcOptics_TL
  PUBLIC :: Compute_IR_Ice_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_IR_Ice_SfcOptics.f90,v 1.1 2017/06/30 19:53:21 sheu Exp $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    TYPE(SEVar_type) :: sevar
  END TYPE iVar_type


CONTAINS


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_IR_Ice_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at infrared
!       frequencies over an ice surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Ice_SfcOptics( &
!                        Surface     , &
!                        SensorIndex , &
!                        ChannelIndex, &
!                        SfcOptics   , &
!                        iVar          )
!
! INPUTS:
!       Surface:         Structure containing the surface state data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation. On input the Angle component
!                        is assumed to contain data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the module containing this procedure.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT as it is assumed to contain some data upon input.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_IR_Ice_SfcOptics( &
    Surface     , &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics   , &  ! Output
    iVar        ) &  ! Internal variable output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    TYPE(iVar_type),              INTENT(IN OUT) :: iVar
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Ice_SfcOptics'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: j
    REAL(fp) :: frequency, emissivity

    ! Set up
    err_stat = SUCCESS
    frequency = SC(SensorIndex)%Wavenumber(ChannelIndex)


    ! Compute Lambertian surface emissivity
    err_stat = SEcategory_Emissivity( &
                 IRiceC          , &  ! Input
                 frequency       , &  ! Input
                 Surface%Ice_Type, &  ! Input
                 emissivity      , &  ! Output
                 iVar%sevar        )  ! Internal variable output
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error occurred in SEcategory_Emissivity()'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF


    ! Solar direct component
    IF ( SpcCoeff_IsSolar(SC(SensorIndex), ChannelIndex=ChannelIndex) ) THEN
      SfcOptics%Direct_Reflectivity(:,1) = ONE - emissivity
    END IF


    ! Fill the return emissivity and reflectivity arrays
    SfcOptics%Emissivity(1:SfcOptics%n_Angles,1) = emissivity
    DO j = 1, SfcOptics%n_Angles
      SfcOptics%Reflectivity(j,1,j,1) = ONE - SfcOptics%Emissivity(j,1)
    END DO

  END FUNCTION Compute_IR_Ice_SfcOptics


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_IR_Ice_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at infrared frequencies over an ice surface.
!
!       This function is a wrapper for third party code.
!
!       NB: CURRENTLY THIS IS A STUB FUNCTION AS THERE ARE NO TL
!           COMPONENTS IN THE IR ICE SFCOPTICS COMPUTATIONS.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Ice_SfcOptics_TL( SfcOptics_TL )
!
! OUTPUTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface optical properties required for the tangent-
!                        linear radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the input SfcOptics_TL argument is IN OUT rather
!       than just OUT as it may be defined upon input.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_IR_Ice_SfcOptics_TL( &
    SfcOptics_TL ) &  ! Output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_TL
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Ice_SfcOptics_TL'
    ! Local variables


    ! Set up
    err_stat = SUCCESS


    ! Compute the tangent-linear surface optical parameters
    ! ***No TL models yet, so default TL output is zero***
    SfcOptics_TL%Reflectivity        = ZERO
    SfcOptics_TL%Direct_Reflectivity = ZERO
    SfcOptics_TL%Emissivity          = ZERO

  END FUNCTION Compute_IR_Ice_SfcOptics_TL


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_IR_Ice_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at infrared frequencies over an ice surface.
!
!       This function is a wrapper for third party code.
!
!       NB: CURRENTLY THIS IS A STUB FUNCTION AS THERE ARE NO AD
!           COMPONENTS IN THE IR ICE SFCOPTICS COMPUTATIONS.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Ice_SfcOptics_AD( SfcOptics_AD )
!
! INPUTS:
!       SfcOptics_AD:    Structure containing the adjoint surface optical
!                        properties required for the adjoint radiative
!                        transfer calculation.
!                        *** COMPONENTS MODIFIED UPON OUTPUT ***
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the input adjoint arguments are IN OUT regardless
!       of their specification as "input" or "output". This is because these
!       arguments may contain information on input, or need to be zeroed on
!       output (or both).
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_IR_Ice_SfcOptics_AD( &
    SfcOptics_AD ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_AD
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Ice_SfcOptics_AD'
    ! Local variables


    ! Set up
    err_stat = SUCCESS


    ! Compute the adjoint surface optical parameters
    ! ***No AD models yet, so there is no impact on AD result***
    SfcOptics_AD%Reflectivity        = ZERO
    SfcOptics_AD%Direct_Reflectivity = ZERO
    SfcOptics_AD%Emissivity          = ZERO

  END FUNCTION Compute_IR_Ice_SfcOptics_AD

END MODULE CRTM_IR_Ice_SfcOptics

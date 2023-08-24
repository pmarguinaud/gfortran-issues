MODULE YOEGWDWMS

USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

SAVE

! ------  CONTROLS FOR SIMPLIFIED NON-OROGRAPHIC GRAVITY WAVE SCHEME

! LREGNOGWD: .TRUE. if the regularization for non-orographic GWD is used

TYPE :: TEGWDWMS
LOGICAL :: LREGWWMS
!----------------------------------------------------------------------------
CONTAINS
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
END TYPE TEGWDWMS
!============================================================================

!!TYPE(TEGWDWMS), POINTER :: YREGWDWMS => NULL()

!     --------------------------------------------------------------------
CONTAINS

SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
  
  CLASS(TEGWDWMS), INTENT(IN) :: SELF
  INTEGER        , INTENT(IN) :: KDEPTH
  INTEGER        , INTENT(IN) :: KOUTNO

  INTEGER :: IDEPTHLOC

  
  
  
  

END SUBROUTINE PRINT_CONFIGURATION

END MODULE YOEGWDWMS

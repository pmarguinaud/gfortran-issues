MODULE YOELWRAD

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

! ------ CLOUD CHARACTERISTICS FOR SIMPLIFIED SCHEME

! NLEVLWC  : level up to which cloud effects on LW fluxes are considered
! NLWFR    : frequency of calling longwave radiation for TL/AD
! NLOOPLW  : required number of loops inside of nproma for TL/AD lw radiation 
! NLOOPLWO : optimized number of loops inside of nproma
! NPROMALW : optimized length of loops inside of nproma
! NLASTLW  : length of the last loop inside of nproma
! LOPTLWPR : .true. if shorter loops inside of nproma are required 
! LWLCLHR  : .true. if cloud effects on LW are only computed to the level
!                      defined using NLEVLWC and the current cloud top height 
!            .false. if computed for all levels          

TYPE :: TELWRAD
INTEGER(KIND=JPIM) :: NLEVLWC
INTEGER(KIND=JPIM) :: NLWFR
INTEGER(KIND=JPIM) :: NLOOPLW
INTEGER(KIND=JPIM) :: NLOOPLWO
INTEGER(KIND=JPIM) :: NPROMALW
INTEGER(KIND=JPIM) :: NLASTLW
 
LOGICAL :: LOPTLWPR
LOGICAL :: LWLCLHR
!----------------------------------------------------------------------------
CONTAINS
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 
END TYPE TELWRAD
!============================================================================

!!TYPE(TELWRAD), POINTER :: YRELWRAD => NULL()

!     --------------------------------------------------------------------

CONTAINS

SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)

CLASS(TELWRAD), INTENT(IN) :: SELF
INTEGER       , INTENT(IN) :: KDEPTH
INTEGER       , INTENT(IN) :: KOUTNO

INTEGER :: IDEPTHLOC













END SUBROUTINE PRINT_CONFIGURATION

END MODULE YOELWRAD

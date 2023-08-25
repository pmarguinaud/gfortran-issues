MODULE YOMTNH

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!   -----------------------------------------------------------------

!*   Buffer for trajectory array at time t-dt (nonhydrostatic dynamics)

!    NLENNH95B                    : length of buffer for 3D fields
!    TRAJNH(NLENNH95B,0:NSTOP)    : buffer for 3D fields

TYPE :: TTNH
INTEGER(KIND=JPIM) :: NLENNH95B
REAL(KIND=JPRB),ALLOCATABLE:: TRAJNH(:,:)
!--------------------------------------------------------------
CONTAINS
  
  PROCEDURE, PASS :: PRINT => PRINT_CONFIGURATION 

END TYPE TTNH

!!TYPE(TTNH), POINTER :: YRTNH => NULL()
!==============================================================

!!TYPE(TPTRSLB1), POINTER :: YRPTRSLB1 => NULL()

CONTAINS 
  
SUBROUTINE PRINT_CONFIGURATION(SELF, KDEPTH, KOUTNO)
  
  CLASS(TTNH), INTENT(IN) :: SELF
  INTEGER    , INTENT(IN) :: KDEPTH
  INTEGER    , INTENT(IN) :: KOUTNO

  INTEGER :: IDEPTHLOC

  

  
  
  

END SUBROUTINE PRINT_CONFIGURATION
!   ----------------------------------------------------------------
END MODULE YOMTNH
